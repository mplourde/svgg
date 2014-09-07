#' svgg.
#'
#' @name svgg
#' @docType package
#' @import grid gridExtra ggplot2 shiny proto htmltools ggplot2 gridSVG

GeomPoint <- proto(ggplot2:::Geom, {
  objname <- "point"

  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm,
      c("x", "y", "size", "shape"), name = "geom_point")
    if (empty(data)) return(zeroGrob())

    with(coord_transform(coordinates, data, scales),
      ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape,
      gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), fontsize = size * .pt, title=title)))
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
      pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape,
      gp=gpar(
        col=alpha(colour, alpha),
        fill=alpha(fill, alpha),
        fontsize = size * .pt,
        title=title)
      )
    )
  }

  browser()

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) {
        aes(shape=16, colour="black", size=2, fill = NA, alpha = 1, 
          title=paste0('(', paste(if (is.numeric(x)) sprintf('%.2f', x) else x, 
                      if (is.numeric(y)) sprintf('%.2f', y) else y,
                      sep=', '), ')')
        )
  }

})

#devParNameToSVGStyleName <- function (name) 
#{
#    browser()
#    switch(name, col = "stroke", colAlpha = "stroke-opacity", 
#        fill = "fill", fillAlpha = "fill-opacity", fontweight = "font-weight", 
#        fontfamily = "font-family", fontstyle = "font-style", 
#        fontsize = "font-size", alpha = "opacity", lty = "stroke-dasharray", 
#        lwd = "stroke-width", lineend = "stroke-linecap", linejoin = "stroke-linejoin", 
#        linemitre = "stroke-miterlimit", name)
#}
#unlockBinding("devParNameToSVGStyleName", getNamespace("gridSVG"))
#assign("devParNameToSVGStyleName", devParNameToSVGStyleName, getNamespace("gridSVG"))
##lockBinding("devParNameToSVGStyleName", getNamespace("gridSVG"))
#
#unlockBinding("devParNameToSVGStyleName", as.environment('package:gridSVG'))
#assign("devParNameToSVGStyleName", devParNameToSVGStyleName, as.environment('package:gridSVG'))

svgDependency <- htmlDependency(name='svg-ouput', version='0.0.1', c(href='roow/grob-label'), script='svgOutput-bindings.js')

##' @export
#geom_point <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
#    na.rm = FALSE, ...) 
#{
#    GeomPoint$new(mapping = mapping, data = data, stat = stat, 
#        position = position, na.rm = na.rm, ...)
#}

#' @export 
geom_point <- ggplot2::geom_point
environment(geom_point) <- environment()
#geom_point <- as.function(c(formals(ggplot2::geom_point), body(ggplot2::geom_point)))


#' @export 
ggplot2SVG <- function(g, ..., id, width=400, height=400, res=72, tooltips=TRUE) {
    png('NUL', height=height, width=width)
    on.exit(dev.off())

    print(g)

    if (tooltips) {
        tooltip.js <- sprintf("
        $(document).ready(function() {
            $('#%s [title]').tooltip({
                trigger : 'hover',
                container : 'body', 
                placement : 'right'
            });
        });", id)
        grid.script(tooltip.js)
    }

    invisible(as(grid.export(..., prefix=id, res=res)$svg, 'character'))
}

#' @export
svgOutput <- function(outputId, width='100%', height='400px', inline=FALSE) {
    style <- paste("width:", validateCssUnit(width), ";", "height:", validateCssUnit(height))
    container <- if (inline) span else div
    attachDependencies(
        container(div(id=outputId, class='shiny-svg-output', style=style), 
            id=paste(outputId, '_proxy_', sep='_'), 
            class='shiny-plot-output', 
            style=style),
        svgDependency
    )
}

# gt <- ggplot_gtable(ggplot_build(g))
# panels <- gt$grobs[gt$layout$name == 'panel']
# points <- getGrob(panels[[1]], gPath('geom_point'), grep=TRUE)


#' @export 
renderSVGG <- function(expr, svg.id, tooltips=TRUE, width='auto', height='auto', res=72, env=parent.frame(), quoted=FALSE) {

    if (tooltips & missing(svg.id)) {
        stop('renderSVGG requires and svg.id when tooltips is TRUE.')
    }

    svg.id <- if (missing(svg.id)) "" else svg.id

    installExprFunction(expr, "func", env, quoted)

    widthWrapper <- if (is.function(width)) reactive({width()}) else NULL
    heightWrapper <- if (is.function(height)) reactive({height()}) else NULL
    outputFunc <- if (identical(height, "auto")) svgOutput else function(outputId) svgOutput(outputId, height = NULL)

    markRenderFunction(outputFunc, 
        function(shinysession, name, ...) {
            if (!is.null(widthWrapper)) width <- widthWrapper()
            if (!is.null(heightWrapper)) height <- heightWrapper()
            prefix <- "output_"
            if (width == 'auto') width <- shinysession$clientData[[paste0(prefix, name, '__proxy__width')]]
            if (height == 'auto') height <- shinysession$clientData[[paste0(prefix, name, '__proxy__height')]]
            if (is.null(width) || is.null(height) || width <= 0 || height <= 0) {
                return(NULL)
            }

            pixelratio <- shinysession$clientData$pixelratio
            if (is.null(pixelratio)) pixelratio <- 1
            
            p <- func()

            if (! any(c('ggplot', 'gtable') %in% class(p))) {
                stop("Expression supplied to renderSVGG returns unrecognized GROB type.")
            }
            list(svg_html=ggplot2SVG(p, id=svg.id, res=res*pixelratio, width=width*pixelratio, 
                height=height*pixelratio, tooltips=tooltips))
        }
    )
}

#' @export
test0 <- function() {
    ui <- fluidPage(
        #tags$head(tags$script(src='roow/grob-label/svgOutput-bindings.js')),
        svgOutput('svg_test')
    )

    server <- function(input, output, session) {
        output$svg_test <- renderSVGG({
            ggplot(mtcars, aes(wt, mpg)) + geom_point()
        }, svg.id='bacon')
    }

    runApp(list(ui=ui, server=server))
}

#' @export
test1 <- function() {
    g <- ggplot(mtcars, aes(x=wt, y=mpg, label=mpg)) + geom_text() + facet_wrap(~ cyl)
    #g <- qplot(disp, mpg, data=mtcars) + facet_wrap(~ cyl)
    view.labeled.grob(g)
}

#' @export
test2 <- function() {
    #g <- qplot(disp, mpg, data=mtcars) + facet_wrap(~ cyl)
    g <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
    #grob2LabeledSVG(g, name='junk.svg', exportCoords='inline', exportMappings='inline', exportJS='inline')
    outpath <- file.path("D:\\Data\\MatthewPlourde\\RAJESH\\roow\\junk", 'junk.svg')
    #grob2SVG(g, name=outpath, exportCoords='inline', exportMappings='inline', exportJS='inline')
    grob2SVG(g, name=outpath)
}




# options(RCurlOptions=list(proxy='web.proxy.com:8080'))
# py <- plotly("me", "mec3g0e1z7")
# p <- ggplot(mtcars, aes(x=mpg, y=cyl)) + geom_point()
# res <- py$ggplotly(k, kwargs=list(auto_open=FALSE))



##' @export 
#grob2LabeledSVG <- function(g=NULL, ..., height=400, width=400) {
#    if (!is.null(g)) {
#        #pdf(file=NULL, width=width, height=height)
#        png('NUL', height=height, width=width)
#        on.exit(dev.off())
#        if ('ggplot' %in% class(g)) {
#            print(g)
#        } else {
#            grid.draw(g)
#        }
#    }
#
#    grobs <- invisible(grid.ls())
#    names <- grobs$name[grobs$type == 'grobListing']
#
#    #for (name in unique(names)) {
#    #    grid.garnish(name, onmouseover=paste0("showTooltip(evt, '", name, "')"), onmouseout="hideTooltip()")
#    #}
#    for (name in unique(names)) {
#        grid.garnish(name, title=name, class='tooltipped', rel='tooltip', group=TRUE)
#    }
#    #tooltip.js.f <- system.file('www', 'grob-label', 'tooltip.js', package='roow')
#    #tooltip.js <- readChar(tooltip.js.f, file.info(tooltip.js.f)$size)
#    tooltip.js <- "
#    $(document).ready(function() {
#        $('.tooltipped').tooltip({
#            trigger : 'click',
#            container : 'body', 
#            placement : 'right'
#        });
#    });
#    "
#            #selector : '[rel=tooltip]'
#    grid.script(tooltip.js)
#    #grid.script(filename=tooltip.js.f, inline=FALSE)
#    as(grid.export(...)$svg, 'character')
#}

