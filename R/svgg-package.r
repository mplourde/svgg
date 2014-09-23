#' svgg.
#'
#' @name svgg
#' @docType package
#' @import grid gridExtra ggplot2 shiny proto htmltools ggplot2 gridSVG RJSONIO scales

GeomPoint <- proto(ggplot2:::Geom, {
  objname <- "point"

  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {
    data <- remove_missing(data, na.rm,
      c("x", "y", "size", "shape"), name = "geom_point")
    if (empty(data)) return(zeroGrob())

    with(coord_transform(coordinates, data, scales),
      ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape,
      gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), fontsize = size * .pt, 
        onmouseover=onmouseover, onmouseout=onmouseout,
        `data-original-title`=data.original.title)))
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))

    with(data,
      pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape,
      gp=gpar(
        col=alpha(colour, alpha),
        fill=alpha(fill, alpha),
        fontsize = size * .pt)
        #`data-original-title`=data.original.title)
      )
    )
  }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) {
        aes(shape=16, colour="black", size=2, fill = NA, alpha = 1, onmouseover='', onmouseout='',
          data.original.title=paste0('(', paste(if (is.numeric(x)) sprintf('%.2f', x) else x, 
                      if (is.numeric(y)) sprintf('%.2f', y) else y,
                      sep=', '), ')')
        )
  }

})



#' @export 
geom_point <- ggplot2::geom_point
environment(geom_point) <- environment()
#geom_point <- as.function(c(formals(ggplot2::geom_point), body(ggplot2::geom_point)))

#GeomHighlight <- proto(ggplot2:::Geom, {
#  objname <- "point"
#
#  draw_groups <- function(., ...) .$draw(...)
#  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {
#    data <- remove_missing(data, na.rm,
#      c("x", "y", "size", "shape"), name = "geom_point")
#    if (empty(data)) return(zeroGrob())
#
#    with(coord_transform(coordinates, data, scales),
#      ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape,
#      gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), fontsize = size * .pt, 
#        `data-original-title`=data.original.title,
#        style=style)))
#    )
#  }
#
#  draw_legend <- function(., data, ...) {
#    data <- aesdefaults(data, .$default_aes(), list(...))
#
#    with(data,
#      pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape,
#      gp=gpar(
#        col=alpha(colour, alpha),
#        fill=alpha(fill, alpha),
#        fontsize = size * .pt)
#        #`data-original-title`=data.original.title)
#      )
#    )
#  }
#
#  default_stat <- function(.) StatIdentity
#  required_aes <- c("x", "y")
#  default_aes <- function(.) {
#        aes(shape=16, colour="black", size=2, fill = NA, alpha = 1, #group=x, #style='visibility:hidden;',
#          onmouseover='',
#          data.original.title=paste0('(', paste(if (is.numeric(x)) sprintf('%.2f', x) else x, 
#                      if (is.numeric(y)) sprintf('%.2f', y) else y,
#                      sep=', '), ')')
#        )
#  }
#
#})
#
#geom_highlight <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
#    na.rm = FALSE, ...) 
#{
#    GeomHighlight$new(mapping = mapping, data = data, stat = stat, 
#        position = position, na.rm = na.rm, ...)
#}
#
#geom_line2 <- function(...) {
#    geom_line(...) + geom_highlight()
#
#}



#GeomPath <- proto(ggplot2:::Geom, {
#  objname <- "path"
#
#  draw_groups <- function(., ...) .$draw(...)
#
#  draw <- function(., data, scales, coordinates, arrow = NULL, lineend = "butt", linejoin = "round", linemitre = 1, ..., na.rm = FALSE) {
#    if (!anyDuplicated(data$group)) {
#      message("geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?")
#    }
#
#    keep <- function(x) {
#      # from first non-missing to last non-missing
#      first <- match(FALSE, x, nomatch = 1) - 1
#      last <- length(x) - match(FALSE, rev(x), nomatch = 1) + 1
#      c(
#        rep(FALSE, first),
#        rep(TRUE, last - first),
#        rep(FALSE, length(x) - last))
#    }
#    # Drop missing values at the start or end of a line - can't drop in the
#    # middle since you expect those to be shown by a break in the line
#    missing <- !complete.cases(data[c("x", "y", "size", "colour",
#      "linetype")])
#    kept <- ave(missing, data$group, FUN=keep)
#    data <- data[kept, ]
#    # must be sorted on group
#    data <- arrange(data, group)
#
#    if (!all(kept) && !na.rm) {
#      warning("Removed ", sum(!kept), " rows containing missing values",
#        " (geom_path).", call. = FALSE)
#    }
#
#    munched <- coord_munch(coordinates, data, scales)
#
#    # Silently drop lines with less than two points, preserving order
#    rows <- ave(seq_len(nrow(munched)), munched$group, FUN = length)
#    munched <- munched[rows >= 2, ]
#    if (nrow(munched) < 2) return(zeroGrob())
#
#    # Work out whether we should use lines or segments
#    attr <- ddply(munched, .(group), function(df) {
#      data.frame(
#        solid = identical(unique(df$linetype), 1),
#        constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
#      )
#    })
#    solid_lines <- all(attr$solid)
#    constant <- all(attr$constant)
#    if (!solid_lines && !constant) {
#      stop("geom_path: If you are using dotted or dashed lines",
#        ", colour, size and linetype must be constant over the line",
#        call.=FALSE)
#    }
#
#    # Work out grouping variables for grobs
#    n <- nrow(munched)
#    group_diff <- munched$group[-1] != munched$group[-n]
#    start <- c(TRUE, group_diff)
#    end <-   c(group_diff, TRUE)
#
#    if (!constant) {
#      with(munched,
#        segmentsGrob(
#          x[!end], y[!end], x[!start], y[!start],
#          default.units="native", arrow = arrow,
#          gp = gpar(
#            col = alpha(colour, alpha)[!end], fill = alpha(colour, alpha)[!end],
#            lwd = size[!end] * .pt, lty = linetype[!end],
#            lineend = lineend, linejoin = linejoin, linemitre = linemitre,
#            `data-original-title`=data.original.title
#
#          )
#        )
#      )
#    } else {
#      id <- match(munched$group, unique(munched$group))
#      with(munched,
#        polylineGrob(
#          x, y, id = id,
#          default.units = "native", arrow = arrow,
#          gp = gpar(
#            col = alpha(colour, alpha)[start], fill = alpha(colour, alpha)[start],
#            lwd = size[start] * .pt, lty = linetype[start],
#            lineend = lineend, linejoin = linejoin, linemitre = linemitre,
#            `data-original-title`=data.original.title
#            )
#        )
#      )
#    }
#  }
#
#  draw_legend <- function(., data, ...) {
#    data$arrow <- NULL
#    data <- aesdefaults(data, .$default_aes(), list(...))
#
#    with(data,
#      ggname(.$my_name(), segmentsGrob(0.1, 0.5, 0.9, 0.5, default.units="npc",
#      gp=gpar(col=alpha(colour, alpha), lwd=size * .pt,
#        lty=linetype, lineend="butt")))
#    )
#  }
#
#  default_stat <- function(.) StatIdentity
#  required_aes <- c("x", "y")
#  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = NA, data.original.title=NA)
#  guide_geom <- function(.) "path"
#
#})
#
#
#geom_line <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) {
#  GeomLine$new(mapping = mapping, data = data, stat = stat, position = position, ...)
#}
#
#GeomLine <- proto(GeomPath, {
#  objname <- "line"
#
#  draw <- function(., data, scales, coordinates, arrow = NULL, ...) {
#    data <- data[order(data$group, data$x), ]
#    GeomPath$draw(data, scales, coordinates, arrow, ...)
#  }
#
#  default_stat <- function(.) StatIdentity
#
#})

#' @export 
ggplot2SVG <- function(g, ..., id, width=400, height=400, res=72, 
    tooltip.opts=list(trigger='hover', container='body', placement='right'),
    popover.opts=list()) {

    if ('content' %in% names(tooltip.opts)) {
        tooltip.opts[['content']] <- as(tooltip.opts[['content']], 'character')
    }

    if ('content' %in% names(popover.opts)) {
        popover.opts[['content']] <- as(popover.opts[['content']], 'character')
    }

    interfaces <- list(
        tooltips=if (is.list(tooltip.opts)) sprintf("$('#%s [data-original-title]').tooltip(%s);", id, toJSON(tooltip.opts)) else '',
        popover=if (is.list(popover.opts)) sprintf("$('#%s').parent().popover(%s);", id, toJSON(popover.opts)) else ''
    )
    js <- sprintf("$(document).ready(function() {%s});", paste(interfaces, collapse='\n'))

    png('NUL', height=height, width=width)
    on.exit(dev.off())
    print(g)
    grid.script(js)
    invisible(as(grid.export(..., name='junk.svg', prefix=id, res=res)$svg, 'character'))
}

svg.js <- htmlDependency(name='svgBindings', version='0.0.1', c(href='svgg'), script='svgOutput.js')
#svgD3 <- htmlDependency(name='svgD3', version='0.0.1', c(href='svgg'), script='d3.min.js')

#' @export
svgOutput <- function(outputId, width='100%', height='400px', inline=FALSE) {
    if (grepl('%', height)) {
        stop("svgOutput does not accept % height units.")
    }
    style <- paste("width:", validateCssUnit(width), ";", "height:", validateCssUnit(height))
    container <- if (inline) span else div
    attachDependencies(
        div(
            container(div(id=outputId, class='shiny-svg-output', style=style), 
                id=paste(outputId, '_proxy_', sep='_'), 
                class='shiny-plot-output', 
                style=style # required if you want the width/height args to svgOutput to take effect
            ),
            tags$canvas(id=paste(outputId, '_canvas_', sep='_'), style='display:none')
        ),
        svg.js
    )
}

# gt <- ggplot_gtable(ggplot_build(g))
# panels <- gt$grobs[gt$layout$name == 'panel']
# points <- getGrob(panels[[1]], gPath('geom_point'), grep=TRUE)

#' @export 
svgDownloadButton <- function(inputId, label, svgOutput.id, download) {
    if (missing(download)) {
        download <- paste0(svgOutput.id, '.png')
    }
    sprintf('<button id="%s" type="button" onclick="download_svg(\'%s\', \'%s\')" class="btn action-button">%s</button>',
        inputId, svgOutput.id, download, label)
}

#' @export 
renderSVGG <- function(svg.id, expr, tooltip.opts=list(trigger='hover', container='body', placement='bottom'), popover.opts=NA, 
    width='auto', height='auto', res=72, env=parent.frame(), quoted=FALSE) {

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

            #if (! any(c('ggplot', 'gtable') %in% class(p))) {
            #    stop("Expression supplied to renderSVGG returns unrecognized GROB type.")
            #}
            list(svg_html=ggplot2SVG(p, id=svg.id, tooltip.opts=tooltip.opts, popover.opts=popover.opts, 
                res=res*pixelratio, width=width*pixelratio, height=height*pixelratio))
        }
    )
}

#' @export
test0 <- function() {
    ui <- fluidPage(
        #tags$head(tags$script(src='roow/grob-label/svgOutput-bindings.js')),
        svgOutput('svg_test'),
        plotOutput('reg_test')
    )

    server <- function(input, output, session) {
        output$svg_test <- renderSVGG({
            #ggplot(mtcars, aes(rescale(wt), rescale(mpg))) + geom_line() + 
            #    geom_point(aes(x=rescale(Sepal.Length), rescale(Sepal.Width), data.original.title=Species), 
            #        data=iris, size=2, colour='black') +
            #    theme(axis.title.x=element_text(size=12, vjust=0))
            
            mtcars <- within(mtcars, gear <- as.factor(gear))
            #ggplot(mtcars, aes(wt, mpg, group=1, colour=gear)) + geom_line()
            ggplot(mtcars, aes(wt, mpg)) + geom_line() + geom_point(alpha=0, onmouseover='set_alpha(this, 1)', 
                onmouseout='set_alpha(this, 0)')
        }, svg.id='bacon', 
           popover.opts=list(trigger='click', placement='bottom', html='true', title='bacon',
                content=svgDownloadButton('svg_test_dl', 'Download PNG', 'svg_test'))
        )
        output$reg_test <- renderPlot({
            print(ggplot(mtcars, aes(wt, mpg)) + geom_line())
        })
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

