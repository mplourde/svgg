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

    with(coord_transform(coordinates, data, scales), {
      ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape,
      gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), fontsize = size * .pt, 
        onmouseover=onmouseover, onmouseout=onmouseout,
        `data-original-title`=data.original.title)))
    })
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

`%||%` <- ggplot2:::`%||%`

#StatBoxplot <- proto(ggplot2:::Stat, {
#  objname <- "boxplot"
#
#  required_aes <- c("x", "y")
#  default_geom <- function(.) svgg:::GeomBoxplot
#
#  calculate_groups <- function(., data, na.rm = FALSE, width = NULL, ...) {
#    data <- remove_missing(data, na.rm, c("y", "weight"), name="stat_boxplot",
#      finite = TRUE)
#    data$weight <- data$weight %||% 1
#    width <- width %||%  resolution(data$x) * 0.75
#
#    .super$calculate_groups(., data, na.rm = na.rm, width = width, ...)
#  }
#
#  calculate <- function(., data, scales, width=NULL, na.rm = FALSE, coef = 1.5, ...) {
#    with(data, {
#      qs <- c(0, 0.25, 0.5, 0.75, 1)
#      if (length(unique(weight)) != 1) {
#        try_require("quantreg")
#        stats <- as.numeric(coef(rq(y ~ 1, weights = weight, tau=qs)))
#      } else {
#        stats <- as.numeric(quantile(y, qs))
#      }
#      names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
#
#      iqr <- diff(stats[c(2, 4)])
#
#      outliers <- y < (stats[2] - coef * iqr) | y > (stats[4] + coef * iqr)
#      if (any(outliers)) {
#        stats[c(1, 5)] <- range(c(stats[2:4], y[!outliers]), na.rm=TRUE)
#      }
#
#      if (length(unique(x)) > 1) width <- diff(range(x)) * 0.9
#
#      browser()
#      df <- as.data.frame(as.list(stats))
#      df$outliers <- I(list(y[outliers]))
#
#      if (is.null(weight)) {
#        n <- sum(!is.na(y))
#      } else {
#        # Sum up weights for non-NA positions of y and weight
#        n <- sum(weight[!is.na(y) & !is.na(weight)])
#      }
#
#      df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
#      df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
#
#      transform(df,
#        x = if (is.factor(x)) x[1] else mean(range(x)),
#        width = width,
#        relvarwidth = sqrt(n)
#      )
#    })
#  }
#})
#
#unlockBinding("StatBoxplot", getNamespace("ggplot2"))
#assign("StatBoxplot", StatBoxplot, getNamespace("ggplot2"))

GeomBoxplot <- proto(ggplot2:::Geom, {
  objname <- "boxplot"

  reparameterise <- function(., df, params) {
    df$width <- df$width %||%
      params$width %||% (resolution(df$x, FALSE) * 0.9)

    if (!is.null(df$outliers)) {
      suppressWarnings({
        out_min <- vapply(df$outliers, min, numeric(1))
        out_max <- vapply(df$outliers, max, numeric(1))
      })

      df$ymin_final <- pmin(out_min, df$ymin)
      df$ymax_final <- pmax(out_max, df$ymax)
    }

    # if `varwidth` not requested or not available, don't use it
    if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(df$relvarwidth)) {
      df$xmin <- df$x - df$width / 2
      df$xmax <- df$x + df$width / 2
    } else {
      # make `relvarwidth` relative to the size of the largest group
      df$relvarwidth <- df$relvarwidth / max(df$relvarwidth)
      df$xmin <- df$x - df$relvarwidth * df$width / 2
      df$xmax <- df$x + df$relvarwidth * df$width / 2
    }
    df$width <- NULL
    if (!is.null(df$relvarwidth)) df$relvarwidth <- NULL

    df
  }

  draw <- function(., data, ..., fatten = 2, outlier.colour = NULL, outlier.shape = NULL, outlier.data.original.title=NULL,
                  outlier.size = 2, notch = FALSE, notchwidth = .5, varwidth = FALSE) {
    common <- data.frame(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group,
      stringsAsFactors = FALSE
    )

    whiskers <- data.frame(
      x = data$x,
      xend = data$x,
      y = c(data$upper, data$lower),
      yend = c(data$ymax, data$ymin),
      alpha = NA,
      common)

    box <- data.frame(
      xmin = data$xmin,
      xmax = data$xmax,
      ymin = data$lower,
      y = data$middle,
      ymax = data$upper,
      ynotchlower = ifelse(notch, data$notchlower, NA),
      ynotchupper = ifelse(notch, data$notchupper, NA),
      notchwidth = notchwidth,
      alpha = data$alpha,
      common)

    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- data.frame(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = ifelse(is.na(outlier.colour) || is.null(outlier.colour), data$colour[1], outlier.colour),
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        data.original.title = data$outlier.data.original.title[[1]],
        data.original.title = '',
        onmouseover='',
        onmouseout='',
        fill = NA,
        alpha = NA,
        stringsAsFactors = FALSE)
      outliers_grob <- svgg:::GeomPoint$draw(outliers, ...)
    } else {
      outliers_grob <- NULL
    }

    ggname(.$my_name(), grobTree(
      outliers_grob,
      ggplot2:::GeomSegment$draw(whiskers, ...),
      ggplot2:::GeomCrossbar$draw(box, fatten = fatten, ...)
    ))
  }

  guide_geom <- function(.) "boxplot"
  draw_legend <- function(., data, ...)  {
    data <- aesdefaults(data, .$default_aes(), list(...))
    gp <- with(data, gpar(col=colour, fill=alpha(fill, alpha), lwd=size * .pt, lty = linetype))
    gTree(gp = gp, children = gList(
      linesGrob(0.5, c(0.1, 0.25)),
      linesGrob(0.5, c(0.75, 0.9)),
      rectGrob(height=0.5, width=0.75),
      linesGrob(c(0.125, 0.875), 0.5)
    ))
  }

  default_stat <- function(.) StatBoxplot
  default_pos <- function(.) PositionDodge
  default_aes <- function(.) aes(weight=1, colour="grey20", fill="white", size=0.5, alpha = NA, shape = 16, linetype = "solid")
  required_aes <- c("x", "lower", "upper", "middle", "ymin", "ymax")

})

#geom_boxplot <- ggplot2::geom_boxplot
#environment(geom_boxplot) <- environment()
geom_boxplot <- function (mapping = NULL, data = NULL, stat = "boxplot", position = "dodge", 
    outlier.colour = NULL, outlier.shape = NULL, outlier.size = NULL, 
    notch = FALSE, notchwidth = 0.5, varwidth = FALSE, ...) 
{
    outlier_defaults <- ggplot2:::Geom$find("point")$default_aes()
    outlier.colour <- outlier.colour %||% outlier_defaults$colour
    outlier.shape <- outlier.shape %||% outlier_defaults$shape
    outlier.size <- outlier.size %||% outlier_defaults$size
    svgg:::GeomBoxplot$new(mapping = mapping, data = data, stat = stat, 
        position = position, outlier.colour = outlier.colour, 
        outlier.shape = outlier.shape, outlier.size = outlier.size, 
        notch = notch, notchwidth = notchwidth, varwidth = varwidth, 
        ...)
}


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
            #ggplot(mtcars, aes(wt, mpg)) + geom_line() + geom_point(alpha=0, onmouseover='set_alpha(this, 1)', 
            #    onmouseout='set_alpha(this, 0)')
            #ggplot(mtcars, aes(factor(cyl), mpg, outlier.data.original.title=mpg)) + geom_boxplot()
            #p <- ggplot(mtcars, aes(factor(cyl), mpg, fill=factor(vs))) + geom_boxplot()
            p <- ggplot(diamonds, aes(factor(cut), carat, colour=factor(color))) + 
                geom_boxplot(outlier.colour=NA)
            p
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

