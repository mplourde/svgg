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
        `data-original-title`=point.labels)))
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
      )
    )
  }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) {
        aes(shape=16, colour="black", size=2, fill = NA, alpha = 1, 
          onmouseover='', onmouseout='',
          point.labels=paste0('(', paste(if (is.numeric(x)) sprintf('%.2f', x) else x, 
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

  draw <- function(., data, ..., fatten = 2, outlier.colour = NULL, outlier.shape = NULL, outlier.labels=NULL,
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
        point.labels = as.character(data$outlier.labels[[1]]),
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

#' @export
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
ggplot2SVG <- function(g, ..., id, width=400, height=400, res=72) {
    plot.temp <- tempfile()
    svg.temp <- tempfile()

    png(file=plot.temp, height=height, width=width)
    print(g)
    svg.txt <- suppressWarnings(invisible(as(grid.export(..., name=svg.temp, res=res)$svg, 'character')))
    dev.off()

    unlink(plot.temp)
    unlink(svg.temp)
    return(svg.txt)
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
        container(
            id=paste(outputId, '_proxy_', sep='_'), 
            class='shiny-plot-output', 
            style=style, # required if you want the width/height args to svgOutput to take effect
            div(id=outputId, class='shiny-svg-output'),
            tags$canvas(id=paste(outputId, '_canvas_', sep='_'), style='display:none')
        ),
        svg.js
    )
}

# gt <- ggplot_gtable(ggplot_build(g))
# panels <- gt$grobs[gt$layout$name == 'panel']
# points <- getGrob(panels[[1]], gPath('geom_point'), grep=TRUE)

#' @export 
renderSVGG <- function(expr, 
    tooltips=list(trigger='hover', container='body', placement='top'), 
    popover=list(trigger='click', placement='bottom', title='Plot', content=''),
    download.btn=TRUE,
    save.as=NULL,
    width='auto', 
    height='auto', 
    res=72, 
    env=parent.frame(), 
    quoted=FALSE) {

    installExprFunction(expr, "func", env, quoted)

    widthWrapper <- if (is.function(width)) reactive({width()}) else NULL
    heightWrapper <- if (is.function(height)) reactive({height()}) else NULL
    outputFunc <- svgOutput
    if (!identical(height, "auto")) 
        formals(outputFunc)["height"] <- list(NULL)
    
    markRenderFunction(outputFunc, 
        function(shinysession, name, ...) {
            if (!is.null(widthWrapper)) width <- widthWrapper()
            if (!is.null(heightWrapper)) height <- heightWrapper()
            prefix <- "output_"
            if (width == 'auto') width <- shinysession$clientData[[paste0(prefix, name, '__proxy__width')]]
            if (height == 'auto') height <- shinysession$clientData[[paste0(prefix, name, '__proxy__height')]]
            #if (width == 'auto') width <- shinysession$clientData[[paste0(prefix, name, '_width')]]
            #if (height == 'auto') height <- shinysession$clientData[[paste0(prefix, name, '_height')]]
            if (is.null(width) || is.null(height) || width <= 0 || height <= 0) {
                return(NULL)
            }

            pixelratio <- shinysession$clientData$pixelratio
            if (is.null(pixelratio)) pixelratio <- 1
            
            popover.opts=list(
                trigger='click', 
                placement='bottom', 
                title='Plot',
                content=''
            )
            show.popover <- FALSE
            if (! identical(popover, FALSE)) {
                if (! is.list(popover)) {
                    stop('renderSVGG: popover options must be a list.')
                }
                show.popover = TRUE
                popover.opts <- modifyList(popover.opts, popover)
            }

            tooltip.opts <- list(trigger='hover', placement='top')
            show.tooltips <- FALSE
            if (! identical(tooltips, FALSE)) {
                if (! is.list(tooltips)) {
                    stop('renderSVGG: tooltip options must be a list.')
                }
                show.tooltips <- TRUE
                tooltip.opts <- modifyList(tooltip.opts, tooltips)
                tooltip.opts$container <- 'body'
            }

            if (is.null(save.as)) {
                save.as <- paste0(popover.opts$title, '.png')
            }

            svg.html <- ggplot2SVG(func(), res=res*pixelratio, width=width*pixelratio, height=height*pixelratio)
            svg.html <- paste0('<div class="svg_container">', svg.html, '</div>')

            list(svg_html=svg.html,
                show_popover=show.popover,
                popover_opts=popover.opts,
                show_tooltips=show.tooltips,
                tooltip_opts=tooltip.opts,
                download_btn=download.btn,
                save_as=save.as
            )
        }
    )
}

#' @export
svgg.example <- function() {
    ui <- fluidPage(
        h2('svgg Example'),
        p('This demo will take a moment to load. Click any plot to activate its download popover.'),
        hr(),
        h4('simple_boxplot'),
        code("
        output$simple_boxplot <- renderSVGG({
            p <- ggplot(diamonds, 
                    aes(cut, carat, outlier.labels=I(color), group=cut)) + 
                geom_boxplot()
            p
        }, popover=list(title='simple_boxplot'))
        "),
        svgOutput('simple_boxplot'),
        h4('complex_boxplot'),
        svgOutput('complex_boxplot'),
        h4('scatter_plot_default_labels'),
        svgOutput('scatter_plot_default_labels'),
        h4('scatter_plot_custom_labels'),
        svgOutput('scatter_plot_custom_labels'),
        h4('line_plot'),
        svgOutput('line_plot'),
        h4('arrangeGrob'),
        svgOutput('arrangeGrob')
    )

    server <- function(input, output, session) {
        output$simple_boxplot <- renderSVGG({
            p <- ggplot(diamonds, 
                    aes(cut, carat, outlier.labels=I(color), group=cut)) + 
                geom_boxplot()
            p
        }, popover=list(title='simple_boxplot'))

        output$complex_boxplot <- renderSVGG({
            p <- ggplot(diamonds, 
                    aes(cut, carat, fill=color, outlier.labels=I(carat), 
                        group=interaction(cut, color))) + 
                geom_boxplot()
            p
        }, popover=list(title='complex_boxplot'))

        output$scatter_plot_default_labels <- renderSVGG({
            p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
        }, popover=list('scatter_plot_default_labels'))

        output$scatter_plot_custom_labels <- renderSVGG({
            p <- ggplot(mtcars, aes(wt, mpg, point.labels=cyl)) + geom_point()
        }, popover=list('scatter_plot_custom_labels'))

        output$line_plot <- renderSVGG({
            set.seed(100)
            n <- 50
            d <- data.frame(x=seq(n), y=rnorm(n))
            ggplot(d, aes(x, y)) + geom_line() + geom_point(alpha=0, size=4, color='red', 
                onmouseover='set_alpha(this, 1)', onmouseout='set_alpha(this, 0)')
        }, popover=list(title='line_plot'))

        output$arrangeGrob <- renderSVGG({
            p1 <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
            p2 <- ggplot(diamonds, 
                    aes(cut, carat, outlier.labels=I(color), group=cut)) + 
                geom_boxplot()
            arrangeGrob(p1, p2, nrow=1)
        }, popover=list(title='arrangeGrob'))
    }

    runApp(list(ui=ui, server=server))
}

test1 <- function() {
    g <- ggplot(mtcars, aes(x=wt, y=mpg, label=mpg)) + geom_text() + facet_wrap(~ cyl)
    #g <- qplot(disp, mpg, data=mtcars) + facet_wrap(~ cyl)
    view.labeled.grob(g)
}

test2 <- function() {
    #g <- qplot(disp, mpg, data=mtcars) + facet_wrap(~ cyl)
    g <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
    #grob2LabeledSVG(g, name='junk.svg', exportCoords='inline', exportMappings='inline', exportJS='inline')
    outpath <- file.path("D:\\Data\\MatthewPlourde\\RAJESH\\roow\\junk", 'junk.svg')
    #grob2SVG(g, name=outpath, exportCoords='inline', exportMappings='inline', exportJS='inline')
    grob2SVG(g, name=outpath)
}

