.onLoad <- function(libname, pkgname) {
    addResourcePath('svgg', system.file('www', package='svgg'))
    #addResourcePath('roow/images', system.file('www/images', package='roow'))
}

.onAttach <- function(libname, pkgname) {
    devParNameToSVGStyleName <- function (name) 
    {
        switch(name, col = "stroke", colAlpha = "stroke-opacity", 
            fill = "fill", fillAlpha = "fill-opacity", fontweight = "font-weight", 
            fontfamily = "font-family", fontstyle = "font-style", 
            fontsize = "font-size", alpha = "opacity", lty = "stroke-dasharray", 
            lwd = "stroke-width", lineend = "stroke-linecap", linejoin = "stroke-linejoin", 
            linemitre = "stroke-miterlimit", name)
    }
    unlockBinding("devParNameToSVGStyleName", getNamespace("gridSVG"))
    assign("devParNameToSVGStyleName", devParNameToSVGStyleName, getNamespace("gridSVG"))

    StatBoxplot <- proto(ggplot2:::Stat, {
      objname <- "boxplot"

      required_aes <- c("x", "y")
      default_geom <- function(.) svgg:::GeomBoxplot

      calculate_groups <- function(., data, na.rm = FALSE, width = NULL, ...) {
        data <- remove_missing(data, na.rm, c("y", "weight"), name="stat_boxplot",
          finite = TRUE)
        data$weight <- data$weight %||% 1
        width <- width %||%  resolution(data$x) * 0.75

        .super$calculate_groups(., data, na.rm = na.rm, width = width, ...)
      }

      calculate <- function(., data, scales, width=NULL, na.rm = FALSE, coef = 1.5, ...) {
        with(data, {
          qs <- c(0, 0.25, 0.5, 0.75, 1)
          if (length(unique(weight)) != 1) {
            try_require("quantreg")
            stats <- as.numeric(coef(rq(y ~ 1, weights = weight, tau=qs)))
          } else {
            stats <- as.numeric(quantile(y, qs))
          }
          names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")

          iqr <- diff(stats[c(2, 4)])

          outliers <- y < (stats[2] - coef * iqr) | y > (stats[4] + coef * iqr)
          if (any(outliers)) {
            stats[c(1, 5)] <- range(c(stats[2:4], y[!outliers]), na.rm=TRUE)
          }

          if (length(unique(x)) > 1) width <- diff(range(x)) * 0.9

          df <- as.data.frame(as.list(stats))
          df$outliers <- I(list(y[outliers]))
          if (exists('outlier.data.original.title')) {
              df$outlier.data.original.title <- I(list(outlier.data.original.title[outliers]))
          } else {
              xo <- x[outliers]
              yo <- y[outliers]
              labels <- if (is.numeric(yo)) sprintf('%.2f', yo) else yo
              df$outlier.data.original.title <- I(list(labels))
          }


          if (is.null(weight)) {
            n <- sum(!is.na(y))
          } else {
            # Sum up weights for non-NA positions of y and weight
            n <- sum(weight[!is.na(y) & !is.na(weight)])
          }

          df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
          df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)

          transform(df,
            x = if (is.factor(x)) x[1] else mean(range(x)),
            width = width,
            relvarwidth = sqrt(n)
          )
        })
      }
    })

    unlockBinding("StatBoxplot", getNamespace("ggplot2"))
    assign("StatBoxplot", StatBoxplot, getNamespace("ggplot2"))

    unlockBinding("devStartClip", getNamespace("gridSVG"))
    unlockBinding(".__T__devStartClip:gridSVG", getNamespace("gridSVG"))
    f <- function (clip, gp, device) 
        {
            svgClipPath(clip$name, clip$x, clip$y, clip$width, clip$height, svgdev=device@dev)
            cl <- get("contextLevels", envir = .gridSVGEnv)
            cl[length(cl)] <- cl[length(cl)] + 1
            assign("contextLevels", cl, envir = .gridSVGEnv)
            svgStartGroup(clip$name, clip = TRUE, attributes = device@attrs, 
                links = device@links, show = device@show, style = devParToSVGStyle(gp, 
                    device), coords = NULL, classes = clip$classes, svgdev = device@dev)
        }

    environment(f) <- getNamespace('gridSVG')
    setGeneric('devStartClip', where=getNamespace('gridSVG'), def=f)

}
##svgAngleTransform <- function (x, y, angle) 
##{
##    #tryCatch( {
##    if (!is.null(angle) && angle != 0) {
##        paste0("rotate(", round(angle, 2), " ", round(x, 2), 
##            " ", round(y, 2), ")")
##    }
##    else {
##        NULL
##    }
##
##    #}, error=function(e) browser())
##}
##
##    unlockBinding("svgAngleTransform", getNamespace("gridSVG"))
##    assign("svgAngleTransform", svgAngleTransform, getNamespace("gridSVG"))
##
##
##svgClipPath <- function (id, vpx, vpy, vpw, vph, vpa, svgdev = svgDevice()) 
##{
##    clipPathID <- gridSVG:::prefixName(paste(id, "clipPath", sep = getSVGoption("id.sep")))
##    if (vpw < 0) {
##        vpx <- vpx + vpw
##        vpw <- abs(vpw)
##    }
##    if (vph < 0) {
##        vpy <- vpy + vph
##        vph <- abs(vph)
##    }
##    tryCatch({
##    newXMLNode("defs", parent = svgDevParent(svgdev), newXMLNode("clipPath", 
##        attrs = attrList(list(id = clipPathID, transform = svgAngleTransform(vpx, 
##            vpy, vpa))), newXMLNode("rect", attrs = list(x = round(vpx, 
##            2), y = round(vpy, 2), width = round(vpw, 2), height = round(vph, 
##            2), fill = "none", stroke = "none"))))
##    }, error=function(e) browser())
##}
##
##    unlockBinding("svgClipPath", getNamespace("gridSVG"))
##    assign("svgClipPath", svgClipPath, getNamespace("gridSVG"))
##
##
##
##Function: devStartClip (package gridSVG)
##clip="ANY", gp="ANY", device="svgDevice"
##function (clip, gp, device) 
##{
##    svgClipPath(clip$name, clip$x, clip$y, clip$width, clip$height, 
##        device@dev)
##    cl <- get("contextLevels", envir = .gridSVGEnv)
##    cl[length(cl)] <- cl[length(cl)] + 1
##    assign("contextLevels", cl, envir = .gridSVGEnv)
##    svgStartGroup(clip$name, clip = TRUE, attributes = device@attrs, 
##        links = device@links, show = device@show, style = devParToSVGStyle(gp, 
##            device), coords = NULL, classes = clip$classes, svgdev = device@dev)
##}
##
##svgClipPath(clip$name, clip$x, clip$y, clip$width, clip$height, svgdev=device@dev)
##
##This devStartClip calls svgClipPath without explicitly assigning device@dev to the svgdev argument. This leads 
##to device@dev being assigned to the vpa argument in svgClipPath, which then attempts to call svgAngleTransform 
##with the vpa containing the device environment.
