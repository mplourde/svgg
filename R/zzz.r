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

}
