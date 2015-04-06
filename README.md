svgg
================================
**svgg** is an R package for adding mouse-over effects to ggplot2 plot outputs and displaying them using the Shiny framework. The mouse-over effects include point tooltips for `geom_point`, `geom_line`, `geom_boxplot`, and any such plots combined with `arrangeGrob` from the **gridExtra** package. This package relies on the **gridSVG** package for converting plots built on the grid graphics library to SVG.

Installation and Usage
================================
Install **devtools** to enable github installation.

```r
setInternet2() # enable R to use your proxy settings, if necessary
options(repos=c(CRAN='http://cran.us.r-project.org')) # set your repository
install.packages('devtools')
```

Install **svgg**
```r
library(devtools)
install_github('mplourde/svgg')
```

Run `svg.example` for examples of the outputs you can generate with this package.

```r
library(svgg)
svgg.example()
```

**svgg** conforms to the established syntax of **ggplot2** and **shiny**. For example, to generate a boxplot with outlier tooltips and a download-as-PNG popover, you would write something like the
following:

```r
output$simple_boxplot <- renderSVGG('simple_boxplot_svg', {
    p <- ggplot(diamonds, 
            aes(cut, carat, outlier.data.original.title=I(color), group=cut)) + 
        geom_boxplot()
    p
}, popover.opts=list(
        trigger='click', 
        placement='bottom', 
        html='true', 
        title='simple_boxplot',
        content=svgDownloadButton('simple_boxplot_dl', 'Download PNG', 'simple_boxplot'))
)
```
