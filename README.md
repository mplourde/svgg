svgg
================================
**svgg** is an R package for adding mouse effects to ggplot2 plot outputs presented with Shiny. The mouse effects include point tooltips for `geom_point`, `geom_line`, `geom_boxplot`, and any combination of these plots created with `arrangeGrob` from the **gridExtra** package. This package relies on the **gridSVG** package for converting ggplot2 plots to SVG.

[See the demo.](http://plourde.shinyapps.io/svgg_example)

[R Philly presentation](https://plourde.shinyapps.io/svgg_presentation/presentation.Rmd#1)

Installation and Usage
================================
If not already installed, install **devtools**.

```r
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
library(shiny)
library(ggplot2)
library(svgg)

ui <- fluidPage(
    svgOutput('myplot')
)

server <- function(input, output) {
    output$simple_boxplot <- renderSVGG({
        p <- ggplot(diamonds, 
                aes(cut, carat, outlier.labels=I(color), group=cut)) + 
            geom_boxplot()
        p
    }, popover=list(title='My Plot'))
}

shinyApp(ui=ui, server=server)
```
