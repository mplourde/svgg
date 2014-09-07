.onLoad <- function(libname, pkgname) {
    addResourcePath('roow', system.file('www', package='roow'))
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
}
