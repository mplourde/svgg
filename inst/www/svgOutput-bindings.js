var svgOutputBinding = new Shiny.OutputBinding(); 
$.extend(svgOutputBinding, {
    find: function(scope) {
        return $(scope).find('.shiny-svg-output');
    },
    renderValue: function(el, data) {
        $(el).html(data.svg_html);
    }
});
Shiny.outputBindings.register(svgOutputBinding, 'shiny.svgOutput');
