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

download_svg = function(svg_output_id, download) {
    var svg_container = $('#' + svg_output_id)
    var svg_html = svg_container.html();

    var svg_el = $('#' + svg_output_id + ' > svg');
    var height = svg_el.attr('height');
    var width = svg_el.attr('width');

    var imgsrc = 'data:image/svg+xml;base64,' + btoa(svg_html);

    var canvas = $('#' + svg_output_id + '__canvas_');
    canvas.attr('height', height);
    canvas.attr('width', width);
    var context = canvas[0].getContext('2d');

    var image = new Image;
    image.src = imgsrc;
    image.onload = function() {
        context.drawImage(image, 0, 0);
        var canvasdata = canvas[0].toDataURL('image/png');
        var a = document.createElement('a');
        a.download = download;
        a.href = canvasdata;
        a.click();
    };
};


set_alpha = function(obj, alpha) {
    console.log('bacon');
    $(obj).css('fill-opacity', alpha);
};

