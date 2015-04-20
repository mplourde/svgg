var svgOutputBinding = new Shiny.OutputBinding(); 
$.extend(svgOutputBinding, {
    find: function(scope) {
        return $(scope).find('.shiny-svg-output');
    },
    renderValue: function(el, data) {
        var $el = $(el)
        $el.html(data.svg_html);

        var el_id = $el.attr('id');

        if (data.show_popover) {
            var $svg_container = $('#' + el_id + ' .svg_container');
            console.log($svg_container);
            var popover_opts = data.popover_opts

            if (data.download_btn) {
                data.popover_opts.html = true;
                var dl_btn = '<div class="text-center">'
                  + '<button type="button" onclick="download_svg(\'' + el_id + '\', \'' + data.save_as + '\')" '
                        + 'class="btn action-button">Download</button>'
                  + '</div>';
                data.popover_opts.content = data.popover_opts.content + dl_btn
            }

            $svg_container.popover(popover_opts);
        }

        if (data.show_tooltips) {
            var $tooltip_els = $('#' + el_id + ' svg [data-original-title]')
            $tooltip_els.tooltip(data.tooltip_opts)
        }
    }
});
Shiny.outputBindings.register(svgOutputBinding, 'shiny.svgOutput');

//var Debouncer = function(target, func, delayMs) {
//    this.target = target;
//    this.func = func;
//    this.delayMs = delayMs;
//
//    this.timerId = null;
//    this.args = null;
//};
//
//$('.shiny-svg-output').each(function() {
//  if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
//    initialValues['.clientdata_output_' + this.id + '_width'] = this.offsetWidth;
//    initialValues['.clientdata_output_' + this.id + '_height'] = this.offsetHeight;
//  }
//});
//
//function doSendSVGSize() {
//  $('.shiny-svg-output').each(function() {
//    if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
//      inputs.setInput('.clientdata_output_' + this.id + '_width', this.offsetWidth);
//      inputs.setInput('.clientdata_output_' + this.id + '_height', this.offsetHeight);
//    }
//  });
//  $('.shiny-bound-output').each(function() {
//    $(this).data('shiny-output-binding').onResize();
//  });
//}
//var sendSVGSizeDebouncer = new Debouncer(null, doSendSVGSize, 0);
//function sendSVGSize() {
//  sendSVGSizeDebouncer.normalCall();
//}


download_svg = function(svg_output_id, download) {
    var svg_html = $('#' + svg_output_id + ' .svg_container').html()
    var imgsrc = 'data:image/svg+xml;base64,' + btoa(svg_html);

    var $svg_el = $('#' + svg_output_id + ' svg');
    var height = $svg_el.attr('height');
    var width = $svg_el.attr('width');

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
    $(obj).css('fill-opacity', alpha);
};

