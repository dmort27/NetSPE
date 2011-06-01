var enclose = function(l, r, s) {
    return l + s + r;
};

var phoneticFonts = 'Charis SIL, Gentium Plus, Doulos SIL, Times New Roman, DejaVu Serif, DejaVu Sans, serif, sans';

var textareaToSortableList = function(id) {

    var newList = $('<ul />').attr('id', id).addClass('sortable');

    $.each($('#' + id).val().split('\n'), function (i, x) {
        newList.append($('<li />').append(x));
    });

    console.log('newList='+newList);
    
    $('#' + id).replaceWith(newList);
    $('#' + id).sortable().disableSelection();
    $('#' + id + ' li').addClass('ui-state-default')
        .css('font-family', phoneticFonts)
        .css('font-size', '0.8em');

};

var sortableListToTextarea = function(id) {
    var ruleList = [];
    $('#' + id + ' li').each(function(i) {
        ruleList[i] = $(this).text();
    });
    $('#' + id).replaceWith(
        $('<textarea />')
            .attr('id', id)
            .text(ruleList.join('\n'))
            .addClass('phonetic')
            .css('font-family', phoneticFonts)
            .css('font-size', '0.8em')
    );
};

$(document).ready( function() {
    
    $('textarea').css('font-family', 'Charis SIL, Gentium Plus, Doulos SIL, Times New Roman, DejaVu Serif, DejaVu Sans, serif, sans')
        .css('font-size', '0.8em');
    $("#main").addClass("ui-widget ui-corner-tl ui-corner-tr phonetic");
    $("#controls").addClass("ui-widget phonetic");
    $(".box").addClass("ui-widget phonetic");
    $(".control").addClass("ui-widget ui-helper-cleafix")
        .css('padding', '2px 2px 2px 2px');
    $(".control textarea").addClass("ui-widget phonetic ui-helper-clearfix")
        .css('padding', '2px 2px 2px 2px');
    $("h2").addClass("ui-widget ui-widget-header ui-corner-tl ui-corner-tr phonetic")
        .css('text-align', 'center')
        .css('padding', '2px 2px 2px 2px');
    $('h1').addClass('ui-widget ui-widget-header  ui-corner-tl ui-corner-tr ui-corner-bl ui-corner-br phonetic');
    
    var formatDerivation = function() {
        $("tr:first").addClass("ur");
        $("tr:last").addClass("sr");
        $("tr.ur td").each(function(i) {
            $(this).text(enclose('/', '/', $(this).text()));
        });

        $("tr.sr td").each(function(i) {
            $(this).text(enclose('[', ']', $(this).text()));
        });

        $("td").each(function(i) {
            if ($(this).text()==='---') { $(this).text('—'); }
        });
        
        $('#derivation table').addClass("ui-widget phonetic")
            .css('font-size', '1em');

        // Build a row of intended SRs.

        unenclose = function(x) {
            return x.slice(1, x.length - 1);
        };
        
        var srepsTR = $('<tr/>');
        var srepsCalc = $('#main table tr:last-child').children().map(function() { return unenclose($(this).text()); });
        $.each($('#sreptext').val().split('\n'), function(i, v) {
            if (v === srepsCalc[i]) {
                srepsTR.append($('<td></td>').append(v));
            } else {
                srepsTR.append($('<td></td>').append($('<b></b>').append(v)));
            }
        });
        $('#main table').append(srepsTR);
    };
    
    var evaluate = function() {

        $.each($('#controls ul.sortable'), function() {
            sortableListToTextarea($(this).attr('id'));
        });
        
        var dt = { ruletext: $('#ruletext').val(),
	           reptext: $('#reptext').val() };
        $('#derivation-container')
            .load('/cgi-bin/netspe/derivation.cgi', dt,
                  function(){
                      formatDerivation();
                  });
    };

    var handleFileSelect = function(evt) {
        console.log("File was changed.");
            var files = evt.target.files;
        
        for (var i = 0, f; f = files[i]; i++) {
            if (!f.type.match(".*")) {
                console.log("Did not match!");
                continue;
            }

            var reader = new FileReader();
            
            reader.onload = (function(theFile) {
                return function(e) {
                    var dataFields =
                        { 'theURs': 'reptext',
                          'theSRs': 'sreptext',
                              'theRules': 'ruletext' };
                    
                    var metadataFields =
                        { 'theLanguage': 'data-language',
                          'theFamily': 'data-family',
                          'theSource': 'data-source' };
                    
                    var json = $.parseJSON(e.target.result);

                    console.log(json);

                    $.each(dataFields, function(k1, k2) {
                        var qid = '#' + k2;
                        $(qid).val(json[k1].join("\n"));
                    });

                    $.each(metadataFields, function(k1, k2) {
                        var qid = '#' + k2 + ' span';
                        $(qid).text(json[k1]);
                    });
                    
                    evaluate();
                };
            })(f);

            reader.readAsText(f);
        }
    };

    
    $('#evaluate').button();
    $('#evaluate').click(evaluate);
    
    $('#files').bind("change", handleFileSelect);
    
    $('div.control').dblclick( function() {
        console.log('clicked ' + $(this));
        var list = $(this).children().first().next();
        var id = list.attr('id');
        if (list.hasClass('sortable')) {
            sortableListToTextarea(id);
        } else {
            textareaToSortableList(id);
        }
    });

    formatDerivation();

});