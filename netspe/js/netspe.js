// Encloses a string s in delimiters l and r.
var enclose = function(l, r, s) {
    return l + s + r;
};

// Strips the first and last characters from x.
var unenclose = function(x) {
    return x.slice(1, x.length - 1);
};

var mapAddBoundaries = function(xs) {
    return $.map(xs, function(v, i) {
        return enclose('#', '#', v);
    });
};

var phoneticFonts = 'Charis SIL, Gentium Plus, Doulos SIL, Monaco, Lucida Grande, Times New Roman, DejaVu Serif, DejaVu Sans, serif, sans';

// converts a textarea control into a sortable list.
var textareaToSortableList = function(id) {

    var newList = $('<ul />').attr('id', id).addClass('sortable-parent');

    $.each($('#' + id).val().split('\n'), function (i, x) {
        newList.append($('<li />').append(x));
    });

    console.log('newList='+newList);
    
    $('#' + id).replaceWith(newList);
    $('#' + id).sortable().disableSelection();
    $('#' + id + ' li').addClass('ui-state-default phonetic sortable-item');

};

// converts a sortable list into a textarea control.
var sortableListToTextarea = function(id) {
    var ruleList = [];
    var qid = '#' + id;
    $(qid + ' li').each(function(i) {
        ruleList[i] = $(this).text();
    });
    $(qid).replaceWith(
        $('<textarea />')
            .attr('id', id)
            .text(ruleList.join('\n'))
            .addClass('phonetic'));
    $(qid).css({'font-family': phoneticFonts,
                'font-size': '0.8em'});
};

$(document).ready( function() {

    // Applies classes and styles to various parts of the document.
    $('textarea').css('font-family', phoneticFonts)
        .css('font-size', '0.8em');
    $("#controls").addClass("ui-widget phonetic");
    $(".control textarea").addClass("ui-widget phonetic ui-helper-clearfix")
        .css('padding', '2px 2px 2px 2px');
    $("h2").addClass("ui-widget ui-widget-header ui-corner-tl ui-corner-tr phonetic");
    
    // This function is called to format the derivation after it has
        // been loaded from the server. 
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
            
        var srepsTR = $('<tr/>');

        var srepsCalc = $('#main table tr:last-child')
            .children().map( function() {
                return unenclose($(this).text());
            });

        $.each($('#sreptext').val().split('\n'), function(i, v) {
            // Applies formatting if the calculated form is different
            // from the expected value.
            if (v.replace(/[-]/gi, '') === srepsCalc[i]) { // remove any hyphens
                srepsTR.append($('<td></td>').append(v));
            } else {
                srepsTR.append($('<td></td>').append($('<b></b>').append(v)));
            }
        });
        $('#main table').append(srepsTR);

        // Get rules (with fillers).
        var rules = ["UR"].concat($('#ruletext').val().split('\n'), ["SR (predicted)", "SR (observed)"]);

        // Append rule to each table row
        $('#derivation-container tr').each( function(i, tr) {
            $(tr).append($('<td />').text(rules[i]));
        });
    };
    
    var evaluate = function() {

        $.each($('#controls ul.sortable'), function() {
            sortableListToTextarea($(this).attr('id'));
        });
        
        var dt = { ruletext: $('#ruletext').val(),
	           reptext: $('#reptext').val().replace(/[-]/gi, '') };
        $('#derivation-container')
            .load('/cgi-bin/netspe/derivation.cgi', dt,
                  function(){
                      formatDerivation();
                  });
    };

    // Handles a file which is uploaded.
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

                        var fixedFields =
                            { 'withFixedURs': 'reptext',
                              'withFixedSRs': 'sreptext',
                              'withFixedRules': 'ruletext'};
                        
                        
                        var json = $.parseJSON(e.target.result);

                        console.log(json);

                        $.each(dataFields, function(k1, k2) {
                            var qid = '#' + k2;
                            var xs;
                            if (k1 != 'theRules' && json['withAutoBounds']) {
                                xs = mapAddBoundaries(json[k1]);
                            } else {
                                xs = json[k1];
                            }
                            $(qid).val(xs.join("\n"));
                        });

                        $.each(metadataFields, function(k1, k2) {
                            var qid = '#' + k2 + ' span';
                            $(qid).text(json[k1]);
                        });

                        $.each(fixedFields, function(k1, k2) {
                            var qid = '#' + k2;
                            if (json[k1]) {
                                $(qid).attr('readonly', 'readonly');
                            };
                        });
                        
                        evaluate();
                    };
            })(f);

            reader.readAsText(f);
        }
    };

    
    $('#evaluate').button();
    $('#evaluate').click(evaluate);

    $("#files").fileinput({
        buttonText: 'Choose Puzzle...',
        inputText: 'None'
    });
    $('#files').bind("change", handleFileSelect);
    
    $('div.control').dblclick( function() {
        var list = $(this).children().first().next();
        var id = list.attr('id');
        if (list.hasClass('sortable-parent')) {
            sortableListToTextarea(id);
        } else {
            textareaToSortableList(id);
        }
    });
    
    formatDerivation();
    
    $("#alphabuttons button").button();
    $("#alphabuttons button").click(function() {
        var ruleText = $("#ruletext").val();
        var varName = $(this).text();
        var pos = $("#ruletext").getSelection();
        $("#ruletext").val(ruleText.slice(0, pos.start) + varName + ruleText.slice(pos.start + 1, ruleText.length)); 
    });

    $('button span').addClass('phonetic');
    $('.fileinput-wrapper').addClass('phonetic');
    $('.fileinput-wrapper span').addClass('phonetic');
    
});