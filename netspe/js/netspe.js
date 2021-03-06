// Set path to CGI programs.
var cgiPath = '/cgi-bin/netspe/';

// Encloses a string s in delimiters l and r.
var enclose = function(l, r, s) {
    return l + s + r;
};

// Strips the first and last characters from x.
var unenclose = function(x) {
    return x.slice(1, x.length - 1);
};

// Add word boundary markers (#) to all forms in a set.
var mapAddBoundaries = function(xs) {
    return $.map(xs, function(v, i) {
        var match = /([^ ]+)( +['‘].+['’])/.exec(v);
        if (match) {
            return (enclose('#', '#', match[1]) + match[2]);
        } else {
            return enclose('#', '#', v);
        }
    });
};

// Parses a form/gloss pair and returns an object with the fields form
// and gloss.
var parseFormGloss = function(x) {
    var match = /([^ ]+)(\s+['‘].+['’]|)/.exec(x);
    var result = new Object;
    result.form = match[1];
    result.gloss = match[2];
    return result;
};


// Converts a textarea control into a sortable list.
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

// Converts a sortable list into a textarea control.
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
            .addClass('phonetic ui-widget ui-helper-clearfix'));
    $(qid).addClass('phonetic');
};

// This function is called to format the derivation after it has
// been loaded from the server. 
var formatDerivation = function() {
    $("tr:first").addClass("ur ui-priority-primary");
    $("tr:last").addClass("sr ui-priority-primary");
    $("tr.ur td").each(function(i) {
        $(this).text(enclose('/', '/', $(this).text()));
    });

    $("tr.sr td").each(function(i) {
        $(this).text(enclose('[', ']', $(this).text()));
    });

    $("td").each(function(i) {
        if ($(this).text()==='---') { $(this).text('—'); }
    });
    
    // Build a row of intended SRs.
    
    var srepsTR = $('<tr/>');
    var srepsCalc = $('#main table tr:last-child')
        .children().map( function() {
            return unenclose($(this).text());
        });

    $.each($('#sreptext').val().split('\n'), function(i, v) {
        // Applies formatting if the calculated form is different
        // from the expected value.
        if (v.replace(/[-]/gi, '').replace(/\s+.+/gi, '') === srepsCalc[i]) { // remove any hyphens and glosses
            srepsTR.append($('<td></td>').append(v));
        } else {
            srepsTR.append($('<td></td>').append($('<b></b>').append(v)));
        }
    });
    $('#main table').append(srepsTR).addClass('phonetic');

    $('#main table tr:last').css({'border-bottom': '2px solid black'});
    
    var rules = $('#ruletext').val().split('\n');
    var urDesc = ($("#reptext").val() != "") ? "UR" : "";
    var sRepPredDesc = ($("#reptext").val() != "" && $("#ruletext").val() != "") ? "SR (predicted)" : "";
    var sRepObsDesc = ($("#sreptext").val() != "") ? "SR (observed)" : "";

    var descriptions = [urDesc].concat(rules, [sRepPredDesc, sRepObsDesc]);

    // Append rule to each table row
    $('#derivation-container tr').each( function(i, tr) {
        $(tr).append($('<td />').text(descriptions[i]));
    });
    $('#derivation table tr').addClass('phonetic derivation');
    $('#derivation table').addClass('ui-widget phonetic derivation');
    $('#derivation td').addClass('derivation');

};

var evaluate = function() {
    
    $.each($('#controls ul.sortable'), function() {
        sortableListToTextarea($(this).attr('id'));
    });
    
    var dt = { ruletext: $('#ruletext').val(),
	       reptext: $('#reptext').val().replace(/[-]/gi, '').replace(/ +['‘].+['’]$/gi, '')
             };
    $('#derivation-container')
        .load(cgiPath + 'derivation.cgi', dt,
              function() {
                  formatDerivation();
              });
};

// Handles a file which is uploaded.
var handleFileSelect = function(evt) {
    console.log("File was changed.");
    var files = evt.target.files;
    
    for (var i = 0, f; f = files[i]; i++) {
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

// Main function.
$(document).ready( function() {

    // Applies classes and styles to various parts of the document.
    $('textarea').addClass('phonetic')
        .css({'font-size': '0.9em'});
    $("#controls").addClass("ui-widget");
    $(".control textarea").addClass("ui-widget ui-helper-clearfix")
        .css('padding', '2px 2px 2px 2px');
    $("h2").addClass("ui-widget ui-widget-header ui-corner-tl ui-corner-tr fancy");
    
    // Format and attach a handler to the 'evaluate' button.
    $('#evaluate').button();
    $('#evaluate').click(evaluate);

    var $validateDialog = $('<div />')
        .html('<div id="validate-dialog"><div>')
        .dialog({
            autoOpen: false,
            title: 'Rule Validation'
        })
        .addClass("fancy");

    // format and attach a handler to the 'validate' button.
    $('#validate').button();
    $('#validate').click(function() {
        var url = cgiPath + "lint.cgi";
        var dt = {
            'ruletext': $('#ruletext').val(),
            'reptext': $('#reptext').val().replace(/\s+.+$/gi, ''),
            'sreptext': $('#sreptext').val().replace(/\s+.+$/gi, '')
        };
        var success = function(data) {
            var msg = "";
            $.each(data.rules, function(i, v) {
                if (!v.valid) {
                    msg += "<p>Rule <b>" + (i+1) + "</b> “" + v.item + "” is invalid.</p>";
                }
            });
            if (msg === '') {
                $('#validate-dialog').html('The rules are alright.');
            } else {
                $('#validate-dialog').html(msg);
            }
            $validateDialog.dialog('open');
            return false;
        };
        $.getJSON( url, dt, success );
        return false;
    });

    // Instantiate file selection widget.
    $("#files").fileinput({
        buttonText: 'Select Puzzle...',
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

    $('button span').addClass('fancy');
    $('.fileinput-wrapper').addClass('fancy');
    $('.fileinput-wrapper span').addClass('fancy');
    
});