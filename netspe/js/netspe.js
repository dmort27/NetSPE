var enclose = function(l, r, s) {
    return l + s + r;
};

// var lenSharedPrefix = function(s1, s2) {
//     var i, result;
//     for (i=0; i < s1.length; i++) {
//         if (s1[i] == s2[i]) {
//             result = i + 1;
//         }
//     }
//     return result;
// };

// var lenSharedSuffix = function(s1, s2) {
//     var t1 = s1.split('').reverse().join('');
//     var t2 = s2.split('').reverse().join('');
//     var len = lenSharedPrefix(t1, t2);

//     if (len === undefined) {
//         return -1;
//     } else {
//         return len;
//     }
// };

// var parts = function(s1, s2) {
//     var parts;
//     if (s1 === s2) {
//         return [s2, "", ""];
//     } else {
//         var pref = lenSharedPrefix(s1, s2);
//         var suff = lenSharedSuffix(s1, s2);
//         if (suff === -1 && pref == -1) {
//             return ["", s2, ""];
//         } else {
//             if (suff === -1) {
//                 return [s2.slice(0, pref), s2.slice(pref + 1), ""];
//             } else {
//                 return [];
//             }
//         }
//     }
// };

var textareaToSortableList = function(id) {
    var ruleList = $(id).val().split('\n')
        .map(function(r) {
            return '<li class="phonetic">' + r + '</li>';
        }).join('');
    $(id).replaceWith('<ul id="ruletext" class="sortable">' + ruleList + '<ul>');
    $(".sortable").sortable();
    $(".sortable").disableSelection();
    $(".sortable li").addClass("ui-state-default");
};

$(document).ready( function() {

    $("#controls").addClass("ui-widget phonetic");
    $(".box").addClass("ui-widget phonetic");
    $(".box textarea").addClass("ui-widget phonetic");
    $("h2").addClass("ui-widget ui-widget-header phonetic");
    
    var formatDerivation = function() {
        $("tr:first").addClass("ur");
        $("tr:last").addClass("sr");
        $("tr.ur td").each(function(i) {
            $(this).text(enclose('/', '/', $(this).text()));
        });

        $("tr.sr td").each(function(i) {
            $(this).text(enclose('[', ']', $(this).text()));
        });
    };
    
    var evaluate = function() {
        var dt = { ruletext: $('#ruletext').val(),
	           reptext: $('#reptext').val() };
        $('#derivation').load('/cgi-bin/netspe/derivation.cgi', dt, function(){ formatDerivation(); });
    };
    
    $('#evaluate').button();
    $('#evaluate').click(evaluate);
    
});