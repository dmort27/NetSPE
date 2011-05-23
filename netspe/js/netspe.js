$(document).ready( function() {

    var enclose = function(l, r, s) {
        return l + s + r;
    };
    
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