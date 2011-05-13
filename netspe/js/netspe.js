$(document).ready( function() {

var evaluate = function() {
    var dt = { ruletext: $('#ruletext').val(),
	       reptext: $('#reptext').val() };
    $('#derivation').load('/cgi-bin/netspe/derivation.cgi');
};

$('#evaluate').button();
$('#evaluate').click(evaluate);

});