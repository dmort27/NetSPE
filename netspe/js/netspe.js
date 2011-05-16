$(document).ready( function() {

var evaluate = function() {
    var dt = { ruletext: $('#ruletext').val(),
	       reptext: $('#reptext').val() };
    $('#derivation').load('/cgi-bin/netspe/derivation.cgi', dt);
};

$('#evaluate').button();
$('#evaluate').click(evaluate);

});