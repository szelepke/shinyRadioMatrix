// allow selecting radio buttons by clicking on the cell they are included in
$('td').click(function() {
  var $cell = $(this);
  $cell.children('input').prop("checked", true);
});
