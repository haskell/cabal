function toggle_con(obj) {
  var x = $(obj.currentTarget);
  x.toggleClass("closed");
  x.parent().siblings().fadeToggle();
  x.siblings().fadeToggle();
}

function toggle_lab(obj) {
  var x = $(obj.currentTarget);
  x.toggleClass("closed");
  x.siblings().fadeToggle();
  }

$(function () {
  $("table.recordList>tbody>tr>th.ix").click(toggle_lab);
  $("table.tallRecord>tbody>tr>th.label").click(toggle_lab);
  $("table.tallList>tbody>tr>th.ix").click(toggle_lab);

  $("table.recordList>tbody>tr>th.con").click(toggle_con);
  $("table.tallList>tbody>tr>th.con").click(toggle_con);
  $("table.wideList>tbody>tr>th.con").click(toggle_con);
  $("table.tallRecord>tbody>tr>th.con").click(toggle_con);
})
