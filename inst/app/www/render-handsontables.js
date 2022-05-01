// based on https://github.com/jrowen/rhandsontable/issues/387
$(document).on('show.bs.modal', function() {
  console.log("here1212");
  // find all handsontable objects widgets in application
  let v =
    document.querySelectorAll(".rhandsontable.html-widget.html-widget-output");
  // if no objects found then exit here
  if (v.length > 0) {return null};
  // re-draw each element
  v.forEach((x) => {
    w = HTMLWidgets.find(`#{x.id}`);
    w.render();
  })
});
