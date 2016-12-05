console.log("complexplane.js loaded...");


function clearPoints() {
  var pjs = Processing.getInstanceById("complexplane");
  pjs.clearPoints();
};

function multiplyPoints() {
  var pjs = Processing.getInstanceById("complexplane");
   a = pjs.getPoints().toArray()
   for (p in a) { a[p].$self = undefined }
   multiplicationRequest = {
     points: a,
     mulBy: {
       r: parseInt($("#mulR").val()),
       i: parseInt($("#mulI").val())
     }
   }
   $.ajax({
     url: "/mul",
     contentType: "application/json",
     method: "POST",
     data: JSON.stringify(multiplicationRequest),
     success: function(data) {
        console.log(data)
       var points = $.parseJSON(data);
       for (i in points) {
         pjs.addPoint(points[i].r, points[i].i, true);
       }
      }
    })
 }
