var sel0 = d3.select("#div1").append("svg").attr("width",300.0).attr("height",300.0).style("background","#eef");
var dat1 = [{x:0.0,y:200.0,width:95.0,height:100.0},{x:100.0,y:280.0,width:95.0,height:20.0},{x:200.0,y:220.0,width:95.0,height:80.0},{x:300.0,y:240.0,width:95.0,height:60.0},{x:400.0,y:180.0,width:95.0,height:120.0}];
sel0.selectAll("rect").data(dat1).enter().append("rect").attr("x",function(d){return d.x;}).attr("y",function(d){return d.y;}).attr("width",function(d){return d.width;}).attr("height",function(d){return d.height;}).style("fill","red");
