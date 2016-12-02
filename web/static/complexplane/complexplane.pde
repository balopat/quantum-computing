int pxSize = 500;
int pxMinX = - pxSize/2;
int pxMinY = - pxSize/2;
int pxMaxX = pxSize/2;
int pxMaxY = pxSize/2;
int trX = pxSize/2;
int trY = pxSize/2;
float pxUnit = 10;
int gridUnit = 5;


ArrayList points;

 void setup() {
   size(500,500);
   points = new ArrayList();
 }

 void draw() {
   translate(pxMaxX, pxMaxY);
   scale(1,-1);

   textFont( createFont("FFScala", 10));

   background(255,255,255);
   drawPoints();
   drawCoordinateSystem();
 }


 void drawCoordinateSystem() {
   stroke(0,0,0);
   fill(0,0,0);
   line(0, pxMinY, 0, pxMaxY);
   line(pxMinX, 0, pxMaxX, 0);
   scale(1,-1);
   for (int i=pxMinX; i < pxMaxX; i+=pxUnit*gridUnit) {
     if (i != 0) {
       line(i,2,i,-2);
       text((int)(i/pxUnit),i-5,15);

       line(2,i,-2,i);
       text((int)(-i/pxUnit),5,i+5);
     }
  }
 }


 void drawPoints() {
   for(int p=0, end=points.size(); p<end; p++) {
   Point pt = (Point) points.get(p);
   pt.draw();
   }
 }

 void mouseClicked() {
   addPoint((mouseX-trX)/pxUnit,(trY-mouseY)/pxUnit);
 }

 ArrayList getPoints() {
    return points;
 }

 Point addPoint(float x, float y) {
   Point pt = new Point(x,y);
   points.add(pt);
   return pt;
 }

 class Point {
   float r,i;
   Point(float x, float y) { this.r=x; this.i=y; }
   void draw() {
     stroke(0,0,0);
     line(r * pxUnit, i * pxUnit,0,0);
     stroke(255,0,0);
     fill(255,0,0);
     ellipse(r * pxUnit,i*pxUnit,2,2);
     scale(1,-1);
     text("["+r+","+i+"]",5+r*pxUnit,5-i*pxUnit);
     scale(1,-1);
 }
 }
