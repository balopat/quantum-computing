 
ArrayList points =new ArrayList();
float r = 0.58;
float im = 0.57;

 
void setup() {
  size(640, 200);
  stroke(255);
 
}

void draw() {
  background(102);
  drawPoints();
}

void mouseReleased(){
    if ( mouseButton == LEFT)
    {
      points.add(new Point(mouseX, mouseY));
    } else{
      if (mouseButton == RIGHT) {
        for (int i = 0; i<points.size(); i++){
          Point p = (Point) points.get(i);
          p.y = r * p.x - im * p.y;
          p.x = r * p.y + p.x * im;
          point(p.x,p.y);
        }
      }
    }
}

void drawPoints(){
   for (int i = 0; i<points.size(); i++){
          Point p = (Point) points.get(i);
          point(p.x,p.y);
          if (i>0) {
            Point p2 = (Point) points.get(i-1);
            line(p.x,p.y, p2.x, p2.y);
          }
        }
}

class Point{
  public float x;
  public float y;
   Point(float x, float y){
     this.x = x;
     this.y =y;
   }
}
