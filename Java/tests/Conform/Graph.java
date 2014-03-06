// Traffic simulation,  Kelly Liu, Feb, 1996
//Tel: (508) 647-7662 (o)   (508) 875-2973

import java.io.InputStream;
import java.net.URL;

import java.util.*;
import java.awt.*;
import java.applet.Applet;
class Node {
    double x;
    double y;
    int lane;
    int road;
    double dx;
    double dy;
    String lbl;
    int carW;
    int carL;
    double carWaiting;
}

class ChangeLight implements Runnable {
    int signal;
    int pauss, redpauss, greenpauss;
    Thread lighter;

    ChangeLight(){
       signal=1;
       redpauss=6000;
       greenpauss=6000;
    }

    public void run() {
        signal=1;
	while (true) {
	    if (signal==1){
                      signal=0;
                      pauss=greenpauss;
            } else {
                      signal=1;
                      pauss=redpauss;
            }
	    try {
		Thread.sleep(pauss);
	    } catch (InterruptedException e) {
		break;
	    }
	}
    }

    public void start() {
	lighter = new Thread(this);
	lighter.start();
    }

    public void stop() {
	lighter.stop();
    }
}  

class CalFlow implements Runnable {
    int carnum, count;
    double carwt;
    int pauss;
    double time0, time1, timelap;
    double carflow[] = new double[40];

    Thread flow;

    CalFlow(){
      carnum=0;
      carwt=0;
      pauss=2000;
      time0=0;
      time1=0;
      for (int k=0; k<40; k++)
         carflow[k]=0;
      count=0;
    }

    public void run() { 
	while (true) {  
                 time1= System.currentTimeMillis();
                 timelap=time1-time0;
                 if (timelap >50) 
	     carflow[count]= ((double)(carnum)/timelap)*1000; 
                 count=(count+1)%40;

	    try {
		Thread.sleep(pauss);
	    } catch (InterruptedException e) {
		break;
	    }
	}
    }
    public void start() {
	flow = new Thread(this);
	flow.start();
    }
    public void stop() {
	flow.stop();
    }
}  


class GraphPanel extends Applet implements Runnable {
    Graph graph;
    int nnodes;
    int lghtOrStp;
    Node nodes[] = new Node[100];
    ChangeLight  light[] = new ChangeLight[5];
    CalFlow carpermin[] = new CalFlow[5];
    Thread relaxer, flow;
    int brgflag[] = new int[5];
    double speed=10;
    int carwidth=6, carlength=9;
    int xpos[]= new int[5];
    int ypos=200;
    int brgright[] = new int[5];
    int brgleft[] = new int[5]; 
    int brgtop =ypos+ carlength; 
    int brgbottom=ypos- carlength;
    int rdleft[]=new int[5];
    int rdright[] = new int[5];
    int rdtop= ypos+ carwidth, rdbottom= ypos- carwidth;

    GraphPanel(Graph graph) {


              lghtOrStp=1;                                    //stop :0, light: 1
	this.graph = graph;
              for (int i=0; i<5; i++)
              {
               light[i]= new ChangeLight();
               carpermin[i]= new CalFlow();
           
               xpos[i]=150*(i+1);
               brgright[i]=xpos[i]- carlength;
               brgleft[i]=xpos[i]+ carlength;
               brgflag[i]=0;
                }
               for(int k=1; k<4; k++){
                  rdleft[k]= xpos[k-1]- carwidth;
                  rdright[k]= xpos[k-1]+ carwidth; 
               }
              rdleft[0]=0;
              rdright[0]=0;
      }

    int findNode(String lbl) {
	for (int i = 0 ; i < nnodes ; i++) {
	    if (nodes[i].lbl.equals(lbl)) {
		return i;
	    }
	}
	return addNode(lbl);
    }
    int addNode(String lbl) {
               int temp;
	 Node n = new Node();
               temp = (int)(5*Math.random());
               if (temp==0||temp==4){
                     n.x = 480 + 210*Math.random();
                     n.y= ypos;
                     n.carW=carlength;
                     n.carL=carwidth;
                }
               else{
                     n.x= xpos[temp-1];
                     n.y= 10+100*Math.random();
                     n.carW=carwidth;
                     n.carL=carlength;
                }
//              temp=(int)(3*Math.random());                   // three lanes
//	    n.y = 150+50*temp;
               if (temp==4)
                   temp=0;
               n.road=temp;
 	 n.lbl = lbl;
               n.carWaiting=-1;
	 nodes[nnodes] = n;
	 return nnodes++;
    }

      public void run() {
              for (int j=0; j<5; j++)
                  light[j].signal=1;
              
              flow = new Thread(carpermin[0]);   
              carpermin[0].time0 = System.currentTimeMillis();
              carpermin[0].carnum=0;
              flow.start();
	while (true) {
	    relax();
	    try {
		Thread.sleep(50);
	    } catch (InterruptedException e) {
		break;
	    }
	}
    }

    synchronized void relax() {
	for (int i = 0 ; i < nnodes; i++) {	 
                  if (nodes[i].road==0){
                    nodes[i].dx = -speed*Math.random();
	      nodes[i].dy = 2*Math.random()-1;
                   }
                  else{
                    nodes[i].dy = speed*Math.random();
	      nodes[i].dx = 2*Math.random()-1;
                  }
	}
	for (int i = 0 ; i < nnodes ; i++) {
	    Node n1 = nodes[i];
	    double dx = 0;
	    double dy = 0;
	    for (int j = 0 ; j < nnodes ; j++) {
                            Node n2 = nodes[j];
		if (i == j||n1.road!=n2.road) {
		    continue;
		}
                            double vx;
		if(n1.road==0)
		    vx = n1.x - n2.x;
                            else
                                vx= n2.y-n1.y;
                            if (vx<0)
                                 continue;
                            double len=vx;
                             
                            if( len<(n2.carW+n2.carL)){
                                if (n1.carWaiting<0) 
                                  n1.carWaiting= System.currentTimeMillis();
                               
                                 if(n1.road==0)
                                      n1.dx=0;
                                 else
                                      n1.dy=0;
                              }                                 
	    }
	
	}         
//move a car
	 Dimension d = size();
               double temp;
              	 for (int i = 0 ; i < nnodes ; i++) {
	        Node n = nodes[i];
                       if(n.road==0){ 
                          temp=n.x;
	            n.x += Math.max(-10, Math.min(10, n.dx));
                          for (int k=0; k<3; k++){
                              if ((n.x<brgleft[k]&&n.x>brgright[k])&&brgflag[k]==1){  
                                if(temp> brgleft[k] ||temp<brgright[k])
                                    n.x=temp;
                              }
                             else if ((n.x< brgleft[k] &&n.x>brgright[k])&&brgflag[k]==0)
                                if (lghtOrStp==0)
                                    brgflag[k]=1;
                                else{
                                    if (light[k].signal==0)
                                        brgflag[k]=1;
                                    else
                                        n.x=temp;
                                }
                             else if(temp< brgleft[k] &&temp>brgright[k])
                                 brgflag[k]=0;
		if (n.x < 0) {
		    n.x = d.width-10*Math.random();
                                carpermin[0].carnum=carpermin[0].carnum+1;
		} else if (n.x > d.width) {
		    n.x = d.width-10*Math.random();
		}
                             if (n.x!=temp&&n.carWaiting==-1){
                                   carpermin[0].carwt+= System.currentTimeMillis()-
n.carWaiting;
                                   n.carWaiting=-1;
                             }
                            }
                      }
                    else{
                          temp=n.y;
	            n.y += Math.max(-10, Math.min(10, n.dy));
                          if ((n.y<brgtop&&n.y>brgbottom)&&brgflag[n.road-1]==1){  
                             if(temp> brgtop ||temp<brgbottom)
                                n.y=temp;     
                           }
                          else if ((n.y< brgtop &&n.y>brgbottom)&&brgflag[n.road-1]==0)
                                    if (lghtOrStp==0)
                                         brgflag[n.road-1]=1;
                                    else{
                                         if (light[n.road-1].signal==1)
                                             brgflag[n.road-1]=1; 
                                         else
                                             n.y=temp;
                                      }
                          else if(temp< brgtop &&temp>brgbottom)
                             brgflag[n.road-1]=0;

		if (n.y > d.height||n.y<0) {
		    n.y = 10*Math.random();
                                  carpermin[0].carnum=carpermin[0].carnum+1;
		}
                      }
	}
	repaint();
    }

    Node pick;
    double pickoldx, pickoldy;
    Image offscreen;
    Dimension offscreensize;
    Graphics offgraphics;

    final Color selectColor = Color.pink;
    final Color edgeColor = Color.black;
    final Color nodeColor = new Color(250, 220, 100);

    public void paintNode(Graphics g, Node n) {
	int x = (int)n.x;
	int y = (int)n.y;
	g.setColor((n==pick) ? selectColor : nodeColor); 
              int w= n.carW;                             
              int h=n.carL;
	g.fillRect(x - w/2, y - h / 2, w, h);
	g.setColor(Color.black);
	g.drawRect(x - w/2, y - h / 2, w-1, h-1);
              g.drawString(".", x-w/2+2, y+h/2-2);
    }

    public void paintRoad(Graphics g){
          Dimension d = size();
          g.setColor(Color.gray);
          for(int k=1; k<4; k++){
               g.drawLine(rdleft[k], 0, rdleft[k], rdbottom);
               g.drawLine(rdleft[k], rdtop, rdleft[k], d.height);
               g.drawLine(rdright[k], 0, rdright[k], rdbottom);
               g.drawLine(rdright[k], rdtop, rdright[k], d.height);
               g.drawLine(rdright[k-1], rdtop, rdleft[k], rdtop);
               g.drawLine(rdright[k-1], rdbottom, rdleft[k], rdbottom);
              }
          g.drawLine(rdright[3], rdbottom, d.width, rdbottom);
          g.drawLine(rdright[3], rdtop, d.width, rdtop);   
    }

public void paintLghtPeriod(Graphics g){
     Font warnFont, dispFont, stopFont;
     warnFont=new Font("Arial", Font.BOLD, 20);
     dispFont=new Font("TimesRoman", 0, 12);
     stopFont=new Font("TimesRoman", Font.BOLD, 14);
     Dimension d = size();

     offgraphics.setColor(Color.black);   
              if(lghtOrStp==1){
                 offgraphics.drawString("Traffic Light Period (1: red, 0: green)",  600,50);
                 offgraphics.setColor(Color.red);
                 offgraphics.drawString("red", 714, 50);
                 offgraphics.setColor(Color.green);
                 offgraphics.drawString("green", 747, 50);
                 offgraphics.setColor(Color.black);
                 for(int k=0; k<3; k++){ 
                   int tempred= light[k].redpauss/200, tempgreen= light[k].greenpauss/200;
                   int temp1=rdright[3]+170;    
                   int temp2, temp3, temp4, temp5;
                   if (light[k].signal==0){
                        temp2=temp1+tempred;
                        temp3=30*(k+1)+40;
                        temp4= temp2+tempgreen;
                        temp5=temp3+12;
                   }
                  else{
                        temp2=temp1+tempgreen;
                        temp3=30*(k+1)+40+12;
                        temp4= temp2+tempred;
                        temp5=temp3-12;
                 }
                 offgraphics.drawString("Light " +Integer.toString(k+1), temp1-40, (temp5+temp3)/2+5);
                 while (temp1<d.width){
                     offgraphics.drawLine(temp1, temp3, temp2, temp3);
	       offgraphics.drawLine(temp2, temp5, temp4, temp5);
                     offgraphics.drawLine(temp1, temp3, temp1, temp5);
                     offgraphics.drawLine(temp2, temp3, temp2, temp5);
                     temp1=temp4;
                     temp2=temp1+tempred;
                     temp4=temp2+tempgreen;
                   }
                }
               offgraphics.setColor(Color.lightGray);
               offgraphics.fillRect(rdright[3]+328, 30, 15, 130);
               offgraphics.setColor(Color.gray);
               offgraphics.drawRect(rdright[3]+120, 20, 223, 145);
               offgraphics.setColor(Color.black);
               }
               else{
                 offgraphics.setFont(warnFont);
                 offgraphics.setColor(Color.white);
                 offgraphics.fillOval(rdleft[3]+175, rdtop-160, 70, 70);
                 offgraphics.setColor(Color.red);
                 offgraphics.fillOval(rdleft[3]+180, rdtop-155, 60, 60);
                 offgraphics.setColor(Color.white);
                 offgraphics.drawString("STOP", rdleft[3]+183, rdtop-116);
                 offgraphics.setColor(Color.black);
                 offgraphics.setFont(dispFont);
                 offgraphics.drawString("(California)", rdleft[3]+185, rdtop-50);
               }
}

public void paintLights(Graphics g){
        Font dispFont, stopFont;
        dispFont=new Font("TimesRoman", 0, 12);
        stopFont=new Font("TimesRoman", Font.BOLD, 14);
 
        g.setFont(dispFont);
        int lightwidth=15;
        for(int k=1; k<4; k++){
           if(lghtOrStp==0){
             g.setColor(Color.red);
             g.fillOval(rdleft[k]-18, rdtop+4, lightwidth, lightwidth); 
             g.setColor(Color.white);
             g.setFont(stopFont);
             g.drawString("S", rdleft[k]-14, rdtop+16);
             g.setFont(dispFont);
           }
          else{ 
             g.setColor(Color.black);
             g.fillRect(rdleft[k]-18, rdtop+4, lightwidth-2, lightwidth-2); 
             g.setColor(light[k-1].signal==1 ? Color.red : Color.green);
             g.fillOval(rdleft[k]-7, rdtop+6, 6, 9); 
             g.setColor(light[k-1].signal==1 ? Color.green : Color.red);
             g.fillOval(rdleft[k]-16, rdtop+2, 9, 6); 
             g.setColor(Color.black);

             g.drawString("Light "+k, rdleft[k]-58, rdtop+17); 
           }
        }    
}

public void paintAxies(Graphics g){
              int temp1=610;
              int temp2=350;
              int temp3=temp1+160;
              int temp4= temp2-80;
              offgraphics.setColor(Color.gray);
              offgraphics.drawRect(rdright[3]+120, temp4-35, 220, 130);
              offgraphics.setColor(Color.black);
              offgraphics.drawLine(temp1, temp2, temp3, temp2);
              offgraphics.drawLine(temp3, temp2, temp3-10, temp2-2);
              offgraphics.drawLine(temp3, temp2, temp3-10, temp2+2);
              offgraphics.drawLine(temp1, temp2, temp1, temp4);
              offgraphics.drawLine(temp1, temp4, temp1-2, temp4+10);
              offgraphics.drawLine(temp1, temp4, temp1+2, temp4+10);
              for (int k=1; k<4; k++){
                int grid=20*k;
                offgraphics.drawLine(temp1, temp2- grid, temp1+5, temp2- grid);
                offgraphics.drawString(k+".0", temp1- 20, temp2- grid+5);
               }
              offgraphics.drawString("Time", temp3-10, temp2-10);
              offgraphics.drawString("Traffic Flow ", temp1-20, temp2-95);
              offgraphics.drawString("  (cars/sec.)", temp1-20, temp2-82);

              for (int k=0; k<40; k++){
                 if (k>=carpermin[0].count){
                        temp3=1;
                        offgraphics.setColor(Color.gray);
                     }
                 else{
                        temp3=2;
                        offgraphics.setColor(Color.black);
                   }
                 offgraphics.drawRect(k*3+temp1, temp2 - (int)(carpermin[0].carflow[k]*20+1), temp3, temp3);
                 }
   }

    public synchronized void update(Graphics g) {

	Dimension d = size();
	if ((offscreen == null) || (d.width != offscreensize.width) || (d.height != offscreensize.height)) {
	    offscreen = createImage(d.width, d.height);
	    offscreensize = d;
	    offgraphics = offscreen.getGraphics();
	}

	offgraphics.setColor(getBackground());
	offgraphics.fillRect(0, 0, d.width, d.height);
              paintRoad(offgraphics);
//draw lights
              paintLights(offgraphics);
//draw light period
              paintLghtPeriod(offgraphics);
//draw axies for the flow chart
             paintAxies(offgraphics);
//draw cars             
             for (int i = 0 ; i < nnodes ; i++) {
	    paintNode(offgraphics, nodes[i]);
	}               
	g.drawImage(offscreen, 0, 0, null);
    }

    public synchronized boolean mouseDown(Event evt, int x, int y) {
	double bestdist = Double.MAX_VALUE;
	for (int i = 0 ; i < nnodes ; i++) {
	    Node n = nodes[i];
	    double dist = (n.x - x) * (n.x - x) + (n.y - y) * (n.y - y);
	    if (dist < bestdist) {
		pick = n;
                             pickoldx=n.x;
                             pickoldy=n.y;
		bestdist = dist;
	    }
	}
	pick.x = x;
	pick.y = y;
	repaint();
	return true;
    }

    public synchronized boolean mouseDrag(Event evt, int x, int y) {
	pick.x = x;
	pick.y = y;
	repaint();
	return true;
    }

    public synchronized boolean mouseUp(Event evt, int x, int y) {
               boolean insidelane;
	pick.x = x;
	pick.y = y;
               insidelane=false;
	for (int k=1; k<4; k++)
                 if (x>rdleft[k]&&x<rdright[k])
                 {
                      pick.road=k;
                      pick.x=xpos[k-1];
                      pick.carW= carwidth;
                      pick.carL= carlength;
                      insidelane=true;
                  }
                 if (!insidelane&&(y<rdtop&&y>rdbottom))
                  {
                      pick.road=0;
                      pick.y=ypos;
                      pick.carW= carlength;
                      pick.carL= carwidth;
                      
                     }
                  else if(!insidelane)
                   {
                     pick.x=pickoldx;
                     pick.y=pickoldy;
                     }
	pick = null;

	repaint();
	return true;
    }

    public void start() {
	relaxer = new Thread(this);
	relaxer.start();
    }
    public void stop() {
	relaxer.stop();
    }
}

public class Graph extends Applet {
    GraphPanel panel;
    int carnum;
    Thread LightThrd[] = new Thread[3];
    public void init() {
	setLayout(new BorderLayout());

	panel = new GraphPanel(this);
	add("Center", panel);

              carnum = Integer.parseInt(getParameter("carnum"));
              carnum=Math.min(carnum, 70);
              for (int k=0; k<carnum; k++)
                 panel.findNode(Integer.toString(k));
              panel.lghtOrStp=1;
              for (int k=0; k<3; k++){
                LightThrd[k] = new Thread(panel.light[k]);
                panel.light[k].redpauss=(k+1)*1000+3000;
                panel.light[k].greenpauss=panel.light[k].redpauss;
                LightThrd[k].start();
              }
              panel.carpermin[0].time0 = System.currentTimeMillis();
              panel.carpermin[0].carnum=0;

             Panel btpnl=new Panel();
             add("South", btpnl);
               
             btpnl.add(new Button("Start"));
             btpnl.add(new Button("End"));

             btpnl.add(new Button("Stop Sign"));
             btpnl.add(new Button("Traffic Light"));
             btpnl.add(new Button("New Schedule for Lights"));
}

   public boolean action(Event evt, Object arg){
     if (((Button)evt.target).getLabel().equals("Traffic Light"))
       {
        if (panel.lghtOrStp==0){
          panel.lghtOrStp=1;
          for (int k=0; k<3; k++){
            LightThrd[k] = new Thread(panel.light[k]);
            panel.light[k].redpauss=(k+1)*1000+3000;
            panel.light[k].greenpauss=panel.light[k].redpauss;
            LightThrd[k].start();
            panel.carpermin[0].time0 = System.currentTimeMillis();
            panel.carpermin[0].carnum=0;
          }
         }
       }
     else if  (((Button)evt.target).getLabel().equals("Stop Sign"))
      {
         panel.lghtOrStp=0;
         for(int k=0; k<3; k++){
           if (LightThrd[k].isAlive())
              LightThrd[k].stop();
          }
         panel.carpermin[0].time0 = System.currentTimeMillis();
         panel.carpermin[0].carnum=0;
       }
      else if  (((Button)evt.target).getLabel().equals("End"))
      {
           for(int k=0; k<3; k++){
             if (LightThrd[k].isAlive())
               LightThrd[k].stop();
          } 
           panel.stop();
        }
      else if  (((Button)evt.target).getLabel().equals("Start"))
      {
         if(panel.lghtOrStp==1)
           for(int k=0; k<3; k++){
            if (!LightThrd[k].isAlive()){
                 LightThrd[k] = new Thread(panel.light[k]);
                 LightThrd[k].start();
                }
          } 
         if (!panel.relaxer.isAlive())
            panel.start();
        }
     else if  (((Button)evt.target).getLabel().equals("New Schedule for Lights"))
      {
         if (panel.lghtOrStp==1){
           for(int k=0; k<3; k++){
             if (LightThrd[k].isAlive()){
                panel.light[k].redpauss=(int)(Math.random()*6000)+3000;
                panel.light[k].greenpauss=(int)(Math.random()*6000)+3000;
               }
            }
           panel.carpermin[0].time0 = System.currentTimeMillis();
           panel.carpermin[0].carnum=0;
           panel.carpermin[0].carwt=0;

          }
        }
     return true;
    }
public void start() {
	panel.start();
}
    public void stop() {
	panel.stop();
    }
}
