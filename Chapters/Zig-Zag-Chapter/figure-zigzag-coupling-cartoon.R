tikz("figure-coupling-cartoon.tex",width=6,height=5)
par(bty='n',xaxt='n',yaxt='n',mar=c(1,2,4,2),mfrow=c(2,2),xpd=T)

plot(NA,NA,xlim=c(0,600),ylim=c(0,300),xlab="x",ylab="")
polygon(c(0,0,300,600,600,300),c(0,75,150,75,0,75),density=15)
polygon(c(0,0,300,600,600,300),c(0,75,150,75,0,75)+150,density=15)

x.points<-c(0,75,150,225,300,375,450,525,600)
y.points<-c(75,(150+75/2)-93.5,(150+75/2)-75,(150+75/2)-56.5,150,(150+75/2)-56.5,(150+75/2)-75,(150+75/2)-93.5,75)

backplate<-rgb(225,225,225,3*225/4,maxColorValue=225)

points(x.points,y.points,pch=16,col=backplate,cex=2)
points(x.points,y.points+75,pch=16,col=backplate,cex=2)
points(x.points,y.points+150,pch=16,col=backplate,cex=2)
points(x.points,y.points-75,pch=16,col=backplate,cex=2)

plus<-"$+$"
minus<-"$-$"

text(x.points,y.points,c(plus),cex=2,col=1)
text (x.points,y.points+75,c(minus),cex=2,col=1)
text(x.points,y.points+150,c(plus),cex=2,col=1)
text(x.points,y.points-75,c(minus),cex=2,col=1)

arrows(500,320,500,400,lwd=3,length=0.15)
text(500,350,"$\\mathbf{E}$",pos=4)

arrows(x.points[3],y.points[3],x.points[2]+40,y.points[2]+75,length=0.15,lwd=3)
arrows(x.points[7],y.points[7],x.points[8]-40,y.points[8]+75,length=0.15,lwd=3)

arrows(x.points[3],y.points[3],x.points[2],y.points[3],length=0.15,lwd=3,col=2)
arrows(x.points[7],y.points[7],x.points[8],y.points[7],length=0.15,lwd=3,col=2)
points(c(0,300,600),c(75+75/2,150+75/2,75+75/2),pch=16,col=2)
par(xaxt='s')
title("TE")
###NEXT

par(bty='n',xaxt='n',yaxt='n',mar=c(1,2,4,2))

plot(NA,NA,xlim=c(0,600),ylim=c(0,300),xlab="",ylab="")
polygon(c(0,0,300,600,600,300),c(0,75,150,75,0,75),density=15)
polygon(c(0,0,300,600,600,300),c(0,75,150,75,0,75)+150,density=15)

x.points<-c(0,75,150,225,300,375,450,525,600)
y.points<-c(75,(150+75/2)-93.5,(150+75/2)-75,(150+75/2)-56.5,150,(150+75/2)-56.5,(150+75/2)-75,(150+75/2)-93.5,75)

backplate<-rgb(225,225,225,3*225/4,maxColorValue=225)

points(x.points,y.points,pch=c(NA,16,16,16,NA,16,16,16,NA),col=backplate,cex=2)
points(x.points,y.points+75,pch=c(NA,16,16,16,NA,16,16,16,NA),col=backplate,cex=2)
points(x.points,y.points+150,pch=c(NA,16,16,16,NA,16,16,16,NA),col=backplate,cex=2)
points(x.points,y.points-75,pch=c(NA,16,16,16,NA,16,16,16,NA),col=backplate,cex=2)

plus<-"$+$"
minus<-"$-$"

text(x.points,y.points,c("",plus,plus,plus,"",minus,minus,minus,""),cex=2,col=1)
text (x.points,y.points+75,c("",minus,minus,minus,"",plus,plus,plus,""),cex=2,col=1)
text(x.points,y.points+150,c("",plus,plus,plus,"",minus,minus,minus,""),cex=2,col=1)
text (x.points,y.points-75,c("",minus,minus,minus,"",plus,plus,plus,""),cex=2,col=1)
title("TM")
arrows(450,350,550,350,lwd=3,length=0.15)
text(500,350,"$\\mathbf{E}$",pos=3)

arrows(x.points[3],y.points[3],x.points[2]+40,y.points[2]+75,length=0.15,lwd=3)
arrows(x.points[7],y.points[7]+75,x.points[6]+40,y.points[6],length=0.15,lwd=3)

arrows(x.points[3],y.points[3],x.points[2],y.points[3],length=0.15,lwd=3,col=2)
arrows(x.points[7],y.points[7]+75,x.points[6],y.points[7]+75,length=0.15,lwd=3,col=2)
points(c(0,300,600),c(75+75/2,150+75/2,75+75/2),pch=16,col=2)


###NEXT
par(bty='n',xaxt='s',yaxt='s',mar=c(1,2,2,2))

etm<-function(x){
  
  -4*pi*sin(x)^2/(cos(2*x)-3)
}

ete<-function(x){
  
  -4*pi*sin(x)/(cos(2*x)-3)
}

x=seq(0,2*pi,,100)
plot(NA,NA,xlim=c(0,2*pi),ylim=c(-1.1*pi,1.1*pi),xlab="$x$",ylab="",xaxt='n',yaxt='n')

lines(x,ete(x),lwd=2)
aX <- c(0,pi,2*pi)
aY <- axTicks(2)
labelsX<-c("","","$\\lambda_g$")
axis(1, at = aX, label = labelsX,pos=0)
axis(2, at = c(-3.5,0,3.5), label = c("$+E_x$","0","$-E_x$"),pos=0)
points(c(0,pi,2*pi),c(0,0,0),col=2,pch=16)
arrows(pi/2,0,pi/2,3,length=0.15,lwd=3,col=2)
arrows(3*pi/2,0,3*pi/2,-3,length=0.15,lwd=3,col=2)

###NEXT

plot(NA,NA,xlim=c(0,2*pi),ylim=c(-1.1*pi,1.1*pi),xlab="$x$",ylab="",xaxt='n',yaxt='n')

lines(x,etm(x),lwd=2)
aX <- c(0,pi,2*pi)
aY <- axTicks(2)
labelsX<-c("","$\\frac{\\lambda_g}{2}$","$\\lambda_g$")
axis(1, at = aX, label = labelsX,pos=0)
axis(2, at = c(-3.5,0,3.5), label = c("$+E_x$","0","$-E_x$"),pos=0)

points(c(0,pi,2*pi),c(0,0,0),col=2,pch=16)
arrows(pi/2,0,pi/2,3,length=0.15,lwd=3,col=2)
arrows(3*pi/2,0,3*pi/2,3,length=0.15,lwd=3,col=2)
