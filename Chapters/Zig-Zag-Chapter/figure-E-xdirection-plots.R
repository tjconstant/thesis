etm<-function(x){
  
  -4*pi*sin(x)^2/(cos(2*x)-3)
}

ete<-function(x){
  
  -4*pi*sin(x)/(cos(2*x)-3)
}

#tikz("figure-E-xdirection-plots.tex",width=3,height=3)

par(mar=c(4, 1, 1, 1) + 0.1,yaxs = "i",xaxs="i",pty='s') 
x=seq(0,2*pi,,100)
plot(NA,NA,xlim=c(0,2*pi),ylim=c(-1.1*pi,1.1*pi),xlab="$x$",ylab="",xaxt='n',yaxt='n')
mtext(text="$E$",side=2,line=1)
lines(x,etm(x),lwd=2)
lines(x,ete(x),lwd=2,col=2)
lines(x,rep(0,100))
#points(c(3.4,5.52),c(-1.5,2))
text(x=3.4,y=-1.5,labels="$E_{TE}$",pos=4,col=2)
text(x=5.52,y=2,labels="$E_{TM}$",pos=2)

aX <- c(0,pi,2*pi)
aY <- axTicks(2)
labelsX<-c("0","$\\pi$","$2\\pi$")
axis(1, at = aX, label = labelsX)

#dev.off()