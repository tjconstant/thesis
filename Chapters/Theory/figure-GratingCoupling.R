library("plotrix")

circle<-function(x0,y0,r,t1,t2){
  
  x<-c()
  y<-c()
  
  for(t in seq(t1,t2,,100)){
    
    x<-c(x,x0+r*cos(t))
    y<-c(y,y0+r*sin(t))
    
  }
  
  return(cbind(x,y))
  
}

par(mfrow=c(1,1),pty='s',mar=c(1,2,1,1))

rcp.space.grating<-function(){
  
  plot(NA,NA,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),ann=F,xaxt='n',yaxt='n')
  mtext(text="$k_x$",side=1,line=1)
  mtext(text="$k_z$",side=2,line=1)
# rect(-2,-2,2,0,border=NA,density=10,col='lightgrey')
  lines(circle(0,0,1,0,pi),lwd=3,col=2)

  lines(seq(-3,3,,100),rep(0,100),lty=2,lwd=1)
#  lines(rep(0,100),seq(-3,3,,100),lty=3)
  points(-1.25,0,col=2,lwd=3,pch=8)
  points(1.25,0,col=2,lwd=3,pch=8)
  points(-1.75,0,lwd=3,pch=8)
  points(1.75,0,lwd=3,pch=8)
  
  rect(-0.5,1.1,0.5,1.35,col=0,border=NA)
#  rect(-0.5,-1.1,0.5,-1.4,col=0,border=NA)
#  text(0,1.25,expression(italic(k[0])),col=2)
  text(0,1.25,"$k_0$",col=2)
#  text(0,-1.25,expression(italic(k[0])), col=2)
  text(-1.25,1.25,"air",col=2,pos=4)
  text(-1.25,-1.25,"metal",col=1,pos=4)
#  text(1.25,-0.25,expression(italic(k)[SPP]),col=2)
#  text(-1.25,-0.25,expression(italic(k)[SPP]),col=2)
text(1.25,-0.25,"$k_{SPP}$",col=2)
text(-1.25,-0.25,"$k_{SPP}$",col=2)
#  text(1.75,-0.25,expression(italic(k)[SPP]),col=1)
#  text(-1.75,-0.25,expression(italic(k)[SPP]),col=1)
}




tikz(file="figure-diffraction.tex",width=2.5,height=2.7)
par(mfrow=c(1,1),pty='s',mar=c(1,2,1,1))
k0=0.7 #actually kx but cant be bothered to change. the labeliing on the graphs are all correct

rcp.space.grating()
kg=0.2

arrows(-k0,sqrt(1^2-k0^2),0,0,lwd=2,length=0.1)
arrows(0,0,k0,sqrt(1^2-k0^2),lwd=2,length=0.1)
arrows(0,0,(k0+kg),sqrt(1^2-(k0+kg)^2),lwd=2,col=4,length=0.1)
arrows(0,0,(k0-kg),sqrt(1^2-(k0-kg)^2),lwd=2,col=4,length=0.1)

lines(rep(-k0,100),seq(0,sqrt(1^2-k0^2),,100),lty=3)
lines(rep(k0,100),seq(0,sqrt(1^2-k0^2),,100),lty=3)
lines(rep(k0+kg,100),seq(-1.1,sqrt(1^2-(k0+kg)^2),,100),lty=3,col=4)
lines(rep(k0-kg,100),seq(-0.7,sqrt(1^2-(k0-kg)^2),,100),lty=3,col=4)

#text(k0+kg,-0.6,expression(italic(k[x])+italic(k)[g]),col=4)
#text(k0-kg,-0.4,expression(italic(k[x])-italic(k)[g]),col=4)
text(k0+kg,-1.3,"$k_x+k_g$",col=4)
text(k0-kg,-0.8,"$k_x-k_g$",col=4)

#title("Diffraction")
dev.off()


tikz("figure-grazing-photon.tex",width=2.5,height=2.7)
par(mfrow=c(1,1),pty='s',mar=c(1,2,1,1))

rcp.space.grating()
kg=0.3

arrows(-k0,sqrt(1^2-k0^2),0,0,lwd=2,length=0.1)
arrows(0,0,k0,sqrt(1^2-k0^2),lwd=2,length=0.1)
arrows(0,0,(k0+kg),sqrt(1^2-(k0+kg)^2),lwd=2,col=4,length=0.1)
arrows(0,0,(k0-kg),sqrt(1^2-(k0-kg)^2),lwd=2,col=4,length=0.1)

lines(rep(-k0,100),seq(0,sqrt(1^2-k0^2),,100),lty=3)
lines(rep(k0,100),seq(0,sqrt(1^2-k0^2),,100),lty=3,length=0.1)
lines(rep(k0+kg,100),seq(-1.1,sqrt(1^2-(k0+kg)^2),,100),lty=3,col=4)
lines(rep(k0-kg,100),seq(-0.7,sqrt(1^2-(k0-kg)^2),,100),lty=3,col=4)

#text(k0+kg,-0.6,expression(italic(k[x])+italic(k)[g]),col=4)
#text(k0-kg,-0.4,expression(italic(k[x])-italic(k)[g]),col=4)
text(k0+kg,-1.3,"$k_x+k_g$",col=4)
text(k0-kg,-0.8,"$k_x-k_g$",col=4)

#title("Grazing diffracted order")
dev.off()

tikz("figure-SPP-grating-coupling.tex",width=2.5,height=2.7)
par(mfrow=c(1,1),pty='s',mar=c(1,2,1,1))

rcp.space.grating()
kg=0.55

arrows(-k0,sqrt(1^2-k0^2),0,0,lwd=2,length=0.1)
arrows(0,0,k0,sqrt(1^2-k0^2),lwd=2,length=0.1)
arrows(0,0,(k0+kg),0,lwd=2,col=4,length=0.1)
arrows(0,0,(k0-kg),sqrt(1^2-(k0-kg)^2),lwd=2,col=4,length=0.1)

rect(-0.1,-0.5,0.4,-0.3,col=0,border=NA)


lines(rep(-k0,100),seq(0,sqrt(1^2-k0^2),,100),lty=3,length=0.1)
lines(rep(k0,100),seq(0,sqrt(1^2-k0^2),,100),lty=3,length=0.1)
lines(rep(k0-kg,100),seq(-0.7,sqrt(1^2-(k0-kg)^2),,100),lty=3,length=0.1,col=4)
lines(rep(k0+kg,100),seq(-1.2,0,,100),lty=3,length=0.1,col=4)

#text(k0+kg,-0.6,expression(italic(k[x])+italic(k)[g]),col=4)
#text(k0-kg,-0.4,expression(italic(k[x])-italic(k)[g]),col=4)
text(k0+kg,-1.3,"$k_x+k_g$",col=4)
text(k0-kg,-0.8,"$k_x-k_g$",col=4)

#title("SPP coupling")
dev.off()

