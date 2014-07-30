
f <-function(kx){
  
  wp=1.32e16
  
  result<-sqrt(-sqrt(4*x^4+wp^4)+2*x^2+wp^2)/sqrt(2)
  #result[(kx^2+ky^2)>20^2]=NA
  return(result)
  
}
g <-function(x,n){
  
  wp=1.32e16
  c=3e8
  
  result<- sqrt((c^2*x^2)/n^3+(c^2*x^2)/n^2-sqrt((-c^2*n*x^2-c^2*x^2-n^2*wp^2)^2-4*c^2*n^3*wp^2*x^2)/n^3+wp^2/n)/sqrt(2)
  return(result)
  
  
  
}

l <-function(kx,ky){
  
  result<-sqrt(kx^2+ky^2)
  result[(kx^2+ky^2)>10^2]=NA
  return(result)
  
}

tikz("figure-diffracted-plasmon-coupling.tex",width=4,height=4)
par(mar=c(5, 4, 4, 4) + 0.1,yaxs = "i",xaxs="i") 
x = seq(-100e7, 100e7,length =5000)
plot(NA,NA,type='l', ylim=c(0,1e16),xlim=c(-5e7,5e7),lwd=2,xlab="in-plane wavevector, $k_x$ ($m^{-1}$)", ylab="angular frequency, $\\omega$ (rad $s^{-1}$)",xaxt='n',yaxt='n')


#rect(xleft=7e15/3e8,ybottom=0,xright=(7e15/3e8)*(-4.5/(1-4.5)),ytop=7e15,col='lightgrey',lty=0)
#text(((7e15/3e8)*(-4.5/(1-4.5))+(7e15/3e8))/2,0.5e15,"$\\Delta k_x$")
#lines(x,rep(7e15,100),col='lightgrey',lty=1)

#rect(0,2*pi*3e8/800e-9,8e7,2*pi*3e8/450e-9,col='blue',lty=0)
a=200e-9


for(i in c(-3,-2,-1,1,2,3)){
  lines(x+i*2*pi/a,g(x,1),col=rgb(0,0,1,1.25-abs(i)/3),lwd=2)
  }


lines(x,g(x,1),col=1,lwd=2)
lines(x,3e8*x,col=2,lwd=2)
lines(-x,3e8*x,col=2,lwd=2)

#aX <- axTicks(1)
aY <- axTicks(2)

aX<-c(-2*2*pi/a,-2*pi/a,0,2*pi/a,2*2*pi/a)
#labelsX<-paste(aX/1e7,"$\\times 10^7$",sep="")
labelsX<-c("$-2k_g$","$-k_g$","0","$k_g$","$2k_g$")
labelsY<-paste(aY/1e15,"$\\times 10^{15}$",sep="")
labelsY[1]<-"0"
axis(1, at = aX, label = labelsX)
axis(2, at = aY, label = labelsY)
#axis(4, at = c(1.32e16/sqrt(2)), label=c("$\\frac{\\omega_p}{\\sqrt{2}}$"),las=2)
#,7e15
#,"$\\omega_0$"

dev.off()
