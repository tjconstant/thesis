
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

tikz("figure-plasmon-planar-dispersion.tex",width=4,height=4)
par(mar=c(5, 4, 4, 4) + 0.1,yaxs = "i",xaxs="i") 
x = seq(0, 8e7,length =100)
plot(NA,NA,xlim=c(min(x),max(x)),type='l', ylim=c(0,1e16),lwd=2,xlab="in-plane wavevector, $k_x$ ($m^{-1}$)", ylab="angular frequency, $\\omega$ (rad $s^{-1}$)",xaxt='n',yaxt='n')


#rect(xleft=7e15/3e8,ybottom=0,xright=(7e15/3e8)*(-4.5/(1-4.5)),ytop=7e15,col='lightgrey',lty=0)
#text(((7e15/3e8)*(-4.5/(1-4.5))+(7e15/3e8))/2,0.5e15,"$\\Delta k_x$")
#lines(x,rep(7e15,100),col='lightgrey',lty=1)

#rect(0,2*pi*3e8/800e-9,8e7,2*pi*3e8/450e-9,col='blue',lty=0)
lines(x,g(x,1),col=1,lwd=2)
lines(x,3e8*x,col=2,lwd=2)
lines(x,rep(1.32e16/sqrt(2),100), lty=2,lwd=2)

aX <- axTicks(1)
aY <- axTicks(2)
labelsX<-paste(aX/1e7,"$\\times 10^7$",sep="")
labelsX[1]<-"0"
labelsY<-paste(aY/1e15,"$\\times 10^{15}$",sep="")
labelsY[1]<-"0"
axis(1, at = aX, label = labelsX)
axis(2, at = aY, label = labelsY)
axis(4, at = c(1.32e16/sqrt(2)), label=c("$\\frac{\\omega_p}{\\sqrt{2}}$"),las=2)
#,7e15
#,"$\\omega_0$"

dev.off()
