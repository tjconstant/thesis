epsir_drude<-function(w,wp,tau){
  
  return(1-((wp^2*tau^2)/(1+w^2*tau^2)))
  
}

epsii_drude<-function(w,wp,tau){
  
  return((wp^2*tau^2)/(w*(1+(w^2*tau^2))))
  
}

eps_drude<-function(w,wp,tau){
  
  return(1-(wp^2/(w^2+w*tau*(1i))))
  
}

wavelength<-seq(400e-9,900e-9,,100)
w<-2*pi*3e8/wavelength

ER<-Re(eps_drude(w,1.32e16,1.4e14))
EI<-Im(eps_drude(w,1.32e16,1.4e14))

plot(wavelength*1e9,ER, type='l',lwd=2)
plot(wavelength*1e9,EI, type='l',lwd=2)


tikz("figure-drudemodel-espilons.tex",width=4,height=4)

par(mar=c(5, 4, 4, 5) + 0.1,yaxs = "i",xaxs="i")

plot(NA,NA,xlim=c(min(wavelength*1e9),max(wavelength*1e9)),ylim=c(min(10*EI-40),max(ER)),xlab="",ylab="")
lines(wavelength*1e9,ER, type='l',lwd=2)
lines(wavelength*1e9,10*EI-40, type='l',lwd=2,col=2)
aX <- axTicks(1)
#axis(3,at=aX,labels=2*pi*3e8/aX*1e9)
aY <- axTicks(2)
axis(4,at=aY,labels= aY/10+4)
title(xlab="wavelength (nm)")
title(ylab="$\\varepsilon_r$")
mtext(text="$\\varepsilon_i$",side=4,padj=4.5)

lines(rep(wavelength[47]*1e9,20),seq(-40,ER[47],,20),lty=2)
lines(seq(wavelength[47]*1e9,1000,,20),rep(10*EI[47]-40,20),lty=2)
lines(seq(0,wavelength[47]*1e9,,20),rep(ER[47],20),lty=2)
points(wavelength[47]*1e9,ER[47],pch=16)
points(wavelength[47]*1e9,10*EI[47]-40,pch=16,col=2)
text(wavelength[47]*1e9,ER[47],labels="$-18.6$",pos=4)
text(wavelength[47]*1e9,10*EI[47]-40,labels="$0.92$",pos=2,col=2)

dev.off()
