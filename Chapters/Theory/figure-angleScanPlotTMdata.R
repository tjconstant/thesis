
library (gplots)
par(family="Helvetica",fig=c(0.15, 0.59, 0.15, 0.75), bg="white",new = T,pty='s',yaxs = "i",xaxs="i",box='n')


hfss.rough<-read.csv("./hfssFit-TM.csv")
HeNe.rough<-read.table("~/Dropbox/280711 - Angle Scan on Zig-Zag using morley. Glass Hemisphere/HeNe Scans/3103RPP9.DAT")

for(i in 1:length(hfss.rough[,1])){
  if(is.na(hfss.rough[i,2])==FALSE) {
    hfss.R<-c(hfss.R,hfss.rough[i,2])
    hfss.SinTheta<-c(hfss.SinTheta,hfss.rough[i,1])}
  
  if(is.na(HeNe.rough[i,2])==FALSE) {
    HeNe.R<-c(HeNe.R,HeNe.rough[i,2])
    HeNe.SinTheta<-c(HeNe.SinTheta,HeNe.rough[i,1])}
}

plot(NA,NA,type='l',lwd=3,xlim=c(15,40),ylim=c(0.8,1),xlab="",ylab="",col=2,xaxt='n',yaxt='n')

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
  "white")

axis(2,at=c(0.8,0.9,1),labels=c("0.8","0.9","1"))
axis(1,at=c(15,40),labels=c("15","40"))

points(asin(hfss.rough[,1])*180/pi,hfss.rough[,2],type='l',lwd=3,xlim=c(15,40),ylim=c(0.8,1),xlab="",ylab="",col=2)
#xlab=expression(paste("polar angle, ",theta," (°)")),ylab="Reflectivity (TM)"

bin<-seq(0,length(HeNe.rough[,1]),,20)
plotCI(HeNe.rough[bin,1]/2,(smooth(HeNe.rough[bin,2]/(1.105*HeNe.rough[bin,3]))),uiw=0.01,liw=0.01,gap=0.3,sfrac=0.0125,pt.bg=0,pch=1,lwd=1, col=2,add=TRUE)
#plotCI(HeNe.rough2[bin,1]/2,smooth(HeNe.rough2[bin,2]/(1.15*HeNe.rough2[bin,3])),uiw=0.01,liw=0.01,gap=0.3,sfrac=0.005,pt.bg=0,pch=1,lwd=1, add=TRUE)



#average<-(smooth(HeNe.rough[bin,2]/(1.05*HeNe.rough[bin,3]))/smooth(HeNe.rough2[bin,2]/(1.15*HeNe.rough2[bin,3])))
#plotCI(HeNe.rough2[bin,1]/2,average/1.1,uiw=0.01,liw=0.01,gap=0.3,sfrac=0.005,pt.bg=0,pch=1,lwd=1, add=TRUE)
grid()
box()
par(fig=c(0,1,0,1))
