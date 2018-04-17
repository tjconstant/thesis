library (gplots)

#tikz(file="figure-anglescan-expmnt.tex",width=4,height=4)
par(family="Helvetica",bg="white",pty='s',yaxs = "i",xaxs="i", box='n')

hfss.rough<-read.csv("./hfssFit.csv")
HeNe.rough<-read.csv("./hene.csv")

hfss.SinTheta<-c()
hfss.R<-c()

HeNe.SinTheta<-c()
HeNe.R<-c()

for(i in 1:length(hfss.rough[,1])){
  if(is.na(hfss.rough[i,2])==FALSE) {
    hfss.R<-c(hfss.R,hfss.rough[i,2])
    hfss.SinTheta<-c(hfss.SinTheta,hfss.rough[i,1])}
  
  if(is.na(HeNe.rough[i,2])==FALSE) {
    HeNe.R<-c(HeNe.R,HeNe.rough[i,2])
    HeNe.SinTheta<-c(HeNe.SinTheta,HeNe.rough[i,1])}
}

plot(asin(hfss.SinTheta)*180/pi,smooth(hfss.R),type='l',lwd=3,xlim=c(15,40),ylim=c(0.4,1),xlab="polar angle, $\\theta$ ($^{\\circ}$)",ylab="Reflectivity")
#expression(paste(R[TE]))

#bin<-seq(1,length(HeNe.SinTheta),,150)
bin<-c(seq(0,342,,25),seq(350,420,,20),seq(431,1100,,50)) #binning data to include detail around resonance
plotCI(asin(HeNe.SinTheta[bin])*180/pi,smooth(HeNe.R[bin]),uiw=0.01,liw=0.01,gap=0.3,sfrac=0.005,pt.bg=0,pch=1,lwd=1, add=TRUE)
#points(asin(HeNe.SinTheta[bin])*180/pi,smooth(HeNe.R[bin]),lwd=2)
grid()
#legend(32,0.64,c("","","TE","TM"),pch=c(NA,NA,1,1),lty=c(NA,NA,NA,NA),lwd=c(NA,NA,1,1),col=c(1,2),box.lwd=0)

legend(32,0.65,c("$R_{TE}$","$R_{TM}$"),pch=c(NA,NA),lty=c(1,1),lwd=c(3,3),box.lwd=0,bg="transparent",col=c(1:2))
box()