tikz("figure-TEdispersion-symZZ-HFSS.tex",width=4,height=4)

par(mfrow=c(1,1),family="Helvetica",pty='s',xaxs='i',yaxs='i')

source("disp.plot.R")
source("disp.plot3.R")
source("fancyLaTexAxis.R")
n=1.518
k0.hfss<-2*pi*(dispTE.hfss[,2]*1e9)/3e8
cool.x<-expression(paste("in-plane wavevector, ",k[x]," (",m^-1,")"))
cool.y<-expression(paste("angular frequency, ",omega," (rad ",s^-1,")"))

dispTE.hfss<-read.csv("TE-full.csv")
#disp.plot(n*k0.hfss*dispTE.hfss[,1],2*pi*dispTE.hfss[,2]*1e9,dispTE.hfss[,3],plotYLim=c(2.21e15,4e15),plotXLim=c(0,2e7),ncol=100,nrow=100,xlab=cool.x, ylab=cool.y)
disp.plot3(dispTE.hfss[,1],dispTE.hfss[,2],dispTE.hfss[,3],n*k0.hfss*dispTE.hfss[,1],2*pi*dispTE.hfss[,2]*1e9,ylim=c(2.21e15,4e15),xlim=c(0,2e7),zlim=c(0,1.01),col=grey(seq(0,1,,100)),xaxt='n',yaxt='n')
text(0,1,"") # workaround for axis labels
#axis(1,at=axTicks(1),labels=c("$0$","$0.5\\times 10^{7}$","$1\\times 10^{7}$","$1.5\\times 10^{7}$","$2\\times 10^{7}$"))
#axis(2,at=axTicks(2),labels=c("$2.5\\times 10^{15}$","$3\\times 10^{15}$","$3.5\\times 10^{15}$","$4\\times 10^{15}$"))

dlines(n=1.518,a=cbind(3e8/dispTE.hfss[,2],asin(dispTE.hfss[,1])*180/pi),lwd=2,col=4)
text(1952305,2.3e15,"-1",col=4)
text(7050079,2.3e15,"+2",col=4)
text(12845151,2.3e15,"0",col=4)
text(17722417,2.3e15,"+3",col=4)
dev.off()


dispTE.data<-read.table("233120110629-7-0-60-deg-400-2-850nm-Rss.txt")
dispTERef.data<-read.table("223620110630-0-0-0-deg-400-2-850nm-Tss-withoutcollectinglens.txt")
disp.TEDkf<-read.table("225020110630-0-0-0-deg-850-0-850nm-DF.txt")

dispTE.data[,3]<-dispTE.data[,3]*1.1 #fudge!!!
dispTE.dataR<-splot(dispTE.data,dispTERef.data,disp.TEDkf)


k0.data<-2*pi/(dispTE.data[,1]*1e-9)
#disp.plot(n*k0.data*sin(dispTE.data[,2]*pi/180),2*pi*3e8/(dispTE.data[,1]*1e-9),dispTE.dataR,plotYLim=c(2.21e15,4e15),plotXLim=c(0,2e7),ncol=160,nrow=110,xlab=cool.x, ylab=cool.y)

disp.plot3(dispTE.data[,1],dispTE.data[,2],dispTE.dataR,n*k0.data*sin(dispTE.data[,2]*pi/180),2*pi*3e8/(dispTE.data[,1]*1e-9),ylim=c(2.21e15,4e15),xlim=c(0,2e7),zlim=c(0,1.01),xlab="in-plane wavevector, $k_x$ ($m^{-1}$)",ylab="angular frequency, $\\omega$ (rad $s^{-1}$) ",col=grey(seq(0,1,,100)),xaxt='n',yaxt='n')
fancyLaTexAxis(side=1)
fancyLaTexAxis(side=2)

dlines(n=1.518,cbind(dispTE.data[,1],dispTE.data[,2]),lwd=2, col=4)
text(1952305,2.3e15,"-1",col=4)
text(7050079,2.3e15,"+2",col=4)
text(12845151,2.3e15,"0",col=4)
text(17722417,2.3e15,"+3",col=4)

par(mfrow=c(1,1))