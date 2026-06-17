#Output analysis
#############################



#1) global FoS
#-----------
#############

#setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /research/live_cribwall_model/SBEE25_scripts/out_SBEE25/FoS_global")
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs/FoS_global")


FoS.gl.fallow.wt.0<-get(load("FoS_cb_global_fallow_wt_0_amend.RData"))
FoS.gl.fallow.wt.1<-get(load("FoS_cb_global_fallow_wt_1_amend.RData"))
FoS.gl.fallow.wt.2<-get(load("FoS_cb_global_fallow_wt_2_amend.RData"))
FoS.gl.fallow.wt.3<-get(load("FoS_cb_global_fallow_wt_3_amend.RData"))
FoS.gl.plant.wt.0<-get(load("FoS_cb_global_plant_wt_0_amend.RData"))
FoS.gl.plant.wt.1<-get(load("FoS_cb_global_plant_wt_1_amend.RData"))
FoS.gl.plant.wt.2<-get(load("FoS_cb_global_plant_wt_2_amend.RData"))
FoS.gl.plant.wt.3<-get(load("FoS_cb_global_plant_wt_3_amend.RData"))



FoS.gl.fallow.wt.0[[1]]
FoS.gl.plant.wt.1[[1]]


FoS.f.wt.0<-lapply(1:3,function(i) data.frame(FoS.f.wt.0=unlist(FoS.gl.fallow.wt.0[[i]])))
FoS.f.wt.1<-lapply(1:3,function(i) data.frame(FoS.f.wt.1=unlist(FoS.gl.fallow.wt.1[[i]])))
FoS.f.wt.2<-lapply(1:3,function(i) data.frame(FoS.f.wt.2=unlist(FoS.gl.fallow.wt.2[[i]])))
FoS.f.wt.3<-lapply(1:3,function(i) data.frame(FoS.f.wt.3=unlist(FoS.gl.fallow.wt.3[[i]])))
FoS.p.wt.0<-lapply(1:3,function(i) data.frame(FoS.p.wt.0=unlist(FoS.gl.plant.wt.0[[i]])))
FoS.p.wt.1<-lapply(1:3,function(i) data.frame(FoS.p.wt.1=unlist(FoS.gl.plant.wt.1[[i]])))
FoS.p.wt.2<-lapply(1:3,function(i) data.frame(FoS.p.wt.2=unlist(FoS.gl.plant.wt.2[[i]])))
FoS.p.wt.3<-lapply(1:3,function(i) data.frame(FoS.p.wt.3=unlist(FoS.gl.plant.wt.3[[i]])))

FoS.gl<-lapply(1:3,function(i) cbind(FoS.f.wt.0[[i]],FoS.f.wt.1[[i]],FoS.f.wt.2[[i]],FoS.f.wt.3[[i]], FoS.p.wt.0[[i]],FoS.p.wt.1[[i]],FoS.p.wt.2[[i]],FoS.p.wt.3[[i]]))
head(FoS.gl[[2]])

setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/inputs")
met.Catt<-read.csv("daily_Catterline_PEDROX.csv")
head(met.Catt)
met.Catt<-subset(met.Catt,met.Catt$date >= "2022-08-03" & met.Catt$date <= "2023-02-15")
met.Catt$ps<-((met.Catt$baro_max+met.Catt$baro_min)/2)/1000
met.Catt$tas<-(met.Catt$max_temp+met.Catt$min_temp)/2

FoS.gl<-lapply(1:3,function(i) cbind(FoS.gl[[i]],met.Catt))

#slip circle 1
par(mfrow=c(2,1),mai=c(1,1,0.5,0.5))
par(oma=c(1.2,1,1,1))
plot(FoS.f.wt.0~as.POSIXct(date),data=FoS.gl[[1]],type="l",ylim=c(0,10),xlab="",ylab="FoS",xaxt="n",main="Factor of Safety - Fallow; slip circle 1 (1.2 m b.g.l)",lwd=2)
axis.POSIXct(1,FoS.gl[[1]]$date,at=as.POSIXct(FoS.gl[[1]]$date),labels=format(as.POSIXct(FoS.gl[[1]]$date),"%Y-%m-%d"),las=2)
mtext(side=3,"(a)")
abline(h=1.3,lty=2,col="red",lwd=2)
lines(FoS.f.wt.1~as.POSIXct(date),data=FoS.gl[[1]],col=4,lwd=2)
lines(FoS.f.wt.2~as.POSIXct(date),data=FoS.gl[[1]],col=3,lwd=2)
lines(FoS.f.wt.3~as.POSIXct(date),data=FoS.gl[[1]],col=2,lwd=2)
legend("topleft",lty=c(1,1,1,1,2),col=c(4,3,2,1,"red"),lwd=c(2,2,2,2,2),c("water table @ 0.1 m b.g.l","water table @ 1 m b.g.l","water table @ 2 m b.g.l","no water table","critical FoS=1.3"),box.lwd=0,cex=0.7)
plot(FoS.p.wt.0~as.POSIXct(date),data=FoS.gl[[1]],type="l",ylim=c(0,20),xlab="",ylab="FoS",xaxt="n",main="Factor of Safety - Willow-vegetated; slip circle 1 (1.2 m b.g.l)",lwd=2)
axis.POSIXct(1,FoS.gl[[1]]$date,at=as.POSIXct(FoS.gl[[1]]$date),labels=format(as.POSIXct(FoS.gl[[1]]$date),"%Y-%m-%d"),las=2)
mtext(side=3,"(b)")
abline(h=1.3,lty=2,col="red",lwd=2)
lines(FoS.p.wt.1~as.POSIXct(date),data=FoS.gl[[1]],col=4,lwd=2)
lines(FoS.p.wt.2~as.POSIXct(date),data=FoS.gl[[1]],col=3,lwd=2)
lines(FoS.p.wt.3~as.POSIXct(date),data=FoS.gl[[1]],col=2,lwd=2)
legend("topleft",lty=c(1,1,1,1,2),col=c(4,3,2,1,"red"),lwd=c(2,2,2,2,2),c("water table @ 0.1 m b.g.l","water table @ 1 m b.g.l","water table @ 2 m b.g.l","no water table","critical FoS=1.3"),box.lwd=0,cex=0.7)

#slip circle 2
par(mfrow=c(2,1),mai=c(1,1,0.5,0.5))
par(oma=c(1.2,1,1,1))
plot(FoS.f.wt.0~as.POSIXct(date),data=FoS.gl[[2]],type="l",ylim=c(0,6),xlab="",ylab="FoS",xaxt="n",main="Factor of Safety - Fallow; slip circle 2 (2.54 m b.g.l)",lwd=2)
axis.POSIXct(1,FoS.gl[[2]]$date,at=as.POSIXct(FoS.gl[[2]]$date),labels=format(as.POSIXct(FoS.gl[[2]]$date),"%Y-%m-%d"),las=2)
mtext(side=3,"(a)")
abline(h=1.3,lty=2,col="red",lwd=2)
lines(FoS.f.wt.1~as.POSIXct(date),data=FoS.gl[[2]],col=4,lwd=2)
lines(FoS.f.wt.2~as.POSIXct(date),data=FoS.gl[[2]],col=3,lwd=2)
lines(FoS.f.wt.3~as.POSIXct(date),data=FoS.gl[[2]],col=2,lwd=2)
legend("topleft",lty=c(1,1,1,1,2),col=c(4,3,2,1,"red"),lwd=c(2,2,2,2,2),c("water table @ 0.1 m b.g.l","water table @ 1 m b.g.l","water table @ 2 m b.g.l","no water table","critical FoS=1.3"),box.lwd=0,cex=0.7)
plot(FoS.p.wt.0~as.POSIXct(date),data=FoS.gl[[2]],type="l",ylim=c(0,6),xlab="",ylab="FoS",xaxt="n",main="Factor of Safety - Willow-vegetated; slip circle 2 (2.54 m b.g.l)",lwd=2)
axis.POSIXct(1,FoS.gl[[2]]$date,at=as.POSIXct(FoS.gl[[2]]$date),labels=format(as.POSIXct(FoS.gl[[2]]$date),"%Y-%m-%d"),las=2)
mtext(side=3,"(b)")
abline(h=1.3,lty=2,col="red",lwd=2)
lines(FoS.p.wt.1~as.POSIXct(date),data=FoS.gl[[2]],col=4,lwd=2)
lines(FoS.p.wt.2~as.POSIXct(date),data=FoS.gl[[2]],col=3,lwd=2)
lines(FoS.p.wt.3~as.POSIXct(date),data=FoS.gl[[2]],col=2,lwd=2)
legend("topleft",lty=c(1,1,1,1,2),col=c(4,3,2,1,"red"),lwd=c(2,2,2,2,2),c("water table @ 0.1 m b.g.l","water table @ 1 m b.g.l","water table @ 2 m b.g.l","no water table","critical FoS=1.3"),box.lwd=0,cex=0.7)

#slip circle 3
par(mfrow=c(2,1),mai=c(1,1,0.5,0.5))
par(oma=c(1.2,1,1,1))
plot(FoS.f.wt.0~as.POSIXct(date),data=FoS.gl[[3]],type="l",ylim=c(0,6),xlab="",ylab="FoS",xaxt="n",main="Factor of Safety - Fallow; slip circle 3 (3.8 m b.g.l)",lwd=2)
axis.POSIXct(1,FoS.gl[[3]]$date,at=as.POSIXct(FoS.gl[[3]]$date),labels=format(as.POSIXct(FoS.gl[[3]]$date),"%Y-%m-%d"),las=2)
mtext(side=3,"(a)")
abline(h=1.3,lty=2,col="red",lwd=2)
lines(FoS.f.wt.1~as.POSIXct(date),data=FoS.gl[[3]],col=4,lwd=2)
lines(FoS.f.wt.2~as.POSIXct(date),data=FoS.gl[[3]],col=3,lwd=2)
lines(FoS.f.wt.3~as.POSIXct(date),data=FoS.gl[[3]],col=2,lwd=2)
legend("topleft",lty=c(1,1,1,1,2),col=c(4,3,2,1,"red"),lwd=c(2,2,2,2,2),c("water table @ 0.1 m b.g.l","water table @ 1 m b.g.l","water table @ 2 m b.g.l","no water table","critical FoS=1.3"),box.lwd=0,cex=0.7)
plot(FoS.p.wt.0~as.POSIXct(date),data=FoS.gl[[3]],type="l",ylim=c(0,6),xlab="",ylab="FoS",xaxt="n",main="Factor of Safety - Willow-vegetated; slip circle 3 (3.8 m b.g.l)",lwd=2)
axis.POSIXct(1,FoS.gl[[3]]$date,at=as.POSIXct(FoS.gl[[3]]$date),labels=format(as.POSIXct(FoS.gl[[3]]$date),"%Y-%m-%d"),las=2)
mtext(side=3,"(b)")
abline(h=1.3,lty=2,col="red",lwd=2)
lines(FoS.p.wt.1~as.POSIXct(date),data=FoS.gl[[3]],col=4,lwd=2)
lines(FoS.p.wt.2~as.POSIXct(date),data=FoS.gl[[3]],col=3,lwd=2)
lines(FoS.p.wt.3~as.POSIXct(date),data=FoS.gl[[3]],col=2,lwd=2)
legend("topleft",lty=c(1,1,1,1,2),col=c(4,3,2,1,"red"),lwd=c(2,2,2,2,2),c("water table @ 0.1 m b.g.l","water table @ 1 m b.g.l","water table @ 2 m b.g.l","no water table","critical FoS=1.3"),box.lwd=0,cex=0.7)

###########################################################
#dates of predicted failure
#--------------------------
#1-dec-22 (Failure with FoS = 1.14 on 23 Dec 2022)
year<-format(as.POSIXct(met.Catt$date),"%Y")
month<-format(as.POSIXct(met.Catt$date),"%m")
day<-format(as.POSIXct(met.Catt$date),"%d")


FoS.gl<-lapply(1:3,function(i) cbind(FoS.gl[[i]],year,month,day))
head(FoS.gl[[1]])

sub.1<-subset(FoS.gl[[1]],FoS.gl[[1]]$date > "2022-12-01" & FoS.gl[[1]]$date < "2022-12-31" )
head(sub.1)
sub.1<-sub.1[sub.1$FoS.f.wt.1 < 1.3,]
print(sub.1) #23/12/2022 - very interesting

#what happened on previous days?
sub.2<-subset(FoS.gl[[1]],FoS.gl[[1]]$date > "2022-12-13" & FoS.gl[[1]]$date <= "2022-12-23" )
##########################
#let's check matric suction profiles
#note: i implemented some changes in Dec25, but lost track - retireving profiles originally generated for SBEE25
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs")
#plant.sl<-get(load("plant_slice_ts_SBEE25_run.RData"))
#plant.sl<-get(load("plant_slice_ts_SBEE25_run.RData"))
plant.sl<-get(load("plant_slice_ts_SBEE25_run_test.RData"))
#plant.sl<-get(load("plant_slice_ts_SBEE25.RData"))
fail.1<-plant.sl[[143]]
head(fail.1)

	y.wt.1<-fail.1$MS_WT1_kPa*(-1)
	y.etp.1<-fail.1$MS_ETP_kPa*(-1)
	y.stm.1<-fail.1$MS_ST_kPa*(-1)
	y.er.1<-fail.1$MS_ER_kPa*(-1)
	y.uni.1<-fail.1$MS_unified_1_kPa*(-1)


#...on days prior to failure
fail.1b<-plant.sl[[142]]
head(fail.1b)

y.wt.1b<-fail.1b$MS_WT1_kPa*(-1)
y.etp.1b <-fail.1b$MS_ETP_kPa*(-1)
y.stm.1b<-fail.1b$MS_ST_kPa*(-1)
y.er.1b<-fail.1b$MS_ER_kPa*(-1)
y.uni.1b<-fail.1b$MS_unified_1_kPa*(-1)

#----on the day after
fail.1.af<-plant.sl[[144]]
y.wt.1.af<-fail.1.af$MS_WT1_kPa*(-1)
y.etp.1.af<-fail.1.af$MS_ETP_kPa*(-1)
y.stm.1.af<-fail.1.af$MS_ST_kPa*(-1)
y.er.1.af<-fail.1.af$MS_ER_kPa*(-1)
y.uni.1.af<-fail.1.af$MS_unified_1_kPa*(-1)



setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs")
soil.sl<-get(load("soil_slice_ts_SBEE25_run.RData"))


s.fail.1<-soil.sl[[143]]
head(s.fail.1)

s.y.wt.1<-s.fail.1$MS_WT1_kPa*(-1)
	s.y.d.1<-s.fail.1$MS_d_kPa*(-1)
	s.y.w.1<-s.fail.1$MS_w_kPa*(-1)
	s.y.uni.1<-s.fail.1$MS_unified_1_kPa*(-1)
#...on days prior to failure
s.fail.1b<-soil.sl[[142]]
s.y.wt.1b<-s.fail.1b$MS_WT1_kPa*(-1)
	s.y.d.1b<-s.fail.1b$MS_d_kPa*(-1)
	s.y.w.1b<-s.fail.1b$MS_w_kPa*(-1)
	s.y.uni.1b<-s.fail.1b$MS_unified_1_kPa*(-1)
#----on the day after
s.fail.1.af<-soil.sl[[144]]
s.y.wt.1.af<-s.fail.1.af$MS_WT1_kPa*(-1)
	s.y.d.1.af<-s.fail.1.af$MS_d_kPa*(-1)
	s.y.w.1.af<-s.fail.1.af$MS_w_kPa*(-1)
	s.y.uni.1.af<-s.fail.1.af$MS_unified_1_kPa*(-1)
##################################################################
#/////////////////////////////////////////////////////////////////
###################################################################
#adding soil-root reinforcement to plots
#####################################################
cR.x<-(-1)*1.2*fail.1$Tr_kPa*fail.1$RAR

##########
#plot
#########
par(mfrow=c(2,3),mai=c(0.8,0.8,0.5,0.5))
par(oma=c(4,1,1,1))
plot(z.point~y.wt.1b,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=fail.1b,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Willow-vegetated 22-Dec 2022",lwd=3,cex.lab=1.5)
abline(h=1.2,lty=3,col="red")
mtext(side=3,"(a)")
lines(fail.1b$z.point~y.etp.1b,col="green",lwd=3)
lines(fail.1b$z.point~y.stm.1b,col="blue",lwd=3)
lines(fail.1b$z.point~y.er.1b,col="cyan",lwd=3)
lines(fail.1b$z.point~y.uni.1b,col="olivedrab1",lwd=3)
lines(fail.1b$z.point~cR.x,col="gray",lwd=3,lty=3)
abline(v=0,lty=2,col="black")


plot(z.point~y.wt.1,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=fail.1,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Willow-vegetated 23-Dec 2022",lwd=3,cex.lab=1.5)
abline(h=1.2,lty=3,col="red")
mtext(side=3,"(b)")
lines(fail.1$z.point~y.etp.1,col="green",lwd=3)
lines(fail.1$z.point~y.stm.1,col="blue",lwd=3)
lines(fail.1$z.point~y.er.1,col="cyan",lwd=3)
lines(fail.1$z.point~y.uni.1,col="olivedrab1",lwd=3)
lines(fail.1b$z.point~cR.x,col="gray",lwd=3,lty=3)
abline(v=0,lty=2,col="black")

plot(z.point~y.wt.1.af,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=fail.1.af,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Willow-vegetated 24-Dec 2022",lwd=3,cex.lab=1.5)
abline(h=1.2,lty=3,col="red")
mtext(side=3,"(c)")
lines(fail.1.af$z.point~y.etp.1.af,col="green",lwd=3)
lines(fail.1.af$z.point~y.stm.1.af,col="blue",lwd=3)
lines(fail.1.af$z.point~y.er.1.af,col="cyan",lwd=3)
lines(fail.1.af$z.point~y.uni.1.af,col="olivedrab1",lwd=3)
lines(fail.1b$z.point~cR.x,col="gray",lwd=3,lty=3)
abline(v=0,lty=2,col="black")


plot(s.fail.1b$z.point~s.y.wt.1b,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=s.fail.1b,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Fallow 22-Dec 2022",lwd=3,cex.lab=1.5)
abline(h=1.2,lty=3,col="red")
mtext(side=3,"(d)")
lines(s.fail.1b$z.point~s.y.d.1b,col="orange",lwd=3)
lines(s.fail.1b$z.point~s.y.w.1b,col="mediumaquamarine",lwd=3)
lines(s.fail.1b$z.point~s.y.uni.1b,col="olivedrab",lwd=3)
abline(v=0,lty=2,col="black")

plot(z.point~s.y.wt.1,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=s.fail.1,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Fallow 23-Dec 2022",lwd=3,cex.lab=1.5)
abline(h=1.2,lty=3,col="red")
mtext(side=3,"(e)")
lines(s.fail.1$z.point~s.y.d.1,col="orange",lwd=3)
lines(s.fail.1$z.point~s.y.w.1,col="mediumaquamarine",lwd=3)
lines(s.fail.1$z.point~s.y.uni.1,col="olivedrab",lwd=3)
abline(v=0,lty=2,col="black")

plot(z.point~s.y.wt.1.af,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=s.fail.1.af,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Fallow 24-Dec 2022",lwd=3,cex.lab=1.5)
abline(h=1.2,lty=3,col="red")
mtext(side=3,"(f)")
lines(s.fail.1.af$z.point~s.y.d.1.af,col="orange",lwd=3)
lines(s.fail.1.af$z.point~s.y.w.1.af,col="mediumaquamarine",lwd=3)
lines(s.fail.1.af$z.point~s.y.uni.1.af,col="olivedrab",lwd=3)
abline(v=0,lty=2,col="black")

par(fig=c(0,1,0,1),oma=c(0,0,0,0),mar=c(0,0,0,0),new=TRUE)
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
par(cex=1.3)

legend("bottom",c("soil evaporation","rainfall infiltration","unified effect (fallow)","saturation boundary","slip depth","root reinforcement"),lty=c(1,1,1,2,2,2),lwd=c(3,3,3,3,3,3),col=c("orange","mediumaquamarine","olivedrab","black","red","gray48"),bty="n",cex=0.7,horiz=TRUE)

par(fig=c(0,1,0,1),oma=c(0,0,0,0),mar=c(1,0,0,0),new=TRUE)
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
par(cex=1.3)

legend("bottom",c("evapotranspiration","stemflow","effective rainfall infiltration","unified effect (vegetation)"),lty=c(1,1,1,1),lwd=c(3,3,3,3),col=c("green","blue","cyan","olivedrab1"),bty="n",cex=0.7,horiz=TRUE)

dev.off()


##---------------------------------------------------------------------------------------------- 
#slip circles 2 & 3 shows failure under vegetated soil on 4 Jan 2023, what happened on this daY?
##---------------------------------------------------------------------------------------------- 

sub.X<-subset(FoS.gl[[3]],FoS.gl[[3]]$date > "2022-12-22" & FoS.gl[[3]]$date < "2023-01-15" )
head(sub.X)
sub.X<-sub.X[sub.X$FoS.f.wt.1 < 1.3,]
print(sub.X) # - very interesting
#the failure is actully predicted on Jan 5th 2023

#what happened on previous days?
sub.X2<-subset(FoS.gl[[3]],FoS.gl[[3]]$date > "2023-01-03" & FoS.gl[[3]]$date <= "2023-01-10" )
#unclear reason - FoS for wt3 is particularly low, but not for the other water tables



sub.Z<-subset(FoS.gl[[2]],FoS.gl[[2]]$date > "2022-12-22" & FoS.gl[[3]]$date < "2023-01-15" )
head(sub.Z)
sub.Z<-sub.Z[sub.Z$FoS.f.wt.1 < 1.3,]
print(sub.Z) # - very interesting
#the failure is actully predicted on Jan 
#same happens for slip circle, only no water table conditions show this behaviour 
#let's check matric suction profiles

#######################

plant.sl<-get(load("plant_slice_ts_SBEE25_run.RData"))
fail.2<-plant.sl[[158]]
head(fail.2)

	y.wt.2<-fail.2$MS_WT1_kPa*(-1)
	y.etp.2<-fail.2$MS_ETP_kPa*(-1)
	y.stm.2<-fail.2$MS_ST_kPa*(-1)
	y.er.2<-fail.2$MS_ER_kPa*(-1)
	y.uni.2<-fail.2$MS_unified_1_kPa*(-1)

#before and after
fail.2.bf<-plant.sl[[157]]
y.wt.2.bf<-fail.2.bf$MS_WT1_kPa*(-1)
	y.etp.2.bf<-fail.2.bf$MS_ETP_kPa*(-1)
	y.stm.2.bf<-fail.2.bf$MS_ST_kPa*(-1)
	y.er.2.bf<-fail.2.bf$MS_ER_kPa*(-1)
	y.uni.2.bf<-fail.2.bf$MS_unified_1_kPa*(-1)

fail.2.af<-plant.sl[[159]]
y.wt.2.af<-fail.2.af$MS_WT1_kPa*(-1)
	y.etp.2.af<-fail.2.af$MS_ETP_kPa*(-1)
	y.stm.2.af<-fail.2.af$MS_ST_kPa*(-1)
	y.er.2.af<-fail.2.af$MS_ER_kPa*(-1)
	y.uni.2.af<-fail.2.af$MS_unified_1_kPa*(-1)

#fallow

soil.sl<-get(load("soil_slice_ts_SBEE25_run.RData"))


s.fail.2<-soil.sl[[158]]
s.y.wt.2<-s.fail.2$MS_WT1_kPa*(-1)
	s.y.d.2<-s.fail.2$MS_d_kPa*(-1)
	s.y.w.2<-s.fail.2$MS_w_kPa*(-1)
	s.y.uni.2<-s.fail.2$MS_unified_1_kPa*(-1)
	
	#before
	s.fail.2.bf<-soil.sl[[157]]

s.y.wt.2.bf<-s.fail.2.bf$MS_WT1_kPa*(-1)
	s.y.d.2.bf<-s.fail.2.bf$MS_d_kPa*(-1)
	s.y.w.2.bf<-s.fail.2.bf$MS_w_kPa*(-1)
	s.y.uni.2.bf<-s.fail.2.bf$MS_unified_1_kPa*(-1)

#after
s.fail.2.af<-soil.sl[[159]]
s.y.wt.2.af<-s.fail.2.af$MS_WT1_kPa*(-1)
	s.y.d.2.af<-s.fail.2.af$MS_d_kPa*(-1)
	s.y.w.2.af<-s.fail.2.af$MS_w_kPa*(-1)
	s.y.uni.2.af<-s.fail.2.af$MS_unified_1_kPa*(-1)

#plots

par(mfrow=c(2,3),mai=c(0.8,0.8,0.5,0.5))
par(oma=c(1,1,1,6))

plot(z.point~y.wt.2.bf,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=fail.2.bf,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Willow-vegetated 04-Jan 2023",lwd=3)
abline(h=2.54,lty=3,col="red")
mtext(side=3,"(a)")
lines(fail.2.bf$z.point~y.etp.2.bf,col="green",lwd=3)
lines(fail.2.bf$z.point~y.stm.2.bf,col="blue",lwd=3)
lines(fail.2.bf$z.point~y.er.2.bf,col="cyan",lwd=3)
lines(fail.2.bf$z.point~y.uni.2.bf,col="olivedrab1",lwd=3)
abline(v=0,lty=2,col="gray48")

plot(z.point~y.wt.2,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=fail.2,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Willow-vegetated 05-Jan 2023",lwd=3)
abline(h=2.54,lty=3,col="red")
mtext(side=3,"(b)")
lines(fail.2$z.point~y.etp.2,col="green",lwd=3)
lines(fail.2$z.point~y.stm.2,col="blue",lwd=3)
lines(fail.2$z.point~y.er.2,col="cyan",lwd=3)
lines(fail.2$z.point~y.uni.2,col="olivedrab1",lwd=3)
abline(v=0,lty=2,col="gray48")

plot(z.point~y.wt.2.af,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=fail.2.af,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Willow-vegetated 06-Jan 2023",lwd=3)
abline(h=2.54,lty=3,col="red")
mtext(side=3,"(c)")
lines(fail.2.af$z.point~y.etp.2.af,col="green",lwd=3)
lines(fail.2.af$z.point~y.stm.2.af,col="blue",lwd=3)
lines(fail.2.af$z.point~y.er.2.af,col="cyan",lwd=3)
lines(fail.2.af$z.point~y.uni.2.af,col="olivedrab1",lwd=3)
abline(v=0,lty=2,col="gray48")

plot(z.point~s.y.wt.2.bf,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=fail.2.bf,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Fallow 04-Jan 2023",lwd=3)
abline(h=2.54,lty=3,col="red")
mtext(side=3,"(d)")
lines(fail.2.bf$z.point~s.y.d.2.bf,col="orange",lwd=3)
lines(fail.2.bf$z.point~s,y.w.2.bf,col="mediumaquamarine",lwd=3)
lines(fail.2.bf$z.point~s.y.uni.2.bf,col="olivedrab",lwd=3)
abline(v=0,lty=2,col="gray48")

plot(z.point~s.y.wt.2,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=fail.2,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Fallow profiles 05-Jan 2023",lwd=3)
abline(h=2.54,lty=3,col="red")
mtext(side=3,"(e)")
lines(fail.2$z.point~s.y.d.2,col="orange",lwd=3)
lines(fail.2$z.point~s.y.w.2,col="mediumaquamarine",lwd=3)
lines(fail.2$z.point~s.y.uni.2,col="olivedrab",lwd=3)
abline(v=0,lty=2,col="gray48")

plot(z.point~s.y.wt.2.af,type="l",ylim=c(max(z.point),0), xlim=c(-120,100), data=fail.2.af,xlab="Soil Pore Pressure (kPa)",ylab="Soil depth (m)",col="brown1",main="Fallow profiles 06-Jan 2023",lwd=3)
abline(h=2.54,lty=3,col="red")
mtext(side=3,"(f)")
lines(fail.2.af$z.point~s.y.d.2.af,col="orange",lwd=3)
lines(fail.2.af$z.point~s.y.w.2.af,col="mediumaquamarine",lwd=3)
lines(fail.2.af$z.point~s.y.uni.2.af,col="olivedrab",lwd=3)
abline(v=0,lty=2,col="gray48")


########################################################
#overturning slice-on-slice
##############
#run cribwall_reinforcement_SBEE.R


setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /research/live_cribwall_model/SBEE25_scripts/out_SBEE25/FoS_overturning")
ov.f.wt.1<-get(load("FoS_cb_ov_fallow_wt_1.RData"))
ov.f.wt.2<-get(load("FoS_cb_ov_fallow_wt_2.RData"))
ov.f.wt.3<-get(load("FoS_cb_ov_fallow_wt_3.RData"))
ov.f.wt.0<-get(load("FoS_cb_ov_fallow_wt_0.RData"))
ov.p.wt.1<-get(load("FoS_cb_ov_plant_wt_1.RData"))
ov.p.wt.2<-get(load("FoS_cb_ov_plant_wt_2.RData"))
ov.p.wt.3<-get(load("FoS_cb_ov_plant_wt_3.RData"))	
ov.p.wt.0<-get(load("FoS_cb_ov_plant_wt_0.RData"))


ov.f.wt.1[[2]][[1]]


head(ct3cb.fallow[[1]][[1]]) #slice, need z.point.x, and date




#ov.f.wt.1[[1]][[6]][4,]




#wt1-fallow

ov.f.wt.1<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ov.f.wt.1[[i]][[j]],slice=ct3cb.fallow[[i]][[j]]$slice,depth=ct3cb.fallow[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.fallow[[i]][[j]]$DATE))))



ov.f.wt.1.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.1[[i]][[j]][4,])))



ov.f.wt.1.sl.4[[1]][[1]][,1]
ov.f.wt.1.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.4[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.4[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.4[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.4[[i]][[k]][,4]))))

ov.f.wt.1.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.1[[i]][[j]][5,])))
ov.f.wt.1.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.5[[i]][[k]][,4]))))

ov.f.wt.1.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.1[[i]][[j]][6,])))
ov.f.wt.1.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.6[[i]][[k]][,4]))))


ov.f.wt.1.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.1[[i]][[j]][7,])))
ov.f.wt.1.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.7[[i]][[k]][,4]))))


ov.f.wt.1.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.1[[i]][[j]][8,])))
ov.f.wt.1.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.8[[i]][[k]][,4]))))

ov.f.wt.1.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.1[[i]][[j]][9,])))
ov.f.wt.1.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.1.sl.9[[i]][[k]][,4]))))

ov.f.wt.1<-lapply(1:3, function(i) rbind(ov.f.wt.1.sl.4.df[[i]],ov.f.wt.1.sl.5.df[[i]],ov.f.wt.1.sl.6.df[[i]],ov.f.wt.1.sl.7.df[[i]],ov.f.wt.1.sl.8.df[[i]],ov.f.wt.1.sl.9.df[[i]]))

tail(ov.f.wt.1[[3]])




#wt2-fallow

ov.f.wt.2<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ov.f.wt.2[[i]][[j]],slice=ct3cb.fallow[[i]][[j]]$slice,depth=ct3cb.fallow[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.fallow[[i]][[j]]$DATE))))


ov.f.wt.2.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.2[[i]][[j]][4,])))



#ov.f.wt.2.sl.4[[1]][[1]][,1]

ov.f.wt.2.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(j) ov.f.wt.2.sl.4[[i]][[j]][,1]),slice=sapply(1:length(soil.sl), function(j) ov.f.wt.2.sl.4[[i]][[j]][,2]),depth=sapply(1:length(soil.sl), function(j) ov.f.wt.2.sl.4[[i]][[j]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(j) ov.f.wt.2.sl.4[[i]][[j]][,4]))))

ov.f.wt.2.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.2[[i]][[j]][5,])))
ov.f.wt.2.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.5[[i]][[k]][,4]))))

ov.f.wt.2.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.2[[i]][[j]][6,])))
ov.f.wt.2.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.6[[i]][[k]][,4]))))


ov.f.wt.2.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.2[[i]][[j]][7,])))
ov.f.wt.2.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.7[[i]][[k]][,4]))))


ov.f.wt.2.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.2[[i]][[j]][8,])))
ov.f.wt.2.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.8[[i]][[k]][,4]))))

ov.f.wt.2.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.2[[i]][[j]][9,])))
ov.f.wt.2.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.2.sl.9[[i]][[k]][,4]))))

ov.f.wt.2<-lapply(1:3, function(i) rbind(ov.f.wt.2.sl.4.df[[i]],ov.f.wt.2.sl.5.df[[i]],ov.f.wt.2.sl.6.df[[i]],ov.f.wt.2.sl.7.df[[i]],ov.f.wt.2.sl.8.df[[i]],ov.f.wt.2.sl.9.df[[i]]))

tail(ov.f.wt.2[[3]])


############
#wt.3
###########

ov.f.wt.3<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ov.f.wt.3[[i]][[j]],slice=ct3cb.fallow[[i]][[j]]$slice,depth=ct3cb.fallow[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.fallow[[i]][[j]]$DATE))))

ov.f.wt.3.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.3[[i]][[j]][4,])))



#ov.f.wt.3.sl.4[[1]][[1]][,1]

ov.f.wt.3.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(j) ov.f.wt.3.sl.4[[i]][[j]][,1]),slice=sapply(1:length(soil.sl), function(j) ov.f.wt.3.sl.4[[i]][[j]][,2]),depth=sapply(1:length(soil.sl), function(j) ov.f.wt.3.sl.4[[i]][[j]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(j) ov.f.wt.3.sl.4[[i]][[j]][,4]))))

ov.f.wt.3.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.3[[i]][[j]][5,])))
ov.f.wt.3.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.5[[i]][[k]][,4]))))

ov.f.wt.3.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.3[[i]][[j]][6,])))
ov.f.wt.3.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.6[[i]][[k]][,4]))))


ov.f.wt.3.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.3[[i]][[j]][7,])))
ov.f.wt.3.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.7[[i]][[k]][,4]))))


ov.f.wt.3.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.3[[i]][[j]][8,])))
ov.f.wt.3.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.8[[i]][[k]][,4]))))

ov.f.wt.3.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.f.wt.3[[i]][[j]][9,])))
ov.f.wt.3.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.f.wt.3.sl.9[[i]][[k]][,4]))))

ov.f.wt.3<-lapply(1:3, function(i) rbind(ov.f.wt.3.sl.4.df[[i]],ov.f.wt.3.sl.5.df[[i]],ov.f.wt.3.sl.6.df[[i]],ov.f.wt.3.sl.7.df[[i]],ov.f.wt.3.sl.8.df[[i]],ov.f.wt.3.sl.9.df[[i]]))

tail(ov.f.wt.3[[1]])


#####################################
#aggreagate values

FoS.ov.f.wt.1<-lapply(1:3,function(i) aggregate(ov.f.wt.1[[i]]$FoS,by=list(ov.f.wt.1[[i]]$slice),FUN="mean",na.action=na.pass))
#FoS.ovwt.1[[3]]

FoS.ov.f.wt.2<-lapply(1:3,function(i) aggregate(ov.f.wt.2[[i]]$FoS,by=list(ov.f.wt.2[[i]]$slice),FUN="mean",na.action=na.pass))
FoS.ov.f.wt.3<-lapply(1:3,function(i) aggregate(ov.f.wt.3[[i]]$FoS,by=list(ov.f.wt.3[[i]]$slice),FUN="mean",na.action=na.pass))

FoS.ov.f.df<-lapply(1:3,function(i) data.frame(slice=FoS.ov.f.wt.1[[i]]$Group.1,FoS.ov.wt1=FoS.ov.f.wt.1[[i]]$x,FoS.ov.wt2=FoS.ov.f.wt.2[[i]]$x,FoS.ovwt3=FoS.ov.f.wt.3[[i]]$x))

########################################same as above for vegetated


#wt1-vegetated

ov.p.wt.1<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ov.p.wt.1[[i]][[j]],slice=ct3cb.plant[[i]][[j]]$slice,depth=ct3cb.plant[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.plant[[i]][[j]]$DATE))))



ov.p.wt.1.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.1[[i]][[j]][4,])))



ov.p.wt.1.sl.4[[1]][[1]][,1]
ov.p.wt.1.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.4[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.4[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.4[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.4[[i]][[k]][,4]))))

ov.p.wt.1.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.1[[i]][[j]][5,])))
ov.p.wt.1.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.5[[i]][[k]][,4]))))

ov.p.wt.1.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.1[[i]][[j]][6,])))
ov.p.wt.1.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.6[[i]][[k]][,4]))))


ov.p.wt.1.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.1[[i]][[j]][7,])))
ov.p.wt.1.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.7[[i]][[k]][,4]))))


ov.p.wt.1.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.1[[i]][[j]][8,])))
ov.p.wt.1.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.8[[i]][[k]][,4]))))

ov.p.wt.1.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.1[[i]][[j]][9,])))
ov.p.wt.1.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.1.sl.9[[i]][[k]][,4]))))

ov.p.wt.1<-lapply(1:3, function(i) rbind(ov.p.wt.1.sl.4.df[[i]],ov.p.wt.1.sl.5.df[[i]],ov.p.wt.1.sl.6.df[[i]],ov.p.wt.1.sl.7.df[[i]],ov.p.wt.1.sl.8.df[[i]],ov.p.wt.1.sl.9.df[[i]]))

tail(ov.p.wt.1[[3]])




#wt2-vegetated

ov.p.wt.2<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ov.p.wt.2[[i]][[j]],slice=ct3cb.plant[[i]][[j]]$slice,depth=ct3cb.plant[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.plant[[i]][[j]]$DATE))))


ov.p.wt.2.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.2[[i]][[j]][4,])))



#ov.p.wt.2.sl.4[[1]][[1]][,1]

ov.p.wt.2.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(j) ov.p.wt.2.sl.4[[i]][[j]][,1]),slice=sapply(1:length(soil.sl), function(j) ov.p.wt.2.sl.4[[i]][[j]][,2]),depth=sapply(1:length(soil.sl), function(j) ov.p.wt.2.sl.4[[i]][[j]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(j) ov.p.wt.2.sl.4[[i]][[j]][,4]))))

ov.p.wt.2.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.2[[i]][[j]][5,])))
ov.p.wt.2.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.5[[i]][[k]][,4]))))

ov.p.wt.2.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.2[[i]][[j]][6,])))
ov.p.wt.2.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.6[[i]][[k]][,4]))))


ov.p.wt.2.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.2[[i]][[j]][7,])))
ov.p.wt.2.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.7[[i]][[k]][,4]))))


ov.p.wt.2.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.2[[i]][[j]][8,])))
ov.p.wt.2.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.8[[i]][[k]][,4]))))

ov.p.wt.2.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.2[[i]][[j]][9,])))
ov.p.wt.2.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.2.sl.9[[i]][[k]][,4]))))

ov.p.wt.2<-lapply(1:3, function(i) rbind(ov.p.wt.2.sl.4.df[[i]],ov.p.wt.2.sl.5.df[[i]],ov.p.wt.2.sl.6.df[[i]],ov.p.wt.2.sl.7.df[[i]],ov.p.wt.2.sl.8.df[[i]],ov.p.wt.2.sl.9.df[[i]]))

tail(ov.p.wt.2[[3]])


############
#wt.3 - vegetated
###########

ov.p.wt.3<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ov.p.wt.3[[i]][[j]],slice=ct3cb.plant[[i]][[j]]$slice,depth=ct3cb.plant[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.plant[[i]][[j]]$DATE))))

ov.p.wt.3.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.3[[i]][[j]][4,])))



#ov.p.wt.3.sl.4[[1]][[1]][,1]

ov.p.wt.3.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(j) ov.p.wt.3.sl.4[[i]][[j]][,1]),slice=sapply(1:length(soil.sl), function(j) ov.p.wt.3.sl.4[[i]][[j]][,2]),depth=sapply(1:length(soil.sl), function(j) ov.p.wt.3.sl.4[[i]][[j]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(j) ov.p.wt.3.sl.4[[i]][[j]][,4]))))

ov.p.wt.3.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.3[[i]][[j]][5,])))
ov.p.wt.3.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.5[[i]][[k]][,4]))))

ov.p.wt.3.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.3[[i]][[j]][6,])))
ov.p.wt.3.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.6[[i]][[k]][,4]))))


ov.p.wt.3.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.3[[i]][[j]][7,])))
ov.p.wt.3.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.7[[i]][[k]][,4]))))


ov.p.wt.3.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.3[[i]][[j]][8,])))
ov.p.wt.3.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.8[[i]][[k]][,4]))))

ov.p.wt.3.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ov.p.wt.3[[i]][[j]][9,])))
ov.p.wt.3.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ov.p.wt.3.sl.9[[i]][[k]][,4]))))

ov.p.wt.3<-lapply(1:3, function(i) rbind(ov.p.wt.3.sl.4.df[[i]],ov.p.wt.3.sl.5.df[[i]],ov.p.wt.3.sl.6.df[[i]],ov.p.wt.3.sl.7.df[[i]],ov.p.wt.3.sl.8.df[[i]],ov.p.wt.3.sl.9.df[[i]]))

tail(ov.p.wt.3[[1]])


#####################################
#aggreagate values

FoS.ov.p.wt.1<-lapply(1:3,function(i) aggregate(ov.p.wt.1[[i]]$FoS,by=list(ov.p.wt.1[[i]]$slice),FUN="mean",na.action=na.pass))
#FoSs.wt.1[[3]]

FoS.ov.p.wt.2<-lapply(1:3,function(i) aggregate(ov.p.wt.2[[i]]$FoS,by=list(ov.p.wt.2[[i]]$slice),FUN="mean",na.action=na.pass))
FoS.ov.p.wt.3<-lapply(1:3,function(i) aggregate(ov.p.wt.3[[i]]$FoS,by=list(ov.p.wt.3[[i]]$slice),FUN="mean",na.action=na.pass))

FoS.ov.p.df<-lapply(1:3,function(i) data.frame(slice=FoS.ov.p.wt.1[[i]]$Group.1,FoS.ov.wt1=FoS.ov.p.wt.1[[i]]$x,FoS.ov.wt2=FoS.ov.p.wt.2[[i]]$x,FoS.ov.wt3=FoS.ov.p.wt.3[[i]]$x))

#############################
#if barplot is the way forward to plot this, then i must create a supper data.frame 
##########################################################################################

FoS.ov.DF<-lapply(1:3, function(i) data.frame(wt1.F=FoS.ov.f.df[[i]][,2],wt2.F=FoS.ov.f.df[[i]][,3],wt3.F=FoS.ov.f.df[[i]][,4],wt1.V=FoS.ov.p.df[[i]][,2],wt2.V=FoS.ov.p.df[[i]][,3],wt3.V=FoS.ov.p.df[[i]][,4]))

for(i in 1:3){
	rownames(FoS.ov.DF[[i]])<-FoS.ov.p.df[[i]][,1]
}



setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /research/outputs")
write.csv(FoS.ov.DF[[1]],"FoS_ov_c1.csv")
write.csv(FoS.ov.DF[[2]],"FoS_ov_c2.csv")
write.csv(FoS.ov.DF[[3]],"FoS_ov_c3.csv")

cols<-c("cyan","darkolivegreen4","cyan3","deepskyblue","deepskyblue3","deepskyblue4")

par(mfrow=c(1,3))
barplot(as.matrix(FoS.ov.DF[[1]]),beside=TRUE,ylim=c(-0.5,1.5),main="Overturning check - r=12 m c=(8,20) ",col=cols)
#mtext(side=1,colnames(FoS.ov.DF[[1]]),las=2)
abline(h=1.3,lty=2,lwd=2)
legend("bottomleft",legend=rownames(FoS.ov.DF[[1]]),fill=cols,box.lty=0, horiz=TRUE,cex=1.2,title="slice")
barplot(as.matrix(FoS.ov.DF[[2]]),beside=TRUE,ylim=c(-0.5,1.5),main="Overturning check - r=12 m c=(8,20) ",col=cols)
#mtext(side=1,colnames(FoS.ov.DF[[1]]),las=2)
abline(h=1.3,lty=2,lwd=2)
legend("bottomleft",legend=rownames(FoS.ov.DF[[2]]),fill=cols,box.lty=0, horiz=TRUE,cex=1.2,title="slice")
barplot(as.matrix(FoS.ov.DF[[3]]),beside=TRUE,ylim=c(-0.5,1.5),main="Overturning check - r=12 m c=(8,20) ",col=cols)
#mtext(side=1,colnames(FoS.ov.DF[[1]]),las=2)
abline(h=1.3,lty=2,lwd=2)
legend("bottomleft",legend=rownames(FoS.ov.DF[[3]]),fill=cols,box.lty=0, horiz=TRUE,cex=1.2,title="slice")



########################################################
#sliding slice-on-slice
##############
#run cribwall_reinforcement_SBEE.R


setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /research/live_cribwall_model/SBEE25_scripts/out_SBEE25/FoS_sliding")
ss.f.wt.1<-get(load("FoS_cb_s_fallow_wt_1.RData"))
ss.f.wt.2<-get(load("FoS_cb_s_fallow_wt_2.RData"))
ss.f.wt.3<-get(load("FoS_cb_s_fallow_wt_3.RData"))
ss.f.wt.0<-get(load("FoS_cb_s_fallow_wt_0.RData"))
ss.p.wt.1<-get(load("FoS_cb_s_plant_wt_1.RData"))
ss.p.wt.2<-get(load("FoS_cb_s_plant_wt_2.RData"))
ss.p.wt.3<-get(load("FoS_cb_s_plant_wt_3.RData"))	
ss.p.wt.0<-get(load("FoS_cb_s_plant_wt_0.RData"))


ss.f.wt.1[[2]][[1]]


head(ct3cb.fallow[[1]][[1]]) #slice, need z.point.x, and date




#ss.f.wt.1[[1]][[6]][4,]




#wt1-fallow

ss.f.wt.1<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ss.f.wt.1[[i]][[j]],slice=ct3cb.fallow[[i]][[j]]$slice,depth=ct3cb.fallow[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.fallow[[i]][[j]]$DATE))))



ss.f.wt.1.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.1[[i]][[j]][4,])))



ss.f.wt.1.sl.4[[1]][[1]][,1]
ss.f.wt.1.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.4[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.4[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.4[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.4[[i]][[k]][,4]))))

ss.f.wt.1.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.1[[i]][[j]][5,])))
ss.f.wt.1.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.5[[i]][[k]][,4]))))

ss.f.wt.1.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.1[[i]][[j]][6,])))
ss.f.wt.1.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.6[[i]][[k]][,4]))))


ss.f.wt.1.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.1[[i]][[j]][7,])))
ss.f.wt.1.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.7[[i]][[k]][,4]))))


ss.f.wt.1.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.1[[i]][[j]][8,])))
ss.f.wt.1.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.8[[i]][[k]][,4]))))

ss.f.wt.1.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.1[[i]][[j]][9,])))
ss.f.wt.1.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.1.sl.9[[i]][[k]][,4]))))

ss.f.wt.1<-lapply(1:3, function(i) rbind(ss.f.wt.1.sl.4.df[[i]],ss.f.wt.1.sl.5.df[[i]],ss.f.wt.1.sl.6.df[[i]],ss.f.wt.1.sl.7.df[[i]],ss.f.wt.1.sl.8.df[[i]],ss.f.wt.1.sl.9.df[[i]]))

tail(ss.f.wt.1[[3]])




#wt2-fallow

ss.f.wt.2<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ss.f.wt.2[[i]][[j]],slice=ct3cb.fallow[[i]][[j]]$slice,depth=ct3cb.fallow[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.fallow[[i]][[j]]$DATE))))


ss.f.wt.2.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.2[[i]][[j]][4,])))



#ss.f.wt.2.sl.4[[1]][[1]][,1]

ss.f.wt.2.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(j) ss.f.wt.2.sl.4[[i]][[j]][,1]),slice=sapply(1:length(soil.sl), function(j) ss.f.wt.2.sl.4[[i]][[j]][,2]),depth=sapply(1:length(soil.sl), function(j) ss.f.wt.2.sl.4[[i]][[j]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(j) ss.f.wt.2.sl.4[[i]][[j]][,4]))))

ss.f.wt.2.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.2[[i]][[j]][5,])))
ss.f.wt.2.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.5[[i]][[k]][,4]))))

ss.f.wt.2.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.2[[i]][[j]][6,])))
ss.f.wt.2.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.6[[i]][[k]][,4]))))


ss.f.wt.2.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.2[[i]][[j]][7,])))
ss.f.wt.2.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.7[[i]][[k]][,4]))))


ss.f.wt.2.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.2[[i]][[j]][8,])))
ss.f.wt.2.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.8[[i]][[k]][,4]))))

ss.f.wt.2.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.2[[i]][[j]][9,])))
ss.f.wt.2.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.2.sl.9[[i]][[k]][,4]))))

ss.f.wt.2<-lapply(1:3, function(i) rbind(ss.f.wt.2.sl.4.df[[i]],ss.f.wt.2.sl.5.df[[i]],ss.f.wt.2.sl.6.df[[i]],ss.f.wt.2.sl.7.df[[i]],ss.f.wt.2.sl.8.df[[i]],ss.f.wt.2.sl.9.df[[i]]))

tail(ss.f.wt.2[[3]])


############
#wt.3
###########

ss.f.wt.3<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ss.f.wt.3[[i]][[j]],slice=ct3cb.fallow[[i]][[j]]$slice,depth=ct3cb.fallow[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.fallow[[i]][[j]]$DATE))))

ss.f.wt.3.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.3[[i]][[j]][4,])))



#ss.f.wt.3.sl.4[[1]][[1]][,1]

ss.f.wt.3.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(j) ss.f.wt.3.sl.4[[i]][[j]][,1]),slice=sapply(1:length(soil.sl), function(j) ss.f.wt.3.sl.4[[i]][[j]][,2]),depth=sapply(1:length(soil.sl), function(j) ss.f.wt.3.sl.4[[i]][[j]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(j) ss.f.wt.3.sl.4[[i]][[j]][,4]))))

ss.f.wt.3.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.3[[i]][[j]][5,])))
ss.f.wt.3.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.5[[i]][[k]][,4]))))

ss.f.wt.3.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.3[[i]][[j]][6,])))
ss.f.wt.3.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.6[[i]][[k]][,4]))))


ss.f.wt.3.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.3[[i]][[j]][7,])))
ss.f.wt.3.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.7[[i]][[k]][,4]))))


ss.f.wt.3.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.3[[i]][[j]][8,])))
ss.f.wt.3.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.8[[i]][[k]][,4]))))

ss.f.wt.3.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.f.wt.3[[i]][[j]][9,])))
ss.f.wt.3.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.f.wt.3.sl.9[[i]][[k]][,4]))))

ss.f.wt.3<-lapply(1:3, function(i) rbind(ss.f.wt.3.sl.4.df[[i]],ss.f.wt.3.sl.5.df[[i]],ss.f.wt.3.sl.6.df[[i]],ss.f.wt.3.sl.7.df[[i]],ss.f.wt.3.sl.8.df[[i]],ss.f.wt.3.sl.9.df[[i]]))

tail(ss.f.wt.3[[1]])


#####################################
#aggreagate values

FoS.ss.f.wt.1<-lapply(1:3,function(i) aggregate(ss.f.wt.1[[i]]$FoS,by=list(ss.f.wt.1[[i]]$slice),FUN="mean",na.action=na.pass))
#FoS.ovwt.1[[3]]

FoS.ss.f.wt.2<-lapply(1:3,function(i) aggregate(ss.f.wt.2[[i]]$FoS,by=list(ss.f.wt.2[[i]]$slice),FUN="mean",na.action=na.pass))
FoS.ss.f.wt.3<-lapply(1:3,function(i) aggregate(ss.f.wt.3[[i]]$FoS,by=list(ss.f.wt.3[[i]]$slice),FUN="mean",na.action=na.pass))

FoS.ss.f.df<-lapply(1:3,function(i) data.frame(slice=FoS.ss.f.wt.1[[i]]$Group.1,FoS.ss.wt1=FoS.ss.f.wt.1[[i]]$x,FoS.ss.wt2=FoS.ss.f.wt.2[[i]]$x,FoS.ovwt3=FoS.ss.f.wt.3[[i]]$x))

########################################same as above for vegetated


#wt1-vegetated

ss.p.wt.1<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ss.p.wt.1[[i]][[j]],slice=ct3cb.plant[[i]][[j]]$slice,depth=ct3cb.plant[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.plant[[i]][[j]]$DATE))))



ss.p.wt.1.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.1[[i]][[j]][4,])))



ss.p.wt.1.sl.4[[1]][[1]][,1]
ss.p.wt.1.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.4[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.4[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.4[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.4[[i]][[k]][,4]))))

ss.p.wt.1.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.1[[i]][[j]][5,])))
ss.p.wt.1.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.5[[i]][[k]][,4]))))

ss.p.wt.1.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.1[[i]][[j]][6,])))
ss.p.wt.1.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.6[[i]][[k]][,4]))))


ss.p.wt.1.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.1[[i]][[j]][7,])))
ss.p.wt.1.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.7[[i]][[k]][,4]))))


ss.p.wt.1.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.1[[i]][[j]][8,])))
ss.p.wt.1.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.8[[i]][[k]][,4]))))

ss.p.wt.1.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.1[[i]][[j]][9,])))
ss.p.wt.1.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.1.sl.9[[i]][[k]][,4]))))

ss.p.wt.1<-lapply(1:3, function(i) rbind(ss.p.wt.1.sl.4.df[[i]],ss.p.wt.1.sl.5.df[[i]],ss.p.wt.1.sl.6.df[[i]],ss.p.wt.1.sl.7.df[[i]],ss.p.wt.1.sl.8.df[[i]],ss.p.wt.1.sl.9.df[[i]]))

tail(ss.p.wt.1[[3]])




#wt2-vegetated

ss.p.wt.2<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ss.p.wt.2[[i]][[j]],slice=ct3cb.plant[[i]][[j]]$slice,depth=ct3cb.plant[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.plant[[i]][[j]]$DATE))))


ss.p.wt.2.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.2[[i]][[j]][4,])))



#ss.p.wt.2.sl.4[[1]][[1]][,1]

ss.p.wt.2.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(j) ss.p.wt.2.sl.4[[i]][[j]][,1]),slice=sapply(1:length(soil.sl), function(j) ss.p.wt.2.sl.4[[i]][[j]][,2]),depth=sapply(1:length(soil.sl), function(j) ss.p.wt.2.sl.4[[i]][[j]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(j) ss.p.wt.2.sl.4[[i]][[j]][,4]))))

ss.p.wt.2.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.2[[i]][[j]][5,])))
ss.p.wt.2.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.5[[i]][[k]][,4]))))

ss.p.wt.2.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.2[[i]][[j]][6,])))
ss.p.wt.2.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.6[[i]][[k]][,4]))))


ss.p.wt.2.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.2[[i]][[j]][7,])))
ss.p.wt.2.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.7[[i]][[k]][,4]))))


ss.p.wt.2.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.2[[i]][[j]][8,])))
ss.p.wt.2.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.8[[i]][[k]][,4]))))

ss.p.wt.2.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.2[[i]][[j]][9,])))
ss.p.wt.2.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.2.sl.9[[i]][[k]][,4]))))

ss.p.wt.2<-lapply(1:3, function(i) rbind(ss.p.wt.2.sl.4.df[[i]],ss.p.wt.2.sl.5.df[[i]],ss.p.wt.2.sl.6.df[[i]],ss.p.wt.2.sl.7.df[[i]],ss.p.wt.2.sl.8.df[[i]],ss.p.wt.2.sl.9.df[[i]]))

tail(ss.p.wt.2[[3]])


############
#wt.3 - vegetated
###########

ss.p.wt.3<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) cbind(FoS.ov=ss.p.wt.3[[i]][[j]],slice=ct3cb.plant[[i]][[j]]$slice,depth=ct3cb.plant[[i]][[j]]$z.point.x,date=as.POSIXct(ct3cb.plant[[i]][[j]]$DATE))))

ss.p.wt.3.sl.4<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.3[[i]][[j]][4,])))



#ss.p.wt.3.sl.4[[1]][[1]][,1]

ss.p.wt.3.sl.4.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(j) ss.p.wt.3.sl.4[[i]][[j]][,1]),slice=sapply(1:length(soil.sl), function(j) ss.p.wt.3.sl.4[[i]][[j]][,2]),depth=sapply(1:length(soil.sl), function(j) ss.p.wt.3.sl.4[[i]][[j]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(j) ss.p.wt.3.sl.4[[i]][[j]][,4]))))

ss.p.wt.3.sl.5<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.3[[i]][[j]][5,])))
ss.p.wt.3.sl.5.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.5[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.5[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.5[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.5[[i]][[k]][,4]))))

ss.p.wt.3.sl.6<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.3[[i]][[j]][6,])))
ss.p.wt.3.sl.6.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.6[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.6[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.6[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.6[[i]][[k]][,4]))))


ss.p.wt.3.sl.7<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.3[[i]][[j]][7,])))
ss.p.wt.3.sl.7.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.7[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.7[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.7[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.7[[i]][[k]][,4]))))


ss.p.wt.3.sl.8<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.3[[i]][[j]][8,])))
ss.p.wt.3.sl.8.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.8[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.8[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.8[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.8[[i]][[k]][,4]))))

ss.p.wt.3.sl.9<-lapply(1:3,function(i) lapply(1:length(soil.sl), function(j) rbind(ss.p.wt.3[[i]][[j]][9,])))
ss.p.wt.3.sl.9.df<-lapply(1:3,function(i) data.frame(FoSs=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.9[[i]][[k]][,1]),slice=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.9[[i]][[k]][,2]),depth=sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.9[[i]][[k]][,3]),date=as.POSIXct(sapply(1:length(soil.sl), function(k) ss.p.wt.3.sl.9[[i]][[k]][,4]))))

ss.p.wt.3<-lapply(1:3, function(i) rbind(ss.p.wt.3.sl.4.df[[i]],ss.p.wt.3.sl.5.df[[i]],ss.p.wt.3.sl.6.df[[i]],ss.p.wt.3.sl.7.df[[i]],ss.p.wt.3.sl.8.df[[i]],ss.p.wt.3.sl.9.df[[i]]))

tail(ss.p.wt.3[[1]])


#####################################
#aggreagate values

FoS.ss.p.wt.1<-lapply(1:3,function(i) aggregate(ss.p.wt.1[[i]]$FoS,by=list(ss.p.wt.1[[i]]$slice),FUN="mean",na.action=na.pass))
#FoSs.wt.1[[3]]

FoS.ss.p.wt.2<-lapply(1:3,function(i) aggregate(ss.p.wt.2[[i]]$FoS,by=list(ss.p.wt.2[[i]]$slice),FUN="mean",na.action=na.pass))
FoS.ss.p.wt.3<-lapply(1:3,function(i) aggregate(ss.p.wt.3[[i]]$FoS,by=list(ss.p.wt.3[[i]]$slice),FUN="mean",na.action=na.pass))

FoS.ss.p.df<-lapply(1:3,function(i) data.frame(slice=FoS.ss.p.wt.1[[i]]$Group.1,FoS.ss.wt1=FoS.ss.p.wt.1[[i]]$x,FoS.ss.wt2=FoS.ss.p.wt.2[[i]]$x,FoS.ss.wt3=FoS.ss.p.wt.3[[i]]$x))

#############################
#if barplot is the way forward to plot this, then i must create a supper data.frame 
##########################################################################################

FoS.ss.DF<-lapply(1:3, function(i) data.frame(wt1.F=FoS.ss.f.df[[i]][,2],wt2.F=FoS.ss.f.df[[i]][,3],wt3.F=FoS.ss.f.df[[i]][,4],wt1.V=FoS.ss.p.df[[i]][,2],wt2.V=FoS.ss.p.df[[i]][,3],wt3.V=FoS.ss.p.df[[i]][,4]))

for(i in 1:3){
	rownames(FoS.ss.DF[[i]])<-FoS.ss.p.df[[i]][,1]
}


setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /research/outputs")
write.csv(FoS.ss.DF[[1]],"FoS_ss_c1.csv")
write.csv(FoS.ss.DF[[2]],"FoS_ss_c2.csv")
write.csv(FoS.ss.DF[[3]],"FoS_ss_c3.csv")

#FoS.ss.DF<-lapply(1:3, function(i) ifelse(FoS.ss.DF[[i]][,]>10,10,FoS.ss.DF[[i]][,]))


cols<-c("cyan","darkolivegreen4","cyan3","deepskyblue","deepskyblue3","deepskyblue4")

par(mfrow=c(1,3))
barplot(as.matrix(FoS.ss.DF[[1]]),beside=TRUE,ylim=c(-0.5,10),main="Sliding check - r=12 m c=(8,20) ",col=cols)
#mtext(side=1,colnames(FoS.ss.DF[[1]]),las=2)
abline(h=1.3,lty=2,lwd=2)
legend("bottomleft",legend=rownames(FoS.ss.DF[[1]]),fill=cols,box.lty=0, horiz=TRUE,cex=1.2,title="slice")
barplot(as.matrix(FoS.ss.DF[[2]]),beside=TRUE,ylim=c(-0.5,10),main="Sliding check - r=12 m c=(8,20) ",col=cols)
#mtext(side=1,colnames(FoS.ss.DF[[1]]),las=2)
abline(h=1.3,lty=2,lwd=2)
legend("bottomleft",legend=rownames(FoS.ss.DF[[2]]),fill=cols,box.lty=0, horiz=TRUE,cex=1.2,title="slice")
barplot(as.matrix(FoS.ss.DF[[3]]),beside=TRUE,ylim=c(-0.5,10),main="Sliding check - r=12 m c=(8,20) ",col=cols)
#mtext(side=1,colnames(FoS.ss.DF[[1]]),las=2)
abline(h=1.3,lty=2,lwd=2)
legend("bottomleft",legend=rownames(FoS.ss.DF[[3]]),fill=cols,box.lty=0, horiz=TRUE,cex=1.2,title="slice")

##############################################################
#wee vegetation and soil moisture plots
#########################################
#run PLANT SLICES MODEL_SBEE.R
#-----------------------------
#soil moisture ts
SM.vg<-SM.null
#then run soil slices
SM.f<-SM.null

LAI.df<-subset(met.ts,met.ts$date >= "2021-01-01" & met.ts$date <= "2023-12-31")


par(mfrow=c(2,3))
plot(SX.Ma.x/1000~as.Date(met.ts$date),type="l",main="aboveground biomass",xlab="year",ylab="Aboveground biomass (kg)",lwd=2,cex.lab=1.5)
mtext(side=3,"(a)")
plot(met.ts$DBHi~as.Date(met.ts$date),type="l",main="diameter @ breast height (DBH)",xlab="year",ylab="DBH (cm)",lwd=3,cex.lab=1.5)
mtext(side=3,"(b)")
plot(met.ts$Aci~as.Date(met.ts$date),type="l",main="Crown area",xlab="year",ylab="Ac (m2)",lwd=3,cex.lab=1.5)
mtext(side=3,"(c)")
plot(LAIi.rcp45~as.Date(date),data=LAI.df,type="l",main="Leaf Area Index (LAI)",xlab="year",ylab="LAI",lwd=1,cex.lab=1.5)
mtext(side=3,"(d)")
plot(plant.df[[1]]$z.point~unlist(plant.df[[1]]$Arz),ylim=c(1,0),xlim=c(0,100),type="l",lwd=3, ylab="Soil depth (m)",xlab="root cross-sectional area (cm2)",main="vertical root distribution",cex.lab=1.5)
mtext(side=3,"(e)")
plot(SM.vg~as.POSIXct(met.Catt$date),type='l',ylim=c(0,1),lwd=3,main="Soil moisture predictions",xlab="Time",ylab="Volumetric soil moisture (/1)",col="blue",cex.lab=1.5)
lines(SM.f~as.POSIXct(met.Catt$date),col="brown1")
legend("topleft",c("vegetated","fallow"),lty=c(1,1),lwd=c(3,3),col=c("blue","brown1"))
mtext(side=3,"(f)")

head(plant.df[[1]])

##############################################################
##############################################################
##############################################################
##############################################################
#missing legend
#####################

plot(NULL,xaxt="n",yaxt="n",bty="n",xlim=0:1,ylim=0:1,xlab="",ylab="")
legend("topleft",c("cribwall face","outfall live pole drain","outfall pipe","slope grid crest","reported landslip"),lty=c(1,1,1,1,2),lwd=c(3,3,3,3,3),col=c("forestgreen","cyan","darkcyan","brown","red"),bty="n",cex=1.5)


plot(NULL,xaxt="n",yaxt="n",bty="n",xlim=0:1,ylim=0:1,xlab="",ylab="")
legend("topleft",c("cribwall face","outfall live pole drain","outfall pipe","slope grid crest","rainfall","air temperature","reported landslip"),lty=c(1,1,1,1,1,1,2),lwd=c(3,3,3,3,3,3,3),col=c("forestgreen","cyan","darkcyan","brown","blue","gray48","red"),bty="n",cex=1.5)


plot(NULL,xaxt="n",yaxt="n",bty="n",xlim=0:1,ylim=0:1,xlab="",ylab="")
legend("topleft",c("water table @ 0.1 m b.g.l","evapotranspiration","stemflow","effective rainfall infiltration","unified effect (vegetation)","soil evaporation","rainfall infiltration","unified effect (fallow)","saturation boundary","slip depth"),lty=c(1,1,1,1,1,1,1,1,2,2),lwd=c(3,3,3,3,3,3,3,3,3,3),col=c("brown1","green","blue","cyan","olivedrab1","orange","mediumaquamarine","olivedrab","gray48","red"),bty="n",cex=1.5)

