#Module iii - PLANT GROWTH AND TIMBER DECAY ESTIMATOR
#############################################################

#Content:

#A- plant growth
#B- timber decay
#C- leaf area index model

###############################################################
library(dplyr)

################
#A-PLANT GROWTH
###############
#plant variables change over time with a Gomprtz-type model (non-calibrated)
#where K is the ceiling, r is the growing rate, Xo (e.g., DBHo) is the value for a plant attribute at planting, and t is time
###################
#driving functions

#basal area - in cm
DBHt.f<-function(K,r,DBHo,t){ 
	DBHt<-K*(DBHo/K)^(exp((-1)*r*t))
	return(DBHt)
} 
#canopy-crown area - in m2
Ac.t.f<-function(K,r,Ac.to,t){
	Ac.t<-K*(Ac.to/K)^(exp((-1)*r*t))
	return(Ac.t)
} 
#rooting depth - in cm
RD.t.f<-function(K,r,RD.to,t){
	RD.t<-K*(RD.to/K)^(exp((-1)*r*t))
	return(RD.t)
} 

################
#rcp4.5
################################################################
#integration fo plant growth variables into met forcing dataset
################################################################
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/inputs")
GS.rcp45<-read.csv("growing_season_rcp45.csv")
met.ts.rcp45<-read.csv("met_ts_rcp45.csv")
head(met.ts.rcp45)

#annual growth rates; 

DBH.t<-DBHt.f(K=40,r=1.0668/15,DBHo=5,t=seq(from=0,to=nrow(GS.rcp45)))
Ac.t<-Ac.t.f(K=40,r=1.0668/15,Ac.to=1,t=seq(from=0,to=nrow(GS.rcp45)))
RD.t<-RD.t.f(K=150,r=1.0668/15,RD.to=30,t=seq(from=0,to=nrow(GS.rcp45)))

#establish daily growth rate - on the basis of growing season duration
dDBH.t<-sapply(1:nrow(GS.rcp45),function(i) (DBH.t[i+1]-DBH.t[i])/(GS.rcp45$J.end[i]-GS.rcp45$J.start[i]))
dAc.t<-sapply(1:nrow(GS.rcp45),function(i) (Ac.t[i+1]-Ac.t[i])/(GS.rcp45$J.end[i]-GS.rcp45$J.start[i]))
dRD.t<-sapply(1:nrow(GS.rcp45),function(i) (RD.t[i+1]-RD.t[i])/(GS.rcp45$J.end[i]-GS.rcp45$J.start[i]))

#create data frame with daily growth rates

GR.rcp45<-data.frame(year=GS.rcp45$year, dDBH.t= dDBH.t, dAc.t= dAc.t, dRD.t= dRD.t)

#split met.ts into gorups by year, so subsetting can be implemented

met.ts.rcp45.XX<-group_split(met.ts.rcp45,met.ts.rcp45$year)

#allocate daily growth rates on the basis of the growing season duration

dDBH.t.XX<-lapply(1:length(met.ts.rcp45.XX),function(i) ifelse(met.ts.rcp45.XX[[i]]$J < GS.rcp45$J.start[i] | met.ts.rcp45.XX[[i]]$J > GS.rcp45$J.end[i],0,GR.rcp45$dDBH.t[i]))

dAc.t.XX<-lapply(1:length(met.ts.rcp45.XX),function(i) ifelse(met.ts.rcp45.XX[[i]]$J < GS.rcp45$J.start[i] | met.ts.rcp45.XX[[i]]$J > GS.rcp45$J.end[i],0,GR.rcp45$dAc.t[i]))

dRD.t.XX<-lapply(1:length(met.ts.rcp45.XX),function(i) ifelse(met.ts.rcp45.XX[[i]]$J < GS.rcp45$J.start[i] | met.ts.rcp45.XX[[i]]$J > GS.rcp45$J.end[i],0,GR.rcp45$dRD.t[i]))

#setup initial values and get cumulative growth for each plant trait

dDBHi<-unlist(dDBH.t.XX)
dDBHi[1]<-5
DBHi<-cumsum(dDBHi)

#graphic check
par(oma=c(3,3,3,0))
plot(DBHi~as.Date(met.ts.rcp45$date),type="l",main="Diameter at breast height (DBH)",ylab="",xlab="",lwd=4,cex.lab=3,cex.axis=2,cex.main=2)
mtext("DBH (cm)",side=2,line=4,cex=3)
mtext("Time (years)",side=1,line=4,cex=3) 
dAci<-unlist(dAc.t.XX)
dAci[1]<-1
Aci<-cumsum(dAci)
par(oma=c(3,3,3,0))
plot(Aci~as.Date(met.ts.rcp45$date),type="l",main="Canopy crown area (Ac)",ylab="",xlab="",lwd=4,cex.lab=3,cex.axis=2,cex.main=2)
mtext("Ac (m2)",side=2,line=4,cex=3)
mtext("Time (years)",side=1,line=4,cex=3) 

dRDi<-unlist(dRD.t.XX)
dRDi[1]<-30
RDi<-cumsum(dRDi)

#input plant traits into met.ts for further modelling

met.ts.rcp45<-cbind(met.ts.rcp45,DBHi,Aci,RDi)
###############################################
#B-timber decay
###############
#parameters
###################################
#R: mean annual rainfall (mm)
#Ta: mean annual temperature (deg)
#k.R: decay coefficient due to rainfall
#k.T: decay coefficient due to temeperature
#k.climate: decay coefficient due to climate
#k.wood: decay coefficient due to timber material [class 1:0.20 (>25 years; in ground); class 2: 0.55 (10-25 years); class 3: 0.80 (5-15 y); class 4: 1.85 (0-5y) - Durability Class AS 5604-2005]
#rtd: decay rate of timber
#t.lag: lag time for the timber to begin decaying
#dcy: decay depth of timber (mm)
#t.y: time in years


#Eq.43: timber decay rate

rtd.f<-function(R,Ta,k.wood){
	k.R<-10*(1-exp((-1)*0.001*(R-250)))
	k.T<-(-1)*1+0.2*Ta
	k.climate<-k.R^0.3*k.T^0.2
	rtd<-k.climate*k.wood
	return(rtd)
}

#Eq.44: time lag for the wood to begin decaying - t.lag (years)

t.lag.f<-function(rtd){
	t.lag<-5.5*rtd^(-1)*0.95
	return(t.lag)
}
#Eq.45: decay depth of timber - dt (mm)

dcy.f<-function(rtd,t.lag,t.y){
	dcy<-(t.y+t.lag)*rtd
	return(dcy)
}

##################################################
#inputs
########

meanR.rcp45<-aggregate(met.ts.rcp45$pr,by=list(met.ts.rcp45$year),FUN=sum)
meanR.rcp45<-mean(meanR.rcp45$x)
meanT.rcp45<-aggregate(met.ts.rcp45$tas,by=list(met.ts.rcp45$year),FUN=mean)
meanT.rcp45<-mean(meanT.rcp45$x)
####################################

rtd.rcp45<-rtd.f(R=meanR.rcp45,Ta=meanT.rcp45,k.wood=0.55)
t.lag.rcp45<-t.lag.f(rtd=rtd.rcp45) #7.43 years until begins to decay....1996+7=2003
dcy.rcp45<-dcy.f(rtd=rtd.rcp45,t.lag=t.lag.rcp45,t.y=seq(from=0,to=nrow(GS.rcp45)))
dcy.rcp45[2]-dcy.rcp45[1]
dcy.rcp45[5]-dcy.rcp45[4]
###########################
#establish daily mm loss of timber

dt.rcp45<-lapply(1:length(met.ts.rcp45.XX),function(i) (dcy.rcp45[5]-dcy.rcp45[4])/nrow(met.ts.rcp45.XX[[i]]))

dt.rcp45<-mean(unlist(dt.rcp45))


dt.mm.rcp45<-sapply(1:nrow(met.ts.rcp45),function(i) ifelse(as.numeric(met.ts.rcp45$year[i]) > 1957,dt.rcp45,0))

#update data frame

met.ts.rcp45<-cbind(met.ts.rcp45,dt.mm.rcp45)


#####################################################################################################
#C - LEAF AREA INDEX - trapezoidal waveform function 
##################################################
#LAI-willow=3.26
#20 days for development and decay, respectively

#i-setup slope of the trapezoidal waves for each growing season
ai.rcp45<-sapply(1:nrow(GS.rcp45),function(i) (3.26-0)/(20))
bi.rcp45<-sapply(1:nrow(GS.rcp45),function(i) (0-3.26)/((GS.rcp45$J.end[i]+20)-GS.rcp45$J.end[i]))

#ii - establish daily LAI values 

LAIi.rcp45.l<-lapply(1:length(met.ts.rcp45.XX),function(i) ifelse(met.ts.rcp45.XX[[i]]$J <= (GS.rcp45$J.start[i]-20) | met.ts.rcp45.XX[[i]]$J >= (GS.rcp45$J.end[i]+20),0.01,ifelse(met.ts.rcp45.XX[[i]]$J > (GS.rcp45$J.start[i]-20) & met.ts.rcp45.XX[[i]]$J < GS.rcp45$J.start[i],ai.rcp45[i]*(met.ts.rcp45.XX[[i]]$J-(GS.rcp45$J.start[i]-20)),ifelse(met.ts.rcp45.XX[[i]]$J > GS.rcp45$J.end[i] &  met.ts.rcp45.XX[[i]]$J < (GS.rcp45$J.end[i]+20),3.26+bi.rcp45[i]*(met.ts.rcp45.XX[[i]]$J-GS.rcp45$J.end[i]),3.26))))

LAIi.rcp45<-unlist(LAIi.rcp45.l)

#update data frame

met.ts.rcp45<-cbind(met.ts.rcp45,LAIi.rcp45)


setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/inputs")
write.csv(met.ts.rcp45,"met_ts_rcp45_plant.csv")



