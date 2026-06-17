#Module iv - slope reinforcement & stability analysis
###############################################################

#Contents
############
#0) load required inputs and functions
#1) Merge slip.cribwall geometry with soil/plant-soil slices
#2) calculate cribwall reinforcement due to member pullout
#3) calculate cribwall reinforcement due to member breakage
#4) calculation of lateral earth pressure
#5) Calculation of overturning resistance
#6) Calculation of overtunrning driving force
#7) crib wall overturning check
#8) Calculate sliding resistance
#9) Calculation of sliding driving force due to Ea
#10) crib wall sliding check
#11) calculation of global resisting forces fallow soil
#12) calculation of global resisting forces vegetated soil
#13) Calculation of global Sliding force
#14) Calculation of Factor of Safety

########################################################
#0) load required inputs and functions
########################################################

#0.1) eco-hydro-met variables
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/inputs")

met.ts<-read.csv("met_ts_rcp45_plant.csv")
met.ts.SBEE<-subset(met.ts,met.ts$date >= "2022-08-03" & met.ts$date <= "2023-02-15")

#0.2) soil slices
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs")
soil.sl<-get(load("soil_slice_ts_SBEE25_run.RData"))

#0.3) plant-soil slices
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs")
plant.sl<-get(load("plant_slice_ts_SBEE25_run.RData"))
length(plant.sl)

#0.4) cribwall and slip geometry

ct3.cb<-get(load("cribwall_SBEE.RData"))
ct3.cb<-ct3.cb[c(2:4)]


setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model")
source("live_crib_wall_model_functions.R")


#1)  Merge slip.cribwall geometry with soil/plant-soil slices
#################################################################

#merge by z.point

#1.1 - soil slice
soil.z.p<-lapply(1:length(soil.sl),function(i) data.frame(z.p=matrix(unlist(round(soil.sl[[i]]$z.point,digits=1)),ncol=1,byrow=TRUE)))

soil.sl<-lapply(1:length(soil.sl),function(i) cbind(soil.sl[[i]],soil.z.p[[i]]))

#1.2 - plant-soil slice

plant.z.p<-lapply(1:length(plant.sl),function(i) data.frame(z.p=matrix(unlist(round(plant.sl[[i]]$z.point,digits=1)),ncol=1,byrow=TRUE)))

plant.sl<-lapply(1:length(plant.sl),function(i) cbind(plant.sl[[i]],plant.z.p[[i]]))


####################
#1.3 - round z.points
ct3cb.z.p<-lapply(1:length(ct3.cb),function(i) data.frame(z.p=matrix(unlist(round(ct3.cb[[i]]$z.point.cb,digits=1)),ncol=1,byrow=TRUE)))
ct3.cb<-lapply(1:length(ct3.cb),function(i) cbind(ct3.cb[[i]], ct3cb.z.p[[i]]))

#1.4 - merge data frames

ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) merge(ct3.cb[[i]],soil.sl[[j]],by="z.p",all.x=TRUE)))


ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) merge(ct3.cb[[i]],plant.sl[[j]],by="z.p",all.x=TRUE)))

#1.5 - order rows by slice number

ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) ct3cb.fallow[[i]][[j]][order(ct3cb.fallow[[i]][[j]]$slice,na.last=FALSE),]))

ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) ct3cb.plant[[i]][[j]][order(ct3cb.plant[[i]][[j]]$slice,na.last=FALSE),]))



#######################################################################################################
#2-calculate cribwall reinforcement due to member pullout
##########################################################
#implement member by member to avoid function collapse

#2.1 - under fallow conditions

		#2.1.1 - calculate unit weigth of soil
Uw.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Uw=matrix(Uw.w.f(SM=ct3cb.fallow[[i]][[j]]$SM,Gs=2.65,Ww=9.98,e=ct3cb.fallow[[i]][[j]]$n.s/(1-ct3cb.fallow[[i]][[j]]$n.s)),ncol=1,byrow=TRUE))))

ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) cbind(ct3cb.fallow[[i]][[j]],Uw.fallow[[i]][[j]])))


		#2.1.2 - implement function (member by memeber)
Fgr.pll.m1.ct3.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fgr.pll.m1=matrix(Fgr.pll.f(z=ct3cb.fallow[[i]][[j]]$dtm.m1,Uw=ct3cb.fallow[[i]][[j]]$Uw,Le=ct3cb.fallow[[i]][[j]]$rfL.m1,f=0.75,FrA=(ct3cb.fallow[[i]][[j]]$FrA)*pi/180),ncol=1,byrow=TRUE))))

Fgr.pll.m2.ct3.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fgr.pll.m2=matrix(Fgr.pll.f(z=ct3cb.fallow[[i]][[j]]$dtm.m2,Uw=ct3cb.fallow[[i]][[j]]$Uw,Le=ct3cb.fallow[[i]][[j]]$rfL.m2,f=0.75,FrA=(ct3cb.fallow[[i]][[j]]$FrA)*pi/180),ncol=1,byrow=FALSE))))

Fgr.pll.m3.ct3.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fgr.pll.m3=matrix(Fgr.pll.f(z=ct3cb.fallow[[i]][[j]]$dtm.m3,Uw=ct3cb.fallow[[i]][[j]]$Uw,Le=ct3cb.fallow[[i]][[j]]$rfL.m3,f=0.75,FrA=(ct3cb.fallow[[i]][[j]]$FrA)*pi/180),ncol=1,byrow=FALSE))))

Fgr.pll.m4.ct3.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fgr.pll.m4=matrix(Fgr.pll.f(z=ct3cb.fallow[[i]][[j]]$dtm.m4,Uw=ct3cb.fallow[[i]][[j]]$Uw,Le=ct3cb.fallow[[i]][[j]]$rfL.m4,f=0.75,FrA=(ct3cb.fallow[[i]][[j]]$FrA)*pi/180),ncol=1,byrow=FALSE))))

	#2.1.3 - update data frame
	
ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) cbind(ct3cb.fallow[[i]][[j]],Fgr.pll.m1.ct3.fallow[[i]][[j]],Fgr.pll.m2.ct3.fallow[[i]][[j]],Fgr.pll.m3.ct3.fallow[[i]][[j]],Fgr.pll.m4.ct3.fallow[[i]][[j]])))

#2.2 - under vegetated conditions
#------------------//////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\------------------------
Uw.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Uw=matrix(Uw.w.f(SM=ct3cb.plant[[i]][[j]]$SM,Gs=2.65,Ww=9.98,e=ct3cb.plant[[i]][[j]]$n.s/(1-ct3cb.plant[[i]][[j]]$n.s)),ncol=1,byrow=TRUE))))

ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) cbind(ct3cb.plant[[i]][[j]],Uw.plant[[i]][[j]])))


		#2.1.2 - implement function
Fgr.pll.m1.ct3.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fgr.pll.m1=matrix(Fgr.pll.f(z=ct3cb.plant[[i]][[j]]$dtm.m1,Uw=ct3cb.plant[[i]][[j]]$Uw,Le=ct3cb.plant[[i]][[j]]$rfL.m1,f=0.75,FrA=(ct3cb.plant[[i]][[j]]$FrA)*pi/180),ncol=1,byrow=TRUE))))

Fgr.pll.m2.ct3.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fgr.pll.m2=matrix(Fgr.pll.f(z=ct3cb.plant[[i]][[j]]$dtm.m2,Uw=ct3cb.plant[[i]][[j]]$Uw,Le=ct3cb.plant[[i]][[j]]$rfL.m2,f=0.75,FrA=(ct3cb.plant[[i]][[j]]$FrA)*pi/180),ncol=1,byrow=FALSE))))

Fgr.pll.m3.ct3.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fgr.pll.m3=matrix(Fgr.pll.f(z=ct3cb.plant[[i]][[j]]$dtm.m3,Uw=ct3cb.plant[[i]][[j]]$Uw,Le=ct3cb.plant[[i]][[j]]$rfL.m3,f=0.75,FrA=(ct3cb.plant[[i]][[j]]$FrA)*pi/180),ncol=1,byrow=FALSE))))

Fgr.pll.m4.ct3.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fgr.pll.m4=matrix(Fgr.pll.f(z=ct3cb.plant[[i]][[j]]$dtm.m4,Uw=ct3cb.plant[[i]][[j]]$Uw,Le=ct3cb.plant[[i]][[j]]$rfL.m4,f=0.75,FrA=(ct3cb.plant[[i]][[j]]$FrA)*pi/180),ncol=1,byrow=FALSE))))

	#2.1.3 - update data frame
	
ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) cbind(ct3cb.plant[[i]][[j]],Fgr.pll.m1.ct3.plant[[i]][[j]],Fgr.pll.m2.ct3.plant[[i]][[j]],Fgr.pll.m3.ct3.plant[[i]][[j]],Fgr.pll.m4.ct3.plant[[i]][[j]])))

#########################################################################################################
#3-calculate cribwall reinforcement due to member breakage
############################################################
#Comment: no timber decay considered
#it is the same under fallow and vegetated conditions

#3.1) Calculate resistance to bending
#assuming: no decay
#C14: coniferus timber 14 N/mm2 -> 14*10e06/10-03

Rt<-sapply(1:nrow(met.ts), function(i) Rt.f(D=0.25,dt=cumsum(met.ts$dt.mm.rcp45[i])/1000,fd=14*1000)) #21.47 kN.m

#3.2) calculate bending strength of timber member
	
BnSt<-sapply(1:nrow(met.ts),function(i) BnSt.f(Rt=Rt[i],Dm=0.25-(cumsum(met.ts$dt.mm.rcp45[i])/1000),Lm=1.5)) #291.66 kN/m2 - 

#this is the same for the four memebers and has to be compared agaisnt pull-out, and the smallest chosen


#3.3) calculate bending strenght in the light of the reinforcement length (kN/m)

Fgr.bn.ct3cb<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Fgr.bn.f(BnSt=BnSt[j],Le=ct3cb.plant[[i]][[j]][,11:14])))

#3.4) update data frames

Fgr.bn.ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fgr.bn.m1=Fgr.bn.ct3cb[[i]][[j]][,1],Fgr.bn.m2=Fgr.bn.ct3cb[[i]][[j]][,2],Fgr.bn.m3=Fgr.bn.ct3cb[[i]][[j]][,3],Fgr.bn.m4=Fgr.bn.ct3cb[[i]][[j]][,4])))

Fgr.bn.ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Fgr.bn.m1=Fgr.bn.ct3cb[[i]][[j]][,1],Fgr.bn.m2=Fgr.bn.ct3cb[[i]][[j]][,2],Fgr.bn.m3=Fgr.bn.ct3cb[[i]][[j]][,3],Fgr.bn.m4=Fgr.bn.ct3cb[[i]][[j]][,4])))

ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) cbind(ct3cb.fallow[[i]][[j]],Fgr.bn.ct3cb.fallow[[i]][[j]])))

ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) cbind(ct3cb.plant[[i]][[j]],Fgr.bn.ct3cb.plant[[i]][[j]])))


########################################################################################################
#4-calculation of lateral earth pressure; related sliding and overturning forces
####################################################################################
#4.1. slope fill
cb.fill<-data.frame(x1=8,y1=10,x2=16,y2=14)
sl.fill<-(cb.fill$y2-cb.fill$y1)/(cb.fill$x2-cb.fill$x1)

		
	#4.2. volume of timber in the cribwall
Vt.cb<-sapply(1:nrow(met.ts),function(i) Vt.cb.f(Dm=0.25-(cumsum(met.ts$dt.mm.rcp45[i])/1000),T.Lcb=((7+6+6)*1.5+(8*5)*2))) #m3


	#4.3. total cribwall volume
Vcb<-Vcb.f(Hcb=1.5,Lcb=1.5,Wd.cb=10)

	#4.4. self-weight of cribwall and filling soil - cribwall built in slice 5
	#timber density: 550 kg/m3

Wcb.ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Wcb= sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) ifelse(ct3cb.fallow[[i]][[j]]$slice[k]==5,Wcb.f(Vt.cb=Vt.cb[j],Uw.t=550*9.8,Vs=Vcb-Vt.cb[j],Uw.s=ct3cb.fallow[[i]][[j]]$Uw[k],Vcb=Vcb,Hcb=ct3cb.fallow[[i]][[j]]$z.point.cb[k],Lcb=1.5,aa=ct3cb.fallow[[i]][[j]]$alpha[k]*pi/180)/1000,0)))))	 #kN/m - it seems FoS subequations are implemented in terms of N/m (force over length)	


	#4.5. - weight of the soil wedge
		
Ws.ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Ws=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) ifelse(ct3cb.fallow[[i]][[j]]$slice[k],Ws.f(Uw=ct3cb.fallow[[i]][[j]]$Uw[k],L=ct3cb.fallow[[i]][[j]]$L.arc[k],Hss=ct3cb.fallow[[i]][[j]]$z.point.cb[k],alpha=ct3cb.fallow[[i]][[j]]$alpha[k]*pi/180,SRCH=0),0)))))

		#4.6. - weight of soil & cribwall where appropiate

Ws_cb.ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Ws_cb=Wcb.ct3cb.fallow[[i]][[j]]+Ws.ct3cb.fallow[[i]][[j]])))

#note slices start counting at x=2, so cribwall is actually located in slice 4

#4.7. weight of cribwall + filling soil, only registered at cribwall slice

Wcb.ct3cb.fallow.ii<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Wcb= sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) ifelse(ct3cb.fallow[[i]][[j]]$slice[k]==5,(Wcb.f(Vt.cb=Vt.cb[j],Uw.t=550*9.8,Vs=Vcb-Vt.cb[j],Uw.s=ct3cb.fallow[[i]][[j]]$Uw[k],Vcb=Vcb,Hcb=ct3cb.fallow[[i]][[j]]$z.point.cb[k],Lcb=1.5,aa=ct3cb.fallow[[i]][[j]]$alpha[k]*pi/180)/1000)+Ws.ct3cb.fallow[[i]][[j]]$Ws[k],0)))))



#-------------------------------------------------------------------------
#4.8 - lateral earth pressure - only evalauted on the 3 reinforced slices 
#-------------------------------------------------------------------------

#4.8.1 set scenarios for different water tables

wt.d<-c(0.1,1,2) #m from ground level

#4.8.2. Calculation - fallow soil


Ea.ct3.fallow.wt.1<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(Ea_kN_m=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k)	 Ea.f.2(FrA=ct3cb.fallow[[i]][[j]]$FrA[k]*pi/180,H=ct3cb.fallow[[i]][[j]]$z.point.x[k],Uw=9.8,Uw.s=ct3cb.fallow[[i]][[j]]$Uw[k],rho=1000,gr=9.81,wt.h=ct3cb.fallow[[i]][[j]]$z.point.x[k]-wt.d[1],ln=5,dm=0.25,nr=12,dns=550,WV=0,sch.cw=Ws_cb.ct3cb.fallow[[i]][[j]]$Wcb[k],sl.wd=2)))))

Ea.ct3.fallow.wt.2<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(Ea_kN_m=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k)	 Ea.f.2(FrA=ct3cb.fallow[[i]][[j]]$FrA[k]*pi/180,H=ct3cb.fallow[[i]][[j]]$z.point.x[k],Uw=9.8,Uw.s=ct3cb.fallow[[i]][[j]]$Uw[k],rho=1000,gr=9.81,wt.h=ct3cb.fallow[[i]][[j]]$z.point.x[k]-wt.d[2],ln=5,dm=0.25,nr=12,dns=550,WV=0,sch.cw=Ws_cb.ct3cb.fallow[[i]][[j]]$Wcb[k],sl.wd=2)))))

Ea.ct3.fallow.wt.3<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(Ea_kN_m=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k)	 Ea.f.2(FrA=ct3cb.fallow[[i]][[j]]$FrA[k]*pi/180,H=ct3cb.fallow[[i]][[j]]$z.point.x[k],Uw=9.8,Uw.s=ct3cb.fallow[[i]][[j]]$Uw[k],rho=1000,gr=9.81,wt.h=ct3cb.fallow[[i]][[j]]$z.point.x[k]-wt.d[3],ln=5,dm=0.25,nr=12,dns=550,WV=0,sch.cw=Ws_cb.ct3cb.fallow[[i]][[j]]$Wcb[k],sl.wd=2)))))


Ea.ct3.fallow.wt.0<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(Ea_kN_m=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k)	 Ea.f.2(FrA=ct3cb.fallow[[i]][[j]]$FrA[k]*pi/180,H=ct3cb.fallow[[i]][[j]]$z.point.x[k],Uw=9.8,Uw.s=ct3cb.fallow[[i]][[j]]$Uw[k],rho=1000,gr=9.81,wt.h=0,ln=5,dm=0.25,nr=12,dns=550,WV=0,sch.cw=Ws_cb.ct3cb.fallow[[i]][[j]]$Wcb[k],sl.wd=2)))))



#4.8.2. Calculation - vegetated soil


Ea.ct3.plant.wt.1<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(Ea_kN_m=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k)	 Ea.f.2(FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,H=ct3cb.plant[[i]][[j]]$z.point.x[k],Uw=9.8,Uw.s=ct3cb.plant[[i]][[j]]$Uw[k],rho=1000,gr=9.81,wt.h=ct3cb.plant[[i]][[j]]$z.point.x[k]-wt.d[1],ln=5,dm=0.25,nr=12,dns=550,WV=ct3cb.plant[[i]][[j]]$WV_kPa[k]*ct3cb.plant[[i]][[j]]$z.point.x[k],sch.cw=Ws_cb.ct3cb.fallow[[i]][[j]]$Wcb[k],sl.wd=2)))))

Ea.ct3.plant.wt.2<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(Ea_kN_m=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k)	 Ea.f.2(FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,H=ct3cb.plant[[i]][[j]]$z.point.x[k],Uw=9.8,Uw.s=ct3cb.plant[[i]][[j]]$Uw[k],rho=1000,gr=9.81,wt.h=ct3cb.plant[[i]][[j]]$z.point.x[k]-wt.d[2],ln=5,dm=0.25,nr=12,dns=550,WV=ct3cb.plant[[i]][[j]]$WV_kPa[k]*ct3cb.plant[[i]][[j]]$z.point.x[k],sch.cw=Ws_cb.ct3cb.fallow[[i]][[j]]$Wcb[k],sl.wd=2)))))

Ea.ct3.plant.wt.3<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(Ea_kN_m=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) Ea.f.2(FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,H=ct3cb.plant[[i]][[j]]$z.point.x[k],Uw=9.8,Uw.s=ct3cb.plant[[i]][[j]]$Uw[k],rho=1000,gr=9.81,wt.h=ct3cb.plant[[i]][[j]]$z.point.x[k]-wt.d[3],ln=5,dm=0.25,nr=12,dns=550,WV=ct3cb.plant[[i]][[j]]$WV_kPa[k]*ct3cb.plant[[i]][[j]]$z.point.x[k],sch.cw=Ws_cb.ct3cb.fallow[[i]][[j]]$Wcb[k],sl.wd=2)))))
#water table is below slip depth, so it returns negative value - needs correcting. 

Ea.ct3.plant.wt.0<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(Ea_kN_m=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k)	 Ea.f.2(FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,H=ct3cb.plant[[i]][[j]]$z.point.x[k],Uw=9.8,Uw.s=ct3cb.plant[[i]][[j]]$Uw[k],rho=1000,gr=9.81,wt.h=0,ln=5,dm=0.25,nr=12,dns=550,WV=ct3cb.plant[[i]][[j]]$WV_kPa[k]*ct3cb.plant[[i]][[j]]$z.point.x[k],sch.cw=Ws_cb.ct3cb.fallow[[i]][[j]]$Wcb[k],sl.wd=2)))))

###########################################
#5) Calculation of overturning resistance
##########################################

#5.1. fallow soil
Ea.r.ov.ct3.fallow.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Ea_r_ov_kN_m=Ea.r.ov.f(Ea=Ea.ct3.fallow.wt.1[[i]][[j]],B=75*pi/180,Sk=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180,H=1.5,CB.inc=5*pi/180,W=Wcb.ct3cb.fallow.ii[[i]][[j]]))))

Ea.r.ov.ct3.fallow.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Ea_r_ov_kN_m=Ea.r.ov.f(Ea=Ea.ct3.fallow.wt.2[[i]][[j]],B=75*pi/180,Sk=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180,H=1.5,CB.inc=5*pi/180,W=Wcb.ct3cb.fallow.ii[[i]][[j]]))))

Ea.r.ov.ct3.fallow.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Ea_r_ov_kN_m=Ea.r.ov.f(Ea=Ea.ct3.fallow.wt.3[[i]][[j]],B=75*pi/180,Sk=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180,H=1.5,CB.inc=5*pi/180,W=Wcb.ct3cb.fallow.ii[[i]][[j]]))))

Ea.r.ov.ct3.fallow.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Ea_r_ov_kN_m=Ea.r.ov.f(Ea=Ea.ct3.fallow.wt.0[[i]][[j]],B=75*pi/180,Sk=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180,H=1.5,CB.inc=5*pi/180,W=Wcb.ct3cb.fallow.ii[[i]][[j]]))))

#5.2. vegetated

Ea.r.ov.ct3.plant.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.ov.f(Ea=Ea.ct3.plant.wt.1[[i]][[j]],B=75*pi/180,Sk=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180,H=1.5,CB.inc=5*pi/180,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))

Ea.r.ov.ct3.plant.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.ov.f(Ea=Ea.ct3.plant.wt.2[[i]][[j]],B=75*pi/180,Sk=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180,H=1.5,CB.inc=5*pi/180,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))

Ea.r.ov.ct3.plant.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.ov.f(Ea=abs(Ea.ct3.plant.wt.3[[i]][[j]]),B=75*pi/180,Sk=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180,H=1.5,CB.inc=5*pi/180,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))

Ea.r.ov.ct3.plant.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.ov.f(Ea=Ea.ct3.plant.wt.0[[i]][[j]],B=75*pi/180,Sk=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180,H=1.5,CB.inc=5*pi/180,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))



#####################################################################################################
#6) - calculation of overtunrning driving force due to Ea (under different water table height cases)
#####################################################################################################

#6.1. fallow

Ea.ov.ct3.fallow.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.ov.f(Ea=Ea.ct3.fallow.wt.1[[i]][[j]],B=75*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180,H=1.5,Lcb=1.5,CB.inc=5*pi/180)))

Ea.ov.ct3.fallow.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.ov.f(Ea=Ea.ct3.fallow.wt.2[[i]][[j]],B=75*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180,H=1.5,Lcb=1.5,CB.inc=5*pi/180)))

Ea.ov.ct3.fallow.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.ov.f(Ea=Ea.ct3.fallow.wt.3[[i]][[j]],B=75*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180,H=1.5,Lcb=1.5,CB.inc=5*pi/180)))

Ea.ov.ct3.fallow.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.ov.f(Ea=Ea.ct3.fallow.wt.0[[i]][[j]],B=75*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180,H=1.5,Lcb=1.5,CB.inc=5*pi/180)))



#6.2. vegetated
#------
Ea.ov.ct3.plant.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.ov.f(Ea=Ea.ct3.plant.wt.1[[i]][[j]],B=75*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180,H=1.5,Lcb=1.5,CB.inc=5*pi/180)))

Ea.ov.ct3.plant.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.ov.f(Ea=Ea.ct3.plant.wt.2[[i]][[j]],B=75*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180,H=1.5,Lcb=1.5,CB.inc=5*pi/180)))

Ea.ov.ct3.plant.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.ov.f(Ea=abs(Ea.ct3.plant.wt.3[[i]][[j]]),B=75*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180,H=1.5,Lcb=1.5,CB.inc=5*pi/180)))

Ea.ov.ct3.plant.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.ov.f(Ea=Ea.ct3.plant.wt.0[[i]][[j]],B=75*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180,H=1.5,Lcb=1.5,CB.inc=5*pi/180)))

##########################################################################
#7) crib wall overturning check - FoS against overturning - [overtuning under saturated conditions]
##########################################################################

#fallow
FoS.cb.ov.fallow.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.ov.ct3.fallow.wt.1[[i]][[j]]/Ea.ov.ct3.fallow.wt.1[[i]][[j]]))

FoS.cb.ov.fallow.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.ov.ct3.fallow.wt.2[[i]][[j]]/Ea.ov.ct3.fallow.wt.2[[i]][[j]]))

FoS.cb.ov.fallow.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.ov.ct3.fallow.wt.3[[i]][[j]]/Ea.ov.ct3.fallow.wt.3[[i]][[j]]))

FoS.cb.ov.fallow.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.ov.ct3.fallow.wt.0[[i]][[j]]/Ea.ov.ct3.fallow.wt.0[[i]][[j]]))


#vegetated

FoS.cb.ov.plant.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.ov.ct3.plant.wt.1[[i]][[j]]/Ea.ov.ct3.plant.wt.1[[i]][[j]]))

FoS.cb.ov.plant.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.ov.ct3.plant.wt.2[[i]][[j]]/Ea.ov.ct3.plant.wt.2[[i]][[j]]))

FoS.cb.ov.plant.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.ov.ct3.plant.wt.3[[i]][[j]]/Ea.ov.ct3.plant.wt.3[[i]][[j]]))

FoS.cb.ov.plant.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.ov.ct3.plant.wt.0[[i]][[j]]/Ea.ov.ct3.plant.wt.0[[i]][[j]]))

########################
#save output
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs/FoS_overturning")
save(FoS.cb.ov.fallow.wt.1,file="FoS_cb_ov_fallow_wt_1.RData")
save(FoS.cb.ov.fallow.wt.2,file="FoS_cb_ov_fallow_wt_2.RData")
save(FoS.cb.ov.fallow.wt.3,file="FoS_cb_ov_fallow_wt_3.RData")
save(FoS.cb.ov.fallow.wt.0,file="FoS_cb_ov_fallow_wt_0.RData")
save(FoS.cb.ov.plant.wt.1,file="FoS_cb_ov_plant_wt_1.RData")
save(FoS.cb.ov.plant.wt.2,file="FoS_cb_ov_plant_wt_2.RData")
save(FoS.cb.ov.plant.wt.3,file="FoS_cb_ov_plant_wt_3.RData")
save(FoS.cb.ov.plant.wt.0,file="FoS_cb_ov_plant_wt_0.RData")

###################################################################
#8) Calculate sliding resistance
#################################


#8.1. set coehison data.frame vs w.t [long-hand]

wt.d<-c(0.1,1,2)
c.wt.1<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(cs_kPa=sapply(1:nrow(ct3cb.fallow[[i]][[j]]), function(k) ifelse(ct3cb.fallow[[i]][[j]]$z.point.cb[k] > wt.d[1],0,ct3cb.fallow[[i]][[j]]$c.s[k])))))
c.wt.2<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(cs_kPa=sapply(1:nrow(ct3cb.fallow[[i]][[j]]), function(k) ifelse(ct3cb.fallow[[i]][[j]]$z.point.cb[k] > wt.d[2],0,ct3cb.fallow[[i]][[j]]$c.s[k])))))
c.wt.3<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(cs_kPa=sapply(1:nrow(ct3cb.fallow[[i]][[j]]), function(k) ifelse(ct3cb.fallow[[i]][[j]]$z.point.cb[k] > wt.d[3],0,ct3cb.fallow[[i]][[j]]$c.s[k])))))

#fallow soil

Ea.r.s.ct3.fallow.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.s.f(Ea=Ea.ct3.fallow.wt.1[[i]][[j]],B=100*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA*pi/180,Sp=0,c=c.wt.1[[i]][[j]]$cs_kPa,Lcb=1.5,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))
Ea.r.s.ct3.fallow.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.s.f(Ea=Ea.ct3.fallow.wt.2[[i]][[j]],B=100*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA*pi/180,Sp=0,c=c.wt.2[[i]][[j]]$cs_kPa,Lcb=1.5,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))
Ea.r.s.ct3.fallow.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.s.f(Ea=abs(Ea.ct3.fallow.wt.3[[i]][[j]]),B=100*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA*pi/180,Sp=0,c=c.wt.3[[i]][[j]]$cs_kPa,Lcb=1.5,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))
Ea.r.s.ct3.fallow.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.s.f(Ea=Ea.ct3.fallow.wt.0[[i]][[j]],B=100*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA*pi/180,Sp=0,c=ct3cb.fallow[[i]][[j]]$c.s,Lcb=1.5,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))

#vegetated soil

Ea.r.s.ct3.plant.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.s.f(Ea=Ea.ct3.plant.wt.1[[i]][[j]],B=100*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA*pi/180,Sp=0,c=c.wt.1[[i]][[j]]$cs_kPa+1.2*0.4*ct3cb.plant[[i]][[j]]$Tr_kPa,Lcb=1.5,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))
Ea.r.s.ct3.plant.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.s.f(Ea=Ea.ct3.plant.wt.2[[i]][[j]],B=100*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA*pi/180,Sp=0,c=c.wt.2[[i]][[j]]$cs_kPa+1.2*0.4*ct3cb.plant[[i]][[j]]$Tr_kPa,Lcb=1.5,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))
Ea.r.s.ct3.plant.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.s.f(Ea=abs(Ea.ct3.plant.wt.3[[i]][[j]]),B=100*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA*pi/180,Sp=0,c=c.wt.3[[i]][[j]]$cs_kPa+1.2*0.4*ct3cb.plant[[i]][[j]]$Tr_kPa,Lcb=1.5,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))
Ea.r.s.ct3.plant.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.s.f(Ea=Ea.ct3.plant.wt.0[[i]][[j]],B=100*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA*pi/180,Sp=0,c=ct3cb.fallow[[i]][[j]]$c.s+1.2*0.4*ct3cb.plant[[i]][[j]]$Tr_kPa,Lcb=1.5,W=Wcb.ct3cb.fallow.ii[[i]][[j]])))


####################################################
#9) calculation of sliding driving force due to Ea
####################################################

#fallow soil
Ea.s.ct3.fallow.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.s.f(Ea=Ea.ct3.fallow.wt.1[[i]][[j]],B=100*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180)))
Ea.s.ct3.fallow.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.s.f(Ea=Ea.ct3.fallow.wt.2[[i]][[j]],B=100*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180)))
Ea.s.ct3.fallow.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.s.f(Ea=abs(Ea.ct3.fallow.wt.3[[i]][[j]]),B=100*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180)))
Ea.s.ct3.fallow.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.s.f(Ea=Ea.ct3.fallow.wt.0[[i]][[j]],B=100*pi/180,sK=0.75*ct3cb.fallow[[i]][[j]]$FrA[4]*pi/180)))


#vegetated soil
Ea.s.ct3.plant.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.s.f(Ea=Ea.ct3.plant.wt.1[[i]][[j]],B=90*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180)))
Ea.s.ct3.plant.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.s.f(Ea=Ea.ct3.plant.wt.2[[i]][[j]],B=90*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180)))
Ea.s.ct3.plant.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.s.f(Ea=abs(Ea.ct3.plant.wt.3[[i]][[j]]),B=90*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180)))
Ea.s.ct3.plant.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.s.f(Ea=Ea.ct3.plant.wt.0[[i]][[j]],B=90*pi/180,sK=0.75*ct3cb.plant[[i]][[j]]$FrA[4]*pi/180)))


########################################################################################################
#10) crib wall sliding check - FoS cribwall sliding
########################################################################################################
FoS.cb.s.fallow.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.s.ct3.fallow.wt.1[[i]][[j]]/Ea.s.ct3.fallow.wt.1[[i]][[j]]))
FoS.cb.s.fallow.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.s.ct3.fallow.wt.2[[i]][[j]]/Ea.s.ct3.fallow.wt.2[[i]][[j]]))
FoS.cb.s.fallow.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.s.ct3.fallow.wt.3[[i]][[j]]/Ea.s.ct3.fallow.wt.3[[i]][[j]]))
FoS.cb.s.fallow.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) Ea.r.s.ct3.fallow.wt.0[[i]][[j]]/Ea.s.ct3.fallow.wt.0[[i]][[j]]))

FoS.cb.s.plant.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.s.ct3.plant.wt.1[[i]][[j]]/Ea.s.ct3.plant.wt.1[[i]][[j]]))
FoS.cb.s.plant.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.s.ct3.plant.wt.2[[i]][[j]]/Ea.s.ct3.plant.wt.2[[i]][[j]]))
FoS.cb.s.plant.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.s.ct3.plant.wt.3[[i]][[j]]/Ea.s.ct3.plant.wt.3[[i]][[j]]))
FoS.cb.s.plant.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) Ea.r.s.ct3.plant.wt.0[[i]][[j]]/Ea.s.ct3.plant.wt.0[[i]][[j]]))

#output
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs/FoS_sliding")
save(FoS.cb.s.fallow.wt.1,file="FoS_cb_s_fallow_wt_1.RData")
save(FoS.cb.s.fallow.wt.2,file="FoS_cb_s_fallow_wt_2.RData")
save(FoS.cb.s.fallow.wt.3,file="FoS_cb_s_fallow_wt_3.RData")
save(FoS.cb.s.fallow.wt.0,file="FoS_cb_s_fallow_wt_0.RData")
save(FoS.cb.s.plant.wt.1,file="FoS_cb_s_plant_wt_1.RData")
save(FoS.cb.s.plant.wt.2,file="FoS_cb_s_plant_wt_2.RData")
save(FoS.cb.s.plant.wt.3,file="FoS_cb_s_plant_wt_3.RData")
save(FoS.cb.s.plant.wt.0,file="FoS_cb_s_plant_wt_0.RData")



########################################################################################################
#11) calculation of global resisting forces
######################################################

#unit weight of timber - C14 (conifer: density: 290 kg/m3 - Eurocode 5)


###########################################
#11.1 - resisting force without vegetation
###########################################
#11.1.1 - self-weight cribwall and soil
		
	#volume of timber in the cribwall

Vt.cb<-sapply(1:nrow(met.ts),function(i) Vt.cb.f(Dm=0.25-(cumsum(met.ts$dt.mm.rcp45[i])/1000),T.Lcb=((7+6+6)*1.5+(8*5)*2))) #m3
	#total cribwall volume

Vcb<-Vcb.f(Hcb=1.5,Lcb=1.5,Wd.cb=10)
	
	
#--------------------------------------------------------------------
#11.1.2 - Suction stress


#wt.d<-0.1

SS.ct3cb.fallow.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(SS_kPa=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) SS.f(MS=max(ct3cb.fallow[[i]][[j]]$MS_w_kPa[k],ct3cb.fallow[[i]][[j]]$MS_d_kPa[k]),a=ct3cb.fallow[[i]][[j]]$a.van[k],n.van=ct3cb.fallow[[i]][[j]]$n.van[k])))))


SS.ct3cb.fallow.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(SS_kPa=matrix(SS.f(MS=ct3cb.fallow[[i]][[j]]$MS_unified_1_kPa,a=ct3cb.fallow[[i]][[j]]$a.van,n.van=ct3cb.fallow[[i]][[j]]$n.van),ncol=1,byrow=TRUE))))
SS.ct3cb.fallow.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(SS_kPa=matrix(SS.f(MS=ct3cb.fallow[[i]][[j]]$MS_unified_2_kPa,a=ct3cb.fallow[[i]][[j]]$a.van,n.van=ct3cb.fallow[[i]][[j]]$n.van),ncol=1,byrow=TRUE))))
SS.ct3cb.fallow.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(SS_kPa=matrix(SS.f(MS=ct3cb.fallow[[i]][[j]]$MS_unified_3_kPa,a=ct3cb.fallow[[i]][[j]]$a.van,n.van=ct3cb.fallow[[i]][[j]]$n.van),ncol=1,byrow=TRUE))))

#20/06/25:: note: negative SS is produced by suction, positive SS is produced by positive pore-water pressure


#11.1.3 - resisting force

#no water table

	Fr.ct3cb.fallow.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fr=Fr.f(cR=0,c=ct3cb.fallow[[i]][[j]]$c.s,Ln=ct3cb.fallow[[i]][[j]]$L.arc,W=Ws_cb.ct3cb.fallow[[i]][[j]],alpha=ct3cb.fallow[[i]][[j]]$alpha*pi/180,SS=SS.ct3cb.fallow.wt.0[[i]][[j]][,1],FrA=ct3cb.fallow[[i]][[j]]$FrA*pi/180,bR=0,lR1=0,lR2=0))))
	
#water table 1
	Fr.ct3cb.fallow.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fr=Fr.f(cR=0,c=c.wt.1[[i]][[j]],Ln=ct3cb.fallow[[i]][[j]]$L.arc,W=Ws_cb.ct3cb.fallow[[i]][[j]],alpha=ct3cb.fallow[[i]][[j]]$alpha*pi/180,SS=SS.ct3cb.fallow.wt.1[[i]][[j]][,1],FrA=ct3cb.fallow[[i]][[j]]$FrA*pi/180,bR=0,lR1=0,lR2=0))))
	
#water table 2
	Fr.ct3cb.fallow.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fr=Fr.f(cR=0,c=c.wt.2[[i]][[j]],Ln=ct3cb.fallow[[i]][[j]]$L.arc,W=Ws_cb.ct3cb.fallow[[i]][[j]],alpha=ct3cb.fallow[[i]][[j]]$alpha*pi/180,SS=SS.ct3cb.fallow.wt.2[[i]][[j]][,1],FrA=ct3cb.fallow[[i]][[j]]$FrA*pi/180,bR=0,lR1=0,lR2=0))))
	
#water table 3
	
	Fr.ct3cb.fallow.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fr=Fr.f(cR=0,c=c.wt.3[[i]][[j]],Ln=ct3cb.fallow[[i]][[j]]$L.arc,W=Ws_cb.ct3cb.fallow[[i]][[j]],alpha=ct3cb.fallow[[i]][[j]]$alpha*pi/180,SS=SS.ct3cb.fallow.wt.3[[i]][[j]][,1],FrA=ct3cb.fallow[[i]][[j]]$FrA*pi/180,bR=0,lR1=0,lR2=0))))
	
###########################################
#12 - resisting force with vegetation
##########################################
#12.1 - self-weight cribwall and soil


#adding more unit weight to the timber, from 290 to 550 N/m
	#self-weight of cribwall and filling soil

Wcb.ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Wcb= sapply(1:nrow(ct3cb.plant[[i]][[j]]),function(k) ifelse(ct3cb.plant[[i]][[j]]$slice[k] <=3,Wcb.f(Vt.cb=Vt.cb[j],Uw.t=550*9.8/1000,Vs=Vcb-Vt.cb[j],Uw.s=ct3cb.plant[[i]][[j]]$Uw[k],Vcb=Vcb,Hcb=ct3cb.plant[[i]][[j]]$z.point.cb[k],Lcb=1.5,aa=ct3cb.plant[[i]][[j]]$alpha*pi/180),0))) ))		


	#12.2- weight of the soil wedge
		
Ws.ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Ws=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) ifelse(ct3cb.plant[[i]][[j]]$slice[k] > 3,Ws.f(Uw=ct3cb.plant[[i]][[j]]$Uw[k],L=ct3cb.plant[[i]][[j]]$L.arc[k],Hss=ct3cb.plant[[i]][[j]]$z.point.cb[k],alpha=ct3cb.plant[[i]][[j]]$alpha[k]*pi/180,SRCH=ct3cb.plant[[i]][[j]]$WV_kPa[k]),0)))))

		#12.3- weight of soil & cribwall where appropiate

Ws_cb.ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Ws_cb=Wcb.ct3cb.plant[[i]][[j]]+Ws.ct3cb.plant[[i]][[j]])))


#12.4 - Suction stress


SS.ct3cb.plant.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(SS_kPa=sapply(1:nrow(ct3cb.plant[[i]][[j]]),function(k) SS.f(MS=ifelse(ct3cb.plant[[i]][[j]]$qST_m_h[k]>0,max(ct3cb.plant[[i]][[j]]$MS_ETP_kPa[k],ct3cb.plant[[i]][[j]]$MS_ER_kPa[k])+ct3cb.plant[[i]][[j]]$MS_ST_kPa[k],max(ct3cb.plant[[i]][[j]]$MS_ETP_kPa[k],ct3cb.plant[[i]][[j]]$MS_ER_kPa[k])),a= ct3cb.plant[[i]][[j]]$a.van[k],n.van=ct3cb.plant[[i]][[j]]$n.van[k])))))

SS.ct3cb.plant.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(SS_kPa=matrix(SS.f(MS= ct3cb.plant[[i]][[j]]$MS_unified_1_kPa,a= ct3cb.plant[[i]][[j]]$a.van,n.van= ct3cb.plant[[i]][[j]]$n.van),ncol=1,byrow=TRUE))))
SS.ct3cb.plant.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(SS_kPa=matrix(SS.f(MS= ct3cb.plant[[i]][[j]]$MS_unified_2_kPa,a= ct3cb.plant[[i]][[j]]$a.van,n.van= ct3cb.plant[[i]][[j]]$n.van),ncol=1,byrow=TRUE))))
SS.ct3cb.plant.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(SS_kPa=matrix(SS.f(MS= ct3cb.plant[[i]][[j]]$MS_unified_3_kPa,a= ct3cb.plant[[i]][[j]]$a.van,n.van= ct3cb.plant[[i]][[j]]$n.van),ncol=1,byrow=TRUE))))


#12.5. rooting zone and soil-root bonding

		#12.5.1 - calculate lR1 - arc length with crossing roots
lR1.ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(lR1=lR1.f(Ln=ct3cb.plant[[i]][[j]]$L.arc,RARz=ct3cb.plant[[i]][[j]]$RAR))))

		#12.5.2 - calculate lR2 - root embedment length - each tap root is 0.5 m
#lR2.ct1cb_plant<-lapply(1:length(ct1cb_plant),function(i) lapply(1:4520,function(j)data.frame(lR2=ifelse(ct1cb_plant[[i]][[j]]$z.point.cb >= 0.5,0.5,ifelse(ct1cb_plant[[i]][[j]]$z.point.cb < 0.5 & ct1cb_plant[[i]][[j]]$z.point.cb !=0,0.5-ct1cb_plant[[i]][[j]]$z.point.cb,0)))))

		#12.5.3. soil-root bonding [this should be calculated in the plant slice]
#aasumption root is embeded up to 0.5 m

bR.ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl), function(j) data.frame(bR_kPa=sapply(1:nrow(ct3cb.fallow[[i]][[j]]), function(k) ifelse(ct3cb.plant[[i]][[j]]$z.point.cb[k] > 0.5,bR.f(z=0.5,Uw=ct3cb.plant[[i]][[j]]$Uw[k],FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,f=0.75),ifelse(ct3cb.plant[[i]][[j]]$z.point.cb[k] < 0.5 & ct3cb.plant[[i]][[j]]$z.point.cb[k] != 0,bR.f(z=ct3cb.plant[[i]][[j]]$z.point.cb[k],Uw=ct3cb.plant[[i]][[j]]$Uw[k],FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,f=0.75),0))))))


		#12.5.4 - resisting force

#no water table
	Fr.ct3cb.plant.wt.0<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Fr=sapply(1:nrow(ct3cb.plant[[i]][[j]]), function(k) Fr.f(cR=1.2*0.4*ct3cb.plant[[i]][[j]]$Tr_kPa[k],c=ct3cb.plant[[i]][[j]]$c.s[k],Ln=ct3cb.plant[[i]][[j]]$L.arc[k],W=Ws_cb.ct3cb.plant[[i]][[j]]$Wcb[k],alpha=ct3cb.plant[[i]][[j]]$alpha[k]*pi/180,SS=SS.ct3cb.plant.wt.0[[i]][[j]]$SS[k],FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,bR=bR.ct3cb.plant[[i]][[j]]$bR_kPa[k],lR1=lR1.ct3cb.plant[[i]][[j]]$lR1[k],lR2=1)))))
	
#water table 1
	
	Fr.ct3cb.plant.wt.1<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Fr=sapply(1:nrow(ct3cb.plant[[i]][[j]]), function(k) Fr.f(cR=1.2*0.4*ct3cb.plant[[i]][[j]]$Tr_kPa[k],c=c.wt.1[[i]][[j]]$cs_kPa[k],Ln=ct3cb.plant[[i]][[j]]$L.arc[k],W=Ws_cb.ct3cb.plant[[i]][[j]]$Wcb[k],alpha=ct3cb.plant[[i]][[j]]$alpha[k]*pi/180,SS=SS.ct3cb.plant.wt.1[[i]][[j]]$SS[k],FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,bR=bR.ct3cb.plant[[i]][[j]]$bR_kPa[k],lR1=lR1.ct3cb.plant[[i]][[j]]$lR1[k],lR2=1)))))
	
#water table 2	
	Fr.ct3cb.plant.wt.2<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Fr=sapply(1:nrow(ct3cb.plant[[i]][[j]]), function(k) Fr.f(cR=1.2*0.4*ct3cb.plant[[i]][[j]]$Tr_kPa[k],c=c.wt.2[[i]][[j]]$cs_kPa[k],Ln=ct3cb.plant[[i]][[j]]$L.arc[k],W=Ws_cb.ct3cb.plant[[i]][[j]]$Wcb[k],alpha=ct3cb.plant[[i]][[j]]$alpha[k]*pi/180,SS=SS.ct3cb.plant.wt.2[[i]][[j]]$SS[k],FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,bR=bR.ct3cb.plant[[i]][[j]]$bR_kPa[k],lR1=lR1.ct3cb.plant[[i]][[j]]$lR1[k],lR2=1)))))
	
#water table 3
	Fr.ct3cb.plant.wt.3<-lapply(1:length(ct3.cb),function(i) lapply(1:length(plant.sl),function(j) data.frame(Fr=sapply(1:nrow(ct3cb.plant[[i]][[j]]), function(k) Fr.f(cR=1.2*0.4*ct3cb.plant[[i]][[j]]$Tr_kPa[k],c=c.wt.3[[i]][[j]]$cs_kPa[k],Ln=ct3cb.plant[[i]][[j]]$L.arc[k],W=Ws_cb.ct3cb.plant[[i]][[j]]$Wcb[k],alpha=ct3cb.plant[[i]][[j]]$alpha[k]*pi/180,SS=SS.ct3cb.plant.wt.3[[i]][[j]]$SS[k],FrA=ct3cb.plant[[i]][[j]]$FrA[k]*pi/180,bR=bR.ct3cb.plant[[i]][[j]]$bR_kPa[k],lR1=lR1.ct3cb.plant[[i]][[j]]$lR1[k],lR2=1)))))
	


######################################
#13) Calculation of global Sliding force
######################################
#20/06/25:: note that normal load has been considered to be under saturation


Fs.ct3cb.fallow<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fs=Fs.f(Ws=Ws_cb.ct3cb.fallow[[i]][[j]],alpha=ct3cb.fallow[[i]][[j]]$alpha*pi/180))))

Fs.ct3cb.plant<-lapply(1:length(ct3.cb),function(i) lapply(1:length(soil.sl),function(j) data.frame(Fs=Fs.f(Ws=Ws_cb.ct3cb.plant[[i]][[j]],alpha=ct3cb.plant[[i]][[j]]$alpha*pi/180))))

######################################
#14) Calculation of Factor of Safety
###########################################
#14.1 - slice on slice

FoS.ct3cb.slices.fallow.wt.0<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(FoS_slice=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) ifelse(ct3cb.fallow[[i]][[j]]$slice[k]==5,(Fr.ct3cb.fallow.wt.0[[i]][[j]]$Wcb[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m1[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m2[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m3[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m4[k]+ct3cb.fallow[[i]][[j]]$F.fw.str1[k]+ct3cb.fallow[[i]][[j]]$F.fw.str2[k]+ct3cb.fallow[[i]][[j]]$F.fw.str3[k]+ct3cb.fallow[[i]][[j]]$F.fw.str4[k])/Fs.ct3cb.fallow[[i]][[j]]$Wcb[k],(Fr.ct3cb.fallow.wt.0[[i]][[j]]$Wcb[k]/Fs.ct3cb.fallow[[i]][[j]]$Wcb[k]))))))

FoS.ct3cb.slices.fallow.wt.1<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(FoS_slice=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) ifelse(ct3cb.fallow[[i]][[j]]$slice[k]==5,(Fr.ct3cb.fallow.wt.1[[i]][[j]]$Wcb[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m1[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m2[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m3[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m4[k]+ct3cb.fallow[[i]][[j]]$F.fw.str1[k]+ct3cb.fallow[[i]][[j]]$F.fw.str2[k]+ct3cb.fallow[[i]][[j]]$F.fw.str3[k]+ct3cb.fallow[[i]][[j]]$F.fw.str4[k])/Fs.ct3cb.fallow[[i]][[j]]$Wcb[k],(Fr.ct3cb.fallow.wt.1[[i]][[j]]$Wcb[k]/Fs.ct3cb.fallow[[i]][[j]]$Wcb[k]))))))

FoS.ct3cb.slices.fallow.wt.2<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(FoS_slice=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) ifelse(ct3cb.fallow[[i]][[j]]$slice[k]==5,(Fr.ct3cb.fallow.wt.2[[i]][[j]]$Wcb[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m1[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m2[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m3[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m4[k]+ct3cb.fallow[[i]][[j]]$F.fw.str1[k]+ct3cb.fallow[[i]][[j]]$F.fw.str2[k]+ct3cb.fallow[[i]][[j]]$F.fw.str3[k]+ct3cb.fallow[[i]][[j]]$F.fw.str4[k])/Fs.ct3cb.fallow[[i]][[j]]$Wcb[k],(Fr.ct3cb.fallow.wt.2[[i]][[j]]$Wcb[k]/Fs.ct3cb.fallow[[i]][[j]]$Wcb[k]))))))

FoS.ct3cb.slices.fallow.wt.3<-lapply(1:length(ct3.cb), function(i) lapply(1:length(soil.sl), function(j) data.frame(FoS_slice=sapply(1:nrow(ct3cb.fallow[[i]][[j]]),function(k) ifelse(ct3cb.fallow[[i]][[j]]$slice[k]==5,(Fr.ct3cb.fallow.wt.3[[i]][[j]]$Wcb[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m1[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m2[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m3[k]+ct3cb.fallow[[i]][[j]]$Fgr.pll.m4[k]+ct3cb.fallow[[i]][[j]]$F.fw.str1[k]+ct3cb.fallow[[i]][[j]]$F.fw.str2[k]+ct3cb.fallow[[i]][[j]]$F.fw.str3[k]+ct3cb.fallow[[i]][[j]]$F.fw.str4[k])/Fs.ct3cb.fallow[[i]][[j]]$Wcb[k],(Fr.ct3cb.fallow.wt.3[[i]][[j]]$Wcb[k]/Fs.ct3cb.fallow[[i]][[j]]$Wcb[k]))))))


#----------------
#plant
#---------------

FoS.ct3cb.slices.plant.wt.0<-lapply(1:length(ct3.cb), function(i) lapply(1:length(plant.sl), function(j) data.frame(FoS_slice=sapply(1:nrow(ct3cb.plant[[i]][[j]]),function(k) ifelse(ct3cb.plant[[i]][[j]]$slice[k]==5,(Fr.ct3cb.plant.wt.0[[i]][[j]]$Fr[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m1[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m2[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m3[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m4[k]+ct3cb.plant[[i]][[j]]$F.fw.str1[k]+ct3cb.plant[[i]][[j]]$F.fw.str2[k]+ct3cb.plant[[i]][[j]]$F.fw.str3[k]+ct3cb.plant[[i]][[j]]$F.fw.str4[k])/Fs.ct3cb.plant[[i]][[j]]$Wcb[k],(Fr.ct3cb.plant.wt.0[[i]][[j]]$Fr[k]/Fs.ct3cb.plant[[i]][[j]]$Wcb[k]))))))

FoS.ct3cb.slices.plant.wt.1<-lapply(1:length(ct3.cb), function(i) lapply(1:length(plant.sl), function(j) data.frame(FoS_slice=sapply(1:nrow(ct3cb.plant[[i]][[j]]),function(k) ifelse(ct3cb.plant[[i]][[j]]$slice[k]==5,(Fr.ct3cb.plant.wt.1[[i]][[j]]$Fr[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m1[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m2[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m3[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m4[k]+ct3cb.plant[[i]][[j]]$F.fw.str1[k]+ct3cb.plant[[i]][[j]]$F.fw.str2[k]+ct3cb.plant[[i]][[j]]$F.fw.str3[k]+ct3cb.plant[[i]][[j]]$F.fw.str4[k])/Fs.ct3cb.plant[[i]][[j]]$Wcb[k],(Fr.ct3cb.plant.wt.1[[i]][[j]]$Fr[k]/Fs.ct3cb.plant[[i]][[j]]$Wcb[k]))))))

FoS.ct3cb.slices.plant.wt.2<-lapply(1:length(ct3.cb), function(i) lapply(1:length(plant.sl), function(j) data.frame(FoS_slice=sapply(1:nrow(ct3cb.plant[[i]][[j]]),function(k) ifelse(ct3cb.plant[[i]][[j]]$slice[k]==5,(Fr.ct3cb.plant.wt.2[[i]][[j]]$Fr[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m1[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m2[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m3[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m4[k]+ct3cb.plant[[i]][[j]]$F.fw.str1[k]+ct3cb.plant[[i]][[j]]$F.fw.str2[k]+ct3cb.plant[[i]][[j]]$F.fw.str3[k]+ct3cb.plant[[i]][[j]]$F.fw.str4[k])/Fs.ct3cb.plant[[i]][[j]]$Wcb[k],(Fr.ct3cb.plant.wt.2[[i]][[j]]$Fr[k]/Fs.ct3cb.plant[[i]][[j]]$Wcb[k]))))))

FoS.ct3cb.slices.plant.wt.3<-lapply(1:length(ct3.cb), function(i) lapply(1:length(plant.sl), function(j) data.frame(FoS_slice=sapply(1:nrow(ct3cb.plant[[i]][[j]]),function(k) ifelse(ct3cb.plant[[i]][[j]]$slice[k]==5,(Fr.ct3cb.plant.wt.3[[i]][[j]]$Fr[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m1[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m2[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m3[k]+ct3cb.plant[[i]][[j]]$Fgr.pll.m4[k]+ct3cb.plant[[i]][[j]]$F.fw.str1[k]+ct3cb.plant[[i]][[j]]$F.fw.str2[k]+ct3cb.plant[[i]][[j]]$F.fw.str3[k]+ct3cb.plant[[i]][[j]]$F.fw.str4[k])/Fs.ct3cb.plant[[i]][[j]]$Wcb[k],(Fr.ct3cb.plant.wt.3[[i]][[j]]$Fr[k]/Fs.ct3cb.plant[[i]][[j]]$Wcb[k]))))))

#output slice on slice
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs/FoS_global_slices")
save(FoS.ct3cb.slices.fallow.wt.0,file="FoS_cb_slices_fallow_wt_0_amend.RData")
save(FoS.ct3cb.slices.fallow.wt.1,file="FoS_cb_slices_fallow_wt_1_amend.RData")
save(FoS.ct3cb.slices.fallow.wt.2,file="FoS_cb_slices_fallow_wt_2_amend.RData")
save(FoS.ct3cb.slices.fallow.wt.3,file="FoS_cb_slices_fallow_wt_3_amend.RData")
save(FoS.ct3cb.slices.plant.wt.0,file="FoS_cb_slices_plant_wt_0_amend.RData")
save(FoS.ct3cb.slices.plant.wt.1,file="FoS_cb_slices_plant_wt_1_amend.RData")
save(FoS.ct3cb.slices.plant.wt.2,file="FoS_cb_slices_plant_wt_2_amend.RData")
save(FoS.ct3cb.slices.plant.wt.3,file="FoS_cb_slices_plant_wt_3_amend.RData")


#------------------------------------------------------------------------
#14.2 - global; includes face wall and cribwall memebers reinforcement
#------------------------------------------------------------------------


#fallow
FoS.ct3cb.fallow.wt.0<-lapply(1:length(ct3.cb),function(i)lapply(1:length(soil.sl),function(j) data.frame(FoS=FoS.f(Fr=sum(Fr.ct3cb.fallow.wt.0[[i]][[j]],na.rm=TRUE),Fgr=sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m1,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m2,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m3,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m4,na.rm=TRUE),Ff=sum(ct3cb.fallow[[i]][[j]]$F.fw.str1,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str2,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str3,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str4,na.rm=TRUE),Fs= sum(Fs.ct3cb.fallow[[i]][[j]],na.rm=TRUE)))))
FoS.ct3cb.fallow.wt.1<-lapply(1:length(ct3.cb),function(i)lapply(1:length(soil.sl),function(j) data.frame(FoS=FoS.f(Fr=sum(Fr.ct3cb.fallow.wt.1[[i]][[j]],na.rm=TRUE),Fgr=sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m1,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m2,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m3,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m4,na.rm=TRUE),Ff=sum(ct3cb.fallow[[i]][[j]]$F.fw.str1,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str2,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str3,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str4,na.rm=TRUE),Fs= sum(Fs.ct3cb.fallow[[i]][[j]],na.rm=TRUE)))))
FoS.ct3cb.fallow.wt.2<-lapply(1:length(ct3.cb),function(i)lapply(1:length(soil.sl),function(j) data.frame(FoS=FoS.f(Fr=sum(Fr.ct3cb.fallow.wt.2[[i]][[j]],na.rm=TRUE),Fgr=sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m1,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m2,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m3,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m4,na.rm=TRUE),Ff=sum(ct3cb.fallow[[i]][[j]]$F.fw.str1,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str2,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str3,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str4,na.rm=TRUE),Fs= sum(Fs.ct3cb.fallow[[i]][[j]],na.rm=TRUE)))))
FoS.ct3cb.fallow.wt.3<-lapply(1:length(ct3.cb),function(i)lapply(1:length(soil.sl),function(j) data.frame(FoS=FoS.f(Fr=sum(Fr.ct3cb.fallow.wt.3[[i]][[j]],na.rm=TRUE),Fgr=sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m1,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m2,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m3,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$Fgr.pll.m4,na.rm=TRUE),Ff=sum(ct3cb.fallow[[i]][[j]]$F.fw.str1,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str2,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str3,na.rm=TRUE)+sum(ct3cb.fallow[[i]][[j]]$F.fw.str4,na.rm=TRUE),Fs= sum(Fs.ct3cb.fallow[[i]][[j]],na.rm=TRUE)))))



#vegetated
FoS.ct3cb.plant.wt.0<-lapply(1:length(ct3.cb),function(i)lapply(1:length(plant.sl),function(j) data.frame(FoS=FoS.f(Fr=sum(Fr.ct3cb.plant.wt.0[[i]][[j]],na.rm=TRUE),Fgr=sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m1,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m2,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m3,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m4,na.rm=TRUE),Ff=sum(ct3cb.plant[[i]][[j]]$F.fw.str1,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str2,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str3,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str4,na.rm=TRUE),Fs= sum(Fs.ct3cb.plant[[i]][[j]],na.rm=TRUE)))))
FoS.ct3cb.plant.wt.1<-lapply(1:length(ct3.cb),function(i)lapply(1:length(plant.sl),function(j) data.frame(FoS=FoS.f(Fr=sum(Fr.ct3cb.plant.wt.1[[i]][[j]],na.rm=TRUE),Fgr=sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m1,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m2,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m3,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m4,na.rm=TRUE),Ff=sum(ct3cb.plant[[i]][[j]]$F.fw.str1,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str2,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str3,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str4,na.rm=TRUE),Fs= sum(Fs.ct3cb.plant[[i]][[j]],na.rm=TRUE)))))
FoS.ct3cb.plant.wt.2<-lapply(1:length(ct3.cb),function(i)lapply(1:length(plant.sl),function(j) data.frame(FoS=FoS.f(Fr=sum(Fr.ct3cb.plant.wt.2[[i]][[j]],na.rm=TRUE),Fgr=sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m1,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m2,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m3,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m4,na.rm=TRUE),Ff=sum(ct3cb.plant[[i]][[j]]$F.fw.str1,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str2,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str3,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str4,na.rm=TRUE),Fs= sum(Fs.ct3cb.plant[[i]][[j]],na.rm=TRUE)))))
FoS.ct3cb.plant.wt.3<-lapply(1:length(ct3.cb),function(i)lapply(1:length(plant.sl),function(j) data.frame(FoS=FoS.f(Fr=sum(Fr.ct3cb.plant.wt.3[[i]][[j]],na.rm=TRUE),Fgr=sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m1,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m2,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m3,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$Fgr.pll.m4,na.rm=TRUE),Ff=sum(ct3cb.plant[[i]][[j]]$F.fw.str1,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str2,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str3,na.rm=TRUE)+sum(ct3cb.plant[[i]][[j]]$F.fw.str4,na.rm=TRUE),Fs= sum(Fs.ct3cb.plant[[i]][[j]],na.rm=TRUE)))))



#soutput
setwd("//Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs/FoS_global")
save(FoS.ct3cb.fallow.wt.0,file="FoS_cb_global_fallow_wt_0_amend.RData")
save(FoS.ct3cb.fallow.wt.1,file="FoS_cb_global_fallow_wt_1_amend.RData")
save(FoS.ct3cb.fallow.wt.2,file="FoS_cb_global_fallow_wt_2_amend.RData")
save(FoS.ct3cb.fallow.wt.3,file="FoS_cb_global_fallow_wt_3_amend.RData")
save(FoS.ct3cb.plant.wt.0,file="FoS_cb_global_plant_wt_0_amend.RData")
save(FoS.ct3cb.plant.wt.1,file="FoS_cb_global_plant_wt_1_amend.RData")
save(FoS.ct3cb.plant.wt.2,file="FoS_cb_global_plant_wt_2_amend.RData")
save(FoS.ct3cb.plant.wt.3,file="FoS_cb_global_plant_wt_3_amend.RData")



