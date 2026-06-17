#Module v - Plant-soil slice model
######################################
#Parent script: cribwall_run1.R and subsequent cribwall_design.R scripts
##########################################################################

#Contents
############## 

#0) load relevant functions and inputs
#1) establish soil column
#2) set soil attributes
#3) calculate soil hydro-mechanical properties
#4) calculate aboveground plant biomass
#5) Calculate root vertical distribution for fine roots
#6) Calculate soil-root mechanical reinforcement when roots break
#7) Calculate soil-root reinforcement due to pullout resistance (roots > 3 mm)
#8) Calculate plant surcharge
#9) Calculate enhanced saturated hydraulic conductivity (Ks) due to vegetation
#10) Calculate evaporation depth in the soil
#11) estimate daily evapotranspiration rate
#12) calculation of rainfall partitioning into throughfall and stemflow for each rainfall event
#13 soil moisture estimation
#14) Calculate infiltration and percolation rates
#15) Output's data frame compilation
#16) Calculation of matric suction profiles under wetting and drying events + water table conditions
#17) save output

###################################################################################    
#0) Load relvant functions from "LiveCW"
#########################################
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model")
source("live_crib_wall_model_functions.R") 

#-------------------------------------------------
#hydraulic permeability enhancement due to vegetation
#---------------------------------------------------


K.eff.f<-function(Ks,rdf){
	k.eff<-Ks+Ks*rdf
	return(k.eff)
}

##############################################
#0.2) Load forcing meteorological dataset
#############################################


setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/inputs")


met.ts<-read.csv("met_ts_rcp45_plant.csv")
head(met.ts)    

met.ts.SBEE<-subset(met.ts,met.ts$date >= "2022-08-03" & met.ts$date <= "2023-02-15")
head(met.ts.SBEE)


#growing seasons

GS.rcp45<-read.csv("growing_season_rcp45.csv")
head(GS.rcp45)

GS.SBEE<-subset(GS.rcp45,GS.rcp45$year >= 2000 & GS.rcp45$year <= 2025)

#Catterline met data
met.Catt<-read.csv("daily_Catterline_PEDROX.csv")

######################################################################################
#1) establish a 2-m soil column 
#################################
s.z<-seq(from=0,to=3.5,length=36) 

#2) set soil attributes - same as in cribwall-run; step 3
################################################################

y.soil<-c(-1,0,1,2,3,4)
Sa<-rep(53.2,length(y.soil))
Si<-rep(2.53,length(y.soil))
Cl<-rep(0.33,length(y.soil))
OM<-rep(1.72,length(y.soil))
Bk<-rep(1.43,length(y.soil))
Gs<-rep(2.65,length(y.soil)) #particle density
n.s<-1-(Bk/Gs)
FrA<-rep(18.4,length(y.soil))
c.s<-rep(17.94,length(y.soil))
a.van<-rep(0.07,length(y.soil))
n.van<-rep(5,length(y.soil))

soil.df<-data.frame(y.point=y.soil,Sa=Sa,Si=Si,Cl=Cl,OM=OM,Bk=Bk,n.s=n.s,Gs=Gs,FrA=FrA,c.s=c.s,a.van=a.van,n.van=n.van)


plant.df<-data.frame(z.point=s.z,y.point=floor(s.z))
plant.df<-merge(plant.df,soil.df,by="y.point")

#3) Calculate soil hydro-mechanical properties
##############################################
#3.1-matric suction of wetting front
plant.df$Sf.x<-Sf.f(Sa=plant.df$Sa/100,Cl=plant.df$Cl/100,n.s=plant.df$n.s) #in mm - 0.613 m; 61.3 cm - quite high but alrigth
#3.2 - field capacity
plant.df$FC.x<-FC.f(OM=plant.df$OM,Cl=plant.df$Cl,Si=plant.df$Si)
#3.3 - wilting point
plant.df$WP.x<-WP.f(OM=plant.df$OM,Cl=plant.df$Cl,Si=plant.df$Si)
#3.4 - saturated hydraulic conductivity (m/h)
plant.df$Ks.x<-Ks.f(OM=plant.df$OM/100,Cl=plant.df$Cl/100,Sa=plant.df$Sa/100,SMs=plant.df$n.s)/1000

#4) Calcualte aboveground plant biomass {for willow}
##############################################


SX.Ma.x<-SX.Ma(DBH=met.ts$DBHi) 
plot(SX.Ma.x/1000~as.Date(met.ts$date),type="l",main="willow aboveground biomass over time",xlab="year",ylab="Aboveground biomass (kg)",lwd=2)

#5) Calculate root vertical distribution for fine roots
#######################################################

	#5.1: mean rooting depth 
	
b.x<-b.w.f(AlphaC=mean(GS.SBEE$alpha.C),n.s=mean(plant.df$n.s[1:21]),FC=mean(plant.df$FC.x[1:21]),WP=mean(plant.df$WP.x[1:21])) #in mm	
	
	#5.2. cross-sectional area at the root-collar

Aro.x<-Aro.f(Ma=(SX.Ma.x),Beta.A=0.88,Alpha.A=4.55,b=b.x,R.dens=0.8*1e-03)

	#5.3. root cross-sectional area with soil depth (in mm2) 
	
Arz.x<-lapply(1:nrow(met.ts),function(i) data.frame(Arz=Arz.f(Aro=Aro.x[i],b=b.x,z=plant.df$z.point*1000)))

#check
plot(plant.df$z.point~unlist(Arz.x[[1]]),ylim=c(1,0),xlim=c(0,100),type="l")
dev.off()

	#5.4. calcualte root area ratio
	
RARz.x<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(RAR=sapply(1:nrow(plant.df),function(j) RARz.f(Arz=Arz.x[[i]][j,],As=1*1e6))))

	#5.5. update plant data.frame - [now it is a list of data frames] 
	
plant.df<-lapply(1:nrow(met.ts.SBEE), function(i) cbind(plant.df,Arz.x[[i]],RARz.x[[i]]))



#6) Calculate soil-root mechanical reinforcement when roots break
###################################################################

#6.1 - calculation of added cohesion - NOTE THAT THIS CHANGES TO 0.4*1.2*Tr*lR
#Tr=50MPa
#cR.x<-cR.f(Tr=50*1000,RARz=RARz.x) 

#6.2 - update data frame

Tr.x<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(Tr_kPa=rep(50*10000,nrow(plant.df[[i]]))))

plant.df<-lapply(1:nrow(met.ts.SBEE), function(i) cbind(plant.df[[i]],Tr.x[[i]]))

#7) Calculate soil-root reinforcement due to pullout resistance (roots > 3 mm)
#################################################################################
#Assumption - one 50 mm tap root developed from cutting insertion into the soil up to 0.5 m
#Uw=13.96 kN/m3 - calculated in the cribwall run

bR.x<-sapply(1:nrow(met.ts.SBEE),function(i) bR.f(z=met.ts.SBEE$RDi[i]/100,Uw=13.96,FrA=soil.df$FrA[2]*pi/180,f=0.75))

bR.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(bR_kPa=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j] <= (met.ts.SBEE$RDi[i])/100,bR.x[i],0))))


plant.df<-lapply(1:nrow(met.ts.SBEE), function(i) cbind(plant.df[[i]],bR.xx[[i]]))

#8) Calculate plant surcharge
###############################

WV.x<-sapply(1:nrow(met.ts.SBEE),function(i) WV.f(b=b.x/10,Aro=Aro.x[i]*1e-02,R.dens=0.8,Ac.a=met.ts$Aci[i],g=9.8,Ma=SX.Ma.x[i]))
WV.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(WV_kPa=rep(WV.x[i],nrow(plant.df[[i]]))))

plant.df<-lapply(1:nrow(met.ts.SBEE), function(i) cbind(plant.df[[i]],WV.xx[[i]]))


#----------------------------------------------
##9) Calculate enhanced Ks due to vegetation
#################################################
K.eff<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(K_eff_m_h=sapply(1:nrow(plant.df[[i]]),function(j) K.eff.f(Ks=plant.df[[i]]$Ks.x[j],rdf=plant.df[[i]]$Arz[j]/Aro.x[1]))))

#note that K.eff will have to replace Ks throughout
plant.df<-lapply(1:nrow(met.ts.SBEE), function(i) cbind(plant.df[[i]],K.eff[[i]]))


#10) Calculate evaporation depth in the soil
#########################################
dx.x<-dx.f(Cl=soil.df$Cl[2]/100,Sa=soil.df$Sa[2]/100) #0.106 m


#11) estimate daily evapotranspiration rate
############################################


met.Catt<-read.csv("daily_Catterline_PEDROX.csv")
head(met.Catt)
met.Catt<-subset(met.Catt,met.Catt$date >= "2022-08-03" & met.Catt$date <= "2023-02-15")
met.Catt$ps<-((met.Catt$baro_max+met.Catt$baro_min)/2)/1000
met.Catt$tas<-(met.Catt$max_temp+met.Catt$min_temp)/2

PSH.out<-PSH.f(AP=met.Catt$ps) #pshychrometric constant (kPa)
A.out<-A.f(Tk=met.Catt$tas+274.5) #slope of staturation vapour pressure
dr.out<-dr.f(J=met.ts.SBEE$J,π=3.1416) #inverse relative distance Earth-Sun
DEC.out<-DEC.f(J=met.ts.SBEE$J,π=3.1416) #declination
ws.out<-ws.f(DEC=DEC.out,LAT=56.89*0.0174532925) #sunset hour angle
N.out<-N.f(π=3.1416,ws=ws.out) #maximum duration of sunshine
#Ra.out<-Ra.f(π=3.1416,Gsc=0.0820,dr=dr.out,ws=ws.out,LAT=56.89*0.0174532925,DEC=DEC.out) #extraterrestial radiation
#Rs.out<-Rs.f(as=0.25,bs=0.50,n=met.ts$sunshine,N=N.out,Ra=Ra.out) #incoming solar radiation
a.out<-a.f(S.al=0.26,Cf=Cf.f(Ma=max(SX.Ma.x))) #albedo considering salix biomass in kg/ha SX.Ma.x
Rnl.out<-Rnl.f(a=a.out,Rs=met.ts.SBEE$Rs) #net solar radiation in MJ/m2 day
Rnl_ly.out<-Rnl.out*23.9 #in ly
Eu.out<-Eu.f(Rnl=Rnl_ly.out,A=A.out,PSH=PSH.out)
Eu_mm.out<-Eu_m.f(Eu=Eu.out,T=met.Catt$tas)*1000 #potential Etp in mm/m2 d
Esp.out<-Esp.f(Eu=Eu_mm.out,LAI=met.ts.SBEE$LAIi.rcp45,As=4) #evaporation under the canopy
Etp.out<-Etp.f(Esp=Esp.out,Eu=Eu_mm.out,As=4) #plant transpiration 


met.ts.SBEE$Eu_mm_m2_d<-Eu_mm.out #mm/d.m2

Eu.df<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(Eu_mm_m2_d=rep(Eu_mm.out[[i]],nrow(plant.df[[i]]))))


#12) calculation of rainfall partitioning into throughfall and stemflow for each rainfall event
##################################################################################

	#12.1 - calculate canopy cover fraction for LAI=3.26 (max. salix viminalis)
	#extinction coefficient=0.75
	
	cc.x<-cc.f(kc=0.75,LAI=met.ts.SBEE$LAIi.rcp45)
	
	#12.2 - calculate effective rainfall	 on the basis of rainfall interception
	
ER_mm<-sapply(1:nrow(met.ts.SBEE), function(i) ER.f(Pg=met.Catt$rainfall[i]*met.ts.SBEE$Aci[i],S=0.72,cc=cc.x[i],Ac.a=met.ts.SBEE$Aci[i])/met.ts.SBEE$Aci[i])


#S:storage capacity (mm/m2) for s.viminalis was set at 0.72 

ER_mm<-sapply(1:nrow(met.ts.SBEE),function(i) ifelse(ER_mm[i] < 0,0,ER_mm[i]))

met.ts.SBEE$ER_mm<-ER_mm

ER.df<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(ER_mm=rep(ER_mm[[i]],nrow(plant.df[[i]]))))

	#12.3 - calculate stemflow volume (in L)
stf_L<-sapply(1:nrow(met.ts.SBEE), function(i) ifelse(met.Catt$rainfall[i] > 0,Ps.f(Pg=met.Catt$rainfall[i],st.i=0.03,st.s=0.23,Ac.b=met.ts.SBEE$Aci[i]),0))


met.ts.SBEE$stf_L<-stf_L


stf.L.df<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(stf_L=rep(stf_L[[i]],nrow(plant.df[[i]]))))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#13) ESTIMATION OF SOIL MOISTURE 
###########################################


SMx<-function(SMi,Vs,R,stf,Etp){
	Vw<-(SMi*Vs*1000+(R+stf-Etp))/1000 #m3
	#Vw.x<-ifelse(Vw <= Vs,Vw,Vs)
	#SMf<-(Vw.x/Vs)
	#return(SMf)
}

SM.null<-c(mean(plant.df[[1]]$FC.x),rep(NA,length=nrow(met.ts.SBEE)-1))

for(i in 2:length(SM.null)){
	SM.null[i]<-SMx(SMi=SM.null[i-1],Vs=mean(plant.df[[1]]$n.s)*2,R=ER_mm[i]*2,Etp=Eu_mm.out[i]*2,stf=stf_L[i]/2) #stemflow is divided into two; 50 % lost to runoff
}

SM.null<-sapply(1:nrow(met.ts.SBEE),function(i) ifelse(SM.null[i] < 0,0,SM.null[i]))

#graphical check
plot(SM.null,type='l',ylim=c(0,1)) 
dev.off()

#14) Calculate infiltration and percolation rates, depths, for each event....
################################################################################
		#############################################################################
		#14.1. for stemflow bypass and subsequent percolation below the soil-root zone
		#############################################################################

		#14.1.1 - calculation of bypass flow due to stemflow
#in m/h

q.st.xx<-sapply(1:nrow(met.ts.SBEE),function(i) (((met.ts.SBEE$stf_L[i]/2)/(pi*((met.ts.SBEE$DBHi[i]/100)/2)^2))/1000)/24) #in m/h
 	#note that this is regarded as bypass flow	
		
		
		#14.1.2 - calculation of water volume available for percolation below soil-root zone (in liters)
	#if water only enters at Ks over 24h, this will be the available for percolation
		

v.st.perc.xx<-sapply(1:nrow(met.ts.SBEE),function(i) ifelse(met.ts.SBEE$stf_L[i]>0,q.st.xx[i]*24*1000,0)) #mm or L/m2

			# 14.1.2.2 - contribution to soil moisture of this water

plus.SM.st<-lapply(1:nrow(met.ts.SBEE),function(i) (v.st.perc.xx[i]/1000)/(met.ts.SBEE$RDi[i]/100))

		#14.1.3 - calculate stemflow-derived q_perc
			#14.1.3.1 - percolation time (h)

TTperc.st.xx<-sapply(1:nrow(met.ts.SBEE),function(i) TTperc.f(SWex=v.st.perc.xx[i],Ks=mean(K.eff[[i]]$K_eff_m_h)))



			#14.1.3.2 - percolation rate (m/h)
q.st.perc.xx<-sapply(1:nrow(met.ts.SBEE),function(i) q_perc.f(SWex=v.st.perc.xx[i],t.step=1,TTperc=TTperc.st.xx[i]))

		#14.1.4 - initial water volume in unsat.zone [below the root zone soil moisture is at field capacity]

VwUi.st.xx<-sapply(1:nrow(met.ts.SBEE),function(i) VwUi.f(SMi=mean(plant.df[[1]]$FC.x),V.unsat=2-(met.ts$RDi[i]/100)))

		#14.1.5 - final volume of water after complete stemflow-derived percolation

VwUf.st.xx<-sapply(1:nrow(met.ts.SBEE),function(i) VwUf.f(VwUi=VwUi.st.xx[i],SWex=v.st.perc.xx[i],As=4))


		#14.1.6 - final moisture content below root zone (without water table)

SMUf.st.xx<-sapply(1:nrow(met.ts.SBEE),function(i) SMf.f(VwUf=VwUf.st.xx[i],V.unsat=2-(met.ts$RDi[i]/100)))
SMUf.st.xx<-sapply(1:nrow(met.ts.SBEE),function(i) ifelse(SMUf.st.xx[i]>1.5,1,SMUf.st.xx[i]))

#graphical check
plot(SMUf.st.xx*plant.df[[1]]$n.s,type="l",ylim=c(0,1)) 
dev.off()
		#14.1.7 - hydraulic conductivity function (m/h)


K_Oi.st.xx<-sapply(1:nrow(met.ts.SBEE),function(i) K_Oi.f(Ks=K.eff[[i]],SMi=SMUf.st.xx[i],SMs=mean(plant.df[[1]]$n.s),n.van=mean(plant.df[[1]]$n.van)))

		
		#14.1.8 - travel distance of stemflow-derived percolation front (m)

z.st.perc.xx<-sapply(1:nrow(met.ts.SBEE),function(i) z.perc.f(K_Of=mean(K_Oi.st.xx[[i]]),TTperc=TTperc.st.xx[[i]]))

		#14.1.9 - depth of stemflow-derived percolation front (m)

Zpf.st.xx<-sapply(1:nrow(met.ts.SBEE),function(i) Zpf.f(Zwf=met.ts$RDi[i]/100,z.perc=z.st.perc.xx[i]))

	
	
	#########################################################
	#14.2 - for the effective rainfall following throughfall
	######################################################### #
Fp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) ifelse(met.ts.SBEE$ER_mm[i]/1000 < mean(K.eff[[1]]$K_eff_m_h[1:21])*24,0,Fp.f(Sf=mean(plant.df[[i]]$Sf.x[1:10])/1000,Ks=mean(K.eff[[1]]$K_eff_m_h[1:21])*24,SMs=mean(plant.df[[i]]$n.s[1:10]),SMi=SM.null[i],P=met.ts.SBEE$ER_mm[i]/1000))) #in m


	#14.2.2 - depth of wetting front at ponding
 
Zwfp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Zwf.f(Fp=Fp.xx[[i]],SMs=mean(plant.df[[i]]$n.s[1:10]),SMi=SM.null[i])) 

	#14.2.3 -runoff
RNF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) RNF.f(ER=met.ts.SBEE$ER_mm[i]/1000,Fp=Fp.xx[[i]],Ks=mean(K.eff[[i]]$K_eff_m_h[1:3]),tr=2)) 

RNF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) ifelse(RNF.xx[[i]]<0,0,RNF.xx[[i]]))

	#14.2.4 - actual infiltration
#cribwall filling

cb.fill<-data.frame(x1=8,y1=10,x2=16,y2=14)
sl.fill<-(cb.fill$y2-cb.fill$y1)/(cb.fill$x2-cb.fill$x1)

AI.xx<-lapply(1:nrow(met.ts.SBEE),function(i) AI.f(ER=met.ts.SBEE$ER_mm[i]/1000,RNF=RNF.xx[[i]],slope=atan(sl.fill)))

	#14.2.5 - wetting front depth + ponding


Zwf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Zwf.f(Fp=AI.xx[[i]],SMs=mean(plant.df[[i]]$n.s[1:10]),SMi=(SM.null[i]-0.01))+Zwfp.xx[[i]])
#notice that the facotr 0.01 is introduced to avoid NaNs and extremely high values as a result of the function implementation

	#14.2.6 - volume of saturated soil (m3)

Vsat.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Vsat.f(Zwf=Zwf.xx[[i]],As=4))



	#14.2.7 - volume of water in the saturated soil (m3)

Vw.sat.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Vw.f(Vsat=Vsat.xx[[i]],SMs=mean(plant.df[[1]]$n.s[1:10])))

	#14.2.8 - volume of water at field capacity in the saturared soil column

wFC.xx<-lapply(1:nrow(met.ts.SBEE),function(i) wFC.f(FC=mean(plant.df[[1]]$FC.x[1:10]),Vsat=Vsat.xx[[i]]))

	#14.2.9 - excess water volume - i.e. percolating volume (l/m2)

SWex.xx<-lapply(1:nrow(met.ts.SBEE),function(i) SWex.f(Vw=Vw.sat.xx[[i]],wFC=wFC.xx[[i]]))


	#14.2.10 - travel time for percolation

TTperc.xx<-lapply(1:nrow(met.ts.SBEE),function(i) TTperc.f(SWex=SWex.xx[[i]],Ks=mean(K.eff[[i]]$K_eff_m_h)))


	#14.2.11 - percolation rate (m/h) - below the wetting front! up to the wetting front we could assume that water enters at Ks

q_perc.xx<-lapply(1:nrow(met.ts.SBEE),function(i) q_perc.f(SWex=SWex.xx[[i]],t.step=1,TTperc=TTperc.xx[[i]])) 

	#14.2.12 - unsaturated soil profile volume (m3) -i.e. proportion of soil not affected by wetting front

v.unsat.xx<-lapply(1:nrow(met.ts.SBEE),function(i) V.unsat.f(sDP=3.5,Zwf=Zwf.xx[[i]],As=4))


############################################################################################################3

	#14.2.13 - initial water volume in unsat.zone [below the root zone soil moisture is at field capacity]
#after stemflow percolation

		#14.2.13.1 - establishment of initial moisture below the wetting front depending on depth - 
				
	SMi.unsat.xx<-lapply(1:nrow(met.ts.SBEE), function(i) ifelse(3.5-Zwf.xx[[i]]>met.ts.SBEE$RDi[i] & Zwf.xx[[i]]>0,SMUf.st.xx[[i]],SM.null[[i]]+plus.SM.st[[i]]))
	
	
	#graphical check
plot(unlist(SMi.unsat.xx),type="l",ylim=c(0,1)) 
dev.off()


	SMi.unsat.xx<-lapply(1:nrow(met.ts.SBEE),function(i) ifelse(SMi.unsat.xx[[i]] > 1,1,SMi.unsat.xx[[i]]))
	
	
		#14.2.13.2 - calculate initial volume of water in the unsaturated zone
VwUi.xx<-lapply(1:nrow(met.ts.SBEE),function(i) VwUi.f(SMi=SMi.unsat.xx[[i]],V.unsat=v.unsat.xx[[i]]))

	#14.2.14 - final volume of water after complete percolation (in L)

VwUf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) VwUf.f(VwUi=VwUi.xx[[i]],SWex=SWex.xx[[i]],As=4))


	#14.2.15 - final moisture content unsat zone 

SMUf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) SMf.f(VwUf=VwUf.xx[[i]],V.unsat=v.unsat.xx[[i]]))

		#14.2.15.2 - correction of excess water over volume of soil (assuming area of 4 m2) soil depth=3.5
SMUf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) ifelse(SMUf.xx[[i]] > (3.5*4),(3.5*4)*plant.df[[i]]$n.s,SMUf.xx[[i]]*plant.df[[i]]$n.s))



#14.1.15 - hydraulic conductivity function (m/h)

K_Oi.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(K_Oi_m_h=sapply(1:nrow(plant.df[[i]]),function(j) K_Oi.f(Ks=K.eff[[i]]$K_eff_m_h[j],SMi=SMi.unsat.xx[[i]],SMs=plant.df[[i]]$n.s[j],n.van=plant.df[[i]]$n.van[j]))))

#14.1.16 - travel distance of percolation front (m)

z.perc.xx<-lapply(1:nrow(met.ts.SBEE),function(i) z.perc.f(K_Of=mean(K_Oi.xx[[i]]$K_Oi_m_h),TTperc=TTperc.xx[[i]]))

#14.1.17 - depth of percolation front (m)

Zpf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Zpf.f(Zwf=Zwf.xx[[i]],z.perc=z.perc.xx[[i]]))

#######################################################################################
#15) Output's DATA-FRAME compilation with variables of interest + informative
#############################################################

#infiltration rate


#m/h
q.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(qER_m_h=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j] < Zwf.xx[[i]],(((met.ts.SBEE$ER_mm[i]/1000)-RNF.xx[[i]])/24),ifelse(plant.df[[i]]$z.point[j] > Zwf.xx[[i]] & plant.df[[i]]$z.point[j] < Zpf.xx[[i]],q_perc.xx[[i]],0)))))

#soil moisture
SM.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(SM=sapply(1:nrow(plant.df[[i]]),function(j)ifelse(plant.df[[i]]$z.point[j] < Zwf.xx[[i]],plant.df[[i]]$n.s[j],SMi.unsat.xx[[i]]))))

#depth of wetting front

ZWF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(ZWF_m=rep(Zwf.xx[[i]],nrow(plant.df[[i]]))))

#depth of percolation front (informative)

ZPF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(ZPF_m=rep(Zpf.xx[[i]],nrow(plant.df[[i]]))))

#saturated/unsaturated (informative)

SAT.xx<-lapply(1:nrow(met.ts.SBEE),function(i)data.frame(SAT=sapply(1:nrow(plant.df[[i]]), function(j) ifelse(plant.df[[i]]$z.point[j]<Zwf.xx[[i]],"S","U"))))

#percolation time (informative)
TP.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(TPerc_h=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j]<Zwf.xx[[i]],0,TTperc.xx[[i]]))))


#effective rainfall derived infiltration rate based on degree of saturation

q.xx.bis<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(qER_m_h=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(q.xx[[i]]$qER_m_h[j] > 0 & SAT.xx[[i]]$SAT[j]=="S",plant.df[[i]]$K_eff_m_h[j],q.xx[[i]]$qER_m_h[j]))))


#stemflow-derived bypass infiltration + percolation rates
###########
q.st<-lapply(1:nrow(met.ts.SBEE),function(i)data.frame(qST_m_h=sapply(1:nrow(plant.df[[i]]),function(j)ifelse(plant.df[[i]]$z.point[j] < (met.ts.SBEE$RDi[i]/100),q.st.xx[i],ifelse(plant.df[[i]]$z.point[j] > (met.ts.SBEE$RDi[i]/100) & plant.df[[i]]$z.point[j] < Zpf.st.xx[i],q.st.perc.xx[i],0)))))

#evapotranspiration

etp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(etp_mm_d_m2=sapply(1:nrow(plant.df[[i]]),function(j)ifelse(plant.df[[i]]$z.point[j]< (met.ts.SBEE$RDi[i]/100),met.ts.SBEE$Eu_mm_m2_d[i],0))))


#hydraulic conductivity function


K_Oi<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(K_Oi_m_h=sapply(1:nrow(plant.df[[i]]),function(j)K_Oi.f(Ks=plant.df[[i]]$K_eff_m_h[j],SMi=SM.xx[[i]][j,1],SMs=plant.df[[i]]$n.s[j],n.van=plant.df[[i]]$n.van[j]))))



#DATA.FRAME


plant.df<-lapply(1:nrow(met.ts.SBEE),function(i)cbind(plant.df[[i]],q.xx.bis[[i]],q.st[[i]],SM.xx[[i]],SAT.xx[[i]],Eu.df[[i]],ER.df[[i]],stf.L.df[[i]],ZWF.xx[[i]],ZPF.xx[[i]],TP.xx[[i]],etp.xx[[i]],K_Oi[[i]]))



###################################################################################################################
##################################################################################################################
#16 - Calculation of MATRIC SUCTION under wetting and drying cycles
#####################################################################
#assumption: no change of hydro-mechanical parameters in plant-soil composites; nor with soil hysteresis

	#16.1 MS due to ER - effective rainfall
	


MS.ER.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_ER_kPa=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j]<= plant.df[[i]]$ZWF_m[j],MS.w.f(a=plant.df[[i]]$a.van[j],q=(-1)*plant.df[[i]]$qER_m_h[j],Ks=plant.df[[i]]$K_eff_m_h[j],Ww=9.8,z=3.5-plant.df[[i]]$z.point[j]),MS.w.f(a=plant.df[[i]]$a.van[j],q=0,Ks=plant.df[[i]]$K_eff_m_h[j],Ww=9.8,z=3.5-plant.df[[i]]$z.point[j])))))

	#16.2 MS due to Stemflow
#q=0 is not double-counted

	
MS.ST.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_ST_kPa=sapply(1:nrow(plant.df[[i]]),function(j) MS.w.f(a=plant.df[[i]]$a.van[j],q=(-1)*plant.df[[i]]$qST_m_h[j],Ks=plant.df[[i]]$K_eff_m_h[j],Ww=9.8,z=3.5-plant.df[[i]]$z.point[j]))))


	#16.3 MS due to evapotranspiration
	

MS.etp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_ETP_kPa=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j]<= (met.ts.SBEE$RDi[i]/100),MS.d.f(a=plant.df[[i]]$a.van[j],Etp=(plant.df[[i]]$etp_mm_d_m2[j])/(1000*24),K_Oi=plant.df[[i]]$K_Oi_m_h[j],Ww=9.8,z=3.5-plant.df[[i]]$z.point[j],Ac=met.ts.SBEE$Aci[i],LAI=met.ts.SBEE$LAIi.rcp45[i]),MS.d.f(a=plant.df[[i]]$a.van[j],Etp=0,K_Oi=plant.df[[i]]$K_Oi_m_h[j],Ww=9.8,z=3.5-plant.df[[i]]$z.point[j],Ac=met.ts.SBEE$Aci[i],LAI=met.ts.SBEE$LAIi.rcp45[i])))))


#UPDATE DATA FRAME

plant.df<-lapply(1:nrow(met.ts.SBEE),function(i) cbind(plant.df[[i]],MS.ER.xx[[i]],MS.ST.xx[[i]],MS.etp.xx[[i]]))

##16.4) WATER TABLE conditions

#in the MS equation, the z term considers the depth of the phreatic zone by establishing at the bottom/lower end of the soil profile. In this case this is assumed to have a depth of 3 m (a bit unrealistic already for the kind of site under study); if the water table were near the surface, 3 would change to zero and z would be negative

#16.4.1 - set water table depths
wt.d<-c(0.1,1,2) #m from the ground level


#16.4.2. calculations

#wt1=0.1m

MS.WT.1<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_WT1_kPa=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j]<= wt.d[1],MS.w.f(a=plant.df[[i]]$a.van[j],q=(-1)*plant.df[[i]]$qER_m_h[j],Ks=plant.df[[i]]$K_eff_m_h[j],Ww=9.8,z=wt.d[1]-plant.df[[i]]$z.point[j]),MS.w.f(a=plant.df[[i]]$a.van[j],q=0,Ks=plant.df[[i]]$K_eff_m_h[j],Ww=9.8,z=wt.d[1]-plant.df[[i]]$z.point[j])))))


#wt2=1 m
MS.WT.2<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_WT2_kPa=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j]<= wt.d[2],MS.w.f(a=plant.df[[i]]$a.van[j],q=(-1)*plant.df[[i]]$qER_m_h[j],Ks=plant.df[[i]]$K_eff_m_h[j],Ww=9.8,z=wt.d[2]-plant.df[[i]]$z.point[j]),MS.w.f(a=plant.df[[i]]$a.van[j],q=0,Ks=plant.df[[i]]$K_eff_m_h[j],Ww=9.8,z=wt.d[2]-plant.df[[i]]$z.point[j])))))


#wt3=2 m
MS.WT.3<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_WT3_kPa=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j]<= wt.d[3],MS.w.f(a=plant.df[[i]]$a.van[j],q=(-1)*plant.df[[i]]$qER_m_h[j],Ks=plant.df[[i]]$K_eff_m_h[j],Ww=9.8,z=wt.d[3]-plant.df[[i]]$z.point[j]),MS.w.f(a=plant.df[[i]]$a.van[j],q=0,Ks=plant.df[[i]]$K_eff_m_h[j],Ww=9.8,z=wt.d[3]-plant.df[[i]]$z.point[j])))))



plant.df<-lapply(1:nrow(met.ts.SBEE),function(i) cbind(plant.df[[i]],MS.WT.1[[i]],MS.WT.2[[i]],MS.WT.3[[i]]))
plant.df[[4]]


#-----------------------------------------------------------------------------------------------
##16.5 Unification of Matric suction profiles
#--------------------------------------------

MS.unified.1<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_unified_1_kPa=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j] >= wt.d[1], plant.df[[i]]$MS_WT1_kPa[j],ifelse(plant.df[[i]]$qST_m_h[j]>0,max(plant.df[[i]]$MS_ETP_kPa[j],plant.df[[i]]$MS_ER_kPa[j])+plant.df[[i]]$MS_ST_kPa[j],max(plant.df[[i]]$MS_ETP_kPa[j],plant.df[[i]]$MS_ER_kPa[j]))))))


MS.unified.2<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_unified_2_kPa=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j] >= wt.d[2], plant.df[[i]]$MS_WT2_kPa[j],ifelse(plant.df[[i]]$qST_m_h[j]>0,max(plant.df[[i]]$MS_ETP_kPa[j],plant.df[[i]]$MS_ER_kPa[j])+plant.df[[i]]$MS_ST_kPa[j],max(plant.df[[i]]$MS_ETP_kPa[j],plant.df[[i]]$MS_ER_kPa[j]))))))



MS.unified.3<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_unified_3_kPa=sapply(1:nrow(plant.df[[i]]),function(j) ifelse(plant.df[[i]]$z.point[j] >= wt.d[3], plant.df[[i]]$MS_WT3_kPa[j],ifelse(plant.df[[i]]$qST_m_h[j]>0,max(plant.df[[i]]$MS_ETP_kPa[j],plant.df[[i]]$MS_ER_kPa[j])+plant.df[[i]]$MS_ST_kPa[j],max(plant.df[[i]]$MS_ETP_kPa[j],plant.df[[i]]$MS_ER_kPa[j]))))))


plant.df<-lapply(1:nrow(met.ts.SBEE),function(i) cbind(plant.df[[i]],MS.unified.1[[i]],MS.unified.2[[i]],MS.unified.3[[i]]))



##########################################################################################################
#17) save output
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#17.1 - adding dates, rainfall and temperature records
#####################################################

date.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(DATE=rep(met.ts.SBEE$date[i],nrow(plant.df[[i]]))))
rain.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(rain_mm=rep(met.Catt$rainfall[i],nrow(plant.df[[i]]))))
temp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(T_air_C=rep(met.Catt$tas[i],nrow(plant.df[[i]]))))
etp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(etp_mm_m2=rep(met.ts.SBEE$Eu_mm_m2_d[i],nrow(plant.df[[i]]))))

plant.df<-lapply(1:nrow(met.ts.SBEE),function(i) cbind(plant.df[[i]],rain.xx[[i]],temp.xx[[i]],date.xx[[i]]))



#setwd("/Users/ollauri/Desktop/work/OPERANDUM/mini_projects/NBS_modelling/data/CC_runs")
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs")
save(plant.df,file="plant_slice_ts_SBEE25_run.RData") 


