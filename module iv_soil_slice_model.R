#Module iv - soil slice model
############################################################


#Contents
################################
#0) load required functions and inputs
#1) establish soil column
#2) set soil attributes
#3) assign soil attributes to soil column
#4) calculate soil hydro-mechanical porperties
#5) Calculate evaporation depth in the soil
#6) estimate daily soil evaporation rate
#7) soil moisture estimation
#8) Calculate infiltration and percolation rates
#9) outputs' data frame compilation
#10) Calculation of matric suction profiles for wetting and drying events + water table conditions
#11) save output


###################################################################################    
#0) Load relvant functions from "LiveCW"
#########################################





setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/inputs")

met.ts<-read.csv("met_ts_rcp45_plant.csv")
head(met.ts)

met.ts.SBEE<-subset(met.ts,met.ts$date >= "2022-08-03" & met.ts$date <= "2023-02-15")
head(met.ts.SBEE)

#Catterline weather station
met.Catt<-read.csv("daily_Catterline_PEDROX.csv")


setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model")
source("live_crib_wall_model_functions.R") 


######################################################################################
#1) establish a 3-m soil column 
#################################
s.z<-seq(from=0,to=3.5,length=36)


#2) Set soil attributes 
##############################

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


#3) Assign soil attributes to soil column
###########################################
SDF<-data.frame(z.point=s.z,y.point=floor(s.z))
SDF<-merge(SDF,soil.df,by="y.point")



#4) Calculate soil hydro-mechanical properties
##############################################
#4.1-matric suction of wetting front
SDF$Sf.x<-Sf.f(Sa=SDF$Sa/100,Cl=SDF$Cl/100,n.s=SDF$n.s) #in mm - 1.34 m; 134.2 cm - quite high but alrigth it is 13.14 kPa (P=rho.g.h)

#4.2 - field capacity
SDF$FC.x<-FC.f(OM=SDF$OM,Cl=SDF$Cl,Si=SDF$Si)
#4.3 - wilting point
SDF$WP.x<-WP.f(OM=SDF$OM,Cl=SDF$Cl,Si=SDF$Si)
#4.4 - saturated hydraulic conductivity (m/h)
SDF$Ks.x<-Ks.f(OM=SDF$OM/100,Cl=SDF$Cl/100,Sa=SDF$Sa/100,SMs=SDF$n.s[2])/1000

###############################################
#5) Calculate evaporation depth in the soil
#########################################
dx.x<-dx.f(Cl=mean(SDF$Cl)/100,Sa=mean(SDF$Sa)/100) #0.106 m


#6) estimate daily soil evaporation rate
############################################

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
a.out<-a.f(S.al=0.26,Cf=Cf.f(Ma=0/1000)) #albedo considering non-cultivated field and a herb biomass
Rnl.out<-Rnl.f(a=a.out,Rs=met.ts.SBEE$Rs) #net solar radiation in MJ/m2 day
Rnl_ly.out<-Rnl.out*23.9 #in ly
Eu.out<-Eu.f(Rnl=Rnl_ly.out,A=A.out,PSH=PSH.out)
Eu_mm.out<-Eu_m.f(Eu=Eu.out,T=met.Catt$tas)*1000 #potential Etp in mm/m2 d
Esp.out<-Esp.f(Eu=Eu_mm.out,LAI=0,As=1) #evaporation under the canopy
#Etp.out<-Etp.f(Esp=Esp.out,Eu=Eu_mm.out,As=1) #plant transpiration [not convincing]

#Esp.out[is.na(Esp.out)]<-mean(Esp.out,na.rm=TRUE)

met.ts.SBEE$Esp_mm_m2_d<-Esp.out


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#7) Soil moisture estimation 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#assumption - initial moisture is at field capacity
#water mass balance is R-Etp; only the first 0.5 m of soil is considered (root-soil zone)

#30/5/25 ... though for trying to simulate the failure under this example, the soil profile will be considered saturated

SMx<-function(SMi,Vs,R,Etp){
	Vw<-(SMi*Vs*1000+(R-Etp))/1000 #m3
	Vw.x<-ifelse(Vw <= Vs,Vw,Vs)
	SMf<-(Vw.x/Vs)
	return(SMf)
}

SM.null<-c(0.18,rep(NA,length=nrow(met.ts.SBEE)-1))

for(i in 2:length(SM.null)){
	SM.null[i]<-SMx(SMi=SM.null[i-1],Vs=n.s[1]*2,R=met.Catt$rainfall[i]*2,Etp=Esp.out[i]*2)
}

SM.null<-sapply(1:nrow(met.ts.SBEE),function(i) ifelse(SM.null[i] < 0,0,SM.null[i]))

plot(SM.null,type='l') #the soil is mostly saturated

SMi.x<-SM.null

#8) Calculate infiltration and percolation rates, depths, for each rainfall event....
################################################################################


#cribwall filling

cb.fill<-data.frame(x1=8,y1=10,x2=16,y2=14)
sl.fill<-(cb.fill$y2-cb.fill$y1)/(cb.fill$x2-cb.fill$x1) #slope of filling 1:2

	#8.1 - water column at ponding
Fp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) ifelse(met.Catt$rainfall[i]/1000 < SDF$Ks.x[1]*24,0,Fp.f(Sf=SDF$Sf.x[1]/1000,Ks=SDF$Ks.x[1]*24,SMs=SDF$n.s[2],SMi=SMi.x[i],P=met.Catt$rainfall[i]/1000))) #in m
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


	#8.2 - depth of wetting front at ponding
Zwfp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Zwf.f(Fp=Fp.xx[[i]],SMs=SDF$n.s[2],SMi=(SMi.x[i]-0.01))) 

	#8.3 -runoff
RNF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) RNF.f(ER=met.Catt$rainfall[i]/1000,Fp=Fp.xx[[i]],Ks=SDF$Ks.x[1],tr=2)) 

RNF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) ifelse(RNF.xx[[i]]<0,0,RNF.xx[[i]])) #in m

	#8.4 - actual infiltration
AI.xx<-lapply(1:nrow(met.ts.SBEE),function(i) AI.f(ER=met.Catt$rainfall[i]/1000,RNF=RNF.xx[[i]],slope=atan(sl.fill)))

	#8.5 - wetting front depth + ponding
Zwf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Zwf.f(Fp=AI.xx[[i]],SMs=SDF$n.s[2],SMi=(SMi.x[i]-0.01))+Zwfp.xx[[i]])
#notice that the facotr 0.01 is introduced to avoid NaNs and extremely high values as a result of the function implementation

	#8.6 - volume of saturated soil (m3)

Vsat.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Vsat.f(Zwf=Zwf.xx[[i]],As=1))


	#8.7 - volume of water in the saturated soil (m3)

Vw.sat.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Vw.f(Vsat=Vsat.xx[[i]],SMs=SDF$n.s[2]))

	#8.8 - volume of water at field capacity in the saturared soil column

wFC.xx<-lapply(1:nrow(met.ts.SBEE),function(i) wFC.f(FC=mean(SDF$FC.x),Vsat=Vsat.xx[[i]]))

	#8.9 - excess water volume - i.e. percolating volume (l/m2)

SWex.xx<-lapply(1:nrow(met.ts.SBEE),function(i) SWex.f(Vw=Vw.sat.xx[[i]],wFC=wFC.xx[[i]]))


	#8.10 - travel time for percolation

TTperc.xx<-lapply(1:nrow(met.ts.SBEE),function(i) TTperc.f(SWex=SWex.xx[[i]],Ks=mean(SDF$Ks.x)))


	#8.11 - percolation rate (m/h) - below the wetting front! up to the wetting front we could assume that water enters at Ks

q_perc.xx<-lapply(1:nrow(met.ts.SBEE),function(i) q_perc.f(SWex=SWex.xx[[i]],t.step=1,TTperc=TTperc.xx[[i]])) 

	#8.12 - unsaturated soil profile volume (m3) -i.e. proportion of soil not affected by wetting front

v.unsat.xx<-lapply(1:nrow(met.ts.SBEE),function(i) V.unsat.f(sDP=3.5,Zwf=Zwf.xx[[i]],As=4))


############################################################################################################3

	#8.13 - initial water volume in unsat.zone [below the root zone soil moisture is at field capacity]
#after stemflow percolation

		#8.13.1 - establishment of initial moisture below the wetting front depending on depth 		
	SMi.unsat.xx<-lapply(1:nrow(met.ts.SBEE), function(i) ifelse(Zwf.xx[[i]]>1,SDF$FC.x[[20]],SDF$FC.x[1]))
	
		#8.13.2 - calculate initial volume of water in the unsaturated zone
VwUi.xx<-lapply(1:nrow(met.ts.SBEE),function(i) VwUi.f(SMi=SMi.unsat.xx[[i]],V.unsat=v.unsat.xx[[i]]))

	#8.14 - final volume of water after complete percolation (in L)

VwUf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) VwUf.f(VwUi=VwUi.xx[[i]],SWex=SWex.xx[[i]],As=4))

#VwUf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) ifelse(VwUf.xx[[i]] <= v.unsat.xx[[i]]*1000,VwUf.xx[[i]],v.unsat.xx[[i]]*1000))

	#8.15 - final moisture content unsat zone 

SMUf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) SMf.f(VwUf=VwUf.xx[[i]],V.unsat=v.unsat.xx[[i]]))

		#8.15.2 - correction of excess water over volume of soil
SMUf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) ifelse(SMUf.xx[[i]] > (3.5*4),(3.5*4)*mean(SDF$n.s),SMUf.xx[[i]]*mean(SDF$n.s)))

#8.16 - hydraulic conductivity function (m/h)

K_Oi.xx<-lapply(1:nrow(met.ts.SBEE),function(i) K_Oi.f(Ks=mean(SDF$Ks.x),SMi=SMi.unsat.xx[[i]],SMs=mean(SDF$n.s),n.van=mean(SDF$n.van)))

#8.17 - travel distance of percolation front (m)

z.perc.xx<-lapply(1:nrow(met.ts.SBEE),function(i) z.perc.f(K_Of=K_Oi.xx[[i]],TTperc=TTperc.xx[[i]]))

#8.18 - depth of percolation front (m)

Zpf.xx<-lapply(1:nrow(met.ts.SBEE),function(i) Zpf.f(Zwf=Zwf.xx[[i]],z.perc=z.perc.xx[[i]]))

######################################################################################
#9- Outputs DATA-FRAME compilation with variables of interest + informative
#############################################################

#infiltration rate
q.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(q_m_h=sapply(1:nrow(SDF),function(j) ifelse(SDF$z.point[j] < Zwf.xx[[i]],mean(SDF$Ks.x),ifelse(SDF$z.point[j] > Zwf.xx[[i]] & SDF$z.point[j] < Zpf.xx[[i]],q_perc.xx[[i]],0)))))

#final soil moisture

SM.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(SM=sapply(1:nrow(SDF),function(j)ifelse(SDF$z.point[j] < Zwf.xx[[i]],mean(SDF$n.s),SMUf.xx[[i]]))))


#depth of wetting front (informative)

ZWF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(ZWF_m=rep(Zwf.xx[[i]],nrow(SDF))))

#depth of percolation front (informative)

ZPF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(ZPF_m=rep(Zpf.xx[[i]],nrow(SDF))))

#saturated/unsaturated (informative)

SAT.xx<-lapply(1:nrow(met.ts.SBEE),function(i)data.frame(SAT=sapply(1:nrow(SDF), function(j) ifelse(SDF$z.point[j]<Zwf.xx[[i]],"S","U"))))

#percolation time (informative)
TP.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(TPerc_h=sapply(1:nrow(SDF),function(j) ifelse(SDF$z.point[j]<Zwf.xx[[i]],0,TTperc.xx[[i]]))))


#evapotranspiration

etp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(Esp_mm_d_m2=sapply(1:nrow(SDF),function(j)ifelse(SDF$z.point[j]<dx.x,met.ts.SBEE$Esp_mm_m2_d[i],0))))


#hydraulic conductivity function

K_Oi<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(K_Oi_m_h=sapply(1:nrow(SDF),function(j)K_Oi.f(Ks=SDF$Ks.x[j],SMi=SM.xx[[i]][j,1],SMs=SDF$n.s[j],n.van=SDF$n.van[j]))))


#DATA.FRAME

SDF.xx<-lapply(1:nrow(met.ts.SBEE),function(i)cbind(SDF,q.xx[[i]],SM.xx[[i]],SAT.xx[[i]],ZWF.xx[[i]],ZPF.xx[[i]],TP.xx[[i]],etp.xx[[i]],K_Oi[[i]]))


head(SDF.xx)



#############################################################################################################
#10-Matric suction calculation for wetting and drying events
##############################################################



#10.1 MS - wetting
	
MS.w.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_w_kPa=sapply(1:nrow(SDF.xx[[i]]),function(j) ifelse(SDF.xx[[i]]$z.point[j]<= SDF.xx[[i]]$ZWF_m[j],MS.w.f(a=SDF.xx[[i]]$a.van[j],q=(-1)*SDF.xx[[i]]$q_m_h[j],Ks=SDF.xx[[i]]$Ks.x[j],Ww=9.8,z=3.5-SDF.xx[[i]]$z.point[j]),MS.w.f(a=SDF.xx[[i]]$a.van[j],q=0,Ks=SDF.xx[[i]]$Ks.x[j],Ww=9.8,z=3.5-SDF.xx[[i]]$z.point[j])))))

#10.2 MS - drying 
 

MS.d.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_d_kPa=sapply(1:nrow(SDF.xx[[i]]),function(j) ifelse(SDF.xx[[i]]$z.point[j]<0.2,MS.d.f(a=SDF$a.van[j],Etp=(SDF.xx[[i]]$Esp_mm_d_m2[j])/24000,K_Oi=SDF.xx[[i]]$K_Oi_m_h[j],Ww=9.8,z=3.5-SDF.xx[[i]]$z.point[j],Ac=1,LAI=1),MS.d.f(a=SDF$a.van[j],Etp=0,K_Oi=SDF.xx[[i]]$K_Oi_m_h[j],Ww=9.8,z=3.5-SDF.xx[[i]]$z.point[j],Ac=1,LAI=1)))))


#10.3 matric suction due to water table


#water table depth from ground level
wt.d<-c(0.1,1,2) #m

MS.WT.1<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_WT1_kPa=sapply(1:nrow(SDF.xx[[i]]),function(j) ifelse(SDF.xx[[i]]$z.point[j]>=wt.d[1],MS.w.f(a=SDF.xx[[i]]$a.van[j],q=0,Ks=SDF.xx[[i]]$Ks.x[j],Ww=9.8,z=wt.d[1]-SDF.xx[[i]]$z.point[j]),MS.w.f(a=SDF.xx[[i]]$a.van[j],q=(-1)*SDF.xx[[i]]$q_m_h[j],Ks=SDF.xx[[i]]$Ks.x[j],Ww=9.8,z=wt.d[1]-SDF.xx[[i]]$z.point[j])))))

MS.WT.1[[1]]

MS.WT.2<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_WT2_kPa=sapply(1:nrow(SDF.xx[[i]]),function(j) ifelse(SDF.xx[[i]]$z.point[j]>=wt.d[2],MS.w.f(a=SDF.xx[[i]]$a.van[j],q=0,Ks=SDF.xx[[i]]$Ks.x[j],Ww=9.8,z=wt.d[2]-SDF.xx[[i]]$z.point[j]),MS.w.f(a=SDF.xx[[i]]$a.van[j],q=(-1)*SDF.xx[[i]]$q_m_h[j],Ks=SDF.xx[[i]]$Ks.x[j],Ww=9.8,z=wt.d[2]-SDF.xx[[i]]$z.point[j])))))

MS.WT.3<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_WT3_kPa=sapply(1:nrow(SDF.xx[[i]]),function(j) ifelse(SDF.xx[[i]]$z.point[j]>=wt.d[3],MS.w.f(a=SDF.xx[[i]]$a.van[j],q=0,Ks=SDF.xx[[i]]$Ks.x[j],Ww=9.8,z=wt.d[3]-SDF.xx[[i]]$z.point[j]),MS.w.f(a=SDF.xx[[i]]$a.van[j],q=(-1)*SDF.xx[[i]]$q_m_h[j],Ks=SDF.xx[[i]]$Ks.x[j],Ww=9.8,z=wt.d[3]-SDF.xx[[i]]$z.point[j])))))



#UPDATE DATA FRAME

SDF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) cbind(SDF.xx[[i]],MS.w.xx[[i]],MS.d.xx[[i]],MS.WT.1[[i]],MS.WT.2[[i]],MS.WT.3[[i]]))


#10.4 unified MS for when water table is high -

#water table depth from ground level
#wtd<-0.1 #m
###############

MS.unified.1<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_unified_1_kPa=sapply(1:nrow(SDF.xx[[i]]),function(j) ifelse(SDF.xx[[i]]$z.point[j] >= wt.d[1], SDF.xx[[i]]$MS_WT1_kPa[j],max(SDF.xx[[i]]$MS_d_kPa[j],SDF.xx[[i]]$MS_w_kPa[j])))))

MS.unified.1[[10]]


MS.unified.2<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_unified_2_kPa=sapply(1:nrow(SDF.xx[[i]]),function(j) ifelse(SDF.xx[[i]]$z.point[j] >= wt.d[2], SDF.xx[[i]]$MS_WT2_kPa[j],max(SDF.xx[[i]]$MS_d_kPa[j],SDF.xx[[i]]$MS_w_kPa[j])))))

MS.unified.3<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(MS_unified_3_kPa=sapply(1:nrow(SDF.xx[[i]]),function(j) ifelse(SDF.xx[[i]]$z.point[j] >= wt.d[3], SDF.xx[[i]]$MS_WT3_kPa[j],max(SDF.xx[[i]]$MS_d_kPa[j],SDF.xx[[i]]$MS_w_kPa[j])))))


#update data frame
SDF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) cbind(SDF.xx[[i]],MS.unified.1[[i]],MS.unified.2[[i]],MS.unified.3[[i]]))


#add dates, rainfall and temperature records...
date.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(DATE=rep(met.ts.SBEE$date[i],nrow(SDF))))
rain.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(rain_mm=rep(met.Catt$rainfall[i],nrow(SDF))))
temp.xx<-lapply(1:nrow(met.ts.SBEE),function(i) data.frame(T_air_C=rep(met.Catt$tas[i],nrow(SDF))))
SDF.xx<-lapply(1:nrow(met.ts.SBEE),function(i) cbind(SDF.xx[[i]],rain.xx[[i]],temp.xx[[i]],date.xx[[i]]))

###################
#11) save output
###################
#setwd("/Users/ollauri/Desktop/work/OPERANDUM/mini_projects/NBS_modelling/data/CC_runs")
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs")
save(SDF.xx,file="soil_slice_ts_SBEE25_run.RData")
