#Module ii - ECO-HYDROLOGICAL VARIABLES estimator
##################################################

#Contents:
###########

#0 - load functions
#i- growing season duration
#ii- mean rainfall during growing season
#iii-number of raninfall events during growing season
#iv-evapotranspiration
#v-aridity index


#0) load function
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model")
source("live_crib_wall_model_functions.R") 



#i-growing season
###################
#air temperature
###################
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/inputs")
t.rcp45<-read.csv("tas_rcp45.csv")
head(t.rcp45)

#1) add J variable1
##################
t.rcp45$date<-as.POSIXct(t.rcp45$date)


#subset from 2011 to 2100 [new]
t.rcp45<-subset(t.rcp45,t.rcp45$date >= "1950-01-01" & t.rcp45$date <= "2100-12-31")


t.rcp45$year<-format(t.rcp45$date,"%Y")
t.rcp45$month<-format(t.rcp45$date,"%m")
t.rcp45$day<-format(t.rcp45$date,"%d")

#set a vector of 1s on which perform cumsum operations

t.rcp45$ones<-rep(1,nrow(t.rcp45))

t.rcp45$J<-ave(t.rcp45$ones,t.rcp45$year,FUN=cumsum)


#2) growing degree days function
#################################

GDDf<-function(mT,baseT){
	if(mT>baseT){
		GDD<-mT-baseT
	}else{
		GDD<-0
	}
	return(GDD)
}

#growing degrees
t.rcp45$GDD<-sapply(1:nrow(t.rcp45),function(i) GDDf(mT=t.rcp45$tas[i],baseT=5))

#cummulative growing degree
t.rcp45$GDD.c<-ave(t.rcp45$GDD,t.rcp45$year,FUN=cumsum)
plot(t.rcp45$GDD.c~as.Date(t.rcp45$date),type="l")

#3) growing season starts
#########################
starts<-subset(t.rcp45,t.rcp45$GDD.c >=200 & t.rcp45$GDD.c <= 215)
start.gs<-starts[!duplicated(starts$year),]

#4) groiwng season ends
#########################
ends<-subset(t.rcp45,t.rcp45$GDD.c >=200 & t.rcp45$tas <= 2.5)
end.gs<-ends[!duplicated(ends$year),]

#comment: with the 2C lower boundary, 2069 is left out; as it is an extremely warm year in which T does not drop below 2 after the growing season

#5) merge starts and ends for further processing
################################################
GS.df<-data.frame(start.date=start.gs$date,year=start.gs$year,start.J=start.gs$J,end.date=end.gs$date,end.J=end.gs$J)


###################################################################################################
###################################################################################################
#ii-mean rainfall during growing season [i.e. alpha-climatic]
###############################################################
#alpha=total rainfall/total number of rainfall events
###############################
#1) load precipitation dataset
r.rcp45<-read.csv("pr_rcp45.csv")
head(r.rcp45)


#2) add J variable

r.rcp45$date<-as.POSIXct(r.rcp45$date)

#subset from 2011 to 2100 [new]
r.rcp45<-subset(r.rcp45,r.rcp45$date >= "1950-01-01" & r.rcp45$date <= "2100-12-31")



r.rcp45$year<-format(r.rcp45$date,"%Y")
r.rcp45$month<-format(r.rcp45$date,"%m")
r.rcp45$day<-format(r.rcp45$date,"%d")
r.rcp45$J<-t.rcp45$J


#3) subset source data.frame by growing season start and end dates [challenging]


library(dplyr)

#3.1 - split and group the time series into year groups 
XX.rcp45<-group_split(r.rcp45,r.rcp45$year)

#3.2 - subset each group on the basis of the growing season duration
YY.rcp45<-lapply(1:151,function(i) subset(XX.rcp45[[i]],XX.rcp45[[i]][,4] >= GS.df$start.J[i] & XX.rcp45[[i]][,4] <= GS.df$end.J[i]))

#4) calculate alpha = total rainfall / No of rainfall event

sum(r.rcp45$pr != 0)
sum(r.rcp45$pr == 0)
nrow(r.rcp45)

#setting a minimum rainfall threshold of 0.2 mm - as there are many event with very little rainfall

alpha.rcp45<-lapply(1:length(YY.rcp45),function(i) sum(YY.rcp45[[i]]$pr)/sum(YY.rcp45[[i]]$pr > 0.2)) #they are abit low, but in agreement with previously reported for Catterline
#this will update GS.df


#5) check outcomes
years<-seq(from=as.Date("1950-01-01"),to=as.Date("2100-12-31"),by="year")

plot(unlist(alpha.rcp45)~years,type="l",main="Alpha-C rcp4.5 & rcp8.5")

#6) total annual rainfall

r.rcp45.year<-lapply(1:length(XX.rcp45),function(i) sum(XX.rcp45[[i]]$pr))

#7) total rainfall growing season

r.rcp45.GS<-lapply(1:length(XX.rcp45),function(i) sum(YY.rcp45[[i]]$pr))

#############################################################################
#iii - frequency of rainfall events
###################################
#setting a minimum threshold of 0.2 mm

lambda.rcp45<-lapply(1:length(YY.rcp45),function(i) sum(YY.rcp45[[i]]$pr > 0.2)/length(YY.rcp45[[i]]$pr)) #good

##########################################################################################
#iv - evapotranspiration totalS
###############################
#1 -load relevant datasets, check units, implement unit conversions where appropriate

#load relative humidy
hr.rcp45<-read.csv("hr_rcp45.csv")
#load atmospheric pressure at sea level in Pascals (i think)
psl.rcp45<-read.csv("psl_rcp45.csv")
#load surface downwelling shortwave radiation in (W/m2)
rs.rcp45<-read.csv("rsds_rcp45.csv")


#subset new data
hr.rcp45<-subset(hr.rcp45,hr.rcp45$date >= "1950-01-01" & hr.rcp45$date <= "2100-12-31")
psl.rcp45<-subset(psl.rcp45, psl.rcp45$date >= "1950-01-01" & psl.rcp45$date <= "2100-12-31")
rs.rcp45<-subset(rs.rcp45, rs.rcp45$date >= "1950-01-01" & rs.rcp45$date <= "2100-12-31")

#conversion form W to MJ; as 1 W == 1 J/s
rs.rcp45$Rs<-rs.rcp45$rsds*24*60*60/1e06
summary(rs.rcp45$Rs) #this is it! - it matches with the outputs from Rs in regulat Etp.model runs

#2 - run Etp model
 
PSH.X<-PSH.f(AP= psl.rcp45$psl*1e-03) #pshychrometric constant (kPa)
A.X<-A.f(Tk=t.rcp45$tas+274.5) #slope of staturation vapour pressure
dr.X<-dr.f(J=t.rcp45$J,π=3.1416) #inverse relative distance Earth-Sun
DEC.X<-DEC.f(J=t.rcp45$J,π=3.1416) #declination
ws.X<-ws.f(DEC=DEC.X,LAT=56.89*0.0174532925) #sunset hour angle
N.X<-N.f(π=3.1416,ws=ws.X) #maximum duration of sunshine
#Ra.X<-Ra.f(π=3.1416,Gsc=0.0820,dr=dr.X,ws=ws.X,LAT=56.89*0.0174532925,DEC=DEC.X) #extraterrestial radiation
#Rs.X<-Rs.f(as=0.25,bs=0.50,n=met.ts$sunshine,N=N.X,Ra=Ra.X) #incoming solar radiation
a.X<-a.f(S.al=0.26,Cf=Cf.f(Ma=61400)) #albedo considering oak - 61400 kh/ha
Rnl.X<-Rnl.f(a=a.X,Rs= rs.rcp45$Rs) #net solar radiation in MJ/m2 day
Rnl_ly.X<-Rnl.X*23.9 #in ly
Eu.X<-Eu.f(Rnl=Rnl_ly.X,A=A.X,PSH=PSH.X)
Eu_mm.X<-Eu_m.f(Eu=Eu.X,T=t.rcp45$tas)*1000 #potential Etp in mm/m2 d
#Esp.X<-Esp.f(Eu=Eu_mm.X,LAI=3.26,As=1) #evaporation under the canopy
#Etp.X<-Etp.f(Esp=Esp.X,Eu=Eu_mm.X,As=1) #plant transpiration [not convincing]

#3-build data frame for further processing
etp.rcp45<-data.frame(date=t.rcp45$date,year=t.rcp45$year,J=t.rcp45$J,etp=Eu_mm.X)

#4 - split and group data frame by years

etp.rcp45.XX<-group_split(etp.rcp45,etp.rcp45$year)

#5 - subset etp time series in terms of growing season duration

etp.rcp45.YY<-lapply(1:151,function(i) subset(etp.rcp45.XX[[i]],etp.rcp45.XX[[i]][[3]] >= GS.df$start.J[i] & etp.rcp45.XX[[i]][[3]] <= GS.df$end.J[i]))

#6- calculate total Etp duting the growing season and year

etp.sum.rcp45<-lapply(1:151,function(i) sum(etp.rcp45.YY[[i]]$etp))
#to be updated into GS.df

etp.rcp45.year<-lapply(1:151,function(i) sum(etp.rcp45.XX[[i]]$etp))
#####################################################################################################
#####################################################################################################
#v-aridity index
################
Ai.GS.rcp45<-lapply(1:151,function(i) sum(YY.rcp45[[i]]$pr)/etp.sum.rcp45[[i]]) #during the growing season

Ai.rcp45<-lapply(1:151,function(i) sum(XX.rcp45[[i]]$pr)/sum(etp.rcp45.XX[[i]]$etp))   #over the entire year cycle
#####################################################################################################
#vi-update data.frame and export results
###########################################
GS.rcp45<-data.frame(year=GS.df$year,GS.start=GS.df$start.date,GS.end=GS.df$end.date,J.start=GS.df$start.J,J.end=GS.df$end.J,alpha.C=unlist(alpha.rcp45),lambda.C=unlist(lambda.rcp45),rain.GS=unlist(r.rcp45.GS),rain.year=unlist(r.rcp45.year),etp.GS=unlist(etp.sum.rcp45),etp.year=unlist(etp.rcp45.year),Ai.GS=unlist(Ai.GS.rcp45),Ai.year=unlist(Ai.rcp45))

setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/inputs")
write.csv(GS.rcp45,"growing_season_rcp45.csv")

#met.ts data.frame

met.ts.rcp45<-data.frame(date=t.rcp45$date,year=t.rcp45$year,tas=t.rcp45$tas,J=t.rcp45$J,pr=r.rcp45$pr,ps=psl.rcp45$psl**1e-03,Rs=rs.rcp45$Rs)
write.csv(met.ts.rcp45,"met_ts_rcp45.csv")




