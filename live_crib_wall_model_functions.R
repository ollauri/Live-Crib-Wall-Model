#18/05/2020 - LIVE CRIBWALL MODEL [LiveCW] (revisited)
##############################################

#LIST OF EQUATIONS
##################
#A-slope geometry
##########################################################
#Eq.1: intersection of circunference with slice boundaries
#Eq.2: coordinate points correction respect to circumference origin
#Eq.3: linear distance between two intersection points located on slice boundaries
#Eq.4: arc angle
#Eq.5: arc length 
#Eq.6: slope of slice/soil wedge
#Eq.7: x-point of arc centre
#Eq.8: y-point of arc centre
#Eq.9: regression parameters for line comprised between two topographic points
#Eq.10: z-ground - y-point of ground surface on the slope profile for a given soil wedge
#Eq.11: Z.slip - depth of soil wedge or z-distance between slip arc and ground surface
#Eq.12: intersection X-point between slip arc and geo-grid
#Eq.13: geo-grid reinforcement length
#Eq.14: depth to member from ground surface
#Eq.15: distribution of geo-grid length among reinforced slices
#Eq.16: Z-distance from slip arc to cribwall stretchers
########################################################
#B-stability analysis
######################
#Eq.17: sliding force
#Eq.18: sliding resistance
#Eq.19: pull-out resistance of geo-grid members
#Eq.20: bending resistance of geo-grid members
#Eq.21: resistance to bending of a geo-grid member (torque)
#Eq.22: bending strength of a geo-grid memeber
#Eq.23: wall-face contribution to stability
#Eq.24: embedment strength of nail into cribwall memeber
#Eq.25: plastic moment of nail 
#Eq.26: force standed by timber-timber unions provided by single shear (i.e. Johansen Equations)
#Eq.27: active lateral earth pressure (only on first slice with cribwall)
#Eq.28: overturning resistance force due to lateral earth pressure
#Eq.29: overturning force due to lateral earth pressure
#Eq.30: sliding resistance due to lateral earth pressure
#Eq.31: sliding force due to lateral earth pressure
#Eq.32: factor of safety
####################################################
#C-soil geotechnical parameters
################################
#Eq.33: Moist unit weight of soil 
#Eq.34: weight of soil column
#################################
#D- soil hydro-mechanical parameters
####################################
#Eq.35: matric suction of wetting front
#Eq.36: field capacity
#Eq.37: wilting point
#Eq.38: saturated hydraulic conductivity
#Eq.39: hydraulic conductivity function
########################################
#E-cribwall members - load and decay
#####################################
#Eq.40: volume of timber in the cribwall per unit length of cribwall
#Eq.41: total cribwall volume 
#Eq.42: self-weight of cribwall and filling soil 
#Eq.43: timber decay rate
#Eq.44: time lag for timber beginning of decay
#Eq.45: decay depth of timber 
##############################
#F-Hydrological model
#####################
#Eq.46: Suction stress
#Eq.47: matric suction under wetting conditions
#Eq.48: martric suction under drying conditions
#Eq.49: Cummulative infiltration at ponding
#Eq.50: depth of wetting front
#Eq.51: runoff
#Eq.52: actual infiltration after ponding
#Eq.53: percolation
#Eq.54: Volume of water in that portion of soil
#Eq.55: Volume of water at FIELD CAPACITY (m3)
#Eq.56: Excess volume of water (i.e. percolation volume; liters/m2 of soil==mm)
#Eq.57: travel time for percolation
#Eq.58: percolation rate
#Eq.59: Unsaturated soil profile volume (m3)
#Eq.60: Initial volume of water in the unsaturated zone (liters)
#Eq.61: Final water volume after complete percolation (liters)
#Eq. 62: Final moisture content after complete percolation (/1)
#Eq. 63: Travel distance of the percolating front in the unsatruated soil (m)
#Eq.64: Depth of the percolation front (m)
###########################################
#G-Evapotranspiration model
############################
#Eq.65: POTENTIAL EVAPOTRANSPIRATION; Priestly-Taylor (1972) 
#Eq.66: Eu transformation to m/m2 d
#Eq.67: PSHYCHROMETRIC CONSTANT (KPa/ºC)
#Eq.68: SLOPE OF SATURATION VAPOUR PRESSURE CURVE (KPa/ºC)
#Eq.69: NET SOLAR RADIATION (MJ/m2.day); x 0.408==mm/day
#Eq.70: INCOMING SOLAR RADIATION (Rs) (MJ/m2.day)
#Eq.71: DAILY ESTIMATION OF THE EXTRATERRESTIAL RADIATION (Ra) (MJ/m2.day)
#Eq.72: Inverse relative distance Earth-Sun
#Eq.73: Solar declination (rad)
#Eq.74: Sunset hour angle (rad)
#Eq.75: Maximum duration of sunsine (hours)
#Eq.76: Albedo
#Eq.77: Coefficient for estimating species-specific albedo
#Eq.78: POTENTIAL SOIL EVAPORATION (under canopy) (mm/d)
#Eq.79: POTENTIAL PLANT TRANSPIRATION (mm/d)
#Eq.80: potential depth soil evaporation (m)
############################################
#H-VEGETATION
###############
#Eq.81: tree biomass
#Eq.82: gross-rainfall
#Eq.83: canopy cover fraction
#Eq.84:effective rainfall
#Eq.85: stemflow
#Eq.86: mean rooting depth temeperate humid climates (mm)
#Eq.87: Mean rooting depth dry cliamtes (mm)
#Eq.88: Root cross-sectional area at ground surface (cm2)
#Eq.89: Root cross-sectional area with soil depth (cm2)
#Eq.90: Root area ratio
#Eq.91: root added cohesion
#Eq.92: Arc length with crossing roots [NOVEL]
#Eq.93: PULL-OUT STRENGTH [for roots > 3 mm] (kPa == kN/m2)
#Eq.94: vegetation surcharge (kN/m2)

#LIST OF VARIABLES
####################
#A-slope geometry
###################
#r.c: radius of circumference (m)
#x: x-point of slice boundary (m)
#a.o: x-point of circumference origin (m)
#b.o: y-point of circumference origin (m)
#y: y-point (initial) of intersection between slice boundary and circumference (m)
#y.f: y-point (final) of intersection between slice boundary and circumference (m)
#x.1: x-point of intersection point on right-hand side slice boundary (m)
#y.1: y-point of intersection point on right-hand side slice boundary (m)
#x.2: x-point of intersection point on left-hand side slice boundary (m)
#y.2: y-point of intersection point on left-hand side slice boundary (m)
#c.x: linear distance between intersection points on slice boundaries (m)
#a: length of arc boundary A  -i.e. circumference raidus (m)
#b: length of arc boundary B -i.e. circumference radius (m)
#X: anlge of the arc comprised between to slice boundaries (deg - rad)
#arc.L: arc length (m)
#alpha: slope of soil wedge (deg - rad)
#sl.b: slice width (m)
#x.m: middle x-point of string between two intersection points on slice boundaries (m)
#y.m: middle y-point of string between two intersection points on slice boundaries (m)
#h.arc: arc height - distance between string centre (x.m,y.m) and arc centre (m)
#x.c.arc: x-point arc centre (m)
#y.c.arc: y-point arc centre (m)
#xt.1: x-coordinate of topographic point 1 (m)
#yt.1: y-coordinate of topographic point 1 (m)
#xt.2: x-coordinate of topographic point 2 (m)
#yt.2: y-coordinate of topographic point 2 (m)
#a.l: slope of linear equation between two topographic points
#b.l: intercept of linear equation between two topographic points
#x.m.sl: x-point of the middle of a given slice (or soil wedge) (m)
#Z.slip: height of soil wedge (m) - Z-distance between slip arc and ground surface
#geo.grid.L: X-Y point inbtersection of geo-grid with slip arc (m)
#mb.L: geo-grid member length (m)
#rf.gg.L: reinforcement length of geo-grid (m) [also Le]
#d.t.m: depth to cribwall memeber from ground surface (m)
#z.gr.slX: y-point of slice X at ground level (m)
#z.member: y-point of ith cribwall memeber (m) 
#Le: geo-grid reinforcement length (m)
#y.str: y-point of cribwall stretcher (m)
#y.arc: y-point of slip arc in a given slice or soil wedge (m)
#dist.str: distance to stretcher from slip arc (m) [for face-wall contribution]
###############################################################################
#B-stability analysis
#####################
#Fs: sliding force for a given soil wedge (kN/m)
#Ws: weight of soil wege (kN/m)
#alpha: slope of soil wedge (deg - rad)
#Fr: sliding resistance force for a given soil wedge (kN/m)
#cR: root added cohesion (kN/m2)
#c: soil apparent cohesion (kN/m2)
#Ln: arc length for a given soil wegde (m)
#SS: suction stress (kN/m2)
#FrA: angle of internal friction (deg - rad)
#bR: soil-root bond (kN/m2)
#lR1: thin root length across slip plane (m)
#lR2: thick root emebedment into slip plane  (m)
#Fgr.pll: geo-grid contribution due to pull-out resistance force (kN/m) 
#z: depth of geo-grid memeber [also d.t.m] (m)
#Le: geo-grid reinforcement length (m)
#Uw: unit weight of soil under variably saturated conditions (kN/m3)
#f: coefficient to determine skin friction (0.7: clay & silt; 0.85: sand & silty sand; 0.9: gravek)
#Fgr.bn: geo-grid contribution due to bending resistance (kN/m)
#BnSt: bending strength of a timber memeber (kN/m2)
#Rt: resistance to bending (torque) of a cribwall memeber (kN.m)
#D: initial diameter of cribwall member (m)
#dt: decay depth of timber at time "t" (m)
#fd: resistance to bending of a given timber material (kN/m2) - [literature]
#Lm: length of cribwall memeber
#Dm: diameter of timber member
#Vm: volume of timber member
#Ffd: wall-face force distribution within cribwall (kN/m)
#Fi: force provided by the shear resistance of the union between memebers (N) at a union point
#dist.str: distance to stretcher from slip arc (m) [for face-wall contribution]
#L.str: length of stretcher member (m)
#fh: embedment strength of the union timber-nail (N/mm2)
#a.n: nail diameter (mm)
#d.w: mass density of timber used in cribwall (kg/m3)
#My: plastic moment [torque resistance] of nail (N.mm)
#fa:torque resistance of metallic materials (N/mm) [600 N/mm for steel nails]
#Rk.t: force stood by timber-timber unions provided by single shear (N)
#t1: penetration length of nail into memeber 1 (mm)
#t2: penetration length of nail into memeber 2 (mm)
#r.e: ratio of characteristic emebedment strength between two members [i.e. timber-timber=1]
#Ea: active earth pressure per unit length of cribwall (KN/m)
#Sp: sub-pressure [not considered herein] kN/m
#B: batter angle - inclination angle of the cribwall's inner wall to the horizontal (deg-rad) -e.g. 90o if straight
#FrA: angle of internal friction (deg - rad)
#Sk: skin friction (i.e. 0.75*FrA) (deg - rad)
#ii: slope angle above the cribwall (deg - rad)
#Ka: coefficient of active earth pressure (dimensionless)
#Uw: unit weight of soil under variably saturated conditions (kN/m3)
#H: cribwall height (m)
#Ea.r.ov: overturning resistance due to lateral earth pressure (kN/m) [added to Ws]
#d3: factor to establish resulting force on cribwall
#Ea.ov: overturning force due to lateral earth pressure (kN/m) [added to Ws]
#Lcb: depth of cribwall (m)
#d2: factor to establish resulting force on cribwall
#Ea.r.s: sliding resistance force due to lateral earth pressure (kN/m) [added to Ws]
#Ea.s: sliding force due to lateral earth pressure (kN/m) [added to Ws]
##########################################################
#C-Soil geotechnical parameters
################################
#Uw.w: moist unit weight of soil (kN/m3)
#SM: soil moisture (/1)
#Gs: specific gravity of soil
#Ww: unit weight of water (kN/m3)
#e: voids ratio; e=n/(1-n) where "n" is soil porosity
#L: length of soil wedge [also arc.L] (m)
#Hss: height of soil wedge [also Z.slip] (m)
#SRCH: surcharge (kN/m2) 
##############################################
#D-Soil hydro-mechanical parameters
####################################
#Sf: matric suction of wetting front (m)
#n.s: soil porosity (/1)
#Sa: soil sand content (%)
#Si: soil silt content (%)
#Cl: soil clay content (%)
#o33: soil moisture retained at 33 kPa
#OM: soil organic matter contetn (%)
#FC: soil moisture content at field capacity (/1)
#o1500: soil moisture retained at 1500 kPa
#WP: moisture contetn at wilting point
#Ks: saturated hydraulic conductivity (m/h)
#SMs: soil moisture content at saturation
#K_Oi: hydraulic conductivity function (m/s)
#Ks: saturated hydraulic conductivity for K_Oi (m/s)
#SMi: soil moisture (/1)
#SMs: soil moisture at saturation (/1)
#n.van: pore-size distribution parameter (van Genutchen) (unitless)
########################################
#E-cribwall members - load and decay
#####################################
#Dm: diameter of timber logs (m) [also D and Dm]
#T.Lcb: total length of cribwall members (m)
#Hcb: heigh of cribwall (m) [also H]
#Lcb: depth of cribwall (m)
#Wd.cb: width of cribwall (m) - [also Lstr = length of stretcher] 
#Uw.cb: combined unit weight timber + filling soil (kN/m3)
#Vt: volume of timber in the cribwall (m3)
#Uw.t: unit weight of timber (kN/m3) [lookpup in tables]
#Vs: volume of soil column (m3)
#Uw.s: unit weight of soil (kN/m3)
#Vcb: volume of cribwall (m3)
#Hcb: height of cribwall (m)
#Lcb: depth of cribwall (m)
#aa: angle/inclination of cribwall to the horizontal (deg - rad)
#R: mean annual rainfall (mm)
#Ta: mean annual temperature (deg)
#k.R: decay coefficient due to rainfall
#k.T: decay coefficient due to temeperature
#k.climate: decay coefficient due to climate
#k.wood: decay coefficient due to timber material [class A:0.20; class B: 0.55; class C: 0.80' class D: 1.85]
#rtd: decay rate of timber
#t.lag: lag time for the timber to begin decaying
#dcy: decay depth of timber (mm)
#t.y: time in years
#############################################
#F-Hydrological model
######################
#SS: suction stress (kN/m2)
#MS: matric suction (kPa == kN/m2)
#a:inverse of the air-entry pressure (kPa-1)
#n.van: pore-size distribution parameter (unitless)
#q: infiltration rate (m/s)
#Ks: saturated hydraulic conductivity (m/s)
#Ww: unit weight of water (kPa/m == kN/m3)
#z: soil depth - z-coordinate belowground (m) [also y.slip]
#Etp: evapotranspiration rate (m/s)
#K_Oi: hydraulic conductivity function (m/s)
#Fp: cummulative infiltration after ponding (m)
#Sf: matric suction of the wetting front (m) - pedotransfer functions
#Ks: saturated hydraulic conductivity (m/s)
#SMs: soil moisture content at saturation (/1)
#SMi: initial soil moisture content (/1)
#P: precipitation intensity (m/s)
#Zwf: depth of wetting front (m)
#RNF: runoff (mm)
#ER: effective rainfall (mm)
#Fp: cumulative infiltration at ponding (mm)
#Ks: saturated hydraulic conductivity (mm/s)
#tr: rainfall duration (s)
#AI: actual infiltration (mm)
#slope angle
#Vsat: volume of saturated soil (m3)
#Zwf: depth of wetting front (m)
#As: area of the soil slice (m2)
#Vw: volume of water in saturated portion of soil (m3)
#Vsat: volume of saturated soil (m3)
#MSs: soil moisture content at saturation (/1)
#wFC:volume of water at field capacity (m3)
#FC: moisture content at field capacity (/1) - see pedotransfer functions
#SWex: excess volume of water or percolation water volume (mm or l/m2)
#wFC: volume of water at field capacity (m3)
#Vw: volume of water in the saturated soil profile (m3)
#SWex: excess volume of water or percolation water volume (mm)
#Ks: saturated hydraulic conductivity (m/h)  
#q_perc: percolation rate (m/h)
#SWex: excess volume of water or percolation water volume (mm)
#t.step (h)
#V.unsat: volume of unsaturated soil (m3)
#sDP: soil profile depth (m) - or depth of the slice or soil wedge
#Zwf: depth of wetting front (m)
#VwUi: initial volume of water in the unsaturated zone (l)
#SMi: initial soil (volumetric) moisture content (/1)
#V.unsat" volume of unsaturated soil (m3)
#VwUf: Final water volume after complete percolation (liters)
#VwUi:initial volume of water in the unsaturated zone (l)
#SWex: excess volume of water or percolation water volume (mm or l/m2)
#VwUf: Final water volume after complete percolation (liters)
#Vunsat: volume of unsaturated soil
#z.perc: traveling distance of the percolating front (m)
#K_Of: hydraulic conductivity at the final soil moisture content in the unsaturated zone (m/s)
#TTperc: percolation time (s)
#Zpf: depth of percolation fron + wetting front (m)
#Zwf: depth of wetting front (m)
#z.perc: depth of percolation front (m)
###########################################
#G-Evapotranspiration model
############################
#Eu=Potential Evapotranspiration (mm/ m2.d)
#A=Slope of saturation capur pressure curve (KPa/ºC)
#PSH=Pshychrometric constant (KPa/ºC)
#AP=Atmospheric pressure (KPa)
#T=mean air temperature (ºC)
#Rnl=Net Solar Radiation (MJ/m2.day); x 0.408==mm/day
#a=albedo (unitless)
#S.al=Soil albedo
#Cf=albedo coefficient
#Ma=above-ground biomass (Kg/ha) for estimating Cf
#Rs=incoming solar radiation (MJ/m2.day)
#as=0.25 Angstrom value 1
#bs=0.50 Angstrom value 2
#n=actual duration of sunshine (hours)
#N=maximum duration of sunshine (hours)
#Ra=Extraterrestial radiation (MJ/m2.day)
#π=3.1416
#Gsc=0.0820; Solar constant (MJ/m2*min)
#dr=inverse reltive distance Earth-Sun (unitless ??) (1/m ?)
#ws=sunset hour angle (radians)
#LAT=Latitude (radians)
#DEC=Solar declination (radians)
#J=Day number in a year 
#Esp=Potential soil evaporation (mm/day)
#Etp=Potential plant transpiration (mm/day)
#LAI=Leaf area index (unitless)
#As=area of soil wedge (m2)
##############################
#H-Vegetation model
#####################
#DBH=diameter at breast height (m)
#Pg: gross rainfall (mm)
#P: rainfall intensity (m/h)
#tr: rainfall duration time (h)
#cc: canopy cover fraction
#LAI: leaf area index
#kc: light extinction coefficient
#ER: effective rainfall reaching the ground surface under vegetation (mm)
#S: canopy sotrage per unit area (mm/m2)
#Ac.a: canopy-crown area (m2)
#Ps.a: stemflow yield (l/m2)
#st.i: stemflow coefficient - intercept
#st.s: stemflow coefficient - slope
#St.v: stemflow volume (l)
#Ac.b: canopy-crown area
#b: mean rooting depth (mm)
#AlphaC: mean rainfall intensity during the growing season (mm)
#n.s: soil porosity
#FC: soil moisture content at filed capacity
#WP: soil moisture content at wilting point
#Yo: frequency of rainfall events during geowing season (unitless - i.e. No of rainfall events/No of days growing season)
#Aro: root collar cross-sectional area (cm2)
#Ma: aboveground biomass (g)
#Beta.A: allometric Beta parameter
#Alpha.A: allometric Beta parameter
#R.dens: root mass density (g/cm3)
#Arz: root cross-sectional area (cm2)
#z: soil depth starting at ground surface
#RARz: root area ratio per soil depth
#As: area of soil wedge or slice (cm2)
#cR: root added cohesion (kPa == kN/m2)
#Tr: root tensile strength (kPa == kN/m2)
#Ln: arc length (m)
#bR: soil-root bond strength (kN/m2)
#z:soil depth (m) - this control confining pressure/stress produced by soil load
#Uw: unit weight of soil (kN/m3) - see Equations above under varianly saturated soil
#FrA: angle of internal friction (deg - rad)
#f: correction factor - coefficient of friction between root and soil - equivalent to skin friction [0.7: clay and silts-0.85: sand and silty sand; 0.9: gravel]
#WV: vegetation surcharge (kN/,2)
#g: gravitational aceleration (m/s^2)

###########
#FUNCTIONS
###########
####################
#A-slope geometry
####################

#Eq.1 & 2: intersection of circunference with slice boundaries
cm.f<-function(r.c,x,b.o,a.o){
	y<-sqrt(r.c^2-(x-a.o)^2)
	y.f<-b.o-y
	return(y.f)}

#Eq.3: linear distance between two intersection points located on slice boundaries

dt.f<-function(x.1,y.1,x.2,y.2){
 	c.x<-sqrt(abs((x.2-x.1)^2)+abs((y.2-y.1)^2))
 	return(c.x)
 }
 
#Eq.4: arc angle

 arc.angle.f<-function(a,b,c.x){
 	X<-acos((b^2+a^2-c.x^2)/(2*a*b)) #angle of the arc, where a and b are the radius and c is the is c.x calculated in Eq. 3
 	X.2<-(X*180)/pi #angle in degrees
 	return(X.2)
 }

#Eq.5: arc length

arc.L.f<-function(X.2,r){
 	arc.L<-2*pi*r*X.2/360 #length of arc
 	return(arc.L)
 }

#Eq.6: slope of soil wedge 

alpha.f<-function(sl.b,arc.L,x,a.o){
 	a1<-acos(sl.b/arc.L)
 	a2<-(a1*180)/pi
 	if(x>=a.o){
 		alpha<-a2
 	}else{
 		alpha<-a2*(-1)
 	}
 	return(alpha)
 }

#x: x-point of slice under evalaution for angle sign correction

#Eq.7: X-point of arc centre

x.c.arc.f<-function(x.1,x.2,y.1,y.2,X,r.c){
 	x.m<-(x.1+x.2)/2
 	y.m<-(y.1+y.2)/2
 	h.arc<-r.c*(1-cos(X/2))
 	x.c.arc<-x.m+h.arc
 	y.c.arc<-y.m-h.arc
 	return(x.c.arc)
 } 


#Eq.8: Y-point of arc centre

y.c.arc.f<-function(x.1,x.2,y.1,y.2,X,r.c){
 	x.m<-(x.1+x.2)/2
 	y.m<-(y.1+y.2)/2
 	h.arc<-r.c*(1-cos(X/2))
 	x.c.arc<-x.m+h.arc
 	y.c.arc<-y.m-h.arc
 	return(y.c.arc)
 } 
 
 
#Eq.9: regression parameters of line between two topographic points

line.f<-function(xt.1,xt.2,yt.1,yt.2){
	a.l<-(yt.1-yt.2)/(xt.1-xt.2) #i.e. line slope
	b.l<-yt.1-xt.1*a.l #intercept
	return(c(a.l,b.l))
	}
	
#Eq.10: Z.ground - y-point of ground surface on slope profile for a given soil wedge

Z.ground.f<-function(a.l,b.l,x.m.sl){
	z.g<-a.l*x.m.sl+b.l
	return(z.g)
} 



#Eq.11: Z-slip - Z.distance between slip arc and ground surface

Z.slip.f<-function(z.g,y.arc){
	Z.slip<-z.g-y.arc
	return(Z.slip)
}



#Eq.12: intersection X-point between slip arc and gep-grid

geo.grid.L.f<-function(y,b.o,a.o,r.c){
	x.c<-sqrt(r.c^2-(y-b.o)^2)
	x.f<-a.o-x.c
	return(x.f)
}

#Eq.13: geo-grid reinforcement length

rf.gg.L.f<-function(geo.grid.L,mb.L){
	rf.gg.L<-mb.L-geo.grid.L
	return(rf.gg.L)
}

#Eq.14: depth to cribwall member from ground surface

depth.to.member.f<-function(z.gr.sl,z.member){ 
	d.t.m<-z.gr.sl-z.member
	return(d.t.m)
}

#Eq.15: distribution of geo-grid length among reinforced slices

Le.sl.f<-function(Le,sl.b){
	if(Le > sl.b){
		Le.sl1<-Le-(Le-sl.b)
		Le.sl2<-Le-(Le-(Le-sl.b))}
	if(Le <= sl.b){
		Le.sl1<-Le
		Le.sl2<-0
	}
	return(c(Le.sl1,Le.sl2))	
}

#Eq. 16: distance to stretcher from slip arc

dist.str.f<-function(y.str,y.arc){
	dist.str<-abs(y.str-y.arc)
	return(dist.str)
}


###################################################################################
##################################################################################
#29/06/2020 - NEW ADDED EQUATIONS TO ESTIMATE INTERSECTION POINT BETWEEN SLIP ARC AND SLOPE TOPOGRAPHY
#for when slip arc is outside slope envelop; generally in the first slices
############################################################################
#comment: the outcome from this algorithm should correct the intersection points with slice boundaries 
#where appropriate
#note that the intersection between a line and a circle always occurs at two points
#line => y=mx+d
#circle => (x-a)^2+(y-b)^2=r^2


x.int.1.f<-function(a,b,m,d,r){
	A<-r^2*(1+m^2)-(b-m*a-d)^2
	x1<-(a+b*m-d*m-sqrt(A))/(1+m^2)
	return(x1)
}


x.int.2.f<-function(a,b,m,d,r){
	A<-r^2*(1+m^2)-(b-m*a-d)^2
	x2<-(a+b*m-d*m+sqrt(A))/(1+m^2)
	return(x2)
}


y.int.1.f<-function(a,b,m,d,r){
	A<-r^2*(1+m^2)-(b-m*a-d)^2
	y1<-(d+a*m+b*m^2-m*sqrt(A))/(1+m^2)
	return(y1)
}

y.int.2.f<-function(a,b,m,d,r){
	A<-r^2*(1+m^2)-(b-m*a-d)^2
	y2<-(d+a*m+b*m^2+m*sqrt(A))/(1+m^2)
	return(y2)
}


#x.int.1.f(a=1,b=4,m=1,d=0,r=3.777)
#x.int.2.f(a=1,b=4,m=1,d=0,r=3.777)
#y.int.1.f(a=1,b=4,m=1,d=0,r=3.777)
#x.int.2.f(a=1,b=4,m=1,d=0,r=3.777)


#x.int.1.f(a=1,b=4,m=1,d=0,r=3.4444)
#y.int.1.f(a=1,b=4,m=1,d=0,r=3.4444)






############################
#B-STABILITY ANALYSIS
###########################

#Eq. 17: sliding force of a given soil wedge

Fs.f<-function(Ws,alpha){
	Fs<-Ws*sin(alpha)
	return(Fs)
}

Fs.f<-function(Ws,alpha){
	Fs<-Ws*sin(abs(alpha))
	return(Fs)
}

#Eq. 18: sliding resistance force - including vegetation

Fr.f<-function(cR,c,Ln,W,alpha,SS,FrA,bR,lR1,lR2){
	Fr<-bR*lR2+cR*lR1+c*Ln+(W*cos(alpha)-SS*Ln)*tan(FrA)
	return(Fr)
}

Fr.f<-function(cR,c,Ln,W,alpha,SS,FrA,bR,lR1,lR2){
	Fr<-bR*lR2+cR*lR1+c*Ln+(W*cos(abs(alpha))-SS*Ln)*tan(FrA)
	return(Fr)
}

#Eq.19: pull-out resistance of a geo-grid memeber

Fgr.pll.f<-function(z,Uw,Le,f,FrA){
	Fgr.pll<-2*z*Uw*Le*f*tan(FrA)
	return(Fgr.pll)
}

#2: accounts for the contribution of the two sides of the member

#Eq.20: geo-grid contribution in terms of bening (kN/m) 

Fgr.bn.f<-function(BnSt,Le){
	Fgr.bn<-BnSt*Le
	return(Fgr.bn)
}

#Eq.21: RESISTANCE TO BENDING (TORQUE) (kN.m)

Rt.f<-function(D,dt,fd){
	Rt<-(pi/32)*(D-dt)^3*fd
	return(Rt)
}

#Eq.22: Bending strength (kN/m2) of a timber member

BnSt.f<-function(Rt,Dm,Lm){
	Vm<-pi*(Dm/2)^2*Lm
	BnSt<-Rt/Vm
	return(BnSt)
}

#Eq. 23: Wall-face contribution (kN/m)

Ffd.f<-function(Fi,dist.str,L.str){
		Ffd.A<-Fi-abs(Fi*(0.12/100)*dist.str)
		Ffd.B<-Ffd.A/L.str
	return(Ffd.B)
}

#Eq.24: EMBEDMENT STRENGTH (fh) - N/mm2

fh.f<-function(a.n,d.w){
	fh<-0.082*(1-0.01*a.n)*d.w
	return(fh)
}

#Eq.25:  PLASTIC MOMENT OF the nail (N.mm)

My.f<-function(fa,a.n){
	My<-(fa/600)*180*a.n^2.6
	return(My)
}

#Eq.26: FORCE STOOD BY THE TIMBER - TIMBER UNIONS provided by SINGLE SHEAR - Johansen equation, one for each type of failure between two blocks [see timber-connections.pdf]


Rk.t.f<-function(fh,t1,t2,a.n,r.e,My){
	R.1<-fh*t1*a.n
	R.2<-fh*t2*a.n
	R.3<-(fh*t1*a.n/(2+r.e))*(sqrt(r.e+(2*r.e^2*(1+(t2/t1)+(t2/t1)^2)+r.e^3*(t2/t1)^2))-r.e*(1+(t2/t1)))
	R.4<-1.05*(fh*t1*a.n/(2+r.e))*(sqrt(2*r.e*(1+r.e)+((4.5*r.e*(2+r.e)*My)/(fh*a.n*t1^2)))-r.e)
	R.5<-1.05*(fh*t2*a.n/(1+2*r.e))*(sqrt(2*r.e^2*(1+r.e)+((4.5*r.e*(1+2*r.e)*My)/(fh*a.n*t2^2)))-r.e)
	R.6<-1.15*(sqrt((2*r.e)/(1+r.e))*sqrt(2*My*fh*a.n))
	#return(c(R.1,R.2,R.3,R.4,R.5,R.6))
	return(min(c(R.1,R.2,R.3,R.4,R.5,R.6)))
}

#Eq.27: active earth pressure under variably saturated conditions 

#Ea.f<-function(B,FrA,Sk,ii,Uw,H){
#Ka<-((1/sin(B)*sin(B-FrA)))/(sqrt(B+Sk)+sqrt((sin(FrA+Sk)*sin(FrA-ii))/(sin(B-ii))))
#Ea<-(1/2)*Ka*Uw*H^2
#return(Ea)
#}

#[updated function]
Ea.f.2<-function(FrA,H,Uw,Uw.s,rho,gr,wt.h,ln,dm,nr,dns,WV,sch.cw,sl.wd){
	Ka<-(1-sin(FrA))/(1+sin(FrA))
	EUws<-Uw.s-Uw #kN/m3
	oV<-EUws*H #kN/m2
	oh<-Ka*oV #kN/m2
	Pa<-(1/2)*oh*H #kN/m
	Hy.p<-(rho*gr*wt.h/1000)*sl.wd #kN/m
	vol.t<-(pi*dm/4)*ln*nr
	mass.t<-vol.t*dns/1000
	load.t<-mass.t*gr #kN
	sch.lg<-(load.t/sl.wd) #kN/m
	Ea.t<-Pa+Hy.p+sch.lg+WV+sch.cw #kN/m
	return(Ea.t)
}

#FrA: angle of internal friction
#oV<-vertical effetive stress (kPa; kN/m2)
#EUws: effective unit weight of the backfill soil (kN/m3)
#EUws=Uw.sat-Uw; saturated unit weight - unit weight of water (9.8 kN/m3)
#Uw.sat=(G+e)*Uw/(1+e); G: specific gravity of soil solids; e: void ratio; Uw: unit weight of water
#H: height of cribwall (m)
#rho:density of water
#gr:gravity acceleration
#wt.h: height of water table

#ln: log length
#dm: log diameter
#nr: number of logs
#dns: mass density of timber 
#gr: gravitational acceleration
#ar: area
#WV: vegetation surcharge calculated with Eq.94 - cribwall_rev.R.
#sch.cw: cribwall surcharge
#sl.wd: slide width (m)

#Eq.28: overturning resistance due to lateral earth pressure  (Ea.r.ov; kN/m)

Ea.r.ov.f<-function(Ea,B,Sk,H,CB.inc,W){
	d1<-(B*CB.inc/2)+cos(CB.inc)*(H/2)*tan(CB.inc)
	d3<-sin(Sk+B-90*(pi/180))*(H/3)*tan(90*(pi/180)-B)
	Ea.r.ov<-W*d1+Ea*d3
	return(Ea.r.ov)
} 

#Eq.29: overturning force due to lateral earth pressire 

Ea.ov.f<-function(Ea,sK,B,H,Lcb,CB.inc){
	d2<-cos(sK+B-90*(pi/180))*((H/3)*Lcb*sin(CB.inc))
	Ea.ov<-Ea*d2
	return(Ea.ov)
}

#Eq.30: sliding resistance due to lateral earth pressure (Ea.r.s; kN/m)

Ea.r.s.f<-function(Ea.sat,sK,B,W,c,Lcb,Sp){
	Ea.r.s<-tan(sK)*(Ea.sat*sin(sK+B-90*(pi/180))+W-Sp)+(c*Lcb)
	return(Ea.r.s)
}

#Eq.31: sliding due to lateral earth pressure (Ea.s; kN/m)

Ea.s.f<-function(Ea.sat,sK,B){
	Ea.s<-Ea.sat*cos(sK+B-90*(pi/180))
	return(Ea.s)
}

#Eq.32: generic factor of safety 

FoS.f<-function(Fr,Fgr,Ff,Fs){
	FoS<-(Fr+Fgr+Ff)/Fs
	return(FoS)
	}



############################################
#C-Soil geotechnical parameters
################################

#Eq.33: Moist unit weight of soil (kN/m3)

Uw.w.f<-function(SM,Gs,Ww,e){
	Uw.w<-((1+SM)*Gs*Ww)/(1+e)
	return(Uw.w)
}

#Eq.34: weight of soil column (kN/m)
#modified on 28/05/2020 to include surcharge!

Ws.f<-function(Uw,L,Hss,alpha,SRCH){
	Ws<-(Uw*Hss+SRCH)*L*cos(alpha)
	return(Ws)
}

#########################################
#D-Soil hydro-mechanical parameters
#####################################

#Eq.35: MATRIC SUCTION OF THE WETTING FRONT (Sf); SWAT-model (in m)

Sf.f<-function(Sa,Cl,n.s){
Sf<-10*exp(6.5309-7.32561*n.s+0.001583*Cl^2+3.809479*n.s+0.0000344*Sa*Cl-0.049837*Sa*n.s-0.0000136*Sa^2*Cl-0.003479*Cl^2*n.s-0.000799*Sa^2*Cl)
	return(Sf)
}

#Eq. 36: field capacity

#FC.f<-function(Sa,Cl,OM){
#	o33<-(-0.251*Sa)+0.195*Cl+0.011*OM+0.006*(Sa*OM)-0.027*(Cl*OM)+0.452*(Sa*Cl)+0.299
#	FC<-o33+(1.238*(o33)^2-0.374*(o33)-0.015)
#	return(FC)
#}

FC.f<-function(OM,Cl,Si){
	FC<-0.2449-0.1887*(1/(OM+1))+0.004527*Cl+0.001535*Si+0.001442*Si*(1/(OM+1))-0.00005110*Si*Cl+0.0008676*Cl*(1/(OM+1))
	return(FC)
}

#Eq.37: wilting point

#WP.f<-function(Sa,Cl,OM){
#	o1500<-(-0.024*Sa)+0.487*Cl+0.006*OM+0.005*(Sa*OM)-0.013*(Cl*OM)+0.068*(Sa*Cl)+0.031
#	WP<-o1500+(0.14*o1500-0.02)
#	return(WP)
#}

WP.f<-function(OM,Cl,Si){
	WP<-0.09878+0.002127*Cl-0.0008366*Si-0.07670*(1/(OM+1))+0.00003853*Si*Cl+0.002330*Cl*(1/(OM+1))+0.0009498*Si*(1/(OM+1))
	return(WP)
}


#Eq.38: saturated hydraulic conductivity - Ks (m/h)

Ks.f<-function(OM,Cl,Sa,SMs){
	o33<-(-0.251*Sa)+0.195*Cl+0.011*OM+0.006*(Sa*OM)-0.027*(Cl*OM)+0.452*(Sa*Cl)+0.299
	o1500<-(-0.024*Sa)+0.487*Cl+0.006*OM+0.005*(Sa*OM)-0.013*(Cl*OM)+0.068*(Sa*Cl)+0.031
	B<-(log(1500)-log(33))/(log(o33)-log(o1500))
	L<-1/B
	Ks<-(1930*(SMs-o33)^(3-L))/1000
	return(Ks) #m/h
}

#Eq.39: hydraulic conductivity function


K_Oi.f<-function(Ks,SMi,SMs,n.van){ #Ks in m/s
	K_Oi<-Ks*(SMi/SMs)^n.van
	return(K_Oi)
}

########################################
#E-cribwall members - load and decay
#####################################

#Eq.40: volume of timber in the cribwall per unit length of cribwall (m3)

Vt.cb.f<-function(Dm,T.Lcb){
	Vt.cb<-((pi*Dm^2)/4)*T.Lcb
	return(Vt.cb)
}

#Eq.41: total cribwall volume (m3)

Vcb.f<-function(Hcb,Lcb,Wd.cb){
	Vcb<-Hcb*Lcb*Wd.cb
	return(Vcb)
}

#Eq.42: self-weight of cribwall and filling soil (kN/m)

Wcb.f<-function(Vt.cb,Uw.t,Vs,Uw.s,Vcb,Hcb,Lcb,aa){
	Uw.cb<-(Vt.cb*Uw.t+Vs*Uw.s)/Vcb
	Wcb<-Uw.cb*Hcb*Lcb*cos(aa)
	return(Wcb)
}

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

##############################################
#F-HYdrological model
#######################

#Eq.46: suction stress

SS.f<-function(MS,a,n.van){
	A<-(1+sign(MS)*(a*abs(MS))^n.van)
	B<-(abs(A)^((n.van-1)/n.van))*sign(A)
	C<-(-1)*((MS)/B)
	return(C)	
}

#Eq.47: matric suction under wetting conditions

MS.w.f<-function(a,q,Ks,Ww,z){ 
	MS<-((-1)*1/a)*log(abs((1+q/Ks)*exp((-1)*Ww*a*z)-(q/Ks)))
	return(MS)
}

#Eq.48: matric suction under drying conditions

#MS.d.f<-function(a,Etp,K_Oi,Ww,z,Ac){ 
#	MS<-(1/a)*(log(abs((1+((Etp*Ac)/K_Oi))*exp((-1)*Ww*a*z)-(Etp*Ac/K_Oi))))
#	return(abs(MS))
#}


MS.d.f<-function(a,Etp,K_Oi,Ww,z,Ac,LAI){ 
	MS<-(1/a)*(log(((1-((Etp*Ac*LAI)/K_Oi))*exp((-1)*Ww*a*z)+((Etp*Ac*LAI)/K_Oi))))
	return(abs(MS))
}

#MS.d.f<-function(a,Etp,K_Oi,Ww,z,Ac,LAI){ 
#	MS<-((1/a)*(log(((1-((Etp)/K_Oi))*exp((-1)*Ww*a*z)+((Etp)/K_Oi)))))*Ac*LAI
#	return(abs(MS))
#}



#Eq.49: Cummulative infiltration at ponding (m)

Fp.f<-function(Sf,Ks,SMs,SMi,P){
	Fp<-(Sf*Ks*(SMs-SMi))/(P-Ks)
	return(Fp)
}

#Eq.50: Depth of the wetting front  at ponding or at 'anytime'- [this may have been simplified to reduce computational effort] (m)

Zwf.f<-function(Fp,SMs,SMi){
	Zwf<-Fp/(SMs-SMi)
	return(Zwf)
}

#Eq.51: runoff (m)

RNF.f<-function(ER,Fp,Ks,tr){
	RNF<-ER-Fp-Ks*tr
	return(RNF)
}

#RNF.f<-function(ER,Ks,tr){
#	RNF<-ER-Ks*tr
#	return(RNF)
#}

#Eq.52: actual infiltration (mm) - after ponding

#AI.f<-function(ER,Fp,RNF,slope){
#	AI<-ER*cos(slope)-Fp-RNF
#	return(AI)
#}

#AI.f<-function(ER,Fp,RNF){
#	AI<-ER-Fp-RNF
#	return(AI)
#}

AI.f<-function(ER,RNF,slope){
	AI<-ER*cos(slope)-RNF
	return(AI)
}

#Eq.53: Volume of saturated soil (with the wetting front)(m3)

Vsat.f<-function(Zwf,As){
	Vsat<-Zwf*As
	return(Vsat)
} 

#Eq.54: Volume of water in that portion of soil (m3)

Vw.f<-function(Vsat,SMs){
	Vw.sat<-Vsat*SMs
	return(Vw.sat)
}

#Eq.55: Volume of water at FIELD CAPACITY (m3)

wFC.f<-function(FC,Vsat){
	wFC<-FC*Vsat
	return(wFC)
}

#Eq.56: Excess volume of water (i.e. percolation volume; liters/m2 of soil==mm)

SWex.f<-function(Vw,wFC){
	SWex<-(Vw-wFC)*1000
	return(SWex)
}  

#Eq.57: Travel time for percolation (hours)
 #(i.e. time in which the excess water will drain from the wetting front); 
#note that divides by 1000 to transform from mm to m, and Ks in m/h

TTperc.f<-function(SWex,Ks){ #hours
	TTperc<-(SWex/1000)/Ks
	return(TTperc)
}

#Eq.58: Percolation rate (m/h)

q_perc.f<-function(SWex,t.step,TTperc){
	q_perc<-(SWex/1000)*(1-exp((-1)*t.step/TTperc))
	return(q_perc)
}

#Eq.59: Unsaturated soil profile volume (m3)

V.unsat.f<-function(sDP,Zwf,As){
	V.unsat<-(sDP-Zwf)*As
	return(V.unsat)
}

#Eq.60: Initial volume of water in the unsaturated zone (liters)

VwUi.f<-function(SMi,V.unsat){
	VwUi<-1000*(SMi*V.unsat)
	return(VwUi)
}

#Eq.61: Final water volume after complete percolation (liters)

VwUf.f<-function(VwUi,SWex,As){
	VwUf<-VwUi+(SWex*As)
	return(VwUf)
}

#Eq.62: Final moisture content after complete percolation (/1)

SMf.f<-function(VwUf,V.unsat){
	SMf<-(VwUf/1000)/V.unsat
	return(SMf)
}

#Eq.63: Travel distance of the percolating front in the unsatruated soil (m)

z.perc.f<-function(K_Of,TTperc){
	z.perc<-K_Of*TTperc
	return(z.perc)
}

#Eq.64: Depth of the percolation front (m)
Zpf.f<-function(Zwf,z.perc){
	Zpf<-Zwf+z.perc
	return(Zpf)
}

###############################################################
#G- Evapotranspiration model
##############################

#Eq.65: POTENTIAL EVAPOTRANSPIRATION; Priestly-Taylor (1972) 

Eu.f<-function(Rnl,A,PSH){
	Eu<-0.00128*(Rnl/58.3)*(A/(A+PSH))
	return(Eu)
}

#Eq.66: Eu transformation to m/m2 d

Eu_m.f<-function(Eu,T){
	Eu_m<-Eu/(2.501-2.361*10^-3*T)
	return(Eu_m)
}


#Eq.67: PSHYCHROMETRIC CONSTANT (KPa/ºC)

PSH.f<-function(AP){
	PSH<-0.665*10^-3*AP
	return(PSH)
}

#Eq.68: SLOPE OF SATURATION VAPOUR PRESSURE CURVE (KPa/ºC)

A.f<-function(Tk){
	A<-(5304/Tk^2)*exp(21.5-(5304/Tk))
	return(A)
}

#Eq.69: NET SOLAR RADIATION (MJ/m2.day); x 0.408==mm/day

Rnl.f<-function(a,Rs){
	Rnl<-(1-a)*Rs
	return(Rnl)
}

#Eq.70: INCOMING SOLAR RADIATION (Rs) (MJ/m2.day)

Rs.f<-function(as,bs,n,N,Ra){
	Rs<-(as+bs*(n/N))*Ra
	return(Rs)
}

#Eq.71: DAILY ESTIMATION OF THE EXTRATERRESTIAL RADIATION (Ra) (MJ/m2.day)

Ra.f<-function(π,Gsc,dr,ws,LAT,DEC){
	Ra<-((24*60)/π)*Gsc*dr*(ws*sin(LAT)*sin(DEC)+cos(LAT)*cos(DEC)*sin(ws))
	return(Ra)
}

#Eq.72: Inverse relative distance Earth-Sun

dr.f<-function(π,J){
	dr<-1+0.033*cos((2*π/365)*J)
	return(dr)
}
#Eq.73: Solar declination (rad)

DEC.f<-function(π,J){
	DEC<-0.409*sin((2*π/365)*J-1.39)
	return(DEC)
}
#Eq. 74: Sunset hour angle (rad)

ws.f<-function(LAT,DEC){
	ws<-acos(-tan(LAT)*tan(DEC))
	return(ws)
}

#Eq.75: Maximum duration of sunsine (hours)

N.f<-function(π,ws){
	N<-(24/π)*ws
	return(N)
}

#Eq.76: Albedo

a.f<-function(Cf,S.al){
	a<-0.23*(1-Cf)+S.al*Cf
	return(a)
}

#Eq.77: Coefficient for estimating species-specific albedo

Cf.f<-function(Ma){
	Cf<-exp(-0.000029*Ma)
	return(Cf)
}


#Eq.78: POTENTIAL SOIL EVAPORATION (under canopy) (mm/d)

Esp.f<-function(Eu,LAI,As){
	Esp<-(Eu*exp((-1)*0.4*LAI))*As
	return(Esp)
}

#Eq. 79: POTENTIAL PLANT TRANSPIRATION (mm/d)

Etp.f<-function(Esp,Eu,As){
	Etp<-((1-(Esp/Eu*As))*Eu)*As
	return(Etp)
}


#Eq.80: potential depth soil evaporation (m)

dx.f<-function(Cl,Sa){
	dx<-0.09-0.00077*Cl*100+0.000006*(Sa*100)^2
	return(dx)
}

###############################################
#H-Vegetation model
#######################

#Eq.81: tree biomass [choose species]

#Acer pseudoplatanus
ACER.Ma<-function(DBH){
	ABW=exp(-2.7018+2.5751*log(DBH))
	return(ABW)
}
#Fraxinus excelsior
FX.Ma<-function(DBH){
	ABW=exp(-2.4718+2.5466*log(DBH))
	return(ABW)
}

#Salix sp
SX.Ma<-function(DBH){ 
	BR<-exp(2.4721+2.4987*log(DBH))
	FL<-exp(1.4718+2.3117*log(DBH))
	ST<-exp(4.5086+1.9234*log(DBH)+0.2613*(log(DBH))^2)
	TB<-BR+FL+ST 
	return(TB)
}

#Fagus sylvatica
FAGUS.Ma<-function(DBH){
	AB=0.0798*DBH^2.601
	return(AB)
}

#Quercus sp [NEEDS TO CHANGE TO OAK]
OAK.Ma<-function(DBH){
	AB=exp(-2.4232+2.4682*log(DBH)) 
	return(AB)
}

#Betula sp
BETULA.Ma<-function(DBH){
 	AB=0.00029*(DBH*10)^2.50038
 	return(AB)
}

#################################
#H.2 - interception & stemflow
##################################

#Eq.82: gross rainfall (mm)

Pg.f<-function(P,tr){
	Pg<-P*tr*1000
	return(Pg)
}

#Eq.83: canopy cover fraction

cc.f<-function(kc,LAI){
	cc<-1-exp((-1)*kc*LAI)
	return(cc)
}

#Eq.84: Effective rainfall (mm)

ER.f<-function(Pg,S,cc,Ac.a){
	ER<-Pg-(S/cc)*Ac.a
	return(ER)
}

#Eq.85: Stemflow (Ps)

Ps.f<-function(Pg,st.i,st.s,Ac.b){
	Ps.a<-st.i+st.s*Pg
	St.v<-Ps.a*Ac.b
	return(St.v)
}

#####################################
#H.3 - plant-soil reinforcement
####################################

#Eq.86: mean rooting depth temeperate humid climates (mm)

b.w.f<-function(AlphaC,n.s,FC,WP){
	b<-AlphaC/(n.s*(FC-WP)*(1))
	return(b)
}

#Eq.87: Mean rooting depth dry cliamtes (mm)

b.d.f<-function(AlphaC,n.s,FC,WP,Yo,ETP){
	b.D<-AlphaC/(n*(FC-WP)*(1-((Yo*AlphaC)/ETP)))
	return(b.D)
}

#Eq.88: Root cross-sectional area at ground surface (cm2)

Aro.f<-function(Ma,Beta.A,Alpha.A,b,R.dens){
	Aro<-((Ma/Beta.A)^(1/Alpha.A))/((b/10)*R.dens)
	return(Aro)
}

#Eq.89: Root cross-sectional area with soil depth (cm2)

Arz.f<-function(Aro,b,z){
	Arz<-Aro*exp((-1)*z/b)
	return(Arz)
}

#Eq.90 Root area ratio

RARz.f<-function(Arz,As){
	RARz<-Arz/As
	return(RARz)
}

#Eq.91: root added cohesion

cR.f<-function(Tr,RARz){
	cR<-0.4*1.2*Tr*RARz
	return(cR)
}

#Eq.92: Arc length with crossing roots [NOVEL]
#it corrects automatically, as if RAR is zero, lR1.f will be zero

lR1.f<-function(Ln,RARz){
	lR1<-Ln*RARz
	return(lR1)
}

#Eq.93: Pull-out strength for roots > 3mm

bR.f<-function(z,Uw,FrA,f){
	bR<-z*Uw*(1-sin(FrA))*f*tan(FrA) #it is interesting the term (1 - sin(FrA)) which does not feature in the members...
	return(bR)
}

#lR2 has to be estimated as for a geo-grid (see slip geometry.R)



#Eq.94: vegetation surcharge

WV.f<-function(b,Aro,R.dens,Ac.a,g,Ma){
	Vr<-b*Aro #root volume in cm3
	Mr<-Vr*R.dens #root mass in g
	MV<-(Mr+Ma)/1000 #vegetation mass (kg)
	WV<-((MV/Ac.a)*g)/1000  #surchage in kPa
	return(WV)
}

