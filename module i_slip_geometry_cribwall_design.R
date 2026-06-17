#Module i: slip geometry & crib wall design
##############################
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model")

#Contents
#########
#1) load functions
#2) establish slope and slip geometry (no cribwall yet)
#3) set up cribwall
#4) set up filling material and geometry
#5) intersection between slip arcs and cribwall members
#6) memeber reinforcement within each slice
#7) Depth to Member - to calculate pullout or breakage reinforcement
#8) Distance from slip arc to cribwall stretcher [for face-wall contribution calcs.]
#9) Cribwall's face-wall contribution
#10) sketch and plot


########################################
#1 - load functions
#########################################
source("live_crib_wall_model_functions.R") 


###############################################
#2 - establish slope and slip geometry (no cribwall yet)
###############################################

#2.1) load slope profile
slope.xy<-data.frame(x=c(0,2,4,6,8,10,12,14,16,18,20,22,24),y=c(3,4,5,7,8,10,11,13,14,15,16,16,16)) #units=meters
plot(y~x,data=slope.xy,type="b",xlab="x (m)",ylab="y (m)",main="Cribwall Sketch",cex.lab=1.5,cex.axis=1.5,cex.main=1.3,ylim=c(0,20))

#2.2) establish slices of 2 m

sl.x<-c(0,2,4,6,8,10,12,14,16,18,20,22,24)

#2.3) calculate intersection points of slice boundaries with surface topography

	#2.3.1 - establish linear equations between known surface points
ln.pars<-matrix(unlist(lapply(1:nrow(slope.xy),function(i) line.f(xt.1=slope.xy$x[i+1],xt.2=slope.xy$x[i],yt.1=slope.xy$y[i+1],yt.2=slope.xy$y[i]))),ncol=2,byrow=TRUE)

	#2.3.2 - calculate intersection points of slices with slope topography

			#2.3.2.1 - re-arrange line parameters and include x-point (right hand side boundary) for knowm points
ln.pars<-na.omit(ln.pars)
ln.pars.df<-data.frame(a.l=ln.pars[,1],b.l=ln.pars[,2],x.pt=c(2,4,6,8,10,12,14,16,18,20,22,24))
			#2.3.2.2 - establish topographic x-point for each slice 
sl.x.df<-data.frame(x.sl=sl.x,x.pt=c(0,2,4,6,8,10,12,14,16,18,20,22,24))
			#2.3.2.3 - merge slices and line parameters, so each slice boundary has line parameters associated to it
sl.ln.pars<-merge(sl.x.df,ln.pars.df,by="x.pt")
			#2.3.2.4 - calculate y-point on the ground surface
z.gr.slices<-Z.ground.f(a.l=sl.ln.pars[,3],b.l=sl.ln.pars[,4],x.m.sl=sl.ln.pars[,2])
			#2.3.2.5 - compose slices data frame
sl.df<-data.frame(x=sl.ln.pars[,1],y=z.gr.slices)
	#2.3.3 - draw slice segments on slope profile
segments(sl.df[1,1],0,sl.df[1,1],sl.df[1,2],col="red",lty=3) #slice 1
segments(sl.df[2,1],0,sl.df[2,1],sl.df[2,2],col="red",lty=3) #slice 2
segments(sl.df[3,1],0,sl.df[3,1],sl.df[3,2],col="red",lty=3) #slice 3
segments(sl.df[4,1],0,sl.df[4,1],sl.df[4,2],col="red",lty=3) #slice 4
segments(sl.df[5,1],0,sl.df[5,1],sl.df[5,2],col="red",lty=3) #slice 5
segments(sl.df[6,1],0,sl.df[6,1],sl.df[6,2],col="red",lty=3) #slice 6
segments(sl.df[7,1],0,sl.df[7,1],sl.df[7,2],col="red",lty=3) #slice 7
segments(sl.df[8,1],0,sl.df[8,1],sl.df[8,2],col="red",lty=3) #slice 8
segments(sl.df[9,1],0,sl.df[9,1],sl.df[9,2],col="red",lty=3) #slice 9
segments(sl.df[10,1],0,sl.df[10,1],sl.df[10,2],col="red",lty=3) #slice 10
segments(sl.df[11,1],0,sl.df[11,1],sl.df[11,2],col="red",lty=3)
segments(sl.df[12,1],0,sl.df[12,1],sl.df[11,2],col="red",lty=3)

#2.4) establish circumferences [for this run: two different centres - ten sizes]
         #2.4.1 - set circumferences origin and radii
cm.o<-data.frame(a.o=8,b.o=20)

cm.r<-c(10,11,12,13,16)


#2.5) - calculate arcs parameters

		#2.5.1 - calculate intersection points between circumference and slice boundaries [one centre at a time]
y.sl.ct3<-lapply(1:length(cm.r),function(i) sapply(1:nrow(sl.df),function(j)cm.f(r.c=cm.r[i],x=sl.df[j,1],b.o=cm.o[1,2],a.o=cm.o[1,1])))
		#check points for one radius on the plot
points(y.sl.ct3[[1]]~sl.df[,1],col="purple",pch=4)  #radius 1
points(y.sl.ct3[[2]]~sl.df[,1],col="blue",pch=7)
points(y.sl.ct3[[3]]~sl.df[,1],col="green",pch=2)
points(y.sl.ct3[[4]]~sl.df[,1],col="orange",pch=2)
points(y.sl.ct3[[5]]~sl.df[,1],col="red",pch=2)  #

#//////////////////////////////////////////////////////////////////////////////////////		
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~		
#correction for poitns outside the slope topographic envelope ~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\		

#0-establish data.frame with XYZ-intersection points between slip arc and slice boundaries & Z.ground

XYZ.sl.ct3<-lapply(1:length(cm.r),function(i) data.frame(cbind(x=sl.df[,1],y=y.sl.ct3[[i]],z=z.gr.slices)))



#A-calculate intersection between slip arc and topography

x1.tp.ct3<-lapply(1:length(cm.r),function(i) x.int.1.f(a=cm.o[1],b=cm.o[2],m=1,d=0,r=cm.r[i]))
		
y1.tp.ct3<-lapply(1:length(cm.r),function(i) y.int.1.f(a=cm.o[1],b=cm.o[2],m=1,d=0,r=cm.r[i]))		
	
XY.int.ct3<-data.frame(cbind(x=unlist(x1.tp.ct3),y=unlist(y1.tp.ct3)))	
		
#B-replace with newly calculated intersection points when y.topo (i.e. z.gr.slices) > y.intersection and
#x.intersection < x.slice

#Y-replacement		
Y.sl.ct3.bis<-lapply(1:length(cm.r),function(i) sapply(1:nrow(sl.df),function(j) ifelse(XYZ.sl.ct3[[i]]$y[j]>XYZ.sl.ct3[[i]]$z[j] && XYZ.sl.ct3[[i]]$y[j+1]>=XYZ.sl.ct3[[i]]$z[j+1],NA,ifelse(XY.int.ct3$x[i]>XYZ.sl.ct3[[i]]$x[j] && XY.int.ct3$y[i]<XYZ.sl.ct3[[i]]$y[j],XY.int.ct3$y[i],XYZ.sl.ct3[[i]]$y[j]))))	
		
#X-replacement (in 4 steps)
X.sl.ct3.A<-lapply(1:length(cm.r),function(i) sl.df$x)
xxx<-lapply(1:length(cm.r),function(i) rep(XY.int.ct3$x[i],nrow(sl.df)))
X.sl.ct3.B<-lapply(1:length(cm.r),function(i) sapply(1:nrow(sl.df),function(j) ifelse(X.sl.ct3.A[[i]][j]<xxx[[i]][j],xxx[[i]][j],X.sl.ct3.A[[i]][j])))
X.sl.ct3.C<-lapply(1:length(cm.r),function(i) sapply(1:nrow(sl.df),function(j) ifelse(is.na(Y.sl.ct3.bis[[i]][j]),NA,X.sl.ct3.B[[i]][j])))

#these two objects must be updated in the subsequent steps
	
		#2.5.2 - calculate arc.angles [two steps]

			#2.5.2.1 - length of string between two interscetion points

dt.sl.ct3<-lapply(1:length(cm.r),function(j) sapply(2:nrow(sl.df),function(i) dt.f(x.1=X.sl.ct3.C[[j]][i],y.1=Y.sl.ct3.bis[[j]][i],x.2=X.sl.ct3.C[[j]][i-1],y.2=Y.sl.ct3.bis[[j]][i-1])))

			#2.5.2.2 - arc angle [in deg]

arc.angl.ct3<-lapply(1:length(cm.r),function(j) sapply(1:(nrow(sl.df)),function(i)arc.angle.f(a=cm.r[j],b=cm.r[j],c.x=dt.sl.ct3[[j]][i])))

		#2.5.3 - calculate arc lengths [this will be Ln in stability analysis]

arc.L.ct3<-lapply(1:length(cm.r),function(j) data.frame(L.arc=matrix(sapply(1:(nrow(sl.df)),function(i)arc.L.f(X.2=arc.angl.ct3[[j]][i],r=cm.r[j])),ncol=1,nrow=nrow(sl.df),byrow=TRUE)))

		#2.5.4 - calculate "alpha" - i.e. slope of slice or soil wedge
	
#alpha.sl.ct3<-vector("list",length(cm.r))
#for(i in 2:(nrow(sl.df)-1)){
#	for(j in 1:length(cm.r)){
#	alpha.sl.ct3[[j]][i]<-alpha.f(sl.b=0.5,arc.L=arc.L.ct3[[j]][i],x=sl.df$x[i-1],a.o=cm.o[1,1])	
#	}
	
#}

#slice.b will change in some instances
sl.b.ct3<-lapply(1:length(cm.r),function(i) sapply(1:nrow(sl.df),function(j) X.sl.ct3.C[[i]][j+1]-X.sl.ct3.C[[i]][j]))

alpha.sl.ct3<-lapply(1:length(cm.r),function(j) data.frame(alpha=matrix(sapply(1:(nrow(sl.df)),function(i)alpha.f(sl.b=sl.b.ct3[[j]][i],arc.L=arc.L.ct3[[j]][i,],x=X.sl.ct3.B[[j]][[i]],a.o=cm.o[1,1])),ncol=1,nrow=nrow(sl.df),byrow=TRUE)))

		#2.5.5 - calculate the XY-point of the arcs' centres so the depth of the soil wedges can be estimated
	

x.c.arc.ct3<-lapply(1:length(cm.r),function(j) data.frame(x.arc=matrix(sapply(1:(nrow(sl.df)),function(i)x.c.arc.f(x.1=X.sl.ct3.C[[j]][i+1],x.2=X.sl.ct3.C[[j]][i],y.1=Y.sl.ct3.bis[[j]][i+1],y.2=Y.sl.ct3.bis[[j]][i],X=(arc.angl.ct3[[j]][i]*pi)/180,r.c=cm.r[j])),ncol=1,nrow=nrow(sl.df),byrow=TRUE)))


y.c.arc.ct3<-lapply(1:length(cm.r),function(j) data.frame(y.arc=matrix(sapply(1:(nrow(sl.df)),function(i)y.c.arc.f(x.1=X.sl.ct3.C[[j]][i+1],x.2=X.sl.ct3.C[[j]][i],y.1=Y.sl.ct3.bis[[j]][i+1],y.2=Y.sl.ct3.bis[[j]][i],X=(arc.angl.ct3[[j]][i]*pi)/180,r.c=cm.r[j])),ncol=1,nrow=nrow(sl.df),byrow=TRUE)))



#LIVE DATA FRAME

slice<-seq(from=1,to=nrow(sl.df),length=nrow(sl.df))

	ct3.df<-lapply(1:length(cm.r),function(i)cbind(slice,x.c.arc.ct3[[i]],y.c.arc.ct3[[i]],arc.L.ct3[[i]],alpha.sl.ct3[[i]]))

#2.6) calculate the "vertical" depth span of each soil wedge

	#2.6.1 - calculate y-point at the ground surface for each x.arc
			
			#2.6.1.1 - set up line parameters for calculation
#it seems like i can use the x.pt established in "sl.ln.pars" - minus the first point, which is for x=0
#sl.ln.pars<-sl.ln.pars[-1,]

y.gr.ct3<-lapply(1:length(cm.r),function(i) data.frame(y.point=matrix(sapply(1:nrow(sl.ln.pars),function(j) Z.ground.f(a.l=sl.ln.pars[j,3],b.l=sl.ln.pars[j,4],x.m.sl=ct3.df[[i]][j,2])),ncol=1,byrow=TRUE)))

	#2.6.2. calculate z-depth for each soil wedge -i.e. difference between y.gr and y.arc

z.gr.ct3<-lapply(1:length(cm.r),function(i) data.frame(z.point=matrix(sapply(1:length(slice),function(j) Z.slip.f(z.g=y.gr.ct3[[i]][j,],y.arc=ct3.df[[i]][j,3])),ncol=1,byrow=TRUE)))

			#2.6.2.1 - corrections of points above the slope topography envelope i.e.negative Z
z.gr.ct3<-lapply(1:length(cm.r),function(i) data.frame(z.point=matrix(ifelse(z.gr.ct3[[i]]$z.point < 0,0,z.gr.ct3[[i]]$z.point),ncol=1,byrow=TRUE)))

#UPDATE DATA FRAME

ct3.o<-lapply(1:length(cm.r),function(i) cbind(ct3.df[[i]],y.gr.ct3[[i]],z.gr.ct3[[i]]))

#setwd("/Users/ollauri/Desktop/work/OPERANDUM/mini_projects/NBS_modelling/data/CC_runs")

setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs")
save(ct3.o,file="base_ct3_3r.RData")

#############################################################################################
#3-set up cribwall
##################
cb.members<-data.frame(x1=c(8,8,8,8),y1=c(8.4,8.9,9.4,9.9),x2=c(9.5,9.5,9.5,9.5),y2=c(8.4,8.9,9.4,9.9))
cb.stretchers<-data.frame(x1=c(8,8,8,8),y1=c(8.3,8.8,9.3,9.8),x2=c(9.5,9.5,9.5,9.5),y2=c(8.3,8.8,9.3,9.8))

segments(cb.members$x1[1],cb.members$y1[1],cb.members$x2[1],cb.members$y2[1],col="darkgreen",lwd=2)
segments(cb.members$x1[2],cb.members$y1[2],cb.members$x2[2],cb.members$y2[2],col="darkgreen",lwd=2)
segments(cb.members$x1[3],cb.members$y1[3],cb.members$x2[3],cb.members$y2[3],col="darkgreen",lwd=2)
segments(cb.members$x1[4],cb.members$y1[4],cb.members$x2[4],cb.members$y2[4],col="darkgreen",lwd=2)

points(cb.stretchers$x1[1],cb.stretchers$y1[1],cex=1,col="darkgreen")
points(cb.stretchers$x1[2],cb.stretchers$y1[2],cex=1,col="darkgreen")
points(cb.stretchers$x1[3],cb.stretchers$y1[3],cex=1,col="darkgreen")
points(cb.stretchers$x1[4],cb.stretchers$y1[4],cex=1,col="darkgreen")
points(cb.stretchers$x2[1],cb.stretchers$y2[1],cex=1,col="darkgreen")
points(cb.stretchers$x2[2],cb.stretchers$y2[2],cex=1,col="darkgreen")
points(cb.stretchers$x2[3],cb.stretchers$y2[3],cex=1,col="darkgreen")
points(cb.stretchers$x2[4],cb.stretchers$y2[4],cex=1,col="darkgreen")

#12/5/25 - adding slope grid 

segments(cb.stretchers$x1[4],cb.stretchers$y1[4],16,14,col="brown",lwd=3)


############################################################################################
#4- set up filling material and geometry
#########################################


#segments(0,1.5,4,3,col="red",lty=3)
cb.fill<-data.frame(x1=cb.stretchers$x1[4],y1=cb.stretchers$y1[4],x2=16,y2=14)

#4.2) filling slope < 0.5 (1:2) check

sl.fill<-(cb.fill$y2-cb.fill$y1)/(cb.fill$x2-cb.fill$x1) #OK - just (0.525)

#4.2) set up topographic-line equations for the filling -i.e. line parameters

lp.fill<-data.frame(a=cb.fill$y1,b=sl.fill)

#4.3) update topo line parameters & calculate surface points on new topo-profile for each slice boundary

	#4.3.1 - update slope.xy
	
		#4.3.1.1 - set up new slope topography -i.e. calculate y.point with the known topo line equation for the filling

y.fill<-lp.fill$a+lp.fill$b*slope.xy$x #from x=8 up to x=20

#slope.xy$y.fill<-y.fill

#y.fill.bis<-NULL
#for(i in 1:nrow(slope.xy)){
#	for(j in 1:length(y.fill)){
#		if(slope.xy$x[i] < 8){
		#y.fill.bis[i]<-slope.xy$y[i]
	#}else{
	#	y.fill.bis[i]<-y.fill[j]
	#}
#}
#}

y.fill.bis<-c(slope.xy$y[1:4],y.fill[1:5],slope.xy$y[10:13])
slope.xy$y.fill<-y.fill.bis


		#4.3.1.2 - check slope envelope and amend points not affected by fill

#slope.xy$y.fill[6]<-slope.xy$y[6]

	#4.3.2 - establish line equations between X-points 
	
#ln.pars.cb<-na.omit(matrix(unlist(lapply(1:nrow(slope.xy),function(i) line.f(xt.1=slope.xy$x[i+1],xt.2=slope.xy$x[i],yt.1=slope.xy$y.fill[i+1],yt.2=slope.xy$y.fill[i]))),ncol=2,byrow=TRUE))

#ln.pars.cb.df<-data.frame(b.l=ln.pars.cb[,1],a.l=ln.pars.cb[,2],x.pt=c(1,2,3,4,5))
#ln.pars.cb.df<-data.frame(b.l=ln.pars.cb[,1],a.l=ln.pars.cb[,2],x.pt=sl.df$x)
#################################################################################
#when points and slices did not match, implement the following:::
##########################################################################
	#4.3.3 - associate slices' X.points to known X.points ~~~[manual step]
#sl.x.df<-data.frame(x.sl=sl.x[-1],x.pt=c(1,1,2,2,3,3,4,4,5,5))

	#4.3.5 - merge slices and line parameters, so each boundary has line pars associates
#sl.ln.pars.cb<-merge(sl.x.df,ln.pars.cb.df,by="x.pt")

#sl.ln.pars.cb<-ln.pars.cb.df

	#4.3.6 - calculate y.point for the established slices in new topography
#y.point.sl.cb<-Z.ground.f(a.l=sl.ln.pars.cb[,2],b.l=sl.ln.pars.cb[,1],x.m.sl=sl.ln.pars.cb[,3])

	#4.3.7 - new slices data frame and draw
#sl.df.cb<-data.frame(x=sl.x[-1],y=y.point.sl.cb)

sl.df.cb<-slope.xy


segments(sl.df.cb$x[1],0,sl.df.cb$x[1],sl.df.cb$y.fill[1],col="blue",lty=3)
segments(sl.df.cb$x[2],0,sl.df.cb$x[2],sl.df.cb$y.fill[2],col="blue",lty=3)
segments(sl.df.cb$x[3],0,sl.df.cb$x[3],sl.df.cb$y.fill[3],col="blue",lty=3)
segments(sl.df.cb$x[4],0,sl.df.cb$x[4],sl.df.cb$y.fill[4],col="blue",lty=3)
segments(sl.df.cb$x[5],0,sl.df.cb$x[5],sl.df.cb$y.fill[5],col="blue",lty=3)
segments(sl.df.cb$x[6],0,sl.df.cb$x[6],sl.df.cb$y.fill[6],col="blue",lty=3)
segments(sl.df.cb$x[7],0,sl.df.cb$x[7],sl.df.cb$y.fill[7],col="blue",lty=3)
segments(sl.df.cb$x[8],0,sl.df.cb$x[8],sl.df.cb$y.fill[8],col="blue",lty=3)
segments(sl.df.cb$x[9],0,sl.df.cb$x[9],sl.df.cb$y.fill[9],col="blue",lty=3)
segments(sl.df.cb$x[10],0,sl.df.cb$x[10],sl.df.cb$y.fill[10],col="blue",lty=3)
segments(sl.df.cb$x[11],0,sl.df.cb$x[11],sl.df.cb$y.fill[11],col="blue",lty=3)
segments(sl.df.cb$x[12],0,sl.df.cb$x[12],sl.df.cb$y.fill[12],col="blue",lty=3)
segments(sl.df.cb$x[13],0,sl.df.cb$x[13],sl.df.cb$y.fill[13],col="blue",lty=3)

points(sl.df.cb[5,1],sl.df.cb[5,3],pch=5)
points(sl.df.cb[6,1],sl.df.cb[6,3],pch=5)
points(sl.df.cb[7,1],sl.df.cb[7,3],pch=5)


#4.4) depth of new soil wedges

	#4.4.1. y.point on new topography for each arc centre
#y.p.cb.ct3<-lapply(1:length(cm.r),function(i) data.frame(y.point.cb=matrix(sapply(1:length(slice),function(j)Z.ground.f(a.l=sl.ln.pars.cb[j,3],b.l=sl.ln.pars.cb[j,4],x.m.sl=ct3.o[[i]]$x.arc[j])),ncol=1,byrow=TRUE)))


sl.df.cb$y.mid<-sapply(1:nrow(sl.df.cb), function(i) (sl.df.cb$y.fill[i+1]+sl.df.cb$y.fill[i])/2)


	#4.2.1 calculate z.point
z.p.cb.ct3<-lapply(1:length(cm.r),function(i) data.frame(z.point.cb=matrix(sapply(1:length(slice),function(j)Z.slip.f(z.g=sl.df.cb[j,4],y.arc=ct3.o[[i]]$y.arc[j])),ncol=1,byrow=TRUE)))

	#4.2.2 correct points outside the slope topography
z.p.cb.ct3<-lapply(1:length(cm.r),function(i) data.frame(z.point.cb=matrix(sapply(1:length(slice),function(j)ifelse(z.p.cb.ct3[[i]][j,]<0,0,z.p.cb.ct3[[i]][j,])),ncol=1,byrow=TRUE)))

#UPDATE DATA FRAME

y.mid<-lapply(1:length(cm.r),function(i) data.frame(y.mid=sl.df.cb$y.mid[-13]))

ct3.cb<-lapply(1:length(cm.r),function(i)cbind(ct3.o[[i]],y.mid[[i]],z.p.cb.ct3[[i]]))
#############################################################################################
#5) intersection between slip arcs and cribwall members
########################################################


##############
#cb.st=where the cribwall starts
geo.grid.L.f.2<-function(y,b.o,a.o,r.c,cb.st){
	x.c<-sqrt(r.c^2-(y-b.o)^2)
	if(cb.st < a.o){
	x.f<-a.o-x.c}
	if(cb.st >= a.o){
	x.f<-a.o+x.c	
	}
	return(x.f)
}


#5.1 intersection point between slip arc and geogrid member


gg.L.ct3<-lapply(1:length(cm.r), function(i) lapply(1:nrow(cb.members),function(j) geo.grid.L.f.2(y=cb.members$y1[j],b.o=cm.o[1,2],a.o=cm.o[1,1],r=cm.r[i],cb.st=8)))

#note: a negative intersection point implies that the whole member is inside the slip surface

gg.L.ct3<-matrix(unlist(lapply(1:length(cm.r),function(i)ifelse(gg.L.ct3[[i]]<0,0,gg.L.ct3[[i]]))),ncol=4,nrow=length(cm.r),byrow=TRUE)
#note: these are intersection points, but the members are not infinitely long, they only span 1.5 m 

#5.2. reinforcement length of each member
#rf.L<-1.5-gg.L.ct3



gg.L.ct3[is.na(gg.L.ct3)]<-0
gg.L.ct3[2,3:4]<-1.5
gg.L.ct3[3,]<-1.5
gg.L.ct3[4,]<-0 
gg.L.ct3[5,]<-0

###########################################################
#6) memeber reinforcement within each slice
############################################
#when crib wall reinforces more than one slice:: 
##################################
#6.1 function - Number of reinforced slices (n.rsL) = 3 [see notes for 4 slices and so on]
#Le.sl3.f<-function(Le,sl.b,n.rsL,i){
#	if(Le[i] > (n.rsL-1)*sl.b){
#		Le.sl1<-Le[i]-(Le[i]-sl.b)
#		left1<-Le[i]-(Le[i]-(Le[i]-sl.b))
#		Le.sl2<-left1-(left1-sl.b)
#		Le.sl3<-left1-(Le.sl2)
#		}
#	if(Le[i] <= (n.rsL-1)*sl.b & Le[i] > 0){
#		Le.sl1<-Le[i]-(Le[i]-sl.b)
#		Le.sl2<-Le[i]-(Le[i]-(Le[i]-sl.b))
#		Le.sl3<-0
#	}
#	if(Le[i] <= sl.b & Le[i] > 0){
#		Le.sl1<-Le[i]
#		Le.sl2<-0
#		Le.sl3<-0
#	}
#	if(Le[i] == 0){
#		Le.sl1<-0
#		Le.sl2<-0
#		Le.sl3<-0
#	}
#	return(c(Le.sl1,Le.sl2,Le.sl3))	
#}
#}

#6.2 - missing values must be removed from rf.L matrix to make function work

#rf.L.mx.ct3<-ifelse(is.na(rf.L),0,rf.L) 

#6.3 - implement function
#Le.sl.ct3<-lapply(1:length(cm.r),function(k) rbind(matrix(unlist(lapply(1:nrow(cb.members),function(j) Le.sl3.f(Le=rf.L.mx.ct3[k,j],sl.b=0.5,n.rsL=3))),ncol=4,nrow=3,byrow=FALSE),matrix(0,nrow=7,ncol=4)))

#6.4 - arrange outcomes into data frame
#Le.ct3<-lapply(1:length(cm.r),function(i) data.frame(rfL.m1=Le.sl.ct3[[i]][,1],rfL.m2=Le.sl.ct3[[i]][,2],rfL.m3=Le.sl.ct3[[i]][,3],rfL.m4=Le.sl.ct3[[i]][,4]))

#6.5 - update data.frame

#ct3.cb<-lapply(1:length(cm.r),function(i)cbind(ct3.cb[[i]],Le.ct3[[i]]))


#a) creating an empty matrix
Le.mx<-lapply(1:length(cm.r), function(i) matrix(0,nrow=nrow(sl.df.cb)-1,ncol=nrow(cb.members)))
#b) (manual) filling the matrix 
Le.sl.df<-lapply(1:length(cm.r),function(i) data.frame(rfL.m1=Le.mx[[i]][,1],rfL.m2=Le.mx[[i]][,2],rfL.m3=Le.mx[[i]][,3],rfL.m4=Le.mx[[i]][,4]))
Le.sl.df[[2]][5,c(3:4)]<-1.5
Le.sl.df[[3]][5,]<-1.5
Le.sl.df[[4]][5,]<-1.5
Le.sl.df[[5]][5,]<-1.5

#c) merge with parent data frame
ct3.cb<-lapply(1:length(cm.r),function(i)cbind(ct3.cb[[i]],Le.sl.df[[i]]))

###################################################################################
#7- Depth to Member - to calculate pullout or breakage reinforcement
########################################################################
# y.mid replaces y.point.cb here; note that the reinforced slice is only N.sl=5
#the function calculates the vertical distance between the regraded slope and a given cribwall members
#this should only be done for slice 5 in this example, and then update
#the function as it was originally considered that the cribwall is built at the toe of the slope profile

#######################################################################
#7.1 - implement function
#d.t.m.ct3<-lapply(1:length(cm.r),function(i) rbind(matrix(unlist(lapply(1:length(cb.members),function(j) depth.to.member.f(z.gr.sl=ct3.cb[[i]]$y.point.cb[1:3],z.member=rev(cb.members$y1[j])))),ncol=4,nrow=3,byrow=FALSE),matrix(NA,ncol=4,nrow=7)))

d.t.m<-depth.to.member.f(z.gr.sl=ct3.cb[[1]]$y.mid[5],z.member=rev(cb.members$y1))


#7.2 - re-arrange outputs / manually :P
dtm.mx<-lapply(1:length(cm.r), function(i) matrix(NA,nrow=nrow(sl.df.cb)-1,ncol=nrow(cb.members)))
dtm.df<-lapply(1:length(cm.r),function(i) data.frame(dtm.m1=dtm.mx[[i]][,1],dtm.m2=dtm.mx[[i]][,2],dtm.m3=dtm.mx[[i]][,3],dtm.m4=dtm.mx[[i]][,4]))
dtm.df[[2]][5,]<-d.t.m
dtm.df[[3]][5,]<-d.t.m
dtm.df[[4]][5,]<-d.t.m
dtm.df[[5]][5,]<-d.t.m
#dtm.ct3<-lapply(1:length(cm.r),function(i) data.frame(dtm.m1=d.t.m.ct3[[i]][,1],dtm.m2=d.t.m.ct3[[i]][,2],dtm.m3=d.t.m.ct3[[i]][,3],dtm.m4=d.t.m.ct3[[i]][,4]))

#7.3 - update data frame
ct3.cb<-lapply(1:length(cm.r),function(i)cbind(ct3.cb[[i]],dtm.df[[i]]))

#######################################################################################
#8 - distance from slip arc to cribwall stretcher [for face-wall contribution calcs.]
#######################################################################################
#this is as above -only slice 5 is reinfoced within circles 2:5, so it is easier to just calculate for slice 5 and then 
#expand the matrix
#notice that in the function implemented below for cribwall operandum, calcs are performed for first 3 slices, results arranged in a matrix, and then the rows of the 



#8.1 - implement function
#dist.str.ct3<-lapply(1:length(cm.r),function(i) rbind(matrix(unlist(lapply(1:nrow(cb.stretchers), function(j) dist.str.f(y.str=cb.stretchers$y1[j],y.arc=ct3.cb[[i]]$y.arc[1:3]))),ncol=4,nrow=3,byrow=FALSE),matrix(NA,ncol=4,nrow=7)))

d.t.s<-lapply(2:length(cm.r),function(i) dist.str.f(y.arc=ct3.cb[[i]]$y.arc[5],y.str=cb.stretchers$y1))

#8.2 - re-arrange outputs
#dts.ct3<-lapply(1:length(cm.r),function(i) data.frame(dts1=dist.str.ct3[[i]][,1],dts2=dist.str.ct3[[i]][,2],dts3=dist.str.ct3[[i]][,3],dts4=dist.str.ct3[[i]][,4]))

dts.mx<-lapply(1:length(cm.r), function(i) matrix(NA,nrow=nrow(sl.df.cb)-1,ncol=nrow(cb.stretchers)))
dts.df<-lapply(1:length(cm.r),function(i) data.frame(dts1=dts.mx[[i]][,1],dts2=dts.mx[[i]][,2],dts3=dts.mx[[i]][,3],dts4=dts.mx[[i]][,4]))
dts.df[[2]][5,]<-d.t.s[[1]]
dts.df[[3]][5,]<-d.t.s[[2]]
dts.df[[4]][5,]<-d.t.s[[3]]
dts.df[[5]][5,]<-d.t.s[[4]]


#7.3 - update data frame
ct3.cb<-lapply(1:length(cm.r),function(i)cbind(ct3.cb[[i]],dts.df[[i]]))

########################################################################################
#9 - cribwall's face-wall contribution
#######################################

#9.1 - calculate EMBEDMENT STRENGTH (fh) - N/mm2


fh.cb<-fh.f(a.n=12,d.w=290)

#density (kg/m3) - coniferous C14: 290
#a.n is nail diameter in mm


#9.2 - calculate  PLASTIC MOMENT OF the nail (N.mm)


My.cb<-My.f(fa=600,a.n=12)

#f.a: torque resistance of metallic materials - steel - 600 N.mm

#9.3 - calculate FORCE STANDED BY THE TIMBER - TIMBER UNIONS provided by SINGLE SHEAR - Johansen equations
#(in N)


Rk.t.cb<-Rk.t.f(fh=fh.cb,t1=250,t2=125,a.n=12,r.e=1,My=My.cb) 


#9.4 - calculate the force provided by the cribwall frame considering the relative distance from the slip arc to a given stretcher in kN/m
#comment: function has been simplified - no 812 mm premise but it is then introduced through "ifelse"
#note that 1:3 in the second lapply term stands for the number of reinforced slices

#it has to be implemented stretcher by stretcher, so the function does not collapse


F.fw.ct3.str1<-lapply(1:length(cm.r),function(i) data.frame(F.fw.str1=rbind(matrix(0,ncol=1,nrow=4),matrix(unlist(lapply(1:1,function(j) ifelse(ct3.cb[[i]]$dts1[5]*1000 <= 812,Ffd.f(Fi=Rk.t.cb/1000,dist.str=ct3.cb[[i]]$dts1[5]*1000,L.str=2),0))),nrow=1,ncol=1,byrow=TRUE),matrix(0,ncol=1,nrow=7))))
F.fw.ct3.str2<-lapply(1:length(cm.r),function(i) data.frame(F.fw.str2=rbind(matrix(0,ncol=1,nrow=4),matrix(unlist(lapply(1:1,function(j) ifelse(ct3.cb[[i]]$dts2[5]*1000 <= 812,Ffd.f(Fi=Rk.t.cb/1000,dist.str=ct3.cb[[i]]$dts2[5]*1000,L.str=2),0))),nrow=1,ncol=1,byrow=TRUE),matrix(0,ncol=1,nrow=7))))
F.fw.ct3.str3<-lapply(1:length(cm.r),function(i) data.frame(F.fw.str3=rbind(matrix(0,ncol=1,nrow=4),matrix(unlist(lapply(1:1,function(j) ifelse(ct3.cb[[i]]$dts3[5]*1000 <= 812,Ffd.f(Fi=Rk.t.cb/1000,dist.str=ct3.cb[[i]]$dts3[5]*1000,L.str=2),0))),nrow=1,ncol=1,byrow=TRUE),matrix(0,ncol=1,nrow=7))))
F.fw.ct3.str4<-lapply(1:length(cm.r),function(i) data.frame(F.fw.str4=rbind(matrix(0,ncol=1,nrow=4),matrix(unlist(lapply(1:1,function(j) ifelse(ct3.cb[[i]]$dts4[5]*1000 <= 812,Ffd.f(Fi=Rk.t.cb/1000,dist.str=ct3.cb[[i]]$dts4[5]*1000,L.str=2),0))),nrow=1,ncol=1,byrow=TRUE),matrix(0,ncol=1,nrow=7))))

#9.5 - update data frame

ct3.cb<-lapply(1:length(cm.r),function(i) cbind(ct3.cb[[i]],F.fw.ct3.str1[[i]],F.fw.ct3.str2[[i]],F.fw.ct3.str3[[i]],F.fw.ct3.str4[[i]]))

###################################################################################################
setwd("/Users/ollauri/Desktop/work/CONFERENCES/SBEE2025 /paper/submit/supplementary material/live crib wall model/outputs")
save(ct3.cb,file="cribwall_SBEE.RData")


#-------------------------------------------------
#10) updated slope and crib wall sketch
#-------------------------------------------------
library(grid)

plot(y~x,data=slope.xy,type="b",xlab="x (m)",ylab="y (m)",main="Slope Profile - sketch",cex.lab=1.5,cex.axis=1.5,cex.main=1.3,ylim=c(0,20))
segments(sl.df[1,1],0,sl.df[1,1],sl.df[1,2],col="red",lty=3) #slice 1
segments(sl.df[2,1],0,sl.df[2,1],sl.df[2,2],col="red",lty=3) #slice 2
segments(sl.df[3,1],0,sl.df[3,1],sl.df[3,2],col="red",lty=3) #slice 3
segments(sl.df[4,1],0,sl.df[4,1],sl.df[4,2],col="red",lty=3) #slice 4
segments(sl.df[5,1],0,sl.df[5,1],sl.df[5,2],col="red",lty=3) #slice 5
segments(sl.df[6,1],0,sl.df[6,1],sl.df[6,2],col="red",lty=3) #slice 6
segments(sl.df[7,1],0,sl.df[7,1],sl.df[7,2],col="red",lty=3) #slice 7
segments(sl.df[8,1],0,sl.df[8,1],sl.df[8,2],col="red",lty=3) #slice 8
segments(sl.df[9,1],0,sl.df[9,1],sl.df[9,2],col="red",lty=3) #slice 9
segments(sl.df[10,1],0,sl.df[10,1],sl.df[10,2],col="red",lty=3) #slice 10
segments(sl.df[11,1],0,sl.df[11,1],sl.df[11,2],col="red",lty=3)
segments(sl.df[12,1],0,sl.df[12,1],sl.df[11,2],col="red",lty=3)
segments(cb.members$x1[1],cb.members$y1[1],cb.members$x2[1],cb.members$y2[1],col="darkgreen",lwd=2)
segments(cb.members$x1[2],cb.members$y1[2],cb.members$x2[2],cb.members$y2[2],col="darkgreen",lwd=2)
segments(cb.members$x1[3],cb.members$y1[3],cb.members$x2[3],cb.members$y2[3],col="darkgreen",lwd=2)
segments(cb.members$x1[4],cb.members$y1[4],cb.members$x2[4],cb.members$y2[4],col="darkgreen",lwd=2)

points(cb.stretchers$x1[1],cb.stretchers$y1[1],cex=1,col="darkgreen")
points(cb.stretchers$x1[2],cb.stretchers$y1[2],cex=1,col="darkgreen")
points(cb.stretchers$x1[3],cb.stretchers$y1[3],cex=1,col="darkgreen")
points(cb.stretchers$x1[4],cb.stretchers$y1[4],cex=1,col="darkgreen")
points(cb.stretchers$x2[1],cb.stretchers$y2[1],cex=1,col="darkgreen")
points(cb.stretchers$x2[2],cb.stretchers$y2[2],cex=1,col="darkgreen")
points(cb.stretchers$x2[3],cb.stretchers$y2[3],cex=1,col="darkgreen")
points(cb.stretchers$x2[4],cb.stretchers$y2[4],cex=1,col="darkgreen")
segments(cb.stretchers$x1[4],cb.stretchers$y1[4],16,14,col="brown",lwd=3)

segments(sl.df.cb$x[1],0,sl.df.cb$x[1],sl.df.cb$y.fill[1],col="blue",lty=3)
segments(sl.df.cb$x[2],0,sl.df.cb$x[2],sl.df.cb$y.fill[2],col="blue",lty=3)
segments(sl.df.cb$x[3],0,sl.df.cb$x[3],sl.df.cb$y.fill[3],col="blue",lty=3)
segments(sl.df.cb$x[4],0,sl.df.cb$x[4],sl.df.cb$y.fill[4],col="blue",lty=3)
segments(sl.df.cb$x[5],0,sl.df.cb$x[5],sl.df.cb$y.fill[5],col="blue",lty=3)
segments(sl.df.cb$x[6],0,sl.df.cb$x[6],sl.df.cb$y.fill[6],col="blue",lty=3)
segments(sl.df.cb$x[7],0,sl.df.cb$x[7],sl.df.cb$y.fill[7],col="blue",lty=3)
segments(sl.df.cb$x[8],0,sl.df.cb$x[8],sl.df.cb$y.fill[8],col="blue",lty=3)
segments(sl.df.cb$x[9],0,sl.df.cb$x[9],sl.df.cb$y.fill[9],col="blue",lty=3)
segments(sl.df.cb$x[10],0,sl.df.cb$x[10],sl.df.cb$y.fill[10],col="blue",lty=3)
segments(sl.df.cb$x[11],0,sl.df.cb$x[11],sl.df.cb$y.fill[11],col="blue",lty=3)
segments(sl.df.cb$x[12],0,sl.df.cb$x[12],sl.df.cb$y.fill[12],col="blue",lty=3)
segments(sl.df.cb$x[13],0,sl.df.cb$x[13],sl.df.cb$y.fill[13],col="blue",lty=3)

points(18,15,pch=11,col="blue",cex=2)

library(plotrix)
draw.arc(cm.o$a.o,cm.o$b.o,cm.r[2],deg1=270,deg2=330,col="forestgreen")
draw.arc(cm.o$a.o,cm.o$b.o,cm.r[3],deg1=270,deg2=335,col="blue")
draw.arc(cm.o$a.o,cm.o$b.o,13,deg1=270,deg2=340,col="red")
segments(0,5,4,5,col="deepskyblue",lty=3,lwd=2)


legend("topleft",bty="n",c("cribwall member","cribwall stretcher","slope profile","slope grid","slice","slip circle 1: r=11 m c=(8,20)","slip circle 2: r=12 m c=(8,20)","slip circle 3: r=13 m c=(8,20)","sea level - spring tide" ,"outfall"),lty=c(1,0,1,1,2,1,1,1,3,0),pch=c(NA,1,NA,NA,NA,NA,NA,NA,NA,11),lwd=c(2,NA,2,2,2,2,2,2,2,NA),col=c("darkgreen","darkgreen","black","brown","blue","forestgreen","blue","red","deepskyblue","blue"),pt.cex=c(0,3,0,0,0,0,0,0,0,3))

#dev.off()

#zoom ins

plot(y~x,data=slope.xy,type="b",xlab="x (m)",ylab="y (m)",main="Slip failure 1",cex.lab=1.5,cex.axis=1.5,cex.main=1.3,ylim=c(6,20),xlim=c(6,22))
segments(sl.df[1,1],0,sl.df[1,1],sl.df[1,2],col="red",lty=3) #slice 1
segments(sl.df[2,1],0,sl.df[2,1],sl.df[2,2],col="red",lty=3) #slice 2
segments(sl.df[3,1],0,sl.df[3,1],sl.df[3,2],col="red",lty=3) #slice 3
segments(sl.df[4,1],0,sl.df[4,1],sl.df[4,2],col="red",lty=3) #slice 4
segments(sl.df[5,1],0,sl.df[5,1],sl.df[5,2],col="red",lty=3) #slice 5
segments(sl.df[6,1],0,sl.df[6,1],sl.df[6,2],col="red",lty=3) #slice 6
segments(sl.df[7,1],0,sl.df[7,1],sl.df[7,2],col="red",lty=3) #slice 7
segments(sl.df[8,1],0,sl.df[8,1],sl.df[8,2],col="red",lty=3) #slice 8
segments(sl.df[9,1],0,sl.df[9,1],sl.df[9,2],col="red",lty=3) #slice 9
segments(sl.df[10,1],0,sl.df[10,1],sl.df[10,2],col="red",lty=3) #slice 10
segments(sl.df[11,1],0,sl.df[11,1],sl.df[11,2],col="red",lty=3)
segments(sl.df[12,1],0,sl.df[12,1],sl.df[11,2],col="red",lty=3)
segments(cb.members$x1[1],cb.members$y1[1],cb.members$x2[1],cb.members$y2[1],col="darkgreen",lwd=2)
segments(cb.members$x1[2],cb.members$y1[2],cb.members$x2[2],cb.members$y2[2],col="darkgreen",lwd=2)
segments(cb.members$x1[3],cb.members$y1[3],cb.members$x2[3],cb.members$y2[3],col="darkgreen",lwd=2)
segments(cb.members$x1[4],cb.members$y1[4],cb.members$x2[4],cb.members$y2[4],col="darkgreen",lwd=2)

points(cb.stretchers$x1[1],cb.stretchers$y1[1],cex=1,col="darkgreen")
points(cb.stretchers$x1[2],cb.stretchers$y1[2],cex=1,col="darkgreen")
points(cb.stretchers$x1[3],cb.stretchers$y1[3],cex=1,col="darkgreen")
points(cb.stretchers$x1[4],cb.stretchers$y1[4],cex=1,col="darkgreen")
points(cb.stretchers$x2[1],cb.stretchers$y2[1],cex=1,col="darkgreen")
points(cb.stretchers$x2[2],cb.stretchers$y2[2],cex=1,col="darkgreen")
points(cb.stretchers$x2[3],cb.stretchers$y2[3],cex=1,col="darkgreen")
points(cb.stretchers$x2[4],cb.stretchers$y2[4],cex=1,col="darkgreen")
segments(cb.stretchers$x1[4],cb.stretchers$y1[4],16,14,col="brown",lwd=3)

segments(sl.df.cb$x[1],0,sl.df.cb$x[1],sl.df.cb$y.fill[1],col="blue",lty=3)
segments(sl.df.cb$x[2],0,sl.df.cb$x[2],sl.df.cb$y.fill[2],col="blue",lty=3)
segments(sl.df.cb$x[3],0,sl.df.cb$x[3],sl.df.cb$y.fill[3],col="blue",lty=3)
segments(sl.df.cb$x[4],0,sl.df.cb$x[4],sl.df.cb$y.fill[4],col="blue",lty=3)
segments(sl.df.cb$x[5],0,sl.df.cb$x[5],sl.df.cb$y.fill[5],col="blue",lty=3)
segments(sl.df.cb$x[6],0,sl.df.cb$x[6],sl.df.cb$y.fill[6],col="blue",lty=3)
segments(sl.df.cb$x[7],0,sl.df.cb$x[7],sl.df.cb$y.fill[7],col="blue",lty=3)
segments(sl.df.cb$x[8],0,sl.df.cb$x[8],sl.df.cb$y.fill[8],col="blue",lty=3)
segments(sl.df.cb$x[9],0,sl.df.cb$x[9],sl.df.cb$y.fill[9],col="blue",lty=3)
segments(sl.df.cb$x[10],0,sl.df.cb$x[10],sl.df.cb$y.fill[10],col="blue",lty=3)
segments(sl.df.cb$x[11],0,sl.df.cb$x[11],sl.df.cb$y.fill[11],col="blue",lty=3)
segments(sl.df.cb$x[12],0,sl.df.cb$x[12],sl.df.cb$y.fill[12],col="blue",lty=3)
segments(sl.df.cb$x[13],0,sl.df.cb$x[13],sl.df.cb$y.fill[13],col="blue",lty=3)
library(plotrix)
draw.arc(cm.o$a.o,cm.o$b.o,cm.r[2],deg1=270,deg2=330,col="forestgreen",lwd=3)
points(18,15,pch=11,col="blue",cex=2)

legend("topleft",bty="n",c("cribwall member","cribwall stretcher","slope profile","slope grid","slice","slip circle 1: r=11 m c=(8,20)","slip circle 2: r=12 m c=(8,20)","slip circle 3: r=13 m c=(8,20)","sea level - spring tide" ),lty=c(1,0,1,1,2,1,1,1,3),pch=c(NA,1,NA,NA,NA,NA,NA,NA,NA),lwd=c(2,NA,2,2,2,2,2,2,2),col=c("darkgreen","darkgreen","black","brown","blue","forestgreen","blue","red","deepskyblue"),pt.cex=c(0,3,0,0,0,0,0,0,0,0))




plot(y~x,data=slope.xy,type="b",xlab="x (m)",ylab="y (m)",main="Slip failure 3",cex.lab=1.5,cex.axis=1.5,cex.main=1.3,ylim=c(6,20),xlim=c(6,22))
segments(sl.df[1,1],0,sl.df[1,1],sl.df[1,2],col="red",lty=3) #slice 1
segments(sl.df[2,1],0,sl.df[2,1],sl.df[2,2],col="red",lty=3) #slice 2
segments(sl.df[3,1],0,sl.df[3,1],sl.df[3,2],col="red",lty=3) #slice 3
segments(sl.df[4,1],0,sl.df[4,1],sl.df[4,2],col="red",lty=3) #slice 4
segments(sl.df[5,1],0,sl.df[5,1],sl.df[5,2],col="red",lty=3) #slice 5
segments(sl.df[6,1],0,sl.df[6,1],sl.df[6,2],col="red",lty=3) #slice 6
segments(sl.df[7,1],0,sl.df[7,1],sl.df[7,2],col="red",lty=3) #slice 7
segments(sl.df[8,1],0,sl.df[8,1],sl.df[8,2],col="red",lty=3) #slice 8
segments(sl.df[9,1],0,sl.df[9,1],sl.df[9,2],col="red",lty=3) #slice 9
segments(sl.df[10,1],0,sl.df[10,1],sl.df[10,2],col="red",lty=3) #slice 10
segments(sl.df[11,1],0,sl.df[11,1],sl.df[11,2],col="red",lty=3)
segments(sl.df[12,1],0,sl.df[12,1],sl.df[11,2],col="red",lty=3)
segments(cb.members$x1[1],cb.members$y1[1],cb.members$x2[1],cb.members$y2[1],col="darkgreen",lwd=2)
segments(cb.members$x1[2],cb.members$y1[2],cb.members$x2[2],cb.members$y2[2],col="darkgreen",lwd=2)
segments(cb.members$x1[3],cb.members$y1[3],cb.members$x2[3],cb.members$y2[3],col="darkgreen",lwd=2)
segments(cb.members$x1[4],cb.members$y1[4],cb.members$x2[4],cb.members$y2[4],col="darkgreen",lwd=2)

points(cb.stretchers$x1[1],cb.stretchers$y1[1],cex=1,col="darkgreen")
points(cb.stretchers$x1[2],cb.stretchers$y1[2],cex=1,col="darkgreen")
points(cb.stretchers$x1[3],cb.stretchers$y1[3],cex=1,col="darkgreen")
points(cb.stretchers$x1[4],cb.stretchers$y1[4],cex=1,col="darkgreen")
points(cb.stretchers$x2[1],cb.stretchers$y2[1],cex=1,col="darkgreen")
points(cb.stretchers$x2[2],cb.stretchers$y2[2],cex=1,col="darkgreen")
points(cb.stretchers$x2[3],cb.stretchers$y2[3],cex=1,col="darkgreen")
points(cb.stretchers$x2[4],cb.stretchers$y2[4],cex=1,col="darkgreen")
segments(cb.stretchers$x1[4],cb.stretchers$y1[4],16,14,col="brown",lwd=3)

segments(sl.df.cb$x[1],0,sl.df.cb$x[1],sl.df.cb$y.fill[1],col="blue",lty=3)
segments(sl.df.cb$x[2],0,sl.df.cb$x[2],sl.df.cb$y.fill[2],col="blue",lty=3)
segments(sl.df.cb$x[3],0,sl.df.cb$x[3],sl.df.cb$y.fill[3],col="blue",lty=3)
segments(sl.df.cb$x[4],0,sl.df.cb$x[4],sl.df.cb$y.fill[4],col="blue",lty=3)
segments(sl.df.cb$x[5],0,sl.df.cb$x[5],sl.df.cb$y.fill[5],col="blue",lty=3)
segments(sl.df.cb$x[6],0,sl.df.cb$x[6],sl.df.cb$y.fill[6],col="blue",lty=3)
segments(sl.df.cb$x[7],0,sl.df.cb$x[7],sl.df.cb$y.fill[7],col="blue",lty=3)
segments(sl.df.cb$x[8],0,sl.df.cb$x[8],sl.df.cb$y.fill[8],col="blue",lty=3)
segments(sl.df.cb$x[9],0,sl.df.cb$x[9],sl.df.cb$y.fill[9],col="blue",lty=3)
segments(sl.df.cb$x[10],0,sl.df.cb$x[10],sl.df.cb$y.fill[10],col="blue",lty=3)
segments(sl.df.cb$x[11],0,sl.df.cb$x[11],sl.df.cb$y.fill[11],col="blue",lty=3)
segments(sl.df.cb$x[12],0,sl.df.cb$x[12],sl.df.cb$y.fill[12],col="blue",lty=3)
segments(sl.df.cb$x[13],0,sl.df.cb$x[13],sl.df.cb$y.fill[13],col="blue",lty=3)

library(plotrix)

points(18,15,pch=11,col="blue",cex=2)


draw.arc(cm.o$a.o,cm.o$b.o,13,deg1=270,deg2=340,col="red",lwd=3)


legend("topleft",bty="n",c("cribwall member","cribwall stretcher","slope profile","slope grid","slice","slip circle 1: r=11 m c=(8,20)","slip circle 2: r=12 m c=(8,20)","slip circle 3: r=13 m c=(8,20)","sea level - spring tide" ),lty=c(1,0,1,1,2,1,1,1,3),pch=c(NA,1,NA,NA,NA,NA,NA,NA,NA),lwd=c(2,NA,2,2,2,2,2,2,2),col=c("darkgreen","darkgreen","black","brown","blue","forestgreen","blue","red","deepskyblue"),pt.cex=c(0,3,0,0,0,0,0,0,0,0))



plot(y~x,data=slope.xy,type="b",xlab="x (m)",ylab="y (m)",main="Slip failure 2",cex.lab=1.5,cex.axis=1.5,cex.main=1.3,ylim=c(6,20),xlim=c(6,22))
segments(sl.df[1,1],0,sl.df[1,1],sl.df[1,2],col="red",lty=3) #slice 1
segments(sl.df[2,1],0,sl.df[2,1],sl.df[2,2],col="red",lty=3) #slice 2
segments(sl.df[3,1],0,sl.df[3,1],sl.df[3,2],col="red",lty=3) #slice 3
segments(sl.df[4,1],0,sl.df[4,1],sl.df[4,2],col="red",lty=3) #slice 4
segments(sl.df[5,1],0,sl.df[5,1],sl.df[5,2],col="red",lty=3) #slice 5
segments(sl.df[6,1],0,sl.df[6,1],sl.df[6,2],col="red",lty=3) #slice 6
segments(sl.df[7,1],0,sl.df[7,1],sl.df[7,2],col="red",lty=3) #slice 7
segments(sl.df[8,1],0,sl.df[8,1],sl.df[8,2],col="red",lty=3) #slice 8
segments(sl.df[9,1],0,sl.df[9,1],sl.df[9,2],col="red",lty=3) #slice 9
segments(sl.df[10,1],0,sl.df[10,1],sl.df[10,2],col="red",lty=3) #slice 10
segments(sl.df[11,1],0,sl.df[11,1],sl.df[11,2],col="red",lty=3)
segments(sl.df[12,1],0,sl.df[12,1],sl.df[11,2],col="red",lty=3)

segments(cb.members$x1[1],cb.members$y1[1],cb.members$x2[1],cb.members$y2[1],col="darkgreen",lwd=2)
segments(cb.members$x1[2],cb.members$y1[2],cb.members$x2[2],cb.members$y2[2],col="darkgreen",lwd=2)
segments(cb.members$x1[3],cb.members$y1[3],cb.members$x2[3],cb.members$y2[3],col="darkgreen",lwd=2)
segments(cb.members$x1[4],cb.members$y1[4],cb.members$x2[4],cb.members$y2[4],col="darkgreen",lwd=2)

points(cb.stretchers$x1[1],cb.stretchers$y1[1],cex=1,col="darkgreen")
points(cb.stretchers$x1[2],cb.stretchers$y1[2],cex=1,col="darkgreen")
points(cb.stretchers$x1[3],cb.stretchers$y1[3],cex=1,col="darkgreen")
points(cb.stretchers$x1[4],cb.stretchers$y1[4],cex=1,col="darkgreen")
points(cb.stretchers$x2[1],cb.stretchers$y2[1],cex=1,col="darkgreen")
points(cb.stretchers$x2[2],cb.stretchers$y2[2],cex=1,col="darkgreen")
points(cb.stretchers$x2[3],cb.stretchers$y2[3],cex=1,col="darkgreen")
points(cb.stretchers$x2[4],cb.stretchers$y2[4],cex=1,col="darkgreen")
segments(cb.stretchers$x1[4],cb.stretchers$y1[4],16,14,col="brown",lwd=3)

segments(sl.df.cb$x[1],0,sl.df.cb$x[1],sl.df.cb$y.fill[1],col="blue",lty=3)
segments(sl.df.cb$x[2],0,sl.df.cb$x[2],sl.df.cb$y.fill[2],col="blue",lty=3)
segments(sl.df.cb$x[3],0,sl.df.cb$x[3],sl.df.cb$y.fill[3],col="blue",lty=3)
segments(sl.df.cb$x[4],0,sl.df.cb$x[4],sl.df.cb$y.fill[4],col="blue",lty=3)
segments(sl.df.cb$x[5],0,sl.df.cb$x[5],sl.df.cb$y.fill[5],col="blue",lty=3)
segments(sl.df.cb$x[6],0,sl.df.cb$x[6],sl.df.cb$y.fill[6],col="blue",lty=3)
segments(sl.df.cb$x[7],0,sl.df.cb$x[7],sl.df.cb$y.fill[7],col="blue",lty=3)
segments(sl.df.cb$x[8],0,sl.df.cb$x[8],sl.df.cb$y.fill[8],col="blue",lty=3)
segments(sl.df.cb$x[9],0,sl.df.cb$x[9],sl.df.cb$y.fill[9],col="blue",lty=3)
segments(sl.df.cb$x[10],0,sl.df.cb$x[10],sl.df.cb$y.fill[10],col="blue",lty=3)
segments(sl.df.cb$x[11],0,sl.df.cb$x[11],sl.df.cb$y.fill[11],col="blue",lty=3)
segments(sl.df.cb$x[12],0,sl.df.cb$x[12],sl.df.cb$y.fill[12],col="blue",lty=3)
segments(sl.df.cb$x[13],0,sl.df.cb$x[13],sl.df.cb$y.fill[13],col="blue",lty=3)

library(plotrix)
points(18,15,pch=11,col="blue",cex=2)

draw.arc(cm.o$a.o,cm.o$b.o,cm.r[3],deg1=270,deg2=335,col="blue",lwd=3)


legend("topleft",bty="n",c("cribwall member","cribwall stretcher","slope profile","slope grid","slice","slip circle 1: r=11 m c=(8,20)","slip circle 2: r=12 m c=(8,20)","slip circle 3: r=13 m c=(8,20)","sea level - spring tide" ),lty=c(1,0,1,1,2,1,1,1,3),pch=c(NA,1,NA,NA,NA,NA,NA,NA,NA),lwd=c(2,NA,2,2,2,2,2,2,2),col=c("darkgreen","darkgreen","black","brown","blue","forestgreen","blue","red","deepskyblue"),pt.cex=c(0,3,0,0,0,0,0,0,0,0))
