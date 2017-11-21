###############################################################################
# SUPPLEMENTARY WEB APPENDIX material:
#   "Reducing and meta-analyzing estimates from distributed lag non-linear models"
#   Gasparrini and Armstrong 
#   BMC Medical research Methodology - 2012
#
# November 2012
# * an updated version of this code, (hopefully) compatible with future
#   versions of the software, is available at the personal website of the
#   first author (www.ag-myresearch.com)
###############################################################################

####################################################################
# RUN AN EXAMPLE OF FIRST-STAGE ANALYSIS FOR A SINGLE REGION
####################################################################

# SELECT REGION
reg <- "N-East"

# ARGUMENTS AND LISTS FOR CROSS-BASIS DEFINITION
bound <- colMeans(ranges)
varknots <- bound[1] + diff(bound)/3*(1:2)
argvar <- list(type="bs",degree=2,knots=varknots,bound=bound,cen=17)
arglag <- list(type="ns",df=5)

# BASIS FOR TEMPERATURE:
# - QUADRATIC SPLINE FOR PREDICTOR, WITH SPECIFIC KNOT SELECTION
# - NATURAL CUBIC SPLINE FOR LAG, WITH DF AT EQUALLY-SPACED LOG-VALUES
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
suppressWarnings(
cb <- crossbasis(data[[reg]]$tmean,lag=21,argvar=argvar,arglag=arglag)
)
summary(cb)

# RUN THE MODEL
model <- glm(death ~ cb + dow + ns(time,df=10*14),
  family=quasipoisson(),data[[reg]])

# PREDICTION USING:
#   crosspred FOR BI-DIMENSIONAL RELATIONSHIP
#   crossreduce FOR UNI-DIMENSIONAL SUMMARIES
# (NB: ALL THE ESTIMATES ARE ALREADY REPORTED BY crosspred ALONE)

cp <- crosspred(cb,model,from=bound[1],to=bound[2],by=1)
crall <- crossreduce(cb,model,from=bound[1],to=bound[2],by=0.2)
crlag <- crossreduce(cb,model,type="lag",value=4,from=bound[1],
  to=bound[2],bylag=0.2)
crvar <- crossreduce(cb,model,type="var",value=22,from=bound[1],
  to=bound[2],bylag=0.2)

# PLOTS

pdf("K:\\RSampleCode\\DLNM\\figure1.pdf",height=6,width=8.5)
par(mar=c(1.5,1,0,0)+0.1,cex.axis=0.9,cex.lab=1)
layout(matrix(rep(1:4,each=2),2,4,byrow=TRUE))

# 3D PLOT WITH DIFFERENT NON-DEFAULT PERSPECTIVE AND GREY SURFACE LINES
d3 <- plot(cp,xlab="Tempeature (C)",zlab="RR",phi=35,theta=205,ltheta=170,
  shade=0.4)

# LINES IN THE SURFACE CORRESPONDING TO THE EFFECTS IN THE PLOTS BELOW
lines(trans3d(x=17,y=0:21,z=cp$matRRfit[as.character(17),],
  pmat=d3),lwd=2)
lines(trans3d(x=22,y=0:21,z=cp$matRRfit[as.character(22),],
  pmat=d3),lwd=2,col=2)a
lines(trans3d(x=cp$predvar,y=4,z=cp$matRRfit[,"lag4"],
  pmat=d3),lwd=2,col=2)

par(mar=c(5,4,1,1)+0.1,mgp=c(2.5,1,0))

# PLOTS FOR PREDICTOR-SPECIFIC, LAG-SPECIFIC AND OVERALL CUMULATIVE SUMMARIES
plot(crvar,xlab="Lag",ylab="RR",col=2,lwd=2)
mtext(text=paste("Predictor-specific association at temperature ",22,
  "C",sep=""),cex=0.7)
plot(crlag,xlab="Tempeature (C)",ylab="RR",col=2,ylim=c(.96,1.06),lwd=2)
mtext(text="Lag-specific association at lag 4",cex=0.7)
plot(crall,xlab="Tempeature (C)",ylab="RR",ylim=c(.8,2),col=2,lwd=2)
mtext(text="Overall cumulative association",cex=0.7)

dev.off()

#