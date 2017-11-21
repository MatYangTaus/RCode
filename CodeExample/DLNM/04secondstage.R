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
# SECOND STAGE
# - RUN THE MULTIVARIATE META-ANALYTICAL MODELS WITH mvmeta
# - CREATE BASIS VARIABLES USING onebasis, TO BE USED FOR PREDICTION
# - OBTAIN PREDICTIONS THROUGH crosspred (dlnm)
####################################################################

####################################################################
# PERFORM MULTIVARIATE META-ANALYSIS

# LOAD THE PACKAGES (mvmeta PACKAGE IS ASSUMED TO BE INSTALLED)
library(mvmeta)

# SELECT THE ESTIMATION METHOD
method <- "reml"
# IN THE CURRENT VERSION, SET control=list(trace=6,REPORT=1) TO 
#   INSPECT THE OPTIMIZATION SEARCH

# OVERALL CUMULATIVE SUMMARY FOR THE MAIN MODEL
mvall <- mvmeta(yall~1,Sall,method=method)
summary(mvall)

# OVERALL CUMULATIVE SUMMARY FOR THE ALTERNATIVE MODELS
mvall2 <- mvmeta(yall2~1,Sall2,method=method)
summary(mvall2)
mvall3 <- mvmeta(yall3~1,Sall3,method=method)
summary(mvall3)

# PREDICTOR-SPECIFIC SUMMARY FOR 22C (MAIN MODEL)
mvhot <- mvmeta(yhot~1,Shot,method=method)
summary(mvhot)
# NOTE THE PROBLEM FOR ESTIMATED (CO)VARIANCE MATRIX

# PREDICTOR-SPECIFIC SUMMARY FOR 0C (MAIN MODEL)
mvcold <- mvmeta(ycold~1,Scold,method=method)
summary(mvcold)

####################################################################
# CREATE BASES FOR PREDICTION

# BASES OF TEMPERATURE AND LAG USED TO PREDICT, EQUAL TO THAT USED FOR ESTIMATION
# COMPUTED USING THE ATTRIBUTES OF THE CROSS-BASIS USED IN ESTIMATION
xvar <- seq(bound[1],bound[2],by=0.1)
bvar <- do.call("onebasis",c(list(x=xvar),attr(cb,"argvar")))
xlag <- 0:210/10
blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))

####################################################################
# REGION-SPECIFIC FIRST-STAGE SUMMARIES

regall <- apply(yall,1,function(x) exp(bvar%*%x))
reghot <- apply(yhot,1,function(x) exp(blag%*%x))
regcold <- apply(ycold,1,function(x) exp(blag%*%x))

####################################################################
# PREDICTION FOR A GRID OF TEMPERATURE AND LAG VALUES

# OVERALL CUMULATIVE SUMMARY ASSOCIATION FOR MAIN MODEL
cpall <- crosspred(bvar,coef=coef(mvall),vcov=vcov(mvall),
  model.link="log",by=0.1,from=bound[1],to=bound[2])

# OVERALL CUMULATIVE SUMMARY ASSOCIATION FOR ALTERNATIVE MODELS
cpall2 <- crosspred(bvar,coef=coef(mvall2),vcov=vcov(mvall2),
  model.link="log",by=0.1,from=bound[1],to=bound[2])
cpall3 <- crosspred(bvar,coef=coef(mvall3),vcov=vcov(mvall3),
  model.link="log",by=0.1,from=bound[1],to=bound[2])

# PREDICTOR-SPECIFIC SUMMARIES FOR 22C (MAIN MODEL)
cphot <- crosspred(blag,coef=coef(mvhot),vcov=vcov(mvhot),
  model.link="log",at=0:210/10)

# PREDICTOR-SPECIFIC SUMMARIES FOR 22C (MAIN MODEL)
cpcold <- crosspred(blag,coef=coef(mvcold),vcov=vcov(mvcold),
  model.link="log",at=0:210/10)

#
