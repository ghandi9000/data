## Create the three hazard periods:
## time periods (3-5), (5-6), (6-7)
## For each period use rgr and lrs from prior period
## priorbv becomes the bole volume at the beginning of the period and bv is bole volume
##  from the end of the period
bc <- read.csv("~/work/data/data/hazards/hazards-bc-firs.csv")

## create hazPeriod column, 1: time==5, 2:time==6, 3:time==7
bc$hazPeriod <- ifelse(bc$time == 5, 1, ifelse(bc$time == 6, 2,
                       ifelse(bc$time == 7, 3, NA)))

hazards <- bc[!is.na(bc$hazPeriod),]

## calculate lambdas: log(number live beginning period) - log (number alive at end period)
library(plyr)
lambdas <- ddply(hazards, .(install, plot), .fun = function(x) {
    data.frame(per1 =
               log(nrow(x[x$hazPeriod==1,])) - log(nrow(x[x$hazPeriod==1 &
                          x$died==0,])),
               per2 =
               log(nrow(x[x$hazPeriod==2,])) - log(nrow(x[x$hazPeriod==2 &
                          x$died==0,])),
               per3 =
               log(nrow(x[x$hazPeriod==3,])) - log(nrow(x[x$hazPeriod==3 &
                          x$died==0,]))
               )
})

## Add lambdas to hazards
lambs <- ddply(hazards, .(install, plot, hazPeriod), .fun = function(x) {
    ls <- lambdas[unique(x$install)==lambdas$install & unique(x$plot) == lambdas$plot,]
    lambda <- as.numeric(ls[2+ unique(x$hazPeriod)])
    data.frame(lambda = rep(lambda, nrow(x)))
})
lambs <- lambs[order(lambs$install, lambs$plot, lambs$hazPeriod),]
hazards <- hazards[order(hazards$install, hazards$plot, hazards$hazPeriod),]
hazards$lambda <- lambs$lambda

## columns to keep
toKeep <- c("install","plot","time","hazPeriod","elev","aspect","tag","id","spec",
            "si.hazard","sdp.hazard","rgr","method","corr","degree","lrs","died",
            "lambda")

## slim data down
hazards <- hazards[,names(hazards) %in% toKeep]

## write data
write.csv(hazards, "~/work/data/data/hazards/hazards-bc-firs-final.csv", row.names=FALSE)

