## Add plot density, prior plot density, plot bole volume,
##  prior plot bole volume, plot basal area, and prior plot basal area
##  to dflong (Using ALL species).
source('functions.R')
dflong <- read.csv("long-df.csv")
dflong <- dflong[order(dflong$install,dflong$plot, dflong$time),]

## some stuff to add to make-long
dflong$pltarea <- as.factor(dflong$pltarea)
dflong$pplot <- as.factor(dflong$pplot)

## get initial stem densities / plot (not by time, because any trees alive in the
##  whole sample period were alive at the beginning, this will be the starting density
##  for 73 period (or period 1), subsequent densities will only remove trees specifically
##  marked as dead
initial.stems <- ddply(dflong, .(install, plot), .fun = function(x) {
    x <- droplevels(x)
    total <- x[unique(x$tag),]
    data.frame = c(
    den = nrow(total[total$stat == "ALIVE",])/as.numeric(levels(total$pltarea)),
    count = nrow(total[total$stat == "ALIVE",]),
    pltarea = as.numeric(levels(total$pltarea)))
})

## get number of deaths by install/plot/time, dead trees appear to only be marked dead
##  once and then not repeatedly sampled
died <- ddply(dflong, .(install, plot, time), .fun = function(x) {
    x <- droplevels(x)
    data.frame = c(
    install = mean(x$install),
    plot = mean(x$plot),
    died = nrow(x[x$stat == "DEAD",]))
})

stems <- ddply(dflong, .(install, plot, time), .fun = function(x) {
    x <- droplevels(x)
    prevdead = sum(died[died$install == mean(x$install) & died$plot == mean(x$plot) &
    died$time <= mean(x$time),]$died)
    numalive = initial.stems[initial.stems$install == mean(x$install) &
    initial.stems$plot == mean(x$plot),]$count - prevdead
    data.frame = c(
    install = mean(x$install),
    plot = mean(x$plot),
    prevdead = prevdead,
    numalive = numalive,
    den = numalive/as.numeric(levels(x$pltarea)))
})

times <- ddply(dflong, .(install,plot,time), function(x) nrow(x)) # rows/plot/time
dflong$plotden <- rep(stems$den, times = times[,4])

## get prior stem densities / plot / time period (N0)
dflong$pplotden <- rep(NA, nrow(dflong))
dflong[dflong$time != 73,]$pplotden <- rep(stems[stems$time!=97,]$den,
       times = times[times$time != 73,4])

## check prior stem density calculations
tst <- subset(dflong, pplot == 1.1 & time %in% c(73,76, 79))
mean(tst[tst$time == 73,]$plotden)
mean(tst[tst$time == 76,]$pplotden)
mean(tst[tst$time == 79,]$plotden)

## get BA values / plot / time period
##  use only live trees??
basal <- ddply(dflong, .(install, plot, time), .fun = function(x) {
    x <- droplevels(x)
    sum(x[x$stat == "ALIVE",]$ba, na.rm=TRUE)/as.numeric(levels(x$pltarea))
})
dflong$plotba <- rep(basal[,4], times = times[,4])

## check plotBA calculations
tst <- droplevels(subset(dflong, pplot == 1.1 & time == 73))
basal1.1 <- sum(tst[tst$stat == "ALIVE",]$ba,na.rm = TRUE)/as.numeric(levels(tst$pltarea))
basal1.1
mean(tst$plotba)

## get prior BA values / plot / time period (used as B0 in calculations)
##  use only live trees??
dflong$pplotba <- rep(NA, nrow(dflong))
dflong[dflong$time != 73,]$pplotba <- rep(basal[basal$time!=97,4],
       times = times[times$time != 73,4])

## check prior plot ba calculations
tst <- droplevels(subset(dflong, pplot == 1.1 & time == 79))
pbasal1.1 <- sum(tst[tst$stat == "ALIVE",]$priorba, na.rm = TRUE)/as.numeric(levels(tst$pltarea))
pbasal1.1
mean(tst$pplotba)

## get BV values / plot / time period
##  use only live trees??
bv <- ddply(dflong, .(install, plot, time), .fun = function(x) {
    x <- droplevels(x)
    sum(x[x$stat == "ALIVE",]$bv, na.rm=TRUE)/as.numeric(levels(x$pltarea))
})
dflong$plotbv <- rep(bv[,4], times = times[,4])

## check plotBV calculations
tst <- droplevels(subset(dflong, pplot == 1.1 & time == 73))
bvsal1.1 <- sum(tst[tst$stat == "ALIVE",]$bv,na.rm=TRUE)/as.numeric(levels(tst$pltarea))
bvsal1.1
mean(tst$plotbv)

## get prior BV values / plot / time period (used as B0 in calculations)
##  use only live trees??
dflong$pplotbv <- rep(NA, nrow(dflong))
dflong[dflong$time != 73,]$pplotbv <- rep(bv[bv$time!=97,4],
       times = times[times$time != 73,4])

## check prior plot bv calculations
tst1 <- droplevels(subset(dflong, pplot == 1.1 & time == 73))
pbvsal1.1 <- sum(tst[tst$stat == "ALIVE",]$priorbv, na.rm = TRUE)/
    as.numeric(levels(tst$pltarea))
pbvsal1.1
mean(tst$plotbv)

## Add SDP values calculated using just Doug Firs
dfs <- read.csv("long-dfonly-derived.csv")
sdps <- ddply(dfs, .(install, plot, time), .fun = function(x) {
    x <- droplevels(x)
    data.frame = c(
    install = mean(x$install),
    plot = mean(x$plot),
    time = mean(x$time),
    sdp = mean(x$sdp),
    sdpclass = levels(x$sdpclass))
})
times = ddply(dflong, .(install, plot, time), .fun = function(x) {
    nrow(x) })
dflong$sdp <- rep(sdps$sdp, times = times[,4])
dflong$sdpclass <- rep(sdps$sdpclass, times = times[,4])
dflong$sdp <- as.numeric(dflong$sdp)

## write data
write.csv(dflong, "long-bc-derived.csv", row.names = FALSE)

################################################################
## make plot-level (single-point per plot/time) dataframe
dflong$time <- as.numeric(dflong$time)
dfplot <- ddply(dflong, .(install, plot, time), function(x) {
    x <- droplevels(x)
    data.frame(
        pplot = levels(x$pplot),
        plotden = mean(x$plotden, na.rm=TRUE),
        pplotden = mean(x$pplotden, na.rm=TRUE),
        plotba = mean(x$plotba, na.rm=TRUE),
        pplotba = mean(x$pplotba, na.rm=TRUE),
        plotbv = mean(x$plotbv, na.rm=TRUE),
        pplotbv = mean(x$pplotbv, na.rm=TRUE))
})
