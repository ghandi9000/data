## Test df long format against original
## Original: not modified at all
## Long: with all transformations, only doug fir
orig <- read.csv("C:/Users/Noah/Dropbox/Shared/hazard/ControlPlots.csv")
dflong <- read.csv("long-dfonly-derived.csv")

names(orig) <- tolower(names(orig))

## Tests needed:
## 1. Prior values: compare pplotbv[i] and pplotden[i]
##    against plotbv[i-1] and plotden[i-1] --> there are minor differences now,
##    probably due to treating factors as integers and taking means
##    - prior densities look OK
## Run Tests
per <- c(1) # choose periods to check
yrs <- as.numeric(names(table(dflong$time)))

for(i in per) {
    origA <- orig[orig[,paste0("stat",i)] != "DEAD" &
                  orig[,paste0("spp")] == "FD" &
                  !is.na(orig[,paste0("bv",i)]),]
    longA <- dflong[dflong[,"time"] == yrs[i] &
                    dflong[,"stat"] != "DEAD" &
                    !is.na(dflong[,"bv"]),]
    den <- ddply(origA, .(install,plot), .fun = function(x) {
        data.frame(plotden = mean(nrow(x)/x$pltarea))
    })
    den2 <- ddply(longA, .(install,plot), .fun = function(x) {
        data.frame(plotden = nrow(x)/x$pltarea, plot=mean(x$plot), install=mean(x$install))
    })
    den3 <- ddply(longA, .(pplot), .fun = function(x) {
        data.frame(plotden = mean(x$plotden),plot=mean(x$plot),install=mean(x$install))
    })
    mass <- ddply(origA, .(install,plot), .fun = function(x) {
        data.frame(plotbv = sum(x[,paste0("bv",i)]))
    })
    mass2 <- ddply(longA, .(pplot), .fun = function(x) {
        data.frame(plotbv = mean(x$plotbv), plot=mean(x$plot), install=mean(x$install))
    })
    den3 <- den3[order(den3$install, den3$plot),]
    mass2 <- mass2[order(mass2$install, mass2$plot),]
    print("Number of doug firs per installation by sampling period")
    print(all(table(longA$install) == table(origA$install))) # check
    print("Total number of live doug firs by sampling period")
    print(nrow(longA) == nrow(origA)) # check
    print("Number of doug firs per plot by sampling period")
    print(all(table(longA$plot) == table(origA$plot))) # check
    print("Total bole volume of live firs"); print(sum(longA$bv));
    print(sum(longA$bv) == sum(origA[,paste0("bv",i)]))
    print("Plot densities by installation / plot / period")
    print(all(round(den$plotden,2) == round(den3$plotden,2)))
    print("Plot total BV by install/plot/period")
    print(all(round(mass$plotbv,2) == round(mass2$plotbv,2)))
}

## Check pplot labeling... checks out!
numplot <- as.numeric(names(table(dflong$pplot))) ## number of plots computed as is

nplot <- ddply(orig, .(install, plot), .fun = function(x) {
    data.frame(pname = paste(x$install, x$plot, sep = ","))
})
length(levels(nplot$pname))

## Check densities, some differences
head(den)
head(den3)
den3[which(!den3$plotden==den$plotden),]
den[which(!den3$plotden==den$plotden),]
den$plotden == den3$plotden
den3[21,]
round(den[21,"plotden"],2) == round(den3[21,"plotden"],2)

## Check total bole volumes, some diffs -- just a matter or rounding
which(!mass$plotbv==mass2$plotbv)
mass[9,]
mass2[9,]
