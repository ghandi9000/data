## Add LRS column to hazard data
## LRS: tree size / max tree size in plot (including all species)
library(plyr)
bc1 <- read.csv("~/work/data/data/long-bc-derived.csv")
## NOTE: there are missing status values!?  Should they be removed before calculating
## LRS?
bc1 <- bc1[bc1$stat == "ALIVE",]
bc <- read.csv("~/work/data/data/hazards/hazards-bc-firs.csv")

## calculate lrs: tree size / max size in plot
lrs <- ddply(bc1, .(install, plot, time), .fun = function(x) {
    data.frame(id = x$id, lrs = x$bv/max(x$bv, na.rm=TRUE))
})

## take just the lrs for hazards
lrs <- lrs[order(lrs$install, lrs$plot, lrs$time, lrs$id),]
bc1 <- bc1[order(bc1$install, bc1$plot, bc1$time, bc1$id),]
bc1$lrs <- lrs$lrs

## add lrs to hazard data
bc1 <- bc1[!is.na(bc1$stat) & !is.na(bc1$bvgrowth) &
           bc1$spec == "FD" & bc1$bvgrowth >= 0,]
bc1 <- bc1[order(bc1$install, bc1$plot, bc1$time, bc1$id),]
bc <- bc[order(bc$install, bc$plot, bc$time, bc$id),]
bc$lrs <- bc1$lrs

## write data
write.csv(bc, "~/work/data/data/hazards/hazards-bc-firs.csv", row.names=FALSE)

