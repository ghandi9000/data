## Add trees that died back into hazards dataset and create a died column:
## 0 if a tree survived previous period, 1 if died
library(plyr)
bc1 <- read.csv("~/work/data/data/long-bc-derived.csv")
bc <- read.csv("~/work/data/data/hazards/hazards-bc-firs.csv")

##############################################################
## Convert long-bc-derived columns to be like hazards colnames
## time as numeric
bc1$time <- as.numeric(factor(bc1$time))
bc1$time <- bc1$time + 1 ## converted to 2 through 7

## Convert sdp to classes, 1 and 2
bc1$sdp.hazard <- cut(bc1$sdp, breaks = c(0,.995,2))
bc1$sdp.hazard <- as.numeric(factor(bc1$sdp.hazard))

## SI to classes <= 24, > 24 <= 29, > 29
bc1$si.hazard <- cut(bc1$si, breaks = c(0,24,29,50))
bc1$si.hazard <- as.numeric(factor(bc1$si.hazard))


## Add doug firs that died back into the dataset
died <- bc1[bc1$spec == "FD" & bc1$stat == "DEAD",]
bc <- merge(bc, died, all = TRUE)

## create a column to determine if a tree died during a hazard period
##  (0 didn't die, 1 did die)
bc$died <- ifelse (bc$stat == "DEAD", 1, 0)
nrow(subset(bc, time == 5 & stat == "DEAD"))

## overwrite data
write.csv(bc, "~/work/data/data/hazards/hazards-bc-firs.csv", row.names = FALSE)
