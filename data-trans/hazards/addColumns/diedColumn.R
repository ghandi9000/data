## Add trees that died back into hazards dataset and create a died column:
## 0 if a tree survived previous period, 1 if died
library(plyr)
bc1 <- read.csv("~/work/data/data/long-bc-derived.csv")
bc <- read.csv("~/work/data/data/hazards/hazards-bc-firs.csv")

## Add doug firs that died back into the dataset
died <- bc1[bc1$spec == "FD" & bc1$stat == "DEAD",]
tst <- merge(bc, died, all = TRUE)

## create a column to determine if a tree died during a hazard period
##  (0 didn't die, 1 did die)
bc$died <- rep(0, nrow(bc))
yrs <- c(85, 91, 97)
periods <- c(4,5,6)
for (i in 1:length(yrs)) {
    print(periods[i])
    col <- paste0("died",yrs[i])
    bc[bc$time == periods[i],"died"] <- bc[bc$time == periods[i], col]
}

nrow(bc[bc$time == periods[i],]$died)
