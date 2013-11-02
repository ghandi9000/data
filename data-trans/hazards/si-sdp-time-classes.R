## Convert time periods to numeric
bc <- read.csv("~/work/data/data/hazards/hazards-bc-firs.csv")
before <- table(bc$time)
bc$time <- as.numeric(factor(bc$time))
bc$time <- bc$time + 1 ## converted to 2 through 7

## Convert sdp to classes, 1 and 2
bc$sdp.hazard <- cut(bc$sdp, breaks = c(0,.995,2))
bc$sdp.hazard <- as.numeric(factor(bc$sdp.hazard))

## SI to classes <= 24, > 24 <= 29, > 29
bc$si.hazard <- cut(bc$si, breaks = c(0,24,29,50))
bc$si.hazard <- as.numeric(factor(bc$si.hazard))

## store hazards dataset
remove <- c("sdp1.hazard")
bc <- bc[,!names(bc) %in% remove]
write.csv(bc, "~/work/data/data/hazards/hazards-bc-firs.csv", row.names=FALSE)

## create a column to determine death / hazard period (0 didn't die, 1 did die)
bc$died <- rep(0, nrow(bc))
yrs <- c(85, 91, 97)
periods <- c(4,5,6)
for (i in 1:length(yrs)) {
    print(periods[i])
    col <- paste0("died",yrs[i])
    bc[bc$time == periods[i],"died"] <- bc[bc$time == periods[i], col]
}

nrow(bc[bc$time == periods[i],]$died)
