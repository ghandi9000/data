## Convert time periods to numeric
bc1 <- read.csv("~/work/data/data/long-bc-derived.csv")
bc2 <- subset(bc1, spec == "FD")
bc <- read.csv("~/work/data/data/hazards/hazards-bc-firs.csv")
## before <- table(bc$time)

## time as numeric
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

## write data
write.csv(bc, "~/work/data/data/hazards/hazards-bc-firs.csv", row.names=FALSE)
