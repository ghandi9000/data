## Issues with the moosilauke data
pp <- read.csv("~/work/data/data/growth/moose-wide.csv")
long <- read.csv("~/work/data/data/growth/moose-long.csv")

## moose-wide.csv (and moose-long.csv) missing bole volumes, 412
missingBV <- sum( sapply(yrs, function(x){
    nrow(pp[!is.na(pp[,paste0("dbh", x)]) &
              is.na(pp[,paste0("bv",x)]),]) }) )

