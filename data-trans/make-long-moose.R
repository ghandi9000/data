## Transform moose data to long format
source("~/work/functions/functions-datatrans.R")
## source("~/work/data/data-prep/read-moose.R")
## source("~/work/data/data-prep/clean-moose.R")
pp <- read.csv("~/work/data/data/growth/moose-wide.csv")

## columns to transform, years to use
yrs <- c(86, 87, 98, 10)
cols <- c("stat","decm","dbh","bv","ba","ht","cpos","dbhgrowth","bagrowth","bvgrowth",
          "htgrowth","priordbh","priorht","priorbv","priorba")

## find missing columns and make dummy columns with their names
missing <- checkCols(pp, cols, yrs)
pp <- addDummies(pp, missing)

## transform to long
long <- makeLong(pp, cols, times)

## remove rows that have NA for all time periods (DBH and HT)
tst <- removeEmpty(long, ns, yrs)
nrow(subset(long, is.na(dbh) & is.na(ht) & is.na(ba) & is.na(bv)))
