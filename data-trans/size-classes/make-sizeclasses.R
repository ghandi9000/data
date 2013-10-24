### Make quantile and non-quantile based size classes neighborhood values
#  and ba/ht/dbh/growth values
#source("make-long.R")
long <- read.csv("long-sr2.csv")
### make size classes for all neighborhood values, dbh, ht, and ba
#
# columns to make classes from:
base3 <- which(names(long)=="dbh" | names(long)=="ht" | names(long)=="ba")
pattern <- "^n|growth"
classcols <- c(grep(pattern, names(long)))

### QUANTILE-based size classes, excluding trees with 0 dbh/ht/ba
# exclude 0 from classes for base3 (ht, dbh, ba)
# choose number of breaks
nbreaks <- 8
for(i in names(long)[base3]) {
    thiscol <- long[,i]
    newname <- paste(i,"clq",sep="")
    new <- quantclass(thiscol, numbreaks = nbreaks, smallest = 0.0001)
    long[,newname] <- new
}
# include 0 values from neighborhood columns
for(i in names(long)[classcols]) {
    thiscol <- long[,i]
    newname <- paste(i,"clq",sep="")
    new <- quantclass(thiscol, numbreaks = nbreaks)
    long[,newname] <- new
}

### Variable-based size classes, uneven sampling, exlude 0 from base3
for(i in names(long)[base3]) {
    thiscol <- long[,i]
    newname <- paste(i,"cl",sep="")
    new <- sizeclasses(thiscol, width=max(thiscol,na.rm = TRUE)/nbreaks,
                       nozero = TRUE)
    long[,newname] <- new
}

# include 0 for values from neighborhood columns
for(i in names(long)[classcols]) {
    thiscol <- long[,i]
    newname <- paste(i,"cl",sep="")
    new <- sizeclasses(thiscol, width=max(thiscol,na.rm = TRUE)/nbreaks,
                       nozero = FALSE)
    long[,newname] <- new
}

# overwrite previous version
write.csv(long,"long-sr2.csv")

## complete cases with dbh & ht > 1 for log-transformed poly fits
comp <- with(long, long[complete.cases(dbh) & complete.cases(ht) & dbh > 1 & ht > 1,])
write.csv(comp,"long-complete-cases-sr2.csv")
