### Make quantile and non-quantile based size classes neighborhood values
#  by __ELEVCL__
#source("make-long.R")
long <- read.csv("long-sr2.csv")

### make size classes for all neighborhood values, dbh, ht, and ba
# columns to make classes from:
base3 <- which(names(long)=="dbh" | names(long)=="ht" | names(long)=="ba")

# drop previously made class columns
drops <- "^n[[:alpha:]]*[[:digit:]]*cl$|clq$|growthcl"
todrop <- grep(drops, names(long))
long <- subset(long, select = !names(long)%in%names(long)[todrop])

pattern <- "^n|growth"
classcols <- grep(pattern, names(long))
classcols <- classcols[!classcols %in%
                       c(grep("98",names(long)), grep("htgrowth", names(long)))]

### QUANTILE-based size classes, excluding trees with 0 dbh/ht/ba
# exclude 0 from classes for base3 (ht, dbh, ba)
# choose number of breaks, species, plots
nbreaks <- 2
plots <- 4:27
elevs <- c("L","M","H")
long <- subset(long, spec=="ABBA" & stat=="ALIVE")

# order long by elev
long <- long[order(long$elevcl),]
long <- long[complete.cases(long$dbh, long$ht),] # check for missing ht/dbh

tt <- sapply(names(long)[classcols], function(x) {
    newname <- paste0(x, "clq2")
    long[,newname] <<- unlist(
        sapply(elevs, function(d) {
            sub <- long[long$elevcl==d,x]
            as.character(quantclass(sub,nbreaks))
        }))
})

# make sclasses with labels 1:3
tt <- sapply(names(long)[classcols], function(x) {
    newname <- paste0(x, "clq")
    long[,newname] <<- unlist(
        sapply(elevs, function(d) {
            sub <- long[long$elevcl==d,x]
            as.numeric(quantclass(sub,nbreaks))
        }))
})

# write
write.csv(long,"long-ABBA-classes-byelev.csv")
