### Make quantile and non-quantile based size classes neighborhood values
#  and ba/ht/dbh/growth values by __PLOT__
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
nbreaks <- 3
plots <- 4:27
long <- subset(long, spec=="ABBA" & stat=="ALIVE")

# order long by pplot
long <- long[order(long$pplot),]
long <- long[complete.cases(long$dbh, long$ht),] # check for missing ht/dbh

tt <- sapply(names(long)[classcols], function(x) {
    newname <- paste0(x, "clq")
    long[,newname] <<- unlist(
        sapply(plots, function(d) {
            sub <- long[long$pplot==d,x]
            as.character(quantclass(sub,nbreaks))
        }))
})

### classes by set class width (as a fraction of maximum value)
tt <- sapply(names(long)[classcols], function(x) {
    newname <- paste0(x, "cl2")
    long[,newname] <<- unlist(
        sapply(plots, function(d) {
            sub <- long[long$pplot==d,x]
            as.character(sizeclasses(sub,nbreaks))
        }))
})

# make sclasses with labels 1:3
tt <- sapply(names(long)[classcols], function(x) {
    newname <- paste0(x, "cl")
    long[,newname] <<- unlist(
        sapply(plots, function(d) {
            sub <- long[long$pplot==d,x]
            as.numeric(sizeclasses(sub,nbreaks))
        }))
})

write.csv(long,"long-ABBA-classes-byplot.csv")
