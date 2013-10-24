### Make neighborhoods with nsumbabig, nsumba5 for sr1, sr2 and sr3
# ie 1 quad, 9 quadrat, 16 quadrat.  Do neighborhoods for all years
# subsequent sr values will only include the summed neighborhood from the additional
#  area added to the neighborhood, ie sr2 will be (total sr2 - sr1) and
#  sr3 will be (total sr3 - (sr2 + sr1))

# psuedocode:
# 3 neighborhood sizes, 3 neighborhood methods, 4 years
#   -- vectorized version :: lapply(srs, lapply(yrs,
#       do neighborhood calculations, assign values to new column))
#   -- alternative: use loops: for(i in yrs) { for(j in neb sizes) { for(k in neb
#        methods) {}}}
pp <- subset(pp, CLASS=="P" & PPLOT > 3)

# neighborhood sizes (in square radii), yrs
srs <- c(1,2,3)
yrs <- c(86,87,98,10)

# make different neighborhood columns for each combination of yr, sr, and
#  neighborhood summing method
#  current runtime: ~ 16 minutes.  Could be faster if neigborhood summing operations
#   were condensed into one apply function
stime <- Sys.time()
makecolumuns <- lapply(srs, function(a) {
    lapply(yrs, function(b) {
        cols.yr <- c(paste0("BA",b),paste0("STAT",b),paste0("DBH",b))
                                        # define targets
        targs <- with(pp, which(BQUDX < (12-a) & BQUDX > (-1+a) &
                                BQUDY > (-1+a) & BQUDY < (12-a) &
                                pp[,cols.yr[2]]=="ALIVE"))
                                        # define neighbors total set of neighbors
        neighbors <- pp[pp[,cols.yr[2]]=="ALIVE",]
                                        # Neighborhood method 1: total neighbor BA
        pp[,paste("NSUMBA",a,b,sep=".")] <<- rep(NA, nrow(pp))
        pp[targs,paste("NSUMBA",a,b,sep=".")] <<-
            apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
                                        # define neighbors for individuals
                nebs <- with(neighbors,
                             neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
                                       x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
                                       x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
                                        # Sum neighbor BA, 0 if no neighbors
                ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
                           0,sum(nebs[,cols.yr[1]],na.rm=T)))
            })
                                        # Neighborhood method 2: sum neighbor BA > 5 dbh
        pp[,paste("NSUMBA5",a,b,sep=".")] <<- rep(NA, nrow(pp))
        pp[targs,paste("NSUMBA5",a,b,sep=".")] <<-
            apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG")], 1, function(x) {
                                        # define neighbors for individuals
                nebs <- with(neighbors,
                             neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
                                       x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
                                       get(cols.yr[3])>=5 & x[["BQUDY"]]>BQUDY-a &
                                       x[["TAG"]]!=TAG,])
                                        # Sum neighbor BA, 0 if no neighbors
                ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
                           0,sum(nebs[,cols.yr[1]],na.rm=T)))
            })
                                        # Neighborhood method 3: sum neighbor BA > focal
        pp[,paste("NSUMBABIG",a,b,sep=".")] <<- rep(NA, nrow(pp))
        pp[targs,paste("NSUMBABIG",a,b,sep=".")] <<-
            apply(pp[targs, c("PPLOT","BQUDX","BQUDY","TAG",cols.yr[3])], 1, function(x) {
                                        # define neighbors for individuals
                nebs <- with(neighbors,
                             neighbors[PPLOT==x[["PPLOT"]] & x[["BQUDX"]]<BQUDX+a &
                                       x[["BQUDX"]]>BQUDX-a & x[["BQUDY"]]<BQUDY+a &
                                       get(cols.yr[3])>=x[cols.yr[3]] &
                                       x[["BQUDY"]]>BQUDY-a & x[["TAG"]]!=TAG,])
                                        # Sum neighbor BA, 0 if no neighbors
                ifelse(nrow(nebs)==0, 0, ifelse(sum(nebs[,cols.yr[1]], na.rm=T)<=0,
                           0,sum(nebs[,cols.yr[1]],na.rm=T)))
            })
    })
})
etime <- Sys.time()
etime-stime

## check values
# if alive with dbh, should have DBH based n-value>=0
targs <- with(pp, pp[which(BQUDX < (11) & BQUDX > (0) &
                           BQUDY > (0) & BQUDY < (11)),])
lapply(subset(targs, !is.na(DBH98) & STAT98=="ALIVE",
              select = names(targs)[
              grep("^N[[:print:]]*98$",names(targs))]),
       function(x) length(x[is.na(x)]))

nrow(subset(targs, !is.na(DBH98) & !is.na(NSUMBABIG.3.98)))

## write
write.csv(pp, 'targs-sr2.csv')
