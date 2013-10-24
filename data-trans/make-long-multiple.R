### 1. put targs-sr2.csv in long format
### 2. make dataset with complete cases for dbh and ht, with dbh & ht > 1
###      and log transform both
#get data in long format
targs <- read.csv("targs-sr2.csv")
targs <- subset(targs, PPLOT>3 & ASPCL!="")
names(targs) <- tolower(names(targs))

# make some dummy columns
targs$dbhgrowth86 <- NA
targs$dbhgrowth87 <- NA
targs$bagrowth86 <- NA
targs$bagrowth87 <- NA
targs$htgrowth86 <- NA
targs$htgrowth87 <- NA
targs$cpos86 <- NA
# reshape
long <- reshape(targs, v.names = c("nsumba.1"), direction = "long")
long <- reshape(targs,varying=
                list(
                    c(grep("nsumba.1",names(targs))),
                    c(grep("nsumba.2",names(targs))),
                    c(grep("nsumba.3",names(targs))),
                    c(grep("nsumba5.1",names(targs))),
                    c(grep("nsumba5.2",names(targs))),
                    c(grep("nsumba5.3",names(targs))),
                    c(grep("nsumbabig.1",names(targs))),
                    c(grep("nsumbabig.2",names(targs))),
                    c(grep("nsumbabig.3",names(targs))),
                    c("dbh86","dbh87","dbh98","dbh10"),
                    c("ht86","ht87","ht98","ht10"),
                    c("dbhgrowth86","dbhgrowth87", "dbhgrowth98","dbhgrowth10"),
                    c("bagrowth86","bagrowth87","bagrowth98","bagrowth10"),
                    c("htgrowth86","htgrowth87","htgrowth98","htgrowth10"),
                    c("stat86","stat87","stat98","stat10"),
                    c("cpos86","cpos87","cpos98","cpos10"),
                    c("ch86","ch87","ch98","ch10"),
                    c("ba86","ba87","ba98","ba10")),
                times=c(1986,1987,1998,2010),
                direction = "long",
                v.names=c("nsumba.1","nsumba.2","nsumba.3","nsumba5.1",
                "nsumba5.2","nsumba5.3","nsumbabig.1","nsumbabig.2","nsumbabig.3",
                "dbh","ht", "dbhgrowth","bagrowth","htgrowth","stat","cpos","ch","ba"))

# remove some unnecessary columns
indices <- match( c("pplot","splot","tag","spec","yrmort","elev","elevcl",
                    "aspcl","asp","soilcl","slopcl","bqudx","bqudy"), names(long))
fromhere <- match("meas87",names(long))
toend <- length(names(long))
tokeep <- c(indices, fromhere:toend)
long <- subset(long, select = tokeep)

# remove rows that have no dbh or ht data
long <- subset(long, !is.na(dbh) | !is.na(ht))

# mortality column
long$yrmort <- factor(long$yrmort)
long$mort <- !is.na(long$yrmort)

# write
write.csv(long, "long-sr2.csv")

