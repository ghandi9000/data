
### Put data in long format, script follows immediately from read-data, clean-data
pp <- subset(pp, PPLOT>3 & ASPCL!="")
names(pp) <- tolower(names(pp))

# make some dummy columns
pp$dbhgrowth86 <- NA
pp$dbhgrowth87 <- NA
pp$bagrowth86 <- NA
pp$bagrowth87 <- NA
pp$htgrowth86 <- NA
pp$htgrowth87 <- NA
pp$cpos86 <- NA
# reshape
long <- reshape(pp,varying=
                list(c("dbh86","dbh87","dbh98","dbh10"),
                     c("ht86","ht87","ht98","ht10"),
                     c("dbhgrowth86","dbhgrowth87", "dbhgrowth98","dbhgrowth10"),
                     c("bagrowth86","bagrowth87","bagrowth98","bagrowth10"),
                     c("htgrowth86","htgrowth87","htgrowth98","htgrowth10"),
                     c("stat86","stat87","stat98","stat10"),
                     c("cpos86","cpos87","cpos98","cpos10"),
                     c("ch86","ch87","ch98","ch10"),
                     c("ba86","ba87","ba98","ba10"),
                     c("priordbh86","priordbh87","priordbh98","priordbh10"),
                     c("priorba86","priorba87","priorba98","priorba10"),
                     c("priorht86","priorht87","priorht98","priorht10")),
                v.names=c("dbh","ht","dbhgrowth","bagrowth","htgrowth","stat","cpos",
                "ch","ba","priordbh","priorba","priorht"),
                     times=c(1986,1987,1998,2010),
                direction = "long")

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
write.csv(long, "long.csv", row.names=FALSE)


##### Doug Fir data
names(df) <- tolower(names(df))

# add some dummy columns for stand development
dummys <- c(73, 76, 79, 82, 85, 91, 97)
for(yr in dummys) {
    df[, paste0("sdp", yr)] <- rep(NA, nrow(df))
}

df <- df[,sort(names(df))]
dflong <- reshape(df, varying =
                  list(c(grep("^dbh[[:digit:]]",names(df))),
                       c(grep("priordbh",names(df))),
                       c(grep("dbhgrowth",names(df))),
                       c(grep("^ba[[:digit:]]",names(df))),
                       c(grep("priorba",names(df))),
                       c(grep("bagrowth",names(df))),
                       c(grep("^bv[[:digit:]]",names(df))),
                       c(grep("priorbv",names(df))),
                       c(grep("bvgrowth",names(df))),
                       c(grep("^ht[[:digit:]]",names(df))),
                       c(grep("priorht",names(df))),
                       c(grep("htgrowth",names(df))),
                       c(grep("cpos",names(df))),
                       c(grep("stat",names(df))),
                       c(grep("sdp[[:digit:]]",names(df)))),
                  sep = "", times  = c(73,76,79,82,85,91,97),
                  direction = c("long"),
                  v.names = c("dbh","priordbh","dbhgrowth","ba","priorba","bagrowth",
                  "bv","priorbv","bvgrowth","ht","priorht","htgrowth","cpos","stat",
                  "sdp"))

## remove rows that have no dbh or ht data if they arent marked as ALIVE (some saplings
## have no DBH/HT data)
dflong <- subset(dflong, !is.na(dbh) | !is.na(ht) | stat=="ALIVE")

# write
write.csv(dflong, "long-df.csv", row.names=FALSE)
