## Preparatory moosilauke data work
## NOTE: using estimated heights and bole volumes from boles project
## "~/work/boles/"
source("~/work/data/data-prep/read-moose.R") ## calls data 'pp'
source("~/work/functions/functions-datatrans.R")

## Check to see that we have the necessary columns
cols <- c("DBH","HT","BV")
yrs <- c(86, 87, 98, 10)
checkCols(pp, cols, yrs) ## returns columns that arent present

## rename columns that have been changed
changed <- c("ebv","EHTTCR")
replacements <- c("BV","HT")
for (i in 1:length(changed))
    names(pp) <- gsub(changed[i], replacements[i], names(pp))
checkCols(pp, cols, yrs) ## recheck cols

## remake the ba and growth columns so they are annualized and named
## with yr suffixes for long transformation
pp$BA86 <- 0.00007854*pp$DBH86*pp$DBH86 ### Basal area column, m^2
pp$BA87 <- 0.00007854*pp$DBH87*pp$DBH87
pp$BA98 <- 0.00007854*pp$DBH98*pp$DBH98
pp$BA10 <- 0.00007854*pp$DBH10*pp$DBH10

pp$BAGROWTH98 <- rep(NA, nrow(pp))
pp[!is.na(pp$BA86),]$BAGROWTH98 <- (pp[!is.na(pp$BA86),]$BA98-
                                    pp[!is.na(pp$BA86),]$BA86)/12
pp[!is.na(pp$BA87),]$BAGROWTH98 <- (pp[!is.na(pp$BA87),]$BA98-
                                    pp[!is.na(pp$BA87),]$BA87)/11
pp$BAGROWTH10 <- (pp$BA10-pp$BA98)/12

pp$BVGROWTH98 <- rep(NA, nrow(pp))
pp[!is.na(pp$BV86),]$BVGROWTH98 <- (pp[!is.na(pp$BV86),]$BV98-
                                    pp[!is.na(pp$BV86),]$BV86)/12
pp[!is.na(pp$BV87),]$BVGROWTH98 <- (pp[!is.na(pp$BV87),]$BV98-
                                    pp[!is.na(pp$BV87),]$BV87)/11
pp$BVGROWTH10 <- (pp$BV10-pp$BV98)/12

pp$DBHGROWTH98 <- rep(NA, nrow(pp))
pp[!is.na(pp$DBH86),]$DBHGROWTH98 <- (pp[!is.na(pp$DBH86),]$DBH98-
                                    pp[!is.na(pp$DBH86),]$DBH86)/12
pp[!is.na(pp$DBH87),]$DBHGROWTH98 <- (pp[!is.na(pp$DBH87),]$DBH98-
                                    pp[!is.na(pp$DBH87),]$DBH87)/11
pp$DBHGROWTH10 <-(pp$DBH10-pp$DBH98)/12

pp$HTGROWTH98 <- rep(NA, nrow(pp))
pp[!is.na(pp$HT86),]$HTGROWTH98 <- (pp[!is.na(pp$HT86),]$HT98-
                                    pp[!is.na(pp$HT86),]$HT86)/12
pp[!is.na(pp$HT87),]$HTGROWTH98 <- (pp[!is.na(pp$HT87),]$HT98-
                                    pp[!is.na(pp$HT87),]$HT87)/11
pp$HTGROWTH10 <-(pp$HT10-pp$HT98)/12

# Make prior columns for ht, dbh, ba, bv
pp$PRIORDBH86 <- rep(NA, nrow(pp))
pp$PRIORDBH87 <- rep(NA, nrow(pp))
pp$PRIORDBH98 <- rep(NA, nrow(pp))
pp$PRIORDBH10 <- rep(NA, nrow(pp))
pp[!is.na(pp$DBH86),]$PRIORDBH98 <- pp[!is.na(pp$DBH86),]$DBH86
pp[!is.na(pp$DBH87),]$PRIORDBH98 <- pp[!is.na(pp$DBH87),]$DBH87
pp[!is.na(pp$DBH98),]$PRIORDBH10 <- pp[!is.na(pp$DBH98),]$DBH98

pp$PRIORBA86 <- rep(NA, nrow(pp))
pp$PRIORBA87 <- rep(NA, nrow(pp))
pp$PRIORBA98 <- rep(NA, nrow(pp))
pp$PRIORBA10 <- rep(NA, nrow(pp))
pp[!is.na(pp$BA86),]$PRIORBA98 <- pp[!is.na(pp$BA86),]$BA86
pp[!is.na(pp$BA87),]$PRIORBA98 <- pp[!is.na(pp$BA87),]$BA87
pp[!is.na(pp$BA98),]$PRIORBA10 <- pp[!is.na(pp$BA98),]$BA98

pp$PRIORBV86 <- rep(NA, nrow(pp))
pp$PRIORBV87 <- rep(NA, nrow(pp))
pp$PRIORBV98 <- rep(NA, nrow(pp))
pp$PRIORBV10 <- rep(NA, nrow(pp))
pp[!is.na(pp$BV86),]$PRIORBV98 <- pp[!is.na(pp$BV86),]$BV86
pp[!is.na(pp$BV87),]$PRIORBV98 <- pp[!is.na(pp$BV87),]$BV87
pp[!is.na(pp$BV98),]$PRIORBV10 <- pp[!is.na(pp$BV98),]$BV98

pp$PRIORHT86 <- rep(NA, nrow(pp))
pp$PRIORHT87 <- rep(NA, nrow(pp))
pp$PRIORHT98 <- rep(NA, nrow(pp))
pp$PRIORHT10 <- rep(NA, nrow(pp))
pp[!is.na(pp$HT86),]$PRIORHT98 <- pp[!is.na(pp$HT86),]$HT86
pp[!is.na(pp$HT87),]$PRIORHT98 <- pp[!is.na(pp$HT87),]$HT87
pp[!is.na(pp$HT98),]$PRIORHT10 <- pp[!is.na(pp$HT98),]$HT98

## make columns that identify direction for dbh and ht from last sampling period
## period 1
## pp$p98dbh <- rep(NA, nrow(pp))
## pp[pp$PPLOT<16,]$p98dbh <- pp[pp$PPLOT<16,]$DBH98 > pp[pp$PPLOT<16,]$DBH86
## pp[pp$PPLOT>=16,]$p98dbh <- pp[pp$PPLOT>=16,]$DBH98 > pp[pp$PPLOT>=16,]$DBH87
## pp$p98ht <- rep(NA, nrow(pp))
## pp[pp$PPLOT<16,]$p98ht <- pp[pp$PPLOT<16,]$HT98 > pp[pp$PPLOT<16,]$HT86
## pp[pp$PPLOT>=16,]$p98ht <- pp[pp$PPLOT>=16,]$HT98 > pp[pp$PPLOT>=16,]$HT87
## # period 2
## pp$p10dbh <- pp$DBH10 > pp$DBH98
## pp$p10ht <- pp$HT10 > pp$HT98

## colnames to lower case and drop unwanted columns
names(pp) <-tolower(names(pp))
yrs <- c(86, 87, 98, 10)
toKeep <- c("pplot","splot","tag","spec","yrmort","elev","elevcl","asp","aspcl",
            "bqudx","bqudy","soilcl","slopcl",
            paste0("stat",yrs), paste0("dbh",yrs), paste0("bv",yrs), paste0("ba",yrs),
            paste0("ht",yrs), paste0("decm",yrs), paste0("cpos",yrs),
            paste0("dbhgrowth",yrs), paste0("htgrowth",yrs), paste0("priordbh",yrs),
            paste0("priorba",yrs), paste0("priorht",yrs), paste0("priorbv",yrs),
            paste0("bvgrowth",yrs))

pp <- pp[,names(pp) %in% toKeep]
write.csv(pp, "~/work/data/data/growth/moose-wide.csv", row.names = FALSE)
