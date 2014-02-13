## Add canopy heights to moose-long.csv

canhts <- read.csv("~/work/data/data/boles/canhts.csv")
pp <- read.csv("~/work/data/data/moose-long.csv")

## Add canhts to dataframe
names(canhts)[2:length(names(canhts))] <-
    gsub("[[:alpha:]]", "", names(canhts)[2:length(names(canhts))])
pp$canht <- apply(pp, 1, function(x) {
    row <- match(as.numeric(x[["pplot"]]), canhts[,1])
    col <- 1 + match(as.numeric(x[["time"]]),
                 as.numeric(names(canhts)[2:length(names(canhts))]))
    if (is.na(row) || is.na(col)) {
        NA
    } else
        canhts[row,col]
})

## change year 10 to 100 to keep linearity in time
pp[pp$time == 10,]$time <- 100

write.csv(pp, "~/work/data/data/dynamicallometry/moose-long-canopies.csv", row.names = FALSE)

