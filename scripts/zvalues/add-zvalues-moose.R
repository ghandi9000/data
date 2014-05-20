## Add z-values for all trees on Moosilauke
## Origin is at the center of the plot
dat <- read.csv("~/work/data/data/moose-wide.csv")
dat$slope <- 2*pi/360 * dat$slope8687 # convert slope to radians
dat$theta <- 2*pi/360 * dat$asp # convert aspect to radians

##########################################################################
##
## test data, data from plot 4, quadrats > 0 and <= 10 (removing outliers)
##
tst <- dat[dat$pplot == 4 & dat$bqudy > 0 & dat$bqudy <= 10 &
           !is.na(dat$bqudy) & !is.na(dat$bqudx) & dat$bqudx > 0 &
           dat$bqudx <= 10, ]
newx <- newy <- seq(-4.5, 4.5, 1)
xx <- tst[!is.na(tst$bqudx) & tst$bqudx > 0 & tst$bqudx <= 10, ]$bqudx
yy <- tst[!is.na(tst$bqudy) & tst$bqudy > 0 & tst$bqudy <= 10, ]$bqudy
x <- newx[xx]
y <- newy[yy]

## Check its the same as original
image(as.matrix(table(x,y)), main = "transformed")
windows()
image(as.matrix(table(tst$bqudx, tst$bqudy)), main = "original")
table(y, tst$bqudy)
table(x, tst$bqudx)

##########################################################################
##
##  compute z-values, |ab|*sin(theta)
##  where a is center or plot, b is a plant, theta is the slope of plot
##
zvals <-
