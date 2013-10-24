# Poke around the doug fir data
#  Available: status ("ALIVE","DEAD"),
#             DBH (7 samplings)
#             HT (7 samplings)
#             Crown position ("c","d","i","o")
#             X,Y coordinates ("X,Y")
#             SLOPE (2-48)
#             ELEV (90 - 460)
#             ASPECT (0-360)
#             PLOTAREA (mostly 0.05, but some 0.07 and 0.1)
#             INSTALL is site index: there are multiple plots per site
# What does SI mean?
str(df)

# samples/plot:  sample sizes vary widely, 16 plots
table(df$PLOT)
ggplot(df, aes(PLOT)) + geom_histogram()

# examine spatial: exlude points outside of (-11, 11) range
plot(df$X, df$Y)
abline(v = c(11,-11), h = c(11,-11))

table(df$SI)
table(df$SI, df$PLOT)
table(df$INSTALL, df$PLOT)
table(df$INSTALL, df$SI)

# testing model fits to doug fir
dflong <- read.csv("long-df.csv")

models <- c("slnm","sldnm","spm","spnm")
spec <- "FD"
sr <- 4
ind.var <- priorba
dep.var <- bagrowth

dffits <- fit.MLE.models(dflong, sr, spec, ind.var, dep.var,
                         models = c("spm"), method="Nelder-Mead",
                         maxit = 10000, savefits = "dfworkspace.Rda", realdist = TRUE)


