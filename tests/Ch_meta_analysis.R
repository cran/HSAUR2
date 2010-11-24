###################################################
### chunk number 1: setup
###################################################
#line 183 "Ch_meta_analysis.Rnw"
rm(list = ls())
if (!file.exists("tables")) dir.create("tables")
if (!file.exists("figures")) dir.create("figures")
set.seed(290875)
options(prompt = "R> ", continue = "+  ",
    width = 63, # digits = 4,
    show.signif.stars = FALSE,
    SweaveHooks = list(leftpar = function()
        par(mai = par("mai") * c(1, 1.05, 1, 1)),
        bigleftpar = function()
        par(mai = par("mai") * c(1, 1.7, 1, 1))))
HSAURpkg <- require("HSAUR2")
if (!HSAURpkg) stop("cannot load package ", sQuote("HSAUR2"))
rm(HSAURpkg)
 ### </FIXME> hm, R-2.4.0 --vanilla seems to need this
a <- Sys.setlocale("LC_ALL", "C")
 ### </FIXME>
book <- TRUE
refs <- cbind(c("AItR", "DAGD", "SI", "CI", "ANOVA", "MLR", "GLM",
                "DE", "RP", "GAM", "SA", "ALDI", "ALDII", "SIMC", "MA", "PCA",
                "MDS", "CA"), 1:18)
ch <- function(x) {
    ch <- refs[which(refs[,1] == x),]
    if (book) {
        return(paste("Chapter~\\\\ref{", ch[1], "}", sep = ""))
    } else {
        return(paste("Chapter~", ch[2], sep = ""))
    }
}
if (file.exists("deparse.R"))
    source("deparse.R")
setHook(packageEvent("lattice", "attach"), function(...) {
    lattice.options(default.theme =
        function()
            standard.theme("pdf", color = FALSE))
    })


###################################################
### chunk number 2: singlebook
###################################################
#line 222 "Ch_meta_analysis.Rnw"
book <- FALSE


###################################################
### chunk number 3: MA-smoking-OR-hand
###################################################
#line 236 "Ch_meta_analysis.Rnw"
data("smoking", package = "HSAUR2")
odds <- function(x) (x[1] * (x[4] - x[3])) /
                    ((x[2] - x[1]) * x[3])
weight <- function(x) ((x[2] - x[1]) * x[3]) / sum(x)
W <- apply(smoking, 1, weight)
Y <- apply(smoking, 1, odds)
sum(W * Y) / sum(W)


###################################################
### chunk number 4: MA-smoking-OR
###################################################
#line 250 "Ch_meta_analysis.Rnw"
library("rmeta")
smokingOR <- meta.MH(smoking[["tt"]], smoking[["tc"]],
                     smoking[["qt"]], smoking[["qc"]],
                     names = rownames(smoking))


###################################################
### chunk number 5: MA-smoking-OR-summary
###################################################
#line 262 "Ch_meta_analysis.Rnw"
summary(smokingOR)


###################################################
### chunk number 6: MA-smoking-OR-plot
###################################################
#line 268 "Ch_meta_analysis.Rnw"
plot(smokingOR, ylab = "")


###################################################
### chunk number 7: MA-smoking-random
###################################################
#line 285 "Ch_meta_analysis.Rnw"
smokingDSL <- meta.DSL(smoking[["tt"]], smoking[["tc"]],
                     smoking[["qt"]], smoking[["qc"]],
                     names = rownames(smoking))
print(smokingDSL)


###################################################
### chunk number 8: MA-BCG-odds
###################################################
#line 316 "Ch_meta_analysis.Rnw"
data("BCG", package = "HSAUR2")
BCG_OR <- meta.MH(BCG[["BCGVacc"]], BCG[["NoVacc"]],
                  BCG[["BCGTB"]], BCG[["NoVaccTB"]],
                  names = BCG$Study)
BCG_DSL <- meta.DSL(BCG[["BCGVacc"]], BCG[["NoVacc"]],
                  BCG[["BCGTB"]], BCG[["NoVaccTB"]],
                  names = BCG$Study)


###################################################
### chunk number 9: MA-BCGOR-summary
###################################################
#line 331 "Ch_meta_analysis.Rnw"
summary(BCG_OR)


###################################################
### chunk number 10: MA-BCGDSL-summary
###################################################
#line 339 "Ch_meta_analysis.Rnw"
summary(BCG_DSL)


###################################################
### chunk number 11: BCG-studyweights
###################################################
#line 352 "Ch_meta_analysis.Rnw"
studyweights <- 1 / (BCG_DSL$tau2 + BCG_DSL$selogs^2)
y <- BCG_DSL$logs
BCG_mod <- lm(y ~ Latitude + Year, data = BCG,
              weights = studyweights)


###################################################
### chunk number 12: MA-mod-summary
###################################################
#line 369 "Ch_meta_analysis.Rnw"
summary(BCG_mod)


###################################################
### chunk number 13: BCG-Latitude-plot
###################################################
#line 375 "Ch_meta_analysis.Rnw"
plot(y ~ Latitude, data = BCG, ylab = "Estimated log-OR")
abline(lm(y ~ Latitude, data = BCG, weights = studyweights))


###################################################
### chunk number 14: MA-funnel-ex
###################################################
#line 387 "Ch_meta_analysis.Rnw"
set.seed(290875)
sigma <- seq(from = 1/10, to = 1, length.out = 35)
y <- rnorm(35) * sigma
gr <- (y > -0.5)
layout(matrix(1:2, ncol = 1))
plot(y, 1/sigma, xlab = "Effect size", ylab = "1 / standard error")
plot(y[gr], 1/(sigma[gr]), xlim = range(y),
     xlab = "Effect size", ylab = "1 / standard error")


###################################################
### chunk number 15: MA-smoking-funnel
###################################################
#line 407 "Ch_meta_analysis.Rnw"
funnelplot(smokingDSL$logs, smokingDSL$selogs,
           summ = smokingDSL$logDSL, xlim = c(-1.7, 1.7))
abline(v = 0, lty = 2)


