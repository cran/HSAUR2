###################################################
### chunk number 1: setup
###################################################
#line 182 "Ch_multiple_linear_regression.Rnw"
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
#line 221 "Ch_multiple_linear_regression.Rnw"
book <- FALSE


###################################################
### chunk number 3: MLR-hubble-tab
###################################################
#line 238 "Ch_multiple_linear_regression.Rnw"
data("hubble", package = "gamair")
names(hubble) <- c("galaxy", "velocity", "distance")
toLatex(HSAURtable(hubble, package = "gamair"), pcol = 2,
    caption = paste("Distance and velocity for 24 galaxies."),
    label = "MLR-hubble-tab")


###################################################
### chunk number 4: MLR-clouds-tab
###################################################
#line 251 "Ch_multiple_linear_regression.Rnw"
data("clouds", package = "HSAUR2")
toLatex(HSAURtable(clouds), pcol = 1,
    caption = paste("Cloud seeding experiments in Florida -- see text for",
                    "explanations of the variables."),
    label = "MLR-clouds-tab")


###################################################
### chunk number 5: MLR-hubble-plot
###################################################
#line 313 "Ch_multiple_linear_regression.Rnw"
plot(velocity ~ distance, data = hubble)


###################################################
### chunk number 6: MLR-hubble-beta1
###################################################
#line 333 "Ch_multiple_linear_regression.Rnw"
sum(hubble$distance * hubble$velocity) /
    sum(hubble$distance^2)


###################################################
### chunk number 7: MLR-hubble-lm
###################################################
#line 338 "Ch_multiple_linear_regression.Rnw"
hmod <- lm(velocity ~ distance - 1, data = hubble)


###################################################
### chunk number 8: MLR-hubble-lm
###################################################
#line 343 "Ch_multiple_linear_regression.Rnw"
coef(hmod)


###################################################
### chunk number 9: MLR-hubble-age
###################################################
#line 361 "Ch_multiple_linear_regression.Rnw"
Mpc <- 3.09 * 10^19
ysec <- 60^2 * 24 * 365.25
Mpcyear <- Mpc / ysec
1 / (coef(hmod) / Mpcyear)


###################################################
### chunk number 10: MLR-hubble-lmplot
###################################################
#line 370 "Ch_multiple_linear_regression.Rnw"
layout(matrix(1:2, ncol = 2))
plot(velocity ~ distance, data = hubble)
abline(hmod)
plot(hmod, which = 1)


###################################################
### chunk number 11: MLR-clouds-boxplots
###################################################
#line 389 "Ch_multiple_linear_regression.Rnw"
data("clouds", package = "HSAUR2")
layout(matrix(1:2, nrow = 2))
bxpseeding <- boxplot(rainfall ~ seeding, data = clouds,
    ylab = "Rainfall", xlab = "Seeding")
bxpecho <- boxplot(rainfall ~ echomotion, data = clouds,
    ylab = "Rainfall", xlab = "Echo Motion")


###################################################
### chunk number 12: MLR-clouds-scatterplots
###################################################
#line 402 "Ch_multiple_linear_regression.Rnw"
layout(matrix(1:4, nrow = 2))
plot(rainfall ~ time, data = clouds)
plot(rainfall ~ cloudcover, data = clouds)
plot(rainfall ~ sne, data = clouds, xlab="S-Ne criterion")
plot(rainfall ~ prewetness, data = clouds)


###################################################
### chunk number 13: MLR-clouds-outliers
###################################################
#line 417 "Ch_multiple_linear_regression.Rnw"
rownames(clouds)[clouds$rainfall %in% c(bxpseeding$out,
                                        bxpecho$out)]


###################################################
### chunk number 14: MLR-clouds-formula
###################################################
#line 432 "Ch_multiple_linear_regression.Rnw"
clouds_formula <- rainfall ~ seeding +
    seeding:(sne + cloudcover + prewetness + echomotion) +
    time


###################################################
### chunk number 15: MLR-clouds-modelmatrix
###################################################
#line 438 "Ch_multiple_linear_regression.Rnw"
Xstar <- model.matrix(clouds_formula, data = clouds)


###################################################
### chunk number 16: MLR-clouds-contrasts
###################################################
#line 444 "Ch_multiple_linear_regression.Rnw"
attr(Xstar, "contrasts")


###################################################
### chunk number 17: MLR-clouds-lm
###################################################
#line 454 "Ch_multiple_linear_regression.Rnw"
clouds_lm <- lm(clouds_formula, data = clouds)
class(clouds_lm)


###################################################
### chunk number 18: MLR-clouds-summary
###################################################
#line 467 "Ch_multiple_linear_regression.Rnw"
summary(clouds_lm)


###################################################
### chunk number 19: MLR-clouds-coef
###################################################
#line 473 "Ch_multiple_linear_regression.Rnw"
betastar <- coef(clouds_lm)
betastar


###################################################
### chunk number 20: MLR-clouds-vcov
###################################################
#line 479 "Ch_multiple_linear_regression.Rnw"
Vbetastar <- vcov(clouds_lm)


###################################################
### chunk number 21: MLR-clouds-sd
###################################################
#line 484 "Ch_multiple_linear_regression.Rnw"
sqrt(diag(Vbetastar))


###################################################
### chunk number 22: MLR-clouds-lmplot
###################################################
#line 489 "Ch_multiple_linear_regression.Rnw"
psymb <- as.numeric(clouds$seeding)
plot(rainfall ~ sne, data = clouds, pch = psymb,
     xlab = "S-Ne criterion")
abline(lm(rainfall ~ sne, data = clouds,
          subset = seeding == "no"))
abline(lm(rainfall ~ sne, data = clouds,
          subset = seeding == "yes"), lty = 2)
legend("topright", legend = c("No seeding", "Seeding"),
       pch = 1:2, lty = 1:2, bty = "n")


###################################################
### chunk number 23: MLR-clouds-residfitted
###################################################
#line 508 "Ch_multiple_linear_regression.Rnw"
clouds_resid <- residuals(clouds_lm)
clouds_fitted <- fitted(clouds_lm)


###################################################
### chunk number 24: MLR-clouds-residplot
###################################################
#line 522 "Ch_multiple_linear_regression.Rnw"
plot(clouds_fitted, clouds_resid, xlab = "Fitted values",
     ylab = "Residuals", type = "n",
     ylim = max(abs(clouds_resid)) * c(-1, 1))
abline(h = 0, lty = 2)
text(clouds_fitted, clouds_resid, labels = rownames(clouds))


###################################################
### chunk number 25: MLR-clouds-qqplot
###################################################
#line 535 "Ch_multiple_linear_regression.Rnw"
qqnorm(clouds_resid, ylab = "Residuals")
qqline(clouds_resid)


###################################################
### chunk number 26: MLR-clouds-cook eval=FALSE
###################################################
## #line 550 "Ch_multiple_linear_regression.Rnw"
## plot(clouds_lm)


###################################################
### chunk number 27: MLR-clouds-cook
###################################################
#line 553 "Ch_multiple_linear_regression.Rnw"
plot(clouds_lm, which = 4, sub.caption = NULL)


