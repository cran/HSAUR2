###################################################
### chunk number 1: setup
###################################################
#line 183 "Ch_survival_analysis.Rnw"
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
#line 222 "Ch_survival_analysis.Rnw"
book <- FALSE


###################################################
### chunk number 3: SA-setup
###################################################
#line 225 "Ch_survival_analysis.Rnw"
x <- library("survival")
x <- library("coin")
x <- library("party")


###################################################
### chunk number 4: SA-glioma-KM
###################################################
#line 237 "Ch_survival_analysis.Rnw"
data("glioma", package = "coin")
library("survival")
layout(matrix(1:2, ncol = 2))
g3 <- subset(glioma, histology == "Grade3")
plot(survfit(Surv(time, event) ~ group, data = g3),
     main = "Grade III Glioma", lty = c(2, 1),
     ylab = "Probability", xlab = "Survival Time in Month",
     legend.text = c("Control", "Treated"),
     legend.bty = "n")
g4 <- subset(glioma, histology == "GBM")
plot(survfit(Surv(time, event) ~ group, data = g4),
     main = "Grade IV Glioma", ylab = "Probability",
     lty = c(2, 1), xlab = "Survival Time in Month",
     xlim = c(0, max(glioma$time) * 1.05))


###################################################
### chunk number 5: SA-glioma-logrank
###################################################
#line 262 "Ch_survival_analysis.Rnw"
survdiff(Surv(time, event) ~ group, data = g3)


###################################################
### chunk number 6: SA-glioma-exact
###################################################
#line 276 "Ch_survival_analysis.Rnw"
library("coin")
surv_test(Surv(time, event) ~ group, data = g3,
          distribution = "exact")


###################################################
### chunk number 7: SA-glioma-g4
###################################################
#line 283 "Ch_survival_analysis.Rnw"
surv_test(Surv(time, event) ~ group, data = g4,
          distribution = "exact")


###################################################
### chunk number 8: SA-glioma-hist
###################################################
#line 291 "Ch_survival_analysis.Rnw"
surv_test(Surv(time, event) ~ group | histology,
    data = glioma, distribution = approximate(B = 10000))


###################################################
### chunk number 9: SA-GBSG2-plot
###################################################
#line 305 "Ch_survival_analysis.Rnw"
data("GBSG2", package = "ipred")
plot(survfit(Surv(time, cens) ~ horTh, data = GBSG2),
     lty = 1:2, mark.time = FALSE, ylab = "Probability",
     xlab = "Survival Time in Days")
legend(250, 0.2, legend = c("yes", "no"), lty = c(2, 1),
       title = "Hormonal Therapy", bty = "n")


###################################################
### chunk number 10: SA-GBSG2-coxph
###################################################
#line 321 "Ch_survival_analysis.Rnw"
GBSG2_coxph <- coxph(Surv(time, cens) ~ ., data = GBSG2)


###################################################
### chunk number 11: SA-GBSG2-coxph-ci
###################################################
#line 329 "Ch_survival_analysis.Rnw"
ci <- confint(GBSG2_coxph)
exp(cbind(coef(GBSG2_coxph), ci))["horThyes",]


###################################################
### chunk number 12: GBSG2-coxph-summary
###################################################
#line 340 "Ch_survival_analysis.Rnw"
summary(GBSG2_coxph)


###################################################
### chunk number 13: SA-GBSG2-zph
###################################################
#line 353 "Ch_survival_analysis.Rnw"
GBSG2_zph <- cox.zph(GBSG2_coxph)
GBSG2_zph


###################################################
### chunk number 14: SA-GBSG2-zph-plot
###################################################
#line 366 "Ch_survival_analysis.Rnw"
plot(GBSG2_zph, var = "age")


###################################################
### chunk number 15: SA-GBSG2-Martingal
###################################################
#line 375 "Ch_survival_analysis.Rnw"
layout(matrix(1:3, ncol = 3))
res <- residuals(GBSG2_coxph)
plot(res ~ age, data = GBSG2, ylim = c(-2.5, 1.5),
     pch = ".", ylab = "Martingale Residuals")
abline(h = 0, lty = 3)
plot(res ~ pnodes, data = GBSG2, ylim = c(-2.5, 1.5),
     pch = ".", ylab = "")
abline(h = 0, lty = 3)
plot(res ~ log(progrec), data = GBSG2, ylim = c(-2.5, 1.5),
     pch = ".", ylab = "")
abline(h = 0, lty = 3)


###################################################
### chunk number 16: SA-GBSG2-ctree
###################################################
#line 401 "Ch_survival_analysis.Rnw"
GBSG2_ctree <- ctree(Surv(time, cens) ~ ., data = GBSG2)


###################################################
### chunk number 17: SA-GBSG2-ctree-plot
###################################################
#line 414 "Ch_survival_analysis.Rnw"
plot(GBSG2_ctree)


