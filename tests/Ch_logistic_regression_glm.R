###################################################
### chunk number 1: setup
###################################################
#line 182 "Ch_logistic_regression_glm.Rnw"
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
#line 221 "Ch_logistic_regression_glm.Rnw"
book <- FALSE


###################################################
### chunk number 3: GLM-plasma-plot
###################################################
#line 233 "Ch_logistic_regression_glm.Rnw"
data("plasma", package = "HSAUR2")
layout(matrix(1:2, ncol = 2))
cdplot(ESR ~ fibrinogen, data = plasma)
cdplot(ESR ~ globulin, data = plasma)


###################################################
### chunk number 4: GLM-plasma-fit1
###################################################
#line 248 "Ch_logistic_regression_glm.Rnw"
plasma_glm_1 <- glm(ESR ~ fibrinogen, data = plasma,
                    family = binomial())


###################################################
### chunk number 5: GLM-plasma-summary-1
###################################################
#line 265 "Ch_logistic_regression_glm.Rnw"
summary(plasma_glm_1)


###################################################
### chunk number 6: GLM-plasma-confint
###################################################
#line 277 "Ch_logistic_regression_glm.Rnw"
ci <- confint(plasma_glm_1)["fibrinogen",]


###################################################
### chunk number 7: GLM-plasma-confint
###################################################
#line 280 "Ch_logistic_regression_glm.Rnw"
confint(plasma_glm_1, parm = "fibrinogen")


###################################################
### chunk number 8: GLM-plasma-confint
###################################################
#line 283 "Ch_logistic_regression_glm.Rnw"
print(ci)


###################################################
### chunk number 9: GLM-plasma-exp
###################################################
#line 289 "Ch_logistic_regression_glm.Rnw"
exp(coef(plasma_glm_1)["fibrinogen"])


###################################################
### chunk number 10: GLM-plasma-exp-ci
###################################################
#line 293 "Ch_logistic_regression_glm.Rnw"
ci <- exp(confint(plasma_glm_1, parm = "fibrinogen"))


###################################################
### chunk number 11: GLM-plasma-exp-ci
###################################################
#line 296 "Ch_logistic_regression_glm.Rnw"
exp(confint(plasma_glm_1, parm = "fibrinogen"))


###################################################
### chunk number 12: GLM-plasma-exp-ci
###################################################
#line 299 "Ch_logistic_regression_glm.Rnw"
print(ci)


###################################################
### chunk number 13: GLM-plasma-fit2
###################################################
#line 308 "Ch_logistic_regression_glm.Rnw"
plasma_glm_2 <- glm(ESR ~ fibrinogen + globulin,
    data = plasma, family = binomial())


###################################################
### chunk number 14: GLM-plasma-summary-2
###################################################
#line 319 "Ch_logistic_regression_glm.Rnw"
summary(plasma_glm_2)


###################################################
### chunk number 15: GLM-plasma-anova-hide
###################################################
#line 323 "Ch_logistic_regression_glm.Rnw"
plasma_anova <- anova(plasma_glm_1, plasma_glm_2, test = "Chisq")


###################################################
### chunk number 16: GLM-plasma-anova
###################################################
#line 335 "Ch_logistic_regression_glm.Rnw"
anova(plasma_glm_1, plasma_glm_2, test = "Chisq")


###################################################
### chunk number 17: GLM-plasma-predict
###################################################
#line 344 "Ch_logistic_regression_glm.Rnw"
prob <- predict(plasma_glm_2, type = "response")


###################################################
### chunk number 18: GLM-plasma-bubble
###################################################
#line 354 "Ch_logistic_regression_glm.Rnw"
plot(globulin ~ fibrinogen, data = plasma, xlim = c(2, 6),
     ylim = c(25, 55), pch = ".")
symbols(plasma$fibrinogen, plasma$globulin, circles = prob,
        add = TRUE)


###################################################
### chunk number 19: GLM-womensrole-fit1
###################################################
#line 378 "Ch_logistic_regression_glm.Rnw"
data("womensrole", package = "HSAUR2")
fm1 <- cbind(agree, disagree) ~ gender + education
womensrole_glm_1 <- glm(fm1, data = womensrole,
                        family = binomial())


###################################################
### chunk number 20: GLM-womensrole-summary-1
###################################################
#line 389 "Ch_logistic_regression_glm.Rnw"
summary(womensrole_glm_1)


###################################################
### chunk number 21: GLM-womensrole-probfit
###################################################
#line 403 "Ch_logistic_regression_glm.Rnw"
role.fitted1 <- predict(womensrole_glm_1, type = "response")


###################################################
### chunk number 22: GLM-plot-setup
###################################################
#line 408 "Ch_logistic_regression_glm.Rnw"
myplot <- function(role.fitted) {
    f <- womensrole$gender == "Female"
    plot(womensrole$education, role.fitted, type = "n",
         ylab = "Probability of agreeing",
         xlab = "Education", ylim = c(0,1))
    lines(womensrole$education[!f], role.fitted[!f], lty = 1)
    lines(womensrole$education[f], role.fitted[f], lty = 2)
    lgtxt <- c("Fitted (Males)", "Fitted (Females)")
    legend("topright", lgtxt, lty = 1:2, bty = "n")
    y <- womensrole$agree / (womensrole$agree +
                              womensrole$disagree)
    text(womensrole$education, y, ifelse(f, "\\VE", "\\MA"),
         family = "HersheySerif", cex = 1.25)
}


###################################################
### chunk number 23: GLM-role-fitted1
###################################################
#line 427 "Ch_logistic_regression_glm.Rnw"
myplot(role.fitted1)


###################################################
### chunk number 24: GLM-womensrole-fit2
###################################################
#line 453 "Ch_logistic_regression_glm.Rnw"
fm2 <- cbind(agree,disagree) ~ gender * education
womensrole_glm_2 <- glm(fm2, data = womensrole,
                        family = binomial())


###################################################
### chunk number 25: GLM-womensrole-summary-2
###################################################
#line 466 "Ch_logistic_regression_glm.Rnw"
summary(womensrole_glm_2)


###################################################
### chunk number 26: GLM-role-fitted2
###################################################
#line 472 "Ch_logistic_regression_glm.Rnw"
role.fitted2 <- predict(womensrole_glm_2, type = "response")
myplot(role.fitted2)


###################################################
### chunk number 27: GLM-role-plot2
###################################################
#line 484 "Ch_logistic_regression_glm.Rnw"
res <- residuals(womensrole_glm_2, type = "deviance")
plot(predict(womensrole_glm_2), res,
     xlab="Fitted values", ylab = "Residuals",
     ylim = max(abs(res)) * c(-1,1))
abline(h = 0, lty = 2)


###################################################
### chunk number 28: GLM-polyps-fit1
###################################################
#line 515 "Ch_logistic_regression_glm.Rnw"
data("polyps", package = "HSAUR2")
polyps_glm_1 <- glm(number ~ treat + age, data = polyps,
                    family = poisson())


###################################################
### chunk number 29: GLM-polyps-summary-1
###################################################
#line 527 "Ch_logistic_regression_glm.Rnw"
summary(polyps_glm_1)


###################################################
### chunk number 30: GLM-polyp-quasi
###################################################
#line 542 "Ch_logistic_regression_glm.Rnw"
polyps_glm_2 <- glm(number ~ treat + age, data = polyps,
                    family = quasipoisson())
summary(polyps_glm_2)


###################################################
### chunk number 31: GLM-backpain-clogit
###################################################
#line 593 "Ch_logistic_regression_glm.Rnw"
library("survival")
backpain_glm <- clogit(I(status == "case") ~
    driver + suburban + strata(ID), data = backpain)


###################################################
### chunk number 32: GLM-backpain-print
###################################################
#line 605 "Ch_logistic_regression_glm.Rnw"
print(backpain_glm)


