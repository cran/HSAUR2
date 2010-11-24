###################################################
### chunk number 1: setup
###################################################
#line 183 "Ch_conditional_inference.Rnw"
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
#line 222 "Ch_conditional_inference.Rnw"
book <- FALSE


###################################################
### chunk number 3: CI-roomwidth-ties
###################################################
#line 227 "Ch_conditional_inference.Rnw"
data("roomwidth", package = "HSAUR2")
nobs <- table(roomwidth$unit)
ties <- tapply(roomwidth$width, roomwidth$unit, function(x) length(x) - length(unique(x)))
library("coin")


###################################################
### chunk number 4: CI-roomwidth-data
###################################################
#line 242 "Ch_conditional_inference.Rnw"
data("roomwidth", package = "HSAUR2")
convert <- ifelse(roomwidth$unit == "feet", 1, 3.28)
feet <- roomwidth$unit == "feet"
metre <- !feet
y <- roomwidth$width * convert


###################################################
### chunk number 5: CI-roomwidth-teststat
###################################################
#line 250 "Ch_conditional_inference.Rnw"
T <- mean(y[feet]) - mean(y[metre])
T


###################################################
### chunk number 6: CI-roomwidth-permutation
###################################################
#line 258 "Ch_conditional_inference.Rnw"
meandiffs <- double(9999)
for (i in 1:length(meandiffs)) {
    sy <- sample(y)
    meandiffs[i] <- mean(sy[feet]) - mean(sy[metre])
}


###################################################
### chunk number 7: CI-roomwidth-plot
###################################################
#line 267 "Ch_conditional_inference.Rnw"
hist(meandiffs)
abline(v = T, lty = 2)
abline(v = -T, lty = 2)


###################################################
### chunk number 8: CI-roomwidth-pvalue
###################################################
#line 287 "Ch_conditional_inference.Rnw"
greater <- abs(meandiffs) > abs(T)
mean(greater)


###################################################
### chunk number 9: CI-roomwidth-pvalue
###################################################
#line 292 "Ch_conditional_inference.Rnw"
binom.test(sum(greater), length(greater))$conf.int


###################################################
### chunk number 10: CI-roomwidth-coin
###################################################
#line 300 "Ch_conditional_inference.Rnw"
library("coin")
independence_test(y ~ unit, data = roomwidth,
                  distribution = exact())


###################################################
### chunk number 11: CI-roomwidth-coin
###################################################
#line 309 "Ch_conditional_inference.Rnw"
wilcox_test(y ~ unit, data = roomwidth,
            distribution = exact())


###################################################
### chunk number 12: CI-suicides-ft
###################################################
#line 319 "Ch_conditional_inference.Rnw"
data("suicides", package = "HSAUR2")
fisher.test(suicides)


###################################################
### chunk number 13: CI-suicides-chisq
###################################################
#line 324 "Ch_conditional_inference.Rnw"
ftp <- round(fisher.test(suicides)$p.value, 3)
ctp <- round(chisq.test(suicides)$p.value, 3)


###################################################
### chunk number 14: CI-Lanza-data
###################################################
#line 336 "Ch_conditional_inference.Rnw"
data("Lanza", package = "HSAUR2")
xtabs(~ treatment + classification + study, data = Lanza)


###################################################
### chunk number 15: CI-width
###################################################
#line 340 "Ch_conditional_inference.Rnw"
options(width = 65)


###################################################
### chunk number 16: CI-Lanza-singleI
###################################################
#line 346 "Ch_conditional_inference.Rnw"
library("coin")
cmh_test(classification ~ treatment, data = Lanza,
         scores = list(classification = c(0, 1, 6, 17, 30)),
         subset = Lanza$study == "I")


###################################################
### chunk number 17: CI-Lanza-singleII
###################################################
#line 356 "Ch_conditional_inference.Rnw"
cmh_test(classification ~ treatment, data = Lanza,
         scores = list(classification = c(0, 1, 6, 17, 30)),
         subset = Lanza$study == "II")


###################################################
### chunk number 18: CI-Lanza-singleIIa
###################################################
#line 363 "Ch_conditional_inference.Rnw"
p <- cmh_test(classification ~ treatment, data = Lanza,
         scores = list(classification = c(0, 1, 6, 17, 30)),
         subset = Lanza$study == "II", distribution =
         approximate(B = 19999))
pvalue(p)


###################################################
### chunk number 19: CI-Lanza-singleIII-IV
###################################################
#line 371 "Ch_conditional_inference.Rnw"
cmh_test(classification ~ treatment, data = Lanza,
         scores = list(classification = c(0, 1, 6, 17, 30)),
         subset = Lanza$study == "III")
cmh_test(classification ~ treatment, data = Lanza,
         scores = list(classification = c(0, 1, 6, 17, 30)),
         subset = Lanza$study == "IV")


###################################################
### chunk number 20: CI-Lanza-all
###################################################
#line 384 "Ch_conditional_inference.Rnw"
cmh_test(classification ~ treatment | study, data = Lanza,
         scores = list(classification = c(0, 1, 6, 17, 30)))


###################################################
### chunk number 21: CI-anomalies
###################################################
#line 393 "Ch_conditional_inference.Rnw"
anomalies <- c(235, 23, 3, 0, 41, 35, 8, 0,
               20, 11, 11, 1, 2, 1, 3, 1)
anomalies <- as.table(matrix(anomalies,
    ncol = 4, dimnames = list(MD = 0:3, RA = 0:3)))
anomalies


###################################################
### chunk number 22: CI-anomalies-mh
###################################################
#line 406 "Ch_conditional_inference.Rnw"
mh_test(anomalies)


###################################################
### chunk number 23: CI-anomalies-ordered
###################################################
#line 414 "Ch_conditional_inference.Rnw"
mh_test(anomalies, scores = list(c(0, 1, 2, 3)))


