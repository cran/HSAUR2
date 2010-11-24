###################################################
### chunk number 1: setup
###################################################
#line 182 "Ch_gam.Rnw"
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
#line 221 "Ch_gam.Rnw"
book <- FALSE


###################################################
### chunk number 3: packages
###################################################
#line 224 "Ch_gam.Rnw"
library("mgcv")
library("mboost")
library("rpart")


###################################################
### chunk number 4: GAM-men1500m-plot
###################################################
#line 267 "Ch_gam.Rnw"
plot(time ~ year, data = men1500m)


###################################################
### chunk number 5: GAM-men1500m-lm
###################################################
#line 275 "Ch_gam.Rnw"
men1500m1900 <- subset(men1500m, year >= 1900)
men1500m_lm <- lm(time ~ year, data = men1500m1900)
plot(time ~ year, data = men1500m1900)
abline(men1500m_lm)


###################################################
### chunk number 6: GAM-men1500m-smooth
###################################################
#line 287 "Ch_gam.Rnw"
x <- men1500m1900$year
y <- men1500m1900$time
men1500m_lowess <- lowess(x, y)
plot(time ~ year, data = men1500m1900)
lines(men1500m_lowess, lty = 2)
men1500m_cubic <- gam(y ~ s(x, bs = "cr"))
lines(x, predict(men1500m_cubic), lty = 3)


###################################################
### chunk number 7: GAM-men1500m-quad
###################################################
#line 302 "Ch_gam.Rnw"
men1500m_lm2 <- lm(time ~ year + I(year^2),
                   data = men1500m1900)
plot(time ~ year, data = men1500m1900)
lines(men1500m1900$year, predict(men1500m_lm2))


###################################################
### chunk number 8: GAM-men1500m-pred
###################################################
#line 316 "Ch_gam.Rnw"
predict(men1500m_lm,
        newdata = data.frame(year = c(2008, 2012)),
        interval = "confidence")
predict(men1500m_lm2,
        newdata = data.frame(year = c(2008, 2012)),
        interval = "confidence")


###################################################
### chunk number 9: GAM-USairpollution-boost
###################################################
#line 352 "Ch_gam.Rnw"
library("mboost")
USair_boost <- gamboost(SO2 ~ ., data = USairpollution)
USair_aic <- AIC(USair_boost)
USair_aic


###################################################
### chunk number 10: GAM-USairpollution-boostplot
###################################################
#line 371 "Ch_gam.Rnw"
USair_gam <- USair_boost[mstop(USair_aic)]
layout(matrix(1:6, ncol = 3))
plot(USair_gam, ask = FALSE)


###################################################
### chunk number 11: GAM-USairpollution-residplot
###################################################
#line 399 "Ch_gam.Rnw"
SO2hat <- predict(USair_gam)
SO2 <- USairpollution$SO2
plot(SO2hat, SO2 - SO2hat, type = "n", xlim = c(0, 110))
text(SO2hat, SO2 - SO2hat, labels = rownames(USairpollution),
     adj = 0)
abline(h = 0, lty = 2, col = "grey")


###################################################
### chunk number 12: GAM-kyphosis-plot
###################################################
#line 427 "Ch_gam.Rnw"
layout(matrix(1:3, nrow = 1))
spineplot(Kyphosis ~ Age, data = kyphosis,
          ylevels = c("present", "absent"))
spineplot(Kyphosis ~ Number, data = kyphosis,
          ylevels = c("present", "absent"))
spineplot(Kyphosis ~ Start, data = kyphosis,
         ylevels = c("present", "absent"))


###################################################
### chunk number 13: GAM-kyphosis-gam
###################################################
#line 447 "Ch_gam.Rnw"
kyphosis_gam <- gam(Kyphosis ~ s(Age, bs = "cr") +
    s(Number, bs = "cr", k = 3) + s(Start, bs = "cr", k = 3),
    family = binomial, data = kyphosis)
kyphosis_gam


###################################################
### chunk number 14: GAM-kyphosis-gamplot
###################################################
#line 463 "Ch_gam.Rnw"
trans <- function(x)
    binomial()$linkinv(x)
layout(matrix(1:3, nrow = 1))
plot(kyphosis_gam, select = 1, shade = TRUE, trans = trans)
plot(kyphosis_gam, select = 2, shade = TRUE, trans = trans)
plot(kyphosis_gam, select = 3, shade = TRUE, trans = trans)


