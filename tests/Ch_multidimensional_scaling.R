###################################################
### chunk number 1: setup
###################################################
#line 183 "Ch_multidimensional_scaling.Rnw"
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
#line 222 "Ch_multidimensional_scaling.Rnw"
book <- FALSE


###################################################
### chunk number 3: MDS-setup
###################################################
#line 225 "Ch_multidimensional_scaling.Rnw"
x <- library("ape")


###################################################
### chunk number 4: MDS-voles-cmdscale
###################################################
#line 238 "Ch_multidimensional_scaling.Rnw"
data("watervoles", package = "HSAUR2")
voles_mds <- cmdscale(watervoles, k = 13, eig = TRUE)
voles_mds$eig


###################################################
### chunk number 5: MDS-voles-criterion1
###################################################
#line 245 "Ch_multidimensional_scaling.Rnw"
sum(abs(voles_mds$eig[1:2]))/sum(abs(voles_mds$eig))


###################################################
### chunk number 6: MDS-voles-criterion2
###################################################
#line 249 "Ch_multidimensional_scaling.Rnw"
sum((voles_mds$eig[1:2])^2)/sum((voles_mds$eig)^2)


###################################################
### chunk number 7: MDS-watervoles-plot
###################################################
#line 260 "Ch_multidimensional_scaling.Rnw"
x <- voles_mds$points[,1]
y <- voles_mds$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(x)*1.2, type = "n")
text(x, y, labels = colnames(watervoles))


###################################################
### chunk number 8: MDS-watervoles-mst
###################################################
#line 273 "Ch_multidimensional_scaling.Rnw"
library("ape")
st <- mst(watervoles)
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(x)*1.2, type = "n")
for (i in 1:nrow(watervoles)) {
    w1 <- which(st[i, ] == 1)
    segments(x[i], y[i], x[w1], y[w1])
}
text(x, y, labels = colnames(watervoles))


###################################################
### chunk number 9: MDS-voting
###################################################
#line 292 "Ch_multidimensional_scaling.Rnw"
library("MASS")
data("voting", package = "HSAUR2")
voting_mds <- isoMDS(voting)


###################################################
### chunk number 10: MDS-voting-plot
###################################################
#line 305 "Ch_multidimensional_scaling.Rnw"
x <- voting_mds$points[,1]
y <- voting_mds$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(voting_mds$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(voting))
voting_sh <- Shepard(voting[lower.tri(voting)],
                     voting_mds$points)


###################################################
### chunk number 11: MDS-voting-Shepard
###################################################
#line 320 "Ch_multidimensional_scaling.Rnw"
plot(voting_sh, pch = ".", xlab = "Dissimilarity",
     ylab = "Distance", xlim = range(voting_sh$x),
     ylim = range(voting_sh$x))
lines(voting_sh$x, voting_sh$yf, type = "S")


