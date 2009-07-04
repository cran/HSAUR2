###################################################
### chunk number 1: setup
###################################################
rm(list = ls())
if (!file.exists("tables")) dir.create("tables")
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
book <- FALSE


###################################################
### chunk number 3: PCA-heptathlon-recode
###################################################
data("heptathlon", package = "HSAUR2")
heptathlon$hurdles <- max(heptathlon$hurdles) -
    heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) -
    heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) -
    heptathlon$run800m


###################################################
### chunk number 4: PCA-heptathlon-scatter
###################################################
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])


###################################################
### chunk number 5: PCA-options65
###################################################
w <- options("width")
options(width = 65)


###################################################
### chunk number 6: PCA-heptathlon-cor
###################################################
round(cor(heptathlon[,-score]), 2)


###################################################
### chunk number 7: PCA-optionsw
###################################################
options(width = w$width)


###################################################
### chunk number 8: PCA-heptathlon-PNG
###################################################
heptathlon <- heptathlon[-grep("PNG", rownames(heptathlon)),]


###################################################
### chunk number 9: PCA-heptathlon-scatter2
###################################################
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])


###################################################
### chunk number 10: PCA-options65
###################################################
w <- options("width")
options(width = 65)


###################################################
### chunk number 11: PCA-heptathlon-cor2
###################################################
round(cor(heptathlon[,-score]), 2)


###################################################
### chunk number 12: PCA-optionsw
###################################################
options(width = w$width)


###################################################
### chunk number 13: PCA-options65
###################################################
w <- options("digits")
options(digits = 4)


###################################################
### chunk number 14: PCA-heptathlon-pca
###################################################
heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)
print(heptathlon_pca)


###################################################
### chunk number 15: PCA-heptathlon-summary
###################################################
summary(heptathlon_pca)


###################################################
### chunk number 16: PCA-optionsw
###################################################
options(digits = w$digits)


###################################################
### chunk number 17: PCA-heptathlon-a1
###################################################
a1 <- heptathlon_pca$rotation[,1]
a1


###################################################
### chunk number 18: PCA-heptathlon-scaling
###################################################
center <- heptathlon_pca$center
scale <- heptathlon_pca$scale


###################################################
### chunk number 19: PCA-heptathlon-s1
###################################################
hm <- as.matrix(heptathlon[,-score])
drop(scale(hm, center = center, scale = scale) %*%
     heptathlon_pca$rotation[,1])


###################################################
### chunk number 20: PCA-heptathlon-s1
###################################################
predict(heptathlon_pca)[,1]


###################################################
### chunk number 21: PCA-heptathlon-pca-plot
###################################################
plot(heptathlon_pca)


###################################################
### chunk number 22: PCA-heptathlon-sdev
###################################################
sdev <- heptathlon_pca$sdev
prop12 <- round(sum(sdev[1:2]^2)/sum(sdev^2)*100, 0)


###################################################
### chunk number 23: PCA-heptathlon-biplot eval=FALSE
###################################################
## biplot(heptathlon_pca, col = c("gray", "black"))


###################################################
### chunk number 24: PCA-heptathlon-biplot
###################################################
tmp <- heptathlon[, -score]
rownames(tmp) <- abbreviate(gsub(" \\(.*", "", rownames(tmp)))
biplot(prcomp(tmp, scale = TRUE), col = c("black", "lightgray"), xlim =
c(-0.5, 0.7))


###################################################
### chunk number 25: PCA-scorecor
###################################################
cor(heptathlon$score, heptathlon_pca$x[,1])


###################################################
### chunk number 26: PCA-heptathlonscore
###################################################
plot(heptathlon$score, heptathlon_pca$x[,1])


