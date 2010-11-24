###################################################
### chunk number 1: setup
###################################################
#line 182 "Ch_principal_components_analysis.Rnw"
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
#line 221 "Ch_principal_components_analysis.Rnw"
book <- FALSE


###################################################
### chunk number 3: PCA-heptathlon-recode
###################################################
#line 231 "Ch_principal_components_analysis.Rnw"
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
#line 242 "Ch_principal_components_analysis.Rnw"
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])


###################################################
### chunk number 5: PCA-options65
###################################################
#line 256 "Ch_principal_components_analysis.Rnw"
w <- options("width")
options(width = 65)


###################################################
### chunk number 6: PCA-heptathlon-cor
###################################################
#line 260 "Ch_principal_components_analysis.Rnw"
round(cor(heptathlon[,-score]), 2)


###################################################
### chunk number 7: PCA-optionsw
###################################################
#line 263 "Ch_principal_components_analysis.Rnw"
options(width = w$width)


###################################################
### chunk number 8: PCA-heptathlon-PNG
###################################################
#line 282 "Ch_principal_components_analysis.Rnw"
heptathlon <- heptathlon[-grep("PNG", rownames(heptathlon)),]


###################################################
### chunk number 9: PCA-heptathlon-scatter2
###################################################
#line 288 "Ch_principal_components_analysis.Rnw"
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])


###################################################
### chunk number 10: PCA-options65
###################################################
#line 297 "Ch_principal_components_analysis.Rnw"
w <- options("width")
options(width = 65)


###################################################
### chunk number 11: PCA-heptathlon-cor2
###################################################
#line 301 "Ch_principal_components_analysis.Rnw"
round(cor(heptathlon[,-score]), 2)


###################################################
### chunk number 12: PCA-optionsw
###################################################
#line 304 "Ch_principal_components_analysis.Rnw"
options(width = w$width)


###################################################
### chunk number 13: PCA-options65
###################################################
#line 312 "Ch_principal_components_analysis.Rnw"
w <- options("digits")
options(digits = 4)


###################################################
### chunk number 14: PCA-heptathlon-pca
###################################################
#line 327 "Ch_principal_components_analysis.Rnw"
heptathlon_pca <- prcomp(heptathlon[, -score], scale = TRUE)
print(heptathlon_pca)


###################################################
### chunk number 15: PCA-heptathlon-summary
###################################################
#line 332 "Ch_principal_components_analysis.Rnw"
summary(heptathlon_pca)


###################################################
### chunk number 16: PCA-optionsw
###################################################
#line 335 "Ch_principal_components_analysis.Rnw"
options(digits = w$digits)


###################################################
### chunk number 17: PCA-heptathlon-a1
###################################################
#line 339 "Ch_principal_components_analysis.Rnw"
a1 <- heptathlon_pca$rotation[,1]
a1


###################################################
### chunk number 18: PCA-heptathlon-scaling
###################################################
#line 349 "Ch_principal_components_analysis.Rnw"
center <- heptathlon_pca$center
scale <- heptathlon_pca$scale


###################################################
### chunk number 19: PCA-heptathlon-s1
###################################################
#line 356 "Ch_principal_components_analysis.Rnw"
hm <- as.matrix(heptathlon[,-score])
drop(scale(hm, center = center, scale = scale) %*%
     heptathlon_pca$rotation[,1])


###################################################
### chunk number 20: PCA-heptathlon-s1
###################################################
#line 363 "Ch_principal_components_analysis.Rnw"
predict(heptathlon_pca)[,1]


###################################################
### chunk number 21: PCA-heptathlon-pca-plot
###################################################
#line 368 "Ch_principal_components_analysis.Rnw"
plot(heptathlon_pca)


###################################################
### chunk number 22: PCA-heptathlon-sdev
###################################################
#line 375 "Ch_principal_components_analysis.Rnw"
sdev <- heptathlon_pca$sdev
prop12 <- round(sum(sdev[1:2]^2)/sum(sdev^2)*100, 0)


###################################################
### chunk number 23: PCA-heptathlon-biplot eval=FALSE
###################################################
## #line 405 "Ch_principal_components_analysis.Rnw"
## biplot(heptathlon_pca, col = c("gray", "black"))


###################################################
### chunk number 24: PCA-heptathlon-biplot
###################################################
#line 408 "Ch_principal_components_analysis.Rnw"
tmp <- heptathlon[, -score]
rownames(tmp) <- abbreviate(gsub(" \\(.*", "", rownames(tmp)))
biplot(prcomp(tmp, scale = TRUE), col = c("black", "lightgray"), xlim =
c(-0.5, 0.7))


###################################################
### chunk number 25: PCA-scorecor
###################################################
#line 423 "Ch_principal_components_analysis.Rnw"
cor(heptathlon$score, heptathlon_pca$x[,1])


###################################################
### chunk number 26: PCA-heptathlonscore
###################################################
#line 432 "Ch_principal_components_analysis.Rnw"
plot(heptathlon$score, heptathlon_pca$x[,1])


