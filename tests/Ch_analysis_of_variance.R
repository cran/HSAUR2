###################################################
### chunk number 1: setup
###################################################
#line 182 "Ch_analysis_of_variance.Rnw"
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
#line 221 "Ch_analysis_of_variance.Rnw"
book <- FALSE


###################################################
### chunk number 3: ANOVA-weightgain-mean-var
###################################################
#line 236 "Ch_analysis_of_variance.Rnw"
data("weightgain", package = "HSAUR2")
tapply(weightgain$weightgain,
       list(weightgain$source, weightgain$type), mean)
tapply(weightgain$weightgain,
       list(weightgain$source, weightgain$type), sd)


###################################################
### chunk number 4: ANOVA-weightgain-plot
###################################################
#line 245 "Ch_analysis_of_variance.Rnw"
plot.design(weightgain)


###################################################
### chunk number 5: ANOVA-weightgain-aov
###################################################
#line 264 "Ch_analysis_of_variance.Rnw"
wg_aov <- aov(weightgain ~ source * type, data = weightgain)


###################################################
### chunk number 6: ANOVA-weightgain-aov-summary
###################################################
#line 272 "Ch_analysis_of_variance.Rnw"
summary(wg_aov)


###################################################
### chunk number 7: ANOVA-weightgain-iplot eval=FALSE
###################################################
## #line 278 "Ch_analysis_of_variance.Rnw"
## interaction.plot(weightgain$type, weightgain$source,
##                  weightgain$weightgain)


###################################################
### chunk number 8: ANOVA-weightgain-iplot-nice
###################################################
#line 282 "Ch_analysis_of_variance.Rnw"
interaction.plot(weightgain$type, weightgain$source, weightgain$weightgain,
legend = FALSE)
legend(1.5, 95, legend = levels(weightgain$source), title = "weightgain$source",
       lty = c(2,1), bty = "n")


###################################################
### chunk number 9: ANOVA-weightgain-coef
###################################################
#line 293 "Ch_analysis_of_variance.Rnw"
coef(wg_aov)


###################################################
### chunk number 10: ANOVA-weightgain-contrasts
###################################################
#line 300 "Ch_analysis_of_variance.Rnw"
options("contrasts")


###################################################
### chunk number 11: ANOVA-weightgain-coef-sum
###################################################
#line 306 "Ch_analysis_of_variance.Rnw"
coef(aov(weightgain ~ source + type + source:type,
    data = weightgain, contrasts = list(source = contr.sum)))


###################################################
### chunk number 12: ANOVA-foster
###################################################
#line 317 "Ch_analysis_of_variance.Rnw"
data("foster", package = "HSAUR2")


###################################################
### chunk number 13: ANOVA-foster-plot
###################################################
#line 322 "Ch_analysis_of_variance.Rnw"
plot.design(foster)


###################################################
### chunk number 14: ANOVA-foster-aov-one eval=FALSE
###################################################
## #line 331 "Ch_analysis_of_variance.Rnw"
## summary(aov(weight ~ litgen * motgen, data = foster))


###################################################
### chunk number 15: ANOVA-foster-aov-one
###################################################
#line 335 "Ch_analysis_of_variance.Rnw"
summary(aov(weight ~ litgen * motgen, data = foster))


###################################################
### chunk number 16: ANOVA-foster-aov-two eval=FALSE
###################################################
## #line 339 "Ch_analysis_of_variance.Rnw"
## summary(aov(weight ~ motgen * litgen, data = foster))


###################################################
### chunk number 17: ANOVA-foster-aov-two
###################################################
#line 343 "Ch_analysis_of_variance.Rnw"
summary(aov(weight ~ motgen * litgen, data = foster))


###################################################
### chunk number 18: ANOVA-weightgain-again eval=FALSE
###################################################
## #line 353 "Ch_analysis_of_variance.Rnw"
## summary(aov(weightgain ~ type * source, data = weightgain))


###################################################
### chunk number 19: ANOVA-foster-aov
###################################################
#line 372 "Ch_analysis_of_variance.Rnw"
foster_aov <- aov(weight ~ litgen * motgen, data = foster)


###################################################
### chunk number 20: ANOVA-foster-tukeyHSD
###################################################
#line 377 "Ch_analysis_of_variance.Rnw"
foster_hsd <- TukeyHSD(foster_aov, "motgen")
foster_hsd


###################################################
### chunk number 21: ANOVA-foster-tukeyHSDplot
###################################################
#line 392 "Ch_analysis_of_variance.Rnw"
plot(foster_hsd)


###################################################
### chunk number 22: ANOVA-water-manova
###################################################
#line 413 "Ch_analysis_of_variance.Rnw"
data("water", package = "HSAUR2")
summary(manova(cbind(hardness, mortality) ~ location,
    data = water), test = "Hotelling-Lawley")


###################################################
### chunk number 23: ANOVA-water-means
###################################################
#line 424 "Ch_analysis_of_variance.Rnw"
tapply(water$hardness, water$location, mean)
tapply(water$mortality, water$location, mean)


###################################################
### chunk number 24: ANOVA-skulls-data
###################################################
#line 437 "Ch_analysis_of_variance.Rnw"
data("skulls", package = "HSAUR2")
means <- aggregate(skulls[,c("mb", "bh", "bl", "nh")],
                   list(epoch = skulls$epoch), mean)
means


###################################################
### chunk number 25: ANOVA-skulls-fig
###################################################
#line 449 "Ch_analysis_of_variance.Rnw"
pairs(means[,-1],
    panel = function(x, y) {
        text(x, y, abbreviate(levels(skulls$epoch)))
    })


###################################################
### chunk number 26: ANOVA-skulls-manova
###################################################
#line 464 "Ch_analysis_of_variance.Rnw"
skulls_manova <- manova(cbind(mb, bh, bl, nh) ~ epoch,
                        data = skulls)
summary(skulls_manova, test = "Pillai")
summary(skulls_manova, test = "Wilks")
summary(skulls_manova, test = "Hotelling-Lawley")
summary(skulls_manova, test = "Roy")


###################################################
### chunk number 27: ANOVA-skulls-manova2
###################################################
#line 479 "Ch_analysis_of_variance.Rnw"
summary.aov(skulls_manova)


###################################################
### chunk number 28: ANOVA-skulls-manova3
###################################################
#line 489 "Ch_analysis_of_variance.Rnw"
summary(manova(cbind(mb, bh, bl, nh) ~ epoch, data = skulls,
               subset = epoch %in% c("c4000BC", "c3300BC")))
summary(manova(cbind(mb, bh, bl, nh) ~ epoch, data = skulls,
               subset = epoch %in% c("c4000BC", "c1850BC")))
summary(manova(cbind(mb, bh, bl, nh) ~ epoch, data = skulls,
               subset = epoch %in% c("c4000BC", "c200BC")))
summary(manova(cbind(mb, bh, bl, nh) ~ epoch, data = skulls,
               subset = epoch %in% c("c4000BC", "cAD150")))


