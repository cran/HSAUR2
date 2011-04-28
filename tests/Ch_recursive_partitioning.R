###################################################
### chunk number 1: setup
###################################################
#line 184 "Ch_recursive_partitioning.Rnw"
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
#line 223 "Ch_recursive_partitioning.Rnw"
book <- FALSE


###################################################
### chunk number 3: RP-setup
###################################################
#line 226 "Ch_recursive_partitioning.Rnw"
library("vcd")
library("lattice")
library("randomForest")
library("party")
if (!require("partykit"))
    install.packages("partykit", repos = "http://R-forge.R-project.org",
                     INSTALL_opts = "--no-test-load")
ltheme <- canonical.theme(color = FALSE) ## in-built B&W theme
ltheme$strip.background$col <- "transparent" ## change strip bg
lattice.options(default.theme = ltheme)
mai <- par("mai")
options(SweaveHooks = list(nullmai = function() { par(mai = rep(0, 4)) },
                           twomai = function() { par(mai = c(0, mai[2], 0, 0)) },
                           threemai = function() { par(mai = c(0, mai[2], 0.1, 0)) }))
numbers <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")


###################################################
### chunk number 4: RP-bodyfat-rpart
###################################################
#line 254 "Ch_recursive_partitioning.Rnw"
library("rpart")
data("bodyfat", package = "mboost")
bodyfat_rpart <- rpart(DEXfat ~ age + waistcirc + hipcirc +
    elbowbreadth + kneebreadth, data = bodyfat,
    control = rpart.control(minsplit = 10))


###################################################
### chunk number 5: RP-bodyfat-plot
###################################################
#line 272 "Ch_recursive_partitioning.Rnw"
library("partykit")
plot(as.party(bodyfat_rpart), tp_args = list(id = FALSE))


###################################################
### chunk number 6: RP-bodyfat-cp
###################################################
#line 284 "Ch_recursive_partitioning.Rnw"
print(bodyfat_rpart$cptable)
opt <- which.min(bodyfat_rpart$cptable[,"xerror"])


###################################################
### chunk number 7: RP-bodyfat-prune
###################################################
#line 292 "Ch_recursive_partitioning.Rnw"
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)


###################################################
### chunk number 8: RP-bodyfat-pruneplot
###################################################
#line 301 "Ch_recursive_partitioning.Rnw"
plot(as.party(bodyfat_prune), tp_args = list(id = FALSE))


###################################################
### chunk number 9: RP-bodyfat-predict
###################################################
#line 316 "Ch_recursive_partitioning.Rnw"
DEXfat_pred <- predict(bodyfat_prune, newdata = bodyfat)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data = bodyfat, xlab = "Observed",
     ylab = "Predicted", ylim = xlim, xlim = xlim)
abline(a = 0, b = 1)


###################################################
### chunk number 10: RP-seed-again
###################################################
#line 328 "Ch_recursive_partitioning.Rnw"
set.seed(290875)


###################################################
### chunk number 11: RP-glaucoma-rpart
###################################################
#line 331 "Ch_recursive_partitioning.Rnw"
data("GlaucomaM", package = "ipred")
glaucoma_rpart <- rpart(Class ~ ., data = GlaucomaM,
    control = rpart.control(xval = 100))
glaucoma_rpart$cptable
opt <- which.min(glaucoma_rpart$cptable[,"xerror"])
cp <- glaucoma_rpart$cptable[opt, "CP"]
glaucoma_prune <- prune(glaucoma_rpart, cp = cp)


###################################################
### chunk number 12: RP-glaucoma-plot
###################################################
#line 343 "Ch_recursive_partitioning.Rnw"
plot(as.party(glaucoma_prune), tp_args = list(id = FALSE))


###################################################
### chunk number 13: RP-glaucoma-cp
###################################################
#line 358 "Ch_recursive_partitioning.Rnw"
nsplitopt <- vector(mode = "integer", length = 25)
for (i in 1:length(nsplitopt)) {
    cp <- rpart(Class ~ ., data = GlaucomaM)$cptable
    nsplitopt[i] <- cp[which.min(cp[,"xerror"]), "nsplit"]
}
table(nsplitopt)


###################################################
### chunk number 14: RP-glaucoma-bagg
###################################################
#line 379 "Ch_recursive_partitioning.Rnw"
trees <- vector(mode = "list", length = 25)
n <- nrow(GlaucomaM)
bootsamples <- rmultinom(length(trees), n, rep(1, n)/n)
mod <- rpart(Class ~ ., data = GlaucomaM,
             control = rpart.control(xval = 0))
for (i in 1:length(trees))
    trees[[i]] <- update(mod, weights = bootsamples[,i])


###################################################
### chunk number 15: RP-glaucoma-splits
###################################################
#line 394 "Ch_recursive_partitioning.Rnw"
table(sapply(trees, function(x) as.character(x$frame$var[1])))


###################################################
### chunk number 16: RP-glaucoma-baggpred
###################################################
#line 405 "Ch_recursive_partitioning.Rnw"
classprob <- matrix(0, nrow = n, ncol = length(trees))
for (i in 1:length(trees)) {
    classprob[,i] <- predict(trees[[i]],
                             newdata = GlaucomaM)[,1]
    classprob[bootsamples[,i] > 0,i] <- NA
}


###################################################
### chunk number 17: RP-glaucoma-avg
###################################################
#line 424 "Ch_recursive_partitioning.Rnw"
avg <- rowMeans(classprob, na.rm = TRUE)
predictions <- factor(ifelse(avg > 0.5, "glaucoma",
                                        "normal"))
predtab <- table(predictions, GlaucomaM$Class)
predtab


###################################################
### chunk number 18: RP-glaucoma-sens
###################################################
#line 433 "Ch_recursive_partitioning.Rnw"
round(predtab[1,1] / colSums(predtab)[1] * 100)


###################################################
### chunk number 19: RP-glaucoma-spez
###################################################
#line 437 "Ch_recursive_partitioning.Rnw"
round(predtab[2,2] / colSums(predtab)[2] * 100)


###################################################
### chunk number 20: RP-glaucoma-baggplot
###################################################
#line 444 "Ch_recursive_partitioning.Rnw"
library("lattice")
gdata <- data.frame(avg = rep(avg, 2),
    class = rep(as.numeric(GlaucomaM$Class), 2),
    obs = c(GlaucomaM[["varg"]], GlaucomaM[["vari"]]),
    var = factor(c(rep("varg", nrow(GlaucomaM)),
                   rep("vari", nrow(GlaucomaM)))))
panelf <- function(x, y) {
           panel.xyplot(x, y, pch = gdata$class)
           panel.abline(h = 0.5, lty = 2)
       }
print(xyplot(avg ~ obs | var, data = gdata,
       panel = panelf,
       scales = "free", xlab = "",
       ylab = "Estimated Class Probability Glaucoma"))


###################################################
### chunk number 21: RP-glaucoma-rf
###################################################
#line 471 "Ch_recursive_partitioning.Rnw"
library("randomForest")
rf <- randomForest(Class ~ ., data = GlaucomaM)


###################################################
### chunk number 22: RP-glaucoma-rf-oob
###################################################
#line 476 "Ch_recursive_partitioning.Rnw"
table(predict(rf), GlaucomaM$Class)


###################################################
### chunk number 23: RP-detach
###################################################
#line 480 "Ch_recursive_partitioning.Rnw"
detach("package:partykit")


###################################################
### chunk number 24: RP-bodyfat-ctree
###################################################
#line 487 "Ch_recursive_partitioning.Rnw"
library("party")
bodyfat_ctree <- ctree(DEXfat ~ age + waistcirc + hipcirc +
    elbowbreadth + kneebreadth, data = bodyfat)


###################################################
### chunk number 25: RP-bodyfat-ctree-plot
###################################################
#line 500 "Ch_recursive_partitioning.Rnw"
plot(bodyfat_ctree)


###################################################
### chunk number 26: RP-glaucoma-ctree
###################################################
#line 509 "Ch_recursive_partitioning.Rnw"
glaucoma_ctree <- ctree(Class ~ ., data = GlaucomaM)


###################################################
### chunk number 27: RP-glaucoma-ctree-plot
###################################################
#line 519 "Ch_recursive_partitioning.Rnw"
plot(glaucoma_ctree)


