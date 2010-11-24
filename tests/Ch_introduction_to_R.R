###################################################
### chunk number 1: setup
###################################################
#line 182 "Ch_introduction_to_R.Rnw"
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
#line 221 "Ch_introduction_to_R.Rnw"
book <- FALSE


###################################################
### chunk number 3: AItR-welcome
###################################################
#line 311 "Ch_introduction_to_R.Rnw"
HSAUR2:::Rwelcome()


###################################################
### chunk number 4: AItR-promt
###################################################
#line 314 "Ch_introduction_to_R.Rnw"
options(prompt = "> ")


###################################################
### chunk number 5: AItR-welcome
###################################################
#line 318 "Ch_introduction_to_R.Rnw"
options(prompt = "R> ")


###################################################
### chunk number 6: AItR-firstex
###################################################
#line 329 "Ch_introduction_to_R.Rnw"
x <- sqrt(25) + 2


###################################################
### chunk number 7: AItR-firstex-print
###################################################
#line 340 "Ch_introduction_to_R.Rnw"
x


###################################################
### chunk number 8: AItR-firstex-print
###################################################
#line 344 "Ch_introduction_to_R.Rnw"
print(x)


###################################################
### chunk number 9: AItR-recommended
###################################################
#line 351 "Ch_introduction_to_R.Rnw"
colwidth <- 4
ip <- installed.packages(priority = "high")
pkgs <- unique(ip[,"Package"])
pkgs <- paste("\\Rpackage{", pkgs, "}", sep = "")
nrows <- ceiling(length(pkgs) / colwidth)
cat(paste(c("\\begin{tabular}{", paste(rep("l", colwidth), collapse=""), "}"), collapse = ""),
    "\n", file = "tables/rec.tex", append = FALSE)
for (i in 1:nrows) {
    cat(paste(pkgs[(1:colwidth) + (i-1)*colwidth], collapse = " & "),
        file = "tables/rec.tex", append = TRUE)
    cat("\\\\ \n", file = "tables/rec.tex", append = TRUE)
}
cat("\\end{tabular}\n", file = "tables/rec.tex", append = TRUE)
rm(ip, nrows)


###################################################
### chunk number 10: AItR-CRAN
###################################################
#line 378 "Ch_introduction_to_R.Rnw"
cp <- available.packages(contriburl = "http://CRAN.r-project.org/src/contrib")
ncp <- sum(!rownames(cp) %in% pkgs)
rm(cp, pkgs)


###################################################
### chunk number 11: AItR-rm
###################################################
#line 391 "Ch_introduction_to_R.Rnw"
rm(ncp, colwidth, i)


###################################################
### chunk number 12: AItR-install-packages eval=FALSE
###################################################
## #line 403 "Ch_introduction_to_R.Rnw"
## install.packages("sandwich")


###################################################
### chunk number 13: AItR-library eval=FALSE
###################################################
## #line 408 "Ch_introduction_to_R.Rnw"
## library("sandwich")


###################################################
### chunk number 14: AItR-help eval=FALSE
###################################################
## #line 433 "Ch_introduction_to_R.Rnw"
## help("mean")


###################################################
### chunk number 15: AItR-help-lib eval=FALSE
###################################################
## #line 447 "Ch_introduction_to_R.Rnw"
## help(package = "sandwich")


###################################################
### chunk number 16: AItR-help-lib eval=FALSE
###################################################
## #line 454 "Ch_introduction_to_R.Rnw"
## vignette("sandwich", package = "sandwich")


###################################################
### chunk number 17: AItR-Forbes2000
###################################################
#line 505 "Ch_introduction_to_R.Rnw"
data("Forbes2000", package = "HSAUR2")
ls()


###################################################
### chunk number 18: AItR-Forbes2000-ls
###################################################
#line 509 "Ch_introduction_to_R.Rnw"
x <- c("x", "Forbes2000")
print(x)


###################################################
### chunk number 19: AItR-Forbes2000-print eval=FALSE
###################################################
## #line 520 "Ch_introduction_to_R.Rnw"
## print(Forbes2000)


###################################################
### chunk number 20: AItR-Forbes2000-print
###################################################
#line 523 "Ch_introduction_to_R.Rnw"
print(Forbes2000[1:3,])
cat("...\n")


###################################################
### chunk number 21: AItR-Forbes2000-str eval=FALSE
###################################################
## #line 531 "Ch_introduction_to_R.Rnw"
## str(Forbes2000)


###################################################
### chunk number 22: AItR-Forbes2000-str
###################################################
#line 534 "Ch_introduction_to_R.Rnw"
str(Forbes2000, vec.len = 2, strict.width = "cut", width = 60)


###################################################
### chunk number 23: AItR-Forbes2000-help eval=FALSE
###################################################
## #line 559 "Ch_introduction_to_R.Rnw"
## help("Forbes2000")


###################################################
### chunk number 24: AItR-Forbes2000-df
###################################################
#line 574 "Ch_introduction_to_R.Rnw"
class(Forbes2000)


###################################################
### chunk number 25: AItR-Forbes2000-dim
###################################################
#line 581 "Ch_introduction_to_R.Rnw"
dim(Forbes2000)


###################################################
### chunk number 26: AItR-Forbes2000-nrow-ncol
###################################################
#line 586 "Ch_introduction_to_R.Rnw"
nrow(Forbes2000)
ncol(Forbes2000)


###################################################
### chunk number 27: AItR-Forbes2000-names
###################################################
#line 594 "Ch_introduction_to_R.Rnw"
names(Forbes2000)


###################################################
### chunk number 28: AItR-Forbes2000-rank
###################################################
#line 600 "Ch_introduction_to_R.Rnw"
class(Forbes2000[,"rank"])


###################################################
### chunk number 29: AItR-Forbes2000-length
###################################################
#line 612 "Ch_introduction_to_R.Rnw"
length(Forbes2000[,"rank"])


###################################################
### chunk number 30: AItR-Forbes2000-one-to-three
###################################################
#line 619 "Ch_introduction_to_R.Rnw"
1:3
c(1,2,3)
seq(from = 1, to = 3, by = 1)


###################################################
### chunk number 31: AItR-Forbes2000-name
###################################################
#line 627 "Ch_introduction_to_R.Rnw"
class(Forbes2000[,"name"])
length(Forbes2000[,"name"])


###################################################
### chunk number 32: AItR-Forbes2000-first
###################################################
#line 632 "Ch_introduction_to_R.Rnw"
Forbes2000[,"name"][1]


###################################################
### chunk number 33: AItR-Forbes2000-category
###################################################
#line 640 "Ch_introduction_to_R.Rnw"
class(Forbes2000[,"category"])


###################################################
### chunk number 34: AItR-Forbes2000-nlevels
###################################################
#line 649 "Ch_introduction_to_R.Rnw"
nlevels(Forbes2000[,"category"])


###################################################
### chunk number 35: AItR-Forbes2000-levels eval=FALSE
###################################################
## #line 653 "Ch_introduction_to_R.Rnw"
## levels(Forbes2000[,"category"])


###################################################
### chunk number 36: AItR-Forbes2000-levels
###################################################
#line 656 "Ch_introduction_to_R.Rnw"
levels(Forbes2000[,"category"])[1:3]
cat("...\n")


###################################################
### chunk number 37: AItR-Forbes2000-table eval=FALSE
###################################################
## #line 663 "Ch_introduction_to_R.Rnw"
## table(Forbes2000[,"category"])


###################################################
### chunk number 38: AItR-Forbes2000-table
###################################################
#line 666 "Ch_introduction_to_R.Rnw"
table(Forbes2000[,"category"])[1:3]
cat("...\n")


###################################################
### chunk number 39: AItR-Forbes2000-sales
###################################################
#line 673 "Ch_introduction_to_R.Rnw"
class(Forbes2000[,"sales"])


###################################################
### chunk number 40: AItR-Forbes2000-numsum
###################################################
#line 678 "Ch_introduction_to_R.Rnw"
median(Forbes2000[,"sales"])
mean(Forbes2000[,"sales"])
range(Forbes2000[,"sales"])


###################################################
### chunk number 41: AItR-Forbes2000-summary
###################################################
#line 687 "Ch_introduction_to_R.Rnw"
summary(Forbes2000[,"sales"])


###################################################
### chunk number 42: AItR-Forbes2000-files
###################################################
#line 708 "Ch_introduction_to_R.Rnw"
pkgpath <- system.file(package = "HSAUR2")
mywd <- getwd()
filep <- file.path(pkgpath, "rawdata")
setwd(filep)


###################################################
### chunk number 43: AItR-Forbes2000-read.table
###################################################
#line 720 "Ch_introduction_to_R.Rnw"
csvForbes2000 <- read.table("Forbes2000.csv",
    header = TRUE, sep = ",", row.names = 1)


###################################################
### chunk number 44: AItR-Forbes2000-csv-names
###################################################
#line 734 "Ch_introduction_to_R.Rnw"
class(csvForbes2000[,"name"])


###################################################
### chunk number 45: AItR-Forbes2000-read.table2
###################################################
#line 740 "Ch_introduction_to_R.Rnw"
csvForbes2000 <- read.table("Forbes2000.csv",
    header = TRUE, sep = ",", row.names = 1,
    colClasses = c("character", "integer", "character",
        "factor", "factor", "numeric", "numeric", "numeric",
        "numeric"))
class(csvForbes2000[,"name"])


###################################################
### chunk number 46: AItR-Forbes2000-all.equal
###################################################
#line 750 "Ch_introduction_to_R.Rnw"
all.equal(csvForbes2000, Forbes2000)


###################################################
### chunk number 47: AItR-Forbes2000-classes
###################################################
#line 757 "Ch_introduction_to_R.Rnw"
classes <- c("character", "integer", "character", "factor",
    "factor", "numeric", "numeric", "numeric", "numeric")
length(classes)
class(classes)


###################################################
### chunk number 48: AItR-Forbes2000-RODBC eval=FALSE
###################################################
## #line 768 "Ch_introduction_to_R.Rnw"
## library("RODBC")
## cnct <- odbcConnectExcel("Forbes2000.xls")
## sqlQuery(cnct, "select * from \"Forbes2000\\$\"")


###################################################
### chunk number 49: AItR-Forbes2000-RODBC
###################################################
#line 776 "Ch_introduction_to_R.Rnw"
setwd(mywd)


###################################################
### chunk number 50: AItR-Forbes2000-write.table
###################################################
#line 784 "Ch_introduction_to_R.Rnw"
write.table(Forbes2000, file = "Forbes2000.csv", sep = ",",
            col.names = NA)


###################################################
### chunk number 51: AItR-Forbes2000-save
###################################################
#line 794 "Ch_introduction_to_R.Rnw"
save(Forbes2000, file = "Forbes2000.rda")


###################################################
### chunk number 52: AItR-Forbes2000-list
###################################################
#line 799 "Ch_introduction_to_R.Rnw"
list.files(pattern = "\\.rda")


###################################################
### chunk number 53: AItR-Forbes2000-load
###################################################
#line 803 "Ch_introduction_to_R.Rnw"
load("Forbes2000.rda")


###################################################
### chunk number 54: AItR-Forbes2000-vector-companies
###################################################
#line 816 "Ch_introduction_to_R.Rnw"
companies <- Forbes2000[,"name"]


###################################################
### chunk number 55: AItR-Forbes2000-vector-indexing
###################################################
#line 826 "Ch_introduction_to_R.Rnw"
companies[1]


###################################################
### chunk number 56: AItR-Forbes2000-vector-indexing
###################################################
#line 831 "Ch_introduction_to_R.Rnw"
1:3
companies[1:3]


###################################################
### chunk number 57: AItR-Forbes2000-vector-negative-indexing
###################################################
#line 840 "Ch_introduction_to_R.Rnw"
companies[-(4:2000)]


###################################################
### chunk number 58: AItR-Forbes2000-top-three
###################################################
#line 847 "Ch_introduction_to_R.Rnw"
Forbes2000[1:3, c("name", "sales", "profits", "assets")]


###################################################
### chunk number 59: AItR-Forbes2000-list-extract
###################################################
#line 854 "Ch_introduction_to_R.Rnw"
companies <- Forbes2000$name


###################################################
### chunk number 60: AItR-Forbes2000-vector-companies
###################################################
#line 858 "Ch_introduction_to_R.Rnw"
companies <- Forbes2000[,"name"]


###################################################
### chunk number 61: AItR-Forbes2000-sales
###################################################
#line 865 "Ch_introduction_to_R.Rnw"
order_sales <- order(Forbes2000$sales)


###################################################
### chunk number 62: AItR-Forbes2000-sales-small
###################################################
#line 871 "Ch_introduction_to_R.Rnw"
companies[order_sales[1:3]]


###################################################
### chunk number 63: AItR-Forbes2000-order
###################################################
#line 876 "Ch_introduction_to_R.Rnw"
Forbes2000[order_sales[c(2000, 1999, 1998)],
           c("name", "sales", "profits", "assets")]


###################################################
### chunk number 64: AItR-Forbes2000-logical
###################################################
#line 884 "Ch_introduction_to_R.Rnw"
Forbes2000[Forbes2000$assets > 1000,
           c("name", "sales", "profits", "assets")]


###################################################
### chunk number 65: AItR-Forbes2000-logical2
###################################################
#line 890 "Ch_introduction_to_R.Rnw"
table(Forbes2000$assets > 1000)


###################################################
### chunk number 66: AItR-Forbes2000-NA
###################################################
#line 901 "Ch_introduction_to_R.Rnw"
na_profits <- is.na(Forbes2000$profits)
table(na_profits)
Forbes2000[na_profits,
           c("name", "sales", "profits", "assets")]


###################################################
### chunk number 67: AItR-Forbes2000-complete-cases
###################################################
#line 914 "Ch_introduction_to_R.Rnw"
table(complete.cases(Forbes2000))


###################################################
### chunk number 68: AItR-Forbes2000-UK
###################################################
#line 922 "Ch_introduction_to_R.Rnw"
UKcomp <- subset(Forbes2000, country == "United Kingdom")
dim(UKcomp)


###################################################
### chunk number 69: AItR-Forbes2000-summary
###################################################
#line 938 "Ch_introduction_to_R.Rnw"
summary(Forbes2000)


###################################################
### chunk number 70: AItR-Forbes2000-summary-output
###################################################
#line 942 "Ch_introduction_to_R.Rnw"
summary(Forbes2000)


###################################################
### chunk number 71: AItR-Forbes2000-lapply eval=FALSE
###################################################
## #line 959 "Ch_introduction_to_R.Rnw"
## lapply(Forbes2000, summary)


###################################################
### chunk number 72: AItR-Forbes2000-tapply-category
###################################################
#line 968 "Ch_introduction_to_R.Rnw"
mprofits <- tapply(Forbes2000$profits,
                   Forbes2000$category, median, na.rm = TRUE)


###################################################
### chunk number 73: AItR-Forbes2000-medianNA
###################################################
#line 979 "Ch_introduction_to_R.Rnw"
median(Forbes2000$profits)


###################################################
### chunk number 74: AItR-Forbes2000-mprofits
###################################################
#line 984 "Ch_introduction_to_R.Rnw"
rev(sort(mprofits))[1:3]


###################################################
### chunk number 75: AItR-Forbes2000-medianNA
###################################################
#line 1007 "Ch_introduction_to_R.Rnw"
median(Forbes2000$profits, na.rm = TRUE)


###################################################
### chunk number 76: AItR-iqr
###################################################
#line 1034 "Ch_introduction_to_R.Rnw"
iqr <- function(x) {
    q <- quantile(x, prob = c(0.25, 0.75), names = FALSE)
    return(diff(q))
}


###################################################
### chunk number 77: AItR-iqr-test
###################################################
#line 1044 "Ch_introduction_to_R.Rnw"
xdata <- rnorm(100)
iqr(xdata)
IQR(xdata)


###################################################
### chunk number 78: AItR-iqr-test eval=FALSE
###################################################
## #line 1051 "Ch_introduction_to_R.Rnw"
## xdata[1] <- NA
## iqr(xdata)


###################################################
### chunk number 79: AItR-iqr-test-results
###################################################
#line 1055 "Ch_introduction_to_R.Rnw"
xdata[1] <- NA
cat(try(iqr(xdata)))


###################################################
### chunk number 80: AItR-iqr
###################################################
#line 1066 "Ch_introduction_to_R.Rnw"
iqr <- function(x, ...) {
    q <- quantile(x, prob = c(0.25, 0.75), names = FALSE,
                  ...)
    return(diff(q))
}
iqr(xdata, na.rm = TRUE)
IQR(xdata, na.rm = TRUE)


###################################################
### chunk number 81: AItR-Forbes2000-iqr
###################################################
#line 1077 "Ch_introduction_to_R.Rnw"
iqr(Forbes2000$profits, na.rm = TRUE)


###################################################
### chunk number 82: AItR-Forbes2000-tapply-category-iqr
###################################################
#line 1085 "Ch_introduction_to_R.Rnw"
iqr_profits <- tapply(Forbes2000$profits,
                      Forbes2000$category, iqr, na.rm = TRUE)


###################################################
### chunk number 83: AItR-Forbes2000-variability
###################################################
#line 1090 "Ch_introduction_to_R.Rnw"
levels(Forbes2000$category)[which.min(iqr_profits)]
levels(Forbes2000$category)[which.max(iqr_profits)]


###################################################
### chunk number 84: AItR-Forbes2000-for
###################################################
#line 1108 "Ch_introduction_to_R.Rnw"
bcat <- Forbes2000$category
iqr_profits2 <- numeric(nlevels(bcat))
names(iqr_profits2) <- levels(bcat)
for (cat in levels(bcat)) {
    catprofit <- subset(Forbes2000, category == cat)$profit
    this_iqr <- iqr(catprofit, na.rm = TRUE)
    iqr_profits2[levels(bcat) == cat] <- this_iqr
}


###################################################
### chunk number 85: AItR-Forbes2000-marketvalue
###################################################
#line 1138 "Ch_introduction_to_R.Rnw"
layout(matrix(1:2, nrow = 2))
hist(Forbes2000$marketvalue)
hist(log(Forbes2000$marketvalue))


###################################################
### chunk number 86: AItR-Forbes2000-formula
###################################################
#line 1159 "Ch_introduction_to_R.Rnw"
fm <- marketvalue ~ sales
class(fm)


###################################################
### chunk number 87: AItR-Forbes2000-marketvalue-sales
###################################################
#line 1178 "Ch_introduction_to_R.Rnw"
plot(log(marketvalue) ~ log(sales), data = Forbes2000,
     pch = ".")


###################################################
### chunk number 88: AItR-Forbes2000-marketvalue-sales-shading
###################################################
#line 1188 "Ch_introduction_to_R.Rnw"
plot(log(marketvalue) ~ log(sales), data = Forbes2000,
     col = rgb(0,0,0,0.1), pch = 16)


###################################################
### chunk number 89: AItR-Forbes2000-country-plot
###################################################
#line 1209 "Ch_introduction_to_R.Rnw"
tmp <- subset(Forbes2000,
    country %in% c("United Kingdom", "Germany",
                   "India", "Turkey"))
tmp$country <- tmp$country[,drop = TRUE]
plot(log(marketvalue) ~ country, data = tmp,
     ylab = "log(marketvalue)", varwidth = TRUE)


###################################################
### chunk number 90: AItR-analysis1
###################################################
#line 1228 "Ch_introduction_to_R.Rnw"
file.create("analysis.R")


###################################################
### chunk number 91: AItR-analysis2 eval=FALSE
###################################################
## #line 1237 "Ch_introduction_to_R.Rnw"
## source("analysis.R", echo = TRUE)


###################################################
### chunk number 92: AItR-analysis3
###################################################
#line 1245 "Ch_introduction_to_R.Rnw"
file.remove("analysis.R")


