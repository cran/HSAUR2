pkgname <- "HSAUR2"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('HSAUR2')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BCG")
### * BCG

flush(stderr()); flush(stdout())

### Name: BCG
### Title: BCG Vaccine Data
### Aliases: BCG
### Keywords: datasets

### ** Examples


  data("BCG", package = "HSAUR2")

  ### sort studies w.r.t. sample size
  BCG <- BCG[order(rowSums(BCG[,2:5])),]

  ### to long format
  BCGlong <- with(BCG, data.frame(Freq = c(BCGTB, BCGVacc - BCGTB, 
                                           NoVaccTB, NoVacc - NoVaccTB),
                  infected = rep(rep(factor(c("yes", "no")), 
                                 rep(nrow(BCG), 2)), 2),
                  vaccined = rep(factor(c("yes", "no")), 
                                 rep(nrow(BCG) * 2, 2)),
                  study = rep(factor(Study, levels = as.character(Study)), 
                              4)))

  ### doubledecker plot
  library("vcd")
  doubledecker(xtabs(Freq ~ study + vaccined + infected, 
                     data = BCGlong))




cleanEx()
nameEx("BtheB")
### * BtheB

flush(stderr()); flush(stdout())

### Name: BtheB
### Title: Beat the Blues Data
### Aliases: BtheB
### Keywords: datasets

### ** Examples


  data("BtheB", package = "HSAUR2")
  layout(matrix(1:2, nrow = 1))   
  ylim <- range(BtheB[,grep("bdi", names(BtheB))], na.rm = TRUE)
  boxplot(subset(BtheB, treatment == "TAU")[,grep("bdi", names(BtheB))],
          main = "Treated as usual", ylab = "BDI", 
          xlab = "Time (in months)", names = c(0, 2, 3, 5, 8), ylim = ylim)
  boxplot(subset(BtheB, treatment == "BtheB")[,grep("bdi", names(BtheB))], 
          main = "Beat the Blues", ylab = "BDI", xlab = "Time (in months)",
          names = c(0, 2, 3, 5, 8), ylim = ylim)




cleanEx()
nameEx("CHFLS")
### * CHFLS

flush(stderr()); flush(stdout())

### Name: CHFLS
### Title: Chinese Health and Family Life Survey
### Aliases: CHFLS
### Keywords: datasets

### ** Examples


## Not run: 
##D 
##D     library("foreign")
##D 
##D     dataurl <- "http://www.src.uchicago.edu/datalib/chfls/data/chfls1.sav"
##D     td <- tempdir()
##D     download.file(dataurl, destfile = file.path(td, "chfls1.sav"), mode = "wb")
##D 
##D     ### for a description see http://popcenter.uchicago.edu/data/chfls.shtml
##D     chfls1 <- read.spss(file.path(td, "chfls1.sav"), to.data.frame = TRUE)
##D 
##D     tmp <- chfls1[, c("REGION6", "ZJ05", "ZJ06", "A35", "ZJ07", "ZJ16M", "INCRM",
##D                       "JK01", "JK02", "JK20", "HY04", "HY07", "A02", "AGEGAPM", 
##D                       "A07M", "A14", "A21", "A22M", "A23", "AX16", "INCAM", "SEXNOW", "ZW04")]
##D 
##D      names(tmp) <- c("Region",
##D                 "Rgender",               ### gender of respondent
##D                 "Rage",                  ### age of respondent
##D 		"RagestartA",		 ### age of respondent at beginning of relationship with partner A
##D                 "Redu",                  ### education of respondent
##D                 "RincomeM",              ### rounded monthly income of respondent
##D 		"RincomeComp",		 ### inputed monthly income of respondent
##D                 "Rhealth",               ### health condition respondent
##D                 "Rheight",               ### respondent's height
##D                 "Rhappy",                ### respondent's happiness
##D                 "Rmartial",              ### respondent's marital status
##D                 "RhasA",                 ### R has current A partner
##D                 "Agender",               ### gender of partner A
##D                 "RAagegap",              ### age gap
##D                 "RAstartage",            ### age at marriage
##D                 "Aheight",               ### height of partner A
##D                 "Aedu",                  ### education of partner A
##D                 "AincomeM",              ### rounded partner A income
##D                 "AincomeEst",            ### estimated partner A income
##D                 "orgasm",                ### orgasm frequency
##D                 "AincomeComp",           ### imputed partner A income
##D                 "Rsexnow",               ### has sex last year
##D                 "Rhomosexual")           ### R is homosexual
##D 
##D     ### code missing values
##D     tmp$AincomeM[tmp$AincomeM < 0] <- NA
##D     tmp$RincomeM[tmp$RincomeM < 0] <- NA
##D     tmp$Aheight[tmp$Aheight < 0] <- NA
##D 
##D     olevels <- c("never", "rarely", "sometimes", "often", "always")
##D     tmpA <- subset(tmp, Rgender == "female" & Rhomosexual != "yes" & orgasm %in% olevels)
##D 
##D     ### 1534 subjects
##D     dim(tmpA)
##D 
##D     CHFLS <- tmpA[, c("Region", "Rage", "Redu", "RincomeComp", "Rhealth", "Rheight", "Rhappy",
##D                       "Aheight", "Aedu", "AincomeComp")]
##D     names(CHFLS) <- c("R_region", "R_age", "R_edu", "R_income", "R_health", "R_height", 
##D                       "R_happy", "A_height", "A_edu", "A_income")
##D     levels(CHFLS$R_region) <- c("Coastal South", "Coastal Easth", "Inlands", "North", "Northeast", "Central West")
##D 
##D     CHFLS$R_edu <- ordered(as.character(CHFLS$R_edu), levels = c("no school", "primary", "low mid", "up mid", "j col", "univ/grad"))
##D     levels(CHFLS$R_edu) <- c("Never attended school", "Elementary school", "Junior high school", "Senior high school",
##D                          "Junior college", "University")
##D     CHFLS$A_edu <- ordered(as.character(CHFLS$A_edu), levels = c("no school", "primary", "low mid", "up mid", "j col", "univ/grad"))
##D     levels(CHFLS$A_edu) <- c("Never attended school", "Elementary school", "Junior high school", "Senior high school",
##D                              "Junior college", "University")
##D 
##D     CHFLS$R_health <- ordered(as.character(CHFLS$R_health), levels = c("poor", "not good", "fair", "good", "excellent"))
##D     levels(CHFLS$R_health) <- c("Poor", "Not good", "Fair", "Good", "Excellent")
##D 
##D     CHFLS$R_happy <- ordered(as.character(CHFLS$R_happy), levels = c("v unhappy", "not too", "relatively", "very"))
##D     levels(CHFLS$R_happy) <- c("Very unhappy", "Not too happy", "Relatively happy", "Very happy")
## End(Not run)




cleanEx()
nameEx("CYGOB1")
### * CYGOB1

flush(stderr()); flush(stdout())

### Name: CYGOB1
### Title: CYG OB1 Star Cluster Data
### Aliases: CYGOB1
### Keywords: datasets

### ** Examples


  data("CYGOB1", package = "HSAUR2")
  plot(logst ~ logli, data = CYGOB1)




cleanEx()
nameEx("Forbes2000")
### * Forbes2000

flush(stderr()); flush(stdout())

### Name: Forbes2000
### Title: The Forbes 2000 Ranking of the World's Biggest Companies (Year
###   2004)
### Aliases: Forbes2000
### Keywords: datasets

### ** Examples

data("Forbes2000", package = "HSAUR2")
summary(Forbes2000)
### number of countries
length(levels(Forbes2000$country))
### number of industries
length(levels(Forbes2000$category))



cleanEx()
nameEx("GHQ")
### * GHQ

flush(stderr()); flush(stdout())

### Name: GHQ
### Title: General Health Questionnaire
### Aliases: GHQ
### Keywords: datasets

### ** Examples


  data("GHQ", package = "HSAUR2")
  male <- subset(GHQ, gender == "male")
  female <- subset(GHQ, gender == "female")
  layout(matrix(1:2, ncol = 2))
  barplot(t(as.matrix(male[,c("cases", "non.cases")])), main = "Male", xlab = "GHC score")
  barplot(t(as.matrix(male[,c("cases", "non.cases")])), main = "Female", xlab = "GHC score")




cleanEx()
nameEx("HSAURtable")
### * HSAURtable

flush(stderr()); flush(stdout())

### Name: HSAURtable
### Title: Produce LaTeX Tables
### Aliases: HSAURtable toLatex.tabtab toLatex.dftab HSAURtable.table
###   HSAURtable.data.frame
### Keywords: misc

### ** Examples


  data("rearrests", package = "HSAUR2")
  toLatex(HSAURtable(rearrests), 
          caption = "Rearrests of juvenile felons.", 
          label = "rearrests_tab")




cleanEx()
nameEx("Lanza")
### * Lanza

flush(stderr()); flush(stdout())

### Name: Lanza
### Title: Prevention of Gastointestinal Damages
### Aliases: Lanza
### Keywords: datasets

### ** Examples


  data("Lanza", package = "HSAUR2")
  layout(matrix(1:4, nrow = 2))
  pl <- tapply(1:nrow(Lanza), Lanza$study, function(indx)
      mosaicplot(table(Lanza[indx,"treatment"], 
                       Lanza[indx,"classification"]),
                 main = "", shade = TRUE))




cleanEx()
nameEx("USairpollution")
### * USairpollution

flush(stderr()); flush(stdout())

### Name: USairpollution
### Title: Air Pollution in US Cities
### Aliases: USairpollution
### Keywords: datasets

### ** Examples


  data("USairpollution", package = "HSAUR2")




cleanEx()
nameEx("USmelanoma")
### * USmelanoma

flush(stderr()); flush(stdout())

### Name: USmelanoma
### Title: USA Malignant Melanoma Data
### Aliases: USmelanoma
### Keywords: datasets

### ** Examples


  data("USmelanoma", package = "HSAUR2")



cleanEx()
nameEx("agefat")
### * agefat

flush(stderr()); flush(stdout())

### Name: agefat
### Title: Total Body Composision Data
### Aliases: agefat
### Keywords: datasets

### ** Examples


  data("agefat", package = "HSAUR2")
  plot(fat ~ age, data = agefat)




cleanEx()
nameEx("aspirin")
### * aspirin

flush(stderr()); flush(stdout())

### Name: aspirin
### Title: Aspirin Data
### Aliases: aspirin
### Keywords: datasets

### ** Examples


  data("aspirin", package = "HSAUR2")
  aspirin




cleanEx()
nameEx("backpain")
### * backpain

flush(stderr()); flush(stdout())

### Name: backpain
### Title: Driving and Back Pain Data
### Aliases: backpain
### Keywords: datasets

### ** Examples


  data("backpain", package = "HSAUR2")
  summary(backpain)




cleanEx()
nameEx("birthdeathrates")
### * birthdeathrates

flush(stderr()); flush(stdout())

### Name: birthdeathrates
### Title: Birth and Death Rates Data
### Aliases: birthdeathrates
### Keywords: datasets

### ** Examples


  data("birthdeathrates", package = "HSAUR2")
  plot(birthdeathrates)




cleanEx()
nameEx("bladdercancer")
### * bladdercancer

flush(stderr()); flush(stdout())

### Name: bladdercancer
### Title: Bladder Cancer Data
### Aliases: bladdercancer
### Keywords: datasets

### ** Examples


  data("bladdercancer", package = "HSAUR2")
  mosaicplot(xtabs(~ number + tumorsize, data = bladdercancer))




cleanEx()
nameEx("clouds")
### * clouds

flush(stderr()); flush(stdout())

### Name: clouds
### Title: Cloud Seeding Data
### Aliases: clouds
### Keywords: datasets

### ** Examples


  data("clouds", package = "HSAUR2")
  layout(matrix(1:2, nrow = 2))
  boxplot(rainfall ~ seeding, data = clouds, ylab = "Rainfall")
  boxplot(rainfall ~ echomotion, data = clouds, ylab = "Rainfall")    




cleanEx()
nameEx("epilepsy")
### * epilepsy

flush(stderr()); flush(stdout())

### Name: epilepsy
### Title: Epilepsy Data
### Aliases: epilepsy
### Keywords: datasets

### ** Examples


  data("epilepsy", package = "HSAUR2")
  library(lattice)
  dotplot(I(seizure.rate / base) ~ period | subject, data = epilepsy, 
          subset = treatment == "Progabide")
  dotplot(I(seizure.rate / base) ~ period | subject, data = epilepsy, 
          subset = treatment == "Progabide")




cleanEx()
nameEx("foster")
### * foster

flush(stderr()); flush(stdout())

### Name: foster
### Title: Foster Feeding Experiment
### Aliases: foster
### Keywords: datasets

### ** Examples


  data("foster", package = "HSAUR2")
  plot.design(foster)




cleanEx()
nameEx("gardenflowers")
### * gardenflowers

flush(stderr()); flush(stdout())

### Name: gardenflowers
### Title: Garden Flowers
### Aliases: gardenflowers
### Keywords: datasets

### ** Examples


  data("gardenflowers", package = "HSAUR2")
  gardenflowers




cleanEx()
nameEx("heptathlon")
### * heptathlon

flush(stderr()); flush(stdout())

### Name: heptathlon
### Title: Olympic Heptathlon Seoul 1988
### Aliases: heptathlon
### Keywords: datasets

### ** Examples


  data("heptathlon", package = "HSAUR2")
  plot(heptathlon)




cleanEx()
nameEx("household")
### * household

flush(stderr()); flush(stdout())

### Name: household
### Title: Household Expenditure Data
### Aliases: household
### Keywords: datasets

### ** Examples


  data("household", package = "HSAUR2")




cleanEx()
nameEx("mastectomy")
### * mastectomy

flush(stderr()); flush(stdout())

### Name: mastectomy
### Title: Survival Times after Mastectomy of Breast Cancer Patients
### Aliases: mastectomy
### Keywords: datasets

### ** Examples


  data("mastectomy", package = "HSAUR2")
  table(mastectomy$metastasized)



cleanEx()
nameEx("men1500m")
### * men1500m

flush(stderr()); flush(stdout())

### Name: men1500m
### Title: Winners of the Olympic Men's 1500m
### Aliases: men1500m
### Keywords: datasets

### ** Examples


  data("men1500m", package = "HSAUR2")
  op <- par(las = 2)
  plot(time ~ year, data = men1500m, axes = FALSE)
  yrs <- seq(from = 1896, to = 2004, by = 4)
  axis(1, at = yrs, labels = yrs)
  axis(2)
  box()
  par(op)




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("meteo")
### * meteo

flush(stderr()); flush(stdout())

### Name: meteo
### Title: Meteorological Measurements for 11 Years
### Aliases: meteo
### Keywords: datasets

### ** Examples


  data("meteo", package = "HSAUR2")
  meteo




cleanEx()
nameEx("orallesions")
### * orallesions

flush(stderr()); flush(stdout())

### Name: orallesions
### Title: Oral Lesions in Rural India
### Aliases: orallesions
### Keywords: datasets

### ** Examples


  data("orallesions", package = "HSAUR2")
  mosaicplot(orallesions)




cleanEx()
nameEx("phosphate")
### * phosphate

flush(stderr()); flush(stdout())

### Name: phosphate
### Title: Phosphate Level Data
### Aliases: phosphate
### Keywords: datasets

### ** Examples


  data("phosphate", package = "HSAUR2")
  plot(t0 ~ group, data = phosphate)




cleanEx()
nameEx("pistonrings")
### * pistonrings

flush(stderr()); flush(stdout())

### Name: pistonrings
### Title: Piston Rings Failures
### Aliases: pistonrings
### Keywords: datasets

### ** Examples

  
  data("pistonrings", package = "HSAUR2")
  mosaicplot(pistonrings)




cleanEx()
nameEx("planets")
### * planets

flush(stderr()); flush(stdout())

### Name: planets
### Title: Exoplanets Data
### Aliases: planets
### Keywords: datasets

### ** Examples


  data("planets", package = "HSAUR2")
  require("scatterplot3d")
  scatterplot3d(log(planets$mass), log(planets$period), log(planets$eccen), 
                type = "h", highlight.3d = TRUE,  angle = 55, 
                scale.y = 0.7, pch = 16)




cleanEx()
nameEx("plasma")
### * plasma

flush(stderr()); flush(stdout())

### Name: plasma
### Title: Blood Screening Data
### Aliases: plasma
### Keywords: datasets

### ** Examples


  data("plasma", package = "HSAUR2")
  layout(matrix(1:2, ncol = 2))
  boxplot(fibrinogen ~ ESR, data = plasma, varwidth = TRUE)
  boxplot(globulin ~ ESR, data = plasma, varwidth = TRUE)




cleanEx()
nameEx("polyps")
### * polyps

flush(stderr()); flush(stdout())

### Name: polyps
### Title: Familial Andenomatous Polyposis
### Aliases: polyps
### Keywords: datasets

### ** Examples


  data("polyps", package = "HSAUR2")
  plot(number ~ age, data = polyps, pch = as.numeric(polyps$treat))
  legend(40, 40, legend = levels(polyps$treat), pch = 1:2, bty = "n")




cleanEx()
nameEx("polyps3")
### * polyps3

flush(stderr()); flush(stdout())

### Name: polyps3
### Title: Familial Andenomatous Polyposis
### Aliases: polyps3
### Keywords: datasets

### ** Examples


  data("polyps3", package = "HSAUR2")
  plot(number3m ~ age, data = polyps3, pch = as.numeric(polyps3$treatment))
  legend("topright", legend = levels(polyps3$treatment), pch = 1:2, bty = "n")




cleanEx()
nameEx("pottery")
### * pottery

flush(stderr()); flush(stdout())

### Name: pottery
### Title: Romano-British Pottery Data
### Aliases: pottery
### Keywords: datasets

### ** Examples


  data("pottery", package = "HSAUR2")
  plot(pottery)




cleanEx()
nameEx("rearrests")
### * rearrests

flush(stderr()); flush(stdout())

### Name: rearrests
### Title: Rearrests of Juvenile Felons
### Aliases: rearrests
### Keywords: datasets

### ** Examples


  data("rearrests", package = "HSAUR2")
  rearrests




cleanEx()
nameEx("respiratory")
### * respiratory

flush(stderr()); flush(stdout())

### Name: respiratory
### Title: Respiratory Illness Data
### Aliases: respiratory
### Keywords: datasets

### ** Examples


  data("respiratory", package = "HSAUR2")
  mosaicplot(xtabs( ~ treatment + month + status, data = respiratory))




cleanEx()
nameEx("roomwidth")
### * roomwidth

flush(stderr()); flush(stdout())

### Name: roomwidth
### Title: Students Estimates of Lecture Room Width
### Aliases: roomwidth
### Keywords: datasets

### ** Examples


  data("roomwidth", package = "HSAUR2")
  convert <- ifelse(roomwidth$unit == "feet", 1, 3.28)
  boxplot(I(width * convert) ~ unit, data = roomwidth)




cleanEx()
nameEx("schizophrenia")
### * schizophrenia

flush(stderr()); flush(stdout())

### Name: schizophrenia
### Title: Age of Onset of Schizophrenia Data
### Aliases: schizophrenia
### Keywords: datasets

### ** Examples


  data("schizophrenia", package = "HSAUR2")
  boxplot(age ~ gender, data = schizophrenia)




cleanEx()
nameEx("schizophrenia2")
### * schizophrenia2

flush(stderr()); flush(stdout())

### Name: schizophrenia2
### Title: Schizophrenia Data
### Aliases: schizophrenia2
### Keywords: datasets

### ** Examples


  data("schizophrenia2", package = "HSAUR2")
  mosaicplot(xtabs( ~ onset + month + disorder, data = schizophrenia2))




cleanEx()
nameEx("schooldays")
### * schooldays

flush(stderr()); flush(stdout())

### Name: schooldays
### Title: Days not Spent at School
### Aliases: schooldays
### Keywords: datasets

### ** Examples


  data("schooldays", package = "HSAUR2")
  plot.design(schooldays)




cleanEx()
nameEx("skulls")
### * skulls

flush(stderr()); flush(stdout())

### Name: skulls
### Title: Egyptian Skulls
### Aliases: skulls
### Keywords: datasets

### ** Examples


  data("skulls", package = "HSAUR2")
  means <- tapply(1:nrow(skulls), skulls$epoch, function(i)
               apply(skulls[i,colnames(skulls)[-1]], 2, mean))
  means <- matrix(unlist(means), nrow = length(means), byrow = TRUE)
  colnames(means) <- colnames(skulls)[-1]
  rownames(means) <- levels(skulls$epoch)
  pairs(means,
      panel = function(x, y) {
          text(x, y, levels(skulls$epoch))
      })




cleanEx()
nameEx("smoking")
### * smoking

flush(stderr()); flush(stdout())

### Name: smoking
### Title: Nicotine Gum and Smoking Cessation
### Aliases: smoking
### Keywords: datasets

### ** Examples


  data("smoking", package = "HSAUR2")
  boxplot(smoking$qt/smoking$tt,
          smoking$qc/smoking$tc,
          names = c("Treated", "Control"), ylab = "Percent Quitters")




cleanEx()
nameEx("students")
### * students

flush(stderr()); flush(stdout())

### Name: students
### Title: Student Risk Taking
### Aliases: students
### Keywords: datasets

### ** Examples


  data("students", package = "HSAUR2")
  layout(matrix(1:2, ncol = 2))
  boxplot(low ~ treatment, data = students, ylab = "low")
  boxplot(high ~ treatment, data = students, ylab = "high")




cleanEx()
nameEx("suicides")
### * suicides

flush(stderr()); flush(stdout())

### Name: suicides
### Title: Crowd Baiting Behaviour and Suicides
### Aliases: suicides
### Keywords: datasets

### ** Examples


  data("suicides", package = "HSAUR2")
  mosaicplot(suicides)




cleanEx()
nameEx("toenail")
### * toenail

flush(stderr()); flush(stdout())

### Name: toenail
### Title: Toenail Infection Data
### Aliases: toenail
### Keywords: datasets

### ** Examples


  data("toenail", package = "HSAUR2")





cleanEx()
nameEx("toothpaste")
### * toothpaste

flush(stderr()); flush(stdout())

### Name: toothpaste
### Title: Toothpaste Data
### Aliases: toothpaste
### Keywords: datasets

### ** Examples


  data("toothpaste", package = "HSAUR2")
  toothpaste




cleanEx()
nameEx("voting")
### * voting

flush(stderr()); flush(stdout())

### Name: voting
### Title: House of Representatives Voting Data
### Aliases: voting
### Keywords: datasets

### ** Examples


  data("voting", package = "HSAUR2")
  require("MASS")
  voting_mds <- isoMDS(voting)
  plot(voting_mds$points[,1], voting_mds$points[,2],
       type = "n", xlab = "Coordinate 1", ylab = "Coordinate 2",
       xlim = range(voting_mds$points[,1])*1.2)
  text(voting_mds$points[,1], voting_mds$points[,2], 
       labels = colnames(voting))
  voting_sh <- Shepard(voting[lower.tri(voting)], voting_mds$points)




cleanEx()
nameEx("water")
### * water

flush(stderr()); flush(stdout())

### Name: water
### Title: Mortality and Water Hardness
### Aliases: water
### Keywords: datasets

### ** Examples


  data("water", package = "HSAUR2")
  plot(mortality ~ hardness, data = water, 
       col = as.numeric(water$location))




cleanEx()
nameEx("watervoles")
### * watervoles

flush(stderr()); flush(stdout())

### Name: watervoles
### Title: Water Voles Data
### Aliases: watervoles
### Keywords: datasets

### ** Examples


  data("watervoles", package = "HSAUR2")
  watervoles




cleanEx()
nameEx("waves")
### * waves

flush(stderr()); flush(stdout())

### Name: waves
### Title: Electricity from Wave Power at Sea
### Aliases: waves
### Keywords: datasets

### ** Examples


  data("waves", package = "HSAUR2")
  plot(method1 ~ method2, data = waves)




cleanEx()
nameEx("weightgain")
### * weightgain

flush(stderr()); flush(stdout())

### Name: weightgain
### Title: Gain in Weight of Rats
### Aliases: weightgain
### Keywords: datasets

### ** Examples


  data("weightgain", package = "HSAUR2")
  interaction.plot(weightgain$type, weightgain$source, 
                   weightgain$weightgain)




cleanEx()
nameEx("womensrole")
### * womensrole

flush(stderr()); flush(stdout())

### Name: womensrole
### Title: Womens Role in Society
### Aliases: womensrole
### Keywords: datasets

### ** Examples


  data("womensrole", package = "HSAUR2")
  summary(subset(womensrole, gender == "Female"))
  summary(subset(womensrole, gender == "Male"))




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
