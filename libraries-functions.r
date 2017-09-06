######## PACKAGES USED ########
library(foreign)
library(dplyr)
library(car)
library(Cairo)
library(cairoDevice)
library(corrplot)
library(effects)
require(ggplot2)
require(GGally)
library(psych)
library(Amelia)
library(corrplot)
library(MASS)
library(gvlma)
library(rms)
library(coefplot)
library(AER)


############ FUNCTIONS ############
#
#

out.stat <- function(x){
  ######## Vector: basic statistics ###########
  # Basic statistics (min, max, mean, SD)
  cat("Min: ", round(min(as.numeric(x), na.rm = TRUE),2),
      "Max: ", round(max(as.numeric(x), na.rm = TRUE),2),
      "Mean: ", round(mean(as.numeric(x), na.rm = TRUE),2),
      "SD: ", round(sd(as.numeric(x), na.rm = TRUE),2)
  )
}

out.tbls.wn <- function(x){
  ######## Vector: detailed summary ###########
  # Frequency table, including and excluding NA
  # Also basic statistics (min, max, mean, SD)
  cat("Variable summary:\n")
  a <- cbind(Freq=table(x, useNA = "ifany"),
             Relative=round(100*prop.table(table(x, useNA = "ifany")), 2),
             Cumul=round(100*cumsum(prop.table(table(x, useNA = "ifany"))),2),
             Relative=round(100*prop.table(table(x)), 2),
             Cumul=round(100*cumsum(prop.table(table(x))),2))
  print(a)
  cat("Ignore warning, if NA present. Also last two columnes for NA.\n")
  if (!(is.numeric(x))) {cat("Not numeric variable! This may not have meaning:\n")}
  cat("Min: ", round(min(as.numeric(x), na.rm = TRUE),2),
      "Max: ", round(max(as.numeric(x), na.rm = TRUE),2),
      "Mean: ", round(mean(as.numeric(x), na.rm = TRUE),2),
      "SD: ", round(sd(as.numeric(x), na.rm = TRUE),2)
  )
}

info.detail <- function(DF){
  ######## More detailed df information ###########
  # returns detailed information on dataframe
  informacije <- sapply(DF, function(x) cbind(min(as.numeric(x), na.rm = TRUE),
                                              max(as.numeric(x), na.rm = TRUE),
                                              mean(as.numeric(x), na.rm = TRUE),
                                              sd(as.numeric(x), na.rm = TRUE),
                                              sum(is.na(x))))
  inform.rounded <- data.frame(
    min=round(informacije[1,],0),
    max=round(informacije[2,],0),
    mean=round(informacije[3,],2),
    SD=round(informacije[4,],2),
    NAs=informacije[5,]
  )
  for (i in (1:nrow(inform.rounded))) {
    if (is.numeric(DF[,i])) {
      inform.rounded[i,"type"] <- "numeric"
      inform.rounded[i,"lvl"] <- "."}
    else if (is.factor(DF[,i])) {
      if (is.ordered(DF[,i]))
        inform.rounded[i,"type"] <- "ordered f."
      else {
        inform.rounded[i,"type"] <- "categ. f."
        inform.rounded[i,1] <- "."
        inform.rounded[i,3] <- "."
        inform.rounded[i,4] <- "."
      }
      inform.rounded[i,"lvl"] <- inform.rounded[i,2]
      inform.rounded[i,2] <- "."
    }
    else {
      inform.rounded[i,"type"] <- "something third"
      inform.rounded[i,"lvl"] <- "."
    }
    if (inform.rounded[i,5] == 0) inform.rounded[i,5] <- "."
  }
  print(inform.rounded)
  cat("Sample size N: ",nrow(DF))
}

average.excluding <- function(G, n){
  ######## AVERAGE EXCLUDING ###########
  # returns the mean of G variables
  # for cases with more than n missing
  # G is dataframe or c(var1, var2, ...)
  apply(G, 1,
        function(x) {
          if (sum(is.na(x)) > n) mean(x)
          else mean(x, na.rm = TRUE)})
}

cor.mtest <- function(mat, conf.level = 0.95){
  ######## SIGNIFICANCE TEST ###########
  # Significance value for use in plotting
  # allows for identification of insignificant
  # corelations (specify level, def. 0.95)
  # From "An intrudction to corrplot package"
  # ftp://cran.r-project.org/pub/R/web/packages/corrplot/vignettes/corrplot-intro.html
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

rec.relig12 <- function(religion, denomination, other) {
  ######## RECODING RELIGION AND RELIGION AT 16 ###########
  # Sherkat and Lehman (2017)
  # To work properly, folder 'Relig' with .csv's of label 
  # names has to be in wokring directory.
  #
  # Function: # rec.relig(religion, denomination, other)
  #	relig or relig16 variable; denom or denom 16; other or oth16
  # function prints frequencies and returns factor vector with
  # religion recoded
  # 
  # it works with GSS dataset imported through 'read.spss',
  # from foreign package, in following way:
  # to.data.frame = TRUE, trim.factor.names = TRUE,
  # trim_values = TRUE, use.missings = FALSE
  
  # Import three varaibles into new dataset used for recoding
  DF <- data.frame(
    relig = religion,
    denom = denomination,
    other = other
  )
  
  # Read values for all variables
  c.relig <- read.csv("Relig/relig.csv")
  c.denom <- read.csv("Relig/denom.csv")
  c.other <- read.csv("Relig/other.csv")
  
  # Create vectors with position corespondign to the code/punch of label in DF codebook for 3 variables 
  c.r <- c()
  for (i in c.relig$code) {
    c.r[i] <- as.character(c.relig[c.relig$code == i, "label"])
  }
  c.r[99] <- "NA"
  
  c.d <- c()
  for (i in c.denom$code) {
    c.d[i] <- as.character(c.denom[c.denom$code == i, "label"])
  }
  c.d[99] <- "NA"
  
  c.o <- c()
  for (i in c.other$code) {
    c.o[i] <- as.character(c.other[c.other$code == i, "label"])
  }
  c.o[999] <- "NA"
  
  # Liberal Protestants
  lp.d.num <- c(40:49)
  lp.o.num <- c(29, 30, 40, 54, 70, 72 , 81, 82, 95, 98, 119, 142, 160, 188)
  lp.denom <- c.d[lp.d.num]
  lp.other <- c.o[lp.o.num]
  DF$lp.true <- DF$denom %in% lp.denom | DF$other %in% lp.other
  DF$rv[DF$lp.true] <- "Liberal Protestant"
  
  # Episcopalians 
  ep.d.num <- c(50)
  ep.denom <- c.d[ep.d.num]
  DF$ep.true <- DF$denom %in% ep.denom
  DF$rv[DF$ep.true] <- "Episcopalian"
  
  # Moderate Protestants
  mp.d.num <- c(10:13, 20:23, 28)
  mp.o.num <- c(1, 8, 15, 19, 25, 32, 42:44, 46, 49:51, 71, 73, 94, 99, 146, 148, 150, 186)
  mp.denom <- c.d[mp.d.num]
  mp.other <- c.o[mp.o.num]
  DF$mp.true <- DF$denom %in% mp.denom | DF$other %in% mp.other
  DF$rv[DF$mp.true] <- "Moderate Protestant"
  
  # Lutherans
  lt.d.num <- c(30:38)
  lt.o.num <- c(105)
  lt.denom <- c.d[lt.d.num]
  lt.other <- c.o[lt.o.num]
  DF$lt.true <- DF$denom %in% lt.denom | DF$other %in% lt.other
  DF$rv[DF$lt.true] <- "Lutheran"
  
  # Baptists
  bp.d.num <- c(14:18)
  bp.o.num <- c(93, 133, 197)
  bp.denom <- c.d[bp.d.num]
  bp.other <- c.o[bp.o.num]
  DF$bp.true <- DF$denom %in% bp.denom | DF$other %in% bp.other
  DF$rv[DF$bp.true] <- "Baptist"
  
  # Sectarian Protestants
  # these initial variables pull out sectarians codes relig=11 (christian) 
  # or relig=5 (other), but also have valid denom codes. 
  DF$sp.pent <- DF$relig == c.r[11] & DF$other == c.o[68]
  DF$sp.centchrist <- DF$relig == c.r[5] & DF$other == c.o[31]
  DF$sp.fsg <- DF$relig == c.r[5] & DF$other == c.o[53]
  DF$sp.jw <- DF$relig == c.r[5] & DF$other == c.o[58]
  DF$sp.sda <- DF$relig == c.r[5] & DF$other == c.o[77]
  DF$sp.ofund <- DF$relig == c.r[5] & DF$other == c.o[97]
  
  sp.o.num <- c(2, 3, 5:7, 9, 10, 12:14, 16:18, 20:24, 26, 27, 31, 33:39, 41,
                45, 47, 48, 52, 53, 55:58, 63, 65:69, 76:79, 83:92, 96, 97, 100:104,
                106:113, 115:118, 120:122, 124, 125, 127:132, 134, 135, 137:141, 144,
                145, 151:156, 158, 159, 166:182, 184, 185, 187, 189:191, 193, 195, 196, 198, 201, 204)
  sp.other <- c.o[sp.o.num]
  
  DF$sp.true <- DF$other %in% sp.other | DF$sp.pent | DF$sp.centchrist | DF$sp.fsg | DF$sp.jw | DF$sp.sda | DF$sp.ofund
  DF$rv[DF$sp.true] <- "Sectarian Protestant"
  
  # Christian, no group identified. 
  DF$cn.christ <- DF$relig == c.r[11] & !DF$sp.pent
  cn.r.num <- c(13)
  cn.d.num <- c(70, 98, 99)
  cn.o.num <- c(998, 999)
  cn.relig <- c.r[cn.r.num]
  cn.denom <- c.d[cn.d.num]
  cn.other <- c.o[cn.o.num]
  DF$cn.true  <- DF$relig %in% cn.relig | DF$denom %in% cn.denom | DF$other %in% cn.other | DF$cn.christ
  DF$rv[DF$cn.true] <- "Christian, no group given"
  
  # Mormons
  mr.o.num <- c(59:62, 64, 157, 162)
  mr.other <- c.o[mr.o.num]
  DF$mr.true <- DF$other %in% mr.other
  DF$rv[DF$mr.true] <- "Mormon"
  
  # Catholics and Orthodox Christians/Protestants? 
  co.r.num <- c(2, 10)
  co.o.num <- c(28, 123, 126, 143, 149, 183, 194)
  co.relig <- c.r[co.r.num]
  co.other <- c.o[co.o.num]
  DF$co.true <- DF$relig %in% co.relig | DF$other %in% co.other
  DF$rv[DF$co.true] <- "Catholic and Orthodox"
  
  # Jews
  jw.r.num <- c(3)
  jw.relig <- c.r[jw.r.num]
  DF$jw.true <- DF$relig %in% jw.relig
  DF$rv[DF$jw.true] <- "Jewish"
  
  # Other religions 
  DF$or.nonsp <- (DF$relig == c.r[5]) & !(DF$sp.pent | DF$sp.centchrist | DF$sp.fsg | DF$sp.jw | DF$sp.sda | DF$sp.ofund)
  or.r.num <- c(6:9, 12)
  or.o.num <- c(11, 74, 75, 80, 114, 136, 161, 163, 164, 192)
  or.relig <- c.r[or.r.num]
  or.other <- c.o[or.o.num]
  DF$or.true <- DF$relig %in% or.relig | DF$other %in% or.other | DF$or.nonsp
  DF$rv[DF$or.true] <- "Other religion"
  
  # No religious identification
  nr.r.num <- c(4)
  nr.relig <- c.r[nr.r.num]
  DF$nr.true <- DF$relig %in% nr.relig
  DF$rv[DF$nr.true] <- "None"
  
  # Missing values  
  # No Answer
  DF$na.relig <- DF$relig == c.r[99]
  DF$na.denom <- DF$denom == c.d[99] 
  DF$na.rd <- DF$na.relig & DF$na.denom
  DF$rv[DF$na.rd] <- "No answer"
  
  # Don't know
  DF$dk.relig <- DF$relig == c.r[98]
  DF$rv[DF$dk.relig] <- "DNTKNW"
  
  # Treat it as factor, reorganize the levels
  DF$rv <- as.factor(DF$rv)
  DF$rv <- factor(DF$rv, levels(DF$rv)[c(14, 1, 3, 9, 8, 7, 5, 2, 10, 6, 13, 12, 4, 11)], ordered = FALSE)
  
  # Provide table with proportions
  print(cbind(Freq=table(DF$rv, useNA = "ifany"),
              Relative=round(100*prop.table(table(DF$rv, useNA = "ifany")), 2),
              Cumul=round(100*cumsum(prop.table(table(DF$rv, useNA = "ifany"))),2)
  ))
  
  # Return the vector with recoded religion
  return(DF$rv)
}

############ IMPORTING DATA ############
#
# importing two datasets, so there is no need to manually deal with different levels of 
# missingdata, in one all missing are as NA, in other, they have individual codes

GSS.14 <- read.spss("GSS2014.sav", to.data.frame = TRUE, trim.factor.names = TRUE,
                    trim_values = TRUE, use.missings = TRUE)
GSS.14.MISS <- read.spss("GSS2014.sav", to.data.frame = TRUE, trim.factor.names = TRUE,
                         trim_values = TRUE, use.missings = FALSE)

# Recoding religion
GSS.14.MISS$religion <- rec.relig12(GSS.14.MISS$relig, GSS.14.MISS$denom, GSS.14.MISS$other)
GSS.14$religion <- GSS.14.MISS$religion

# selecting only cases that did the ISSP module on nationalism 
GSS.14$clseusa.miss <- GSS.14.MISS$clseusa
table(GSS.14$clseusa, GSS.14$clseusa.miss, useNA = "always")
rm.cases <- which(GSS.14$clseusa.miss == "IAP")
GSS.14 <- GSS.14[-rm.cases,]
table(GSS.14$clseusa, GSS.14$clseusa.miss, useNA = "always")
rm(rm.cases)

# Create vectors with names of variables to be used in analysis
var.n.criteria <- c("ambornin", "amcit", "amlived", "amenglsh",
                    "amchrstn", "amgovt", "amfeel")
var.n.criteria.plus <- c(var.n.criteria, "amancstr")	# asked in 04 and 14
var.n.other.plus <- c("amproud1")	# asked in 04 and 14
var.other <- c("year", "sex", "coninc", "age", "born", "race", "citizen", "parcit",
               "region", "religion", "padeg", "madeg", "polviews", "degree", "srcbelt",
               "res16", "xnorcsiz", "size")
var.other.14 <- c(var.other, "vetyears")	# asked only in 14
var.all.14 <- c(var.other.14, var.n.criteria.plus, var.n.other.plus)

# Select only desired varaibles
GSS.14 <- GSS.14[var.all.14]

# Create a backup copy of the dataset
GSS.2014.BACKUP <- GSS.14

info.detail(GSS.14)
summary(GSS.14)

# Remove 50 cases that answered question 'How proud are you of being American?'
# as 'I AM NOT AMERICAN' 
GSS.14 <- subset(GSS.14, ((GSS.14$amproud1!="I AM NOT AMERICAN") | is.na(GSS.14$amproud1)))
GSS.14$amproud1 <- droplevels(GSS.14$amproud1)


###### RECODING #####
#
#
#

# urban rural residence
GSS.14$r.srcbelt <- Recode(GSS.14$srcbelt, "c('SUBURB, 12 LRGST','SUBURB, 13-100')='SUBURB100'; 
                           'OTHER URBAN'='OTH URBAN';'OTHER RURAL'='RURAL'; else='CITY100'",
                           levels = c("CITY100", "SUBURB100", "OTH URBAN", "RURAL"))
# place where lived until age of 16
GSS.14$r.res16 <- Recode(GSS.14$res16, "c('CITY GT 250000','BIG-CITY SUBURB')='LRG CITY & SUB';
                         c('50000 TO 250000','TOWN LT 50000')='CITY';
                         c('FARM','COUNTRY,NONFARM')='RURAL'",
                         levels = c("LRG CITY & SUB", "CITY", "RURAL"))
# religion
GSS.14$r.religion <- Recode(GSS.14$religion, "
                            c('Sectarian Protestant', 'Baptist')='SECT&BAPT';
                            c('Moderate Protestant', 'Liberal Protestant')='MOD&LIB';
                            c('Lutheran', 'Episcopalian', 'Mormon')='LUTH&EPI&MORM';
                            c('Jewish', 'Other religion', 'None')='NONE&OR&JEW';
                            c('Catholic and Orthodox')='CATH&ORTH';
                            c('Christian, no group given')='CHR-NGG';
                            c('DNTKNW','No answer')=NA")
# summary(GSS.14$race)	# no recoding
# summary(GSS.14$sex)	# no recoding
# summary(GSS.14$age)	# no recoding
# but also age into decades to see if there is a break
GSS.14$year.num <- as.character(GSS.14$year)
GSS.14$year.num <- as.numeric(GSS.14$year.num)
GSS.14$age.born <- GSS.14$year.num - GSS.14$age
GSS.14$age.born <- GSS.14$age.born - 1900
GSS.14$age.born <- GSS.14$age.born/10
GSS.14$age.born <- trunc(GSS.14$age.born)
GSS.14$age.born[GSS.14$age.born < 3] <- 2 # grouping 1910 & 1920s
GSS.14$age.born <- (GSS.14$age.born*10)+1900
GSS.14$age.bd <- as.factor(GSS.14$age.born)

GSS.14$veteran <- Recode(GSS.14$vetyears, "'NONE'='NO'; else='YES'")
# immigration background
GSS.14$pcitizens <- Recode(GSS.14$parcit, "'BOTH WERE CITIZENS OF AMERICA'='YES'; NA=NA; else='NO'")
GSS.14$immigrant <- !((GSS.14$citizen %in% c("YES")) & (GSS.14$parcit %in% c("BOTH WERE CITIZENS OF AMERICA"))
                      & (GSS.14$born %in% c("YES")))
GSS.14$immigrant[is.na(GSS.14$citizen) & is.na(GSS.14$parcit) & is.na(GSS.14$born)] <- NA
GSS.14$immigrant <- as.factor(GSS.14$immigrant)
# parents' education
GSS.14$prt.ba <- GSS.14$madeg %in% c("BACHELOR", "GRADUATE") | GSS.14$padeg %in% c("BACHELOR", "GRADUATE")
GSS.14$prt.ba[is.na(GSS.14$madeg) & is.na(GSS.14$padeg)] <- NA
GSS.14$prt.ba <- factor(GSS.14$prt.ba)
# region
GSS.14$r.region.4 <- Recode(GSS.14$region, "c('NEW ENGLAND','MIDDLE ATLANTIC')='NORTHEAST';
                            c('E. NOR. CENTRAL','W. NOR. CENTRAL')='MIDWEST';
                            c('SOUTH ATLANTIC','E. SOU. CENTRAL','W. SOU. CENTRAL')='SOUTH';
                            c('MOUNTAIN','PACIFIC')='WEST'")
# political views - although unrecoded will be used
GSS.14$r.polviews <- Recode(GSS.14$polviews, "c('EXTREMELY LIBERAL','LIBERAL','SLIGHTLY LIBERAL')='LIBERAL';
                            c('SLGHTLY CONSERVATIVE','CONSERVATIVE','EXTRMLY CONSERVATIVE')='CONSERVATIVE'",
                            levels = c("LIBERAL", "MODERATE", "CONSERVATIVE"))
# education
GSS.14$r.degree <- Recode(GSS.14$degree, "c('JUNIOR COLLEGE','HIGH SCHOOL')='HS OR JC';
                          c('BACHELOR','GRADUATE')='BA OR MORE';
                          c('LT HIGH SCHOOL')='LT HS'",
                          levels = c("LT HS", "HS OR JC", "BA OR MORE"))
# income in $20k
GSS.14$r.coninc <- GSS.14$coninc/20000

# calculate criteria of belonging (factor analysis is later)
GSS.14$criteria.plus <- average.excluding(sapply(GSS.14[var.n.criteria.plus], as.numeric), 6)
GSS.14$criteria.plus.05 <- round(GSS.14$criteria.plus/0.5)*0.5	# rounded to nearest 0.5
GSS.14$criteria.plus.1 <- round(GSS.14$criteria.plus)			# rounded to whole number

##### Reducing to regession N
var.regression.14 <- c("criteria.plus", "age", "r.res16", "r.religion", "race", "sex",
                       "veteran", "immigrant", "prt.ba", "r.region.4", "polviews", "r.degree",
                       "r.coninc", "r.srcbelt")
# first remove the one outlier
clm.outlier <- lm(criteria.plus ~ age + sex + race + immigrant + veteran + 
                    prt.ba + r.degree + r.coninc + r.srcbelt +
                    relevel(r.religion, ref = "MOD&LIB") + r.region.4 +
                    relevel(polviews, ref = "MODERATE") + 
                    relevel(r.res16, ref = "CITY"), data = GSS.14)

avPlots(clm.outlier, id.n=2)
leveragePlots(clm.outlier, id.n = 2)
outlierTest(clm.outlier)
influenceIndexPlot(clm.outlier, id.n = 3)
car::influencePlot(clm.outlier, id.n = 3)

GSS.14[2451, var.regression.14]
clm.full.outl <- update(clm.outlier, subset = rownames(GSS.14) != "2451")
summary(clm.full.outl) # r.res16 increased significance
compareCoefs(clm.outlier, clm.full.outl) # r.res16 coef changes, and slightly SE
multiplot(clm.outlier, clm.full.outl, intercept = FALSE)
outlierTest(clm.full.outl)
influenceIndexPlot(clm.full.outl, id.n = 3)
car::influencePlot(clm.full.outl, id.n = 3)

GSS.14[111,var.regression.14]
clm.full.outl <- update(clm.outlier, subset = !(rownames(GSS.14) %in% c("57","2451")))
summary(clm.full.outl)
compareCoefs(clm.outlier, clm.full.outl)
multiplot(clm.outlier, clm.full.outl, intercept = FALSE)
outlierTest(clm.full.outl)
influenceIndexPlot(clm.full.outl, id.n = 3)

# decision to remove only the case #1234 and not 57
GSS.14 <- subset(GSS.14, rownames(GSS.14) != "2451")

# remove all other cases that have missing values

GSS.14$missing <- apply(GSS.14[,var.regression.14], 1, function(x) sum(is.na(x)))
GSS.14 <- subset(GSS.14, missing == 0)


##### FACTOR ANALYSIS ######

# creating numeric matrix for corelations
CRIT <- sapply(GSS.14[var.n.criteria.plus], as.numeric)

# Corelations
res1.crit <- cor.mtest(CRIT, 0.95)
crl.crit <- cor(CRIT, use = "pairwise.complete.obs", method = "spearman")
corrplot::corrplot.mixed(crl.crit, p.mat = res1.crit[[1]],
                         sig.level=0.001, insig = "p-value", upper = "circle", lower = "number",
                         tl.col = 'black', tl.cex = 0.70)
# round(crl.crit, 2) # regular numeric corelations
# hierarchical cluster analysis
corrplot::corrplot(crl.crit, p.mat = res1.crit[[1]], insig = "pch", pch = ".", method = "color",
                   order = "hclust", addrect = 3, tl.col = 'black', tl.cex = 0.70)

# Factor and reliability analysis from one to three factors
crit.factor3 <- principal(CRIT, nfactors=3, rotate="varimax")
crit.factor2 <- principal(CRIT, nfactors=2, rotate="varimax")
crit.factor1 <- principal(CRIT, nfactors=1, rotate="varimax")

print(crit.factor3)

print(crit.factor2)
psych::alpha(subset(CRIT, select = -amgovt))

print(crit.factor1)
psych::alpha(CRIT)

##### UNIVARIATE STATISTICS #####
#
#


# First criteria of belonging
out.stat(GSS.14$criteria.plus)
hist(GSS.14$criteria.plus, breaks = "FD")
out.tbls.wn(GSS.14$criteria.plus.1)
out.tbls.wn(GSS.14$ambornin)
out.tbls.wn(GSS.14$amcit)
out.tbls.wn(GSS.14$amlived)
out.tbls.wn(GSS.14$amenglsh)
out.tbls.wn(GSS.14$amchrstn)
out.tbls.wn(GSS.14$amgovt)
out.tbls.wn(GSS.14$amfeel)
out.tbls.wn(GSS.14$amancstr)

# Other variables
out.stat(GSS.14$age)
out.stat(GSS.14$r.coninc)
out.stat(GSS.14$coninc)

out.tbls.wn(GSS.14$r.res16)
out.tbls.wn(GSS.14$r.religion)
out.tbls.wn(GSS.14$race)
out.tbls.wn(GSS.14$sex)
out.tbls.wn(GSS.14$veteran)
out.tbls.wn(GSS.14$immigrant)
out.tbls.wn(GSS.14$prt.ba)
out.tbls.wn(GSS.14$r.region.4)
out.tbls.wn(GSS.14$polviews)
out.tbls.wn(GSS.14$r.degree)
out.tbls.wn(GSS.14$r.srcbelt)

##### BIVARIATE RELATIONSHIP #####
# first conducting corelations, t-test, and ANOVA
# then creating simple linear models,
# for ploting of the effects, where it's different.
# Also creating effects objects for printing.

t.test(criteria.plus ~ sex, data = GSS.14)
t.test(criteria.plus ~ immigrant, data = GSS.14)
t.test(criteria.plus ~ veteran, data = GSS.14)
t.test(criteria.plus ~ prt.ba, data = GSS.14)
avr.race <- aov(criteria.plus ~ race, data = GSS.14)
avr.res16 <- aov(criteria.plus ~ r.res16, data = GSS.14)
avr.relig <- aov(criteria.plus ~ r.religion, data = GSS.14)
avr.region <- aov(criteria.plus ~ r.region.4, data = GSS.14)
avr.plvw <- aov(criteria.plus ~ polviews, data = GSS.14)
avr.degre <- aov(criteria.plus ~ r.degree, data = GSS.14)
avr.srcblt <- aov(criteria.plus ~ r.srcbelt, data = GSS.14)

summary(avr.race)
TukeyHSD(avr.race)
summary(avr.res16)
TukeyHSD(avr.res16)
summary(avr.relig)
TukeyHSD(avr.relig)
summary(avr.region)
TukeyHSD(avr.region)
summary(avr.plvw)
TukeyHSD(avr.plvw)
summary(avr.degre)
TukeyHSD(avr.degre)
summary(avr.srcblt)
TukeyHSD(avr.srcblt)

bim.age <- lm(criteria.plus ~ age, data = GSS.14)
ef.age <- allEffects(bim.age)
bim.race <- lm(criteria.plus ~ race, data = GSS.14)
eff.race <- allEffects(bim.race)
bim.sex <- lm(criteria.plus ~ sex, data = GSS.14)
eff.sex <- allEffects(bim.sex)
bim.veteran <- lm(criteria.plus ~ veteran, data = GSS.14)
eff.vet <- allEffects(bim.veteran)
bim.immigrant <- lm(criteria.plus ~ immigrant, data = GSS.14)
eff.imm <- allEffects(bim.immigrant)
bim.prt.ba <- lm(criteria.plus ~ prt.ba, data = GSS.14)
eff.pba <- allEffects(bim.prt.ba)
bim.r.region.4 <- lm(criteria.plus ~ r.region.4, data = GSS.14)
eff.region <- allEffects(bim.r.region.4)
bim.polviews <- lm(criteria.plus ~ relevel(polviews, ref = "MODERATE"), data = GSS.14)
bim.polviews.plt <- lm(criteria.plus ~ polviews, data = GSS.14)
eff.plvw <- allEffects(bim.polviews.plt)
bim.r.degree <- lm(criteria.plus ~ r.degree, data = GSS.14)
eff.degre <- allEffects(bim.r.degree)
bim.r.srcbelt <- lm(criteria.plus ~ r.srcbelt, data = GSS.14)
eff.rbelt <- allEffects(bim.r.srcbelt)
bim.r.res16 <- lm(criteria.plus ~ relevel(r.res16, ref = "CITY"), data = GSS.14)
bim.r.res16.plt <- lm(criteria.plus ~ r.res16, data = GSS.14)
eff.res16 <- allEffects(bim.r.res16.plt)
bim.r.religion <- lm(criteria.plus ~ relevel(r.religion, ref = "MOD&LIB"), data = GSS.14)
bim.r.religion.plt <- lm(criteria.plus ~ r.religion, data = GSS.14)
eff.rel <- allEffects(bim.r.religion.plt)
bim.coninc <- lm(criteria.plus ~ coninc, data = GSS.14)
bim.r.coninc <- lm(criteria.plus ~ r.coninc, data = GSS.14)

# first plots.
plot(ef.age)
plot(eff.race)
plot(eff.sex)
plot(eff.vet)
plot(eff.imm)
plot(eff.pba)
plot(eff.region)
plot(eff.plvw)
plot(eff.degre)
plot(eff.rbelt)
plot(eff.rel)
plot(eff.res16)
plot(allEffects(bim.coninc))
plot(allEffects(bim.r.coninc))

# summary of models, and group mean and SE, for whhich probably exists an easier way to obtain.

summary(bim.age)
summary(bim.race)
eff.race
eff.race$race$se
summary(bim.sex)
eff.sex
eff.sex$sex$se
summary(bim.veteran)
eff.vet
eff.vet$veteran$se
summary(bim.immigrant)
eff.imm
eff.imm$immigrant$se
summary(bim.prt.ba)
eff.pba
eff.pba$prt.ba$se
summary(bim.r.region.4)
eff.region
eff.region$r.region.4$se
summary(bim.polviews)
eff.plvw
eff.plvw$polviews$se
summary(bim.r.degree)
eff.degre
eff.degre$r.degree$se
summary(bim.r.srcbelt)
eff.rbelt
eff.rbelt$r.srcbelt$se
summary(bim.r.res16)

eff.res16
eff.res16$r.res16$se
summary(bim.r.religion)
eff.rel
eff.rel$r.religion$se
summary(bim.coninc)
summary(bim.r.coninc)


###### REGRESSION ANALYSIS #####
#
#
#

clm.small <- lm(criteria.plus ~ age + sex + race + immigrant + veteran + prt.ba + r.degree + 
                  r.coninc + r.srcbelt, data = GSS.14)
summary(clm.small)
Anova(clm.small)
coefplot(clm.small)

clm.small.res16 <- update(clm.small, ~ . + relevel(r.res16, ref = "CITY"))
summary(clm.small.res16)
Anova(clm.small.res16)
coefplot(clm.small.res16)

clm.small.plvws <- update(clm.small, ~ . + relevel(polviews, ref = "MODERATE"))
summary(clm.small.plvws)
Anova(clm.small.plvws)
coefplot(clm.small.plvws)

clm.small.regns <- update(clm.small, ~ . + r.region.4)
summary(clm.small.regns)
Anova(clm.small.regns)
coefplot(clm.small.regns)

clm.small.relgs <- update(clm.small, ~ . + relevel(r.religion, ref = "MOD&LIB"))
summary(clm.small.relgs)
Anova(clm.small.relgs)
coefplot(clm.small.relgs)

clm.full <- update(clm.small, ~ . + relevel(r.religion, ref = "MOD&LIB") + r.region.4 +
                     relevel(polviews, ref = "MODERATE") + relevel(r.res16, ref = "CITY"))
summary(clm.full)
Anova(clm.full)
coefplot(clm.full)

#Diagnostics
crPlots(clm.full)
qqPlot(clm.full, main="QQ Plot, with comparison line")

# distribution of studentized residuals
sresid <- studres(clm.full) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) 

# checking for heteroscadiscity
car::residualPlots(clm.full, ~ 1, fitted = TRUE, id.n = 0, quadratic = FALSE, tests = FALSE)
spreadLevelPlot(clm.full)
ncvTest(clm.full)
?ncvTest
ncvTest(clm.full, ~ age + sex + race + immigrant + veteran + 
          prt.ba + r.degree + r.coninc + r.srcbelt + 
          relevel(r.religion, ref = "MOD&LIB") + r.region.4 + 
          relevel(polviews, ref = "MODERATE") + 
          relevel(r.res16, ref = "CITY"), data = GSS.14)

# checking for influential cases - unnecessary, already checked
# avPlots(clm.full, id.n=2)
# leveragePlots(clm.full, id.n = 2)
# outlierTest(clm.full)
# influenceIndexPlot(clm.full, id.n = 3)
# car::influencePlot(clm.full, id.n = 3)

#autocorelation
vif(clm.full)
durbinWatsonTest(clm.full)

# function checking everything
summary(gvlma(clm.full))


# create a coefficient plot with all the models
multiplot(clm.small, clm.small.plvws, clm.small.regns,
          clm.small.relgs, clm.small.res16, clm.full, intercept = FALSE, shorten=TRUE, 
          legend.reverse = TRUE, pointSize = 1.2,
          names = c(" (1) BASIC", "(2) + pol. views", "(3) + regions", "(4) + religion",
                    "(5) + res. until 16", "(6) FULL"),
          newNames = c('age'="age",
                       'sexFEMALE'="sex - female",
                       'raceBLACK'="race - black",
                       'raceOTHER'="race - other",
                       'immigrantTRUE'='immigrant background - yes',
                       'veteranYES'='veteran - yes',
                       'prt.baTRUE'='at least one parent has BA - yes',
                       'r.degreeHS OR JC'='education - HS or JC',
                       'r.degreeBA OR MORE'='education - BA or higher',
                       'r.coninc'="income in $20,000",
                       'r.srcbeltSUBURB100'="pl. - suburban largest 100 SMSA",
                       'r.srcbeltOTH URBAN'="pl. - other urban",
                       'r.srcbeltRURAL'="pl. - rural",
                       'relevel(r.religion, ref = "MOD&LIB")CATH&ORTH'="rel. - catholic or orthodox",
                       'relevel(r.religion, ref = "MOD&LIB")CHR-NGG'="rel. - christian no group given",
                       'relevel(r.religion, ref = "MOD&LIB")LUTH&EPI&MORM'="rel. - luth., episc., or morm.",
                       'relevel(r.religion, ref = "MOD&LIB")NONE&OR&JEW'="rel. - none, jewish, or other",
                       'relevel(r.religion, ref = "MOD&LIB")SECT&BAPT'="rel. - sectarian or baptis",
                       'r.region.4NORTHEAST'="region - northeast",
                       'r.region.4SOUTH'="region - south",
                       'r.region.4WEST'="region - west",
                       'relevel(polviews, ref = "MODERATE")EXTREMELY LIBERAL'="pol.views - extremly lib.",
                       'relevel(polviews, ref = "MODERATE")LIBERAL'="pol.views - liberal",
                       'relevel(polviews, ref = "MODERATE")SLIGHTLY LIBERAL'="pol.views - slightly lib.",
                       'relevel(polviews, ref = "MODERATE")SLGHTLY CONSERVATIVE'="pol.views - slightly cons.",
                       'relevel(polviews, ref = "MODERATE")CONSERVATIVE'="pol.views - conservative",
                       'relevel(polviews, ref = "MODERATE")EXTRMLY CONSERVATIVE'="pol.views - extremly cons.",
                       'relevel(r.res16, ref = "CITY")LRG CITY & SUB'= "res. until 16 - large city or suburb",
                       'relevel(r.res16, ref = "CITY")RURAL'="res. until 16 - rural")
)

# exrtact coeficients and SE into separate files

write.csv(compareCoefs(clm.small, clm.small.regns, clm.small.plvws, clm.small.relgs, clm.small.res16, clm.full),
          file = "coeficientsse.csv")
