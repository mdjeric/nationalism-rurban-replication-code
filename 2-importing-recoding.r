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