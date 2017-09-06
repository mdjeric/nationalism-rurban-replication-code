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