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
  c.relig <- read.csv("relig/relig.csv")
  c.denom <- read.csv("relig/denom.csv")
  c.other <- read.csv("relig/other.csv")
  
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