# Dimitri Zaras - 7/8/2021
# Data Analysis for the comparison of entropy values between ASD, MCI, and AD patients
library(ggplot2); library(HSAUR); library(Rmisc); library(car); library(dplyr)

# this dataset contains the variables we want to use for the correlation analyses
dat <- read.csv("~/OneDrive - Emory University/Stats HelpDesk/Nishant/Nishant_7-7-21/ALL_MASTER_UPDATED_EXCEL_FINAL.csv")

# these are the variables we want to conduct correlation tests for

# MMSCORE
hist(dat$MMSCORE)
# CCI12TOT
hist(dat$CCI12TOT)
# CDGLOBAL
hist(dat$CDGLOBAL)
# CDSOB
hist(dat$CDSOB)
# GDTOTAL
hist(dat$GDTOTAL)
# EcogPtTotal
hist(dat$EcogPtTotal)
# EcogSPTotal
hist(dat$EcogSPTotal)

# scatterplots
plot(dat$MMSCORE, dat$CCI12TOT)
plot(dat$MMSCORE, dat$CDSOB)
plot(dat$CDSOB, dat$CDGLOBAL)


# Hampel filter to take care of outliers - for both controls and treatments for the GDTOTAL
lower_bound <- median(dat$GDTOTAL) - 3 * mad(dat$GDTOTAL, constant = 1)
lower_bound

upper_bound <- median(dat$GDTOTAL) + 3 * mad(dat$GDTOTAL, constant = 1)
upper_bound

outlier_ind <- which(dat$GDTOTAL < lower_bound | dat$GDTOTAL > upper_bound)
outlier_ind
#  34  47  55  61  81  88  90  93 103

dat[34,]

# take out case with row number 34, RID = 6314; 

# Hampel filter to take care of outliers - for both controls and treatments for the CCI12TOT
lower_bound <- median(dat$CCI12TOT) - 3 * mad(dat$CCI12TOT, constant = 1)
lower_bound

upper_bound <- median(dat$CCI12TOT) + 3 * mad(dat$CCI12TOT, constant = 1)
upper_bound

outlier_ind <- which(dat$CCI12TOT < lower_bound | dat$CCI12TOT > upper_bound)
outlier_ind

# Hampel filter to take care of outliers - for both controls and treatments for the MMSCORE
lower_bound <- median(dat$MMSCORE) - 3 * mad(dat$MMSCORE, constant = 1)
lower_bound

upper_bound <- median(dat$MMSCORE) + 3 * mad(dat$MMSCORE, constant = 1)
upper_bound

outlier_ind <- which(dat$MMSCORE < lower_bound | dat$MMSCORE > upper_bound)
outlier_ind

boxplot(dat$MMSCORE)

# Hampel filter to take care of outliers - for both controls and treatments for the CDGLOBAL
lower_bound <- median(dat$CDGLOBAL) - 3 * mad(dat$CDGLOBAL, constant = 1)
lower_bound

upper_bound <- median(dat$CDGLOBAL) + 3 * mad(dat$CDGLOBAL, constant = 1)
upper_bound

outlier_ind <- which(dat$CDGLOBAL < lower_bound | dat$CDGLOBAL > upper_bound)
outlier_ind

boxplot(dat$CDGLOBAL)

# Hampel filter to take care of outliers - for both controls and treatments for the EcogPtTotal
lower_bound <- median(dat$EcogPtTotal) - 3 * mad(dat$EcogPtTotal, constant = 1)
lower_bound

upper_bound <- median(dat$EcogPtTotal) + 3 * mad(dat$EcogPtTotal, constant = 1)
upper_bound

outlier_ind <- which(dat$EcogPtTotal < lower_bound | dat$EcogPtTotal > upper_bound)
outlier_ind

boxplot(dat$EcogPtTotal)

# Hampel filter to take care of outliers - for both controls and treatments for the EcogSPTotal
lower_bound <- median(dat$EcogSPTotal) - 3 * mad(dat$EcogSPTotal, constant=1)
lower_bound

upper_bound <- median(dat$EcogSPTotal) + 3 * mad(dat$EcogSPTotal, constant = 1)
upper_bound

outlier_ind <- which(dat$EcogSPTotal < lower_bound | dat$EcogSPTotal > upper_bound)
outlier_ind

boxplot(dat$EcogSPTotal)


IQR.outliers <- function(x) {
  if(any(is.na(x)))
    stop("x is missing values")
  if(!is.numeric(x))
    stop("x is not numeric")
  Q3<-quantile(x,0.75)
  Q1<-quantile(x,0.25)
  IQR<-(Q3-Q1)
  left<- (Q1-(1.5*IQR))
  right<- (Q3+(1.5*IQR))
  c(x[x <left],x[x>right])
}

IQR.outliers(dat$MMSCORE)

IQR.outliers(dat$EcogSPTotal)

# Boxplot of consume variable distribution
boxplot(dat$MMSCORE)

# Calculate quartiles: consume_quartiles
(dat_quartiles <- quantile(dat$MMSCORE))

# Calculate upper threshold: upper_th
upper_th <- dat_quartiles[4] + 
  1.5 * (dat_quartiles[4] - dat_quartiles[2])

# Print the sorted vector of distinct potential outliers
sort(unique(dat$MMSCORE[dat$MMSCORE > upper_th]))

# Calculate lower threshold: lower 5th
lower_th <- dat_quartiles[2] + 
  1.5 * (dat_quartiles[2] - dat_quartiles[4])

# Print the sorted vector of distinct potential outliers
sort(unique(dat$MMSCORE[dat$MMSCORE > lower_th]))



