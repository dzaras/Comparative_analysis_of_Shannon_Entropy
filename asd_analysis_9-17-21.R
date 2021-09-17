# Dimitri Zaras - 6/22/2021
# Data Analysis for the comparison of entropy values between ASD, MCI, and AD patients
library(ggplot2); library(HSAUR); library(Rmisc); library(car); library(dplyr)

# this dataset contains the info for all patients from both original datasets, treatment and control
asd_data <- read.csv("~/OneDrive - Emory University/Stats HelpDesk/Nishant/Nishant_9-17-21/finalMaster2.csv")

# convert "Diagnosis Number" into a factor variable
asd_data$Diagnosis <- as.factor(asd_data$Diagnosis)

asd_data$Dataset <- as.factor(asd_data$Dataset)

# creating a new dataset without the 13 observations with the 'ADNI2' value in the Dataset variable
#asd_dat2 <- asd_data[asd_data$Dataset!="ADNI2",]

# check the controls only
asd_con <- asd_data[asd_data$Clinical==0,]

# create a subset of the controls from only the ABIDEII dataset
asd_con_adni <- asd_con[asd_con$Phase=='ADNI3',]

# create a subset of the controls from only the ABIDEII dataset
asd_con_abideii <- asd_con[asd_con$Phase=='ABIDE II',]

# two-way ANOVA
res1 <- aov(ADN1Entropy ~ Age + Diagnosis, data = asd_data)
summary(res1)

# checking for outliers among the controls
histogram(asd_con$ADN1Entropy)
boxplot(asd_con$ADN1Entropy)

#histogram(asd_data$ADN1Entropy[asd_data$Dataset!="ADNI2"],)

histogram(asd_con$KBIT_Score, breaks=20)
boxplot(asd_con$KBIT_Score)

histogram(asd_con$MMSE.SCORE, breaks=20)
boxplot(asd_con$MMSE.SCORE)

# checking for outliers among controls in terms of MMSE.Score from ADNI only
asd_conADNI1 <- asd_con[asd_con$Dataset=='ADNI1',]
asd_conABIDEII <- asd_con[asd_con$Dataset=='ABIDE II',]


histogram(asd_conADNI1$MMSE.Score)

boxplot(asd_conABIDEII$KBIT.Score)

library(ggplot2)
ggplot(asd_conADNI1,aes(x=MMSE.Score))+geom_boxplot(aes(lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd),stat="identity")

# Box plot with multiple groups
# +++++++++++++++++++++
# Plot tooth length ("len") by groups ("dose")
# Color box plot by a second group: "supp"
library("ggpubr")
ggboxplot(asd_new, x = "Clinical", y = "ADN1Entropy", color = "Dataset",
          palette = c("#00AFBB", "#E7B800"))



# Hampel filter to take care of outliers - for both controls and treatments 
lower_bound <- median(asd_conADNI1$MMSE.Score) - 3 * mad(asd_conADNI1$MMSE.Score, constant = 1)
lower_bound

upper_bound <- median(asd_conADNI1$MMSE.Score) + 3 * mad(asd_conADNI1$MMSE.Score, constant = 1)
upper_bound

outlier_ind <- which(asd_conADNI1$MMSE.Score < lower_bound | asd_conADNI1$MMSE.Score > upper_bound)
outlier_ind

asd_conADNI1[8,]

# take out case with row number 8, Subject.ID = 137_S_6826, and mmse.score = 25 from the asd_conADNI1

# Hampel filter to take care of outliers - for both controls and treatments 
lower_bound <- median(asd_data$KBIT.Score) - 3 * mad(asd_data$KBIT.Score, constant = 1)
lower_bound

upper_bound <- median(asd_data$KBIT.Score) + 3 * mad(asd_data$KBIT.Score, constant = 1)
upper_bound

outlier_ind <- which(asd_data$KBIT.Score < lower_bound | asd_data$KBIT.Score > upper_bound)
outlier_ind

asd_conABIDEII[8,]

# Hampel filter to take care of outliers - 
lower_bound <- median(asd_data$SRS.Total) - 3 * mad(asd_data$SRS.Total, constant = 1)
lower_bound

upper_bound <- median(asd_data$SRS.Total) + 3 * mad(asd_data$SRS.Total, constant = 1)
upper_bound

outlier_ind <- which(asd_data$SRS.Total  < lower_bound | asd_data$SRS.Total > upper_bound)
outlier_ind

# the following is output from late June 2021
which(asd_conABIDEII$SRS.Total==90)
asd_conABIDEII[6,]  # subject ID BNI29020
asd_conABIDEII[20,]  # subject ID BNI29047
asd_conABIDEII[16,] # subject ID BNI_29038 
asd_conABIDEII[17,] # subject ID BNI_29040

## new dataset after excluding outliers according to the Hampel filter
# this dataset contains the info for all patients from both original datasets, treatment and control
asd_new <- read.csv("~/OneDrive - Emory University/Stats HelpDesk/Nishant/Outlierexcluded_ConsolidatedSpreadsheet_6_21_21 copy.csv")

# Hampel filter to take care of outliers - for both controls and treatments 
lower_bound <- median(asd_conADNI1$MMSE.Score) - 3 * mad(asd_conADNI1$MMSE.Score, constant = 1)
lower_bound

upper_bound <- median(asd_conADNI1$MMSE.Score) + 3 * mad(asd_conADNI1$MMSE.Score, constant = 1)
upper_bound

outlier_ind <- which(asd_conADNI1$MMSE.Score < lower_bound | asd_conADNI1$MMSE.Score > upper_bound)
outlier_ind

asd_conADNI1[8,]

# take out case with row number 8, Subject.ID = 137_S_6826, and mmse.score = 25 from the asd_conADNI1
#---------------------------------------------------------------
# implementing Hampel filter to controls Sep 17 2021
# Hampel filter to take care of outliers - for both controls and treatments 
lower_bound <- median(asd_con_abideii$KBIT.Score) - 3 * mad(asd_con_abideii$KBIT.Score, constant = 1)
lower_bound

upper_bound <- median(asd_con_abideii$KBIT.Score) + 3 * mad(asd_con_abideii$KBIT.Score, constant = 1)
upper_bound

outlier_ind <- which(asd_con_abideii$KBIT.Score < lower_bound | asd_con_abideii$KBIT.Score > upper_bound)
outlier_ind

#asd_conABIDEII[8,]

# Hampel filter to take care of outliers - 
lower_bound <- median(asd_con_abideii$SRS.Total) - 3 * mad(asd_con_abideii$SRS.Total, constant = 1)
lower_bound

upper_bound <- median(asd_con_abideii$SRS.Total) + 3 * mad(asd_con_abideii$SRS.Total, constant = 1)
upper_bound

outlier_ind <- which(asd_con_abideii$SRS.Total  < lower_bound | asd_con_abideii$SRS.Total > upper_bound)
outlier_ind

# asd_con_adni check for outliers 9/17/21
# Hampel filter to take care of outliers - for both controls and treatments 
lower_bound <- median(asd_con_adni$MMSE.Score) - 3 * mad(asd_con_adni$MMSE.Score, constant = 1)
lower_bound

upper_bound <- median(asd_con_adni$MMSE.Score) + 3 * mad(asd_con_adni$MMSE.Score, constant = 1)
upper_bound

outlier_ind <- which(asd_con_adni$MMSE.Score < lower_bound | asd_con_adni$MMSE.Score > upper_bound)
outlier_ind

# Hampel filter to take care of outliers - for  controls  for the GDTOTAL 9/17/21
lower_bound <- median(asd_con_adni$GDTOTAL) - 3 * mad(asd_con_adni$GDTOTAL, constant = 1)
lower_bound

upper_bound <- median(asd_con_adni$GDTOTAL) + 3 * mad(asd_con_adni$GDTOTAL, constant = 1)
upper_bound

outlier_ind <- which(asd_con_adni$GDTOTAL < lower_bound | asd_con_adni$GDTOTAL > upper_bound)
outlier_ind

# 2-way ANOVA

res1 <- aov(ADN1Entropy ~ Dataset*Clinical, data = asd_new)
summary(res1)

res1 <- aov(ADN1Entropy ~ Age*Diagnosis, data = asd_data)
summary(res1)

res2 <- aov(ADN1Entropy ~ ADNI1Site*Clinical, data = asd_new)
summary(res2)

# converting scanner model into a categorical varaible
asd_dat2$Scanner.Model <- as.factor(asd_dat2$Scanner.Model)

asd_data$Scanner.Model <- as.factor(asd_data$Scanner.Model)

res3 <- aov(ADN1Entropy ~ Scanner.Model*Clinical, data = asd_data)
summary(res3)

res4 <- aov(ADN1Entropy ~ Age*Clinical, data = asd_data)
summary(res4)

res5 <- aov(ADN1Entropy ~ Age*Dataset, data = asd_new)
summary(res5)

# breaking down the analysis based on the dataset
# ADNI
asd_new_adni <- asd_new[asd_new$Dataset == "ADNI",]

res2 <- aov(ADN1Entropy ~ Scanner.Model*Clinical, data = asd_new_)
summary(res2)

table(asd_new_adni$Scanner.Model)

res3 <- aov(ADN1Entropy ~ Age*Clinical, data = asd_new_abideii)
summary(res3)

table(asd_new_adni$ADNI1Site)

# ABIDEII
asd_new_abideii <- asd_new[asd_new$Dataset == "ABIDE II",]

cor.test(asd_data$MMSE.SCORE, asd_data$Age)

cor.test(asd_data$MMSE.SCORE, asd_data$ADN1Entropy)

cor.test(asd_data$ADN1Entropy, asd_data$Age)





