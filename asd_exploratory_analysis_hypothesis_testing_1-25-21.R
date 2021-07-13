# Dimitri Zaras - 1/25/2021
# Preliminary Data Analysis for the comparison of entropy values between ASD, MCI, and AD patients
library(ggplot2); library(HSAUR); library(Rmisc); library(car); library(dplyr)

# this dataset contains the info for all patients from both original datasets, treatment and control
asd_data <- read.csv("~/OneDrive - Emory University/Stats HelpDesk/Nishant/asd_data.csv")

asd_data$Group <- as.factor(asd_data$Group)
asd_data$Scanner.Model <- as.factor(asd_data$Scanner.Model)
asd_data$Scanner.Manufacturer <- as.factor(asd_data$Scanner.Manufacturer)
asd_data$Dataset <- as.factor(asd_data$Dataset)

summary(asd_data)
hist(asd_data$FA.Entopy.Value, breaks=20,main="Histogram of Entropy Values",
     xlab="FA Entropy Value", ylab="Group")
# create a subset with only the treatments without the controls
tr_dat <- asd_data[asd_data$Group != 'CN',]

summary(tr_dat)

# in order to get rid of the CN level in the Group variable
tr_dat$Group <- as.character(tr_dat$Group)
tr_dat$Group <- as.factor(tr_dat$Group)
summary(tr_dat$Group)

# descriptive stats for treatment
table1 <- table(tr_dat$Group)
margin.table(table1,1)

boxplot(tr_dat$Age ~ tr_dat$Group)
boxplot(tr_dat$FA.Entopy.Value ~ tr_dat$Group)
boxplot(tr_dat$FA.Entopy.Value ~ tr_dat$Scanner.Manufacturer)

hist(tr_dat$FA.Entopy.Value, breaks = 40)

# 1) create a subset with only the obs from the ABIDEII dataset to run a t-test between treatment
# and control for ASD in terms of FA entropy values

abideii <- asd_data[asd_data$Dataset == 'ABIDEII',]

# get rid of the 2 levels of the Group variable that are not used anymore
abideii$Group <- as.character(abideii$Group)

summary(abideii$Group)
abideii$Group <- as.factor(abideii$Group)

summary(abideii)

# in case we want to compare entropy values for ASD and age
plot(abideii$Age[abideii$Group=='ASD'], abideii$FA.Entopy.Value[abideii$Group =='ASD'])

#Use summarySE to compare. Recall that:
## measurevar=quantitative variable of interest,
## groupvars = categorical variable
summarySE(data= abideii, measurevar = "FA.Entopy.Value", groupvars = "Group")

#Histogram for those who have ASD
hist(abideii$FA.Entopy.Value[abideii$Group=="ASD"], breaks=20,main="Subjects that have ASD",
     xlab="FA Entropy Values for those who have ASD")

#Histogram for those who have ASD
hist(abideii$FA.Entopy.Value[abideii$Group=="CN"], breaks=20, main="Subjects that do not have ASD",
     xlab="FA Entropy Values for those who do not have ASD")

sd(abideii$FA.Entopy.Value[abideii$Group=='ASD'])
sd(abideii$FA.Entopy.Value[abideii$Group=='CN'])
sd(abideii$FA.Entopy.Value)
summary(abideii)
sd(abideii$FA.Entopy.Value)

#Perform two sample t-test
t.test(abideii$FA.Entopy.Value ~  abideii$Group, var.equal=FALSE)

# 2) perform a two sample t-test between subjects from ADNI and ABIDEII to check whether
# they differ significantly in terms of age

hist(tr_dat$Age[tr_dat$Dataset == "ADNI"], breaks = 20, main="Age of Subjects from the ADNI data set", 
     xlab = "Age for ADNI sujects")

hist(tr_dat$Age[tr_dat$Dataset == "ABIDEII"], breaks = 20, main="Age of Subjects from the ABIDEII data set", 
     xlab = "Age for ABIDEII sujects")

sd(tr_dat$Age[tr_dat$Dataset == 'ADNI'])
sd(tr_dat$Age[tr_dat$Dataset == 'ABIDEII'])

summary(tr_dat$Age[tr_dat$Dataset == 'ADNI'])
summary(tr_dat$Age[tr_dat$Dataset == 'ABIDEII'])

#Perform two sample t-test
t.test(tr_dat$Age ~  tr_dat$Dataset, var.equal=FALSE)

# 3) differences in Entropy Value based on Scanner Manufacturer
summary(tr_dat$Scanner.Manufacturer)
table(tr_dat$Scanner.Manufacturer, tr_dat$Group)

# create a new subset where we have enough obs. within each disease category for each 
# scanner manufacturer
#mci <- tr_dat[tr_dat$Group == 'MCI',]
non_mci <- tr_dat[tr_dat$Group != 'MCI',]

boxplot(non_mci$FA.Entopy.Value ~ non_mci$Scanner.Manufacturer)

summarySE(data = non_mci, measurevar = 'FA.Entopy.Value', groupvars = 'Scanner.Manufacturer')

hist(non_mci$FA.Entopy.Value[non_mci$Scanner.Manufacturer=='GE'])
hist(non_mci$FA.Entopy.Value[non_mci$Scanner.Manufacturer=='Philips'])
hist(non_mci$FA.Entopy.Value[non_mci$Scanner.Manufacturer=='Siemens'])

# before conducting ANOVA, we test for homogeneity of variance to see if we must use Kruskal-Wallis instead

# the results show that we don't have homogeneity of variance and we also don't have normality of data
# so, we should use a Kruskal-Wallis test instead of a one-way ANOVA

# Conduct ANOVA
#anova.scanner <- aov(mci$FA.Entopy.Value ~ mci$Scanner.Manufacturer)
# View results
#summary(anova.scanner)

#Perform Tukey Test
#TukeyHSD(anova.scanner)

#Visually inspect results
#plot(TukeyHSD(anova.scanner))

kruskal.test(FA.Entopy.Value ~ Scanner.Manufacturer, data = non_mci)

# As the p-value is less than the significance level 0.05,
# we can conclude that there are significant differences between the treatment groups

# pairwise comparison
pairwise.wilcox.test(non_mci$FA.Entopy.Value, non_mci$Scanner.Manufacturer,
                     p.adjust.method = "BH")
# the results show that only the pairs MCI-ASD and MCI-AD are significantly different (p<0.05)


# 4 Analysis of FA Entropy Value 
# update 2/10/21: I changed the data from tr_dat to asd_data to include CN in the analysis
summary(asd_data$FA.Entopy.Value)
summarySE(asd_data, measurevar = 'FA.Entopy.Value', groupvars = 'Group')

boxplot(asd_data$FA.Entopy.Value ~ asd_data$Group)
# we also use Levene's Test for the purpose of checking homogeneity of variance 
# we can't use Bartlett's because our data are not normally distributed
leveneTest(y = asd_data$FA.Entopy.Value, group = asd_data$Group)

# just to be sure, we also conduct a Fligner-Killeen's Test for 
fligner.test(FA.Entopy.Value ~ Group, data = asd_data)

# Kruskal-Wallis Test

kruskal.test(FA.Entopy.Value ~ Group, data = asd_data)

# As the p-value is less than the significance level 0.05,
# we can conclude that there are significant differences between the treatment groups

# pairwise comparison
pairwise.wilcox.test(asd_data$FA.Entopy.Value, asd_data$Group,
                     p.adjust.method = "BH")
# the results show that only the pairs MCI-ASD and MCI-AD are significantly different (p<0.05)


# 5)
# Clinical Correlations

abide <- read.csv("~/OneDrive - Emory University/Stats HelpDesk/Nishant/asd_abideii_srs_data_v2_noSRStotal.csv")

# Correlations of Entropy values and KBIT scores for ASD and CN
cor.test(abide$FA_Entropy_Value[abide$Group == "ASD"], abide$KBIT_Score[abide$Group == "ASD"])

abide_asd <- abide[abide$Group == "ASD",]
abide_cn <- abide[abide$Group == "CN",]

plot(abide_asd$KBIT_Score, abide_asd$FA_Entropy_Value)
abline(lm(abide_asd$FA_Entropy_Value ~ abide_asd$KBIT_Score))

cor.test(abide_cn$FA_Entropy_Value, abide_cn$KBIT_Score)

plot(abide_cn$KBIT_Score, abide_cn$FA_Entropy_Value)
abline(lm(abide_cn$FA_Entropy_Value ~ abide_cn$KBIT_Score))

# Correlations of Entropy Values and SRS Awareness for ASD and CN
cor.test(abide_asd$FA_Entropy_Value, abide_asd$SRS_Awareness)

plot(abide_asd$SRS_Awareness, abide_asd$FA_Entropy_Value)
abline(lm(abide_asd$FA_Entropy_Value ~ abide_asd$SRS_Awareness))

cor.test(abide_cn$FA_Entropy_Value, abide_cn$SRS_Awareness)

plot(abide_cn$SRS_Awareness, abide_cn$FA_Entropy_Value)
abline(lm(abide_cn$FA_Entropy_Value ~ abide_cn$SRS_Awareness))

# Correlations of Entropy Values and SRS Cognition for ASD and CN
cor.test(abide_asd$FA_Entropy_Value, abide_asd$SRS_Cognition)

plot(abide_asd$SRS_Cognition, abide_asd$FA_Entropy_Value)
abline(lm(abide_asd$FA_Entropy_Value ~ abide_asd$SRS_Cognition))

cor.test(abide_cn$FA_Entropy_Value, abide_cn$SRS_Cognition)

plot(abide_cn$SRS_Cognition, abide_cn$FA_Entropy_Value)
abline(lm(abide_cn$FA_Entropy_Value ~ abide_cn$SRS_Cognition))

# Correlations of Entropy Values and SRS Communication for ASD and CN
cor.test(abide_asd$FA_Entropy_Value, abide_asd$SRS_Communication)

plot(abide_asd$SRS_Communication, abide_asd$FA_Entropy_Value)
abline(lm(abide_asd$FA_Entropy_Value ~ abide_asd$SRS_Communication))

cor.test(abide_cn$FA_Entropy_Value, abide_cn$SRS_Communication)

plot(abide_cn$SRS_Communication, abide_cn$FA_Entropy_Value)
abline(lm(abide_cn$FA_Entropy_Value ~ abide_cn$SRS_Communication))

# Correlations of Entropy Values and SRS Motivation for ASD and CN
cor.test(abide_asd$FA_Entropy_Value, abide_asd$SRS_Motivation)

plot(abide_asd$SRS_Motivation, abide_asd$FA_Entropy_Value)
abline(lm(abide_asd$FA_Entropy_Value ~ abide_asd$SRS_Motivation))

cor.test(abide_cn$FA_Entropy_Value, abide_cn$SRS_Motivation)

plot(abide_cn$SRS_Motivation, abide_cn$FA_Entropy_Value)
abline(lm(abide_cn$FA_Entropy_Value ~ abide_cn$SRS_Motivation))

# Correlations of Entropy Values and SRS Mannerisms for ASD and CN
cor.test(abide_asd$FA_Entropy_Value, abide_asd$SRS_Mannerisms)

plot(abide_asd$SRS_Mannerisms, abide_asd$FA_Entropy_Value)
abline(lm(abide_asd$FA_Entropy_Value ~ abide_asd$SRS_Mannerisms))

cor.test(abide_cn$FA_Entropy_Value, abide_cn$SRS_Mannerisms)

plot(abide_cn$SRS_Mannerisms, abide_cn$FA_Entropy_Value)
abline(lm(abide_cn$FA_Entropy_Value ~ abide_cn$SRS_Mannerisms))

# t-test between asd and cn to check for significant difference in terms of SRS_Mannerisms
t.test(abide$FA_Entropy_Value ~  abide$Group, var.equal=FALSE)

# correlation of SRS Mannerisms and SRS Motivation within ASD subjects
cor.test(abide_asd$SRS_Motivation, abide_asd$SRS_Mannerisms)
# correlation of SRS Cognition and SRS Awareness within ASD subjects
cor.test(abide_asd$SRS_Cognition, abide_asd$SRS_Awareness)

# linear regression

model1 <- lm(asd_data$FA.Entopy.Value ~ asd_data$Group + asd_data$Age + asd_data$Scanner.Manufacturer)
summary(model1)

rstandard(model1) #standardized residuals
#produce qq plot
qqnorm(rstandard(model1))
#add line to qq plot
qqline(rstandard(model1))

model1$residuals #regular residuals
resid(model1) #regular residuals
rstandard(model1) #standardized residuals
predict(model1) #predicted values
#It is hard to interpret the residuals with these lists, so we should inspect them visually.
hist(rstandard(model1))

plot(predict(model1),rstandard(model1),xlab="Fitted Values",ylab="Standardized Residuals")
abline(h=0,lty=2)

model2 <- lm(asd_data$FA.Entopy.Value ~ asd_data$Group*asd_data$Scanner.Manufacturer + asd_data$Age )
summary(model2)

rstandard(model2) #standardized residuals
#produce qq plot
qqnorm(rstandard(model2))
#add line to qq plot
qqline(rstandard(model2))

plot(predict(model2),rstandard(model2),xlab="Fitted Values",ylab="Standardized Residuals")
abline(h=0,lty=2)

# create new subset
asd_data2 <- asd_data[asd_data$Group != 'AD',]
asd_data3 <- asd_data2[asd_data2$Scanner.Manufacturer == 'Philips',]

# getting rid of the AD level from the Group variable because it has zero obs.
asd_data3$Group = as.character(asd_data3$Group)
asd_data3$Group = as.factor(asd_data3$Group)

summary(asd_data3$Group)

summary(asd_data3)

model3 <- lm(asd_data3$FA.Entopy.Value ~ asd_data3$Group*asd_data3$Age )
summary(model3)

model3$residuals #regular residuals
resid(model3) #regular residuals
rstandard(model3) #standardized residuals
predict(model3) #predicted values
#It is hard to interpret the residuals with these lists, so we should inspect them visually.
hist(rstandard(model3))
#produce qq plot
qqnorm(rstandard(model3))
#add line to qq plot
qqline(rstandard(model3))
#numeric summary of residuals
summary(rstandard(model3))
sd(rstandard(model3))

plot(predict(model3),rstandard(model3),xlab="Fitted Values",ylab="Standardized Residuals")
abline(h=0,lty=2)
