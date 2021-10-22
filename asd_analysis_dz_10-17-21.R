# Dimitri Zaras
# 10/15/21
# ANOVA and correlations for DV: ADN1Entropy 

setwd("~/OneDrive - Emory University/Stats HelpDesk/Nishant/Nishant_10-15-21/")
require(ggplot2); require(HSAUR); require(Rmisc); require(ggstatsplot); require(car); require(dplyr); require(tidyverse);require(FNN);require(dbscan)
require(magrittr)

dat_no_outliers <- read.csv("~/OneDrive - Emory University/Stats HelpDesk/Nishant/Nishant_10-15-21/finalmaster_no-outliers_10-15-21.csv")
dat_no_outliers2 <- read.csv("~/OneDrive - Emory University/Stats HelpDesk/Nishant/Nishant_10-15-21/finalmaster_no-outliers_10-17-21.csv")

# distribution of the DV
hist(dat_no_outliers$ADN1Entropy)
# univariate normality
# Q-Q Plot for variable MPG
qqnorm(dat_no_outliers$ADN1Entropy)
qqline(dat_no_outliers$ADN1Entropy)

set.seed(0)
# perform Shapiro-Wilk test for normality
shapiro.test(dat_no_outliers$ADN1Entropy)
# The p-value of the test turns out to be 9.128e-13. Since this value is less than .05, 
# we can assume the sample data does not come from a population that is normally distributed.

#perform log transformation
log_adn1entropy <- log10(dat_no_outliers$ADN1Entropy)

#create histogram for log-transformed distribution 
hist(log_adn1entropy, col='coral2', main='Log Transformed')

# perform Shapiro-Wilk test for normality
shapiro.test(log_adn1entropy)   # the p-value is still less than 0.05

#perform square root transformation
sqrt_adn1entropy <- sqrt(dat_no_outliers$ADN1Entropy)

#create histogram for log-transformed distribution 
hist(sqrt_adn1entropy, col='coral2', main='Square Root Transformed')

# perform Shapiro-Wilk test for normality
shapiro.test(sqrt_adn1entropy)   # the p-value is still less than 0.05

#perform square root transformation
cube_adn1entropy <- dat_no_outliers$ADN1Entropy^(1/3)

#create histogram for square root-transformed distribution 
hist(cube_adn1entropy, col='coral2', main='Cube Root Transformed')

# although the differen transformations didn't improve much the normality of the distr. of the DV
# if we know that the distribution of ADN1Entropy is normal in the population and that its distribution 
# is approximately normal within the different groups of interest, then we are ok

#Start by visualizing the relationship - one way to visually inspect homogeneity of variance
boxplot(dat_no_outliers$ADN1Entropy ~ dat_no_outliers$Scanner.Model, main = 'ADN1Entropy by Scanner Model', xlab = 'Scanner Model')

boxplot(dat_no_outliers$ADN1Entropy ~ dat_no_outliers$Diagnosis, main = 'ADN1Entropy by Diagnosis', xlab = 'Diagnosis')

#Inspect the average ADN1Entropy for each scanner model group
summarySE(data=dat_no_outliers,measurevar="ADN1Entropy", groupvars = "Scanner.Model")

#Inspect the average ADN1Entropy for each Diagnosis group
summarySE(data=dat_no_outliers,measurevar="ADN1Entropy", groupvars = "Diagnosis")

#Histogram for ADN1Entropy and GE 
hist(dat_no_outliers$ADN1Entropy[dat_no_outliers$Scanner.Model=="GE"])
#Histogram for ADN1Entropy and Philips
hist(dat_no_outliers$ADN1Entropy[dat_no_outliers$Scanner.Model=="Philips"])
#Histogram for ADN1Entropy and Siemens
hist(dat_no_outliers$ADN1Entropy[dat_no_outliers$Scanner.Model=="Siemens"])

# # by default leveneTest will test variance around the median
leveneTest(y = dat_no_outliers$ADN1Entropy, group = dat_no_outliers$Scanner.Model)

# The independent samples t-test and ANOVA utilize the t and F statistics respectively, 
# which are generally robust to violations of the assumption as long as group sizes are equal. 
# Equal group sizes may be defined by the ratio of the largest to smallest group being less than 1.5. 
table(dat_no_outliers$Scanner.Model) 

fligner.test(ADN1Entropy ~ Scanner.Model, data = dat_no_outliers)

# if we were to delete the 4 data points from the Siemens group that have an ADN1Entropy value of less than 0.86
which(dat_no_outliers$ADN1Entropy < 0.86) # row indexes in the dat_no_outliers dataframe: 4  22  54  57 101

# new dataframe 
dat_no_outliers2 <- dat_no_outliers[!dat_no_outliers$ADN1Entropy < 0.86,]

hist(dat_no_outliers2$ADN1Entropy)
# univariate normality
# Q-Q Plot for variable MPG
qqnorm(dat_no_outliers2$ADN1Entropy)
qqline(dat_no_outliers2$ADN1Entropy)

set.seed(0)
# perform Shapiro-Wilk test for normality
shapiro.test(dat_no_outliers2$ADN1Entropy)
# The p-value of the test turns out to be 0.0001051. Since this value is less than .05, 
# we can assume the sample data does not come from a population that is normally distributed - but this result is better than before

# Homogeneity of Variances
boxplot(dat_no_outliers2$ADN1Entropy ~ dat_no_outliers2$Scanner.Model, main = 'ADN1Entropy by Scanner Model', xlab = 'Scanner Model')

boxplot(dat_no_outliers2$ADN1Entropy ~ dat_no_outliers2$Diagnosis, main = 'ADN1Entropy by Diagnosis', xlab = 'Diagnosis')

#Inspect the average ADN1Entropy for each scanner model group
summarySE(data=dat_no_outliers2, measurevar="ADN1Entropy", groupvars = "Scanner.Model")   # the sd's are much closer for the groups now

#Inspect the average ADN1Entropy for each Diagnosis group
summarySE(data=dat_no_outliers2,measurevar="ADN1Entropy", groupvars = "Diagnosis")

# Bartlett Test of Homogeneity of Variances
bartlett.test(dat_no_outliers2$ADN1Entropy ~ dat_no_outliers2$Scanner.Model, data= dat_no_outliers2)

# Figner-Killeen Test of Homogeneity of Variances
fligner.test(dat_no_outliers2$ADN1Entropy ~ dat_no_outliers2$Scanner.Model, data= dat_no_outliers2)

# save the new dataset with 175 total observations
write.csv(dat_no_outliers2, file = "finalmaster_no-outliers_10-17-21.csv", row.names = F)


res1 <- aov(ADN1Entropy ~ Scanner.Model*Age, data = dat_no_outliers)
summary(res1)

res1b <- aov(ADN1Entropy ~ Scanner.Model*Age, data = dat_no_outliers2)
summary(res1b)

res2 <- aov(ADN1Entropy ~ Scanner.Model*Clinical, data = dat_no_outliers)
summary(res2)

res2b <- aov(ADN1Entropy ~ Scanner.Model*Clinical, data = dat_no_outliers2)
summary(res2b)

res3 <- aov(ADN1Entropy ~ Scanner.Model*Diagnosis, data = dat_no_outliers)
summary(res3)

res3b <- aov(ADN1Entropy ~ Scanner.Model*Diagnosis, data = dat_no_outliers2)
summary(res3b)

res4 <- aov(ADN1Entropy ~ Age*Diagnosis, data = dat_no_outliers)
summary(res4)

res5 <- aov(ADN1Entropy ~ Age + Diagnosis + Scanner.Model, data = dat_no_outliers)
summary(res5)

res5b <- aov(ADN1Entropy ~ Age + Diagnosis + Scanner.Model, data = dat_no_outliers2)
summary(res5b)

m1 <- lm(logADN1Entropy ~ Age + Diagnosis + Scanner.Model, data = dat_no_outliers)
summary(m1)

interaction.plot(x.factor     = dat_no_outliers$Age,
                 trace.factor = dat_no_outliers$Diagnosis,
                 response     = dat_no_outliers$ADN1Entropy,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")
