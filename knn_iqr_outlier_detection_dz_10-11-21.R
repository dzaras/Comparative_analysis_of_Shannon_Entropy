# Dimitri Zaras - 10-11-21
# Outlier detection using interquartile range (IQR) and K-Nearest-Neighbors methods
# for variables used in the comparison of entropy values between ASD, MCI, and AD patients

require(ggplot2); require(HSAUR); require(Rmisc); require(ggstatsplot); require(car); require(dplyr); require(tidyverse);require(FNN);require(dbscan)
require(magrittr)

# import the dataset 
dat <- read.csv("~/OneDrive - Emory University/Stats HelpDesk/Nishant/Nishant_10-04-21/finalmaster_statsready.csv")
# check the number of observations in each dataset - indicated by the variable 'Phase'
table(dat$Phase)

# create a subset of only the observations included in the ABIDE II dataset
adni <- dat[dat$Phase=='ADNI3',]
# Create separate dataset with just CON in ADNI
adni_con <- dat[dat$Clinical==0 & dat$Phase=='ADNI3',]
#check if that worked
table(dat$Diagnosis, dat$Phase)
nrow(adni_con)  # 69 rows, so we're good

# Create separate dataset with just 'AD' diagnoses in ADNI
adni_ad <- dat[dat$Diagnosis.Number==4 & dat$Phase=='ADNI3',]
#check if that worked
table(dat$Diagnosis, dat$Phase)
nrow(adni_ad)  # 14 rows, so we're good

# create a subset of only the observations included in the ABIDE II dataset
abide <- dat[dat$Phase=='ABIDE II',]
abide <- abide[abide$RID != 'BNI_29046',]  # deleting this observation because of missing values for SRS and other measures

# Create separate dataset with just controls (CON) in ADNI
abide_con <- dat[dat$Clinical==0 & dat$Phase=='ABIDE II',]
# Select only 'ASD' diagnosis cases from ABIDE II
abide_asd <- dat[dat$Clinical==1 & dat$Phase=='ABIDE II',]

# 	Create separate dataset with just MCI in ADNI
adni_mci <- dat[dat$Diagnosis.Number==3 & dat$Phase=='ADNI3',]

# Outlier Detection in the finalmaster_forstats.csv dataset
# using the IQR method because we want to detect outliers at univariate distributed variables, and we don't have to worry about 
# normal distribution

var_to_be_checked <- abide_con$SRS.Total    # here we specify the variable we're interested in checking for outliers

# A Function to give the indices of the data points identified as outliers using the IQR method and the boxplots.stats function

boxplot_iqr_outlier_detection <- function(var_to_be_checked) {
  
  # Boxplot of variable of interest
  boxplot(var_to_be_checked)
  # use .stats function to calculate iqr and outliers
  out <- boxplot.stats(var_to_be_checked)$out
  # pass the index numbers of the ouliers into a new object called 'out_ind'
  out_ind <- which(var_to_be_checked %in% c(out))
  # Print the row index numbers of the outliers in the dataframe that the variable is found (e.g., abide_con)
  out_ind
}

outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)

# Function for finding the RID for outliers in the abide_con dataframe
outliers_RID_abide_con <- function(outliers) {
  abide_con_var_outliers <- data.frame(matrix(nrow = length(outliers), ncol = 1))
  column_titles <- c("RID")
    colnames(abide_con_var_outliers) <- column_titles
  # for-loop over the two columns of interest
  for(i in (outliers)) {
    abide_con_var_outliers[i,"RID"] <- abide_con[ i, "RID"]
  }
  na.omit(abide_con_var_outliers) 
}

outliers_RID_abide_con(outliers)

# Function for finding the RID for outliers in the adni_con dataframe
outliers_RID_adni_con <- function(outliers) {
  adni_con_var_outliers <- data.frame(matrix(nrow = length(outliers), ncol = 1))
  column_titles <- c("RID")
    colnames(adni_con_var_outliers) <- column_titles
  # for-loop over the two columns of interest
  for(i in (outliers)) {
    adni_con_var_outliers[i,"RID"] <- adni_con[ i, "RID"]
  }
  na.omit(adni_con_var_outliers) 
}

outliers_RID_adni_con(outliers)

# Function for finding the RID for outliers in the adni_ad dataframe
outliers_RID_adni_ad <- function(outliers) {
  adni_ad_var_outliers <- data.frame(matrix(nrow = length(outliers), ncol = 1))
  column_titles <- c("RID")
    colnames(adni_ad_var_outliers) <- column_titles
  # for-loop over the two columns of interest
  for(i in (outliers)) {
    adni_ad_var_outliers[i,"RID"] <- adni_ad[ i, "RID"]
  }
  na.omit(adni_ad_var_outliers) 
}


# Function for finding the RID for outliers in the adni_mci dataframe
outliers_RID_adni_mci <- function(outliers) {
  adni_mci_var_outliers <- data.frame(matrix(nrow = length(outliers), ncol = 1))
  column_titles <- c("RID")
  colnames(adni_mci_var_outliers) <- column_titles
  # for-loop over the two columns of interest
  for(i in (outliers)) {
    adni_mci_var_outliers[i,"RID"] <- adni_mci[ i, "RID"]
  }
  na.omit(adni_mci_var_outliers) 
}
outliers_RID_adni_mci(outliers)

# doing the same processs for the variable KBIT.Score for controls from the ABIDE II dataset
var_to_be_checked <- abide_con$KBIT_Score
boxplot_iqr_outlier_detection(var_to_be_checked)  # in this case, we have zero data points identified as outliers 
ggbetweenstats(abide_con, plot.type = "box", Clinical, KBIT_Score, outlier.tagging = TRUE) #Create boxplot that labels the outliers

# abide_asd SRS.Total
var_to_be_checked <- abide_asd$SRS.Total
boxplot_iqr_outlier_detection(var_to_be_checked)  #  zero data points identified as outliers 
ggbetweenstats(abide_asd, plot.type = "box", Clinical, SRS.Total, outlier.tagging = TRUE)

# abide_asd KBIT_Score
var_to_be_checked <- abide_asd$KBIT_Score
boxplot_iqr_outlier_detection(var_to_be_checked)  # zero data points identified as outliers 
ggbetweenstats(abide_asd, plot.type = "box", Clinical, KBIT_Score, outlier.tagging = TRUE)

# adni_con MMSE.Score
var_to_be_checked <- adni_con$MMSE.SCORE
boxplot_iqr_outlier_detection(var_to_be_checked)  # 6 data points identified as outliers 
outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)
outliers_RID_adni_con(outliers)
ggbetweenstats(adni_con, plot.type = "box", Clinical, MMSE.Score, outlier.tagging = TRUE)

# adni_con GDTOTAL
var_to_be_checked <- adni_con$GDTOTAL
boxplot_iqr_outlier_detection(var_to_be_checked)  # 4 data points identified as outliers 
outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)
outliers_RID_adni_con(outliers)

# adni_con CCI12TOT
var_to_be_checked <- adni_con$CCI12TOT
boxplot_iqr_outlier_detection(var_to_be_checked)  # 5 data points identified as outliers 
outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)
outliers_RID_adni_con(outliers)

# adni_ad MMSE.Score
var_to_be_checked <- adni_ad$MMSE.SCORE
boxplot_iqr_outlier_detection(var_to_be_checked)  # no data points identified as outliers 
outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)
outliers_RID_adni_ad(outliers)
ggbetweenstats(adni_ad, plot.type = "box", Clinical, MMSE.SCORE, outlier.tagging = TRUE)

# adni_ad GDTOTAL
var_to_be_checked <- adni_ad$GDTOTAL
boxplot_iqr_outlier_detection(var_to_be_checked)  # 1 data point identified as outliers 
outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)
outliers_RID_adni_ad(outliers)
ggbetweenstats(adni_ad, plot.type = "box", Clinical, GDTOTAL, outlier.tagging = TRUE)

# adni_ad CCI12TOT
var_to_be_checked <- adni_ad$CCI12TOT
boxplot_iqr_outlier_detection(var_to_be_checked)  # 5 data points identified as outliers 
outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)
outliers_RID_adni_ad(outliers)
ggbetweenstats(adni_ad, plot.type = "box", Clinical, CCI12TOT, outlier.tagging = TRUE)

# adni_ad MMSE.Score
var_to_be_checked <- adni_mci$MMSE.SCORE
boxplot_iqr_outlier_detection(var_to_be_checked)  # 0 data points identified as outliers 
outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)
outliers_RID_adni_mci(outliers)
# ggbetweenstats(adni_mci, plot.type = "box", Clinical, MMSE.Score, outlier.tagging = TRUE)

# adni_mci GDTOTAL
var_to_be_checked <- adni_mci$GDTOTAL
boxplot_iqr_outlier_detection(var_to_be_checked)  # 0 data points identified as outliers 
outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)
outliers_RID_adni_mci(outliers)
ggbetweenstats(adni_mci, plot.type = "box", Clinical, GDTOTAL, outlier.tagging = TRUE)

# adni_mci CCI12TOT
var_to_be_checked <- adni_mci$CCI12TOT
boxplot_iqr_outlier_detection(var_to_be_checked)  #  data points identified as outliers 
outliers <- boxplot_iqr_outlier_detection(var_to_be_checked)
outliers_RID_adni_mci(outliers)
ggbetweenstats(adni_mci, plot.type = "box", Clinical, CCI12TOT, outlier.tagging = TRUE)

# this is the vector with the 18 RID's we want to delete from the original dataset (imported as 'dat')
ids_to_be_deleted <- c("6098" ,"6371" ,"6624", "6717", "6826", "6896", "6314", "6470", "6566",
                       "6147", "6567", "6578", "6969", "6683", "6389", "BNI_29038", "BNI_29040", "BNI_29047")
# dat_no_outliers is the dataframe without the outliers
dat_no_outliers <- dat[!dat$RID %in% ids_to_be_deleted,]
table(dat_no_outliers$Diagnosis, dat_no_outliers$Phase)

write.csv(dat_no_outliers, file = "finalmaster_no-outliers.csv", row.names = F)

# KNN Outlier Detection +++++++++++++++++++++++++++++++++++++++++++++++++
# when we are interested in detecting outliers in a bivariate distribution

# Scale data and create scatterplot: abide_scaled
abide_srstotal_scaled <- as.data.frame(scale(abide$SRS.Total))
abide_kbitscore_scaled <- as.data.frame(scale(abide$KBIT_Score))

colnames(abide_srstotal_scaled) <- 'SRS.Total'
colnames(abide_kbitscore_scaled) <- 'KBIT_Score'
abide_scaled <- cbind(abide_srstotal_scaled, abide_kbitscore_scaled) # there's a NA value in row 41

plot(SRS.Total ~ KBIT_Score, data = abide_scaled, 
     main = 'SRS Total vs. KBIT_Score')

# Compute KNN score
abide_knn <- get.knn(data = abide_scaled, k = 7)
abide1 <- abide
abide1$knn_score <- rowMeans(abide_knn$nn.dist)

# Print top 5 KNN scores and data point indices: top5_knn
(top5_knn <- order(abide1$knn_score, decreasing = TRUE)[1:5])
# [1]  6 46  1 37 32
print(abide1$knn_score[top5_knn])
# [1] 1.130860 1.125681 1.048313 1.040129 1.022978

# Plot variables using KNN score as size of points
plot(SRS.Total ~ KBIT_Score, data = abide1, cex = knn_score, pch = 20)

# The LOF (Local Outlier Factor) score
# I have already scaled the abide dataset

# Add lof_score column to abide1
#abide1 <- abide
abide1$lof_score <- lof(abide_scaled, minPts = 7)

# Print top 5 LOF scores and data point indices: top5_lof
(top5_lof <- order(abide1$lof_score, decreasing = TRUE)[1:5])
# [1]  6 32  1 46 37

# all 5 of the top 5 data points identified as outliers with the LOF method also appeared in the top 5 produced with the KNN distance method

print(abide1$lof_score[top5_lof])
# [1] 2.150131 1.983696 1.616321 1.514586 1.474494

# Plot variables using LOF score as size of points
plot(SRS.Total ~ KBIT_Score, data = abide1, 
     cex = lof_score, pch = 20)




