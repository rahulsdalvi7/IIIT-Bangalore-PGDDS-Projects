##################################################################################################################################
# Capstone BFSI final submission 
# 
# - Rahul Dalvi
# - Ravi Gandhi
# - Sanjay Kushwah
# - Winnie Unnikrishnan
##################################################################################################################################
rm(list = ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(lattice)
library(cowplot)
library(Information)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
#install.packages("e1071")
library(e1071)
library("ROCR")
# install.packages("GGally")
#install.packages("corrplot")
#install.packages("ROSE")
library(corrplot)
library(DMwR) # for smote analysis in caret
library(ranger) # for Random Forest in Caret
library(glmnet) # for glmnet model in caret
library(ROSE)

##################################################################################################################################
# loading Files
##################################################################################################################################

# Load demographic dataframe
demographic.df <- read.csv(file = 'Demographic data.csv',header = T,stringsAsFactors = T, na.strings = 'NA')
# Load credit bureau dataframe
credit_buraeu.df<- read.csv(file = 'Credit Bureau data.csv',header = T,stringsAsFactors = T, na.strings = 'NA')

##################################################################################################################################
# Data preparation
##################################################################################################################################

#Duplicate check

sum(duplicated(demographic.df$Application.ID)) 
# There are 3 duplicated Application.ID

demographic.df$Application.ID[which(duplicated(demographic.df$Application.ID))]
# 765011468, 653287861, 671989187

sum(duplicated(credit_buraeu.df$Application.ID)) 
# There are 3 duplicated Application.ID

credit_buraeu.df$Application.ID[which(duplicated(credit_buraeu.df$Application.ID))]
# 765011468, 653287861, 671989187

#Removing the records corresponding to duplicated Application.ID
credit_buraeu.df <- credit_buraeu.df[!credit_buraeu.df$Application.ID %in% c(765011468, 653287861, 671989187),]
demographic.df<-demographic.df[!demographic.df$Application.ID %in% c(765011468, 653287861, 671989187),]

sum(duplicated(credit_buraeu.df$Application.ID))
sum(duplicated(demographic.df$Application.ID))
# There are 0 duplicated Application.ID

# sapply(demographic.df, function(x) sum(is.na(x) | is.null(x)))
# sapply(credit_buraeu.df, function(x) sum(is.na(x) | is.null(x)))

# merge rwo datframes
combined.df <- merge(x = demographic.df, y = credit_buraeu.df, by = 'Application.ID')

# take a peek
head(combined.df)
str(combined.df)
dim(combined.df)
summary(combined.df)
# Application.ID           Age        Gender   
# Min.   :1.004e+05   Min.   :-3.00    :    2  
# 1st Qu.:2.484e+08   1st Qu.:37.00   F:16837  
# Median :4.976e+08   Median :45.00   M:54450  
# Mean   :4.989e+08   Mean   :44.94            
# 3rd Qu.:7.496e+08   3rd Qu.:53.00            
# Max.   :1.000e+09   Max.   :65.00            
# 
# Marital.Status..at.the.time.of.application. No.of.dependents     Income    
# :    6                               Min.   :1.000    Min.   :-0.5  
# Married:60725                               1st Qu.:2.000    1st Qu.:14.0  
# Single :10558                               Median :3.000    Median :27.0  
# Mean   :2.865    Mean   :27.2  
# 3rd Qu.:4.000    3rd Qu.:40.0  
# Max.   :5.000    Max.   :60.0  
# NA's   :3                      
# Education       Profession              Type.of.residence
# :  119          :   14                      :    8  
# Bachelor    :17695   SAL    :40435   Company provided   : 1629  
# Masters     :23970   SE     :14305   Living with Parents: 1817  
# Others      :  121   SE_PROF:16535   Others             :  199  
# Phd         : 4548                   Owned              :14243  
# Professional:24836                   Rented             :53393  
# 
# No.of.months.in.current.residence No.of.months.in.current.company Performance.Tag.x
# Min.   :  6.00                    Min.   :  3.00                  Min.   :0.0000   
# 1st Qu.:  6.00                    1st Qu.: 16.00                  1st Qu.:0.0000   
# Median : 11.00                    Median : 34.00                  Median :0.0000   
# Mean   : 34.56                    Mean   : 33.96                  Mean   :0.0422   
# 3rd Qu.: 60.00                    3rd Qu.: 51.00                  3rd Qu.:0.0000   
# Max.   :126.00                    Max.   :133.00                  Max.   :1.0000   
# NA's   :1425     
# No.of.times.90.DPD.or.worse.in.last.6.months
# Min.   :0.0000                              
# 1st Qu.:0.0000                              
# Median :0.0000                              
# Mean   :0.2703                              
# 3rd Qu.:0.0000                              
# Max.   :3.0000                              
# 
# No.of.times.60.DPD.or.worse.in.last.6.months
# Min.   :0.0000                              
# 1st Qu.:0.0000                              
# Median :0.0000                              
# Mean   :0.4305                              
# 3rd Qu.:1.0000                              
# Max.   :5.0000                              
# 
# No.of.times.30.DPD.or.worse.in.last.6.months
# Min.   :0.0000                              
# 1st Qu.:0.0000                              
# Median :0.0000                              
# Mean   :0.5772                              
# 3rd Qu.:1.0000                              
# Max.   :7.0000                              
# 
# No.of.times.90.DPD.or.worse.in.last.12.months
# Min.   :0.0000                               
# 1st Qu.:0.0000                               
# Median :0.0000                               
# Mean   :0.4503                               
# 3rd Qu.:1.0000                               
# Max.   :5.0000                               
# 
# No.of.times.60.DPD.or.worse.in.last.12.months
# Min.   :0.0000                               
# 1st Qu.:0.0000                               
# Median :0.0000                               
# Mean   :0.6555                               
# 3rd Qu.:1.0000                               
# Max.   :7.0000                               
# 
# No.of.times.30.DPD.or.worse.in.last.12.months Avgas.CC.Utilization.in.last.12.months
# Min.   :0.0000                                Min.   :  0.0                         
# 1st Qu.:0.0000                                1st Qu.:  8.0                         
# Median :0.0000                                Median : 15.0                         
# Mean   :0.8009                                Mean   : 29.7                         
# 3rd Qu.:1.0000                                3rd Qu.: 46.0                         
# Max.   :9.0000                                Max.   :113.0                         
# NA's   :1058                          
# No.of.trades.opened.in.last.6.months No.of.trades.opened.in.last.12.months
# Min.   : 0.000                       Min.   : 0.000                       
# 1st Qu.: 1.000                       1st Qu.: 2.000                       
# Median : 2.000                       Median : 5.000                       
# Mean   : 2.298                       Mean   : 5.827                       
# 3rd Qu.: 3.000                       3rd Qu.: 9.000                       
# Max.   :12.000                       Max.   :28.000                       
# NA's   :1                                                                 
# No.of.PL.trades.opened.in.last.6.months No.of.PL.trades.opened.in.last.12.months
# Min.   :0.000                           Min.   : 0.000                          
# 1st Qu.:0.000                           1st Qu.: 0.000                          
# Median :1.000                           Median : 2.000                          
# Mean   :1.207                           Mean   : 2.397                          
# 3rd Qu.:2.000                           3rd Qu.: 4.000                          
# Max.   :6.000                           Max.   :12.000                          
# 
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
# Min.   : 0.000                                                
# 1st Qu.: 0.000                                                
# Median : 1.000                                                
# Mean   : 1.764                                                
# 3rd Qu.: 3.000                                                
# Max.   :10.000                                                
# 
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
# Min.   : 0.000                                                 
# 1st Qu.: 0.000                                                 
# Median : 3.000                                                 
# Mean   : 3.535                                                 
# 3rd Qu.: 5.000                                                 
# Max.   :20.000                                                 
# 
# Presence.of.open.home.loan Outstanding.Balance Total.No.of.Trades
# Min.   :0.0000             Min.   :      0     Min.   : 0.000    
# 1st Qu.:0.0000             1st Qu.: 211542     1st Qu.: 3.000    
# Median :0.0000             Median : 774997     Median : 6.000    
# Mean   :0.2565             Mean   :1249233     Mean   : 8.187    
# 3rd Qu.:1.0000             3rd Qu.:2920801     3rd Qu.:10.000    
# Max.   :1.0000             Max.   :5218801     Max.   :44.000    
# NA's   :272                NA's   :272                           
# Presence.of.open.auto.loan Performance.Tag.y
# Min.   :0.00000            Min.   :0.0000   
# 1st Qu.:0.00000            1st Qu.:0.0000   
# Median :0.00000            Median :0.0000   
# Mean   :0.08461            Mean   :0.0422   
# 3rd Qu.:0.00000            3rd Qu.:0.0000   
# Max.   :1.00000            Max.   :1.0000   
# NA's   :1425 

# check for any space 
sapply(combined.df, function(x) length(which(x == "")))

# convert any space to NA
combined.df <- as.data.table(apply(combined.df, 2, function(x) ifelse(x %in% c("", " ", "NA"), NA, x)))


# check the no of NAs in combined df
sum(is.na(combined.df))

# check duplicates 
nrow(combined.df[duplicated(combined.df$Application.ID), ])

## Converting the required columns to numerical type
combined.df$Age <- as.numeric(combined.df$Age)
combined.df$Income <- as.numeric(combined.df$Income)
combined.df$No.of.months.in.current.residence <- as.numeric(combined.df$No.of.months.in.current.residence)
combined.df$No.of.months.in.current.company <- as.numeric(combined.df$No.of.months.in.current.company)



# check how many Performance Tag has NAs
length(which( is.na(combined.df$Performance.Tag.x) == TRUE))


# Lets filter them out
rejected_application.df <- combined.df[is.na(combined.df$Performance.Tag.x) == TRUE,]
dim(rejected_application.df)
colSums(is.na(rejected_application.df))
#1425 application rejected
filtered_combined.df <- combined.df[!is.na(combined.df$Performance.Tag.x) == TRUE,]
# View(rejected_application.df)
sapply(filtered_combined.df, function(x) length(unique(x)))

str(filtered_combined.df)

table(filtered_combined.df$Performance.Tag.x)
# 0     1 
# 66917  2947 

prop.table(table(filtered_combined.df$Performance.Tag.x))
# 0          1 
# 0.95781805 0.04218195 
# the data seems highly unbalanced


# MIssing value Analysis

sapply(filtered_combined.df, function(x) sum(is.na(x)))

# Application.ID                               0 
# Age                                          0 
# Gender                                       2 
# Marital.Status..at.the.time.of.application.  6 
# No.of.dependents                             3 
# Income                                       0 
# Education                                    118 
# Profession                                   13 
# Type.of.residence                            8 
# No.of.months.in.current.residence            0 
# No.of.months.in.current.company              0 
# Performance.Tag.x                            0 
# No.of.times.90.DPD.or.worse.in.last.6.months 0 
# No.of.times.60.DPD.or.worse.in.last.6.months 0 
# No.of.times.30.DPD.or.worse.in.last.6.months 0 
# No.of.times.90.DPD.or.worse.in.last.12.months0 
# No.of.times.60.DPD.or.worse.in.last.12.months0 
# No.of.times.30.DPD.or.worse.in.last.12.months0 
# Avgas.CC.Utilization.in.last.12.months       1023 
# No.of.trades.opened.in.last.6.months         1 
# No.of.trades.opened.in.last.12.months        0 
# No.of.PL.trades.opened.in.last.6.months      0 
# No.of.PL.trades.opened.in.last.12.months     0 
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0 
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans 0 
# Presence.of.open.home.loan                   272 
# Outstanding.Balance                          272 
# Total.No.of.Trades                           0 
# Presence.of.open.auto.loan                   0 
# Performance.Tag.y                            0 
##################################################################################################################################

# Following variables have very less NAs and hence can be replaced with the mode values
# Gender                                       2 
# Marital.Status..at.the.time.of.application.  6 
# Profession                                   13 
# Type.of.residence                            8 
##################################################################################################################################

# 1 is considered as 'good' during IV Calculation, hence introducing a new variable Negated.performance.tag
filtered_combined.df$Negated.Performance.Tag <- ifelse(as.numeric(filtered_combined.df$Performance.Tag.x) == 0,1,0)
sum(is.na(filtered_combined.df$Negated.Performance.Tag))

##################################################################################################################################
# Data Imputation for Default values 
##################################################################################################################################


########### Demographic Variables ##################
# AGE 
summary(as.numeric(filtered_combined.df$Age)) 
summary(as.numeric(rejected_application.df$Age)) 
colSums(is.na(rejected_application.df))
#  There are some -ve values which are wrong as age can't be <0
sum(filtered_combined.df$Age <= 0)
# 20 values with age <0 which needs to be removed
filtered_combined.df <- filtered_combined.df[-which(filtered_combined.df$Age <= 0),]
# Checking outliers 
boxplot(filtered_combined.df$Age) 
# Obs: No outliers 
summary(filtered_combined.df$Age)

# Gender 
sum(is.na(filtered_combined.df$Gender))
sum(is.na(rejected_application.df$Gender))
table(filtered_combined.df$Gender)
table(rejected_application.df$Gender)
# Imputing the Missing value with the mode
filtered_combined.df[which(is.na(filtered_combined.df$Gender)),]$Gender = 'M'
filtered_combined.df$Gender <- factor(filtered_combined.df$Gender)

# Marital_status
sum(is.na(filtered_combined.df$Marital.Status..at.the.time.of.application.))
table(filtered_combined.df$Marital.Status..at.the.time.of.application.)
# Imputing the Missing value with the mode
filtered_combined.df[which(is.na(filtered_combined.df$Marital.Status..at.the.time.of.application.)),]$Marital.Status..at.the.time.of.application. = "Married"
filtered_combined.df$Marital.Status..at.the.time.of.application. <- factor(filtered_combined.df$Marital.Status..at.the.time.of.application.)

# no of dependants - NAs to be replaced with 0
sum(is.na(filtered_combined.df$No.of.dependents))
table(filtered_combined.df$No.of.dependents)
filtered_combined.df$No.of.dependents <- as.numeric(filtered_combined.df$No.of.dependents)
filtered_combined.df[which(is.na(filtered_combined.df$No.of.dependents)),]$No.of.dependents=0
summary(filtered_combined.df$No.of.dependents)

# Income variable check
summary(as.numeric(filtered_combined.df$Income)) 
#  There are some -ve values which are wrong as income can't be <0
sum(filtered_combined.df$Income <= 0)
sum(rejected_application.df$Income <= 0)
# 107 values with Income <0 which needs to be removed
filtered_combined.df[which(filtered_combined.df$Income <= 0),]$Income = NA
# NA's will be imputed using WOE values

# Profession Imputation
table(filtered_combined.df$Profession)
table(rejected_application.df$Profession)
sum(is.na(filtered_combined.df$Profession))
sum(is.na(rejected_application.df$Profession))
# imputing with the mode value
filtered_combined.df[which(is.na(filtered_combined.df$Profession)),]$Profession = "SAL"
rejected_application.df[which(is.na(rejected_application.df$Profession)),]$Profession = "SAL"

# Type of Residence  Imputation
table(filtered_combined.df$Type.of.residence)
sum(is.na(filtered_combined.df$Type.of.residence))
# Will be imputed with the WOE values

# No.of.months.in.current.residence 
quantile(filtered_combined.df$No.of.months.in.current.residence, probs= seq(0,1, by=0.01))
# Checking outliers 
boxplot(filtered_combined.df$No.of.months.in.current.residence) 
# Obs: No outliers 
summary(filtered_combined.df$No.of.months.in.current.residence)
histogram(filtered_combined.df$No.of.months.in.current.residence)


#No.of.months.in.current.company
quantile(filtered_combined.df$No.of.months.in.current.company, probs= seq(0,1, by=0.01))
# treating outlier as 99% of the population is less than 74
filtered_combined.df$No.of.months.in.current.company[which(filtered_combined.df$No.of.months.in.current.company > 74)]  = 74
summary(filtered_combined.df$No.of.months.in.current.company)
histogram(filtered_combined.df$No.of.months.in.current.company)



######## Credit Buereau Data #########

#No.of.times.90.DPD.or.worse.in.last.6.months
filtered_combined.df$No.of.times.90.DPD.or.worse.in.last.6.months<-as.numeric(filtered_combined.df$No.of.times.90.DPD.or.worse.in.last.6.months)
rejected_application.df$No.of.times.90.DPD.or.worse.in.last.6.months<-as.numeric(rejected_application.df$No.of.times.90.DPD.or.worse.in.last.6.months)
summary(filtered_combined.df$No.of.times.90.DPD.or.worse.in.last.6.months)

#No.of.times.60.DPD.or.worse.in.last.6.months
filtered_combined.df$No.of.times.60.DPD.or.worse.in.last.6.months<-as.numeric(filtered_combined.df$No.of.times.60.DPD.or.worse.in.last.6.months)
rejected_application.df$No.of.times.60.DPD.or.worse.in.last.6.months<-as.numeric(rejected_application.df$No.of.times.60.DPD.or.worse.in.last.6.months)
summary(filtered_combined.df$No.of.times.60.DPD.or.worse.in.last.6.months)

#No.of.times.30.DPD.or.worse.in.last.6.months
filtered_combined.df$No.of.times.30.DPD.or.worse.in.last.6.months<-as.numeric(filtered_combined.df$No.of.times.30.DPD.or.worse.in.last.6.months)
rejected_application.df$No.of.times.30.DPD.or.worse.in.last.6.months<-as.numeric(rejected_application.df$No.of.times.30.DPD.or.worse.in.last.6.months)
summary(filtered_combined.df$No.of.times.30.DPD.or.worse.in.last.6.months)

#No.of.times.90.DPD.or.worse.in.last.12.months
filtered_combined.df$No.of.times.90.DPD.or.worse.in.last.12.months<-as.numeric(filtered_combined.df$No.of.times.90.DPD.or.worse.in.last.12.months)
rejected_application.df$No.of.times.90.DPD.or.worse.in.last.12.months<-as.numeric(rejected_application.df$No.of.times.90.DPD.or.worse.in.last.12.months)
summary(filtered_combined.df$No.of.times.90.DPD.or.worse.in.last.12.months)

#No.of.times.60.DPD.or.worse.in.last.12.months
filtered_combined.df$No.of.times.60.DPD.or.worse.in.last.12.months<-as.numeric(filtered_combined.df$No.of.times.60.DPD.or.worse.in.last.12.months)
rejected_application.df$No.of.times.60.DPD.or.worse.in.last.12.months<-as.numeric(rejected_application.df$No.of.times.60.DPD.or.worse.in.last.12.months)
summary(filtered_combined.df$No.of.times.60.DPD.or.worse.in.last.12.months)


#No.of.times.30.DPD.or.worse.in.last.12.months
filtered_combined.df$No.of.times.30.DPD.or.worse.in.last.12.months<-as.numeric(filtered_combined.df$No.of.times.30.DPD.or.worse.in.last.12.months)
rejected_application.df$No.of.times.30.DPD.or.worse.in.last.12.months<-as.numeric(rejected_application.df$No.of.times.30.DPD.or.worse.in.last.12.months)
summary(filtered_combined.df$No.of.times.30.DPD.or.worse.in.last.12.months)


# Avgas.CC.Utilization.in.last.12.months
# Assumption - .	NA values in Avg CC utilization in last 12 months are present the data imply that the customers does not having no other credit card. So, we will replace the Avg. Credit card utilization value from NA to 0. 
# Creating the NoCC variable to flag such customers

sum(is.na(filtered_combined.df$Avgas.CC.Utilization.in.last.12.months))
filtered_combined.df$NoCC <- ifelse(is.na(filtered_combined.df$Avgas.CC.Utilization.in.last.12.months),1,0)
sum(filtered_combined.df$NoCC)
filtered_combined.df$Avgas.CC.Utilization.in.last.12.months[which(is.na(filtered_combined.df$Avgas.CC.Utilization.in.last.12.months))] <- 0
filtered_combined.df$Avgas.CC.Utilization.in.last.12.months<-as.numeric(filtered_combined.df$Avgas.CC.Utilization.in.last.12.months)
summary(filtered_combined.df$Avgas.CC.Utilization.in.last.12.months)
sum(filtered_combined.df$NoCC)

#for rejected variables
sum(is.na(rejected_application.df$Avgas.CC.Utilization.in.last.12.months))
rejected_application.df$NoCC <- ifelse(is.na(rejected_application.df$Avgas.CC.Utilization.in.last.12.months),1,0)
sum(rejected_application.df$NoCC)
rejected_application.df$Avgas.CC.Utilization.in.last.12.months[which(is.na(rejected_application.df$Avgas.CC.Utilization.in.last.12.months))] <- 0
rejected_application.df$Avgas.CC.Utilization.in.last.12.months<-as.numeric(rejected_application.df$Avgas.CC.Utilization.in.last.12.months)
summary(rejected_application.df$Avgas.CC.Utilization.in.last.12.months)
sum(rejected_application.df$NoCC)

#No.of.trades.opened.in.last.6.months
summary(factor(filtered_combined.df$No.of.trades.opened.in.last.6.months))
summary(factor(rejected_application.df$No.of.trades.opened.in.last.6.months))
sum(is.na(filtered_combined.df$No.of.trades.opened.in.last.6.months))
filtered_combined.df[is.na(filtered_combined.df$No.of.trades.opened.in.last.6.months),]$No.of.trades.opened.in.last.6.months<-"0"
filtered_combined.df$No.of.trades.opened.in.last.6.months<-as.numeric(filtered_combined.df$No.of.trades.opened.in.last.6.months)
rejected_application.df$No.of.trades.opened.in.last.6.months<-as.numeric(rejected_application.df$No.of.trades.opened.in.last.6.months)

#No.of.trades.opened.in.last.12.months
summary(filtered_combined.df$No.of.trades.opened.in.last.12.months)
sum(is.na(filtered_combined.df$No.of.trades.opened.in.last.12.months))
filtered_combined.df$No.of.trades.opened.in.last.12.months<-as.numeric(filtered_combined.df$No.of.trades.opened.in.last.12.months)
rejected_application.df$No.of.trades.opened.in.last.12.months<-as.numeric(rejected_application.df$No.of.trades.opened.in.last.12.months)


#No.of.PL.trades.opened.in.last.6.months
summary(filtered_combined.df$No.of.PL.trades.opened.in.last.6.months)
sum(is.na(filtered_combined.df$No.of.PL.trades.opened.in.last.6.months))
filtered_combined.df$No.of.PL.trades.opened.in.last.6.months<-as.numeric(filtered_combined.df$No.of.PL.trades.opened.in.last.6.months)
rejected_application.df$No.of.PL.trades.opened.in.last.6.months<-as.numeric(rejected_application.df$No.of.PL.trades.opened.in.last.6.months)


#No.of.PL.trades.opened.in.last.12.months 
summary(filtered_combined.df$No.of.PL.trades.opened.in.last.12.months)
sum(is.na(filtered_combined.df$No.of.PL.trades.opened.in.last.12.months))
filtered_combined.df$No.of.PL.trades.opened.in.last.12.months<-as.numeric(filtered_combined.df$No.of.PL.trades.opened.in.last.12.months)
rejected_application.df$No.of.PL.trades.opened.in.last.12.months<-as.numeric(rejected_application.df$No.of.PL.trades.opened.in.last.12.months)

#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
summary(filtered_combined.df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
sum(is.na(filtered_combined.df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))
filtered_combined.df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-as.numeric(filtered_combined.df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
rejected_application.df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-as.numeric(rejected_application.df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
summary(filtered_combined.df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
sum(is.na(filtered_combined.df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))
filtered_combined.df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<-as.numeric(filtered_combined.df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
rejected_application.df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<-as.numeric(rejected_application.df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

# Presence.of.open.home.loan                   272 
summary(factor(filtered_combined.df$Presence.of.open.home.loan))
# # filtered_combined.df$Presence.of.open.home.loan[which(is.na(filtered_combined.df$Presence.of.open.home.loan))] <- " 1"
# filtered_combined.df$Presence.of.open.home.loan<-as.numeric(filtered_combined.df$Presence.of.open.home.loan)
#272 NAs to be imputed by WOE later 

# Outstanding.Balance 
filtered_combined.df$Outstanding.Balance<-as.numeric(filtered_combined.df$Outstanding.Balance)
rejected_application.df$Outstanding.Balance<-as.numeric(rejected_application.df$Outstanding.Balance)
summary(filtered_combined.df$Outstanding.Balance)
#272 NAs to be imputed by WOE later

# Total.No.of.Trades                   
filtered_combined.df$Total.No.of.Trades<-as.numeric(filtered_combined.df$Total.No.of.Trades)
rejected_application.df$Total.No.of.Trades<-as.numeric(rejected_application.df$Total.No.of.Trades)
summary(filtered_combined.df$Total.No.of.Trades)


#Presence.of.open.auto.loan 
summary(as.factor(filtered_combined.df$Presence.of.open.auto.loan))
filtered_combined.df$Presence.of.open.auto.loan<-as.numeric(filtered_combined.df$Presence.of.open.auto.loan)
rejected_application.df$Presence.of.open.auto.loan<-as.numeric(rejected_application.df$Presence.of.open.auto.loan)


################ Binning and Imputing Missing values using WOE ##########################

####AGE

# Binning the age variable and store it into age.bin
summary(filtered_combined.df$Age)
filtered_combined.df$age.bin <- as.factor(cut(filtered_combined.df$Age, 
                                              breaks = c(0,37,44,50,70)))

ggplot(filtered_combined.df,aes(age.bin))+geom_bar()

# Create and check WOE variable for age buckets
# Age_bin_info <- create_infotables(data=filtered_combined.df[,c("age.bin","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
# Age_bin_info $Tables$age.bin

# levels(filtered_combined.df$age.bin)[which(levels(filtered_combined.df$age.bin)=="(0,15]")] <-
#   levels(filtered_combined.df$age.bin)[which(levels(filtered_combined.df$age.bin)=="(15,37]")]
levels(filtered_combined.df$age.bin)
levels(filtered_combined.df$age.bin)[1]
summary(filtered_combined.df$age.bin)

# Imputing age with the median of the range
filtered_combined.df$age.bin <- factor(filtered_combined.df$age.bin)
age.bin_info <- create_infotables(data=filtered_combined.df[,c("age.bin","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
age.bin_info $Tables$age.bin
filtered_combined.df$Age_WOE = 0
number_of_levels = length(unique(filtered_combined.df$age.bin))
for(i in 1:number_of_levels) {
  level_name = levels(filtered_combined.df$age.bin)[i]
  filtered_combined.df$Age_WOE[(filtered_combined.df$age.bin == level_name)] = filter(age.bin_info $Tables$age.bin,age.bin == level_name)$WOE
}

head(filtered_combined.df[,c('Age','age.bin','Age_WOE')])
ncol(filtered_combined.df)



###Gender
summary(filtered_combined.df$Gender)
filtered_combined.df$Gender <- factor(filtered_combined.df$Gender)
Gender_info <- create_infotables(data=filtered_combined.df[,c("Gender","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
Gender_info $Tables$Gender

filtered_combined.df$Gender_WOE = 0
filtered_combined.df$Gender_WOE[(filtered_combined.df$Gender == "F")] = filter(Gender_info $Tables$Gender,Gender == "F")$WOE
filtered_combined.df$Gender_WOE[(filtered_combined.df$Gender == "M")] = filter(Gender_info $Tables$Gender,Gender == "M")$WOE
# head(filtered_combined.df$Gender_WOE)
# head(filtered_combined.df$Gender)


###Marital Status
summary(filtered_combined.df$Marital.Status..at.the.time.of.application.)
# filtered_combined.df$Marital.Status..at.the.time.of.application. <- factor(filtered_combined.df$Marital.Status..at.the.time.of.application.)
Marital_status_info <- create_infotables(data=filtered_combined.df[,c("Marital.Status..at.the.time.of.application.","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
Marital_status_info $Tables$Marital.Status..at.the.time.of.application.


filtered_combined.df$Marital_status_WOE = 0
number_of_levels = length(unique(filtered_combined.df$Marital.Status..at.the.time.of.application.))
for(i in 1:number_of_levels) {
  level_name = levels(filtered_combined.df$Marital.Status..at.the.time.of.application.)[i]
  filtered_combined.df$Marital_status_WOE[(filtered_combined.df$Marital.Status..at.the.time.of.application. == level_name)] = filter(Marital_status_info $Tables$Marital.Status..at.the.time.of.application.,Marital.Status..at.the.time.of.application. == level_name)$WOE
}

head(filtered_combined.df[,c('Marital.Status..at.the.time.of.application.','Marital_status_WOE')])
ncol(filtered_combined.df)


### Income 
summary(filtered_combined.df$Income) 
# Binning the Income variable and store it into "Income.bin".
filtered_combined.df$Income.bin <- as.factor(cut(filtered_combined.df$Income, breaks = c(1,10,18,27,35,43,60)))
summary(filtered_combined.df$Income.bin)
# Create and check WOE variable for Income buckets
Income_bin_info <- create_infotables(data=filtered_combined.df[,c("Income.bin","Negated.Performance.Tag")], y="Negated.Performance.Tag",  parallel=FALSE)
Income_bin_info$Tables$Income.bin
# Imputing the NA values in Income Bin to nearest WOE value
filtered_combined.df$Income.bin[which(is.na(filtered_combined.df$Income.bin))] = "(43,60]"
# imputing the original impute values with the media of the range as per the WOE values
imputed_income = filtered_combined.df %>% 
  filter(Income.bin == '(43,60]') %>%
  summarise(md = median(Income, na.rm = T))
filtered_combined.df$Income[which(is.na(filtered_combined.df$Income))] = imputed_income$md
head(filtered_combined.df$Income)
levels(filtered_combined.df$Income.bin)
summary(filtered_combined.df$Income.bin)

filtered_combined.df$Income_WOE = 0
number_of_levels = length(unique(filtered_combined.df$Income.bin))
for(i in 1:number_of_levels) {
  level_name = levels(filtered_combined.df$Income.bin)[i]
  filtered_combined.df$Income_WOE[(filtered_combined.df$Income.bin == level_name)] = filter(Income_bin_info $Tables$Income.bin,Income.bin == level_name)$WOE
}

sum(is.na(filtered_combined.df$Income))
head(filtered_combined.df[,c('Income','Income.bin','Income_WOE')])
ncol(filtered_combined.df)

# Checking outliers 
boxplot(filtered_combined.df$Income) 
# Obs: No outliers 
ggplot(filtered_combined.df,aes(Income.bin))+geom_bar()
# Income column can be removed now
ncol(filtered_combined.df)
# filtered_combined.df <- subset(filtered_combined.df, select = -c(Income))


###Education 

levels(as.factor(filtered_combined.df$Education))
sum(is.na(filtered_combined.df$Education))
filtered_combined.df$Education <- factor(filtered_combined.df$Education)
Education_info <- create_infotables(data=filtered_combined.df[,c("Education","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
Education_info $Tables$Education

# imputing the value of NA to the nearest WOE category i.e. MASTERS in this case
filtered_combined.df$Education[which(is.na(filtered_combined.df$Education))] <- "Masters"

#creating the WOE variable for the categorical variable
filtered_combined.df$Education_WOE = 0
filtered_combined.df$Education_WOE[(filtered_combined.df$Education == "Bachelor")] = filter(Education_info $Tables$Education,Education == "Bachelor")$WOE
filtered_combined.df$Education_WOE[(filtered_combined.df$Education == "Masters")] = filter(Education_info $Tables$Education,Education == "Masters")$WOE
filtered_combined.df$Education_WOE[(filtered_combined.df$Education == "Others")] = filter(Education_info $Tables$Education,Education == "Others")$WOE
filtered_combined.df$Education_WOE[(filtered_combined.df$Education == "Phd")] = filter(Education_info $Tables$Education,Education == "Phd")$WOE
filtered_combined.df$Education_WOE[(filtered_combined.df$Education == "Professional")] = filter(Education_info $Tables$Education,Education == "Professional")$WOE
head(filtered_combined.df$Education_WOE)

### Profession
filtered_combined.df$Profession <- factor(filtered_combined.df$Profession)
Profession_info <- create_infotables(data=filtered_combined.df[,c("Profession","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
Profession_info $Tables$Profession
filtered_combined.df$Profession_WOE = 0
filtered_combined.df$Profession_WOE[(filtered_combined.df$Profession == "SAL")] = filter(Profession_info $Tables$Profession,Profession == "SAL")$WOE
filtered_combined.df$Profession_WOE[(filtered_combined.df$Profession == "SE")] = filter(Profession_info $Tables$Profession,Profession == "SE")$WOE
filtered_combined.df$Profession_WOE[(filtered_combined.df$Profession == "SE_PROF")] = filter(Profession_info $Tables$Profession,Profession == "SE_PROF")$WOE
head(filtered_combined.df$Profession_WOE)
head(filtered_combined.df$Profession)


###Type.of.residence
# filtered_combined.df <- filtered_combined.df[-which(is.na(filtered_combined.df$Type.of.residence)),]
filtered_combined.df$Type.of.residence <- factor(filtered_combined.df$Type.of.residence)
Residence_info <- create_infotables(data=filtered_combined.df[,c("Type.of.residence","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
Residence_info $Tables$Type.of.residence

# imputing the value of NA to the nearest WOE category i.e. Others in this case
filtered_combined.df$Type.of.residence[which(is.na(filtered_combined.df$Type.of.residence))] <- "Owned"

filtered_combined.df$Type.of.residence_WOE = 0
filtered_combined.df$Type.of.residence_WOE[(filtered_combined.df$Type.of.residence == "Company provided")] = filter(Residence_info $Tables$Type.of.residence,Type.of.residence == "Company provided")$WOE
filtered_combined.df$Type.of.residence_WOE[(filtered_combined.df$Type.of.residence == "Living with Parents")] = filter(Residence_info $Tables$Type.of.residence,Type.of.residence == "Living with Parents")$WOE
filtered_combined.df$Type.of.residence_WOE[(filtered_combined.df$Type.of.residence == "Others")] = filter(Residence_info $Tables$Type.of.residence,Type.of.residence == "Others")$WOE
filtered_combined.df$Type.of.residence_WOE[(filtered_combined.df$Type.of.residence == "Owned")] = filter(Residence_info $Tables$Type.of.residence,Type.of.residence == "Owned")$WOE
filtered_combined.df$Type.of.residence_WOE[(filtered_combined.df$Type.of.residence == "Rented")] = filter(Residence_info $Tables$Type.of.residence,Type.of.residence == "Rented")$WOE
head(filtered_combined.df$Type.of.residence_WOE)
head(filtered_combined.df$Type.of.residence)


###No.of.months.in.current.residence
# Checking outliers 
boxplot(filtered_combined.df$No.of.months.in.current.residence) 
# Obs: No outliers 
summary(filtered_combined.df$No.of.months.in.current.residence)
histogram(filtered_combined.df$No.of.months.in.current.residence)
# Binning the No.of.months.in.current.residence variable and store it into "NbrMntRes.bin".
filtered_combined.df$NbrMntRes.bin <- as.factor(cut(filtered_combined.df$No.of.months.in.current.residence, breaks = c(0,25,50,75,100,126)))
ggplot(filtered_combined.df,aes(NbrMntRes.bin))+geom_bar()
# Create and check WOE variable for Income buckets
NbrMntRes_bin_info <- create_infotables(data=filtered_combined.df[,c("NbrMntRes.bin","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
NbrMntRes_bin_info $Tables$NbrMntRes.bin
# No.of.months.in.current.residence column can be removed now
ncol(filtered_combined.df)
# filtered_combined.df <- subset(filtered_combined.df, select = -c(No.of.months.in.current.residence))
ncol(filtered_combined.df)

###No.of.months.in.current.company
quantile(filtered_combined.df$No.of.months.in.current.company, probs= seq(0,1, by=0.01))
filtered_combined.df$No.of.months.in.current.company[which(filtered_combined.df$No.of.months.in.current.company > 74)]  = 74
summary(filtered_combined.df$No.of.months.in.current.company)
histogram(filtered_combined.df$No.of.months.in.current.company)
# Binning the No.of.months.in.current.company variable and store it into "NbrMntCMP.bin".
filtered_combined.df$NbrMntCmp.bin <- as.factor(cut(filtered_combined.df$No.of.months.in.current.company, breaks = c(0,20,40,60,80)))
ggplot(filtered_combined.df,aes(NbrMntCmp.bin))+geom_bar()
# Create and check WOE variable for Income buckets
NbrMntCmp_bin_info <- create_infotables(data=filtered_combined.df[,c("NbrMntCmp.bin","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
NbrMntCmp_bin_info $Tables$NbrMntCmp.bin
# No.of.months.in.current.company column can be removed now
ncol(filtered_combined.df)
# filtered_combined.df <- subset(filtered_combined.df, select = -c(No.of.months.in.current.company))
ncol(filtered_combined.df)

### Performance.Tag.x
#this variable willbe used for modelling
filtered_combined.df$Performance.Tag.x<-as.numeric(filtered_combined.df$Performance.Tag.x)
summary(filtered_combined.df$Performance.Tag.x)

###No.of.times.90.DPD.or.worse.in.last.6.months
summary(filtered_combined.df$No.of.times.90.DPD.or.worse.in.last.6.months)

###No.of.times.60.DPD.or.worse.in.last.6.months
summary(filtered_combined.df$No.of.times.60.DPD.or.worse.in.last.6.months)

###No.of.times.30.DPD.or.worse.in.last.6.months
summary(filtered_combined.df$No.of.times.30.DPD.or.worse.in.last.6.months)

###No.of.times.90.DPD.or.worse.in.last.12.months
summary(filtered_combined.df$No.of.times.90.DPD.or.worse.in.last.12.months)

###No.of.times.60.DPD.or.worse.in.last.12.months
summary(filtered_combined.df$No.of.times.60.DPD.or.worse.in.last.12.months)


###No.of.times.30.DPD.or.worse.in.last.12.months
summary(filtered_combined.df$No.of.times.30.DPD.or.worse.in.last.12.months)


### Avgas.CC.Utilization.in.last.12.months
summary(filtered_combined.df$Avgas.CC.Utilization.in.last.12.months)
filtered_combined.df$Avgcc.bin <- as.factor(cut(as.numeric(filtered_combined.df$Avgas.CC.Utilization.in.last.12.months), breaks = c(0,10,20,30,40,50,60,70,80,110,120), include.lowest = T))

AvgCC_info <- create_infotables(data=filtered_combined.df[,c("Avgcc.bin","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
AvgCC_info $Tables$Avgcc.bin

ggplot(filtered_combined.df,aes(Avgcc.bin))+geom_bar()

ncol(filtered_combined.df)
# filtered_combined.df <- subset(filtered_combined.df, select = -c(Avgas.CC.Utilization.in.last.12.months))
ncol(filtered_combined.df)

###No.of.trades.opened.in.last.6.months
summary((filtered_combined.df$No.of.trades.opened.in.last.6.months))

###No.of.trades.opened.in.last.12.months
summary(filtered_combined.df$No.of.trades.opened.in.last.12.months)

filtered_combined.df$trade12.bin <- as.factor(cut(as.numeric(filtered_combined.df$No.of.trades.opened.in.last.12.months), breaks = seq(0,30,5), include.lowest = T))
ggplot(filtered_combined.df,aes(trade12.bin))+geom_bar()

levels(filtered_combined.df$trade12.bin)[which(levels(filtered_combined.df$trade12.bin)=="(20,25]")] <- 
  levels(filtered_combined.df$trade12.bin)[which(levels(filtered_combined.df$trade12.bin)=="(25,30]")]
levels(filtered_combined.df$trade12.bin)


# Create and check WOE variable for trade buckets
trade12_info <- create_infotables(data=filtered_combined.df[,c("trade12.bin","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
trade12_info $Tables$trade12.bin

ncol(filtered_combined.df)
# filtered_combined.df <- subset(filtered_combined.df, select = -c(No.of.trades.opened.in.last.12.months))
ncol(filtered_combined.df)

###No.of.PL.trades.opened.in.last.6.months
summary(filtered_combined.df$No.of.PL.trades.opened.in.last.6.months)


###No.of.PL.trades.opened.in.last.12.months 
summary(filtered_combined.df$No.of.PL.trades.opened.in.last.12.months)

###No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
summary(filtered_combined.df$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

###No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
summary(filtered_combined.df$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

### Presence.of.open.home.loan      
summary(filtered_combined.df$Presence.of.open.home.loan)

#### Outstanding.Balance 
filtered_combined.df[which(is.na(filtered_combined.df)),]
summary(filtered_combined.df$Outstanding.Balance)
filtered_combined.df$Outstanding.Balance.in.m<-filtered_combined.df$Outstanding.Balance/1000000
filtered_combined.df$bal.bin <- as.factor(cut(filtered_combined.df$Outstanding.Balance.in.m, breaks = c(-.1,.008,0.1,0.39,0.59,0.77,0.97,1.36,2.96,3.28,6)))
ggplot(filtered_combined.df,aes(bal.bin))+geom_bar()
boxplot(filtered_combined.df$Outstanding.Balance.in.m)
quantile(filtered_combined.df$Outstanding.Balance.in.m, probs = seq(0,1,0.01),na.rm = T)
# Create and check WOE variable for trade buckets
bal_info <- create_infotables(data=filtered_combined.df[,c("bal.bin","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE,bins = 15)
bal_info $Tables$bal.bin
# imputing the value of NA to the nearest WOE category i.e. (1.36,2.96]  in this case
filtered_combined.df$bal.bin[which(is.na(filtered_combined.df$bal.bin))] = "(1.36,2.96]"

#imputing the out standing balance with the mean of the range
imputed_balance = filtered_combined.df %>%
  filter(bal.bin == "(1.36,2.96]") %>% 
  summarise(mn = min(Outstanding.Balance.in.m, na.rm = T), mx = max(Outstanding.Balance.in.m, na.rm = T), md = median(Outstanding.Balance.in.m, na.rm = T))

filtered_combined.df$Outstanding.Balance.in.m[which(is.na(filtered_combined.df$Outstanding.Balance.in.m))] = imputed_balance$md
head(filtered_combined.df$Outstanding.Balance.in.m)
levels(filtered_combined.df$bal.bin)
summary(filtered_combined.df$bal.bin)

filtered_combined.df$Balance_WOE = 0
number_of_levels = length(unique(filtered_combined.df$bal.bin))
for(i in 1:number_of_levels) {
  level_name = levels(filtered_combined.df$bal.bin)[i]
  filtered_combined.df$Balance_WOE[(filtered_combined.df$bal.bin == level_name)] = filter(bal_info $Tables$bal.bin,bal.bin == level_name)$WOE
}

summary(filtered_combined.df$bal.bin)
summary(filtered_combined.df$Outstanding.Balance.in.m)
ncol(filtered_combined.df)
#filtered_combined.df <- subset(filtered_combined.df, select = -c(Outstanding.Balance ))
#ncol(filtered_combined.df)

############## Outstanding WOE for Rejected Population ######################
# rejected_application.df[which(is.na(rejected_application.df)),]
head(rejected_application.df$Outstanding.Balance)
rejected_application.df$Outstanding.Balance = as.numeric(trimws(rejected_application.df$Outstanding.Balance))
rejected_application.df$Outstanding.Balance.in.m<-rejected_application.df$Outstanding.Balance/1000000
rejected_application.df$bal.bin <- as.factor(cut(rejected_application.df$Outstanding.Balance.in.m, breaks = c(-.1,.008,0.1,0.39,0.59,0.77,0.97,1.36,2.96,3.28,6)))
head(rejected_application.df$bal.bin)
head(rejected_application.df$Outstanding.Balance.in.m)
ggplot(rejected_application.df,aes(bal.bin))+geom_bar()
boxplot(rejected_application.df$Outstanding.Balance.in.m)
quantile(filtered_combined.df$Outstanding.Balance.in.m, probs = seq(0,1,0.01),na.rm = T)

head(rejected_application.df$Outstanding.Balance.in.m)
levels(rejected_application.df$bal.bin)
summary(rejected_application.df$bal.bin)

rejected_application.df$Balance_WOE = 0
number_of_levels = length(unique(rejected_application.df$bal.bin))

for(i in 1:number_of_levels) {
  level_name = levels(rejected_application.df$bal.bin)[i]
  rejected_application.df$Balance_WOE[(rejected_application.df$bal.bin == level_name)] = filter(bal_info $Tables$bal.bin,bal.bin == level_name)$WOE
}

summary(rejected_application.df$bal.bin)
summary(rejected_application.df$Outstanding.Balance.in.m)
summary(rejected_application.df$Balance_WOE)
ncol(filtered_combined.df)


### Total.No.of.Trades                   
summary(filtered_combined.df$Total.No.of.Trades)
filtered_combined.df$tot.trade.bin <- as.factor(cut(as.numeric(filtered_combined.df$Total.No.of.Trades), breaks = c(0,3,6,10,45), include.lowest = T))
ggplot(filtered_combined.df,aes(tot.trade.bin))+geom_bar()
# Create and check WOE variable for trade buckets
tot.trade.bin.info <- create_infotables(data=filtered_combined.df[,c("tot.trade.bin","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE)
tot.trade.bin.info $Tables$tot.trade.bin

ncol(filtered_combined.df)
# filtered_combined.df <- subset(filtered_combined.df, select = -c(Total.No.of.Trades ))
ncol(filtered_combined.df)


### Presence.of.open.home.loan 
trimws(filtered_combined.df$Presence.of.open.home.loan)
summary(filtered_combined.df$Presence.of.open.home.loan)
filtered_combined.df$Presence.of.open.home.loan = as.factor(filtered_combined.df$Presence.of.open.home.loan)
# filtered_combined.df$home <- as.factor(cut(filtered_combined.df$Outstanding.Balance.in.m, breaks = c(-.1,.008,0.1,0.39,0.59,0.77,0.97,1.36,2.96,3.28,6)))
ggplot(filtered_combined.df,aes(Presence.of.open.home.loan))+geom_bar()
# Create and check WOE variable for trade buckets
home_loan_info <- create_infotables(data=filtered_combined.df[,c("Presence.of.open.home.loan","Negated.Performance.Tag")], y="Negated.Performance.Tag", parallel=FALSE,bins = 15)
home_loan_info $Tables$Presence.of.open.home.loan

#imputing presence of home with 1 as per nearest woe
summary(as.numeric(filtered_combined.df$Presence.of.open.home.loan))
summary(filtered_combined.df$Presence.of.open.home.loan)

filtered_combined.df$Presence.of.open.home.loan[which(is.na(filtered_combined.df$Presence.of.open.home.loan))] <- " 1"
summary(as.numeric(trimws(filtered_combined.df$Presence.of.open.home.loan)))
filtered_combined.df$Presence.of.open.home.loan = as.numeric(trimws(filtered_combined.df$Presence.of.open.home.loan))
summary(filtered_combined.df$Presence.of.open.home.loan)



### Presence.of.open.auto.loan 
summary(filtered_combined.df$Presence.of.open.auto.loan)
colSums(is.na(filtered_combined.df))
### Performance.Tag.y 
# this field is  no longer required

filtered_combined.df <- subset(filtered_combined.df, select = -c(Performance.Tag.y ))

###Negated.Performance.Tag
summary(filtered_combined.df$Negated.Performance.Tag)

###NoCC
summary(filtered_combined.df$NoCC)


str(filtered_combined.df)
# colnames(filtered_combined.df)

write.csv(x = filtered_combined.df, file = "master_data.csv")

##################################################################################################################################
#Univariate and Multivariate analysis
##################################################################################################################################

filtered_combined.df$Performance.Tag.x<-as.factor(filtered_combined.df$Performance.Tag.x)
# summary(filtered_combined.df$Performance.Tag.x)
barplot_by_performance <- function(col, position = F){
  
  if(position == T) {
    ggplot(filtered_combined.df %>% group_by_(col, "Performance.Tag.x") %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
      geom_bar(aes_string(col, "Count", fill="Performance.Tag.x"), stat = "identity", position = "fill") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    ggplot(filtered_combined.df %>% group_by_(col, "Performance.Tag.x") %>% summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
      geom_bar(aes_string(col, "Count", fill="Performance.Tag.x"), stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}

plot_grid(barplot_by_performance("age.bin",F),
          barplot_by_performance("Gender",F),
          barplot_by_performance("Marital.Status..at.the.time.of.application.",F),
          barplot_by_performance("age.bin", T),
          barplot_by_performance("Gender" , T),
          barplot_by_performance("Marital.Status..at.the.time.of.application.",T)
)


plot_grid(barplot_by_performance("Profession",F),
          barplot_by_performance("Education",F),
          barplot_by_performance("Type.of.residence",F),
          barplot_by_performance("Profession",T),
          barplot_by_performance("Education",T),
          barplot_by_performance("Type.of.residence",T)
)

plot_grid(barplot_by_performance("No.of.dependents",F),
          barplot_by_performance("Income.bin",F),
          barplot_by_performance("NbrMntRes.bin",F),
          # barplot_by_performance("NbrMntCmp.bin",F),
          barplot_by_performance("No.of.dependents",T),
          barplot_by_performance("Income.bin",T),
          barplot_by_performance("NbrMntRes.bin",T)
          # barplot_by_performance("NbrMntCmp.bin",T)
)


# filtered_combined.df = as.factor(filtered_combined.df$Presence.of.open.home.loan)
plot_grid(barplot_by_performance("NbrMntCmp.bin", F),
          barplot_by_performance("Presence.of.open.home.loan",F),
          barplot_by_performance("Presence.of.open.auto.loan",F),
          barplot_by_performance("NbrMntCmp.bin", T),
          barplot_by_performance("Presence.of.open.home.loan",T),
          barplot_by_performance("Presence.of.open.auto.loan",T)
)

plot_grid(barplot_by_performance("No.of.times.90.DPD.or.worse.in.last.6.months",F),
          barplot_by_performance("No.of.times.60.DPD.or.worse.in.last.6.months",F),
          barplot_by_performance("No.of.times.30.DPD.or.worse.in.last.6.months",F),
          barplot_by_performance("No.of.times.90.DPD.or.worse.in.last.6.months",T),
          barplot_by_performance("No.of.times.60.DPD.or.worse.in.last.6.months",T),
          barplot_by_performance("No.of.times.30.DPD.or.worse.in.last.6.months",T)
          # barplot_by_performance("Presence.of.open.home.loan")
)

plot_grid(barplot_by_performance("No.of.times.90.DPD.or.worse.in.last.12.months",F),
          barplot_by_performance("No.of.times.60.DPD.or.worse.in.last.12.months",F),
          barplot_by_performance("No.of.times.30.DPD.or.worse.in.last.12.months",F),
          barplot_by_performance("No.of.times.90.DPD.or.worse.in.last.12.months",T),
          barplot_by_performance("No.of.times.60.DPD.or.worse.in.last.12.months",T),
          barplot_by_performance("No.of.times.30.DPD.or.worse.in.last.12.months",T)
          # barplot_by_performance("No.of.trades.opened.in.last.6.months"),
          # barplot_by_performance("No.of.trades.opened.in.last.6.months")
)
filtered_combined.df$No.of.trades.opened.in.last.12.months
plot_grid(barplot_by_performance("Avgcc.bin",F),
          barplot_by_performance("No.of.trades.opened.in.last.6.months",F),
          barplot_by_performance("No.of.trades.opened.in.last.12.months",F),
          barplot_by_performance("Avgcc.bin",T),
          barplot_by_performance("No.of.trades.opened.in.last.6.months",T),
          barplot_by_performance("No.of.trades.opened.in.last.12.months",T)
)

filtered_combined.df$No.of.PL.trades.opened.in.last.6.months
plot_grid(barplot_by_performance("bal.bin",F),
          barplot_by_performance("tot.trade.bin",F),
          barplot_by_performance("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",F),
          barplot_by_performance("bal.bin",T),
          barplot_by_performance("tot.trade.bin",T),
          barplot_by_performance("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",T)
)

plot_grid(barplot_by_performance("Presence.of.open.auto.loan",F),
          barplot_by_performance("Presence.of.open.home.loan",F),
          barplot_by_performance("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",F),
          barplot_by_performance("Presence.of.open.auto.loan",T),
          barplot_by_performance("Presence.of.open.home.loan",T),
          barplot_by_performance("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",T)
)

plot_grid(barplot_by_performance("No.of.PL.trades.opened.in.last.6.months",F),
          barplot_by_performance("No.of.PL.trades.opened.in.last.12.months",F),
          # barplot_by_performance("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",F),
          barplot_by_performance("No.of.PL.trades.opened.in.last.6.months",T),
          barplot_by_performance("No.of.PL.trades.opened.in.last.12.months",T)
          # barplot_by_performance("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",T)
)

boxplot_by_performance <- function(col, i, name){
  ggplot(filtered_combined.df, aes_string(x="Performance.Tag.x",y=col, fill="Performance.Tag.x")) +
    geom_boxplot(width=0.2) +  
    labs(title=paste("Plot", i, name, " - Performance"), x=name,y="Ratio/Percentage",fill="Performance.Tag.x")
}


plot_grid(boxplot_by_performance("Age", 1, "Age"),
          boxplot_by_performance("Income", 2, "Income"),
          boxplot_by_performance("No.of.months.in.current.residence", 3, "Current Residence"),
          boxplot_by_performance("No.of.months.in.current.company", 4, "Current Company Tenure"))

plot_grid(boxplot_by_performance("No.of.trades.opened.in.last.12.months", 1, "No.of.trades.opened.in.last.12.months"),
          boxplot_by_performance("No.of.PL.trades.opened.in.last.6.months", 2, "No.of.PL.trades.opened.in.last.6.months"),
          boxplot_by_performance("No.of.PL.trades.opened.in.last.12.months", 3, "No.of.PL.trades.opened.in.last.12.months"),
          boxplot_by_performance("No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.", 4, "Auto Loans Inquiries (6 Months)"))

plot_grid(boxplot_by_performance("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.", 1, "Auto Loans Inquiries (12 Months)"),
          boxplot_by_performance("Total.No.of.Trades", 2, "Total.No.of.Trades"),
          boxplot_by_performance("Avgas.CC.Utilization.in.last.12.months", 3, "Avgas.CC.Utilization.in.last.12.months"),
          boxplot_by_performance("Outstanding.Balance.in.m", 4, "Outstanding.Balance.in.m"))

# plot_grid(boxplot_by_performance("Avgas.CC.Utilization.in.last.12.months", 1, "Avgas.CC.Utilization.in.last.12.months"),
#           boxplot_by_performance("Outstanding.Balance", 2, "Outstanding.Balance"))
# as will be passed to the model
filtered_combined.df$Performance.Tag.x<-numeric(filtered_combined.df$Performance.Tag.x)
summary(filtered_combined.df$Performance.Tag.x)

##################################################################################################################################
#Checking for IV value for important variables

IV_ds <- create_infotables(data = subset(filtered_combined.df,select = -c(Application.ID)), y="Negated.Performance.Tag",bins = 10,  parallel=FALSE)
IV_ds$Tables
# IV_Value = data.frame(IV_ds$Tables)
IV_Value = data.frame(IV_ds$Summary)

print(IV_ds$Summary, row.names=FALSE)

#                                             Variable           IV
#                          Avgas.CC.Utilization.in.last.12.months 2.995293e-01
#                                                       Avgcc.bin 2.984164e-01
#                           No.of.trades.opened.in.last.12.months 2.981227e-01
#                        No.of.PL.trades.opened.in.last.12.months 2.960707e-01
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 2.955825e-01
#                                        Outstanding.Balance.in.m 2.442698e-01
#                                                         bal.bin 2.436411e-01
#                                                     Balance_WOE 2.436411e-01
#                    No.of.times.30.DPD.or.worse.in.last.6.months 2.417464e-01
#                                              Total.No.of.Trades 2.367702e-01
#                         No.of.PL.trades.opened.in.last.6.months 2.198083e-01
#                   No.of.times.90.DPD.or.worse.in.last.12.months 2.140097e-01
#                    No.of.times.60.DPD.or.worse.in.last.6.months 2.059924e-01
#  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 2.053559e-01
#                                                   tot.trade.bin 2.006287e-01
#                   No.of.times.30.DPD.or.worse.in.last.12.months 1.984152e-01
#                            No.of.trades.opened.in.last.6.months 1.861445e-01
#                                                     trade12.bin 1.858131e-01
#                   No.of.times.60.DPD.or.worse.in.last.12.months 1.856055e-01
#                    No.of.times.90.DPD.or.worse.in.last.6.months 1.602292e-01
#                               No.of.months.in.current.residence 7.898170e-02
#                                                          Income 4.354888e-02
#                                                      Income.bin 3.906885e-02
#                                                      Income_WOE 3.906885e-02
#                                                   NbrMntRes.bin 2.772164e-02
#                                 No.of.months.in.current.company 2.176916e-02
#                                                   NbrMntCmp.bin 1.988276e-02
#                                      Presence.of.open.home.loan 1.755890e-02
#                                                             Age 3.325965e-03
#                                                No.of.dependents 2.647224e-03
#                                                      Profession 2.222643e-03
#                                                  Profession_WOE 2.222643e-03
#                                      Presence.of.open.auto.loan 1.657899e-03
#                                               Type.of.residence 9.248682e-04
#                                                       Education 7.799826e-04
#                                                   Education_WOE 7.735971e-04
#                                                         age.bin 6.529036e-04
#                                                         Age_WOE 6.529036e-04
#                                                          Gender 3.233938e-04
#                                                      Gender_WOE 3.233938e-04
#                                           Type.of.residence_WOE 3.039164e-04
#                                                            NoCC 1.933581e-04
#                     Marital.Status..at.the.time.of.application. 9.679065e-05
#                                              Marital_status_WOE 9.679065e-05

arrange(IV_Value [IV_Value$IV >=0.02, ], desc(IV))


# Reference table for Variable Importance Analysis based on Information Value(IV)
# -------------------------------------------------------------------------------
# Information Value(IV)   Predictive Power
# -------------------------------------------------------------------------------
#                 <0.02 -> Useless for Prediction
#            0.02 - 0.1 -> Weak Predictor
#            0.1  - 0.3 -> Medium Predictor
#            0.3  - 0.5 -> Strong Predictor
#                  >0.5 -> Suspecious
# -------------------------------------------------------------------------------
# Printing values >=0.02

#                                                           Variable        IV
# 1                           Avgas.CC.Utilization.in.last.12.months 0.29952926
# 2                                                        Avgcc.bin 0.29841637
# 3                            No.of.trades.opened.in.last.12.months 0.29812273
# 4                         No.of.PL.trades.opened.in.last.12.months 0.29607072
# 5  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.29558254
# 6                                         Outstanding.Balance.in.m 0.24426983
# 7                                                          bal.bin 0.24364107
# 8                                                      Balance_WOE 0.24364107
# 9                     No.of.times.30.DPD.or.worse.in.last.6.months 0.24174638
# 10                                              Total.No.of.Trades 0.23677017
# 11                         No.of.PL.trades.opened.in.last.6.months 0.21980829
# 12                   No.of.times.90.DPD.or.worse.in.last.12.months 0.21400973
# 13                    No.of.times.60.DPD.or.worse.in.last.6.months 0.20599241
# 14  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.20535587
# 15                                                   tot.trade.bin 0.20062868
# 16                   No.of.times.30.DPD.or.worse.in.last.12.months 0.19841516
# 17                            No.of.trades.opened.in.last.6.months 0.18614455
# 18                                                     trade12.bin 0.18581309
# 19                   No.of.times.60.DPD.or.worse.in.last.12.months 0.18560548
# 20                    No.of.times.90.DPD.or.worse.in.last.6.months 0.16022924
# 21                               No.of.months.in.current.residence 0.07898170
# 22                                                          Income 0.04354888
# 23                                                      Income.bin 0.03906885
# 24                                                      Income_WOE 0.03906885
# 25                                                   NbrMntRes.bin 0.02772164
# 26                                 No.of.months.in.current.company 0.02176916
################################################
str(filtered_combined.df)

# For plotting correlation matrix
# plot_correlationMatrix <- function (data, features) {
plot_correlationMatrix <- function (data) {
  
  melted_cor_matrix <- melt(round(cor(data ,
                                      use="complete.obs"),2))

  ggplot(data = melted_cor_matrix, aes(x=Var1, y=Var2, fill=value, label=value)) +
    geom_tile()    +
    geom_text()  +
    xlab('')   +
    ylab('') +
    theme_minimal() +
    theme(axis.text.x = element_text(size=10,
                                     hjust=-0.08,
                                     angle= -35 ))
}


variable_for_correlation_demographic = c('Age',
                             'Gender_WOE',
                             'Marital_status_WOE',
                             'No.of.dependents',
                             'Income',
                             'Education_WOE',
                             'Profession_WOE',
                             'Type.of.residence_WOE',
                             'No.of.months.in.current.residence',
                             'No.of.months.in.current.company'
)

#correlation between demographic variables
Correlation.df<-subset(filtered_combined.df, select = which(names(filtered_combined.df) %in% variable_for_correlation_demographic))
plot_correlationMatrix(Correlation.df)
# cor_mat = cor(Correlation.df)
# corrplot(cor_mat)

variable_for_correlation = c('Age',
                             'Gender_WOE',
                             'Marital_status_WOE',
                             'No.of.dependents',
                             'Income',
                             'Education_WOE',
                             'Profession_WOE',
                             'Type.of.residence_WOE',
                             'No.of.months.in.current.residence',
                             'No.of.months.in.current.company',
                             'No.of.times.90.DPD.or.worse.in.last.6.months',
                             'No.of.times.60.DPD.or.worse.in.last.6.months',
                             'No.of.times.30.DPD.or.worse.in.last.6.months',
                             'No.of.times.90.DPD.or.worse.in.last.12.months',
                             'No.of.times.60.DPD.or.worse.in.last.12.months',
                             'No.of.times.30.DPD.or.worse.in.last.12.months',
                             'Avgas.CC.Utilization.in.last.12.months',
                             'No.of.trades.opened.in.last.6.months',
                             'No.of.trades.opened.in.last.12.months',
                             'No.of.PL.trades.opened.in.last.6.months',
                             'No.of.PL.trades.opened.in.last.12.months',
                             'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.',
                             'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.',
                             'Outstanding.Balance',
                             #'Balance_WOE',
                             'Presence.of.open.home.loan',
                             'Total.No.of.Trades',
                             'Presence.of.open.auto.loan'
                             )

Correlation.df<-subset(filtered_combined.df, select = which(names(filtered_combined.df) %in% variable_for_correlation))
names(Correlation.df)
cor_mat = cor(Correlation.df)
plot_correlationMatrix(Correlation.df)
# corrplot(cor_mat)
# ggcorr(cor_mat, label = TRUE, label_size = 3, label_round = 1, label_alpha = TRUE, hjust = 1)
##################################################################################################

#  Feature Engineering - Encoding/Dummy Variables 

# Using No.of.dependents as numerical only

levels(filtered_combined.df$Gender) 
head(filtered_combined.df$Gender) 
# Gender
filtered_combined.df$Gender <- as.factor(filtered_combined.df$Gender)
levels(filtered_combined.df$Gender) <- c(1,0)

# Marital Status
levels(filtered_combined.df$Marital.Status..at.the.time.of.application.)
head(filtered_combined.df$Marital.Status..at.the.time.of.application.)
filtered_combined.df$Marital.Status..at.the.time.of.application. <- as.factor(filtered_combined.df$Marital.Status..at.the.time.of.application.)
levels(filtered_combined.df$Marital.Status..at.the.time.of.application.) <- c(1,0)

# Type of Residence
filtered_combined.df$Type.of.residence <- as.factor(filtered_combined.df$Type.of.residence)

# One-Hot encoding for Education_imputed
levels(filtered_combined.df$Education)
head(filtered_combined.df$Education)
filtered_combined.df$Education <- as.factor(filtered_combined.df$Education)
dummy_education <- data.frame(model.matrix(~Education,
                                           data=filtered_combined.df))
dummy_education <- dummy_education[,-1]
filtered_combined.df <- cbind(filtered_combined.df, dummy_education)

# One-Hot encoding for Profession
filtered_combined.df$Profession <- as.factor(filtered_combined.df$Profession)
dummy_profession <- data.frame(model.matrix(~Profession,
                                            data=filtered_combined.df))
dummy_profession <- dummy_profession[,-1]
filtered_combined.df <- cbind(filtered_combined.df, dummy_profession)

# One-Hot encoding for Residence Type
filtered_combined.df$Type.of.residence <- as.factor(filtered_combined.df$Type.of.residence)
dummy_residencetype <- data.frame(model.matrix(~Type.of.residence,
                                               data=filtered_combined.df))
dummy_residencetype <- dummy_residencetype[,-1]
filtered_combined.df <- cbind(filtered_combined.df, dummy_residencetype)

# Creating a .CSV file with WoE values
write.csv(filtered_combined.df,"filtered_combined.df_cleaned_WoE_feature_engineering.csv")
names(filtered_combined.df)

demographic.Variables <- c("Application.ID",
                    "Age", 
                    "Gender",
                    "Marital.Status..at.the.time.of.application.",
                    "No.of.dependents",
                    "Income",
                    "EducationMasters",
                    "EducationOthers",
                    "EducationPhd",
                    "EducationProfessional",
                    "ProfessionSE",
                    "ProfessionSE_PROF",
                    "Type.of.residenceLiving.with.Parents",
                    "Type.of.residenceOthers",
                    "Type.of.residenceOwned",
                    "Type.of.residenceRented",
                    "No.of.months.in.current.residence",
                    "No.of.months.in.current.company",
                    "Performance.Tag.x"
                    )


WOE_Variables <-   c("Application.ID",
                     "Age_WOE",                                                             
                     "Gender_WOE" ,
                     "Marital_status_WOE",
                     "No.of.dependents",
                     "Income_WOE"  ,
                     "EducationMasters" ,
                     "EducationOthers",
                     "EducationPhd",
                     "EducationProfessional",
                     "ProfessionSE",
                     "ProfessionSE_PROF",
                     "Type.of.residenceLiving.with.Parents",
                     "Type.of.residenceOthers" ,
                     "Type.of.residenceOwned",
                     "Type.of.residenceRented",
                     "No.of.months.in.current.residence",
                     "No.of.months.in.current.company" ,
                     "No.of.times.90.DPD.or.worse.in.last.6.months",
                     "No.of.times.60.DPD.or.worse.in.last.6.months",
                     "No.of.times.30.DPD.or.worse.in.last.6.months",
                     "No.of.times.90.DPD.or.worse.in.last.12.months",
                     "No.of.times.60.DPD.or.worse.in.last.12.months" ,                 
                     "No.of.times.30.DPD.or.worse.in.last.12.months" ,
                     "Avgas.CC.Utilization.in.last.12.months",
                     "No.of.trades.opened.in.last.6.months",
                     "No.of.trades.opened.in.last.12.months",
                     "No.of.PL.trades.opened.in.last.6.months",
                     "No.of.PL.trades.opened.in.last.12.months",
                     "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                     "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
                     "Presence.of.open.home.loan",
                     "Total.No.of.Trades",
                     "Presence.of.open.auto.loan",
                     "NoCC",
                     "Balance_WOE",
                     "Performance.Tag.x")                   

Master.df.variables <- c("Application.ID",
                         "Age",
                         "Gender" ,
                         "Marital.Status..at.the.time.of.application.",
                         "No.of.dependents",
                         "Income",
                         "EducationMasters" ,
                         "EducationOthers",
                         "EducationPhd",
                         "EducationProfessional",
                         "ProfessionSE",
                         "ProfessionSE_PROF",
                         "Type.of.residenceLiving.with.Parents",
                         "Type.of.residenceOthers" ,
                         "Type.of.residenceOwned",
                         "Type.of.residenceRented",
                         "No.of.months.in.current.residence",
                         "No.of.months.in.current.company" ,
                         "No.of.times.90.DPD.or.worse.in.last.6.months",
                         "No.of.times.60.DPD.or.worse.in.last.6.months",
                         "No.of.times.30.DPD.or.worse.in.last.6.months",
                         "No.of.times.90.DPD.or.worse.in.last.12.months",
                         "No.of.times.60.DPD.or.worse.in.last.12.months" ,                 
                         "No.of.times.30.DPD.or.worse.in.last.12.months" ,
                         "Avgas.CC.Utilization.in.last.12.months",
                         "No.of.trades.opened.in.last.6.months",
                         "No.of.trades.opened.in.last.12.months",
                         "No.of.PL.trades.opened.in.last.6.months",
                         "No.of.PL.trades.opened.in.last.12.months",
                         "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                         "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
                         "Presence.of.open.home.loan",
                         "Total.No.of.Trades",
                         "Presence.of.open.auto.loan",
                         "NoCC",
                         "Outstanding.Balance.in.m",
                         "Performance.Tag.x")

##################################################################################################################################

demo.df<-subset(filtered_combined.df, select = which(names(filtered_combined.df) %in% demographic.Variables))
Master.df<-subset(filtered_combined.df, select = which(names(filtered_combined.df) %in% Master.df.variables))
WOE.df<-subset(filtered_combined.df, select = which(names(filtered_combined.df) %in% WOE_Variables))

##################################################################################################################################

##################################################################################################################################
# Model building 
##################################################################################################################################
# Logistic regression model 
# Demographic data model
##################################################################################################################################

set.seed(1)

split_indices <- sample.split(demo.df$Performance.Tag.x, SplitRatio = 0.7)

train.lr.dm <- demo.df[split_indices, ]

test.lr.dm <-demo.df[!split_indices, ]

nrow(train.lr.dm)/nrow(demo.df)

nrow(test.lr.dm)/nrow(demo.df)

# Model 1
logistic_demo_1 <- glm(Performance.Tag.x ~ ., family = "binomial", data = train.lr.dm[,-1])

summary(logistic_demo_1)

logistic_demo_2 <- stepAIC(logistic_demo_1, direction = "both")
summary(logistic_demo_2 )

#Call:
#  glm(formula = Performance.Tag.x ~ Income + No.of.months.in.current.residence + 
#        No.of.months.in.current.company + EducationOthers + ProfessionSE, 
#      family = "binomial", data = train.lr.dm[, -1])
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.5166  -0.3154  -0.2871  -0.2600   2.7881  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)                       -2.6194684  0.0666961 -39.275  < 2e-16 ***
#  Income                            -0.0145657  0.0015146  -9.617  < 2e-16 ***
#  No.of.months.in.current.residence  0.0011411  0.0006015   1.897   0.0578 .  
#  No.of.months.in.current.company   -0.0056637  0.0011264  -5.028 4.95e-07 ***
#  EducationOthers                    0.6928695  0.3961775   1.749   0.0803 .  
#  ProfessionSE                       0.1038640  0.0549811   1.889   0.0589 .  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 17098  on 48890  degrees of freedom
#Residual deviance: 16970  on 48885  degrees of freedom
#AIC: 16982
#
#Number of Fisher Scoring iterations: 6


vif(logistic_demo_2)
#  Income  
#  1.019561
#  No.of.months.in.current.residence
#  1.018300 
#  No.of.months.in.current.company                   
#  1.017830                         
#  ProfessionSE 
#  1.000106 
#  EducationOthers 
#  1.000400 

# removing No.of.months.in.current.residence as it has low significance

logistic_demo_3 <- glm(formula = Performance.Tag.x ~ Income  + 
                       No.of.months.in.current.company + EducationOthers + ProfessionSE, 
                       family = "binomial", data = train.lr.dm[, -1])

summary(logistic_demo_3)
vif(logistic_demo_3)


# removing ProfessionSE as it has low significance

logistic_demo_4 <- glm(formula = Performance.Tag.x ~ Income  + 
                         No.of.months.in.current.company + EducationOthers , 
                       family = "binomial", data = train.lr.dm[, -1])

summary(logistic_demo_4)
vif(logistic_demo_4)

logistic_demo_5 <- glm(formula = Performance.Tag.x ~ Income  + 
                         No.of.months.in.current.company , 
                       family = "binomial", data = train.lr.dm[, -1])

summary(logistic_demo_5)
vif(logistic_demo_5)

# Looks like the only significant variables when it comes to demographics are Income 
# and  No.of.months.in.current.company

Final_DemoGraphic_model <- logistic_demo_5
summary(Final_DemoGraphic_model)

#Call:
#glm(formula = Performance.Tag.x ~ Income + No.of.months.in.current.company, 
#    family = "binomial", data = train.lr.dm[, -1])
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.3739  -0.3158  -0.2878  -0.2609   2.7885  

#Coefficients:
#                                   Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)                     -2.540831   0.058651 -43.321  < 2e-16 ***
#  Income                          -0.014877   0.001507  -9.871  < 2e-16 ***
#  No.of.months.in.current.company -0.005862   0.001121  -5.229  1.7e-07 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 17098  on 48890  degrees of freedom
#Residual deviance: 16980  on 48888  degrees of freedom
#AIC: 16986
#
#Number of Fisher Scoring iterations: 6

test_demo_pred = predict (Final_DemoGraphic_model, type = "response",
                          newdata= test.lr.dm[,-9])

summary(test_demo_pred)


test_demo_perf <- factor(ifelse(test_demo_pred >= 0.04222, "Yes", "No"))
test_actual_perf<- factor(ifelse(test.lr.dm$Performance.Tag.x==1,"Yes","No"))
table(test_demo_perf,test_actual_perf)


test_conf <- confusionMatrix(test_demo_perf, test_actual_perf, positive = "Yes")
test_conf

#Confusion Matrix and Statistics
#
#Reference
#Prediction    No   Yes
#       No  10727   398
#       Yes  9342   486
#           
#           Accuracy : 0.5352          
#             95% CI : (0.5284, 0.5419)
#No Information Rate : 0.9578          
#P-Value [Acc > NIR] : 1               
#
#Kappa : 0.0144          
#Mcnemar's Test P-Value : <2e-16          
#                                          
#            Sensitivity : 0.54977         
#            Specificity : 0.53451         
#         Pos Pred Value : 0.04945         
#         Neg Pred Value : 0.96422         
#             Prevalence : 0.04219         
#         Detection Rate : 0.02319         
#   Detection Prevalence : 0.46905         
#      Balanced Accuracy : 0.54214         
#                                          
#       'Positive' Class : Yes      

##################################################################################################################################
# Master data model

set.seed(1)

split_indices <- sample.split(Master.df$Performance.Tag.x, SplitRatio = 0.7)

train.lr.master <- Master.df[split_indices, ]

test.lr.master <-Master.df[!split_indices, ]

nrow(train.lr.master)/nrow(Master.df)

nrow(test.lr.master)/nrow(Master.df)

# Model 1
logistic_master_1 <- glm(Performance.Tag.x ~ ., family = "binomial", data = train.lr.master[,-1])

summary(logistic_master_1)

logistic_master_2 <- stepAIC(logistic_master_1, direction = "both")
# Performance.Tag.x ~ Income + No.of.months.in.current.residence + 
#  No.of.months.in.current.company + No.of.times.60.DPD.or.worse.in.last.6.months + 
#  No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
#  Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
#  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
#  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
#  Total.No.of.Trades + NoCC + EducationOthers
summary(logistic_master_2 )
vif(logistic_master_2)

# removing No.of.times.60.DPD.or.worse.in.last.6.months due to high VIF and low significance

logistic_master_3 <-glm(formula = Performance.Tag.x ~ Income + 
      No.of.months.in.current.residence + No.of.months.in.current.company +  
      No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
      Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
      No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
      Total.No.of.Trades + NoCC + EducationOthers, family = "binomial", 
    data = train.lr.master[, -1])
summary(logistic_master_3 )
vif(logistic_master_3)

# removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. due to high VIF and low significance
logistic_master_4 <-glm(formula = Performance.Tag.x ~ Income + 
                          No.of.months.in.current.residence + No.of.months.in.current.company +  
                          No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                          NoCC + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades + EducationOthers, family = "binomial", 
                        data = train.lr.master[, -1])
summary(logistic_master_4 )
vif(logistic_master_4)

# removing  No.of.times.90.DPD.or.worse.in.last.12.months  as it is has high VIF low significance

logistic_master_5 <-glm(formula = Performance.Tag.x ~ Income + 
                          No.of.months.in.current.residence + No.of.months.in.current.company +  
                          No.of.times.30.DPD.or.worse.in.last.6.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                          NoCC + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades + EducationOthers, family = "binomial", 
                        data = train.lr.master[, -1])
summary(logistic_master_5)
vif(logistic_master_5)


#Removing No.of.months.in.current.residence due to high VIF and low significance

logistic_master_6 <-glm(formula = Performance.Tag.x ~ Income + 
                          No.of.months.in.current.company +  
                          No.of.times.30.DPD.or.worse.in.last.6.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                          NoCC + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades + EducationOthers, family = "binomial", 
                        data = train.lr.master[, -1])
summary(logistic_master_6)
vif(logistic_master_6)

#Removing NoCC due to high VIF and low significance

logistic_master_7 <-glm(formula = Performance.Tag.x ~ Income + 
                          No.of.months.in.current.company +  
                          No.of.times.30.DPD.or.worse.in.last.6.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades + EducationOthers, family = "binomial", 
                        data = train.lr.master[, -1])
summary(logistic_master_7)
vif(logistic_master_7)


# Removing EducationOthers as it has very low significance

logistic_master_8 <-glm(formula = Performance.Tag.x ~ Income + 
                          No.of.months.in.current.company +  
                          No.of.times.30.DPD.or.worse.in.last.6.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades , family = "binomial", 
                        data = train.lr.master[, -1])
summary(logistic_master_8)
vif(logistic_master_8)

# removing No.of.months.in.current.company due tolow significance

logistic_master_9 <-glm(formula = Performance.Tag.x ~ Income + 
                          No.of.times.30.DPD.or.worse.in.last.6.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades , family = "binomial", 
                        data = train.lr.master[, -1])
summary(logistic_master_9)
vif(logistic_master_9)


# removing Income due tolow significance

logistic_master_10 <-glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                          Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                          Total.No.of.Trades , family = "binomial", 
                        data = train.lr.master[, -1])
summary(logistic_master_10)
vif(logistic_master_10)

Final_master_model <- logistic_master_10
summary(Final_master_model)

#Call:
# glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
#        Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
#        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
#        Total.No.of.Trades, family = "binomial", data = train.lr.master[,-1])
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.7732  -0.3335  -0.2491  -0.2078   2.8476  
#
#Coefficients:
#                                                                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                                     -3.7963445  0.0524945 -72.319  < 2e-16 ***
#  No.of.times.30.DPD.or.worse.in.last.6.months                     0.2182971  0.0198880  10.976  < 2e-16 ***
#  Avgas.CC.Utilization.in.last.12.months                           0.0064468  0.0008064   7.995 1.30e-15 ***
#  No.of.PL.trades.opened.in.last.12.months                         0.1450270  0.0202242   7.171 7.45e-13 ***
#  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.  0.0523859  0.0096308   5.439 5.35e-08 ***
#  Total.No.of.Trades                                              -0.0367453  0.0077167  -4.762 1.92e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 17098  on 48890  degrees of freedom
#Residual deviance: 16434  on 48885  degrees of freedom
#AIC: 16446
#
#Number of Fisher Scoring iterations: 6
######################
test_master_pred = predict (Final_master_model, type = "response",
                            newdata= test.lr.master[,-25])

summary(test_master_pred)


test_master_perf <- factor(ifelse(test_master_pred >= 0.04237, "Yes", "No"))
test_actual_master_perf<- factor(ifelse(test.lr.master$Performance.Tag.x==1,"Yes","No"))
table(test_master_perf,test_actual_master_perf)


test_conf <- confusionMatrix(test_master_perf, test_actual_master_perf, positive = "Yes")
test_conf
#Confusion Matrix and Statistics

#Reference
#Prediction    No   Yes
#       No  11953   306
#       Yes  8116   578

#           Accuracy : 0.5981          
#             95% CI : (0.5914, 0.6047)
#No Information Rate : 0.9578          
#P-Value [Acc > NIR] : 1               
#
#Kappa : 0.0478          
#Mcnemar's Test P-Value : <2e-16          
#
#         Sensitivity : 0.65385         
#         Specificity : 0.59560         
#      Pos Pred Value : 0.06648         
#      Neg Pred Value : 0.97504         
#          Prevalence : 0.04219         
#      Detection Rate : 0.02759         
#Detection Prevalence : 0.41493         
#   Balanced Accuracy : 0.62472         
#
#    'Positive' Class : Yes             

##################################################################################################################################
# WOE imputed data model


set.seed(1)

split_indices <- sample.split(WOE.df$Performance.Tag.x, SplitRatio = 0.7)

train.lr.WOE <- WOE.df[split_indices, ]

test.lr.WOE <-WOE.df[!split_indices, ]

nrow(train.lr.WOE)/nrow(WOE.df)

nrow(test.lr.WOE)/nrow(WOE.df)

# Model 1
logistic_WOE_1 <- glm(Performance.Tag.x ~ ., family = "binomial", data = train.lr.WOE[,-1])
summary(logistic_WOE_1)

logistic_WOE_2 <- stepAIC(logistic_WOE_1, direction = "both")

summary (logistic_WOE_2)
vif(logistic_WOE_2)
# removing  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 
logistic_WOE_3 <- glm(formula = Performance.Tag.x ~ No.of.months.in.current.residence + 
                        No.of.months.in.current.company + No.of.times.30.DPD.or.worse.in.last.6.months + 
                        No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades + NoCC + Age_WOE + Income_WOE + Balance_WOE + 
                        EducationOthers, family = "binomial", data = train.lr.WOE[,-1])

summary (logistic_WOE_3)
vif(logistic_WOE_3)

# removing No.of.times.90.DPD.or.worse.in.last.12.months

logistic_WOE_4 <- glm(formula = Performance.Tag.x ~ No.of.months.in.current.residence + 
                        No.of.months.in.current.company + No.of.times.30.DPD.or.worse.in.last.6.months + 
                        Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades + NoCC + Age_WOE + Income_WOE + Balance_WOE + 
                        EducationOthers, family = "binomial", data = train.lr.WOE[,-1])

summary (logistic_WOE_4)
vif(logistic_WOE_4)

# removing No.of.months.in.current.residence

logistic_WOE_5 <- glm(formula = Performance.Tag.x ~ No.of.months.in.current.company + 
                        No.of.times.30.DPD.or.worse.in.last.6.months + 
                        Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades + NoCC + Age_WOE + Income_WOE + Balance_WOE + 
                        EducationOthers, family = "binomial", data = train.lr.WOE[,-1])

summary (logistic_WOE_5)
vif(logistic_WOE_5)


# removing EducationOthers

logistic_WOE_6 <- glm(formula = Performance.Tag.x ~ No.of.months.in.current.company + 
                        No.of.times.30.DPD.or.worse.in.last.6.months + 
                        Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades + NoCC + Age_WOE + Income_WOE + 
                        Balance_WOE , family = "binomial", data = train.lr.WOE[,-1])

summary (logistic_WOE_6)
vif(logistic_WOE_6)

# removing No.of.months.in.current.company 

logistic_WOE_7 <- glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                        Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades + NoCC + Age_WOE + Income_WOE + 
                        Balance_WOE , family = "binomial", data = train.lr.WOE[,-1])

summary (logistic_WOE_7)
vif(logistic_WOE_7)


# removing Income_WOE
logistic_WOE_8 <- glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                        Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades + NoCC + Age_WOE + 
                        Balance_WOE , family = "binomial", data = train.lr.WOE[,-1])

summary (logistic_WOE_8)
vif(logistic_WOE_8)


# removing Age_WOE
logistic_WOE_9 <- glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                        Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades + NoCC +  
                        Balance_WOE , family = "binomial", data = train.lr.WOE[,-1])

summary (logistic_WOE_9)
vif(logistic_WOE_9)

# removing NoCC
logistic_WOE_10 <- glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                        Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades +  
                        Balance_WOE , family = "binomial", data = train.lr.WOE[,-1])

summary (logistic_WOE_10)
vif(logistic_WOE_10)



Final_WOE_model <- logistic_WOE_10
summary(Final_WOE_model)

#glm(formula = Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
#      Avgas.CC.Utilization.in.last.12.months + No.of.PL.trades.opened.in.last.12.months + 
#      No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
#      Total.No.of.Trades + Balance_WOE, family = "binomial", data = train.lr.WOE[, 
#                                                                                -1])
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.7016  -0.3382  -0.2578  -0.1953   2.8928  
#
#Coefficients:
#                                                                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                                       -3.5970944  0.0619715 -58.044  < 2e-16 ***
#  No.of.times.30.DPD.or.worse.in.last.6.months                     0.1980481  0.0202503   9.780  < 2e-16 ***
#  Avgas.CC.Utilization.in.last.12.months                           0.0045058  0.0008722   5.166 2.39e-07 ***
#  No.of.PL.trades.opened.in.last.12.months                         0.1048143  0.0213486   4.910 9.12e-07 ***
#  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.  0.0415969  0.0098246   4.234 2.30e-05 ***
#  Total.No.of.Trades                                              -0.0325741  0.0076504  -4.258 2.06e-05 ***
#  Balance_WOE                                                     -0.4191353  0.0701857  -5.972 2.35e-09 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 17098  on 48890  degrees of freedom
#Residual deviance: 16397  on 48884  degrees of freedom
#AIC: 16411
#
#Number of Fisher Scoring iterations: 6
test_WOE_pred = predict (Final_WOE_model, type = "response",
                         newdata= test.lr.WOE[,-5])
summary(test_WOE_pred)

test_WOE_perf <- factor(ifelse(test_WOE_pred >= 0.04239, "Yes", "No"))
test_actual_WOE_perf<- factor(ifelse(test.lr.WOE$Performance.Tag.x==1,"Yes","No"))
table(test_WOE_perf,test_actual_WOE_perf)


test_conf <- confusionMatrix(test_WOE_perf, test_actual_WOE_perf, positive = "Yes")
test_conf
#Confusion Matrix and Statistics

#Reference
#Prediction    No   Yes
#       No  11544   286
#       Yes  8525   598
#
#           Accuracy : 0.5795          
#             95% CI : (0.5728, 0.5862)
#No Information Rate : 0.9578          
#P-Value [Acc > NIR] : 1               

#Kappa : 0.0461          
#Mcnemar's Test P-Value : <2e-16          
#                                          
#            Sensitivity : 0.67647         
#            Specificity : 0.57522         
#         Pos Pred Value : 0.06555         
#         Neg Pred Value : 0.97582         
#             Prevalence : 0.04219         
#         Detection Rate : 0.02854         
#   Detection Prevalence : 0.43540         
#      Balanced Accuracy : 0.62584         
#                                          
#       'Positive' Class : Yes    


#picking the WOEmodel as the reference for Model evaluation
##################################################################################################################################
# Model evaluation
##################################################################################################################################
#Functions for Model evalution
perform_fn <- function(cutoff, test_data_prediction) {
  predicted_response <- factor(ifelse(test_data_prediction >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_response, test_actual_WOE_perf, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

get_optimal_cutoff <- function (test_data_prediction, threshold_start, threshold_end, rowname) {
  # Summary of test probability
  summary(test_data_prediction)
  
  s = seq(threshold_start, threshold_end, length=100)
  OUT = matrix(0,100,3)
  
  for(i in 1:100) {
    OUT[i,] = perform_fn(s[i], test_data_prediction)
  } 
  
  OUT <- data.frame(cbind(OUT, s))
  colnames(OUT) <- c("sensitivity", "specificity", "accuracy", "steps")
  
  print(ggplot(OUT, aes(x = steps)) + 
          geom_line(aes(y = sensitivity, colour="red"), size = 1.1) + 
          geom_line(aes(y = specificity, colour = "green"), size = 1.1) + 
          geom_line(aes(y = accuracy, colour = "blue"), size = 1.1) + 
          ylab(label="Values") + 
          xlab("Cutoff") + 
          scale_color_discrete(name = "Data Lines", labels = c("accuracy", "sensitivity", "specificity")))
  
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
  
  cat("Optimal Cutoff = ", round(cutoff,3)[1])
  
  test_pred <- factor(ifelse(test_data_prediction >= round(cutoff,3)[1], "Yes", "No"))
  test_conf <- confusionMatrix(test_pred, test_actual_WOE_perf, positive = "Yes")
  roc_metrics <- roc.curve(test_actual_WOE_perf, test_pred, plotit = T)
  
  metrics <- data.frame(Accuracy=test_conf$overall[1], 
                        Sensitivity=test_conf$byClass[1], 
                        Specificity = test_conf$byClass[2],
                        AUC=roc_metrics$auc,
                        False_positive_Rate=roc_metrics$false.positive.rate[2],
                        True_positive_Rate=roc_metrics$true.positive.rate[2])
  
  rownames(metrics) <- rowname
  
  return(metrics)
}

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

gain_lift_chart_with_KAStatistics <- function(model, data, value) {
  
  temp <- data
  temp$Predict <- predict(model, type=value, newdata=temp)
  
  LG = lift(temp$Performance, temp$Predict, groups = 10)
  Random.Gain <- seq(from=10,to=100,by=10)
  
  response_decile <- cbind(LG, Random.Gain)
  
  print(ggplot(data = response_decile, aes(x = bucket)) +
    geom_line(aes(y = Gain, color = "red"), size = 1.1) + 
    geom_point(aes(y = Gain), shape = 0, size = 3, stroke= 1.1) +
    geom_text(aes(y = Gain, label=round(Gain, digits = 1)),hjust=-1, vjust=0) +
    geom_line(aes(y = Random.Gain, color="green"), size = 1.1) + 
    ylab(label="Bucket") + 
    xlab("Gain") +
    scale_x_continuous(breaks=seq(0, 10, 1)) +
    scale_color_discrete(name = "Data Lines", labels = c("Random Gain", "Gain")))
  
  Lift <- response_decile$Gain/Random.Gain
  Random.Lift <- Random.Gain/Random.Gain  
  
  response_decile <- cbind(response_decile, Lift, Random.Lift)
  
  print(ggplot(data = response_decile, aes(x = bucket)) +
    geom_line(aes(y = Lift, color = "red"), size = 1.1) + 
    geom_point(aes(y = Lift), shape = 5, size = 3, stroke= 1.1) +
    geom_text(aes(y = Lift, label=round(Lift, digits = 1)),hjust=-1, vjust=0) +
    geom_line(aes(y = Random.Lift, color="green"), size = 1.1) + 
    geom_point(y = Random.Lift, shape = 1, size = 3, stroke= 1.1) +
    ylab(label="Bucket") + 
    xlab("Lift") +
    scale_x_continuous(breaks=seq(0, 10, 1)) +
    scale_color_discrete(name = "Data Lines", labels = c("Random Lift", "Lift")))
  
  # KS-Statistic
  if(value=="raw"){
    pred_object_test<- prediction(as.numeric(temp$Predict), as.numeric(temp$Performance))
  }else{
    pred_object_test<- prediction(temp$Predict,temp$Performance)
  }
  
  performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])

  print(LG)
  max(ks_table_test)
} 
##################################################################################################################################
# Building Model_metrics that will help evaluate the models


test_actual_WOE_perf <- factor(ifelse(test.lr.WOE$Performance.Tag.x == 1, "Yes", "No"))

test_WOE_pred = predict (Final_WOE_model, type = "response",
                         newdata= test.lr.WOE[,-5])

summary(test_WOE_pred)
model_metrics <- get_optimal_cutoff(test_WOE_pred, .0141, .20883, "Logistic Regression Model")

test_WOE_perf <- factor(ifelse(test_WOE_pred >= 0.046, "Yes", "No"))
test_actual_WOE_perf<- factor(ifelse(test.lr.WOE$Performance.Tag.x==1,"Yes","No"))
table(test_WOE_perf,test_actual_WOE_perf)
test_conf <- confusionMatrix(test_WOE_perf, test_actual_WOE_perf, positive = "Yes")
test_conf

##############################################
# Optimal Cutoff =  0.046
###############################################

# Caret models
train.lr.WOE$Performance.Tag.x <- as.factor(train.lr.WOE$Performance.Tag.x)

glm_down_model <- caret::train(Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                                  Avgas.CC.Utilization.in.last.12.months + 
                                  No.of.PL.trades.opened.in.last.12.months + 
                                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                                  Total.No.of.Trades +  
                                  Balance_WOE,
                               data = train.lr.WOE,
                               method = "glm",
                               family="binomial",
                               preProcess = c("scale", "center"),
                               tuneLength = 5,
                               trControl = trainControl(method = "cv", 
                                                        number = 5, 
                                                        verboseIter = TRUE,
                                                        sampling = "down"))

glm_down_model.pred <- predict(glm_down_model, test.lr.WOE, preProcess = c("center", "scale"),type = "prob")
summary(glm_down_model.pred)
# 0                1         
# Min.   :0.1230   Min.   :0.2333  
# 1st Qu.:0.4221   1st Qu.:0.3085  
# Median :0.5477   Median :0.4523  
# Mean   :0.5398   Mean   :0.4602  
# 3rd Qu.:0.6915   3rd Qu.:0.5779  
# Max.   :0.7667   Max.   :0.8770  
model_metrics <- rbind(model_metrics, get_optimal_cutoff(glm_down_model.pred[[1]], .2481, .8644, "GLM Down Model"))
##############################################
# Optimal Cutoff =  0.485
###############################################
glm_up_model <- caret::train(Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                                Avgas.CC.Utilization.in.last.12.months + 
                                No.of.PL.trades.opened.in.last.12.months + 
                                No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                                Total.No.of.Trades +  
                                Balance_WOE,
                             data = train.lr.WOE,
                             method = "glm",
                             family="binomial",
                             preProcess = c("scale", "center"),
                             tuneLength = 5,
                             trControl = trainControl(method = "cv", 
                                                      number = 5, 
                                                      verboseIter = TRUE,
                                                      sampling = "up"))

glm_up_model.pred <- predict(glm_up_model, test.lr.WOE, preProcess = c("center", "scale"),type = "prob")
summary(glm_up_model.pred)
# 0                1         
# Min.   :0.1322   Min.   :0.2445  
# 1st Qu.:0.4178   1st Qu.:0.3027  
# Median :0.5359   Median :0.4641  
# Mean   :0.5394   Mean   :0.4606  
# 3rd Qu.:0.6973   3rd Qu.:0.5822  
# Max.   :0.7555   Max.   :0.8678  
model_metrics <- rbind(model_metrics, get_optimal_cutoff(glm_up_model.pred[[1]], .2445, .8678, "GLM Up Model"))
##############################################
# Optimal Cutoff =  0.477
##############################################

glm_smote_model <- caret::train(Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                                  Avgas.CC.Utilization.in.last.12.months + 
                                  No.of.PL.trades.opened.in.last.12.months + 
                                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                                  Total.No.of.Trades +  
                                  Balance_WOE,
                                data = train.lr.WOE,
                                method = "glm",
                                family="binomial",
                                preProcess = c("scale", "center"),
                                tuneLength = 5,
                                trControl = trainControl(method = "cv", 
                                                         number = 5, 
                                                         verboseIter = TRUE,
                                                         sampling = "smote"))


glm_smote_model.pred <- predict(glm_smote_model, test.lr.WOE, preProcess = c("center", "scale"),type = "prob")
summary(glm_smote_model.pred)
# 0                1         
# Min.   :0.1721   Min.   :0.1938  
# 1st Qu.:0.4935   1st Qu.:0.2511  
# Median :0.6095   Median :0.3905  
# Mean   :0.6041   Mean   :0.3959  
# 3rd Qu.:0.7489   3rd Qu.:0.5065  
# Max.   :0.8062   Max.   :0.8279  
model_metrics <- rbind(model_metrics, get_optimal_cutoff(glm_smote_model.pred[[1]], .1938, .8279, "GLM SMOTE Model"))


glmnet_model <- train(Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                        Avgas.CC.Utilization.in.last.12.months + 
                        No.of.PL.trades.opened.in.last.12.months + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                        Total.No.of.Trades +  
                        Balance_WOE,
                      data = train.lr.WOE, 
                      method = "glmnet", 
                      trControl =  trainControl(method = "cv", 
                                                number = 5, 
                                                verboseIter = TRUE,
                                                sampling = "smote"),
                      tuneGrid = expand.grid(alpha = 1,
                                             lambda = seq(0.001,0.1,by = 0.001)))

glmnet_model.pred <- predict(glmnet_model, test.lr.WOE, preProcess = c("center", "scale"),type = "prob")
summary(glmnet_model.pred)
# 0                1         
# Min.   :0.5498   Min.   :0.3945  
# 1st Qu.:0.5549   1st Qu.:0.3964  
# Median :0.5732   Median :0.4268  
# Mean   :0.5751   Mean   :0.4249  
# 3rd Qu.:0.6036   3rd Qu.:0.4451  
# Max.   :0.6055   Max.   :0.4502  
model_metrics <- rbind(model_metrics, get_optimal_cutoff(glm_down_model.pred[[1]], .3945, .4502, "GLMNET Model"))
# Not overlapping


decision_tree_smote_model <- caret::train(Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                                            Avgas.CC.Utilization.in.last.12.months + 
                                            No.of.PL.trades.opened.in.last.12.months + 
                                            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                                            Total.No.of.Trades +  
                                            Balance_WOE,
                                          data = train.lr.WOE,
                                          method = "rpart",
                                          preProcess = c("scale", "center"),
                                          tuneLength = 5,
                                          trControl = trainControl(method = "cv", 
                                                                   number = 5, 
                                                                   verboseIter = TRUE,
                                                                   sampling = "smote"))

decision_tree_smote_model.pred <- predict(decision_tree_smote_model, test.lr.WOE, preProcess = c("center", "scale"),type = "prob")
summary(decision_tree_smote_model.pred)
# 0                1         
# Min.   :0.0000   Min.   :0.0000  
# 1st Qu.:0.6667   1st Qu.:0.1008  
# Median :0.8511   Median :0.1489  
# Mean   :0.7378   Mean   :0.2622  
# 3rd Qu.:0.8992   3rd Qu.:0.3333  
# Max.   :1.0000   Max.   :1.0000  
model_metrics <- rbind(model_metrics, get_optimal_cutoff(decision_tree_smote_model.pred[[1]], 0, 1, "Decision Tree Model"))
# Optimal Cutoff =  0.818


random_forest_smote_model <- caret::train(Performance.Tag.x ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                                            Avgas.CC.Utilization.in.last.12.months + 
                                            No.of.PL.trades.opened.in.last.12.months + 
                                            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                                            Total.No.of.Trades +  
                                            Balance_WOE,
                                          data = train.lr.WOE,
                                          method = "rf",
                                          ntree = 1000,
                                          preProcess = c("scale", "center"),
                                          trControl = trainControl(method = "cv", 
                                                                   number = 5, 
                                                                   verboseIter = TRUE,
                                                                   sampling = "smote"))


random_forest_smote_model.pred <- predict(random_forest_smote_model, newdata=test.lr.WOE,type = "prob")
summary(random_forest_smote_model.pred)
# 0                1         
# Min.   :0.0260   Min.   :0.0000  
# 1st Qu.:0.6710   1st Qu.:0.0060  
# Median :0.8470   Median :0.1530  
# Mean   :0.8013   Mean   :0.1987  
# 3rd Qu.:0.9940   3rd Qu.:0.3290  
# Max.   :1.0000   Max.   :0.9740  

model_metrics <- rbind(model_metrics, get_optimal_cutoff(random_forest_smote_model.pred[[1]], 0, .9740, "Random Forest Smote Model"))
# Optimal Cutoff =  0.797


model_comparison <- resamples(list(GLM_DOWN_MODEL = glm_down_model, 
                                   GLM_UP_MODEL = glm_up_model, 
                                   GLM_SMOTE_MODEL = glm_smote_model,
                                   GLMNET_MODEL = glmnet_model,
                                   DECISION_TREE_SMOTE_MODEL = decision_tree_smote_model,
                                   RANDOM_FOREST_SMOTE_MODEL = random_forest_smote_model))
summary(model_comparison)

bwplot(model_comparison , metric = c("Kappa","Accuracy"))

densityplot(model_comparison , metric = "Kappa" ,auto.key = list(columns = 3))

model_metrics
#                           Accuracy Sensitivity Specificity       AUC False_positive_Rate True_positive_Rate
# Logistic Regression Model 0.6201021   0.6176471   0.6202103 0.6189287           0.3797897          0.6176471
# GLM Down Model            0.3825705   0.3766968   0.3828292 0.6202370           0.3766968          0.6171708
# GLM Up Model              0.3791343   0.3800905   0.3790921 0.6204087           0.3800905          0.6209079
# GLM SMOTE Model           0.3823796   0.3812217   0.3824306 0.6181738           0.3812217          0.6175694
# Decision Tree Model       0.4116833   0.4355204   0.4106333 0.5769232           0.4355204          0.5893667
# Random Forest Smote Model 0.4141650   0.4061086   0.4145199 0.5896857           0.4061086          0.5854801

# normal logistic regression model does very well



# Lift and Gain Chant with KS Statistics
gain_lift_chart_with_KAStatistics(Final_WOE_model, test.lr.WOE, "response")
# # A tibble: 10 x 6
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  2096      169.    169.  19.1    1.91
# 2      2  2095      161.    330.  37.3    1.87
# 3      3  2095      122.    452.  51.1    1.70
# 4      4  2096      110.    562.  63.6    1.59
# 5      5  2095       87.    649.  73.4    1.47
# 6      6  2095       86.    735.  83.1    1.39
# 7      7  2096       48.    783.  88.6    1.27
# 8      8  2095       55.    838.  94.8    1.18
# 9      9  2095       30.    868.  98.2    1.09
# 10     10  2095       16.    884. 100.     1.00
# [1] 0.2586011

gain_lift_chart_with_KAStatistics(glm_down_model, test.lr.WOE, "raw")
# # A tibble: 10 x 6
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  2096      130.    130.  14.7    1.47
# 2      2  2095      134.    264.  29.9    1.49
# 3      3  2095      152.    416.  47.1    1.57
# 4      4  2096      137.    553.  62.6    1.56
# 5      5  2095       94.    647.  73.2    1.46
# 6      6  2095       52.    699.  79.1    1.32
# 7      7  2096       54.    753.  85.2    1.22
# 8      8  2095       39.    792.  89.6    1.12
# 9      9  2095       45.    837.  94.7    1.05
# 10     10  2095       47.    884. 100.     1.00
# [1] 0.2560669

gain_lift_chart_with_KAStatistics(glm_up_model, test.lr.WOE, "raw")
# # A tibble: 10 x 6
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  2096      133.    133.  15.0    1.50
# 2      2  2095      134.    267.  30.2    1.51
# 3      3  2095      151.    418.  47.3    1.58
# 4      4  2096      133.    551.  62.3    1.56
# 5      5  2095       90.    641.  72.5    1.45
# 6      6  2095       51.    692.  78.3    1.30
# 7      7  2096       54.    746.  84.4    1.21
# 8      8  2095       38.    784.  88.7    1.11
# 9      9  2095       49.    833.  94.2    1.05
# 10     10  2095       51.    884. 100.     1.00
# [1] 0.2519258

gain_lift_chart_with_KAStatistics(glm_smote_model, test.lr.WOE, "raw")
# # A tibble: 10 x 6
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  2096      142.    142.  16.1    1.61
# 2      2  2095      166.    308.  34.8    1.74
# 3      3  2095      125.    433.  49.0    1.63
# 4      4  2096       68.    501.  56.7    1.42
# 5      5  2095       69.    570.  64.5    1.29
# 6      6  2095       70.    640.  72.4    1.21
# 7      7  2096       59.    699.  79.1    1.13
# 8      8  2095       69.    768.  86.9    1.09
# 9      9  2095       62.    830.  93.9    1.04
# 10     10  2095       54.    884. 100.     1.00
# [1] 0.2065533

gain_lift_chart_with_KAStatistics(glmnet_model, test.lr.WOE, "raw")
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  2096       89.     89.  10.1   1.01 
# 2      2  2095       91.    180.  20.4   1.02 
# 3      3  2095       83.    263.  29.8   0.992
# 4      4  2096       98.    361.  40.8   1.02 
# 5      5  2095       90.    451.  51.0   1.02 
# 6      6  2095       78.    529.  59.8   0.997
# 7      7  2096      104.    633.  71.6   1.02 
# 8      8  2095       84.    717.  81.1   1.01 
# 9      9  2095       78.    795.  89.9   0.999
# 10     10  2095       89.    884. 100.    1.00 
# [1] 0

gain_lift_chart_with_KAStatistics(decision_tree_smote_model, test.lr.WOE, "raw")
# # A tibble: 10 x 6
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  2096      120.    120.  13.6    1.36
# 2      2  2095      139.    259.  29.3    1.46
# 3      3  2095       85.    344.  38.9    1.30
# 4      4  2096       86.    430.  48.6    1.22
# 5      5  2095       87.    517.  58.5    1.17
# 6      6  2095       79.    596.  67.4    1.12
# 7      7  2096       73.    669.  75.7    1.08
# 8      8  2095       71.    740.  83.7    1.05
# 9      9  2095       75.    815.  92.2    1.02
# 10     10  2095       69.    884. 100.     1.00
# [1] 0.09620334


gain_lift_chart_with_KAStatistics(random_forest_smote_model, test.lr.WOE, "raw")
# # A tibble: 10 x 6
# bucket total totalresp Cumresp  Gain Cumlift
# <int> <int>     <dbl>   <dbl> <dbl>   <dbl>
#   1      1  2096      152.    152.  17.2    1.72
# 2      2  2095       74.    226.  25.6    1.28
# 3      3  2095       90.    316.  35.7    1.19
# 4      4  2096       80.    396.  44.8    1.12
# 5      5  2095       92.    488.  55.2    1.10
# 6      6  2095       82.    570.  64.5    1.07
# 7      7  2096       87.    657.  74.3    1.06
# 8      8  2095       72.    729.  82.5    1.03
# 9      9  2095       74.    803.  90.8    1.01
# 10     10  2095       81.    884. 100.     1.00
# [1] 0.06906963



##################################################################################################################################

##################################################################################################################################



################### Creating Application Score Card ##########################

# Considering only WOE model for entire data frame
Final.model.cutoff = 0.046 # cutoff from the final model
Final.df =  filtered_combined.df
Final.df$bad = predict (Final_WOE_model, type = "response",
                                        newdata= Final.df)

Final.df$good = (1 - Final.df$bad) 

Final.df$odds = Final.df$good / Final.df$bad
Final.df$log_odds <- log(Final.df$odds)

factorOdds <- 20/log(2) # Odds to double 20 points
factorOdds # 28.85
offset_factorOdds <- 400 - (factorOdds*log(10)) #since good to bad odds of 10 to 1 at a score of 400 
Final.df$score <- offset_factorOdds + factorOdds * Final.df$log_odds

# Score V/S Default
ggplot(Final.df, aes_string(x="Performance.Tag.x",y="score", fill="Performance.Tag.x")) +
  geom_boxplot(width=0.2) +  
  labs(title=paste(" ","Applicant Default V/S Score - Approved Customers"), y="Score",x="Applicant Default",fill="Performance.Tag.x")


ggplot(Final.df, aes_string(x="score", fill="Performance.Tag.x")) +
  geom_histogram(bins = 10, col = "black") +  
  labs(title=paste(" ","Applicant Default V/S Score - Approved Customers"), y="Score",x="Applicant Default",fill="Performance.Tag.x")

cut_off_score <- offset_factorOdds + factorOdds * log((1-Final.model.cutoff)/Final.model.cutoff)
cut_off_score #421.05

summary(Final.df$score)
hist(Final.df$score)
# creating application score card for the rejected application 
rejected_application.df$bad = predict (Final_WOE_model, type = "response",
                        newdata= rejected_application.df)
rejected_application.df$good = (1 - rejected_application.df$bad) 
rejected_application.df$odds = rejected_application.df$good / rejected_application.df$bad
rejected_application.df$log_odds <- log(rejected_application.df$odds)
rejected_application.df$score <- offset_factorOdds + factorOdds * rejected_application.df$log_odds
rejected_application.df$pred_performance_tag <- ifelse(rejected_application.df$score >= cut_off_score,1,0)
table(rejected_application.df$pred_performance_tag)
prop.table(table(rejected_application.df$pred_performance_tag))
# In the rejected population 29 i.e. ~2% good application were rejected 
#Rejected Population
boxplot(rejected_application.df$score, ylab = "Score", main = "Score - Rejected Customers")
#misclassified rate in the approved population
# 1 is approved and 0 is not approved. 
Final.df$pred_performance_tag <- ifelse(Final.df$score >= cut_off_score,1,0)
prop.table(table(Final.df$pred_performance_tag))
#Auto Approval rate = 61.33%
# The model will approve automatically 61.3% of the applicants
# cor(Final.df[,c("score","good")])
Final.df$is_miss_classified <- ifelse(Final.df$Performance.Tag.x == Final.df$pred_performance_tag,1,0)
miss_classified_percentage <- sum(Final.df$is_miss_classified)/nrow(Final.df) * 100
miss_classified_percentage # 37.6%
boxplot(Final.df$score)

summary(Final.df$score)
summary(rejected_application.df$score)
############### Expected Creadit Loss ################  
#Expected loss(c1) = PD * EAD * LGD
  #PD = Probability of defafault of each customer, EAD = Exposure at default or oustanding
#LGD = Loss given default.
#Lets assume if recovery likelihood is 30% then LDG = 1  - 0.30 = 0.7
# All the values are in millions

LGD = 1 - 0.3
Final.df$expected_loss = Final.df$bad * Final.df$Outstanding.Balance.in.m * LGD
# Total Total Loss expected
# sum(Final.df$Final.df$expected_loss_existing)
sum(Final.df$expected_loss)
# total probabilty 


potential_credit_loss_df <- Final.df[, c("Performance.Tag.x", "bad","Outstanding.Balance.in.m")]

default_cust <- Final.df[(Final.df$Performance.Tag.x == 1),]                                             
sum(default_cust$Outstanding.Balance.in.m*.3)

default_cust$expected_loss <- default_cust$bad * default_cust$Outstanding.Balance.in.m * LGD

total_expected_loss = sum(default_cust$expected_loss)
total_extected_loss_default_cust <- sum(default_cust$expected_loss)
print(total_expected_loss)
print(total_extected_loss_default_cust)

#Expected loss by rejeted customers

prop.table(table(rejected_application.df$pred_performance_tag))
# In the rejected population 29 i.e. ~2% good application were rejected 
boxplot(rejected_application.df$score)
hist(rejected_application.df$score)



rejected_application.df$expected_loss = rejected_application.df$bad * rejected_application.df$Outstanding.Balance.in.m * LGD
# rejecteddata_loss <- rejecteddata_scorecard[,c("Score","bad","Outstanding.Balance_imputed")]
rejected_application.approved <- rejected_application.df[(rejected_application.df$score >= cut_off_score),]
nrow(rejected_application.approved)
loss_by_rejected_good_customer <- sum(rejected_application.approved$Outstanding.Balance.in.m * 0.7)

sum(rejected_application.df$expected_loss)
loss_by_rejected_good_customer

Actual_risk_from_rejected_customers_after_using_model = sum(rejected_application.df$expected_loss) - loss_by_rejected_good_customer

## Total Expected loss from the rejected customers = 36.1 M
## Loss due to rejection of Good Customers = 39.57 M
## Actual risk due to rejected customers after using model   = 56.53
## The model would have reduced the loss due to rejected population by ~ 41%
###################################################################################################

# current approval system
#total approved application
nrow(Final.df)
#total rejected applications
nrow(rejected_application.df)
# total approved that defaulted
nrow(Final.df[which(Final.df$Performance.Tag.x==1),])
# total outstanding balance of defaulters
sum(Final.df$Outstanding.Balance.in.m[which(Final.df$Performance.Tag.x==1)])

# new system
# total Approvals
sum(nrow(Final.df[which(Final.df$score>=421.05),]),nrow(Final.df[which(rejected_application.df$score>=421.05),]))
# total rejections
sum(nrow(Final.df[which(Final.df$score<421.05),]),nrow(Final.df[which(rejected_application.df$score<421.05),]))

#total defaults on approvals
nrow(Final.df[which(Final.df$score>=421.05& Final.df$Performance.Tag.x==1),])
# % of defulters
nrow(Final.df[which(Final.df$score>=421.05& Final.df$Performance.Tag.x==1),])*100/sum(nrow(Final.df[which(Final.df$score>=421.05),]),nrow(Final.df[which(rejected_application.df$score>=421.05),]))

#Outstanding balance of defauters
sum(Final.df$Outstanding.Balance.in.m[which(Final.df$score>=421.05& Final.df$Performance.Tag.x==1)])

#outstanding balance of deafulters that will be rejected by new risk analysis system
sum(Final.df$Outstanding.Balance.in.m[which(Final.df$score<421.05& Final.df$Performance.Tag.x==1)])





