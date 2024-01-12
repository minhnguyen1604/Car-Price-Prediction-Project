install.packages("ggplot2")
install.packages("broom")
install.packages("ggpubr")
install.packages("dplyr", type = "binary")

#Working dir
setwd("C:/Users/Admin/IS_Subjects/Các mô hình thống kê dữ liệu 1")

# Load necessary library
library(dplyr)
library(ggplot2)
library(broom)
library(ggpubr)

#Read data
cars=read.csv("CarPrice_Assignment.csv", header=TRUE)
summary(cars)

#DATA CLEANING
# Drop the 'car_ID' column
cars <- select(cars, -car_ID)

# Extract the first word from 'CarName' to get the car company
cars$CarName <- sapply(strsplit(as.character(cars$CarName), " "), `[`, 1)

# Convert CarName to lowercase and handle duplicate values
cars$CarName <- tolower(cars$CarName)
cars$CarName <- gsub('maxda', 'mazda', cars$CarName)
cars$CarName <- gsub('porcshce', 'porsche', cars$CarName)
cars$CarName <- gsub('toyouta', 'toyota', cars$CarName)
cars$CarName <- gsub('vokswagen|vw', 'volkswagen', cars$CarName)

# Convert 'symboling' to character type
cars$symboling <- as.character(cars$symboling)

# Identify categorical and numerical columns
categorical_cols <- names(cars)[sapply(cars, class) %in% c('factor', 'character')]
numerical_cols <- names(cars)[!names(cars) %in% categorical_cols]

# Output the first few rows of the dataframe to check
head(cars)

#MLR
df_reg <- data.frame(brand=cars$CarName,
                     wheelchase = cars$wheelbase,
                     carlength = cars$carlength,
                     carwidth=cars$carwidth,
                     carheight=cars$carheight,
                     curbweight=cars$curbweight,
                     enginesize=cars$enginesize,
                     boreratio=cars$boreratio,
                     stroke=cars$stroke,
                     compressionratio=cars$compressionratio,
                     peakrpm=cars$peakrpm,
                     citympg=cars$citympg,
                     highwaympg=cars$highwaympg,
                     horsepower=cars$horsepower,
                     fueltype=cars$fueltype,
                     aspiration=cars$aspiration,
                     cyl=cars$cylindernumber,
                     price=cars$price)
head(df_reg)
# Base model
model<-lm(price~brand + wheelchase + carlength +carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+peakrpm+citympg+highwaympg+fueltype+aspiration+cyl,data=df_reg)
summary(model)

# Check for collinearity
library(car)
vif (model)

# Remove variables with VIF exceed threshold of 10
model1<-lm(price~brand + wheelchase + carlength +carwidth+carheight+curbweight+enginesize+boreratio+stroke+peakrpm+citympg+highwaympg+aspiration+cyl,data=df_reg)
summary(model)

# Check for collinearity after removing variables with VIF exceed threshold of 10
library(car)
vif (model1)

# Assumption 1
plot(model1, 1)
# -> If the residuals are spread equally around a horizontal line without distinct patterns (red line is approximately horizontal at zero), that is a good indication of having a linear relationship.

# Fix assumption 1 by taking natural log of price
model_fix_as1=lm(log(price)~brand+wheelchase + carlength +carwidth+carheight+curbweight+enginesize+boreratio+stroke+peakrpm+citympg+highwaympg+aspiration+cyl,data=df_reg)
plot(model_fix_as1, 1)

# Assumption 2
hist(model_fix_as1$residuals)
shapiro.test((resid(model_fix_as1)))
plot(model_fix_as1, 2)

# Fix assumption 2 by taking natural log of price
model_fix_as2=lm(log(price)~brand+wheelchase + carlength +carwidth+carheight+curbweight+enginesize+boreratio+stroke+peakrpm+citympg+highwaympg+aspiration+cyl,data=df_reg)
hist(model_fix_as2$residuals)
shapiro.test((resid(model_fix_as2)))
plot(model_fix_as2, 2)

# Remove outlier in wheelbase
Q1 <- quantile(df_reg$wheelchase, .25)
Q3 <- quantile(df_reg$wheelchase, .75)
IQR <- IQR(df_reg$wheelchase)
no_outliers1 <- subset(df_reg, df_reg$wheelchase> (Q1 - 1.5*IQR) & df_reg$wheelchase< (Q3 + 1.5*IQR))
dim(no_outliers1)

# Remove outlier in carlength
Q1 <- quantile(df_reg$carlength, .25)
Q3 <- quantile(df_reg$carlength, .75)
IQR <- IQR(df_reg$carlength)
no_outliers2 <- subset(df_reg, df_reg$carlength> (Q1 - 1.5*IQR) & df_reg$carlength< (Q3 + 1.5*IQR))
dim(no_outliers2)

# Remove outlier in stroke
Q1 <- quantile(df_reg$stroke, .25)
Q3 <- quantile(df_reg$stroke, .75)
IQR <- IQR(df_reg$stroke)
no_outliers3 <- subset(df_reg, df_reg$stroke> (Q1 - 1.5*IQR) & df_reg$stroke< (Q3 + 1.5*IQR))
dim(no_outliers3)

# Remove outlier in carwidth
Q1 <- quantile(df_reg$carwidth, .25)
Q3 <- quantile(df_reg$carwidth, .75)
IQR <- IQR(df_reg$carwidth)
no_outliers4 <- subset(df_reg, df_reg$carwidth> (Q1 - 1.5*IQR) & df_reg$carwidth< (Q3 + 1.5*IQR))
dim(no_outliers4)

# Remove outlier in carheight
Q1 <- quantile(df_reg$carheight, .25)
Q3 <- quantile(df_reg$carheight, .75)
IQR <- IQR(df_reg$carheight)
no_outliers5 <- subset(df_reg, df_reg$carheight> (Q1 - 1.5*IQR) & df_reg$carheight< (Q3 + 1.5*IQR))
dim(no_outliers5)

# Remove outlier in curbweight
Q1 <- quantile(df_reg$curbweight, .25)
Q3 <- quantile(df_reg$curbweight, .75)
IQR <- IQR(df_reg$curbweight)
no_outliers6 <- subset(df_reg, df_reg$curbweight> (Q1 - 1.5*IQR) & df_reg$curbweight< (Q3 + 1.5*IQR))
dim(no_outliers6)

# Remove outlier in enginesize
Q1 <- quantile(df_reg$enginesize, .25)
Q3 <- quantile(df_reg$enginesize, .75)
IQR <- IQR(df_reg$enginesize)
no_outliers7 <- subset(df_reg, df_reg$enginesize> (Q1 - 1.5*IQR) & df_reg$enginesize< (Q3 + 1.5*IQR))
dim(no_outliers7)

# Remove outlier in boreratio
Q1 <- quantile(df_reg$boreratio, .25)
Q3 <- quantile(df_reg$boreratio, .75)
IQR <- IQR(df_reg$boreratio)
no_outliers8<- subset(df_reg, df_reg$boreratio> (Q1 - 1.5*IQR) & df_reg$boreratio< (Q3 + 1.5*IQR))
dim(no_outliers8)

# Remove outlier in compressionratio
Q1 <- quantile(df_reg$compressionratio, .25)
Q3 <- quantile(df_reg$compressionratio, .75)
IQR <- IQR(df_reg$compressionratio)
no_outliers9 <- subset(df_reg, df_reg$compressionratio> (Q1 - 1.5*IQR) & df_reg$compressionratio< (Q3 + 1.5*IQR))
dim(no_outliers9)

# Assumption 3 - Test for heteroskedasticity
plot(model1, 3)
# ->Scale-location plot / spread-location plot  ‘We want a more or less horizontal line with more or less equally spread points around.’
# does deviate slightly from being horizontal. There is no clear sign of heteroscedasticity (no clear deviation from horizontal line, no funnel shape of errors being larger where fitted values get larger).
lmtest::bptest(model1)

# Fix assumption 3 by taking natural log of price
model_fix_as3=lm(log(price)~brand+wheelchase + carlength +carwidth+carheight+curbweight+enginesize+boreratio+stroke+peakrpm+citympg+highwaympg+aspiration+cyl,data=df_reg)
plot(model_fix_as3, 3)
lmtest::bptest(model_fix_as3)

# Assumption 4 - Test for multicollinearity
library(car)
vif (model1)

model_final1=lm(price~brand+wheelchase + carlength +carwidth+carheight+curbweight+enginesize+boreratio+stroke+peakrpm+citympg+highwaympg+aspiration+cyl,data=df_reg)
summary(model_final1)

#INTERACTION MODELS
#1st interaction model
model_interact1 <- lm(price~brand+wheelchase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+peakrpm+citympg+highwaympg+aspiration+cyl+carwidth*curbweight*enginesize, data=df_reg)

#2nd interaction model
model_interact2 <- lm(price~brand+wheelchase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+peakrpm+citympg+highwaympg+aspiration+cyl+carwidth*curbweight, data=df_reg)

#3rd interaction model
model_interact3 <- lm(price~brand+wheelchase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+peakrpm+citympg+highwaympg+aspiration+cyl+carlength*carheight*enginesize, data=df_reg)

#ANOVA
anova_result1 <- anova(model_final1, model_interact1)
anova_result2 <- anova(model_final1, model_interact2)
anova_result3 <- anova(model_final1, model_interact3)

# Print ANOVA results
cat("\nANOVA results for Model 1:\n")
print(anova_result1)

cat("\nANOVA results for Model 2:\n")
print(anova_result2)

cat("\nANOVA results for Model 3:\n")
print(anova_result3)

# Compare p-values and make a decision
alpha <- 0.05

if (anova_result1$`Pr(>F)`[2] < alpha) {
  cat("\nReject null hypothesis for Model 1 - Interaction terms improve the fit.\n")
} else {
  cat("\nFail to reject null hypothesis for Model 1 - Interaction terms do not significantly improve the fit.\n")
}

if (anova_result2$`Pr(>F)`[2] < alpha) {
  cat("\nReject null hypothesis for Model 2 - Interaction terms improve the fit.\n")
} else {
  cat("\nFail to reject null hypothesis for Model 2 - Interaction terms do not significantly improve the fit.\n")
}

if (anova_result3$`Pr(>F)`[2] < alpha) {
  cat("\nReject null hypothesis for Model 3 - Interaction terms improve the fit.\n")
} else {
  cat("\nFail to reject null hypothesis for Model 3 - Interaction terms do not significantly improve the fit.\n")
}

#MODEL SELECTION
#Stepwise Search
#stepwise search using AIC
#Start with price ~ 1 and search up to full model
car_mod_start = lm(price~ 1, data = df_reg)
car_mod_both_aic = step(
  car_mod_start,
  scope = price~ brand+wheelchase + carlength +carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+peakrpm+citympg+highwaympg+aspiration+cyl ,
  direction = "both"
)
#adjust R Squared and LOOCV RMSE
#compare the adjusted R Squared
summary(model_final1)$adj.r.squared
summary(car_mod_both_aic)$adj.r.squared