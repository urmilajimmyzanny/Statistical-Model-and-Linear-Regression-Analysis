##########################################################################################
#################################### Assignment ##########################################
################################# Name: Urmila Dhundhwal(M20MA207) ########################
###########################################################################################

######################################### Task:3 ##########################################


# Install ISLR2 package from CRAN 
install.packages("ISLR2")

# Load ISLR package
library(ISLR2)

# In our task given that we have to use Bostan data set
# View Bostan data set
View(Boston)

Boston_Dataset <- Boston
View(Boston_Dataset)

# Structure of the data:
str(Boston_Dataset)

# Column names of the Boston data:
names(Boston_Dataset)

# Check class of the Boston data:
class(Boston_Dataset)

# Check the shape of the data
shape <- dim(Boston_Dataset)

print(shape)

# Display first 6 rows of the data:
head(Boston_Dataset)

# Display last 6 rows of the data:
tail(Boston_Dataset)

# Remove rows with missing values
Boston_Dataset <- na.omit(Boston_Dataset)

print(dim(Boston_Dataset))

# Given that we have to use per capita crime rate is the
# response, and the predictors are as follows: zn, indus, nox, rm, dis, tax,
# medv from the Boston data set.

# Make data frame which contain only response variable and these predictors.
v <- c(1, 2, 3, 5, 6, 8, 10, 13)
Data_set <- Boston_Dataset[, v]

View(Data_set)

head(Data_set)

tail(Data_set)

# Summary of the data set
summary(Data_set)

# (a) Fit simple linear regression models for each predictor

# Fit simple linear regression model for response variable crime rate and predictor zn-

pairs(~ crim+zn, Data_set)

model1.fit <- lm(crim ~ zn , Data_set)
summary(model1.fit)

# The fitted regression model1 eq will be- crime rate = b1 + m1 * zn 
# here b1 = 4.45369 and m1 = -0.07393
#-------------------------------------------------------------------------------------------

# Fit simple linear regression model for response variable crime rate and predictor indus-

pairs(~ crim+indus, Data_set)

model2.fit <- lm(crim ~ indus , Data_set)
summary(model2.fit)

# The fitted regression model2 eq will be- crime rate = b2 + m2 * indus 
# here b2 = -2.06374 and m2 = 0.50978    
#------------------------------------------------------------------------------------------

# Fit simple linear regression model for response variable crime rate and predictor nox-

pairs(~ crim+nox, Data_set)

model3.fit <- lm(crim ~ zn , Data_set)
summary(model3.fit)

# The fitted regression model3 eq will be- crime rate = b3 + m3 * nox
# here b3 = 4.45369 and m3 = -0.07393
#-------------------------------------------------------------------------------------------

# Fit simple linear regression model for response variable crime rate and predictor rm-

pairs(~ crim+rm, Data_set)

model4.fit <- lm(crim ~ rm , Data_set)
summary(model4.fit)

# The fitted regression model4 eq will be- crime rate = b4 + m4 * rm 
# here b4 = 20.482 and m4 = -2.684
#------------------------------------------------------------------------------------------

# Fit simple linear regression model for response variable crime rate and predictor dis-

pairs(~ crim+dis, Data_set)

model5.fit <- lm(crim ~ dis , Data_set)
summary(model5.fit)

# The fitted regression model5 eq will be- crime rate = b5 + m5 * dis 
# here b5 = 9.4993 and m5 = -1.5509
#------------------------------------------------------------------------------------------

# Fit simple linear regression model for response variable crime rate and predictor tax-

pairs(~ crim+tax, Data_set)

model6.fit <- lm(crim ~ tax , Data_set)
summary(model6.fit)

# The fitted regression model6 eq will be- crime rate = b6 + m6 * tax 
# here b6 = -8.528369 and m6 = 0.029742
#------------------------------------------------------------------------------------------

# Fit simple linear regression model for response variable crime rate and predictor medv-

pairs(~ crim+medv, Data_set)

model7.fit <- lm(crim ~ medv , Data_set)
summary(model7.fit)

# The fitted regression model7 eq will be- crime rate = b7 + m7 * medv 
# here b7 = 11.79654 and m7 = -0.36316
#------------------------------------------------------------------------------------------

# To determine whether there is a statistically significant association between a predictor
# variable and a response variable in models we will perform hypothesis testing.
# For testing the association between a predictor and a response, the null hypothesis 
# usually states that there is no association (the coefficient for the predictor is zero), 
# while the alternative hypothesis states that there is a nonzero association.
# Null Hypothesis (H0): There is no association between the predictor and the response.
# Alternative Hypothesis (H1): There is an association between the predictor and the response.

# Use significance level α = 0.05 
######### For model1 ##########
# Testing Hypothisis: For b1 (intercept)
# H01 : b1 = 0 vs. H11 : b1 != 0
# p − value < 2 × 10^−16
# i.e., p = value < α and hence H01 is rejected.

# Testing Hypothisis: For m1 (slop)
# H01 : m1 = 0 vs. H11 : m1 != 0
# p−value < 5.51 × 10^−6
# i.e., p-value < α and hence H01 is rejected.

######### For model2 ##########
# Testing Hypothisis: For b2 (intercept)
# H02 : b2 = 0 vs. H12 : b2 != 0
# p−value = 0.00209
# i.e., p-value < α and hence H02 is rejected.

# Testing Hypothisis: For m2 (slop)
# H02 : m2 = 0 vs. H12 : m2 != 0
# p−value < 2 × 10^−16
# i.e., p-value < α and hence H02 is rejected.

######### For model3 ##########
# Testing Hypothisis: For b3 (intercept)
# H03 : b3 = 0 vs. H12 : b3 != 0
# p−value < 2 × 10^−16
# i.e., p-value < α and hence H03 is rejected.

# Testing Hypothisis: For m3 (slop)
# H03 : m3 = 0 vs. H13 : m3 != 0
# p−value = 5.51 x 10^-6
# i.e., p-value < α and hence H03 is rejected.

######### For model4 ##########
# Testing Hypothisis: For b4 (intercept)
# H04 : b4 = 0 vs. H14 : b3 != 0
# p−value < 2.27 × 10^−9
# i.e., p-value < α and hence H04 is rejected.

# Testing Hypothisis: For m4 (slop)
# H04 : m4 = 0 vs. H14 : m4 != 0
# p−value = 6.35 x 10^-7
# i.e., p-value < α and hence H04 is rejected.

######### For model5 ##########
# Testing Hypothisis: For b5 (intercept)
# H05 : b5 = 0 vs. H15 : b5 != 0
# p−value < 2 × 10^−16
# i.e., p-value < α and hence H05 is rejected.

# Testing Hypothisis: For m5 (slop)
# H05 : m5 = 0 vs. H15 : m5 != 0
# p−value = 2 x 10^-16
# i.e., p-value < α and hence H05 is rejected.

######### For model6 ##########
# Testing Hypothisis: For b6 (intercept)
# H06 : b6 = 0 vs. H16 : b6 != 0
# p−value < 2 × 10^−16
# i.e., p-value < α and hence H06 is rejected.

# Testing Hypothisis: For m6 (slop)
# H06 : m6 = 0 vs. H16 : m6 != 0
# p−value = 2 x 10^-16
# i.e., p-value < α and hence H06 is rejected.

######### For model7 ##########
# Testing Hypothisis: For b7 (intercept)
# H07 : b7 = 0 vs. H17 : b7 != 0
# p−value < 2 × 10^−16
# i.e., p-value < α and hence H07 is rejected.

# Testing Hypothisis: For m7 (slop)
# H07 : m7 = 0 vs. H14 : m7 != 0
# p−value = 2 x 10^-16
# i.e., p-value < α and hence H07 is rejected.

# We can see that in each model there is a statistically significant association 
# between the predictor and the response.
#-------------------------------------------------------------------------------

# Check godness of Fit of models-

# Model-1: RSE = 8.435, R2_adj = 0.03828 and R2 = 0.04019

# Model-2: RSE = 7.866, R2_adj = 0.1637 and R2 = 0.1653,

# Model-3: RSE = 8.435, R2_adj = 0.03828 and R2 = 0.04019,

# Model-4: RSE = 8.401, R2_adj = 0.04618 and R2 = 0.04807

# Model-5: RSE = 7.965, R2_adj = 0.1425 and R2 = 0.1441

# Model-6: RSE = 6.997, R2_adj = 0.3383 and R2 = 0.3396 

# Model-7: RSE = 7.934, R2_adj = 0.1491  and R2 = 0.1508

# So, Model-6 is the best among these seven models.

#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

# (b) Fit a multiple regression model to predict the response using all of the predictors.
# The fitted regression model eq will be crim =  β0 + β1*zn + β2*indus + β3*nox + β4*rm + 
#                                                   β5*dis + β6*tax + β7*medv

pairs(~ crim+zn+indus+nox+rm+dis+tax+medv, Data_set)

# Calculate the covariance matrix
correlation_matrix <- cor(Data_set)

# Print the covariance matrix
print(correlation_matrix)

multi_model <- lm(crim ~ zn+indus+nox+rm+dis+tax+medv, Data_set)
summary(multi_model) 


# R2 = 0.3845 (say), then it indicates the model explained 38% of the variation that is 
# there in the response. Thus, the fitted regression model is given by
# crim = 2.417 + 0.039*zn - 0.226798*indus − 3.107230*nox + 0.596972*rm - 1.138369*dis 
#          + 0.026635*tax - 0.235403*medv

# Note that p-value< 2.2 × 10−16 is very small and hence, we should
# reject the overall hypothesis H0 : β0 = β1 = β2 = β3 = β4 = β5 = β6

# We have F0 = 44.45 i.e. model is not too good.
#------------------------------------------------------------------------------------------

# Test hypothesis on Individual Regression Coefficients: 
# Level-α-test with α = 0.05 

## Testing Problem 1 (For intercept): H01 : β0 = 0 vs. H11 : β0 != 0 
# p − value = 0.60997
# i.e., p-value > α and hence H01 is accepted.

## Testing Problem 2 (For coefficient of zn): H02 : β1 = 0 vs. H12 : β1 != 0
# p − value = 0.03517
# i.e., p-value < α and hence H02 is rejected.

## Testing Problem 3 (For coefficient of indus): H03 : β2 = 0 vs. H13 : β2 != 0 
# p − value = 0.00678
# i.e., p-value < α and hence H03 is rejected.

## Testing Problem 4 (For coefficient of nox): H04 : β3 = 0 vs. H14 : β3 != 0
# p − value = 0.52922 
# i.e., p-value > α and hence H04 is accepted.

## Testing Problem 5 (For coefficient of rm): H05 : β4 = 0 vs. H15 : β4 != 0 
# p − value = 0.32725
# i.e., p-value > α and hence H05 is accepted.

## Testing Problem 6 (For coefficient of dis): H06 : β5 = 0 vs. H16 : β5 != 0
# p − value = 4.99 X 10^-5
# i.e., p-value < α and hence H06 is rejected.

## Testing Problem 7 (For coefficient of tax): H07 : β6 = 0 vs. H17 : β6 != 0
# p − value < 2e X 10^-16
# i.e., p-value < α and hence H07 is rejected.

## Testing Problem 8 (For coefficient of medv): H08 : β7 = 0 vs. H18 : β7 != 0
# p − value = 6.82 X 10^-6 
# i.e., p-value < α and hence H08 is rejected.

# Hence, for zn, indus, dis, tax ans medv predictors we can reject the null hypothesis.

#------------------------------------------------------------------------------------------

# Standarized and Studentized Residuals:

rstandard(multi_model)

rstudent(multi_model)

# Plot 
par(mfrow = c(2,2))
plot(rstandard(multi_model), ylab = "Standarized Resisuals")
plot(rstudent(multi_model), ylab = "Studentized Resisuals")

# Check outlier is present in our data or not:

# Detection of Leverage Points:
h <- hatvalues(multi_model)
h
k <- 7
n <- 506
t <- 2*k/n
for( i in 1:n){
  if(h[i]>t){
    print(h[i])
  }
}

Leverage_point <- which(h > t)
Leverage_point_values <- h[Leverage_point]

## Detection of Influential Points: Cook’s Distance ##
cook <- cooks.distance(multi_model)
cook 
plot(cook, type = 'b', pch = 18, col = 'red')
cutoff <- k/(n-k-1)
cutoff
abline(h = cutoff, lty = 3)
for( i in 1:506){
  if(cook[i]>cutoff){
    print(cook[i])
  }
}
Influential_Points <- which(cook > cutoff)
Influential_Points_values <- cook[Influential_Points]
Influential_Points_values

## Detection of Influential Points: DFBETAS and DFFITS##
def <- dfbetas(multi_model)
abs(def) 

cutoff_ <- 2*sqrt(1/n)
cutoff_

for( i in 1:n){
  if(abs(def)[i]>cutoff_){
    print(abs(def)[i])
  }
}

DFBETAS_Points <- which(abs(def) > cutoff_)
DFBETAS_Points_values <- abs(def)[DFBETAS_Points]
DFBETAS_Points_values

plot(abs(def), type = 'b', pch = 18, col = 'red')

dff <- dffits(multi_model)
abs(dff)

plot(abs(dff), type = 'b', pch = 18, col = 'red')

cutoff <- 2*sqrt(k/n)
cutoff

abline(h = cutoff, lty = 3)
for( i in 1:n){
  if(abs(dff)[i]>cutoff){
    print(abs(dff)[i])
  }
}

DFFITS_Points <- which(abs(dff) > cutoff)
DFFITS_Points_values <- abs(dff)[DFFITS_Points]
DFFITS_Points_values

##### A measure of model performance: COVRATIO #########

cv <- covratio(multi_model)
cv

u <- 1+3*k/n
l <- 1-3*k/n

for( i in 1:n){
  if( cv[i] < l | cv[i] > u ){
    print(cv[i])
  }
}
outliers <- which(cv < l | cv > u)
outlier_values <- cv[outliers]
outlier_values

# data <- Data_set[-outliers,]
# dim(data)
# model <- lm(crim ~ zn+indus+nox+rm+dis+tax+medv, data)
# summary(model) 
# cv



###################################Assumption##############################################

# (i)The error term has zero mean
error <- multi_model$residuals
mean(error)

# (ii) The error term has constant variance-
residuals_e <- multi_model$residuals
fitted_vale_ycap <- multi_model$fitted.values
# By above graph we can see that error term has not constant variance.

plot( fitted_vale_ycap, residuals_e, xlab = "fitted values", ylab = " residuals")
abline(h=0)

# (iii) The errors are normally distributed-
# Normality check by using Q-Q plot for studentized residuals
qqnorm(rstudent(multi_model))
qqline(rstudent(multi_model), col = 'red')
# By graph we can see that distribution of the studentized residual positive skew.
# It indicates that the residuals are not normally distributed.

# (iv) Check the Multicollinearity-
install.packages("mctest")
library(mctest)
imcdiag(multi_model, method = "VIF")

# Extracting the model matrix
X <- model.matrix(multi_model)

# Computing the variance-covariance matrix
cov_matrix <- cov(X)
cov_matrix
# Calculating the eigenvalues
eigenvalues <- eigen(cov_matrix)$values

eigenvalues
# Checking the eigenvalues
summary(eigenvalues)

# The minimum eigenvalue is approximately 0, it is indicating that one or more 
# variables are linear combinations of other variables, suggesting multicollinearity.

# (v) Check Uncorreleated error assumption:

install.packages("DescTools")

library(DescTools)

DurbinWatsonTest(multi_model, alternative = "less")

DurbinWatsonTest(multi_model, alternative = "greater")

DurbinWatsonTest(multi_model, alternative = "two.sided")

# the Durbin-Watson test statistic (DW) is approximately 1.3535, and the p-value is very 
# small (close to zero) for all three alternative hypotheses:
# Alternative = "less": The p-value is 1. This indicates that there is no evidence to 
# suggest that the autocorrelation is less than 0 (i.e., negative autocorrelation).
# Alternative = "greater": The p-value is approximately 2.531 x 10^-14. 
# This indicates strong evidence to suggest that the autocorrelation is greater than 0 
# (i.e., positive autocorrelation).

# Alternative = "two.sided": The p-value is also very small, approximately 5.061 x 10^-14.
# This indicates strong evidence to suggest that the autocorrelation is not equal to 0.

# Based on these results, we can conclude that there is strong evidence of positive 
# autocorrelation in the errors of the regression model. This means that consecutive 
# residuals are correlated with each other, violating the assumption of independence of 
# errors.

# (vi) Check for linearity assumption:
install.packages("car")
library(car)
avPlots(multi_model)
# By plot we can see that the relationship btw response variable and predictor variable are 
# linear so linearity assumption hold.

# Fix non-normality and non-constant variance problem be transformation of response
# variable: Use Box-Cox method
install.packages("Car")
library(car)

p <- powerTransform(crim ~ zn+indus+nox+rm+dis+tax+medv, family = "bcPower",Data_set)
summary(p)

# The estimated transformation parameter is λ = −0.0405

# Updated model:
new_MLR_model <- lm(bcPower(crim, p$roundlam, jacobian.adjusted = TRUE)~ zn+indus+nox+rm+dis+tax+medv, Data_set)
summary(new_MLR_model)

# R2 = 0.8062 (say), then it indicates the model explained 81% of the variation that is 
# there in the response. Thus, the fitted regression model is given by
# crim = -2.9110237 + -0.0074602*zn - 0.0063652*indus + 2.6853020*nox - 0.0017209*rm - 
#                  0.0290243*dis  + 0.0031805*tax + 0.0002168*medv
# Note that p-value < 2.2 × 10−16 is very small and hence, we should reject the 
# overall hypothesis H0 : β0 = β1 = β2 = β3 = β4 = β5 = β6=0



############################################################################################
######################################### Task:2 ##########################################
###########################################################################################

# (a) create a vector, x
x <- rnorm(100, mean = 0, sd = 1)

#------------------------------------------------------------------------------------------

# (b) create a vector, eps
eps <- rnorm(100, mean = 0, sd = sqrt(0.25))

#-----------------------------------------------------------------------------------------

# (c) Using x and eps, generate a vector y according to the model
y <- -1 + 0.5*x + eps

cat("Length of the y is", length(y))

# In this linear model, β0 = -1 and β1 = 0.5

#------------------------------------------------------------------------------------------

# (d) Create a scatterplot displaying the relationship between x and y.
plot(x, y, main = "Scatter plot btw x and y", col = "green")
# install.packages("ggplot2")
# library(ggplot2)
# d <- data.frame(x, y)
# ggplot(d,aes(x=x, y=y))+geom_point()
# I found positive correlation between x and y. It means that as the value of one variable 
# increases, the value of the other variable also tends to increase.

#------------------------------------------------------------------------------------------

# (e) Fit a least squares linear model to predict y using x:
model.fit <- lm(y ~ x)
summary(model.fit)
# estimating coefficients β0_cap = -0.99770 and β1_cap = 0.48777 both are close to β0 and β1
# respectivey. It indicates that the linear regression model has effectively captured the 
# underlying relationship between the predictor variable X and the target variable Y.

# R2 = 0.50 (say), then it indicates the model explained 50%
# of the variation that is there in the response.
#Thus, the fitted regression model is given by
# y = -0.99770 + 0.48777*x

# RSE = 0.4873, R2_adj = 0.4967 and R2 = 0.5018
# Note that p-value< 2.2 × 10−16 is very small and hence, we should
# reject the overall hypothesis H0 : β0_cap = β1_cap = 0.

# Check model is statisticaly significate or not:
# Use significance level α = 0.05 
# Testing Hypothisis: For β0_cap (intercept)
# H01 : β0_cap = 0 vs. H11 : β0_cap != 0
# p − value < 2 × 10^−16
# i.e., p = value < α and hence H01 is rejected.

# Testing Hypothisis: For β1_cap (slop)
# H01 : β1_cap = 0 vs. H11 : β1_cap != 0
# p−value < 2 × 10^−16
# i.e., p-value < α and hence H01 is rejected.

# Hence model a statistically significant.

#-----------------------------------------------------------------------------------------

# (f) Display the least squares line and the population regression line on the scatterplot:

abline(model.fit, col = "red")  # Least squares line
abline(-1, 0.5, col = "blue")   # Population regression line
legend("bottom", legend = c("Least Squares Line", "Population Regression Line"), col = c("red", "blue"), lty = 1)
