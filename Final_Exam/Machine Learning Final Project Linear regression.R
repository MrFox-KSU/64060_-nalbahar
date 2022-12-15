###########################################################################
## 1. Initialize and import data
###########################################################################
set.seed(1)
options(scipen = 9999)
library(MASS)
df <- read.csv("/Users/nawwaf/Desktop/Kent/Kent Master_s/Machine Learning/data.csv")

Cancer3$diagnosis = as.factor(Cancer3$diagnosis) ##automatic
head(df)

df <- df[,-33]

head(df)

df <- df[,-1]
#df <- na.omit(df) # Remove NA (missing) values

summary(df)



###########################################################################
## 2. Split the data in training and evaluating set (70%-30%)
###########################################################################

s = sample(length(df$diagnosis), floor(length(df$diagnosis)*0.7))
s
train = df[s,]
eval = df[-s,]

###########################################################################
## 3. Run a standard linear regression model on the radius_mean of tumor (radius_mean)
###########################################################################
reg = lm(train$radius_mean~.,data=train)
reg
summary(reg)

## Task 5. predictions and test the quality of the model

predict = predict(reg, eval)
predict
me = mean(eval$radius_mean-predict)
me
rmse = sqrt(mean((eval$radius_mean-predict)**2))
rmse
mape = mean(abs(eval$radius_mean-predict)/eval$radius_mean)*100
mape

###########################################################################
## 4. Investigate reducing complexity by dropping variables
###########################################################################

drop1(reg)

reg1 = lm(radius_mean~.-texture_mean, data=train)
drop1(reg1)

reg2 = lm(radius_mean~.-texture_mean-concave.points_worst, data=train)
drop1(reg2)

reg3 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se, data=train)
drop1(reg3)

reg4 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se, data=train)
drop1(reg4)

reg5 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst, data=train)
drop1(reg5)

reg6 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst-concavity_worst, data=train)
drop1(reg6)

reg7 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst-concavity_worst-concave.points_mean, data=train)
drop1(reg7)

reg8 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst-concavity_worst-concave.points_mean-diagnosis, data=train)
drop1(reg8)

reg9 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst-concavity_worst-concave.points_mean-diagnosis-fractal_dimension_se, data=train)
drop1(reg9)

reg10 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst-concavity_worst-concave.points_mean-diagnosis-fractal_dimension_se-compactness_se, data=train)
drop1(reg10)

reg11 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst-concavity_worst-concave.points_mean-diagnosis-fractal_dimension_se-compactness_se-fractal_dimension_worst, data=train)
drop1(reg11)


reg12 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst-concavity_worst-concave.points_mean-diagnosis-fractal_dimension_se-compactness_se-fractal_dimension_worst-fractal_dimension_mean, data=train)
drop1(reg12)


reg13 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst-concavity_worst-concave.points_mean-diagnosis-fractal_dimension_se-compactness_se-fractal_dimension_worst-fractal_dimension_mean-symmetry_mean, data=train)
drop1(reg13)
## with reg13 me, rmse and mape were slightly reduced compared to the original dataset

###########################################################################
## 5. testing the quality of both regression models (The orignal and the latest one)
###########################################################################

predict = predict(reg13, eval)
me = mean(eval$radius_mean-predict)
me
rmse = sqrt(mean((eval$radius_mean-predict)**2))
rmse
mape = mean(abs(eval$radius_mean-predict)/eval$radius_mean)*100
mape

predictCV = length(df)

###########################################################################
## 6. Perform cross validation on both models
###########################################################################

##cross-validation (full) - leave-one-out-method

for (k in 1:length(df$radius_mean)) {
  eval = df[k,]
  train = df[-k,]
  # here we use the leave-one-out-method on the whole dataFrame
  m1 = lm(radius_mean~.,data=train)
  predictCV[k] = predict(m1,eval)
}

me = mean(df$radius_mean-predictCV)
me
rmse = sqrt(mean((df$radius_mean-predictCV)**2))
rmse
mape = mean(abs(df$radius_mean-predictCV)/df$radius_mean)*100
mape

##cross-validation(reduced)
for (k in 1:length(df$radius_mean)) {
  eval = df[k,]
  train = df[-k,]
# here we exclude the columns we identified as not necessary
  m1 = lm(radius_mean~.-texture_mean-concave.points_worst-area_se-smoothness_se-symmetry_worst-concavity_worst-concave.points_mean-diagnosis-fractal_dimension_se-compactness_se-fractal_dimension_worst-fractal_dimension_mean-symmetry_mean, data=train)
  predictCV[k] = predict(m1,eval)
}

me = mean(df$radius_mean-predictCV)
me
rmse = sqrt(mean((df$radius_mean-predictCV)**2))
rmse
mape = mean(abs(df$radius_mean-predictCV)/df$radius_mean)*100
mape

