options(scipen=999)
set.seed(1)
Cancer3 <- read.csv("/Users/nawwaf/Desktop/Kent/Kent Master_s/Machine Learning/data.csv")
hist(Cancer3$texture_mean)
hist(Cancer3$radius_mean)
hist(Cancer3$perimeter_mean)
hist(Cancer3$area_mean)
mean(Cancer3$concavity_worst)
summary(Cancer3)

#converting the categorical variable to factor 
Cancer3$diagnosis = as.factor(Cancer3$diagnosis) 

Cancer3Final = Cancer3[,c(-1,-13,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34)] #remove id and X
summary(Cancer3Final)
Cancer3Final$diagnosis<- as.numeric(Cancer3Final$diagnosis)
Cancer3Final$diagnosis[1:569] <- Cancer3Final$diagnosis[1:569] - 1
Cancer3Final$diagnosis

##split the data set into training and eval
s = sample(399,170)
eval= Cancer3Final[s,]  # eval with 60%
train= Cancer3Final[-s,] # train with 40%

## regression on train set
res = glm(diagnosis~.,family=binomial,data=train)

summary(res)

drop1(res)

reg1 = glm(diagnosis~.-compactness_mean, family = binomial, data=train)
drop1(reg1)

reg2 = glm(diagnosis~.-radius_mean-compactness_mean, family = binomial, data=train)
drop1(reg2)

reg3 = glm(diagnosis~.-radius_mean-compactness_mean-concavity_mean, family = binomial, data=train)
drop1(reg3)




eval$Prediction=predict(reg3,eval,type="response")
eval[1:20,]
hist(eval$Prediction)
plot(eval$diagnosis~eval$Prediction)

#get fasle positive and false negative
# Calculation of KPI's
eval$falsePositive = ifelse(eval$Prediction>=0.5 & eval$diagnosis == 0, 1, 0) 
eval$falseNegative = ifelse(eval$Prediction<0.5 & eval$diagnosis == 1, 1, 0)

eval$falsePositive
eval$falseNegative
# Where was the model wrong?

eval[1:20,] 

# Here we FILTER false positives and negatives from the data
falsePositive = eval[eval$falsePositive==1,]
falseNegative = eval[eval$falseNegative==1,]
falsePositive # This displays it on screen
falseNegative

error = (nrow(falseNegative) + nrow(falsePositive))/nrow(eval)
error
## Our model  got ~ 6.47% of the predictions wrong.


##  PLOT THE LIFT CURVE
eval[1:3,]
evalOrdered = eval[order(eval$Prediction, decreasing = TRUE),]
evalOrdered[1:3,]
# dummy variables
xaxis = NULL
cumMean = NULL # cumulative mean
cumLift = NULL # cumulative lift
meanResponse = mean(eval$diagnosis) # mean curve
meanResponse # Our model predicts the results better than just guessing by ~37% 

# initiate variables
xaxis[1] = 1
cumMean[1] = meanResponse 
cumLift[1] = evalOrdered$diagnosis[1] # We are most interested in the case where we assume we are right
# repeat for all rows
for (i in 2:nrow(evalOrdered)){
  xaxis[i] = i
  cumMean[i] = cumMean[i-1]+meanResponse
  cumLift[i] = cumLift[i-1]+evalOrdered$diagnosis[i]
}
# At this point we need to check the cumulative lift in the values section. In the beginning it goes up by 1, this is good sign.
plot(cumMean~xaxis) # plots the mean line
points(cumLift~xaxis) # plots the lift curve

## HERE WE PLOT THE ROC CURVE ##
library(ROCR)
prediction2 = prediction(eval$Prediction, eval$diagnosis)
roc = performance(prediction2, "sens", "fpr") 
plot(roc) #this checks for different cutt off points


# cross-validation (leave one out) with the full model
CV = length(Cancer3Final$diagnosis)
for (k in 1:569) {
  Cancer_evaluation_cv = Cancer3Final[k, ]
  Cancer_training_cv = Cancer3Final[-k, ]
  model_CV = glm(diagnosis~ ., data = Cancer_training_cv, family = binomial)
  CV[k] = predict(model_CV, Cancer_evaluation_cv, type="response")
}
Cancer3Final$CV_prediction = CV

# repeating previous steps
## false positive, false negatives, misdiagnosisification rate
false_positive_cv=Cancer3Final[Cancer3Final$CV_prediction >= 0.5 & Cancer3Final$diagnosis==0,]
false_positive_cv
nrow(false_positive_cv)
false_negative_cv=Cancer3Final[Cancer3Final$CV_prediction < 0.5 & Cancer3Final$diagnosis==1,]
false_negative_cv
nrow(false_negative_cv)
error_cv = (nrow(false_negative_cv) + nrow(false_positive_cv))/nrow(Cancer3Final)
error_cv
## lift curve
evalOrderedCV = Cancer3Final[order(Cancer3Final$CV_prediction, decreasing = TRUE),]
evalOrderedCV
#dummy variables
xaxis = NULL
cumMean = NULL
cumLift = NULL
meanResponse = mean(Cancer3Final$diagnosis)
meanResponse

#initiate variables
xaxis[1] = 1
cumMean[1] = meanResponse
cumLift[1] = evalOrderedCV$diagnosis[1]

#repeating for all rows
for (i in 2 : nrow(evalOrderedCV)) {
  xaxis[i] = i
  cumMean[i] = cumMean[i-1] + meanResponse
  cumLift[i] = cumLift[i-1] + evalOrderedCV$diagnosis[i]
}

plot(cumMean ~ xaxis)
points(cumLift ~ xaxis)





## ROC 
library(ROCR)
pred_cv = prediction(Cancer3Final$CV_prediction,Cancer3Final$diagnosis)
roc_cv = performance(pred_cv,"sens","fpr")
plot(roc_cv)

falsePositive = eval[eval$falsePositive==1,]
falseNegative = eval[eval$falseNegative==1,]
falsePositive # This displays it on screen
falseNegative

error = (nrow(falseNegative) + nrow(falsePositive))/nrow(eval)
error

