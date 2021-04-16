# Load Dataset
rm(list=ls())
HealthData <- read.csv('/Users/janruffner/Desktop/HealthcareFraudCapstoneProject/HealthData.csv')

# Creates dummies for states
HealthData <- dummy_cols(HealthData, select_columns = 'Majority')

# Create Train/Test data
set.seed(0)
trainIndex <- createDataPartition(HealthData$PotentialFraud2, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train <- HealthData[trainIndex,]
test  <- HealthData[-trainIndex,]

nrow(train)
nrow(test)
ncol(train)
ncol(test)

# Create controls for train and test
controls <- train %>% select(sum.BeneID, sum.InscClaimAmtReimbursed, mean.Age,
                             OupatientInpatient2Ratio,
                             sum.AttendingPhysicians, GenderRatio, sum.Claims, 
                             sum.ChronicCond_Alzheimer, sum.ChronicCond_Cancer, 
                             sum.ChronicCond_Heartfailure, sum.ChronicCond_Diabetes, 
                             sum.ChronicCond_Osteoporasis, sum.ChronicCond_rheumatoidarthritis,
                             sum.ChronicCond_stroke, sum.ChronicCond_ObstrPulmonary, starts_with("Majority")) %>% select(-"Majority_1")

controls2 <- test %>% select(sum.BeneID, sum.InscClaimAmtReimbursed, mean.Age,
                             OupatientInpatient2Ratio,
                             sum.AttendingPhysicians, GenderRatio, sum.Claims, 
                             sum.ChronicCond_Alzheimer, sum.ChronicCond_Cancer, 
                             sum.ChronicCond_Heartfailure, sum.ChronicCond_Diabetes, 
                             sum.ChronicCond_Osteoporasis, sum.ChronicCond_rheumatoidarthritis,
                             sum.ChronicCond_stroke, sum.ChronicCond_ObstrPulmonary, starts_with("Majority")) %>% select(-"Majority_1")

y_train <- as.factor(train$PotentialFraud2)
x_train <- data.matrix(controls)
y_test <- as.factor(test$PotentialFraud2)
x_test <- data.matrix(controls2)


### support vector machine 
x_train <- as.matrix(as.data.frame(x_train) %>% select(-starts_with("Majority")))
x_test <- as.matrix(as.data.frame(x_test) %>% select(-starts_with("Majority")))

library(e1071)
set.seed(0)
cv.svc.linear = tune(svm,
                     y_train ~ x_train,
                     kernel = "linear",
                     ranges = list(cost = 10^(seq(-5, -.5, length = 50))))

cv.svc.linear
plot(cv.svc.linear$performances$cost,
     cv.svc.linear$performances$error,
     xlab = "Cost",
     ylab = "Error Rate",
     type = "l")

best.svc.model <- cv.svc.linear$best.model
summary(best.svc.model)

y.pred.svc.train = predict(best.svc.model, x_train)
table("Predicted Values" = y.pred.svc.train, "True Values" = y_train)
(3901 + 150)/4328

y.pred.svc.test <- predict(best.svc.model, x_test, type="response")
table("Predicted Values" = y.pred.svc.test, "True Values" = y_test)

# Try radial kernel
# tune model to find optimal cost, gamma values
cv.svc.radial <- tune(svm,
                      y_train ~ x_train,
                      kernel = "radial",
                      ranges = list(cost = c(0.1,1,10,100,1000),
                      gamma = c(0.5,1,2,3,4)))
# show best model
best.svc.model.radial <- cv.svc.radial$best.model
summary(best.svc.model.radial)

y.pred.svc.train.radial = predict(best.svc.model.radial, x_train)
table("Predicted Values" = y.pred.svc.train.radial, "True Values" = y_train)
(3906 + 189)/4328

y.pred.svc.test.radial <- predict(best.svc.model.radial, x_test, type="response")
table("Predicted Values" = y.pred.svc.test.radial, "True Values" = y_test)