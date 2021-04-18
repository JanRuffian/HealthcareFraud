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
                             sum.ChronicCond_stroke, sum.ChronicCond_ObstrPulmonary, sum.States, 
                             sum.Diagnosis, summean_perc, starts_with("Majority")) %>% select(-"Majority_1")

controls2 <- test %>% select(sum.BeneID, sum.InscClaimAmtReimbursed, mean.Age,
                             OupatientInpatient2Ratio,
                             sum.AttendingPhysicians, GenderRatio, sum.Claims, 
                             sum.ChronicCond_Alzheimer, sum.ChronicCond_Cancer, 
                             sum.ChronicCond_Heartfailure, sum.ChronicCond_Diabetes, 
                             sum.ChronicCond_Osteoporasis, sum.ChronicCond_rheumatoidarthritis,
                             sum.ChronicCond_stroke, sum.ChronicCond_ObstrPulmonary, sum.States, 
                             sum.Diagnosis, summean_perc, starts_with("Majority")) %>% select(-"Majority_1")

y_train <- as.factor(train$PotentialFraud2)
x_train <- data.matrix(controls)
y_test <- as.factor(test$PotentialFraud2)
x_test <- data.matrix(controls2)

### Lasso Regression
#Values of lambda over which to check.
grid = 10^seq(5, -2, length = 100)

# Normalize train data
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

x_train <- as.matrix(as.data.frame(x_train) %>% select(-"Majority"))
x_train_normalized <- normalize(x_train)

#Run lasso regression with cross validation
set.seed(0)
cv.lasso = cv.glmnet(x_train_normalized, as.factor(y_train), lambda = grid, alpha = 1,  family = "binomial", nfolds = 10)

#Best Lambda
plot(cv.lasso, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

#Fitting model with best lambda
cv.lasso.bestfit = predict(cv.lasso, s = bestlambda.lasso, newx = x_train, type = "class")
table(truth = y_train, prediction = cv.lasso.bestfit)
(3102+347)/4328

predict(cv.lasso, s = bestlambda.lasso, type = "coefficients")
#Intercept
#sum.InscClaimAmtReimbursed
#OupatientInpatient2Ratio
#sum.Diagnosis
#Majority_49

#Fitting the lasso regression. Alpha = 1 for lasso regression.
lasso.models = glmnet(x_train_normalized, y_train, alpha = 1,   family = "binomial",lambda = grid)

#Visualizing the lasso regression shrinkage.
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

#Different choice of lambda 
grid2=c(0.00001, 0.001)
cv.lasso2 = cv.glmnet(x_train_normalized, as.factor(y_train), lambda = grid2, alpha = 1,  family = "binomial", nfolds = 10)
cv.lasso.bestfit2 = predict(cv.lasso2, s = 0.001, newx = x_train, type = "class")
predict(cv.lasso2, s = 0.001, type = "coefficients")
#Intercept
#sum.BeneID
#sum.InscClaimAmtReimbursed 
#mean.Age
#OupatientInpatient2Ratio
#sum.AttendingPhysicians
#sum.Diagnosis 
#summean_perc
#Majority
