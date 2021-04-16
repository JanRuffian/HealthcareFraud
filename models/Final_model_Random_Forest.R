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

######################################
###### random forest #################
######################################

train$PotentialFraud2 <- as.factor(train$PotentialFraud2)

set.seed(0)
rf_classifier <- randomForest(PotentialFraud2~sum.BeneID+sum.InscClaimAmtReimbursed+mean.Age+
                                OupatientInpatient2Ratio+sum.AttendingPhysicians+sum.AttendingPhysicians+
                                GenderRatio+sum.Claims+sum.ChronicCond_Alzheimer+sum.ChronicCond_Cancer+
                                sum.ChronicCond_Heartfailure+sum.ChronicCond_Diabetes+sum.ChronicCond_Osteoporasis+
                                sum.ChronicCond_rheumatoidarthritis+sum.ChronicCond_stroke+
                                sum.ChronicCond_ObstrPulmonary+as.factor(train$Majority), data=train, ntree=100, importance=TRUE)

table(train$PotentialFraud2, predict(rf_classifier))
(3812+196)/4328

#Importance of the variables
importance(rf_classifier)
varImpPlot(rf_classifier, 
           sort=T,
           main = "Variable Importance")

### Error Rate
plot(rf_classifier)

#Tune mtry
set.seed(0)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=c(1:5))
rf_gridsearch <- train(PotentialFraud2~sum.BeneID+sum.InscClaimAmtReimbursed+mean.Age+
                         OupatientInpatient2Ratio+sum.AttendingPhysicians+sum.AttendingPhysicians+
                         GenderRatio+sum.Claims+sum.ChronicCond_Alzheimer+sum.ChronicCond_Cancer+
                         sum.ChronicCond_Heartfailure+sum.ChronicCond_Diabetes+sum.ChronicCond_Osteoporasis+
                         sum.ChronicCond_rheumatoidarthritis+sum.ChronicCond_stroke+
                         sum.ChronicCond_ObstrPulmonary+as.factor(Majority), data=train,
                       method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
#optimal mtry is 3

#No. of nodes for the trees
hist(treesize(rf_classifier),
     main ="No. of Nodes for the Trees",
     col = "green")




# Cross validation
library(caret)
set.seed(0)

ctrl <- trainControl(method="repeatedcv", 
                     number=10, 
                     repeats=10)

rf2 <- train(PotentialFraud2~sum.BeneID+sum.InscClaimAmtReimbursed+mean.Age+
               OupatientInpatient2Ratio+sum.AttendingPhysicians+sum.AttendingPhysicians+
               GenderRatio+sum.Claims+sum.ChronicCond_Alzheimer+sum.ChronicCond_Cancer+
               sum.ChronicCond_Heartfailure+sum.ChronicCond_Diabetes+sum.ChronicCond_Osteoporasis+
               sum.ChronicCond_rheumatoidarthritis+sum.ChronicCond_stroke+
               sum.ChronicCond_ObstrPulmonary, data=train,
               method="rf", 
               ntree = 500,
               metric=metric, 
               trControl = ctrl)
# mtry = 2

best.rf <- rf2$results
rfTune.predictions = round(predict(best.rf,
                                      newdata = as.data.frame(x_train),
                                      n.trees = 500,
                                      type = "response"),0)
table(truth = y_train, prediction = rfTune.predictions)
(3907+151)/nrow(x_train)

rfTune.predictions.test = round(predict(boostTune,
                                        newdata = as.data.frame(x_test),
                                        n.trees = 500,
                                        type = "response"),0)
table(truth = y_test, prediction = boostTune.predictions.test)
(965+41)/nrow(x_test)