# Load Dataset
rm(list=ls())
HealthData <- read.csv('/Users/janruffner/Desktop/HealthcareFraudCapstoneProject/Data/HealthData.csv')

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
                             sum.ChronicCond_stroke, sum.ChronicCond_ObstrPulmonary, sum.Diagnosis,summean_perc, starts_with("Majority")) 

controls2 <- test %>% select(sum.BeneID, sum.InscClaimAmtReimbursed, mean.Age,
                             OupatientInpatient2Ratio,
                             sum.AttendingPhysicians, GenderRatio, sum.Claims, 
                             sum.ChronicCond_Alzheimer, sum.ChronicCond_Cancer, 
                             sum.ChronicCond_Heartfailure, sum.ChronicCond_Diabetes, 
                             sum.ChronicCond_Osteoporasis, sum.ChronicCond_rheumatoidarthritis,
                             sum.ChronicCond_stroke, sum.ChronicCond_ObstrPulmonary, sum.Diagnosis,summean_perc, starts_with("Majority")) 

y_train <- as.factor(train$PotentialFraud2)
x_train <- data.matrix(controls)
y_test <- as.factor(test$PotentialFraud2)
x_test <- data.matrix(controls2)

######################################
###### random forest #################
######################################

library(gbm)
set.seed(0)
boost <- gbm(PotentialFraud2~sum.BeneID+sum.InscClaimAmtReimbursed+mean.Age+
               OupatientInpatient2Ratio+sum.AttendingPhysicians+sum.AttendingPhysicians+
               sum.Diagnosis+summean_perc+as.factor(Majority),
               data=train, 
               distribution="bernoulli",
               n.trees = 1000,
               interaction.depth = 4,
               shrinkage = 0.001)

boost.predictions = round(predict(boost,
                            newdata = as.data.frame(x_train),
                            n.trees = 1000,
                            type = "response"),0)
table(truth = y_train, prediction = boost.predictions)
(3910+137)/nrow(x_train)

gbmGrid <- expand.grid(
          n.trees = seq(10, 1000, by = 100), 
          interaction.depth = c(4), 
          shrinkage = c(0.01, 0.1), 
          n.minobsinnode = c(5, 10, 20, 30)        
)

train_control <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 10
)

gbmTune <- train(as.factor(PotentialFraud2)~sum.BeneID+sum.InscClaimAmtReimbursed+mean.Age+
                   OupatientInpatient2Ratio+sum.AttendingPhysicians+sum.Diagnosis+summean_perc+as.factor(Majority),
                   data=train, 
                   method="gbm",
                   distribution="bernoulli",
                   tuneGrid = gbmGrid,
                   trControl = train_control)

plot(gbmTune)
ggplot(gbmTune)

gbmTune$bestTune
# trees: 110, interaction depth: 4, shrinkage: 0.01, n.minobsinnode: 5
gbmTune$results
set.seed(0)
boostTune <- gbm(PotentialFraud2~sum.BeneID+sum.InscClaimAmtReimbursed+mean.Age+
                   OupatientInpatient2Ratio+sum.AttendingPhysicians+sum.AttendingPhysicians+
                   sum.Diagnosis+summean_perc+as.factor(Majority), data=train, 
              distribution="bernoulli",
              n.trees = 110,
              interaction.depth = 4,
              shrinkage = 0.01,
              n.minobsinnode = 5)


boostTune.predictions = round(predict(boostTune,
                                      newdata = as.data.frame(x_train),
                                      n.trees = 110,
                                      type = "response"),0)
table(truth = y_train, prediction = boostTune.predictions)
(3907+151)/nrow(x_train)

boostTune.predictions.test = round(predict(boostTune,
                                      newdata = as.data.frame(x_test),
                                      n.trees = 110,
                                      type = "response"),0)
table(truth = y_test, prediction = boostTune.predictions.test)
(965+41)/nrow(x_test)
