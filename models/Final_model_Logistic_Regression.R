# Load Dataset
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
                             sum.ChronicCond_stroke, sum.ChronicCond_ObstrPulmonary, summean_perc, starts_with("Majority")) %>% select(-"Majority_1")

controls2 <- test %>% select(sum.BeneID, sum.InscClaimAmtReimbursed, mean.Age,
                             OupatientInpatient2Ratio,
                             sum.AttendingPhysicians, GenderRatio, sum.Claims, 
                             sum.ChronicCond_Alzheimer, sum.ChronicCond_Cancer, 
                             sum.ChronicCond_Heartfailure, sum.ChronicCond_Diabetes, 
                             sum.ChronicCond_Osteoporasis, sum.ChronicCond_rheumatoidarthritis,
                             sum.ChronicCond_stroke, sum.ChronicCond_ObstrPulmonary, summean_perc, starts_with("Majority")) %>% select(-"Majority_1")

y_train <- as.factor(train$PotentialFraud2)
x_train <- data.matrix(controls)
y_test <- as.factor(test$PotentialFraud2)
x_test <- data.matrix(controls2)

### Run Logistic Regression
logit <- glm(PotentialFraud2~sum.BeneID+sum.InscClaimAmtReimbursed+mean.Age+
               OupatientInpatient2Ratio+sum.AttendingPhysicians+sum.AttendingPhysicians+
               GenderRatio+sum.Claims+sum.ChronicCond_Alzheimer+sum.ChronicCond_Cancer+
               sum.ChronicCond_Heartfailure+sum.ChronicCond_Diabetes+sum.ChronicCond_Osteoporasis+
               sum.ChronicCond_rheumatoidarthritis+sum.ChronicCond_stroke+
               sum.ChronicCond_ObstrPulmonary+as.factor(Majority)+summean_perc, data=train, family = "binomial")

summary(logit)

### Evaluate model based on train data
logic.predicted <- round(logit$fitted.values,0)
table(truth = y_train, prediction = logic.predicted)
(3881+182)/4328

### Evaluate model based on test data
logic.predicted.test <- round(predict(logit, as.data.frame(x_test), type="response"),0)
table(truth = y_test, prediction = logic.predicted.test)
(962+47)/1082