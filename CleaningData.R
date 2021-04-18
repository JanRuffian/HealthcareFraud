rm(list=ls())
library(gtools)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(lubridate)
library(fastDummies)
library(glmnet)
library(caret)
library(cowplot)
library(randomForest)

#Load Dataframes:
PotentialFraudData= read.csv('/Users/janruffner/Desktop/HealthcareFraudCapstoneProject/data/Train-1542865627584.csv')
OutpatientData = read.csv('/Users/janruffner/Desktop/HealthcareFraudCapstoneProject/data/Train_Outpatientdata-1542865627584.csv')
InpatientData = read.csv('/Users/janruffner/Desktop/HealthcareFraudCapstoneProject/data/Train_Inpatientdata-1542865627584.csv')
Beneficiarydata = read.csv('/Users/janruffner/Desktop/HealthcareFraudCapstoneProject/data/Train_Beneficiarydata-1542865627584.csv')

OutpatientData$OupatientInpatient="Outpatient"
InpatientData$OupatientInpatient="Inpatient"
OutpatientData$OupatientInpatient2=1
InpatientData$OupatientInpatient2=0

#Combine Dataframes:
HealthData = smartbind(OutpatientData, InpatientData)
HealthData = merge(HealthData, Beneficiarydata, by='BeneID')
HealthData = merge(HealthData, PotentialFraudData, by='Provider')
nrow(HealthData)

#Missing Observations
colSums(is.na(HealthData))

########### Cleaning Data #########################################################################
###################################################################################################

# Reclassifying Variables
HealthData$PotentialFraud2 <- ifelse(HealthData$PotentialFraud=="No", 0, 1)

dummychange_2_1 <- function(x){
  x = ifelse(x==2,1,0)
}
dummies <- c("Gender","ChronicCond_Heartfailure", "ChronicCond_Alzheimer", "ChronicCond_Cancer", 
             "ChronicCond_KidneyDisease", "ChronicCond_IschemicHeart", 
             "ChronicCond_Diabetes", "ChronicCond_ObstrPulmonary", "ChronicCond_stroke",
             "ChronicCond_rheumatoidarthritis", "ChronicCond_Osteoporasis", "ChronicCond_Depression")
HealthData[,dummies] <- dummychange_2_1(HealthData[, dummies])

# Generating Variables
# Age:
HealthData$DOB <- as.Date(HealthData$DOB)
HealthData$Age <- 2015-year(HealthData$DOB)

########### EDA ###################################################################################
###################################################################################################

# Analyzing Insurance Claim Amount Reimbursed:
HealthData %>% group_by(PotentialFraud) %>% 
               summarize(mean.InscClaimAmtReimbursed=mean(InscClaimAmtReimbursed)) %>%
              ggplot()+geom_bar(aes(x=PotentialFraud, y=mean.InscClaimAmtReimbursed), 
                                stat = "identity",
                                fill=c("#01BFC4", "#F8766D")) +
                                xlab("Potenial Fraud") +
                                ylab("Mean InscClaimAmtReimbursed") +
                                ggtitle("Average Cost by Hospital group")
# The average cost of hospitals seems to be higher in hospitals where there is fraud.

###########

#Analyzing fraud and the number of physicians in a hospital
HealthData_2 <- HealthData[!is.na(HealthData$AttendingPhysician),]
HealthData_2 %>% group_by(PotentialFraud, Provider) %>% 
                summarize(NumberOfPhyscian = n_distinct(AttendingPhysician)) %>% 
                filter(NumberOfPhyscian<150) %>%
                  ggplot(aes(x=NumberOfPhyscian, color=PotentialFraud, fill=PotentialFraud)) +
                  geom_density(alpha=0.4) +
                  xlab("Number of Physicians working in a hospital") +
                  ylab('density')+
                  ggtitle("Number of Physicians working in a hospital")
# If you have more physicians in a hospital there is a higher likelihood of fraud

###########

#Analyzing fraud and the number of patients in a hospital
HealthData_2 %>% group_by(PotentialFraud, Provider) %>% 
                summarize(NumberOfPatients = n_distinct(BeneID)) %>% 
                filter(NumberOfPatients<300) %>%
                  ggplot(aes(NumberOfPatients, fill = PotentialFraud, color = PotentialFraud)) +
                  geom_density(alpha=0.5) +
                  xlab("Number of Patients visiting a hospital") +
                  ylab('density')+
                  ggtitle("Number of Patients visiting a hospital")
# Hospitals with a higher amount of patients tend to be more fraudulent

###########

#Analyzing fraud and the number of claims in a hospital
HealthData_2 %>% group_by(PotentialFraud, Provider) %>% 
  summarize(NumberOfClaims = n_distinct(ClaimID)) %>% 
  filter(NumberOfClaims<1000) %>%
  ggplot(aes(NumberOfClaims, fill = PotentialFraud, color = PotentialFraud)) +
  geom_density(alpha=0.5) +
  xlab("Number of Claims in a hospital") +
  ylab('density')+
  ggtitle("Number of Claims in a hospital")
# Hospitals with a higher amount of claims tend to be more fraudulent

########### 

#Analyzing fraud by state
HealthData_3 <- HealthData %>% add_count(Provider, State) %>% 
              group_by(Provider) %>% 
              mutate(Majority = State[n == max(n)][1])

HealthData_3 %>% group_by(Majority, PotentialFraud) %>% 
               summarize(sumProvider=n_distinct(Provider)) %>%
               ggplot()+geom_bar(aes(x=Majority, y=sumProvider, fill=PotentialFraud), stat="identity")+
               ylab("Providers")+
               xlab("State")+
               ggtitle("Fraud by State")

########### 

#Analyzing fraud by County
HealthData_4 <- HealthData %>% add_count(Provider, County) %>% 
  group_by(Provider) %>% 
  mutate(Majority = County[n == max(n)][1])

HealthData_4 %>% group_by(Majority, PotentialFraud) %>% 
  summarize(sumProvider=n_distinct(Provider)) %>%
  ggplot()+geom_bar(aes(x=Majority, y=sumProvider, fill=PotentialFraud), stat="identity")+
  ylab("Providers")

########### 

# Anylzing Fraud by Type of Service (Inpatient, Outpatient, Both Services)
HealthData_6 <- HealthData_2 %>% group_by(Provider, PotentialFraud) %>%
  summarize(Both_Services = n_distinct(OupatientInpatient))

HealthData_6$BothServices = ifelse(HealthData_6$Both_Services==2, 1, 0) 
HealthData_7 <- HealthData_2 %>% group_by(Provider) %>% summarize(Distinct_Services = sum(OupatientInpatient2))
HealthData_7$Outpatient = ifelse(HealthData_7$Distinct_Services>0, 1, 0) 
HealthData_7$Inpatient = ifelse(HealthData_7$Distinct_Services==0, 1, 0) 
HealthData_7 <- merge(HealthData_7, HealthData_6, by="Provider")
HealthData_7 <- HealthData_7 %>% select(-Distinct_Services, -Both_Services)
HealthData_7$Outpatient <- ifelse(HealthData_7$BothServices==1, 0, HealthData_7$Outpatient)
HealthData_7$Inpatient <- ifelse(HealthData_7$BothServices==1, 0, HealthData_7$Inpatient)
HealthData_7$DistinctServices <- as.factor(ifelse(HealthData_7$Outpatient==1, "Outpatient", ifelse(HealthData_7$Inpatient==1, "Inpatient", "Both Services")))

HealthData_7 %>% group_by(DistinctServices, PotentialFraud) %>% summarize(sum.Services=n()) %>%
  ggplot()+geom_bar(aes(x=DistinctServices, y=sum.Services, fill=PotentialFraud), 
                    stat="identity", position=position_dodge())+
  ylab("Number of Providers")+
  xlab("Services")+
  ggtitle("Services provided by hospitals")

########### 

#Analyzing fraud by Diagnosis Code
HealthData_8 <- HealthData_2[!is.na(HealthData_2$ClmDiagnosisCode_1),]
HealthData_9 <- HealthData_8 %>% group_by(ClmDiagnosisCode_1, PotentialFraud) %>% summarize(sum.Claims = n_distinct(ClaimID), sum.InscClaimAmtReimbursed=sum(InscClaimAmtReimbursed)) 
HealthData_10 <- HealthData_8 %>% group_by(ClmDiagnosisCode_1) %>% summarize(sum.InscClaimAmtReimbursed2=sum(InscClaimAmtReimbursed), Total.Claims=n_distinct(ClaimID)) %>% filter(sum.InscClaimAmtReimbursed2>10000)
HealthData_9 <- merge(HealthData_9, HealthData_10, by="ClmDiagnosisCode_1")
HealthData_9$ratio = round(HealthData_9$sum.Claims/HealthData_9$Total.Claims,2) 
HealthData_9$ratio2 = ifelse(HealthData_9$PotentialFraud=="Yes" & HealthData_9$ratio>0.55, 1, ifelse(HealthData_9$PotentialFraud=="No" & HealthData_9$ratio<0.45, 1, 0))
HealthData_9 %>% filter(sum.InscClaimAmtReimbursed2>1000000, Total.Claims>100, ratio2==1) %>% 
  ggplot()+geom_bar(aes(x=ClmDiagnosisCode_1, y=sum.Claims, fill=PotentialFraud, colour=PotentialFraud), stat="identity", position=position_dodge())+
  ylab("Count of Claims")+
  xlab("Diagnosis Code")+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

###########
# Creating ranking for physicians and hospitals
# HealthData_6 <- HealthData_5 %>% filter(ClmDiagnosisCode_1 == "03842" | ClmDiagnosisCode_1 == "0389" | ClmDiagnosisCode_1 == "41071" | ClmDiagnosisCode_1 == "44024" | ClmDiagnosisCode_1 == "486" |ClmDiagnosisCode_1 == "5070" |ClmDiagnosisCode_1 == "51881" )
# HealthData_11 <- HealthData_2 %>% filter(OupatientInpatient=="Inpatient")
HealthData_11 <- HealthData_2
Codes <- HealthData_11 %>% group_by(ClmDiagnosisCode_1) %>% summarize(sumCodes = n())
CodesAbove10 <- Codes[Codes$sumCodes>10,]
library(plyr)
HealthData_11 <- join(HealthData_11, CodesAbove10, type = "left", by="ClmDiagnosisCode_1")
HealthData_11 <- HealthData_11[!is.na(HealthData_11$sumCodes),]
HealthData_12 <- HealthData_11 %>% group_by(ClmDiagnosisCode_1) %>% mutate(my_ranks = rank(InscClaimAmtReimbursed), perc = rank(InscClaimAmtReimbursed)/length(InscClaimAmtReimbursed))

HealthData_12 %>% group_by(AttendingPhysician, PotentialFraud) %>% summarize(mean_perc = mean(perc)) %>%
  ggplot(aes(x=mean_perc, fill = PotentialFraud, color = PotentialFraud)) +
  geom_density(alpha=0.5)+xlab("Physician Rating")+ggtitle("Physician Rating") 

HealthData_13 <- HealthData_12 %>% group_by(AttendingPhysician, PotentialFraud, Provider) %>% summarize(mean_perc = mean(perc))
HealthData_14 <- HealthData_13 %>% group_by(Provider, PotentialFraud) %>% summarize(summean_perc = mean(mean_perc))

HealthData_14 %>%  ggplot(aes(x=summean_perc, fill = PotentialFraud, color = PotentialFraud)) +
  geom_density(alpha=0.5) +xlab("Hospital Rating")+ggtitle("Hospital Rating") 

HealthData_12 %>% filter(OupatientInpatient=="Inpatient", ClmDiagnosisCode_1=='0088') %>% select(InscClaimAmtReimbursed, my_ranks, perc)

###################################################################################################
###################################################################################################

# Aggregating data on hospital level:
HealthData_5 <- HealthData_3[!is.na(HealthData_3$AttendingPhysician),]
HealthData_5 <- HealthData_5 %>% group_by(Provider, PotentialFraud2, Majority) %>% summarize(sum.BeneID = n_distinct(BeneID),
                                                                sum.InscClaimAmtReimbursed = sum(InscClaimAmtReimbursed),
                                                                mean.Age=mean(Age),
                                                                sum.AttendingPhysicians=n_distinct(AttendingPhysician),
                                                                GenderRatio = sum(Gender)/n(), 
                                                                sum.Claims = n_distinct(ClaimID),
                                                                sum.ChronicCond_Alzheimer = sum(ChronicCond_Alzheimer),
                                                                sum.ChronicCond_Cancer = sum(ChronicCond_Cancer), 
                                                                sum.ChronicCond_Heartfailure = sum(ChronicCond_Heartfailure), 
                                                                sum.ChronicCond_ObstrPulmonary = sum(ChronicCond_ObstrPulmonary),
                                                                sum.ChronicCond_stroke = sum(ChronicCond_stroke),
                                                                sum.ChronicCond_rheumatoidarthritis = sum(ChronicCond_rheumatoidarthritis),
                                                                sum.ChronicCond_Osteoporasis = sum(ChronicCond_Osteoporasis),
                                                                sum.ChronicCond_Diabetes = sum(ChronicCond_Diabetes), 
                                                                sum.ClaimsID = n_distinct(ClaimID),
                                                                OupatientInpatient2Ratio = sum(OupatientInpatient2)/n(),
                                                                sum.States = n_distinct(State), 
                                                                sum.Diagnosis = n_distinct(ClmDiagnosisCode_1)
                                                                )


# add Inpatient, Outpatient and BothServices Dummy
HealthData_7 <- HealthData_7 %>% select(-"PotentialFraud")
HealthData_5 <- merge(HealthData_5, HealthData_7, by="Provider")

# add Fraud Indicator on Provider Level
HealthData_14 <- HealthData_14 %>% select(-"PotentialFraud")
HealthData_5 <- merge(HealthData_5, HealthData_14, by="Provider")

# Creates dummies for states
HealthData_5 <- dummy_cols(HealthData_5, select_columns = 'Majority')

write_csv(HealthData_5, '/Users/janruffner/Desktop/HealthcareFraudCapstoneProject/Data/HealthData.csv')


