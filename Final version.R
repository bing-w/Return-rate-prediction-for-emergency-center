# Initialise
rm(list=ls())

########## Only in training set

#### Load Hospital_Train.csv
set.seed(123)
hospital_train <- read.csv('~/Dropbox/Courtney/Project/Hospitals_Train.csv')
#hospital_test <- read.csv('~/Dropbox/Courtney/Project/Hospitals_Test_X.csv')
#hospital_test$RETURN = 0
#hospital_train <- rbind(hospital_train, hospital_test)

dim(hospital_train)
sum(is.na(hospital_train))
###### DATA CLEANING ######
### HOSPITAL
hospital_train$HOSPITAL[hospital_train$HOSPITAL==''] = NA
sum(is.na(hospital_train$HOSPITAL))
### GENDER
hospital_train$GENDER = ifelse(hospital_train$GENDER == 'Female',1,0)  ######## Female 1 Male 0
hospital_train$GENDER[hospital_train$GENDER == ''] = NA
hospital_train$GENDER = as.factor(hospital_train$GENDER)
sum(is.na(hospital_train$GENDER))
### AGE
sum(is.na(hospital_train$AGE))
### RACE
hospital_train$RACE[hospital_train$RACE==''] = 'Unknown'
hospital_train$RACE[hospital_train$RACE=='Declined to Answer'] = 'Unknown'
summary(hospital_train$RACE)
hospital_train$RACE[hospital_train$RACE=='American Indian or Alaskan Native'] = 'Other'
hospital_train$RACE[hospital_train$RACE=='Hispanic'] = 'Other'
hospital_train$RACE[hospital_train$RACE=='Native Hawaiian or Other Pacific Islander'] = 'Other'
hospital_train$RACE[hospital_train$RACE=='Two or More Races'] = 'Other'
sum(is.na(hospital_train$RACE))

### ETHNICITY
hospital_train$ETHNICITY[hospital_train$ETHNICITY == ''] = 'Unknown'
hospital_train$ETHNICITY[hospital_train$ETHNICITY == 'Declined to Answer'] = 'Unknown'
summary(hospital_train$ETHNICITY)
sum(is.na(hospital_train$ETHNICITY))
### FINANCIAL CLASS
hospital_train$FINANCIAL_CLASS[hospital_train$FINANCIAL_CLASS == 'Medicaid Pending'] = 'Other'
hospital_train$FINANCIAL_CLASS[hospital_train$FINANCIAL_CLASS == 'Out of State Medicaid'] = 'Other'
hospital_train$FINANCIAL_CLASS[hospital_train$FINANCIAL_CLASS == 'Global Contracts'] = 'Other'
summary(hospital_train$FINANCIAL_CLASS) 
sum(is.na(hospital_train$FINANCIAL_CLASS))
### WEEKDAY_ARR
hospital_train$WEEKDAY_ARR = ifelse(hospital_train$WEEKDAY_ARR > 1 & hospital_train$WEEKDAY_ARR < 7,'Weekday','Weekend')
hospital_train$WEEKDAY_ARR = ifelse(hospital_train$WEEKDAY_ARR=='Weekday',1,0)
hospital_train$WEEKDAY_ARR = as.factor(hospital_train$WEEKDAY_ARR)
sum(is.na(hospital_train$WEEKDAY_ARR))
summary(hospital_train$WEEKDAY_ARR)
### HOUR_ARR
hospital_train$HOUR_ARR = ifelse(hospital_train$HOUR_ARR > 7 & hospital_train$HOUR_ARR < 21, 'Day', 'Night' )
hospital_train$HOUR_ARR = ifelse(hospital_train$HOUR_ARR=='Day',1,0)
hospital_train$HOUR_ARR = as.factor(hospital_train$HOUR_ARR)
sum(is.na(hospital_train$HOUR_ARR))
summary(hospital_train$HOUR_ARR)

### MONTH_ARR
hospital_train$MONTH_ARR = ifelse(hospital_train$MONTH_ARR > 2 & hospital_train$MONTH_ARR < 6, 'Spring', ifelse(
  hospital_train$MONTH_ARR > 5 & hospital_train$MONTH_ARR < 9,'Summer',ifelse(
    hospital_train$MONTH_ARR > 8 & hospital_train$MONTH_ARR < 12,'Autumn','Winter'
  )
))
hospital_train$MONTH_ARR = as.factor(hospital_train$MONTH_ARR)
summary(hospital_train$MONTH_ARR)
sum(is.na(hospital_train$MONTH_ARR))
### WEEKDAY_DEP

hospital_train$WEEKDAY_DEP = ifelse(hospital_train$WEEKDAY_DEP > 1 & hospital_train$WEEKDAY_DEP < 7,'Weekday','Weekend')
hospital_train$WEEKDAY_DEP = ifelse(hospital_train$WEEKDAY_DEP=='Weekday',1,0)
hospital_train$WEEKDAY_DEP = as.factor(hospital_train$WEEKDAY_DEP)
sum(is.na(hospital_train$WEEKDAY_DEP))
### HOUR_DEP
hospital_train$HOUR_DEP = ifelse(hospital_train$HOUR_DEP > 7 & hospital_train$HOUR_DEP < 21, 'Day', 'Night' )
hospital_train$HOUR_DEP = ifelse(hospital_train$HOUR_DEP=='Day',1,0)
hospital_train$HOUR_DEP = as.factor(hospital_train$HOUR_DEP)
sum(is.na(hospital_train$HOUR_DEP))
summary(hospital_train$HOUR_DEP)

### MONTH_DEP
hospital_train$MONTH_DEP = ifelse(hospital_train$MONTH_DEP > 2 & hospital_train$MONTH_DEP < 6, 'Spring', ifelse(
  hospital_train$MONTH_DEP > 5 & hospital_train$MONTH_DEP < 9,'Summer',ifelse(
    hospital_train$MONTH_DEP > 8 & hospital_train$MONTH_DEP < 12,'Autumn','Winter'
  )
))
hospital_train$MONTH_DEP = as.factor(hospital_train$MONTH_DEP)
sum(is.na(hospital_train$MONTH_DEP))
### SAME_DAY
hospital_train$SAME_DAY = as.factor(hospital_train$SAME_DAY)
sum(is.na(hospital_train$SAME_DAY))
### ED_RESULT
hospital_train$ED_RESULT[hospital_train$ED_RESULT == ''] = NA
summary(hospital_train$ED_RESULT)
hospital_train$ED_RESULT = factor(hospital_train$ED_RESULT, levels = c(levels(hospital_train$ED_RESULT), 'Other'))
hospital_train$ED_RESULT[hospital_train$ED_RESULT == 'Admit to External Psychiatric Facility'] = 'Other'
hospital_train$ED_RESULT[hospital_train$ED_RESULT == 'Admit to UMMS Psychiatry '] = 'Other'
hospital_train$ED_RESULT[hospital_train$ED_RESULT == 'Deceased'] = 'Other'
hospital_train$ED_RESULT[hospital_train$ED_RESULT == 'Send to L&D after Rooming'] = 'Other'
sum(is.na(hospital_train$ED_RESULT))
### ACUITY_ARR 
hospital_train$ACUITY_ARR = factor(hospital_train$ACUITY_ARR, levels = c(levels(hospital_train$ACUITY_ARR), 'Unknown'))
hospital_train$ACUITY_ARR[hospital_train$ACUITY_ARR == ''] = 'Unknown'
hospital_train$ACUITY_ARR[hospital_train$ACUITY_ARR == '5 Purple'] = NA
sum(is.na(hospital_train$ACUITY_ARR))
summary(hospital_train$ACUITY_ARR)
### DC_RESULT
hospital_train$DC_RESULT = factor(hospital_train$DC_RESULT, levels = c(levels(hospital_train$DC_RESULT), 'Other'))
hospital_train$DC_RESULT[hospital_train$DC_RESULT == ''] = 'Not specified Other or Unknown'
summary(hospital_train$DC_RESULT)
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Administrative Discharge - IP Only'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Admitted as an Inpatient - OP Only'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'AWOL - IP Only'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Discharge to a chronic hospital - IP Only'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Discharge to on-site distinct psychiatric unit from acute care - IP Only'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Discharge to on-site distinct rehabilitation unit from acute care - IP Only'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Discharge to Onsite Chronic From Acute'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Discharge to Residential Treatment Center'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == ' Discharge to Shelters'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Discharge to State Psychiatric Hospital care hospital'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Discharge to Substance Abuse Rehabilitation Facility'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Discharge to Supervised/Congregate House'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Discharged to Other Facility'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'ER to Trauma'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Hospice/Home'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Hospice/Medical Facility'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Intermediate Care Facility'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Left Prior to Exam'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Long Term Care'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Nursing Facility - IP Only'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Psychiatric Unit or Psychiatric Hospital'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'Still a Patient'] = 'Other'
hospital_train$DC_RESULT[hospital_train$DC_RESULT == 'To another institution for OP services as spec. by the disch plan of care - OP Only'] = 'Other'
sum(is.na(hospital_train$DC_RESULT))
### ADMIT_RESULT -- 36110 NAs
hospital_train$ADMIT_RESULT = factor(hospital_train$ADMIT_RESULT, levels = c(levels(hospital_train$ADMIT_RESULT), 'Not Admitted'))
hospital_train$ADMIT_RESULT[hospital_train$ADMIT_RESULT == ''] = 'Not Admitted'
sum(is.na(hospital_train$ADMIT_RESULT))
summary(hospital_train$ADMIT_RESULT)
### CONSULT_ORDER
hospital_train$CONSULT_ORDER = as.factor(hospital_train$CONSULT_ORDER)
summary(hospital_train$CONSULT_ORDER)
### CONSULT_CHARGE
hospital_train$CONSULT_CHARGE = as.factor(hospital_train$CONSULT_CHARGE)
summary(hospital_train$CONSULT_CHARGE)
### CONSULT_IN_ED -- 42821 NAs
hospital_train$CONSULT_IN_ED[is.na(hospital_train$CONSULT_IN_ED)] = 0
hospital_train$CONSULT_IN_ED = as.factor(hospital_train$CONSULT_IN_ED)
sum(is.na(hospital_train$CONSULT_IN_ED))
summary(hospital_train$CONSULT_IN_ED)

### DIAGNOSIS
hospital_train$DIAGNOSIS = ifelse(hospital_train$DIAGNOSIS=='Yes',1,0)
hospital_train$DIAGNOSIS = as.factor(hospital_train$DIAGNOSIS)
summary(hospital_train$DIAGNOSIS)
sum(is.na(hospital_train$DIAGNOSIS))
### DIAG_DETAILS
sum(is.na(hospital_train$DIAG_DETAILS))
summary(hospital_train$DIAG_DETAILS)
### RISK -- 38545 NAs
hospital_train$RISK = factor(hospital_train$RISK, levels = c(levels(hospital_train$RISK), 'unknown'))
hospital_train$RISK[hospital_train$RISK == ''] = 'unknown'
### SEVERITY -- 38545 NAs
hospital_train$SEVERITY = factor(hospital_train$SEVERITY, levels = c(levels(hospital_train$SEVERITY), 'unknown'))
hospital_train$SEVERITY[hospital_train$SEVERITY == ''] = 'unknown'
### CHARGES -- NA filled with mean
hospital_train$CHARGES[hospital_train$CHARGES == '#VALUE!'] = NA
hospital_train$CHARGES = as.numeric(as.character(hospital_train$CHARGES))
summary(hospital_train$CHARGES)
#hospital_train$CHARGES[hospital_train$CHARGES == 3] = NA
sum(is.na(hospital_train$CHARGES))

### RETURN
hospital_train$RETURN[hospital_train$RETURN == '#N/A'] = NA
hospital_train$RETURN = ifelse(hospital_train$RETURN=='Yes',1,0)
hospital_train$RETURN = as.factor(hospital_train$RETURN)
summary(hospital_train$RETURN)

################# Create new variables ##############
##### as numeric
hospital_train$CONSULT_ORDER = as.numeric(hospital_train$CONSULT_ORDER)
hospital_train$CONSULT_CHARGE = as.numeric(hospital_train$CONSULT_CHARGE)
###### Add order and charge
hospital_train$CONSULT_C = hospital_train$CONSULT_ORDER + hospital_train$CONSULT_CHARGE
######## Delete column CONSULT_CHARGE
hospital_train = subset(hospital_train, select = -c(CONSULT_CHARGE))
hospital_train$CONSULT_C = as.factor(hospital_train$CONSULT_C)
#### Filling missing value in CONSULT_IN_ED as 0
hospital_train$CONSULT_IN_ED[is.na(hospital_train$CONSULT_IN_ED)] <- 0
##### as numeric
hospital_train$CONSULT_IN_ED = as.numeric(hospital_train$CONSULT_IN_ED)
######### Add order and in_ed
hospital_train$CONSULT_ED = hospital_train$CONSULT_ORDER + hospital_train$CONSULT_IN_ED
######## Delete column CONSULT_ORDER and CONSULT_IN_ED
hospital_train = subset(hospital_train, select = -c(CONSULT_ORDER,CONSULT_IN_ED))
hospital_train$CONSULT_ED = as.factor(hospital_train$CONSULT_ED)

### DROP NAs
hospital_train = na.omit(hospital_train)
nrow(hospital_train)
sum(is.na(hospital_train))
### Drop levels
hospital_train$HOSPITAL = factor(hospital_train$HOSPITAL, levels = unique(hospital_train$HOSPITAL))
hospital_train$GENDER = factor(hospital_train$GENDER, levels = unique(hospital_train$GENDER))
hospital_train$RACE = factor(hospital_train$RACE, levels = unique(hospital_train$RACE))
hospital_train$ETHNICITY = factor(hospital_train$ETHNICITY, levels = unique(hospital_train$ETHNICITY))
hospital_train$FINANCIAL_CLASS = factor(hospital_train$FINANCIAL_CLASS, levels = unique(hospital_train$FINANCIAL_CLASS))
hospital_train$ED_RESULT = factor(hospital_train$ED_RESULT, levels = unique(hospital_train$ED_RESULT))
hospital_train$ACUITY_ARR = factor(hospital_train$ACUITY_ARR, levels = unique(hospital_train$ACUITY_ARR))
hospital_train$DC_RESULT = factor(hospital_train$DC_RESULT, levels = unique(hospital_train$DC_RESULT))
hospital_train$ADMIT_RESULT = factor(hospital_train$ADMIT_RESULT, levels = unique(hospital_train$ADMIT_RESULT))
hospital_train$RISK = factor(hospital_train$RISK, levels = unique(hospital_train$RISK))
hospital_train$SEVERITY = factor(hospital_train$SEVERITY, levels = unique(hospital_train$SEVERITY))


colnames(hospital_train)[colnames(hospital_train)=="MONTH_ARR"] = "SEASON"
colnames(hospital_train)
hospital_train <- hospital_train[-c(1,11:13) ]
colnames(hospital_train)




####### Data partitioning
library(caTools)  # partitioning
library(randomForest) ## randomForest modeling
library(caret) #confusion matrix


data = rbind(hospital_train,test)
newdata = data.frame(model.matrix(~.-1, data[,c(1:2,4:18,20:22)]))
hospital_train = newdata[1:37909,]

hospital_test = newdata[37910:49941,]
hospital_test$RETURN1
### Attention here, this is for XGboost
hospital_train=data.frame(model.matrix(~.-1,hospital_train[,c(1:2,4:18,20:22)]))

## Partitioning
num_obs = nrow(hospital_train)
test_instn = sample(num_obs,0.15*num_obs)
test_set <- hospital_train[test_instn, ]
rest_set <- hospital_train[-test_instn, ]
valid_instn = sample(nrow(rest_set),0.15*nrow(rest_set))
valid_set <- rest_set[valid_instn, ]
train_set <- rest_set[-valid_instn, ]


### Try XGB
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(carData)
library(car)
label = hospital_train$RETURN1

bst = xgboost(data = as.matrix(hospital_train[,-5]), label <- label, max.depth = 15, 
              eta = 0.2, max_delta_step = 5, nrounds = 25, seed = 1,
              subsample = 0.5, objective = "binary:logistic")


y_pred <- predict(bst, as.matrix(hospital_train[,-5]))


y_pred <- predict(bst, as.matrix(test_set[,-5]))
y_pred
bst_class_test=ifelse(y_pred>0.5,1,0)
bst_class_test
table_test_bst = table(test_set$RETURN1,bst_class_test)
sum(ifelse(bst_class_test==hospital_train$RETURN1,1,0))/nrow(hospital_train)
table_test_bst

## See the result of XGBoost
model <- bst.dump(bst, with.stats = T)
model[1:10] 



###### RandomRorest for all variables
RandomForest <- This T37907randomForest(
  RETURN ~ 
    HOSPITAL+
    RACE+
    GENDER+
    AGE+
    ETHNICITY+
    FINANCIAL_CLASS+
    WEEKDAY_ARR+
    HOUR_ARR+
    SEASON+
    SAME_DAY+
    ED_RESULT+
    ACUITY_ARR+
    DC_RESULT+
    ADMIT_RESULT+
    DIAGNOSIS+
    DIAG_DETAILS+
    RISK+
    SEVERITY+
    CONSULT_ED+
    CONSULT_C+
    CHARGES,
  data=hospital_train,
  importance=TRUE, 
  ntree=5000,
  mtry = 15)

varImpPlot(RandomForest)

#Prediction2 <- predict(RandomForest, test_set)
#rftable = table(Prediction2, test_set$RETURN)
#confusionMatrix(Prediction2, test_set$RETURN)

#Prediction22<- predict(RandomForest,train_set)
#table(Prediction22, train_set$RETURN)
#confusionMatrix(Prediction22, train_set$RETURN)    ####### how to handle overfitting here?



cutoffs=c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
rf_acc=rep(0,13)
rf_TPR=rep(0,13)
rf_TNR=rep(0,13)

rf_preds=predict(RandomForest,newdata=valid_set,type="prob")
rf_probs=rf_preds[,2]
for(i in 1:13){
  rf_class=ifelse(rf_probs>cutoffs[i],1,0)
  confuse_valid=table(valid_set$RETURN,rf_class)
  rf_acc[i]=(confuse_valid[1,1]+confuse_valid[2,2])/sum(confuse_valid)
  rf_TNR[i]=(confuse_valid[1,1])/(confuse_valid[1,1]+confuse_valid[1,2])
  rf_TPR[i]=(confuse_valid[2,2])/(confuse_valid[2,1]+confuse_valid[2,2])
}

rf_acc
rf_TPR
rf_TNR

###### ROC
plot(1-rf_TNR, rf_TPR, type='l')
###### Spider plot
plot(cutoffs, rf_TNR, type = 'l', col = 'red')
lines(cutoffs, rf_TPR, col = 'green')
lines(cutoffs, rf_acc, col = 'blue')
legend(0.8, 0.5, legend=c("rf_TNR", "rf_TPR", "rf_acc"),
       col=c("red", "green", "blue"), lty=1:2, cex=0.7)

## Accuracy in training set
rf_preds_train=predict(RandomForest,newdata=train_set,type="prob")
rf_probs_train=rf_preds_train[,2]
rf_class_train=ifelse(rf_probs_train>0.5,1,0)

table_train = table(train_set$RETURN,rf_class_train)
sum(ifelse(rf_class_train==train_set$RETURN,1,0))/nrow(train_set)

rf_TPR_train=(table_train[2,2])/(table_train[2,1]+table_train[2,2])
rf_TPR_train

## Accuracy in testing set.
rf_preds_test=predict(RandomForest,newdata=test_set,type="prob")
rf_probs_test=rf_preds_test[,2]
rf_class_test=ifelse(rf_probs_test>0.5,1,0)

table_test = table(test_set$RETURN,rf_class_test)
sum(ifelse(rf_class_test==test_set$RETURN,1,0))/nrow(test_set)

rf_TPR_test=(table_test[2,2])/(table_test[2,1]+table_test[2,2])
rf_TPR_test


## Bagging for all variables
bag.trees <- randomForest(
  RETURN ~ 
    HOSPITAL+
    RACE+
    GENDER+
    AGE+
    ETHNICITY+
    FINANCIAL_CLASS+
    WEEKDAY_ARR+
    HOUR_ARR+
    SEASON+
    SAME_DAY+
    ED_RESULT+
    ACUITY_ARR+
    DC_RESULT+
    ADMIT_RESULT+
    DIAGNOSIS+
    DIAG_DETAILS+
    RISK+
    SEVERITY+
    CONSULT_ED+
    CONSULT_C+
    CHARGES,
  data=hospital_train,
  importance=TRUE, 
  ntree=5000,
  mtry = 21)

varImpPlot(bag.trees)

#Prediction3 <- predict(bag.trees, test_set)
#confusionMatrix(Prediction3, test_set$RETURN)

#Prediction33<- predict(bag.trees,train_set)
#confusionMatrix(Prediction33, train_set$RETURN)

## accuracy for training set 


## accuracy for val set
cutoffs=c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
bag_acc=rep(0,13)
bag_TPR=rep(0,13)
bag_TNR=rep(0,13)

bag_preds=predict(bag.trees,newdata=valid_set,type="prob")
bag_probs=bag_preds[,2]
for(i in 1:13){
  bag_class=ifelse(bag_probs>cutoffs[i],1,0)
  table_valid_bag=table(valid_set$RETURN,bag_class)
  bag_acc[i]=(table_valid_bag[1,1]+table_valid_bag[2,2])/sum(table_valid_bag)
  bag_TNR[i]=(table_valid_bag[1,1])/(table_valid_bag[1,1]+table_valid_bag[1,2])
  bag_TPR[i]=(table_valid_bag[2,2])/(table_valid_bag[2,1]+table_valid_bag[2,2])
}

bag_acc
bag_TPR
bag_TNR

## accuracy for test set 
bag_preds_test=predict(bag.trees,newdata=test_set,type="prob")
bag_probs_test=bag_preds_test[,2]
bag_class_test=ifelse(bag_probs_test>0.6,1,0)

table_test_bag = table(test_set$RETURN,bag_class_test)
sum(ifelse(bag_class_test==test_set$RETURN,1,0))/nrow(test_set)

bag_TPR_test=(table_test_bag[2,2])/(table_test_bag[2,1]+table_test_bag[2,2])
bag_TPR_test

## accuracy for training set
bag_preds_train=predict(bag.trees,newdata=train_set,type="prob")
bag_probs_train=bag_preds_train[,2]
bag_class_train=ifelse(bag_probs_train>0.6,1,0)

table_train_bag = table(train_set$RETURN,bag_class_train)
sum(ifelse(bag_class_train==train_set$RETURN,1,0))/nrow(train_set)

bag_TPR_train=(table_train_bag[2,2])/(table_train_bag[2,1]+table_train_bag[2,2])
bag_TPR_train





#### Boosting
library(gbm)

#hospital_train$scal_charge = (hospital_train$CHARGES - min(hospital_train$CHARGES))/(max(hospital_train$CHARGES)-min(hospital_train$CHARGES))
#hospital_train$scal_age = (hospital_train$AGE - min(hospital_train$AGE))/(max(hospital_train$AGE)-min(hospital_train$AGE))


num_obs = nrow(now)
test_instn = sample(num_obs,0.15*num_obs)
test_set <- now[test_instn, ]
rest_set <- now[-test_instn, ]
valid_instn = sample(nrow(rest_set),0.15*nrow(rest_set))
valid_set <- rest_set[valid_instn, ]
train_set <- rest_set[-valid_instn, ]

boost=gbm(RETURN1~.,data=train_set,distribution="bernoulli",
                 n.trees=10000)

summary(boost)
### choosing cutoff in validation set.
cutoffs=c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
boost_acc=rep(0,13)
boost_TPR=rep(0,13)
boost_TNR=rep(0,13)


boost_probs=predict(boost,newdata=valid_set,n.trees=1000,type="response")
boost_probs
for(i in 1:13){
  boost_class=ifelse(boost_probs>cutoffs[i],1,0)
  table_valid_boost=table(valid_set$RETURN1,boost_class)
  boost_acc[i]=(table_valid_boost[1,1]+table_valid_boost[2,2])/sum(table_valid_boost)
  boost_TNR[i]=(table_valid_boost[1,1])/(table_valid_boost[1,1]+table_valid_boost[1,2])
  boost_TPR[i]=(table_valid_boost[2,2])/(table_valid_boost[2,1]+table_valid_boost[2,2])
}

boost_acc
boost_TPR
boost_TNR

## accuracy in testing set


boost_probs_test=predict(boost,newdata=test_set,n.trees=10000,type="response")

boost_class_test=ifelse(boost_probs_test>0.5,1,0)

table(test_set$RETURN1,boost_class_test)
sum(ifelse(boost_class_test==test_set$RETURN1,1,0))/nrow(test_set)


# accuracy in training set
boost_probs_train=predict(boost,newdata=train_set,n.trees=10000,type="response")

boost_class_train=ifelse(boost_probs_train>0.5,1,0)

table(train_set$RETURN1,boost_class_train)
sum(ifelse(boost_class_train==train_set$RETURN1,1,0))/nrow(train_set)





## Try 




####### Most common baseline
table(train_set$RETURN)
28584 / (28584 + 9325) = 0.7540162
table(valid_set$RETURN)
table(test_set$RETURN)
7137 /(7137 + 2341) = 0.753007

######## Logistic regression
lg = glm(RETURN ~ 
           HOSPITAL+
           RACE+
           GENDER+
           AGE+
           ETHNICITY+
           FINANCIAL_CLASS+
           WEEKDAY_ARR+
           HOUR_ARR+
           MONTH_ARR+
           MONTH_DEP+
           WEEKDAY_DEP+ 
           HOUR_DEP+ 
           MONTH_DEP+
           SAME_DAY+
           DC_RESULT+
           ED_RESULT+
           ADMIT_RESULT+
           ACUITY_ARR+
           DIAGNOSIS+
           DIAG_DETAILS+
           RISK+
           SEVERITY+
           CONSULT_ED+
           CONSULT_C+
           CHARGES,
         data=train_set, 
         family = 'binomial')

summary(lg)

#############
lg_train = predict(lg, newdata = train_set, type = 'response')
lg_test = predict(lg, newdata = test_set, type = 'response')
lg_class_test = ifelse(lg_test > 0.4, 1, 0)
table_lg_test = table(test_set$RETURN, lg_class_test, dnn = c('actual', 'predicted'))
table_lg_test

lg_acc=(table_lg_test[1,1]+table_lg_test[2,2])/sum(table_lg_test)
lg_acc
lg_TNR=(table_lg_test[1,1])/(table_lg_test[1,1]+table_lg_test[1,2])
lg_TNR
lg_TPR=(table_lg_test[2,2])/(table_lg_test[2,1]+table_lg_test[2,2])
lg_TPR

# False Negtive is too high, which means that acutal return but predict not return, 
# this is possibility because our cut off is too high, leading to less predicted return
# I would decrease cutoff 
lg_class_test.3 = ifelse(lg_test > 0.3, 1, 0)
table_lg_test.3 = table(test$RETURN, lg_class_test.3, dnn = c('actual', 'predicted'))
table_lg_test.3

lg_acc.3=(table_lg_test.3[1,1]+table_lg_test.3[2,2])/sum(table_lg_test.3)
lg_acc.3
lg_TNR.3=(table_lg_test.3[1,1])/(table_lg_test.3[1,1]+table_lg_test.3[1,2])
lg_TNR.3
lg_TPR.3=(table_lg_test.3[2,2])/(table_lg_test.3[2,1]+table_lg_test.3[2,2])
lg_TPR.3

####### What about other cutoffs?
cutoffs=c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99)
log_acc=rep(0,13)
log_TPR=rep(0,13)
log_TNR=rep(0,13)

log_preds=predict(lg,newdata=valid_set,type="response")

for(i in 1:13){
  log_class=ifelse(log_preds>cutoffs[i],1,0)
  confuse_test=table(valid_set$RETURN,log_class)
  log_acc[i]=(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)
  log_TNR[i]=(confuse_test[1,1])/(confuse_test[1,1]+confuse_test[1,2])
  log_TPR[i]=(confuse_test[2,2])/(confuse_test[2,1]+confuse_test[2,2])
  
}

log_acc
log_TPR
log_TNR

###### ROC
plot(1-log_TNR, log_TPR, type='l')
###### Spider plot
plot(cutoffs, log_TNR, type = 'l', col = 'red')
lines(cutoffs, log_TPR, col = 'green')
lines(cutoffs, log_acc, col = 'blue')

###### LDA
library(MASS)
lda_model = lda(train_set$RETURN~.-INDEX, data = train_set)
## Warning message:
## In lda.default(x, grouping, ...) : variables are collinear
lda_predict = predict(lda_model,newdata=test_set)
lda_predict$posterior
lda_preds = ifelse(lda_predict$posterior[,2]>0.4,1,0)
lda_table = table(test_set$RETURN,lda_preds)
lda_acc = (lda_table[1,1]+lda_table[2,2])/sum(lda_table)
lda_acc
lda_table
lg_TPR=(lda_table[2,2])/(lda_table[2,1]+lda_table[2,2])
lg_TPR

##### Decision tree
library(tree)
tree = tree(RETURN ~ 
              HOSPITAL+
              RACE+
              GENDER+
              AGE+
              ETHNICITY+
              FINANCIAL_CLASS+
              WEEKDAY_ARR+
              HOUR_ARR+
              MONTH_ARR+
              MONTH_DEP+
              WEEKDAY_DEP+ 
              HOUR_DEP+ 
              MONTH_DEP+
              SAME_DAY+
              DC_RESULT+
              ED_RESULT+
              ADMIT_RESULT+
              ACUITY_ARR+
              DIAGNOSIS+
              DIAG_DETAILS+
              RISK+
              SEVERITY+
              CONSULT_ED+
              CONSULT_C+
              CHARGES,
            data = train_set)

plot(tree)
text(tree)

library(rpart) #fit tree model
library(rattle) #fancyplot

fit <- rpart(RETURN ~ 
               HOSPITAL+
               RACE+
               GENDER+
               AGE+
               ETHNICITY+
               FINANCIAL_CLASS+
               WEEKDAY_ARR+
               HOUR_ARR+
               MONTH_ARR+
               MONTH_DEP+
               WEEKDAY_DEP+ 
               HOUR_DEP+ 
               MONTH_DEP+
               SAME_DAY+
               DC_RESULT+
               ED_RESULT+
               ADMIT_RESULT+
               ACUITY_ARR+
               DIAGNOSIS+
               DIAG_DETAILS+
               RISK+
               SEVERITY+
               CONSULT_ED+
               CONSULT_C+
               CHARGES,
             data=train_set,
             method="class",
             control = rpart.control(maxdepth = 30)
)

printcp(fit)
summary(fit)
plot(fit, uniform=TRUE, 
     main="Classification Tree for Hospital")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

fancyRpartPlot(fit)

Prediction <- predict(fit, test_set, type = "class")
table = table(Prediction, test_set$RETURN)
confusionMatrix(Prediction, test_set$RETURN)
lg_TPR=(table[2,2])/(table[2,1]+table[2,2])
lg_TPR


https://dinsdalelab.sdsu.edu/metag.stats/code/randomforest.html
https://stats.stackexchange.com/questions/111968/random-forest-how-to-handle-overfitting
http://www.michaeljgrogan.com/neural-network-modelling-neuralnet-r/
  https://www.analyticsvidhya.com/blog/2017/09/creating-visualizing-neural-network-in-r/
  

