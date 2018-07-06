### https://www.kaggle.com/yifanxie/telstra-competition-dataset ###

rm(list=ls())
gc()

setwd("C:\\Users\\Eric\\Desktop\\NCSTATE\\Career\\Jobs\\Other\\InterWorks\\Test\\Data")

library(data.table)
library(dplyr)

event = fread("event_type.csv")
lg_feat = fread("log_feature.csv")
resource = fread("resource_type.csv")
severity = fread("severity_type.csv")
test = fread("test.csv")
train = fread("train.csv")

# Tagging Train & test and then rbinding
setDT(train[, train := TRUE])
setDT(test[, train := FALSE])
DT = rbind(train, test, fill = TRUE)

rm(list = c('train','test'))

library(reshape2)
# Number of observations by location
DT[, loc_count := .(.N), by = 'location']

# Include?
loc = dcast(DT, id ~ location)

DT = inner_join(severity, DT,by = 'id')
DT = data.table(DT)

# Making location numeric
DT[, loc_num := as.numeric(substr(location, 10, length(location)))] 
# Order of time w/in each location
DT[, loc_order := sequence(rle(loc_num)$lengths)*1, by = location]
# Reverse order of time w/in each location
DT[, loc_r_order := (loc_count - loc_order) + 1 ]
# Standardizing order btw 0 & 1
DT[ , loc_plt := loc_r_order/(loc_count + 1)]
# Rank of obeservations for each location
DT[, rnk := rank(loc_count), by = 'location']

DT = cbind(DT,data.table(seq(1,nrow(DT), by = 1)))
DT[, ind_eventf := ifelse(fault_severity >= 1, V1,NA)]
DT[, ind_eventb := ifelse(fault_severity >= 1, V1,NA)]

# Finding number min # of rows from an event for each observation
# Not using bc overfits
library(zoo)
DT$ind_eventf <- na.locf(DT$ind_eventf,na.rm=FALSE) # backfilling NAs
DT$ind_eventb <- na.locf(DT$ind_eventb,na.rm=FALSE, fromLast = TRUE) # backfilling NAs
DT[ , dist := pmin(abs(V1 - ind_eventf),abs(V1 - ind_eventb), na.rm = TRUE)]

# Will calculate moving average of fault severity
get.mav <- function(x,n=5){
  if(is.na(x[1])) x[1] <- mean(x,na.rm=TRUE)
  x <- na.locf(x,na.rm=FALSE)
  if(length(x)<n) return(x)
  c(x[1:(n-1)],rollapply(x,width=n,mean,align="right"))  
}

# Not using bc overfits

#DT[, mean(fault_severity, na.rm = TRUE), by = 'severity_type']
#DT[, sev_t := as.numeric(substr(severity_type,14,length(severity_type)))]
DT[ , MA := get.mav(fault_severity, n = 10)]

DT[,c('location','severity_type','ind_eventf','ind_eventb', 'V1')] <- NULL

# ploting relationship betwen location and time. 
library(ggplot2)
#qplot(loc_num, loc_plt, colour = as.factor(fault_severity), data = DT[!is.na(fault_severity),])

#### Severity ####
# Label encoding for merge 
severity$severity_type <- gsub(" ", "_", severity$severity_type)
severity = dcast(severity, id ~ severity_type, function(x)1, fill = 0)
severity = data.table(severity)

#### Resource ####
setDT(resource[, resource_cnt := .(.N), by = 'id'])
resource$resource_type <- gsub(" ", "_", resource$resource_type)

resource = dcast(resource, id + resource_cnt ~ resource_type, function(x) 1, fill = 0)


##### Event #####
event = left_join(event, DT, by = "id")
event = data.table(event)

## Summarizing events - Consider changing to mode
m = event[, .(N = (.N),
              Fault = round(mean(fault_severity, na.rm = TRUE)),
              Trn = mean(train)),
          by = 'event_type']

# Encoding events that happened more than 100 times, combining others based on most freq severity
m[, new_event := ifelse(N > 100, event_type, paste("event_other",Fault, sep = "_"))]
m$new_event <- gsub(" ", "_", m$new_event)
event = left_join(event, m, by = 'event_type')
event = data.table(event)
event[, event_count := .(.N), by = 'id']

# Label EnCoding
event = dcast(event, id + event_count ~ new_event, function(x) 1, fill = 0)
# Deleting column for events that didn't occur in training set
event[,'event_other_NaN'] <- NULL

##### Log Features #####
lg_feat = left_join(lg_feat, DT, by = "id")
lg_feat = data.table(lg_feat)


# Label encoding frequent logs and then grouping others based on average severity
m = lg_feat[, .(vol = sum(volume),
              Fault = round(mean(fault_severity, na.rm = TRUE)),
              Trn = mean(train)),
          by = 'log_feature']
m[, new_log := ifelse(vol > 1500, log_feature, paste("log_other",Fault, sep = "_"))]
m$new_log <- gsub(" ", "_", m$new_log)

# Merging in new_log and then dropping uncommon logs
d = left_join(lg_feat, m, by = 'log_feature')
d = data.table(d)
d = d[!(new_log == 'log_other_0' | new_log == 'log_other_1' | 
          new_log == 'log_other_2' | new_log == 'log_other_NaN'),]
d[,volume := log(1+volume)]
d = dcast(d, id ~ new_log, value.var = 'volume', fill = 0)

# Log stats by ID
lg_feat = data.table(lg_feat)
lg_feat[,N := .(.N), by = 'id']
look = lg_feat[, .(TOTAL = log(1+sum(volume)),
                   MN = log(1+mean(volume)),
                   MED = log(1+median(volume)),
                   MIN = log(1+min(volume)),
                   MAX = log(1+max(volume))),
               by = 'id']
look = left_join(look, unique(lg_feat[,c('id','N')]), by = 'id')

# Making new log data set
lg_feat = left_join(look, d, by = 'id')
lg_feat[is.na(lg_feat)] <- 0

rm(list = c('m', 'look', 'd'))

DT = inner_join(DT, event, by='id')
DT = inner_join(DT,lg_feat, by='id')
DT = inner_join(DT,resource, by='id')
DT = inner_join(DT,severity, by='id')

rm(list = c('event','lg_feat','resource','severity'))

summary(DT)

#### Models ####
DT = data.table(DT)
Train = DT[train == TRUE,-c('train','id')]
test = DT[train == FALSE,-c('train','id')]

## XGBBoost
library(xgboost)
library(readr)
library(caret)
library(mlr)
library(car)

tr_sev = Train[,fault_severity] 
Train = Train[, -c("fault_severity","dist","MA")]

t_sev = test[, fault_severity]
Test = test[, -c("fault_severity","dist","MA")]


# XGboost matricies
#dtrain <- xgb.DMatrix(as.matrix(Train), label = as.matrix(tr_sev))
#dtest <- xgb.DMatrix(as.matrix(Test), label = as.matrix(t_sev))

#[Tune] Result: booster=gbtree; max_depth=10; min_child_weight=2.79; subsample=0.832; 
#colsample_bytree=0.9; gamma=1.48 : acc.test.mean=0.815

#### XGB 1 ####

#Setting parameters from tuned model
params <- list(booster = "gbtree", 
               num_class = 3,
               objective = "multi:softprob",
               eval_metric = "mlogloss",
               eta=0.1, 
               gamma=1.48, 
               max_depth=10, 
               min_child_weight=2.79, 
               subsample=.832, 
               colsample_bytree=.9)

cv.nround <- 200
cv.nfold <- 10

bst.cv = xgb.cv(param=params, data = data.matrix(Train), label = tr_sev, 
                nfold = cv.nfold, nrounds = cv.nround, print_every_n = 10, early_stop_rounds=100)

info = bst.cv$evaluation_log #test_rmse_mean
nr = which.min(info$test_mlogloss_mean)

# Prediction XGB Model 1
set.seed(757)
model1 = xgboost(param=params, data = data.matrix(Train), label = tr_sev, nrounds=nr, print_every_n = 10)
Pred = predict(model1,data.matrix(Train) )
train_preds1 = matrix(Pred, ncol=3, byrow=TRUE)

Pred = predict(model1,data.matrix(Test) )
test_preds1 = matrix(Pred, ncol=3, byrow=TRUE)

# Prediction XGB Model 2
set.seed(91911)
model2 = xgboost(param=params, data = data.matrix(Train), label = tr_sev, nrounds=nr, print_every_n = 10)
Pred = predict(model2,data.matrix(Train) )
train_preds2 = matrix(Pred, ncol=3, byrow=TRUE)

Pred = predict(model2,data.matrix(Test) )
test_preds2 = matrix(Pred, ncol=3, byrow=TRUE)

# Prediction XGB Model 2
set.seed(11999)
model3 = xgboost(param=params, data = data.matrix(Train), label = tr_sev, nrounds=nr, print_every_n = 10)
Pred = predict(model3,data.matrix(Train) )
train_preds3 = matrix(Pred, ncol=3, byrow=TRUE)

Pred = predict(model3,data.matrix(Test) )
test_preds3 = matrix(Pred, ncol=3, byrow=TRUE)

rm(list = c('model1', 'model2','model3','params','info'))

#### XGB 2 ####
# Including dummies for location
DT2 = inner_join(DT, loc, by = 'id')
DT2 = data.table(DT2)
Train = DT2[train == TRUE, -c('train','id')]
test = DT2[train == FALSE, -c('train','id')]


tr_sev = Train[, fault_severity] 
Train = Train[, -c("fault_severity","dist","MA")]

t_sev = test[, fault_severity]
Test = test[, -c("fault_severity","dist","MA")]

#Setting parameters from tuned model
params <- list(booster = "gbtree", 
               num_class = 3,
               objective = "multi:softprob",
               eval_metric = "mlogloss",
               eta=0.1, 
               gamma=1.48, 
               max_depth=10, 
               min_child_weight=2.79, 
               subsample=.832, 
               colsample_bytree=.9)
set.seed(99909)

cv.nround <- 200
cv.nfold <- 10

bst.cv = xgb.cv(param=params, data = data.matrix(Train), label = tr_sev, 
                nfold = cv.nfold, nrounds = cv.nround, print_every_n = 10, early_stop_rounds=100)

info = bst.cv$evaluation_log #test_rmse_mean
nr = which.min(info$test_mlogloss_mean)

# Prediction XGB Model 4
set.seed(345)
model4 = xgboost(param=params, data = data.matrix(Train), label = tr_sev, nrounds=nr, print_every_n = 10)
Pred = predict(model4,data.matrix(Train) )
train_preds4 = matrix(Pred, ncol=3, byrow=TRUE)

Pred = predict(model4,data.matrix(Test) )
test_preds4 = matrix(Pred, ncol=3, byrow=TRUE)

# Prediction XGB Model 5
set.seed(543)
model5 = xgboost(param=params, data = data.matrix(Train), label = tr_sev, nrounds=nr, print_every_n = 10)
Pred = predict(model5,data.matrix(Train) )
train_preds5 = matrix(Pred, ncol=3, byrow=TRUE)

Pred = predict(model5,data.matrix(Test) )
test_preds5 = matrix(Pred, ncol=3, byrow=TRUE)

# Prediction XGB Model 6
set.seed(734)
model6 = xgboost(param=params, data = data.matrix(Train), label = tr_sev, nrounds=nr, print_every_n = 10)
Pred = predict(model6,data.matrix(Train) )
train_preds6 = matrix(Pred, ncol=3, byrow=TRUE)

Pred = predict(model6,data.matrix(Test) )
test_preds6 = matrix(Pred, ncol=3, byrow=TRUE)


rm(list = c('model4', 'model5','model6','params','info'))


#### RF ####
set.seed(1204)
DT[, fault_severity := as.factor(fault_severity)]
Train = DT[train == TRUE,-c('train','id',"dist","MA")]
Test = DT[train == FALSE,-c('train','id',"dist","MA")]

mtry = round(sqrt(ncol(Train)))
library(randomForest)
set.seed(1204)
rf.fit = randomForest(fault_severity~., data = Train, mtry = mtry, nodesize = 10)
summary(rf.fit)
rf.preds = predict(rf.fit,newdata = Train,type = 'prob')
rf_train_preds1 = data.table(rf.preds)
colnames(rf_train_preds1) <- c('one','two','three')

rf.preds = predict(rf.fit,newdata = Test,type = 'prob')
rf_test_preds1 = data.table(rf.preds)
colnames(rf_test_preds1) <- c('one','two','three')


#### RF ####
set.seed(409)
DT[, fault_severity := as.factor(fault_severity)]
Train = DT[train == TRUE,-c('train','id',"dist","MA")]
Test = DT[train == FALSE,-c('train','id',"dist","MA")]

mtry = round(sqrt(ncol(Train)))
set.seed(409)
rf.fit = randomForest(fault_severity~., data = Train, mtry = mtry, nodesize = 15)
summary(rf.fit)
rf.preds = predict(rf.fit,newdata = Train,type = 'prob')
rf_train_preds2 = data.table(rf.preds)
colnames(rf_train_preds2) <- c('one','two','three')

rf.preds = predict(rf.fit,newdata = Test,type = 'prob')
rf_test_preds2 = data.table(rf.preds)
colnames(rf_test_preds2) <- c('one','two','three')


rm( list = c('rf.fit', 'rf.preds'))


#### Ensemble - Average ####
out_ensemble=data.table((test_preds1 + test_preds2 + test_preds3 +
                          test_preds4 + test_preds5 + test_preds6 +
                           rf_test_preds1 + rf_test_preds2)/8)

test = DT[train == FALSE, c('id')]
submit = data.frame(cbind(test[,1],out_ensemble))
colnames(submit) <- c('id','predict_0','predict_1','predict_2')


write.csv(submit,file = "submit3.csv", row.names = F)



