library(xgboost)
library(readr)
library(caret)
library(mlr)
library(parallel)
library(parallelMap) 
library(car)

train = cbind(Train,tr_sev)
train[, tr_sev := as.factor(tr_sev)]

Traintask <- makeClassifTask (data = train,target = "tr_sev")

lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="multi:softprob", eval_metric="mlogloss", nrounds=200L, eta=0.1)

params <- makeParamSet( makeDiscreteParam("booster",values = "gbtree"), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                        makeNumericParam("gamma",lower = 0L,upper = 2L))

rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
ctrl <- makeTuneControlRandom(maxit = 15L)

library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())


# Tuning parameters
mytune <- tuneParams(learner = lrn, 
                     task = Traintask, 
                     resampling = rdesc,
                     measures = acc, 
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)

#Result: booster=gbtree; max_depth=8; min_child_weight=7.16; subsample=0.959; 
#colsample_bytree=0.943; gamma=3.9 : acc.test.mean=0.703

rm(list = c('ctrl','lrn','params','rdesc','mytune','Traintask','Validtask'))  