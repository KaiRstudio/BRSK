


# ----------------------- Structure to save results/tune control
modelLib <- list()
yhat <- list()
auc <- list()

tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)
# - Define resampling procedure (here: 5-fold cross validation) -
rdesc <- makeResampleDesc(method="CV", iters=5, stratify=TRUE)
task <- makeClassifTask(data=train.woe, target="return", positive="1")

# ----------------------- 





# ----------------------- Start Tuning
# ----------------------- # ----------------------- # ----------------------- # ----------------------- 

# ----------------------- Start: Random Forest with wrapper
# ---------------------------- Don't use because less good -----
# - Set parameter, task, tune and update learner -
#set.seed(123)
#rf.task <- makeClassifTask(data=rf.train.woe, target="return", positive="1")
#mtry.rfw.set <- sqrt(ncol(rf.train.woe))*c(0.1,0.25,0.5,1,2,4) # c(0,1,2,4,8)
#rfw.parms <- makeParamSet(
# The recommendation for mtry by Breiman is squareroot number of columns
#  makeDiscreteParam("mtry", values= c(0,1,2,4,8)), # Number of features selected at each node, smaller -> faster
#  makeDiscreteParam("ntree", values = c(100, 250, 500, 750, 1000 )) # Number of tree, smaller -> faster
#) 

# mtry from half of squaretoot to 2 times squareroot
# number of bagging iterations 5, 10, 15, 20, 25
# number of trees 100, 200, 
#parallelStartSocket(3, level = "mlr.tuneParams")
#rfw.tuning <- tuneParams(rf, task = rf.task, resampling = rdesc,
#                        par.set = rfw.parms, control = tuneControl, measures = mlr::auc)
#parallelStop()
#rfw.tuning$x
#rfw.tuning <- setHyperPars(rf, par.vals = rfw.tuning$x) 

# - Train, predict, AUC -
#modelLib[["rf_wrap"]] <- mlr::train(rfw.tuning, task = rf.task)
#yhat[["rf_wrap"]] <- predict(modelLib[["rf_wrap"]], newdata = rf.test.woe)
#auc[["rf_wrap"]] <- mlr::performance(yhat[["rf_wrap"]], measures = mlr::auc)
#auc

# ----------------------- End: Random Forest with wrapper




# ----------------------- Start: Random Forest without wrapper - WoE data

# - Set parameter, task, tune and update learner -
set.seed(123)
mtry.set <- sqrt(ncol(train.woe))*c(0.1,0.25,0.5,1,2,4)
rf.parms <- makeParamSet(
  makeDiscreteParam("mtry", values= round(mtry.set)), 
  makeDiscreteParam("ntree", values = c(100, 250, 500, 750, 1000 ))
) 
parallelStartSocket(3, level = "mlr.tuneParams")
rf.tuning <- tuneParams(rf, task = task, resampling = rdesc,
                        par.set = rf.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
rf.tuning$x
rf.tuning <- setHyperPars(rf, par.vals = rf.tuning$x)

# - Train, predict, AUC -
modelLib[["rf"]] <- mlr::train(rf.tuning, task = task)
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = test.woe)
auc[["rf"]] <- mlr::performance(yhat[["rf"]], measures = mlr::auc)
auc

# ----------------------- End: Random Forest without wrapper




# ----------------------- Start: Random Forest without wrapper - Test.2 (with categories)

# - Set parameter, task, tune and update learner -
set.seed(123)
rf.task2 <- makeClassifTask(data=train.2, target="return", positive="1")

parallelStartSocket(3, level = "mlr.tuneParams")
rf.tuning.cat <- tuneParams(rf, task = rf.task2, resampling = rdesc,
                            par.set = rf.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
rf.tuning.cat$x
rf.tuning.cat <- setHyperPars(rf, par.vals = rf.tuning.cat$x)

# - Train, predict, AUC -
modelLib[["rf.cat"]] <- mlr::train(rf.tuning.cat, task = rf.task2)
yhat[["rf.cat"]] <- predict(modelLib[["rf.cat"]], newdata = test.2)
auc[["rf.cat"]] <- mlr::performance(yhat[["rf.cat"]], measures = mlr::auc)
auc

# ----------------------- End: Random Forest with categories without wrapper




# ----------------------- Start: Logistic Regression no wrap

# - Set parameter, task, tune and update learner -
set.seed(123)
lr.parms <- makeParamSet(
  makeNumericParam("s", lower = 0.001, upper =1), 
  makeDiscreteParam("alpha", value = 1)
) 
parallelStartSocket(3, level = "mlr.tuneParams")
lr.tuning <- tuneParams(lr, task = task, resampling = rdesc,
                        par.set = lr.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
lr.tuning$x
lr.tuning <- setHyperPars(lr, par.vals = lr.tuning$x) # necessary or how is tuned data extracted?

# - Train, predict, AUC -
modelLib[["lr"]] <- mlr::train(lr.tuning, task = task)
yhat[["lr"]] <- predict(modelLib[["lr"]], newdata = test.woe)
auc[["lr"]] <- mlr::performance(yhat[["lr"]], measures = mlr::auc)
auc

# ----------------------- End: Logistic Regression



# ----------------------- Start: Logistic Regression no wrap - Test.2 - Categories

# - Set parameter, task, tune and update learner -
set.seed(123)
lr.task.cat <- makeClassifTask(data=train.2, target="return", positive="1")

parallelStartSocket(3, level = "mlr.tuneParams")
lr.tuning.cat <- tuneParams(lr, task = lr.task.cat, resampling = rdesc,
                            par.set = lr.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
lr.tuning.cat$x
lr.tuning.cat <- setHyperPars(lr, par.vals = lr.tuning.cat$x) # necessary or how is tuned data extracted?

# - Train, predict, AUC -
modelLib[["lr.cat"]] <- mlr::train(lr.tuning.cat, task = lr.task.cat)
yhat[["lr.cat"]] <- predict(modelLib[["lr.cat"]], newdata = test.2)
auc[["lr.cat"]] <- mlr::performance(yhat[["lr.cat"]], measures = mlr::auc)
auc

# ----------------------- End: Logistic Regression





# ----------------------- Start: Logistic Regression no wrap - Test.3 - Categories normalized
# --------------------- Don't use because it is not good

# - Set parameter, task, tune and update learner -
#set.seed(123)
#lr.task.cat.norm <- makeClassifTask(data=train.3, target="return", positive="1")

#parallelStartSocket(3, level = "mlr.tuneParams")
#lr.tuning.cat.norm <- tuneParams(lr, task = lr.task.cat.norm, resampling = rdesc,
#                            par.set = lr.parms, control = tuneControl, measures = mlr::auc)
#parallelStop()
#lr.tuning.cat.norm$x
#lr.tuning.cat.norm <- setHyperPars(lr, par.vals = lr.tuning.cat.norm$x) # necessary or how is tuned data extracted?

# - Train, predict, AUC -
#modelLib[["lr.cat.norm"]] <- mlr::train(lr.tuning.cat.norm, task = lr.task.cat.norm)
#yhat[["lr.cat.norm"]] <- predict(modelLib[["lr.cat.norm"]], newdata = test.3)
#auc[["lr.cat.norm"]] <- mlr::performance(yhat[["lr.cat.norm"]], measures = mlr::auc)
#auc

# ----------------------- End: Logistic Regression




# ----------------------- Start: Neural Networks

# - Set parameter, task, tune and update learner -
set.seed(123)
nn.task <- makeClassifTask(data=nn.train.woe, target="return", positive="1")
nn.parms <- makeParamSet(
  makeNumericParam("decay", lower = -8, upper = 0, trafo = function(x) 10^(0.5*x)), 
  makeDiscreteParam("size", values = c(1,2,4,8,16))) 
# number of neurons in the hidden layer  
# decay from 0.0001 to 1
parallelStartSocket(3)
nn.tuning <- tuneParams(nn, task = nn.task, resampling = rdesc,
                        par.set = nn.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
nn <- setHyperPars(nn, par.vals = nn.tuning$x)

# - Train, predict, AUC -
modelLib[["nn"]] <- mlr::train(nn, task = nn.task)
yhat[["nn"]] <- predict(modelLib[["nn"]], newdata = nn.test.woe)
auc[["nn"]] <- mlr::performance(yhat[["nn"]], measure = mlr::auc)
auc

# ----------------------- End: Neural Networks




# ----------------------- Start: Extreme Gradient Boosting

# - Set parameter, task, tune and update learner -
set.seed(123)
xgb.parms <- makeParamSet(
  makeIntegerParam("nrounds", lower= 100,upper = 200), 
  makeDiscreteParam("max_depth", values =c(3,15,25)), 
  makeNumericParam("eta", lower =0.01, upper= 0.1),
  makeNumericParam("gamma", lower =0.01, upper= 0.1),
  makeNumericParam("colsample_bytree", lower =0.6, upper= 1),
  makeIntegerParam("min_child_weight",lower = 1, upper = 7),
  makeNumericParam("subsample", lower =0.6, upper= 1)
)
# choose lambda and alpha randomly - how?
# currently default: lambda = 1, alpha= 0
# eta went until 0.15 in excercise
# nrounds = number of iterations/trees?
parallelStartSocket(3)
xgb.tuning <- tuneParams(xgb, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
xgb.tuning$x
xgb <- setHyperPars(xgb, par.vals=xgb.tuning$x)
# - Train, predict, AUC -
modelLib[["xgb"]] <- mlr::train(xgb, task = task)
yhat[["xgb"]] <- predict(modelLib[["xgb"]], newdata = test.woe)
auc[["xgb"]] <- mlr::performance(yhat[["xgb"]], measures = mlr::auc)
auc

# ----------------------- End: Extreme Gradient Boosting



# ----- Save probability predictions

rf.cat.pred <- predict(modelLib[["rf.cat"]], newdata = test.2, type = "prob")
rf.pred <- predict(modelLib[["rf"]], newdata = test.woe, type = "prob")
lr.pred <- predict(modelLib[["lr"]], newdata = test.woe, type = "prob")
lr.cat.pred <- predict(modelLib[["lr.cat"]], newdata = test.2, type = "prob")
nn.pred <- predict(modelLib[["nn"]], newdata = nn.test.woe, type = "prob")
xgb.pred <- predict(modelLib[["xgb"]], newdata = test.woe, type = "prob")



# ---------------- Variable Importance
getFeatureImportance(rf.cat.pred)
# ... Stuff ...
# to use different kinds of performances:
#mlr::performance(pred_raw, measures = list(mlr::auc, mlr::brier, mlr::acc))
