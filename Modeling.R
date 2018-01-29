# file for building models
# For each model:
# Parameter optimization (=Loops over parameters of model)
# Loops over choice of variables
# Training/Testing    
# data splitting, model prediction, cross validation


# ----------------------- Read Data Prep
source("VarSel.R")
# ----------------------- 




# ----------------------- Packages
if(!require("NeuralNetTools")) install.packages("NeuralNetTools"); library("NeuralNetTools")
if(!require("caret"))         install.packages("caret");        library("caret")
if(!require("mlr"))         install.packages("mlr");        library("mlr")

if(!require("nnet")) install.packages("nnet"); library("nnet")


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

# - Set parameter, task, tune and update learner -
set.seed(123)
rf.task <- makeClassifTask(data=rf.train.woe, target="return", positive="1")
rfw.parms <- makeParamSet(
  # The recommendation for mtry by Breiman is squareroot number of columns
  makeIntegerParam("mtry", lower = round(sqrt(ncol(rf.train.woe))/2),
                   upper = round(sqrt(ncol(rf.train.woe))*2)), # Number of features selected at each node, smaller -> faster
  makeIntegerParam("ntree", lower = 100, upper = 1000) # Number of tree, smaller -> faster
) 

# mtry from half of squaretoot to 2 times squareroot
# number of bagging iterations 5, 10, 15, 20, 25
# number of trees 100, 200, 
parallelStartSocket(3, level = "mlr.tuneParams")
rfw.tuning <- tuneParams(rf, task = rf.task, resampling = rdesc,
                        par.set = rfw.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
rfw.tuning$x
rfw.tuning <- setHyperPars(rf, par.vals = rfw.tuning$x) 

# - Train, predict, AUC -
modelLib[["rf_wrap"]] <- mlr::train(rfw.tuning, task = rf.task)
yhat[["rf_wrap"]] <- predict(modelLib[["rf_wrap"]], newdata = rf.test.woe)
auc[["rf_wrap"]] <- mlr::performance(yhat[["rf_wrap"]], measures = mlr::auc)
auc

# ----------------------- End: Random Forest with wrapper




# ----------------------- Start: Random Forest without wrapper - WoE data

# - Set parameter, task, tune and update learner -
set.seed(123)
rf.parms <- makeParamSet(
  makeDiscreteParam("mtry", values= (sqrt(ncol(train.woe))*c(0.1,0.25,0.5,1,2,4))), 
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
rf_cat.tuning <- tuneParams(rf, task = rf.task2, resampling = rdesc,
                        par.set = rf.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
rf_cat.tuning$x
rf_cat.tuning <- setHyperPars(rf, par.vals = rf_cat.tuning$x)

# - Train, predict, AUC -
modelLib[["rf.cat"]] <- mlr::train(rf.tuning, task = rf.task2)
yhat[["rf.cat"]] <- predict(modelLib[["rf.cat"]], newdata = test.2)
auc[["rf.cat"]] <- mlr::performance(yhat[["rf.cat"]], measures = mlr::auc)
auc

# ----------------------- End: Random Forest without wrapper




# ----------------------- Start: Logistic Regression no wrap

# - Set parameter, task, tune and update learner -
set.seed(123)
lr.parms <- makeParamSet(
  makeNumericParam("s", lower = 0.001, upper =1) 
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




# ----------------------- Start: Neural Networks

# - Set parameter, task, tune and update learner -
set.seed(123)
nn.task <- makeClassifTask(data=nn.train.woe, target="return", positive="1")
nn.parms <- makeParamSet(
  makeNumericParam("decay", lower = -4, upper = 0, trafo = function(x) 10^x), 
  makeDiscreteParam("size", values = c(1,2,4,8,16)),
  makeIntegerParam("maxit", lower= 200, upper=800)) 
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
  makeIntegerParam("max_depth", lower= 3, upper= 10), 
  makeNumericParam("eta", lower = 0.01, upper = 0.5), 
  makeNumericParam("gamma", lower =0, upper= 0.3),
  makeNumericParam("colsample_bytree", lower= 0.6, upper= 0.8),
  makeNumericParam("min_child_weight",lower=0.8,upper=2),
  makeNumericParam("subsample", lower= 0.8, upper=1)
)
# choose lambda and alpha randomly - how?
# currently default: lambda = 1, alpha= 0
# eta went until 0.15 in excercise
# nrounds = number of iterations/trees?
parallelStartSocket(3, level = "mlr.tuneParams")
xgb.tuning <- tuneParams(xgb, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
xgb.tuning$x
xgb <- setHyperPars(xgb, par.vals = c(xgb.tuning$x, "verbose" = 0)) # What is verbose and do we need to do that shit?

# - Train, predict, AUC -
modelLib[["xgb"]] <- mlr::train(xgb, task = task)
yhat[["xgb"]] <- predict(modelLib[["xgb"]], newdata = test.woe)
auc[["xgb"]] <- mlr::performance(yhat[["xgb"]], measures = mlr::auc)
auc

# ----------------------- End: Extreme Gradient Boosting


# ... Stuff ...
# to use different kinds of performances:
#mlr::performance(pred_raw, measures = list(mlr::auc, mlr::brier, mlr::acc))
