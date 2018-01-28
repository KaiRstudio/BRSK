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

# ----------------------- 
featureSearchCtrl <- makeFeatSelControlSequential(method="sfs", alpha = 0.01) 

# - Define resampling procedure (here: 5-fold cross validation) -
rdesc <- makeResampleDesc(method="CV", iters=5, stratify=TRUE)
task <- makeClassifTask(data=train.woe, target="return", positive="1")


# ----------------------- Start Tuning
# ----------------------- # ----------------------- # ----------------------- # ----------------------- 

# ----------------------- Random Forest

set.seed(123)

rf.task <- makeClassifTask(data=rf.train.woe, target="return", positive="1")

rf.parms <- makeParamSet(
  # The recommendation for mtry by Breiman is squareroot number of columns
  makeIntegerParam("mtry", lower = round(sqrt(ncol(rf.train.woe))/2),
                   upper = round(sqrt(ncol(rf.train.woe))*2)), # Number of features selected at each node, smaller -> faster
  makeIntegerParam("ntree", lower = 100, upper = 1000) # Number of tree, smaller -> faster
) 

# mtry from half of squaretoot to 4 times squareroot
#the space of number of trees in the forest to 100, 250, 500, 750, 1000 
# number of bagging iterations 5, 10, 15, 20, 25

parallelStartSocket(3, level = "mlr.tuneParams")
rf.tuning <- tuneParams(rf, task = rf.task, resampling = rdesc,
                        par.set = rf.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
# Start resampling
rf.tuning$x
rf.tuning <- setHyperPars(rf, par.vals = rf.tuning$x) # necessary or how is tuned data extracted?

# Train the model on the full training data (not only a CV-fold)
modelLib[["rf"]] <- mlr::train(rf.tuning, task = rf.task)

# Make prediction on test data
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = rf.test.woe)


# Calculate AUC performance on test set 
auc[["rf"]] <- mlr::performance(yhat[["rf"]], measures = mlr::auc)
auc
# ----------------------- End: Random Forest




# ----------------------- Start: Logistic Regression

set.seed(123)

lr.task <- makeClassifTask(data=lr.train.woe, target="return", positive="1")

lr.parms <- makeParamSet(
  makeIntegerParam("lambda1", lower = 0, upper =200) 
) 

parallelStartSocket(3, level = "mlr.tuneParams")
lr.tuning <- tuneParams(lr, task = task, resampling = rdesc,
                        par.set = lr.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
lr.tuning$x
lr.tuning <- setHyperPars(lr, par.vals = lr.tuning$x) # necessary or how is tuned data extracted?

# Train the model on the full training data (not only a CV-fold)
modelLib[["lr"]] <- mlr::train(rf.tuning, task = task)

# Make prediction on test data
yhat[["lr"]] <- predict(modelLib[["lr"]], newdata = test.woe)


# Calculate AUC performance on test set 
auc[["lr"]] <- mlr::performance(yhat[["lr"]], measures = mlr::auc)
auc

# ----------------------- End: Logistic Regression




# ----------------------- Start: Extreme Gradient Boosting

set.seed(123)

xgb.parms <- makeParamSet(
  makeIntegerParam("nrounds", lower= 100,upper = 200), 
  makeIntegerParam("max_depth", lower= 3, upper= 10), 
  makeNumericParam("eta", lower = 0.01, upper = 0.5), 
  makeNumericParam("gamma", lower =0, upper= 0.3),
  makeDiscreteParam("colsample_bytree", values = c(0.6, 0.7, 0.8)),
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
# Extract optimal parameter values after tuning 
xgb.tuning$x

# Update the learner to the optimal hyperparameters
xgb <- setHyperPars(xgb, par.vals = c(xgb.tuning$x, "verbose" = 0)) # What is verbose and do we need to do that shit?
xgb

# Train the model on the full training data (not only a CV-fold)
modelLib[["xgb"]] <- mlr::train(xgb, task = task)

# Make prediction on test data
yhat[["xgb"]] <- predict(modelLib[["xgb"]], newdata = test.woe)

# Calculate AUC performance on test set 
auc[["xgb"]] <- mlr::performance(yhat[["xgb"]], measures = mlr::auc)
auc
# ----------------------- End: Extreme Gradient Boosting




# ----------------------- Start: Neural Networks

set.seed(123)

nn.task <- makeClassifTask(data=nn.train.woe, target="return", positive="1")

# Neural networks work better when the data inputs are on the same scale, e.g. standardized
nn.parms <- makeParamSet(
  makeNumericParam("decay", lower = -4, upper = 0, trafo = function(x) 10^x), 
  makeDiscreteParam("size", values = c(1,2,4,8,16))) 
# number of neurons in the hidden layer  
#decay from 0.0001 to 1

parallelStartSocket(3)
nn.tuning <- tuneParams(nn, task = nn.task, resampling = rdesc,
                        par.set = nn.parms, control = tuneControl, measures = mlr::auc)
parallelStop()

nn <- setHyperPars(nn, par.vals = nn.tuning$x)

modelLib[["nn"]] <- mlr::train(nn, task = nn.task)

yhat[["nn"]] <- predict(modelLib[["nn"]], newdata = nn.test.woe)

# to use different kinds of performances:
#mlr::performance(pred_raw, measures = list(mlr::auc, mlr::brier, mlr::acc))
auc[["nn"]] <- mlr::performance(yhat[["nn"]], measure = mlr::auc)
auc
# ----------------------- End: Neural Networks
