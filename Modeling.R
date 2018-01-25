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


#Structure to save results
# A number of models
modelLib <- list()
# Test set predictions
yhat <- list()
# AUC performance for each model
auc <- list()


tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)


# ----------------------- Random Forest
rf.parms <- makeParamSet(
  # The recommendation for mtry by Breiman is squareroot number of columns
  makeIntegerParam("mtry", lower = 2, upper = 6), # Number of features selected at each node, smaller -> faster
  makeDiscreteParam("sampsize", values = c(300, 200)), # bootstrap sample size, smaller -> faster
  makeIntegerParam("ntree", lower = 200, upper = 1000) # Number of tree, smaller -> faster
) 

parallelStartSocket(3, level = "mlr.tuneParams")
rf.tuning <- tuneParams(rf, task = task, resampling = rdesc,
                     par.set = rf.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
# Start resampling

rf.tuning <- setHyperPars(rf, par.vals = rf.tuning$x) # necessary or how is tuned data extracted?

# Train the model on the full training data (not only a CV-fold)
modelLib[["rf"]] <- mlr::train(rf.tuning, task = task)

# Make prediction on test data
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = test.woe)
str(yhat[["rf"]])

# Calculate AUC performance on test set 
auc[["rf"]] <- mlr::performance(yhat[["rf"]], measures = mlr::auc)

# ----------------------- End: Random Forest




# ----------------------- Start: Logistic Regression


# ----------------------- End: Logistic Regression




# ----------------------- Start: Extreme Gradient Boosting
xgb.parms <- makeParamSet(
  makeDiscreteParam("nrounds", values = c(20, 100, 200)), 
  makeDiscreteParam("max_depth", values = c(2, 4)), 
  makeDiscreteParam("eta", values = c(0.01, 0.05, 0.1, 0.15)), 
  makeDiscreteParam("gamma", values = 0),
  makeDiscreteParam("colsample_bytree", values = 0.8),
  makeDiscreteParam("min_child_weight", values = 1),
  makeDiscreteParam("subsample", values = 0.8)
)
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

# ----------------------- End: Extreme Gradient Boosting




# ----------------------- Start: Neural Networks

# Neural networks work better when the data inputs are on the same scale, e.g. standardized
nn.parms <- makeParamSet(
  makeNumericParam("decay", lower = -4, upper = 0, trafo = function(x) 10^x), #decay from 0.0001 to 1
  makeDiscreteParam("size", values = c(seq(3, 15, 3)))) #size from 3 to 15 

parallelStartSocket(3, level = "mlr.tuneParams")
nn.tuning <- tuneParams(nn, task = nn.task, resampling = rdesc,
                        par.set = nn.parms, control = tuneControl, measures = mlr::auc)
parallelStop()

nn <- setHyperPars(nn, par.vals = nn.tuning$x)

modelLib[["nn"]] <- mlr::train(nn, task = nn.task)

yhat[["nn"]] <- predict(modelLib[["nn"]], newdata = test.woe)

# to use different kinds of performances:
#mlr::performance(pred_raw, measures = list(mlr::auc, mlr::brier, mlr::acc))
auc[["nn"]] <- mlr::performance(yhat[["nn"]], measure = mlr::auc)

# ----------------------- End: Neural Networks
