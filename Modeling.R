
# -------------------------------- Parameter Tuning and Performance ----------------------------
# - Tune parameter based on literature, AUC performance -

# *** Some data show less good performance for some modells and are written as comments in the following code ***
# *** This includes the normalized data for all models except NN and the complete WoE dataset for other models than RF ***


# ----------------------- Structure to save results
modelLib <- list()
yhat <- list()
auc <- list()




# ----------------------- Set cross validation, control grid and tasks
tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)
# - 5-fold cross validation -
rdesc <- makeResampleDesc(method="CV", iters=5, stratify=TRUE)

# ---- Tasks for each dataset ----
task <- makeClassifTask(data=train.woe, target="return", positive="1")
rf.task2 <- makeClassifTask(data=train.2, target="return", positive="1")
nn.task <- makeClassifTask(data=nn.train.woe, target="return", positive="1")
# - lr.task.cat.norm <- makeClassifTask(data=train.3, target="return", positive="1") 
lr.task.cat <- makeClassifTask(data=train.2, target="return", positive="1")

# ----------------------- 




# ----------------------- Start: Random Forest - WoE data

# - Set parameter, task, tune and update learner -
set.seed(132)
mtry.set <- round(sqrt(ncol(train.woe))*c(0.1,0.25,0.5,1,2,4))
rf.parms <- makeParamSet(
  makeDiscreteParam("mtry", values= c(1,2,3,7)), 
  makeDiscreteParam("ntree", values = c(500, 750, 1000 ))
) 
parallelStartSocket(cores, level = "mlr.tuneParams")
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
set.seed(133)

rf.tuneControl <-  makeTuneControlGrid(resolution = 4, tune.threshold = FALSE)
rf.parms <- makeParamSet(
  makeDiscreteParam("mtry", values= c(1,2,3,7)), 
  makeDiscreteParam("ntree", values = c(500, 750))
) 
parallelStartSocket(cores, level = "mlr.tuneParams")
rf.tuning.cat <- tuneParams(rf, task = rf.task2, resampling = rdesc,
                            par.set = rf.parms, control = rf.tuneControl, measures = mlr::auc)
parallelStop()
rf.tuning.cat$x
rf.tuning.cat <- setHyperPars(rf, par.vals = rf.tuning.cat$x)

# - Train, predict, AUC -
modelLib[["rf.cat"]] <- mlr::train(rf.tuning.cat, task = rf.task2)
yhat[["rf.cat"]] <- predict(modelLib[["rf.cat"]], newdata = test.2)
auc[["rf.cat"]] <- mlr::performance(yhat[["rf.cat"]], measures = mlr::auc)
auc

# ----------------------- End: Random Forest with categories without wrapper




# ----------------------- Start: Logistic Regression - WoE Data
# --------------------- Less good performance --> excluded in the following

# - Set parameter, task, tune and update learner -
#set.seed(134)
#lr.parms <- makeParamSet(
#  makeDiscreteParam("s", values = c(0.001, 0.01, 0.1, 0.5,1)), 
#  makeDiscreteParam("alpha", value = 1)
#) 
#parallelStartSocket(cores, level = "mlr.tuneParams")
#lr.tuning <- tuneParams(lr, task = task, resampling = rdesc,
#                        par.set = lr.parms, control = rf.tuneControl, measures = mlr::auc)
#parallelStop()
#lr.tuning <- setHyperPars(lr, par.vals = lr.tuning$x) 
#
## - Train, predict, AUC -
#modelLib[["lr"]] <- mlr::train(lr.tuning, task = task)
#yhat[["lr"]] <- predict(modelLib[["lr"]], newdata = test.woe)
#auc[["lr"]] <- mlr::performance(yhat[["lr"]], measures = mlr::auc)
#auc

# ----------------------- End: Logistic Regression



# ----------------------- Start: Logistic Regression - Test.2 - Categories

# - Set parameter, task, tune and update learner -
set.seed(135)

parallelStartSocket(cores, level = "mlr.tuneParams")
lr.tuning.cat <- tuneParams(lr, task = lr.task.cat, resampling = rdesc,
                            par.set = lr.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
lr.tuning.cat <- setHyperPars(lr, par.vals = lr.tuning.cat$x)

# - Train, predict, AUC -
modelLib[["lr.cat"]] <- mlr::train(lr.tuning.cat, task = lr.task.cat)
yhat[["lr.cat"]] <- predict(modelLib[["lr.cat"]], newdata = test.2)
auc[["lr.cat"]] <- mlr::performance(yhat[["lr.cat"]], measures = mlr::auc)
auc

# ----------------------- End: Logistic Regression





# ----------------------- Start: Logistic Regression no wrap - Test.3 - Categories normalized
# --------------------- Less good performance --> excluded in the following

# - Set parameter, task, tune and update learner -
#set.seed(136)

#parallelStartSocket(cores, level = "mlr.tuneParams")
#lr.tuning.cat.norm <- tuneParams(lr, task = lr.task.cat.norm, resampling = rdesc,
#                            par.set = lr.parms, control = tuneControl, measures = mlr::auc)
#parallelStop()
#lr.tuning.cat.norm <- setHyperPars(lr, par.vals = lr.tuning.cat.norm$x) # necessary or how is tuned data extracted?

# - Train, predict, AUC -
#modelLib[["lr.cat.norm"]] <- mlr::train(lr.tuning.cat.norm, task = lr.task.cat.norm)
#yhat[["lr.cat.norm"]] <- predict(modelLib[["lr.cat.norm"]], newdata = test.3)
#auc[["lr.cat.norm"]] <- mlr::performance(yhat[["lr.cat.norm"]], measures = mlr::auc)
#auc

# ----------------------- End: Logistic Regression




# ----------------------- Start: Neural Networks

# - Set parameter, task, tune and update learner -
set.seed(137)
nn.parms <- makeParamSet(
  makeDiscreteParam("decay", values = c(0.0001, 0.001, 0.01, 0.1)), 
  makeDiscreteParam("size", values = c(1,4,8,16)))
parallelStartSocket(cores)
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




# ----------------------- Start: Extreme Gradient Boosting only WoE

# - Set parameter, task, tune and update learner -
set.seed(138)
xgb.parms <- makeParamSet(
  makeDiscreteParam("nrounds", values = c(100,125)), 
  makeDiscreteParam("max_depth", values =c(3,15)), 
  makeDiscreteParam("eta", values = c(0.01,0.1)),
  makeDiscreteParam("gamma", values = c(0.01,0.1)),
  makeDiscreteParam("colsample_bytree", values = c(0.75,1)),
  makeDiscreteParam("min_child_weight",values = c(1,4,7)),
  makeDiscreteParam("subsample", values = c(0.75,1))
)
parallelStartSocket(cores)
xgb.tuning <- tuneParams(xgb, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc)
parallelStop()
xgb <- setHyperPars(xgb, par.vals=xgb.tuning$x)

# - Train, predict, AUC -
modelLib[["xgb"]] <- mlr::train(xgb, task = task)
yhat[["xgb"]] <- predict(modelLib[["xgb"]], newdata = test.woe)
auc[["xgb"]] <- mlr::performance(yhat[["xgb"]], measures = mlr::auc)
auc

# ----------------------- End: Extreme Gradient Boosting only WoE





# ----------------------- Save probability predictions one by one

rf.cat.pred <- predict(modelLib[["rf.cat"]], newdata = test.2, type = "prob")
rf.pred <- predict(modelLib[["rf"]], newdata = test.woe, type = "prob")
#lr.pred <- predict(modelLib[["lr"]], newdata = test.woe, type = "prob")
lr.cat.pred <- predict(modelLib[["lr.cat"]], newdata = test.2, type = "prob")
nn.pred <- predict(modelLib[["nn"]], newdata = nn.test.woe, type = "prob")
xgb.pred <- predict(modelLib[["xgb"]], newdata = test.woe, type = "prob")



# ---------------- Variable Importance

featureImportance <- list()

featureImportance[["rf"]] <- unlist(getFeatureImportance(modelLib$rf, type = 1)$res)
featureImportance[["rf.cat"]] <- unlist(getFeatureImportance(modelLib$rf.cat, type = 1)$res)
featureImportance[["xgb"]] <- unlist(getFeatureImportance(modelLib$xgb)$res)


# - Plot final model -
windowsFonts(times = windowsFont("Times New Roman")) 
par(family = "times", font = 2, font.lab = 1, font.axis = 1)
par(mar = c(4,6,2,2) + 0.1)
barplot(featureImportance$rf.cat[c(order(featureImportance$rf.cat))],
        horiz = TRUE,
         col = "peachpuff",
         xlab = "Mean Decrease in Accuracy",
          las = 1,
        xlim = c(0,350)
)
