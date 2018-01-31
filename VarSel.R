# file for variable selection
# we still have to decide which filter we will use 
# probably IV and caret ?

# ----------------------- read pre-processed data
source("BADSproj.R")
# ----------------------- 



# ----------------------- Packages
if(!require("NeuralNetTools")) install.packages("NeuralNetTools"); library("NeuralNetTools")
if(!require("corrplot"))         install.packages("corrplot");        library("corrplot")
if(!require("glmnet"))         install.packages("glmnet");        library("glmnet")
if(!require("caret"))         install.packages("caret");        library("caret")
if(!require("mlr"))         install.packages("mlr");        library("mlr")
if(!require("parallelMap"))         install.packages("parallelMap");        library("parallelMap")
# ----------------------- 



# - Apply Filter and Wrapper and exclude not necessary data -
# Benchmarks: Cramers V < 0.01, Fisher score < 0.015, IV <0.02



# ----------------------- Start: Define functions

# - Fisher Score -
fisherScore <- function (feature, targetVariable)
{
  classMeans <- tapply(feature, targetVariable, mean)
  classStds <- tapply(feature, targetVariable, sd)
  classDiff <- abs(diff(classMeans))
  score <- as.numeric(classDiff/sqrt(sum(classStds^2)))
  return(score)
}


# - Cramer's V -
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}
# ----------------------- End: Define functions




# ----------------------- Start: Information Value (IV)

# - WoE only data
woe.values$IV
filtered <- names(woe.values$IV[woe.values$IV <0.02])

# - WoE and categoricals data
woe.values_ids$IV

# ----------------------- End: Information Value (IV)




# ----------------------- Start: Fisher-Score
# - For numerical variables -
# - Normalized data has same values -

fisher_scores_1 <- apply(train.woe[,sapply(train.woe, is.numeric)], 2, fisherScore, train.woe$return)
fisher_scores_1
fisher_scores_2 <- apply(train.2[,sapply(train.2, is.numeric)], 2, fisherScore, train.2$return)
fisher_scores_2

# ----------------------- End: Fisher-Score




# ----------------------- Start: Cramer’s V
# - For categorical variables -

cv.test(train.2$user_title , train.2$return)
cv.test(train.2$item_color , train.2$return)
cv.test(train.2$user_state , train.2$return)
cv.test(train.2$order_month , train.2$return)
cv.test(train.2$delivery_time , train.2$return)

# ----------------------- End: Cramer's V




# ----------------------- Drop not important variables

dropswoe <- c("woe.item_color",
              "woe.user_title",
              "woe.user_state",
              "woe.order_month")
train.woe <- train.woe[,!(names(train.woe) %in% dropswoe)]
test.woe <- test.woe[,!(names(test.woe) %in% dropswoe)]

# Same for normalized data
nn.train.woe <- nn.train.woe[,names(nn.train.woe) %in% names(train.woe)]
nn.test.woe <- nn.test.woe[,names(nn.test.woe) %in% names(test.woe)]

# - No Drops for other sets -
# ----------------------- End: Drop not important variables




# ----------------------- Start: Build models 

# - Use parallel computation -
parallelStartSocket(3)
set.seed(123)
rf <- makeLearner("classif.randomForest", predict.type="prob", par.vals=list("replace"=TRUE, "importance"=TRUE))
nn <- makeLearner("classif.nnet", predict.type="prob")
lr <- makeLearner("classif.glmnet", predict.type="prob")
xgb <- makeLearner("classif.xgboost", predict.type="prob")
parallelStop()

# - Set configuration for models - 
task <- makeClassifTask(data=train.woe, target="return", positive="1")
nn.task <- makeClassifTask(data=nn.train.woe, target="return", positive="1")

# - Use stepwise forward selection -
featureSearchCtrl <- makeFeatSelControlSequential(method="sfs", alpha = 0.01) 

# - Use 5-fold cross validation -
rdesc <- makeResampleDesc(method="CV", iters=5, stratify=TRUE)

# ----------------------- End: Build models




# ----------------------- Start: Wrapper
# - Wrapper for rf, lr, xgb not needed and show less good results -
# - Therefore only neural network wrapper was used for final data - 

set.seed(123)
parallelStartSocket(3, level = "mlr.selectFeatures")
# featureSelectionRF <- selectFeatures(rf, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
featureSelectionNN <- selectFeatures(nn, task=nn.task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
# featureSelectionLR <- selectFeatures(lr, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
# featureSelectionXGB <- selectFeatures(xgb, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
parallelStop()

# - Show wrapper results -
# featureSelectionRF # user_id, item_id, delivery_time
featureSelectionNN # user_id, item_id, delivery_time
# featureSelectionLR # user_id, item_id
# featureSelectionXGB

# ----------------------- End: Wrapper


# ----------------------- Adjust datasets based on wrapper
# - Since wrapped models have shown less good performance the needed code for wrapped rf/lr/xgb
# - is written as comment -

# rf.train.woe <- train.woe[,(names(train.woe) %in% c("return","woe.user_id", "woe.item_id", "woe.delivery_time"))]
# rf.test.woe <- test.woe[,(names(test.woe) %in% c("return","woe.user_id", "woe.item_id", "woe.delivery_time"))]

# lr.train.woe <- train.woe[,names(train.woe) %in% c("return","woe.user_id", "woe.item_id")]
# lr.test.woe <- test.woe[,names(test.woe) %in% c("return","woe.user_id", "woe.item_id")]

nn.train.woe <- nn.train.woe[, names(nn.train.woe) %in% c("return","woe.user_id", "woe.item_id")]
nn.test.woe <- nn.test.woe[, names(nn.train.woe) %in% c("return","woe.user_id", "woe.item_id")]
