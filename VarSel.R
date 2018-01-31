# file for variable selection
# we still have to decide which filter we will use 
# probably IV and caret ?

# ----------------------- read pre-processed data
source("BADSproj.R")
# ----------------------- 


dummy <- train.woe

# ----------------------- Packages
if(!require("NeuralNetTools")) install.packages("NeuralNetTools"); library("NeuralNetTools")
if(!require("corrplot"))         install.packages("corrplot");        library("corrplot")
if(!require("glmnet"))         install.packages("glmnet");        library("glmnet")
if(!require("caret"))         install.packages("caret");        library("caret")
if(!require("mlr"))         install.packages("mlr");        library("mlr")
if(!require("parallelMap"))         install.packages("parallelMap");        library("parallelMap")


# maybe not needed packages

# ----------------------- 



# ----------------------- Filter
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 

# ----------------------- Information Value (IV)

woe.values$IV
filtered <- names(woe.values$IV[woe.values$IV <0.02])


woe.values_ids$IV
# ----------------------- --> Should WoE be computed for all variables 




# ----------------------- Remaining Numerical Variables: Fisher-Score

# function
fisherScore <- function (feature, targetVariable)
{
  classMeans <- tapply(feature, targetVariable, mean)
  classStds <- tapply(feature, targetVariable, sd)
  classDiff <- abs(diff(classMeans))
  score <- as.numeric(classDiff/sqrt(sum(classStds^2)))
  return(score)
}

# apply to numeric variables:
fisher_scores_1 <- apply(train.woe[,sapply(train.woe, is.numeric)], 2, fisherScore, train.woe$return)
fisher_scores_1
fisher_scores_2 <- apply(train.2[,sapply(train.2, is.numeric)], 2, fisherScore, train.2$return)
fisher_scores_2 # same for train.3

# ----------------------- 




# ----------------------- Categorical: Cramer’s V
cor.mat <- cor(train.2[, !(names(train.2) == "return")])
corrplot(cor.mat)

## function
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

cv.test(train.2$user_title , train.2$return)
cv.test(train.2$item_color , train.2$return)
cv.test(train.2$user_state , train.2$return)
cv.test(train.2$order_month , train.2$return)
cv.test(train.2$delivery_time , train.2$return)

# - Same for train.3 - 

# apply to categorical variables

# remove those variables that are not predictive regarding IV, Fisher and/or Cramer V
# not predictve: Cramers V < 0.01
# not predictive: fischer_score < 0.015
# <0.02: not predictive; 0.02-0.1: weak, 0.1-0.3: medium, >0.3 strong IV

# ----------------------- 



# ----------------------- Drop filtered columns in train sets
cor.mat <- cor(train.woe[, !(names(train.woe) == "return")])
corrplot(cor.mat)
filtered
#train.filtered <- train[,!(names(train) %in% filtered)]
dropswoe <- c("woe.item_color",
              "woe.user_title",
              "woe.user_state",
              "woe.order_month")
train.woe <- train.woe[,!(names(train.woe) %in% dropswoe)]

### -----------> Drop (User_ID or Return_rate) and brand_id ?
train.woe <- train.woe[,!(names(train.woe) %in% c("customersReturnRate"))]
cor.mat <- cor(train.woe[, !(names(train.woe) == "return")])
corrplot(cor.mat, method = "number")

#Same for test
test.woe <- test.woe[,!(names(test.woe) %in% dropswoe)]
test.woe <- test.woe[,!(names(test.woe) %in% c("customersReturnRate"))]

# And nn
nn.test.woe <- nn.test.woe[,names(nn.test.woe) %in% names(test.woe)]
nn.train.woe <- nn.train.woe[,names(nn.train.woe) %in% names(train.woe)]
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 
# ----------------------- End Filter





# ----------------------- Original models and Wrapper 
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 

gc()
# - Define tasks -
task <- makeClassifTask(data=train.woe, target="return", positive="1")
nn.task <- makeClassifTask(data=nn.train.woe, target="return", positive="1")

# - Start parallel -
library("parallelMap")
parallelStartSocket(3)

set.seed(123)
# - Define learning algorithms -
rf <- makeLearner("classif.randomForest", predict.type="prob", par.vals=list("replace"=TRUE, "importance"=FALSE))
nn <- makeLearner("classif.nnet", predict.type="prob")
lr <- makeLearner("classif.glmnet", predict.type="prob")
#lr <- makeLearner("classif.penalized.lasso", predict.type="prob")
xgb <- makeLearner("classif.xgboost", predict.type="prob")

# - Stop parallel -
parallelStop()

# - Define selection procedure (here: stepwise forward selection) -
featureSearchCtrl <- makeFeatSelControlSequential(method="sfs", alpha = 0.01) 

# - Define resampling procedure (here: 5-fold cross validation) -
rdesc <- makeResampleDesc(method="CV", iters=5, stratify=TRUE)

set.seed(123)

# - Parallel feature selection for all models -
parallelStartSocket(3, level = "mlr.selectFeatures")
#featureSelectionRF <- selectFeatures(rf, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
#featureSelectionNN <- selectFeatures(nn, task=nn.task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
#featureSelectionLR <- selectFeatures(lr, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
#featureSelectionXGB <- selectFeatures(xgb, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
parallelStop()

# - Number of variables in total - 
ncol(task$env$train)

# - Variables selected by different models using treshold alpha -
featureSelectionRF # user_id, item_id, delivery_time
featureSelectionNN # user_id, item_id, delivery_time
featureSelectionLR # user_id, item_id
featureSelectionXGB

# remove irrelevant variables from dataset for each model respectively


featureSelectionRF$y
analyzeFeatSelResult(featureSelectionRF)

rf.train.woe <- train.woe[,(names(train.woe) %in% c("return","woe.user_id", "woe.item_id", "woe.delivery_time"))]
rf.test.woe <- test.woe[,(names(test.woe) %in% c("return","woe.user_id", "woe.item_id", "woe.delivery_time"))]

lr.train.woe <- train.woe[,names(train.woe) %in% c("return","woe.user_id", "woe.item_id")]
lr.test.woe <- test.woe[,names(test.woe) %in% c("return","woe.user_id", "woe.item_id")]

nn.train.woe <- nn.train.woe[, names(nn.train.woe) %in% c("return","woe.user_id", "woe.item_id")]
nn.test.woe <- nn.test.woe[, names(nn.train.woe) %in% c("return","woe.user_id", "woe.item_id")]
