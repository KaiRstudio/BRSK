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

# maybe not needed packages
if(!require("level.stat"))         install.packages("level.stat");        library("level.stat")
if(!require("gstat"))         install.packages("gstat");        library("gstat")
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")
# ----------------------- 



# ----------------------- Filter
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 

# ----------------------- Information Value (IV)

woe.values$IV
filtered <- names(woe.values$IV[woe.values$IV <0.02])

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
fisher_scores <- apply(train.woe[,sapply(train.woe, is.numeric)], 2, fisherScore, train.woe$return)
fisher_scores 
# ----------------------- 




# ----------------------- Categorical: Cramer’s V
cor.mat <- cor(train.woe[, !(names(train.woe) == "return")])
corrplot(cor.mat)

## function
#cv.test = function(x,y) {
#  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
#              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
#  print.noquote("Cramér V / Phi:")
#  return(as.numeric(CV))
#}

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
train.woe <- train.woe[,!(names(train.woe) %in% c("woe.brand_id", "customersReturnRate"))]
cor.mat <- cor(train.woe[, !(names(train.woe) == "return")])
corrplot(cor.mat, method = "number")

#Same for test
test.woe <- test.woe[,!(names(test.woe) %in% dropswoe)]
test.woe <- test.woe[,!(names(test.woe) %in% c("woe.brand_id", "customersReturnRate"))]
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 
# ----------------------- End Filter




# ----------------------- Start: Prep Input for NN

# Neural networks work better when the data inputs are on the same scale, e.g. standardized
# Be careful to use the training mean/sd for normalization
normalizer <- caret::preProcess(nn.train.woe[,names(nn.train.woe) %in% c("item_price","regorderdiff","age","ct_basket_size","ct_same_items")],
                                method = c("center", "scale"))
nn.train.woe <- predict(normalizer, newdata = train.woe)
nn.test.woe <- predict(normalizer, newdata = test.woe)

# - Adjust return values for Neural Network
nn.train.woe$return <- as.factor(ifelse(nn.train.woe$return == 1, 1, -1))
nn.test.woe$return <- as.factor(ifelse(nn.test.woe$return == 1, 1, -1))

# ----------------------- End: Prep Input for NN




# ----------------------- Original models and Wrapper 
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 

library(mlr)
gc()
# depending on the stage of preprocessing: delete variables that do not contribute to a logical understanding
# test.woe <- test.woe[, -c(1, 2, 3, 4, 6, 7, 8)]

# define task
task <- makeClassifTask(data=train.woe, target="return", positive="1")
nn.task <- makeClassifTask(data=nn.train.woe, target="return", positive="1")

# Start parallel
library("parallelMap")
parallelStartSocket(3)

# define learning algorithms
rf <- makeLearner("classif.randomForest", predict.type="prob", par.vals=list("replace"=TRUE, "importance"=FALSE))
nn <- makeLearner("classif.neuralnet", predict.type="prob") #par.vals = list("trace" = FALSE, "maxit" = 400) ?
lr <- makeLearner("classif.penalized.lasso", predict.type="prob")
xgb <- makeLearner("classif.xgboost", predict.type="prob") #par.vals = list("verbose" = 1) hinzufügen?

# Stop parallel
parallelStop()

# define selection procedure (here: stepwise forward selection)
featureSearchCtrl <- makeFeatSelControlSequential(method="sfs", alpha = 0.01) 

# define resampling procedure (here: 5-fold cross validation)
rdesc <- makeResampleDesc(method="CV", iters=5, stratify=TRUE)


parallelStartSocket(3, level = "mlr.selectFeatures")
# feature selection for all models 
featureSelectionRF <- selectFeatures(rf, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
featureSelectionNN <- selectFeatures(nn, task=nn.task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
featureSelectionLR <- selectFeatures(lr, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
featureSelectionXGB <- selectFeatures(xgb, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)

parallelStop()

# Number of variables in total
ncol(task$env$train)

# Variables selected by different models using treshold alpha
featureSelectionRF 
featureSelectionNN 
featureSelectionLR 
featureSelectionXGB

# remove irrelevant variables from dataset for each model respectively


featureSelectionRF$y
analyzeFeatSelResult(featureSelectionRF)
