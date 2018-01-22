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
#test.woe <- test.woe[,!(names(test.woe) %in% c("woe.brand_id", "customersReturnRate"))]
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 
# ----------------------- End Filter





# ----------------------- Build Models
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 

# - Adapt return factors for prediction
#train.woe$return<-as.factor(ifelse(train.woe$return == 1, "Return", "No.Return"))

# ----------------------- Start: Random Forest
gc() # clean cache
yhat <- list()

# ---------- simple model
set.seed(123)
rf <- train (return~., data = train.woe,
             method = "rf",
             ntree = 500,
             na.action = na.omit)


# ----------- complex model
k <- 5 # Set number of cross validation
set.seed(123)
# - Set controls for rf
model.control <- trainControl(
  method = "cv", 
  number = k, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary, 
  allowParallel = TRUE 
)
#set search grid
rf.parms <- expand.grid(mtry = 1:10)
# Train random forest rf with a 5-fold cross validation 
# ntree set as default value 500
rf.caret <- train(return~., data = train.woe,
                  method = "rf", 
                  ntree = 500, 
                  tuneGrid = rf.parms, 
                  importance = TRUE,
                  metric = "ROC", 
                  trControl = model.control, 
                  na.action = na.omit)


yhat[["rf"]] <- predict(rf, test.woe, type="prob")[,2]

# ----------------------- End: Random Forest



# ----------------------- Start: Logistic Regression

x.tr <- model.matrix(return~.-1, train.woe)
y.tr <- train.woe$return
lasso <- glmnet(x = x.tr, y = y.tr, family = "binomial", standardize = TRUE,
                alpha = 1, nlambda = 100)
newx.tr <- model.matrix(return~.-1, test.woe)
yhat[["lasso"]] <- as.vector( predict(lasso, newx = newx.tr, s = 0.001, type = "response") )
# ----------------------- End: Logistic Regression




# ----------------------- Start: Neural Networks
# - Adjust return values for Neural Network
nn.train.woe$return <- as.factor(ifelse(nn.train.woe$return == 1, 1, -1))
nn.test.woe$return <- as.factor(ifelse(nn.test.woe$return == 1, 1, -1))

# Neural networks work better when the data inputs are on the same scale, e.g. standardized
# Be careful to use the training mean/sd for normalization
normalizer <- caret::preProcess(nn.train.woe[,names(nn.train.woe) %in% c("item_price","regorderdiff","age","ct_basket_size","ct_same_items")],
                                method = c("center", "scale"))

nn.train.woe <- predict(normalizer, newdata = train.woe)
nn.test.woe <- predict(normalizer, newdata = test.woe)


model.control<- trainControl(
  method = "cv", 
  number = 5, 
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, 
  returnData = FALSE )

# Define a search grid of values to test
nn.parms <- expand.grid("decay" = 0.001, 
                        "size" = seq(3, 15, 3))

#Train neural network nn with 5-fold cv
set.seed(123)
nn_caret <- caret::train(return~., data = nn.train.woe,  
                         method = "nnet", maxit = 100, trace = FALSE, # options for nnet function
                         tuneGrid = nn.parms, # parameters to be tested
                         metric = "ROC", trControl = model.control)




nn_raw <- nnet(BAD~., data = train, # the data and formula to be used
               trace = FALSE, maxit = 1000, # general options
               size = 3, # the number of nodes in the model
               decay = 0.001) # regularization parameter similar to lambda in ridge regression

plotnet(nn, max_sp = TRUE)
# ----------------------- End: Neural Network



# ----------------------- Start: (Extreme) Gradient Boosting

model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = TRUE)

#set search grid
xgb.parms <- expand.grid(nrounds = c(20, 40, 60, 80), 
                         max_depth = c(2, 4), 
                         eta = c(0.01, 0.05, 0.1, 0.15), 
                         gamma = 0,
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)
# Train model
xgb <- train(return_customer~., data = train,  
             method = "xgbTree",
 #            tuneGrid = xgb.parms, 
             metric = "ROC", trControl = model.control)

# ----------------------- End: Gradient Boosting  



# First Accuracy check
# AUC & ROC curve
yhat.df <- data.frame(yhat) 
h <- HMeasure( as.numeric(test.woe$return)-1, yhat.df, severity.ratio = 0.1) 
plotROC(h, which = 1) 
auc_testset <- h$metrics['AUC']
auc_testset









# ----------------------- Wrapper Approach
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 

library(mlr)

# depending on the stage of preprocessing: delete variables that do not contribute to a logical understanding
# test.woe <- test.woe[, -c(1, 2, 3, 4, 6, 7, 8)]


# define task
task <- makeClassifTask(data=train.filtered, target="return", positive="1")

# define learning algorithms
rf <- makeLearner("classif.randomForest", predict.type="prob", par.vals=list("replace"=TRUE, "importance"=FALSE))
nn <- makeLearner("classif.neuralnet", predict.type="prob")
lr <- makeLearner("classif.penalized.lasso", predict.type="prob")
xgb <- makeLearner("classif.xgboost", predict.type="prob")

# define selection procedure (here: stepwise forward selection)
featureSearchCtrl <- makeFeatSelControlSequential(method="sfs", alpha = 0.01) 

# define resampling procedure (here: 5-fold cross validation)
rdesc <- makeResampleDesc(method="CV", iters=5, stratify=TRUE)

# feature selection for all models 
featureSelectionRF <- selectFeatures(rf, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
featureSelectionNN <- selectFeatures(nn, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
featureSelectionLR <- selectFeatures(lr, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
featureSelectionXGB <- selectFeatures(xgb, task=task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)

# Number of variables in total
ncol(task$env$train)

# Variables selected by different models using treshold alpha
featureSelectionRF 
featureSelectionNN 
featureSelectionLR 
featureSelectionXGB

# remove irrelevant variables from dataset for each model respectively


