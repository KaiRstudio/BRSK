# file for variable selection
# we still have to decide which filter we will use 
# probably IV and caret ?

# ----------------------- read pre-processed data
source("BADSproj.R")
# ----------------------- 


# ----------------------- Filter
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 
# ----------------------- WoE Variables: Information Value (IV)

woe.values$IV
filtered <- names(woe.values$IV[woe.values$IV <0.02])

# ----------------------- 




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
fisher_scores <- apply(train[,sapply(train, is.numeric)], 2, fisherScore, train$return)
fisher_scores 
# ----------------------- 




# ----------------------- Categorical: Cramer’s V

# function
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramér V / Phi:")
  return(as.numeric(CV))
}

# apply to categorical variables

  # I think it makes sense to apply WoE to all categorcial variables



# remove those variables that are not predictive regarding IV, Fisher and/or Cramer V

# not predictve: Cramers V < 0.01
# not predictive: fischer_score < 0.015
# <0.02: not predictive; 0.02-0.1: weak, 0.1-0.3: medium, >0.3 strong IV

# ----------------------- 




# ----------------------- Find correlation with caret
# Multicollinearity
library(caret)
numeric_columns = c("price", "tax", "order_date_day", "order_date_month", "delivery_time")
# these columns also contain N/A values --> the option "pairwise.complete.obs" should be used
numeric_columns_correlation = cor(training_data[, numeric_columns], use="pairwise.complete.obs")
numeric_columns_correlation
# works for non-N/A only (remove N/A rows or fill with mean, median, etc)
high_cor_columns = findCorrelation(numeric_columns_correlation)
high_cor_columns

### Add-on: Efficient removal of highly correlated features with carets findCorrelation() ####
# Identify variables which should be efficiently removed so that no
# correlation exceeds threshold
varCorrelation <- cor(train[,sapply(train,is.numeric)]) # Make sure that the target variable is not included
correlatedVars <- caret::findCorrelation(cutoff = 0.5, names = TRUE, x = varCorrelation)
# Report and remove (?) variables
if(length(correlatedVars)>0){
  print(paste0("Highly correlated variables: ", paste(correlatedVars, collapse = " ")))
}
# ----------------------- 



# ----------------------- Drop filtered columns in train sets

train.filtered <- train[,!(names(train) %in% filtered)]
filtered

dropswoe <- c("woe.item_color",
           "woe.user_title",
           "woe.user_state",
           "woe.order_month")
train.woe <- train.woe[,!(names(train.woe) %in% dropswoe)]
names(train.woe) <- c("item_price", "return", "delivery_time", "regorderdiff", "age", "ct_basket_size",
"ct_same_items", "item_id", "item_size", "brand_id", "user_id")

# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 
# ----------------------- End Filter





# ----------------------- Build Models
# ----------------------- # ----------------------- # ----------------------- # ----------------------- # ----------------------- 

if(!require("NeuralNetTools")) install.packages("NeuralNetTools"); library("NeuralNetTools")
if(!require("caret"))         install.packages("caret");        library("caret")

# ----------------------- Random Forest

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
                  method = "rf", ntree = 500, tuneGrid = rf.parms, importance = TRUE,
                  metric = "ROC", trControl = model.control, na.action = na.omit)

# ----------------------- End Random Forest




# ----------------------- Gradient Boosting (Extreme ???)

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
             tuneGrid = xgb.parms, 
             metric = "ROC", trControl = model.control)

# ----------------------- End Gradient Boosting  
  



# ----------------------- Neural Networks

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
nn_caret <- caret::train(BAD~., data = tr,  
                         method = "nnet", maxit = 100, trace = FALSE, # options for nnet function
                         tuneGrid = nn.parms, # parameters to be tested
                         metric = "ROC", trControl = model.control)


# Neural networks work better when the data inputs are on the same scale, e.g. standardized
# Be careful to use the training mean/sd for normalization
normalizer <- caret::preProcess(train, method = c("center", "scale"))
tr <- predict(normalizer, newdata = train)
ts <- predict(normalizer, newdata = test)

nn_raw <- nnet(BAD~., data = train, # the data and formula to be used
               trace = FALSE, maxit = 1000, # general options
               size = 3, # the number of nodes in the model
               decay = 0.001) # regularization parameter similar to lambda in ridge regression

plotnet(nn, max_sp = TRUE)
# ----------------------- End Neural Network



# ----------------------- Logistic Regression

# Regularized Logistic Regression
indep.var <- model.matrix(return~.-1, daten)
dep.var <- daten$return
lasso <- glmnet(x = indep.var, y = dep.var, family = "binomial", standardize = TRUE,
                alpha = 1, nlambda = 100)
yhat["lasso"] <- as.vector( predict(lasso, newx = x, s = 0.001, type = "response") )

# ----------------------- End Logistic Regression





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


