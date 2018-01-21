# file for variable selection
# we still have to decide which filter we will use 
# probably IV and caret ?

# ----------------------- read pre-processed data
source("BADSproj.R")
# ----------------------- 



  
# ----------------------- WoE Variables: Information Value (IV)

woe.values$IV
highIV_col <- names(woe.values$IV[woe.values$IV >0.02])
highIV_col <- c(highIV_col,"return")

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

train.filtered <- train[,highIV_col]
# numerical variables are missing --> need to add
?findCorrelation



# ----------------------- Wrapper Approach

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


