
# -------------------------------- Variable Selection ----------------------------
# - Apply Filter (Cramer, Fisher, IV) and Wrapper and exclude not necessary data -


# ----------------------- Set Number of cores
cores <- max(1,detectCores()-1)


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




# ----------------------- Start: Filter
# - Benchmarks: Cramers V < 0.01, Fisher score < 0.015, IV <0.02 -

# ----------------------- Information Value (IV)

# - WoE only data
woe.values$IV
filtered <- names(woe.values$IV[woe.values$IV <0.02])

# - WoE and categoricals data
woe.values_ids$IV

# ----------------------- End: Information Value (IV)



# ----------------------- Start: Fisher-Score
# - For numerical variables -

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
cv.test(train.2$item_size , train.2$return)

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

# ----------------------- End: Drop not important variables

# ----------------------- End: Filter




# ----------------------- Start: Build models 

# - Use parallel computation -
parallelStartSocket(cores)
rf <- makeLearner("classif.randomForest", predict.type="prob", par.vals=list("replace"=TRUE, "importance"=TRUE))
nn <- makeLearner("classif.nnet", predict.type="prob")
lr <- makeLearner("classif.glmnet", predict.type="prob")
xgb <- makeLearner("classif.xgboost", predict.type="prob")
parallelStop()

# - Set tasks for wrapper - 
task <- makeClassifTask(data=train.woe, target="return", positive="1")
nn.task <- makeClassifTask(data=nn.train.woe, target="return", positive="1")

# - Stepwise forward selection -
featureSearchCtrl <- makeFeatSelControlSequential(method="sfs", alpha = 0.01) 

# - 5-fold cross validation -
rdesc <- makeResampleDesc(method="CV", iters=5, stratify=TRUE)

# ----------------------- End: Build models




# ----------------------- Start: Wrapper
# - Wrapper for rf, lr, xgb not necessarily needed and showed less good results -
# - Therefore only neural network wrapper was used for final data - 

set.seed(121)
parallelStartSocket(cores, level = "mlr.selectFeatures")
featureSelectionNN <- selectFeatures(nn, task=nn.task, resampling=rdesc, control=featureSearchCtrl, measures=mlr::auc, show.info=TRUE)
parallelStop()


# - Show wrapper results -
featureSelectionNN # user_id, item_id, delivery_time

# ----------------------- End: Wrapper




# ----------------------- Start: Exclude not relevant variables

# - NN Wrapper -
nn.train.woe <- nn.train.woe[, names(nn.train.woe) %in% c("return","woe.user_id", "woe.item_id","delivery_time")]
nn.test.woe <- nn.test.woe[, names(nn.train.woe) %in% c("return","woe.user_id", "woe.item_id","delivery_time")]

# ----------------------- End: Exclude not relevant variables