
# -------------------------------- Adaption to Cost-Sensitive Learning ----------------------------
# -------------------------- Cost calculation based on different thresholds ----------------------


# ----------------------- Start: Theoretical Threshold based on mean price

meanItemPrice<-mean(daten$item_price)
a<-0
b<-0.5*-meanItemPrice
c<-2.5*-(3+0.1*meanItemPrice)
d<-0
costs <- matrix(c(a,b,c,d), 2)
colnames(costs) <- rownames(costs) <-levels(daten$return)

th <- costs[2,1]/(costs[2,1] + costs[1,2])

# ----------------------- End: Theoretical Threshold based on mean price




# ----------------------- Start: Naive confusion matrices

rf.cat.pred.class <- factor(rf.cat.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(rf.cat.pred.class, testset.1$return)

rf.pred.class <- factor(rf.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(rf.pred.class, testset.2$return)

#lr.pred.class <- factor(lr.pred$data[,3] > 0.5, labels = c(0, 1))
#caret::confusionMatrix(lr.pred.class, testset.3$return)

lr.cat.pred.class <- factor(lr.cat.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(lr.cat.pred.class, testset.4$return)

nn.pred.class <- factor(nn.pred$data[,3] > 0.5, labels = c(-1, 1))
caret::confusionMatrix(nn.pred.class, testset.5$return)

xgb.pred.class <- factor(xgb.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(xgb.pred.class, testset.6$return)

# ----------------------- End: Naive confusion matrices




# ---- Prepare Cross Validation ----
k <- 5
xxx <- seq(from =0.1,to= 0.9, by = 0.001)
folds <- cut(1:nrow(test), breaks = k, labels = FALSE)
set.seed(711)
folds <- sample(folds)


# ---- Save sets ----

# - RF with categories -
testset.1 <- test.2
testset.1$pred <- rf.cat.pred$data$prob.1

# - RF with WoE -
testset.2 <- test.woe
testset.2$pred <- rf.pred$data$prob.1

# - LR with WoE -
#testset.3 <- test.woe
#testset.3$pred <- lr.pred$data$prob.1

# - LR with categories -
testset.4 <- test.2
testset.4$pred <- lr.cat.pred$data$prob.1

# - NN -
testset.5 <- nn.test.woe
testset.5$pred <- nn.pred$data$prob.1
testset.5$price <- test.woe$item_price

# - XGB -
testset.6 <- test.woe
testset.6$pred <- xgb.pred$data$prob.1



# --------------------------------------------------------------------- Start: Cost calculation

# - Costs as sum of item prices within test sample -

# ---- Thresholds ----
# I:    naive threshold = 0.5
# II:   theoretical threshold = th
# III:  empirical threshold = empth

# -------------------------------------------- Start: RF with categories

# - Find threshold with min costs -
calculating.costs.rf.cat <- function(rf, cv_train, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <-all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
}


# - Empirical Thresholding: Find Optimum -
cv_results <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.1[-idx_val,]
  cv_results[1,j] <- calculating.costs.rf.cat(rf, cv_train, item_price, xxx)
}
cv.emp.th.rf.cat <- mean(cv_results)


# ---- Costs: |no warning| but item is |returned| ----
falsely.not.warned.th.rf.cat <- sum(3+0.1*(testset.1$item_price[rf.cat.pred$data[,3]<th & testset.1$return==1])) # theoretical threshold
falsely.not.warned.nth.rf.cat <- sum(3+0.1*(testset.1$item_price[rf.cat.pred$data[,3]<0.5 & testset.1$return==1])) # naive threshold
falsely.not.warned.empth.rf.cat <- sum(3+0.1*(testset.1$item_price[rf.cat.pred$data[,3]<cv.emp.th.rf.cat & testset.1$return==1])) # empirical threshold


# ---- Costs: |warning| but item would have been |kept| ----
falsely.warned.th.rf.cat <- sum(testset.1$item_price[rf.cat.pred$data[,3]>th & testset.1$return==0]) # theoretical threshold
falsely.warned.nth.rf.cat <- sum(testset.1$item_price[rf.cat.pred$data[,3]>0.5 & testset.1$return==0]) # naive threshold
falsely.warned.empth.rf.cat <- sum(testset.1$item_price[rf.cat.pred$data[,3]>cv.emp.th.rf.cat & testset.1$return==0]) # empirical threshold


# ---- RF with categories costs ----
exp.costs.th.rf.cat <- (2.5*falsely.not.warned.th.rf.cat + 0.5*falsely.warned.th.rf.cat)
exp.costs.nth.rf.cat <- (2.5*falsely.not.warned.nth.rf.cat + 0.5*falsely.warned.nth.rf.cat)
exp.costs.empth.rf.cat <- (2.5*falsely.not.warned.empth.rf.cat + 0.5*falsely.warned.empth.rf.cat)

# -------------------------------------------- End: RF with categories




# -------------------------------------------- Start: RF with WoE

# - Find threshold with min costs -
calculating.costs.rf <- function(rf, cv_train, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
}


# - Empirical Thresholding: Find Optimum -
cv_results2 <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.2[-idx_val,]
  cv_results2[1,j] <- calculating.costs.rf(rf, cv_train, item_price, xxx)
}
cv.emp.th.rf <- mean(cv_results2)


# ---- Costs: |no warning| but item is |returned| ----
falsely.not.warned.th.rf <- sum(3+0.1*(testset.2$item_price[rf.pred$data[,3]<th & testset.2$return==1])) # theoretical threshold
falsely.not.warned.nth.rf <- sum(3+0.1*(testset.2$item_price[rf.pred$data[,3]<0.5 & testset.2$return==1])) # naive threshold
falsely.not.warned.empth.rf <- sum(3+0.1*(testset.2$item_price[rf.pred$data[,3]<cv.emp.th.rf & testset.2$return==1])) # empirical threshold


# ---- Costs: |warning| but item would have been |kept| ----
falsely.warned.th.rf <- sum(testset.2$item_price[rf.pred$data[,3]>th & testset.2$return==0]) # theoretical threshold
falsely.warned.nth.rf <- sum(testset.2$item_price[rf.pred$data[,3]>0.5 & testset.2$return==0]) # naive threshold
falsely.warned.empth.rf <- sum(testset.2$item_price[rf.pred$data[,3]>cv.emp.th.rf & testset.2$return==0]) # naive threshold


# ---- Total Costs: RF with WoE ----
exp.costs.th.rf <- (2.5*falsely.not.warned.th.rf + 0.5*falsely.warned.th.rf)
exp.costs.nth.rf <- (2.5*falsely.not.warned.nth.rf + 0.5*falsely.warned.nth.rf)
exp.costs.empth.rf <- (2.5*falsely.not.warned.empth.rf + 0.5*falsely.warned.empth.rf)

# -------------------------------------------- End: RF with WoE




# -------------------------------------------- Start: LR with WoE
# - Less good AUC and more costs than LR with categories -

# - Find threshold with min costs -
#calculating.costs.lr <- function(lr, cv_train, item_price, x){
#  all_cost <- numeric()
#  for (i in seq_along(x)){
#    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
#  }
#  opt_cutoff <- x[which.min(all_cost)]
#  return (opt_cutoff)
#}
#
#
## - Empirical Thresholding: Find Optimum -
#cv_results3 <- matrix(nrow = 1, ncol = k)
#for (j in 1:k) {
#  idx_val <- which(folds == j, arr.ind = TRUE)
#  cv_train <- testset.3[-idx_val,]
#  cv_results3[1,j] <- calculating.costs.lr(lr, cv_train, item_price, xxx)
#}
#cv.emp.th.lr <- mean(cv_results3)
#
#
## ---- Costs: |no warning| but item is |returned| ----
#falsely.not.warned.th.lr <- sum(3+0.1*(testset.3$item_price[lr.pred$data[,3]<th & testset.3$return==1])) # theoretical threshold
#falsely.not.warned.nth.lr <- sum(3+0.1*(testset.3$item_price[lr.pred$data[,3]<0.5 & testset.3$return==1])) # naive threshold
#falsely.not.warned.empth.lr <- sum(3+0.1*(testset.3$item_price[lr.pred$data[,3]<cv.emp.th.lr & testset.3$return==1])) # empirical threshold
#
#
## ---- Costs: |warning| but item would have been |kept| ----
#falsely.warned.th.lr <- sum(testset.3$item_price[lr.pred$data[,3]>th & testset.3$return==0]) # theoretical threshold
#falsely.warned.nth.lr <- sum(testset.3$item_price[lr.pred$data[,3]>0.5 & testset.3$return==0]) # naive threshold
#falsely.warned.empth.lr <- sum(testset.3$item_price[lr.pred$data[,3]>cv.emp.th.lr & testset.3$return==0]) # empirical threshold
#
#
## ---- Total Costs: LR with WoE ----
#exp.costs.th.lr <- (2.5*falsely.not.warned.th.lr + 0.5*falsely.warned.th.lr)
#exp.costs.nth.lr <- (2.5*falsely.not.warned.nth.lr + 0.5*falsely.warned.nth.lr)
#exp.costs.empth.lr <- (2.5*falsely.not.warned.empth.lr + 0.5*falsely.warned.empth.lr)

# -------------------------------------------- End: LR with WoE




# -------------------------------------------- Start: LR with categories

# - Find threshold with min costs -
calculating.costs.lr.cat <- function(lr, cv_train, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
}


# - Empirical Thresholding: Find Optimum -
cv_results4 <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.4[-idx_val,]
  cv_results4[1,j] <- calculating.costs.lr.cat(lr, cv_train, item_price, xxx)
}
cv.emp.th.lr.cat <- mean(cv_results4)


# ---- Costs: |no warning| but item is |returned| ----
falsely.not.warned.th.lr.cat <- sum(3+0.1*(testset.4$item_price[lr.cat.pred$data[,3]<th & testset.4$return==1])) # theoretical threshold
falsely.not.warned.nth.lr.cat <- sum(3+0.1*(testset.4$item_price[lr.cat.pred$data[,3]<0.5 & testset.4$return==1])) # naive threshold
falsely.not.warned.empth.lr.cat <- sum(3+0.1*(testset.4$item_price[lr.cat.pred$data[,3]<cv.emp.th.lr.cat & testset.4$return==1])) # empirical threshold


# ---- Costs: |warning| but item would have been |kept| ----
falsely.warned.th.lr.cat <- sum(testset.4$item_price[lr.cat.pred$data[,3]>th & testset.4$return==0]) # theoretical threshold
falsely.warned.nth.lr.cat <- sum(testset.4$item_price[lr.cat.pred$data[,3]>0.5 & testset.4$return==0]) # naive threshold
falsely.warned.empth.lr.cat <- sum(testset.4$item_price[lr.cat.pred$data[,3]>cv.emp.th.lr.cat & testset.4$return==0]) # empirical threshold


# ---- Total Costs: LR with categories ----
exp.costs.th.lr.cat <- (2.5*falsely.not.warned.th.lr.cat + 0.5*falsely.warned.th.lr.cat)
exp.costs.nth.lr.cat <- (2.5*falsely.not.warned.nth.lr.cat + 0.5*falsely.warned.nth.lr.cat)
exp.costs.empth.lr.cat <- (2.5*falsely.not.warned.empth.lr.cat + 0.5*falsely.warned.empth.lr.cat)

# -------------------------------------------- End: LR with categories




# -------------------------------------------- Start: NN

# - Find threshold with min costs -
calculating.costs.nn <- function(nn, cv_train, price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$price[cv_train$pred>x[i] & cv_train$return==-1]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
}


# - Empirical Thresholding: Find Optimum -
cv_results5 <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.5[-idx_val,]
  cv_results5[1,j] <- calculating.costs.nn(nn, cv_train, price, xxx)
}
cv.emp.th.nn <- mean(cv_results5)


# ---- Costs: |no warning| but item is |returned| ----
falsely.not.warned.th.nn <- sum(3+0.1*(testset.5$price[nn.pred$data[,3]<th & testset.5$return==1])) # theoretical threshold
falsely.not.warned.nth.nn <- sum(3+0.1*(testset.5$price[nn.pred$data[,3]<0.5 & testset.5$return==1])) # naive threshold
falsely.not.warned.empth.nn <- sum(3+0.1*(testset.5$price[nn.pred$data[,3]<cv.emp.th.nn & testset.5$return==1])) # empirical threshold


# ---- Costs: |warning| but item would have been |kept| ----
falsely.warned.th.nn <- sum(testset.5$price[nn.pred$data[,3]>th & testset.5$return==-1]) # theoretical threshold
falsely.warned.nth.nn <- sum(testset.5$price[nn.pred$data[,3]>0.5 & testset.5$return==-1]) # naive threshold
falsely.warned.empth.nn <- sum(testset.5$price[nn.pred$data[,3]>cv.emp.th.nn & testset.5$return==-1]) # empirical threshold


# ---- Total Costs: NN ----
exp.costs.th.nn <- (2.5*falsely.not.warned.th.nn + 0.5*falsely.warned.th.nn)
exp.costs.nth.nn <- (2.5*falsely.not.warned.nth.nn + 0.5*falsely.warned.nth.nn)
exp.costs.empth.nn <- (2.5*falsely.not.warned.empth.nn + 0.5*falsely.warned.empth.nn)

# -------------------------------------------- End: NN




# -------------------------------------------- Start: XGB

# - Function: Find threshold with min costs -
calculating.costs.xgb <- function(xgb, cv_train, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
}


# - Empirical Thresholding: Find Optimum -
cv_results6 <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.6[-idx_val,]
  cv_results6[1,j] <- calculating.costs.xgb(xgb, cv_train, item_price, xxx)
}
cv.emp.th.xgb <- mean(cv_results6)


# ---- Costs: |no warning| but item is |returned| ----
falsely.not.warned.th.xgb <- sum(3+0.1*(testset.6$item_price[xgb.pred$data[,3]<th & testset.6$return==1])) # theoretical threshold
falsely.not.warned.nth.xgb <- sum(3+0.1*(testset.6$item_price[xgb.pred$data[,3]<0.5 & testset.6$return==1])) # naive threshold
falsely.not.warned.empth.xgb <- sum(3+0.1*(testset.6$item_price[xgb.pred$data[,3]<cv.emp.th.xgb & testset.6$return==1])) # empirical threshold


# ---- Costs: |warning| but item would have been |kept| ----
falsely.warned.th.xgb <- sum(testset.6$item_price[xgb.pred$data[,3]>th & testset.6$return==0]) # theoretical threshold
falsely.warned.nth.xgb <- sum(testset.6$item_price[xgb.pred$data[,3]>0.5 & testset.6$return==0]) # naive threshold
falsely.warned.empth.xgb <- sum(testset.6$item_price[xgb.pred$data[,3]>cv.emp.th.xgb & testset.6$return==0]) # empirical threshold


# ---- Total Costs: XGB ----
exp.costs.th.xgb <- (2.5*falsely.not.warned.th.xgb + 0.5*falsely.warned.th.xgb)
exp.costs.nth.xgb <- (2.5*falsely.not.warned.nth.xgb + 0.5*falsely.warned.nth.xgb)
exp.costs.empth.xgb <- (2.5*falsely.not.warned.empth.xgb + 0.5*falsely.warned.empth.xgb)

# -------------------------------------------- End: XGB




# -------------------------------------------- Save Results

naive.th <- round(c(exp.costs.nth.rf.cat,exp.costs.nth.rf,exp.costs.nth.lr.cat,exp.costs.nth.nn,exp.costs.nth.xgb)/nrow(test),2)
theoretical.th <- round(c(exp.costs.th.rf.cat,exp.costs.th.rf,exp.costs.th.lr.cat,exp.costs.th.nn,exp.costs.th.xgb)/nrow(test),2)
empirical.th <- round(c(exp.costs.empth.rf.cat,exp.costs.empth.rf,exp.costs.empth.lr.cat,exp.costs.empth.nn,exp.costs.empth.xgb)/nrow(test),2)
naming <- c("rf.cat", "rf", "lr.cat" , "nn", "xgb")
cost.matrix <- data.frame(naive.th,theoretical.th, empirical.th, row.names = naming)
cost.matrix

