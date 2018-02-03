# file for Adaptation to Cost Sensitive Learning



# ----------------------- Cost matrix by mean
meanItemPrice<-mean(daten$item_price)
a<-0
b<-0.5*-meanItemPrice
c<-2.5*-(3+0.1*meanItemPrice)
d<-0

costs <- matrix(c(a,b,c,d), 2)

colnames(costs) <- rownames(costs) <-levels(daten$return)

th <- costs[2,1]/(costs[2,1] + costs[1,2])

# ----------------------- end calculation of theoretical threshold

#-------------------------Start Calculation of empirical threshold



# ----------------------- confusion matrices for different thresholds

rf.cat.pred.class <- factor(rf.cat.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(rf.cat.pred.class, test.2$return)

rf.pred.class <- factor(rf.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(rf.pred.class, test.woe$return)

lr.pred.class <- factor(lr.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(lr.pred.class, test.woe$return)

lr.cat.pred.class <- factor(lr.cat.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(lr.cat.pred.class, test.2$return)

nn.pred.class <- factor(nn.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(nn.pred.class, nn.test.woe$return)

xgb.pred.class <- factor(xgb.pred$data[,3] > 0.5, labels = c(0, 1))
caret::confusionMatrix(xgb.pred.class, test.woe$return)

# ----------------------- end confusion matrices


# ----------------------- Cross validation with caret
set.seed(500)
cv_splits.test.woe <- createFolds(test.woe, k = 10, returnTrain = TRUE)
cv_splits.test.2 <- createFolds(test.2, k = 10, returnTrain = TRUE)
cv_splits.nn <- createFolds(nn.test.woe, k = 10, returnTrain = TRUE)

str(cv_splits.test.woe)


# ----------------------- start cost calculations with three different thresholds.

# I:    naive threshold = 0.5
# II:   theoretical threshold = th
# III:  empirical threshold = empth


testset.1 <- test.2
testset.1$pred <- rf.cat.pred$data$prob.1

# calculate empirical cost-dependent threshold
calculating.costs.rf.cat <- function(rf, cv_train, item_price, x){
  all_cost <- numeric()
  revenue <- numeric()
  profits <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
    revenue[i] <- sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==1]);
    profits[i] <- revenue[i] - all_cost[i];
  }
  opt_cutoff <- x[which.max(profits)]
  return (opt_cutoff)
    }


# Cross validated calculation of optimal, empirical threshold
xxx <- seq(from =0.1,to= 0.9, by = 0.01)

k <- 10
folds <- cut(1:nrow(testset.1), breaks = k, labels = FALSE)
set.seed(411)
folds <- sample(folds)
cv_results <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.1[-idx_val,]
  cv_results[1,j] <- calculating.costs.rf.cat(rf, cv_train, item_price, xxx)
}
cv.emp.th.rf.cat <- mean(cv_results)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned.th.rf.cat <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<th & test.2$return==1])) # theoretical threshold
falsely.not.warned.nth.rf.cat <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<0.5 & test.2$return==1])) # naive threshold
falsely.not.warned.empth.rf.cat <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<cv.emp.th.rf.cat & test.2$return==1])) # empirical threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned.th.rf.cat <- sum(test.2$item_price[rf.cat.pred$data[,3]>th & test.2$return==0]) # theoretical threshold
falsely.warned.nth.rf.cat <- sum(test.2$item_price[rf.cat.pred$data[,3]>0.5 & test.2$return==0]) # naive threshold
falsely.warned.empth.rf.cat <- sum(test.2$item_price[rf.cat.pred$data[,3]>cv.emp.th.rf.cat & test.2$return==0]) # empirical threshold

exp.costs.th.rf.cat <- (2.5*falsely.not.warned.th.rf.cat + 0.5*falsely.warned.th.rf.cat)
exp.costs.nth.rf.cat <- (2.5*falsely.not.warned.nth.rf.cat + 0.5*falsely.warned.nth.rf.cat)
exp.costs.empth.rf.cat <- (2.5*falsely.not.warned.empth.rf.cat + 0.5*falsely.warned.empth.rf.cat)

# ---------------------------------------------------------------------

testset.2 <- test.woe
testset.2$pred <- rf.pred$data$prob.1

# calculate empirical cost-dependent threshold
calculating.costs.rf <- function(rf, cv_train, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
}

# Cross validated calculation of optimal, empirical threshold
xxx <- seq(from =0.1,to= 0.9, by = 0.001)

k <- 10
folds <- cut(1:nrow(testset.2), breaks = k, labels = FALSE)
set.seed(412)
folds <- sample(folds)
cv_results2 <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.2[-idx_val,]
  cv_results2[1,j] <- calculating.costs.rf(rf, cv_train, item_price, xxx)
}
cv.emp.th.rf <- mean(cv_results2)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned.th.rf <- sum(3+0.1*(test.woe$item_price[rf.pred$data[,3]<th & test.woe$return==1])) # theoretical threshold
falsely.not.warned.nth.rf <- sum(3+0.1*(test.woe$item_price[rf.pred$data[,3]<0.5 & test.woe$return==1])) # naive threshold
falsely.not.warned.empth.rf <- sum(3+0.1*(test.woe$item_price[rf.pred$data[,3]<cv.emp.th.rf & test.woe$return==1])) # empirical threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned.th.rf <- sum(test.woe$item_price[rf.pred$data[,3]>th & test.woe$return==0]) # theoretical threshold
falsely.warned.nth.rf <- sum(test.woe$item_price[rf.pred$data[,3]>0.5 & test.woe$return==0]) # naive threshold
falsely.warned.empth.rf <- sum(test.woe$item_price[rf.pred$data[,3]>cv.emp.th.rf & test.woe$return==0]) # naive threshold

exp.costs.th.rf <- (2.5*falsely.not.warned.th.rf + 0.5*falsely.warned.th.rf)
exp.costs.nth.rf <- (2.5*falsely.not.warned.nth.rf + 0.5*falsely.warned.nth.rf)
exp.costs.empth.rf <- (2.5*falsely.not.warned.empth.rf + 0.5*falsely.warned.empth.rf)

# ---------------------------------------------------------------------

testset.3 <- test.woe
testset.3$pred <- lr.pred$data$prob.1

# calculate empirical cost-dependent threshold
calculating.costs.lr <- function(lr, cv_train, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
}

# Cross validated calculation of optimal, empirical threshold
xxx <- seq(from =0.1,to= 0.9, by = 0.001)

k <- 10
folds <- cut(1:nrow(testset.3), breaks = k, labels = FALSE)
set.seed(123)
folds <- sample(folds)
cv_results3 <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.3[-idx_val,]
  cv_results3[1,j] <- calculating.costs.lr(lr, cv_train, item_price, xxx)
}
cv.emp.th.lr <- mean(cv_results3)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned.th.lr <- sum(3+0.1*(test.woe$item_price[lr.pred$data[,3]<th & test.woe$return==1])) # theoretical threshold
falsely.not.warned.nth.lr <- sum(3+0.1*(test.woe$item_price[lr.pred$data[,3]<0.5 & test.woe$return==1])) # naive threshold
falsely.not.warned.empth.lr <- sum(3+0.1*(test.woe$item_price[lr.pred$data[,3]<cv.emp.th.lr & test.woe$return==1])) # empirical threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned.th.lr <- sum(test.woe$item_price[lr.pred$data[,3]>th & test.woe$return==0]) # theoretical threshold
falsely.warned.nth.lr <- sum(test.woe$item_price[lr.pred$data[,3]>0.5 & test.woe$return==0]) # naive threshold
falsely.warned.empth.lr <- sum(test.woe$item_price[lr.pred$data[,3]>cv.emp.th.lr & test.woe$return==0]) # empirical threshold

exp.costs.th.lr <- (2.5*falsely.not.warned.th.lr + 0.5*falsely.warned.th.lr)
exp.costs.nth.lr <- (2.5*falsely.not.warned.nth.lr + 0.5*falsely.warned.nth.lr)
exp.costs.empth.lr <- (2.5*falsely.not.warned.empth.lr + 0.5*falsely.warned.empth.lr)

# ---------------------------------------------------------------------

testset.4 <- test.2
testset.4$pred <- lr.cat.pred$data$prob.1

# calculate empirical cost-dependent threshold
calculating.costs.lr.cat <- function(lr, cv_train, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
}

# Cross validated calculation of optimal, empirical threshold
xxx <- seq(from =0.1,to= 0.9, by = 0.001)

k <- 10
folds <- cut(1:nrow(testset.4), breaks = k, labels = FALSE)
set.seed(123)
folds <- sample(folds)
cv_results4 <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.4[-idx_val,]
  cv_results4[1,j] <- calculating.costs.lr.cat(lr, cv_train, item_price, xxx)
}
cv.emp.th.lr.cat <- mean(cv_results4)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned.th.lr.cat <- sum(3+0.1*(test.2$item_price[lr.cat.pred$data[,3]<th & test.2$return==1])) # theoretical threshold
falsely.not.warned.nth.lr.cat <- sum(3+0.1*(test.2$item_price[lr.cat.pred$data[,3]<0.5 & test.2$return==1])) # naive threshold
falsely.not.warned.empth.lr.cat <- sum(3+0.1*(test.2$item_price[lr.cat.pred$data[,3]<cv.emp.th.lr.cat & test.2$return==1])) # empirical threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned.th.lr.cat <- sum(test.2$item_price[lr.cat.pred$data[,3]>th & test.2$return==0]) # theoretical threshold
falsely.warned.nth.lr.cat <- sum(test.2$item_price[lr.cat.pred$data[,3]>0.5 & test.2$return==0]) # naive threshold
falsely.warned.empth.lr.cat <- sum(test.2$item_price[lr.cat.pred$data[,3]>cv.emp.th.lr.cat & test.2$return==0]) # empirical threshold

exp.costs.th.lr.cat <- (2.5*falsely.not.warned.th.lr.cat + 0.5*falsely.warned.th.lr.cat)
exp.costs.nth.lr.cat <- (2.5*falsely.not.warned.nth.lr.cat + 0.5*falsely.warned.nth.lr.cat)
exp.costs.empth.lr.cat <- (2.5*falsely.not.warned.empth.lr.cat + 0.5*falsely.warned.empth.lr.cat)

# ---------------------------------------------------------------------

testset.4 <- nn.test.woe
testset.4$pred <- nn.pred$data$prob.1
testset.4$price <- test.woe$item_price

# calculate empirical cost-dependent threshold
calculating.costs.nn <- function(nn, cv_train, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$price[cv_train$pred>x[i] & cv_train$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
}

# Cross validated calculation of optimal, empirical threshold
xxx <- seq(from =0.1,to= 0.9, by = 0.001)

k <- 10
folds <- cut(1:nrow(testset.4), breaks = k, labels = FALSE)
set.seed(123)
folds <- sample(folds)
cv_results5 <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.4[-idx_val,]
  cv_results5[1,j] <- calculating.costs.nn(nn, cv_train, price, xxx)
}
cv.emp.th.nn <- mean(cv_results5)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned.th.nn <- sum(3+0.1*(nn.test.woe2$price[nn.pred$data[,3]<th & nn.test.woe2$return==1])) # theoretical threshold
falsely.not.warned.nth.nn <- sum(3+0.1*(nn.test.woe2$price[nn.pred$data[,3]<0.5 & nn.test.woe2$return==1])) # naive threshold
falsely.not.warned.empth.nn <- sum(3+0.1*(nn.test.woe2$price[nn.pred$data[,3]<cv.emp.th.nn & nn.test.woe2$return==1])) # empirical threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned.th.nn <- sum(nn.test.woe2$price[nn.pred$data[,3]>th & nn.test.woe2$return==0]) # theoretical threshold
falsely.warned.nth.nn <- sum(nn.test.woe2$price[nn.pred$data[,3]>0.5 & nn.test.woe2$return==0]) # naive threshold
falsely.warned.empth.nn <- sum(nn.test.woe2$price[nn.pred$data[,3]>cv.emp.th.nn & nn.test.woe2$return==0]) # empirical threshold

exp.costs.th.nn <- (2.5*falsely.not.warned.th.nn + 0.5*falsely.warned.th.nn)
exp.costs.nth.nn <- (2.5*falsely.not.warned.nth.nn + 0.5*falsely.warned.nth.nn)
exp.costs.empth.nn <- (2.5*falsely.not.warned.empth.nn + 0.5*falsely.warned.empth.nn)

# ---------------------------------------------------------------------

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned11 <- sum(3+0.1*(test.woe$item_price[xgb.pred$data[,3]<th & test.woe$return==1])) # theoretical threshold
falsely.not.warned12 <- sum(3+0.1*(test.woe$item_price[xgb.pred$data[,3]<0.5 & test.woe$return==1])) # naive threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned11 <- sum(test.woe$item_price[xgb.pred$data[,3]>th & test.woe$return==0]) # theoretical threshold
falsely.warned12 <- sum(test.woe$item_price[xgb.pred$data[,3]>0.5 & test.woe$return==0]) # naive threshold

exp.costs11 <- (2.5*falsely.not.warned11 + 0.5*falsely.warned11)
exp.costs12 <- (2.5*falsely.not.warned12 + 0.5*falsely.warned12)

# ----------------------- end cost calculations

naive.th <- round(c(exp.costs.nth.rf.cat,exp.costs.nth.rf,exp.costs.nth.lr,exp.costs.nth.lr.cat,exp.costs.nth.nn))
theoretical.th <- round(c(exp.costs.th.rf.cat,exp.costs.th.rf,exp.costs.th.lr,exp.costs.th.lr.cat,exp.costs.th.nn))
empirical.th <- round(c(exp.costs.empth.rf.cat,exp.costs.empth.rf,exp.costs.empth.lr,exp.costs.empth.lr.cat,exp.costs.empth.nn))
naming <- c("rf.cat", "rf", "lr", "lr.cat" , "nn")
cost.matrix <- data.frame(naive.th,theoretical.th, empirical.th, row.names = naming)
cost.matrix