# file for Adaptation to Cost Sensitive Learning
# ----------------------- start calculation of theoretical threshold
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



# ----------------------- start cost calculations

testset.1 <- test.2
testset.1$pred <- rf.cat.pred$data$prob.1

# calculate empirical cost-dependent threshold
abcdefg <- function(rf, cv_train, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(cv_train$item_price[cv_train$pred<x[i] & cv_train$return==1]))+0.5*sum(cv_train$item_price[cv_train$pred>x[i] & cv_train$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
    }

# Cross validated calculation of optimal, empirical threshold
xxx <- seq(from =0.1,to= 0.9, by = 0.01)

k <- 10
folds <- cut(1:nrow(testset.1), breaks = k, labels = FALSE)
set.seed(123)
folds <- sample(folds)
cv_results <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.1[-idx_val,]
  cv_results[1,j] <- abcdefg(rf, cv_train, item_price, xxx)
}
opt.cross.val.th1 <- mean(cv_results)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned1 <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<th & test.2$return==1])) # theoretical threshold
falsely.not.warned2 <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<0.5 & test.2$return==1])) # naive threshold
falsely.not.warned.opt1 <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<opt.cross.val.th1 & test.2$return==1])) # naive threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned1 <- sum(test.2$item_price[rf.cat.pred$data[,3]>th & test.2$return==0]) # theoretical threshold
falsely.warned2 <- sum(test.2$item_price[rf.cat.pred$data[,3]>0.5 & test.2$return==0]) # naive threshold
falsely.warned.opt2 <- sum(test.2$item_price[rf.cat.pred$data[,3]>opt.cross.val.th1 & test.2$return==0]) # empirical threshold

exp.costs1 <- (2.5*falsely.not.warned1 + 0.5*falsely.warned1)
exp.costs2 <- (2.5*falsely.not.warned2 + 0.5*falsely.warned2)
exp.costs.opt1 <- (2.5*falsely.not.warned.opt1 + 0.5*falsely.warned.opt2)

# ---------------------------------------------------------------------

testset.2 <- test.woe
testset.2$pred <- rf.pred$data$prob.1

# calculate empirical cost-dependent threshold
abcdefg2 <- function(rf, cv_train, item_price, x){
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
set.seed(123)
folds <- sample(folds)
cv_results2 <- matrix(nrow = 1, ncol = k)
for (j in 1:k) {
  idx_val <- which(folds == j, arr.ind = TRUE)
  cv_train <- testset.2[-idx_val,]
  cv_results2[1,j] <- abcdefg2(rf, cv_train, item_price, xxx)
}
opt.cross.val.th2 <- mean(cv_results2)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned3 <- sum(3+0.1*(test.woe$item_price[rf.pred$data[,3]<th & test.woe$return==1])) # theoretical threshold
falsely.not.warned4 <- sum(3+0.1*(test.woe$item_price[rf.pred$data[,3]<0.5 & test.woe$return==1])) # naive threshold
falsely.not.warned.opt3 <- sum(3+0.1*(test.woe$item_price[rf.pred$data[,3]<opt.cross.val.th2 & test.woe$return==1])) # empirical threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned3 <- sum(test.woe$item_price[rf.pred$data[,3]>th & test.woe$return==0]) # theoretical threshold
falsely.warned4 <- sum(test.woe$item_price[rf.pred$data[,3]>0.5 & test.woe$return==0]) # naive threshold
falsely.warned.opt3 <- sum(test.woe$item_price[rf.pred$data[,3]>opt.cross.val.th2 & test.woe$return==0]) # naive threshold

exp.costs3 <- (2.5*falsely.not.warned3 + 0.5*falsely.warned3)
exp.costs4 <- (2.5*falsely.not.warned4 + 0.5*falsely.warned4)
exp.costs.opt2 <- (2.5*falsely.not.warned.opt3 + 0.5*falsely.warned.opt3)

# ---------------------------------------------------------------------

testset.3 <- test.woe
testset.3$pred <- lr.pred$data$prob.1

# calculate empirical cost-dependent threshold
abcdefg3 <- function(lr, cv_train, item_price, x){
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
  cv_results3[1,j] <- abcdefg3(lr, cv_train, item_price, xxx)
}
opt.cross.val.th3 <- mean(cv_results3)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned5 <- sum(3+0.1*(test.woe$item_price[lr.pred$data[,3]<th & test.woe$return==1])) # theoretical threshold
falsely.not.warned6 <- sum(3+0.1*(test.woe$item_price[lr.pred$data[,3]<0.5 & test.woe$return==1])) # naive threshold
falsely.not.warned.opt5 <- sum(3+0.1*(test.woe$item_price[lr.pred$data[,3]<opt.cross.val.th3 & test.woe$return==1])) # empirical threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned5 <- sum(test.woe$item_price[lr.pred$data[,3]>th & test.woe$return==0]) # theoretical threshold
falsely.warned6 <- sum(test.woe$item_price[lr.pred$data[,3]>0.5 & test.woe$return==0]) # naive threshold
falsely.warned.opt3 <- sum(test.woe$item_price[lr.pred$data[,3]>opt.cross.val.th3 & test.woe$return==0]) # empirical threshold

exp.costs5 <- (2.5*falsely.not.warned5 + 0.5*falsely.warned5)
exp.costs6 <- (2.5*falsely.not.warned6 + 0.5*falsely.warned6)
exp.costs.opt3 <- (2.5*falsely.not.warned.opt5 + 0.5*falsely.warned.opt5)

# ---------------------------------------------------------------------

testset.4 <- test.2
testset.4$pred <- lr.cat.pred$data$prob.1

# calculate empirical cost-dependent threshold
abcdefg4 <- function(lr, cv_train, item_price, x){
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
  cv_results4[1,j] <- abcdefg4(lr, cv_train, item_price, xxx)
}
opt.cross.val.th4 <- mean(cv_results4)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned7 <- sum(3+0.1*(test.2$item_price[lr.cat.pred$data[,3]<th & test.2$return==1])) # theoretical threshold
falsely.not.warned8 <- sum(3+0.1*(test.2$item_price[lr.cat.pred$data[,3]<0.5 & test.2$return==1])) # naive threshold
falsely.not.warned.opt7 <- sum(3+0.1*(test.2$item_price[lr.cat.pred$data[,3]<opt.cross.val.th4 & test.2$return==1])) # empirical threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned7 <- sum(test.2$item_price[lr.cat.pred$data[,3]>th & test.2$return==0]) # theoretical threshold
falsely.warned8 <- sum(test.2$item_price[lr.cat.pred$data[,3]>0.5 & test.2$return==0]) # naive threshold
falsely.warned.opt7 <- sum(test.2$item_price[lr.cat.pred$data[,3]>opt.cross.val.th4 & test.2$return==0]) # empirical threshold

exp.costs7 <- (2.5*falsely.not.warned7 + 0.5*falsely.warned7)
exp.costs8 <- (2.5*falsely.not.warned8 + 0.5*falsely.warned8)
exp.costs.opt4 <- (2.5*falsely.not.warned.opt7 + 0.5*falsely.warned.opt7)

# ---------------------------------------------------------------------

testset.4 <- nn.test.woe
testset.4$pred <- nn.pred$data$prob.1
testset.4$price <- test.woe$item_price

# calculate empirical cost-dependent threshold
abcdefg5 <- function(nn, cv_train, item_price, x){
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
  cv_results5[1,j] <- abcdefg5(nn, cv_train, price, xxx)
}
opt.cross.val.th5 <- mean(cv_results5)

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned9 <- sum(3+0.1*(nn.test.woe2$price[nn.pred$data[,3]<th & nn.test.woe2$return==1])) # theoretical threshold
falsely.not.warned10 <- sum(3+0.1*(nn.test.woe2$price[nn.pred$data[,3]<0.5 & nn.test.woe2$return==1])) # naive threshold
falsely.not.warned.opt9 <- sum(3+0.1*(nn.test.woe2$price[nn.pred$data[,3]<opt.cross.val.th5 & nn.test.woe2$return==1])) # empirical threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned9 <- sum(nn.test.woe2$price[nn.pred$data[,3]>th & nn.test.woe2$return==0]) # theoretical threshold
falsely.warned10 <- sum(nn.test.woe2$price[nn.pred$data[,3]>0.5 & nn.test.woe2$return==0]) # naive threshold
falsely.warned.opt9 <- sum(nn.test.woe2$price[nn.pred$data[,3]>opt.cross.val.th5 & nn.test.woe2$return==0]) # empirical threshold

exp.costs9 <- (2.5*falsely.not.warned9 + 0.5*falsely.warned9)
exp.costs10 <- (2.5*falsely.not.warned10 + 0.5*falsely.warned10)
exp.costs.opt5 <- (2.5*falsely.not.warned.opt9 + 0.5*falsely.warned.opt9)

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

naive.th <- round(c(exp.costs2,exp.costs4,exp.costs6,exp.costs8,exp.costs10))
theoretical.th <- round(c(exp.costs1,exp.costs3,exp.costs5,exp.costs7,exp.costs9))
empirical.th <- round(c(exp.costs.opt1,exp.costs.opt2,exp.costs.opt3,exp.costs.opt4,exp.costs.opt5))
naming <- c("rf.cat", "rf", "lr", "lr.cat" , "nn")
cost.matrix <- data.frame(naive.th,theoretical.th, empirical.th, row.names = naming)
cost.matrix