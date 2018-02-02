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

abcdefg <- function(rf, test, item_price, x){
  all_cost <- numeric()
  for (i in seq_along(x)){
    all_cost[i] <- (2.5*sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<x[i] & test.2$return==1]))+0.5*sum(test.2$item_price[rf.cat.pred$data[,3]>x[i] & test.2$return==0]));
  }
  opt_cutoff <- x[which.min(all_cost)]
  return (opt_cutoff)
    }
xxx <- seq(from =0.6109,to= 0.6111, by = 0.000001)
optimal.threshold1 <- abcdefg(rf, test, item_price, xxx)


# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned1 <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<th & test.2$return==1])) # theoretical threshold
falsely.not.warned2 <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<0.5 & test.2$return==1])) # naive threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned1 <- sum(test.2$item_price[rf.cat.pred$data[,3]>th & test.2$return==0]) # theoretical threshold
falsely.warned2 <- sum(test.2$item_price[rf.cat.pred$data[,3]>0.5 & test.2$return==0]) # naive threshold

exp.costs1 <- (2.5*falsely.not.warned1 + 0.5*falsely.warned1)
exp.costs1 # costs for th
exp.costs2 <- (2.5*falsely.not.warned2 + 0.5*falsely.warned2)
exp.costs2 # costs for naive

# ---------------------------------------------------------------------

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned3 <- sum(3+0.1*(test.woe$item_price[rf.pred$data[,3]<th & test.woe$return==1])) # theoretical threshold
falsely.not.warned4 <- sum(3+0.1*(test.woe$item_price[rf.pred$data[,3]<0.5 & test.woe$return==1])) # naive threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned3 <- sum(test.woe$item_price[rf.pred$data[,3]>th & test.woe$return==0]) # theoretical threshold
falsely.warned4 <- sum(test.woe$item_price[rf.pred$data[,3]>0.5 & test.woe$return==0]) # naive threshold

exp.costs3 <- (2.5*falsely.not.warned3 + 0.5*falsely.warned3)
exp.costs3 # costs for th
exp.costs4 <- (2.5*falsely.not.warned4 + 0.5*falsely.warned4)
exp.costs4 # costs for naive

# ---------------------------------------------------------------------

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned5 <- sum(3+0.1*(test.woe$item_price[lr.pred$data[,3]<th & test.woe$return==1])) # theoretical threshold
falsely.not.warned6 <- sum(3+0.1*(test.woe$item_price[lr.pred$data[,3]<0.5 & test.woe$return==1])) # naive threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned5 <- sum(test.woe$item_price[lr.pred$data[,3]>th & test.woe$return==0]) # theoretical threshold
falsely.warned6 <- sum(test.woe$item_price[lr.pred$data[,3]>0.5 & test.woe$return==0]) # naive threshold

exp.costs5 <- (2.5*falsely.not.warned5 + 0.5*falsely.warned5)
exp.costs5 # costs for th
exp.costs6 <- (2.5*falsely.not.warned6 + 0.5*falsely.warned6)
exp.costs6 # costs for naive

# ---------------------------------------------------------------------

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned7 <- sum(3+0.1*(test.2$item_price[lr.cat.pred$data[,3]<th & test.2$return==1])) # theoretical threshold
falsely.not.warned8 <- sum(3+0.1*(test.2$item_price[lr.cat.pred$data[,3]<0.5 & test.2$return==1])) # naive threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned7 <- sum(test.2$item_price[lr.cat.pred$data[,3]>th & test.2$return==0]) # theoretical threshold
falsely.warned8 <- sum(test.2$item_price[lr.cat.pred$data[,3]>0.5 & test.2$return==0]) # naive threshold

exp.costs7 <- (2.5*falsely.not.warned7 + 0.5*falsely.warned7)
exp.costs7 # costs for th
exp.costs8 <- (2.5*falsely.not.warned8 + 0.5*falsely.warned8)
exp.costs8 # costs for naive

# ---------------------------------------------------------------------

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned9 <- sum(3+0.1*(nn.test.woe$item_price[nn.pred$data[,3]<th & nn.test.woe$return==1])) # theoretical threshold
falsely.not.warned10 <- sum(3+0.1*(nn.test.woe$item_price[nn.pred$data[,3]<0.5 & nn.test.woe$return==1])) # naive threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned9 <- sum(nn.test.woe$item_price[nn.pred$data[,3]>th & nn.test.woe$return==0]) # theoretical threshold
falsely.warned10 <- sum(nn.test.woe$item_price[nn.pred$data[,3]>0.5 & nn.test.woe$return==0]) # naive threshold

exp.costs9 <- (2.5*falsely.not.warned9 + 0.5*falsely.warned9)
exp.costs9 # costs for th
exp.costs10 <- (2.5*falsely.not.warned10 + 0.5*falsely.warned10)
exp.costs10 # costs for naive

# ---------------------------------------------------------------------

# costs as sum of item prices within test sample, where |no warning| is given but item is |returned|
falsely.not.warned11 <- sum(3+0.1*(test.woe$item_price[xgb.pred$data[,3]<th & test.woe$return==1])) # theoretical threshold
falsely.not.warned12 <- sum(3+0.1*(test.woe$item_price[xgb.pred$data[,3]<0.5 & test.woe$return==1])) # naive threshold

# costs as sum of item prices within test sample, where |warning| was given although item would have been |kept|
falsely.warned11 <- sum(test.woe$item_price[xgb.pred$data[,3]>th & test.woe$return==0]) # theoretical threshold
falsely.warned12 <- sum(test.woe$item_price[xgb.pred$data[,3]>0.5 & test.woe$return==0]) # naive threshold

exp.costs11 <- (2.5*falsely.not.warned11 + 0.5*falsely.warned11)
exp.costs11 # costs for th
exp.costs12 <- (2.5*falsely.not.warned12 + 0.5*falsely.warned12)
exp.costs12 # costs for naive

# ----------------------- end cost calculations

naive.th <- c(exp.costs2,exp.costs4,exp.costs6,exp.costs8,exp.costs10)
theoretical.th <- c(exp.costs,exp.costs3,exp.costs5,exp.costs7,exp.costs9)
naming <- c("rf.cat", "rf", "lr", "lr.cat" , "nn")
cost.matrix <- data.frame(naive.th,theoretical.th, row.names = naming)
