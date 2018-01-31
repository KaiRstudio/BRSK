# file for Adaptation to Cost Sensitive Learning
meanItemPrice<-mean(daten$item_price)
a<-0
b<-0.5*-meanItemPrice
c<-2.5*-(3+0.1*meanItemPrice)
d<-0

costs <- matrix(c(a,b,c,d), 2)
colnames(costs) <- rownames(costs) <-levels(daten$return)

#theoretical threshold
th <- costs[2,1]/(costs[2,1] + costs[1,2])


falsely.not.warned <- sum(3+0.1*(daten$item_price[no warning, returned]))
# costs as sum of item prices within test sample, where no warning is given but item is returned
falsely.not.warned <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<th & test.2$return==1])) # theoretical threshold
falsely.not.warned2 <- sum(3+0.1*(test.2$item_price[rf.cat.pred$data[,3]<0.5 & test.2$return==1])) # naive threshold

falsely.warned <- sum(itemprices[warning, kept])
# costs as sum of item prices within test sample, where warning was given although item would have been kept
falsely.warned <- sum(test.2$item_price[rf.cat.pred$data[,3]>th & test.2$return==0]) # theoretical threshold
falsely.warned2 <- sum(test.2$item_price[rf.cat.pred$data[,3]>0.5 & test.2$return==0]) # naive threshold

exp.costs <- (2.5*falsely.not.warned + 0.5*falsely.warned)
exp.costs # costs for th
exp.costs2 <- (2.5*falsely.not.warned2 + 0.5*falsely.warned2)
exp.costs2 # costs for naive
