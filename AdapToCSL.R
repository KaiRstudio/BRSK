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

# needs to be done for each model and for each threshold --> minimised costs wanted
falsely.not.warned <- sum(3+0.1(daten$item_price[no warning, returned]))
falsely.not.warned <- sum(3+0.1(daten$item_price[daten$ProbabilityOfReturn<ThresholdForReturns & daten$return==1]))

falsely.warned <- sum(daten$item_price[warning, kept])
falsely.warned <- sum(daten$item_price[daten$ProbabilityOfReturn>ThresholdForReturn &  daten$return==0])

exp.costs <- 2.5*falsely.not.warned + 0.5*falsely.warned