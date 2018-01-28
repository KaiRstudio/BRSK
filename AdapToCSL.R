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

