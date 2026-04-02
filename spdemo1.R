
# Demo: demographic stochasticity

N=5     # number of INDIVIDUALS
g = 0.22  # per capita survival rate
b = 0.45   # per capita birth rate

 ## 
barplot(dbinom(0:N,N,g),names.arg=0:N,xlab="Number of survivors",ylab="Probability")

barplot(dpois(0:(N*1.3),N*b),names.arg=0:(N*1.3),xlab="# offspring produced",ylab="Probability")




# Demo: environmental stochasticity

N=1     # number of POPULATIONS
b_mu = 0.22  # per capita survival rate
b_sd = 0.15   # per capita birth rate
lots=10000

meansurv = replicate(lots,mean(rnorm(N,b_mu,b_sd)))
meansurv[meansurv<0] = 0

hist(meansurv,xlab="Mean survival",ylab="Probability",freq=F,xlim=c(0,1))





