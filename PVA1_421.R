
#  NRES 421, PVA 
#   University of Nevada, Reno
#   PVA basics  ------------------------------


             # plot a discrete distribution!
xvals <- seq(0,10,1)
probs <- dbinom(xvals,10,prob=0.3)
names(probs) <- xvals
               
barplot(probs,ylab="Probability",xlab="Possibilities",main="Binomial distribution (discrete)")



             # plot a discrete distribution!
xvals <- seq(0,10,1)
probs <- dpois(xvals,lambda=2.2)
names(probs) <- xvals
               
barplot(probs,ylab="Probability",xlab="Possibilities",main="Poisson distribution (discrete)")


mean = 7.1
stdev = 1.9

curve(dnorm(x,mean,stdev),0,15,ylab="Probability (density)",xlab="Possibilities",main="Normal distribution (continuous)")   # probability density


####
# Basic simulation parameters
####

nyears <- 100     # number of years
nreps <- 500      # number of replicates

####
# Basic life history parameters
####

R_max <- 1.03       # Maximum rate of growth (max lambda)
Init_N <- 51        # Initial abundance
K <- 175            # Carrying capacity

####
# Environmental stochasticity
####

SD_lambda <- 0.11  # standard deviation of lambda

####
# Density-dependence (Ricker model)
####

Ricker <- function(prev_abund){       # this is a function for computing next-year abundance -- includes env stochasticity
  prev_abund * exp(log(rnorm(1,R_max,SD_lambda))*(1-(prev_abund/K)))
}

####
# Catastrophe
####

Flood_prob <- 0.0     # 0% chance of major flood
Flood_lambda <- 0.25    # 25% of population can survive a flood, should it occur!




## Set up data structures to store simulation results!

PVAdemo <- function(nreps,nyears,Init_N,R_max,K,Flood_prob,Flood_lambda){
  #browser()
  PopArray2 <- array(0,dim=c((nyears+1),nreps))
  
  ## start looping through replicates
  
  for(rep in 1:nreps){
    
    # set initial abundance
    PopArray2[1,rep] <- Init_N     # initial abundance
    
    ### loop through years
    for(y in 2:(nyears+1)){
      ### stochasticity and d-d
      nextyear <- max(0,trunc(Ricker(PopArray2[y-1,rep])))
      
      ### catastrophe
      if(runif(1)<Flood_prob) nextyear <- nextyear*Flood_lambda
      PopArray2[y,rep] <- nextyear 
    }
  }
  
  return(PopArray2)
}

### Run the PVA!

Default <- PVAdemo(nreps,nyears,Init_N,R_max,K,Flood_prob,Flood_lambda)


PlotCloud <- function(simdata){
  plot(c(1:101),simdata[,1],col=gray(0.7),type="l",ylim=c(0,max(simdata)),xlab="Years",ylab="Abundance")
  
  for(r in 2:ncol(simdata)){
    lines(c(1:101),simdata[,r],col=gray(0.7),type="l")
  }
}

PlotCloud(Default)


Extinction_byyear <- function(simdata){
  apply(simdata,1,function(t)  length(which(t==0)))/ncol(simdata)
}

plot(c(1:101),Extinction_byyear(Default),type="l",lwd=2,xlab="year",ylab="extinction risk")
abline(h=0.05,col="red",lwd=2)



hist(Default[nrow(Default),],xlab="Final abundance after 100 years",ylab="Number of replicates",main="")
abline(v=Init_N,col="green",lwd=2)


prob_decline <- round(length(which(Default[nrow(Default),]<Init_N))/ncol(Default),2)
cat("the probability of decline is: ", prob_decline)


Flood_prob <- 0.1     # 10% chance of major flood

Exctinction_risk <- function(simdata){
  length(which(simdata[nrow(simdata),]==0))/ncol(simdata)
}

flood_lambdas <- seq(0.9,0.1,by=-0.05)

all_scenarios <- numeric(length(flood_lambdas))
for(scenario in 1:length(flood_lambdas)){
  PVA <- PVAdemo(nreps,nyears,Init_N,R_max,K,Flood_prob,flood_lambdas[scenario])
  all_scenarios[scenario] <- Exctinction_risk(PVA)
}

plot(flood_lambdas,all_scenarios,type="p",cex=2,xlab="flood impact (lambda in flood year)",ylab="extinction risk")
abline(h=0.05,col="red",lwd=2)

