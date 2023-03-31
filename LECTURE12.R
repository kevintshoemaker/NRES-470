  
#  NRES 470/670, Lecture 12 -----------------------           
#   University of Nevada, Reno         
#   Population Viability Analysis 


# Demonstration PVA ---------------------------------

# STEP 1: conceptualize life history (we are modeling this population as a simple, single-stage stochastic model with density dependence)

# STEP 2: parameterize the model ---------------------------------

# Basic life history parameters

R_max <- 1.15       # Maximum rate of growth
Init_N <- 51        # Initial abundance (not stage structured)
K <- 175            # Carrying capacity

# Environmental stochasticity ------------------------------------

SD_anngrowth <- 0.11  # standard deviation of annual growth rate

# Density-dependence (Ricker model) ----------------------------------

Ricker <- function(prev_abund){       # this is a function for computing next-year abundance -- includes env stochasticity
  prev_abund * exp(log(rnorm(1,R_max,SD_anngrowth))*(1-(prev_abund/K)))
}

# Catastrophe -------------------------

Flood_prob <- 0.05      # 5% chance of major flood
Flood_lambda <- 0.25    # 25% of population can survive a flood 


# STEP 3: add spatial structure (not applicable here)

# STEP 4: simulate!  -------------------------------

# Basic simulation parameters

nyears <- 100     # number of years
nreps <- 500      # number of replicates


PVAdemo <- function(nreps,nyears,Init_N,R_max,K,Flood_prob,Flood_lambda){
  #browser()
  PopArray2 <- array(0,dim=c((nyears+1),nreps))   # set up storage array
  
  ## start looping through replicates
  
  for(rep in 1:nreps){
    
    # set initial abundance
    PopArray2[1,rep] <- Init_N     # set the initial abundance
    
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

### Run the PVA! -------------------

Default <- PVAdemo(nreps,nyears,Init_N,R_max,K,Flood_prob,Flood_lambda)


# STEP 5: results  ------------------------- 

# Graphical visualization

PlotCloud <- function(simdata){
  plot(c(1:101),simdata[,1],col=gray(0.7),type="l",ylim=c(0,max(simdata)),xlab="Years",ylab="Abundance")
  
  for(r in 2:ncol(simdata)){
    lines(c(1:101),simdata[,r],col=gray(0.7),type="l")
  }
}

PlotCloud(Default)


# Visualize extinction rates over time

Extinction_byyear <- function(simdata){
  apply(simdata,1,function(t)  length(which(t==0)))/ncol(simdata)
}

plot(c(1:101),Extinction_byyear(Default),type="l",lwd=2,xlab="year",ylab="extinction risk")
abline(h=0.05,col="red",lwd=2)




# visualize final abundance after 100 years relative to initial abundance

hist(Default[nrow(Default),],xlab="Final abundance after 100 years",ylab="Number of replicates",main="")
abline(v=Init_N,col="green",lwd=2)


# plot probabilities of different severities of decline

declines <- seq(0,100,by=1)
declineprob <- numeric(length(declines))

for(s in 1:length(declines)){
  declineprob[s] <- length(which(Default[nrow(Default),]<(Init_N-(declines[s]/100)*Init_N)))/ncol(Default)
}

plot(declines,declineprob,type="l",lwd=2,xlab="Decline threshold (percent)",ylab="Probability of falling below threshold")

abline(v=25,col="red",lwd=2)


# Plot extinction risk as a function of flood severity

Exctinction_risk <- function(simdata){
  length(which(simdata[nrow(simdata),]==0))/ncol(simdata)
}

flood_lambdas <- seq(0.9,0.1,by=-0.05)

all_scenarios <- numeric(length(flood_lambdas))
for(scenario in 1:length(flood_lambdas)){
  PVA <- PVAdemo(nreps,nyears,Init_N,R_max,K,Flood_prob,flood_lambdas[scenario])
  all_scenarios[scenario] <- Exctinction_risk(PVA)
}

plot(1-flood_lambdas,all_scenarios,type="p",cex=2,xlab="flood impact (mortality in flood year)",ylab="extinction risk")
abline(h=0.05,col="red",lwd=2)



