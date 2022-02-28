
############################################################
####                                                    ####  
####  NRES 470, Lab 5                                   ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Stochasticity and Uncertainty                     ####
############################################################



###############
# Review random number generators:

# Poisson distribution

hist(rpois(10000,(100*0.8)),main="",freq = F,ylab="Probability",xlab="Possibilities (Total Births)")


# Binomial distribution

hist(rbinom(10000,100,0.2),main="",xlab="Possibilities (Total Deaths)",freq = F,ylab="Probability")


# Normal distribution

hist(rnorm(10000,mean=1.25,sd=0.15),main="",freq=F,xlab="Possibilities (annual birth rates)",ylab="Probability")


hist(rnorm(10000,mean=1.1,sd=0.4),main="",xlab="Possibilities (annual birth rates)",freq = F,ylab="Probability")

