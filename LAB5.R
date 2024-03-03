
#  NRES 470, Lab 5    ------------------------------------                          
#  University of Nevada, Reno                        
#  Stochasticity and Uncertainty        


# Review random number generators in R ----------------------

# Poisson distribution (often used for modeling total annual births in a population)

hist(rpois(10000,(100*0.8)),main="",freq = F,ylab="Probability",xlab="Possibilities (Total Births)")


# Binomial distribution (often used to represent total survivors or deaths in a population model )

hist(rbinom(10000,100,0.2),main="",xlab="Possibilities (Total Deaths)",freq = F,ylab="Probability")


# Normal distribution (often used to model environmental stochasticity)

hist(rnorm(10000,mean=1.25,sd=0.15),main="",freq=F,xlab="Possibilities (annual birth rates)",ylab="Probability")
curve(dnorm(x,1.25,0.15),0,3,lwd=3,add=T)


hist(rnorm(10000,mean=1.1,sd=0.3),main="",freq=F,xlab="Possibilities (annual birth rates)",ylab="Probability",xlim=c(0,3))
curve(dnorm(x,1.1,0.3),0,3,add=T,lwd=2)

