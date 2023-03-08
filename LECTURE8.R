
#  NRES 470, Lecture 8       ---------------------------------                         
#   University of Nevada, Reno                        
#  Stochasticity and Uncertainty              


# Random number generation! ----------------

# define an arbitrary distribution

box <- c(rep(1,10),rep(2,5),rep(3,2))                                           # define what's in the lottery ball machine (10 "1" balls, 5 "2" balls and 2 "3" balls)
barplot(table(box)/sum(table(box)),ylab="probability",xlab="possibility")       # visualize the distribution of possibilities


# Discrete distributions ----------------------------

# binomial distribution ('coin flipping' distribution)

             # plot a discrete distribution!
xvals <- seq(0,10,1)
probs <- dbinom(xvals,10,prob=0.3)
names(probs) <- xvals
               
barplot(probs,ylab="Probability",xlab="Possibilities",main="Binomial distribution (discrete)")




# Poisson distribution

xvals <- seq(0,10,1)
probs <- dpois(xvals,lambda=2.2)     # POisson distribution
names(probs) <- xvals
               
barplot(probs,ylab="Probability",xlab="Possibilities",main="Poisson distribution (discrete)")



# CONTINUOUS DISTRIBUTIONS -----------------------------

# Uniform distribution

lower = 0
upper = 10

curve(dunif(x,lower,upper),0,10,ylab="Probability (density)",xlab="Possibilities",main="Uniform distribution (continuous)",ylim=c(0,1))   # probability density


# Normal distribution

mean = 7.1
stdev = 1.9

curve(dnorm(x,mean,stdev),0,15,ylab="Probability (density)",xlab="Possibilities",main="Normal distribution (continuous)")   # probability density


# Random number generation!   ------------------------


### Binomial random number generator
rbinom(1,size=10,prob=0.5)    # note: "size" is the number of coin flips, and "prob" is the probability of coming up 'heads'

### Poisson random number generator
rpois(1,lambda=4.1)     # note: "lambda" represents the mean (and variance!) of the Poisson distribution

### Uniform random number generator
runif(1,min=1,max=3.5)   # "min" and "max" are pretty obvious!

### Normal random number generator
rnorm(1,mean=3,sd=4.1)   # normal distribution is defined by "mean" and "sd" (standard deviation).


# Demonstration: use data to determine a distribution! --------------------

# Canvasback data- average number of eggs hatched per female for 20 years

hatch_perfem <- c(3.05, 1.45, 0.99, 3.24, 1.49, 1.70, 1.66, 2.32, 0.83, 2.41,
2.33, 1.68, 1.43, 2.74, 2.05, 3.13, 1.90, 3.69, 1.55, 2.79)

hist(hatch_perfem)


# Try to identify a normal distribution to represent the canvasback data

## first, plot a histogram of the data from the 20-year study
hist(hatch_perfem,freq=F,main="Histogram of avg number hatched per female",xlab="possibilities",ylab="probability",xlim=c(0,10),ylim=c(0,1))

## now, overlay a normal probability distribution with arbitrary parameters (mean and sd). This is just a starting point.

curve(dnorm(x,mean=4,sd=2),col="green",lty=2,lwd=2,add=T)

curve(dnorm(x,mean=1.8,sd=0.39),col="green",lty=2,lwd=2,add=T)    # try a different value...

#### Keep changing the value for 'mean' and 'sd' until you find best parameters to fit the data!

#### Once you find the best-fit parameters, generate 5 random numbers from this distribution using the "rnorm()" function in R

rnorm(5,mean=4,sd=1)    # for example! (remember to change the "mean" and "sd" parameters to the values you identified above!)

