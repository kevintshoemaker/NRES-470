
#  NRES 470, Lab 4 -------------------------------
#  Kevin Shoemaker                                 
#  University of Nevada, Reno              
#  Matrix population models           


# Matrix projection in R ---------------------

# Syntax for projecting abundance using a transition matrix (NOTE: this code won't run until we specify the terms on the right)

# Year1 <- projection_matrix %*% Abundance_year0  # matrix multiplication!
  

# First, build a simple age-structured projection matrix called TMat

TMat <- matrix(     # 
  c(
    0.25,     1.5,   1.5,
    0.4,   0,     0,
    0,     0.75,   0
  )
  ,nrow=3,ncol=3,byrow=T
)
TMat    # print to the console to check!


# Then we specify initial abundances for the three age classes

InitAbund <- c(1000,0,0)    # initial abundance vector
InitAbund    # print to the console to check!


# Now we can run the code for real

# project year-1 abundance:

Year1 <- TMat %*% InitAbund  # matrix multiplication in R uses the symbol '%*%'
Year1


# Project year-2 abundance

Year2 <- TMat %*% Year1  # matrix multiplication!
Year2


# Multi-year projection code ----------------------------

#  You may want to modify this code for the examples below:


# Set key parameters -----------------------

nYears <- 20                                            # set the number of years to project
TMat <- matrix(     # 
  c(
    0.25,     1.5,   1.5,
    0.4,   0,     0,
    0,     0.75,   0
  )
  ,nrow=3,ncol=3,byrow=T
)
InitAbund <- c(1000,0,0)                                # initial abundance vector
AgeStructured <- TRUE          # set to TRUE for Leslie matrix and FALSE for Lefkovitch 


# Use a FOR loop for multi-year projection  -------------

   # NOTE: the code below can be re-used without modification:

allYears <- matrix(0,nrow=nrow(TMat),ncol=nYears+1)     # build a storage array for all stages and all years!
allYears[,1] <- InitAbund  # set the year 0 abundance                                    
for(t in 2:(nYears+1)){   # loop through all years
  allYears[,t] <-  TMat %*% allYears[,t-1]
}
plot(1,1,pch="",ylim=c(0,max(allYears)),xlim=c(0,nYears+1),xlab="Years",ylab="Abundance",xaxt="n")  # set up blank plot
cols <- rainbow(ncol(TMat))    # set up colors to use
for(s in 1:ncol(TMat)){
  points(allYears[s,],col=cols[s],type="l",lwd=2)     # plot out each life stage abundance, one at a time
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   # label the axis
if(AgeStructured){
  leg <-  paste("Age",seq(1,(ncol(TMat))))
}else{
  leg <- paste("Stage",seq(1,ncol(TMat))) 
}
legend("topleft",col=cols,lwd=rep(2,ncol(TMat)),legend=leg,bty="n")  # put a legend on the plot


# Use 'popbio' package to compute lambda and SSD -----------

# Use the following line of code if you haven't installed 'popbio' yet. Once you've installed it, you can delete the line or comment this line out by adding a pound sign before the "i" in "install.packages"

# install.packages("popbio")   # uncomment this line to run - you only need to do this once

# Use the 'popbio' package to compute lambda (NOTE: you first have to install the popbio package! You only have to install the package once...)

library(popbio)   # load the 'popbio' package in R

lambda(TMat)


# Use the 'popbio' package to compute the stable stage distribution!

stable.stage(TMat)


# Construct a four-age matrix:

TMat <- matrix(     # 
  c(
    0,     1.9,   1.1,    0.4,   
    0.4,     0,     0,      0,    
    0,       0.75,  0,      0,   
    0,       0,     0.65,   0  
  )
  ,nrow=4,ncol=4,byrow=T
)
TMat

stmat <- read.csv("stage_matrix1.csv")
stmat <- as.matrix(stmat[,-1])
rownames(stmat) <- colnames(stmat)
stmat

# lambda(stmat) 

