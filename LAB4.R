
#  NRES 470, Lab 4 -------------------------------
#  Kevin Shoemaker                                 
#  University of Nevada, Reno              
#  Matrix population models           


# Matrix projection in R ---------------------

# Syntax for projecting abundance using a transition matrix (NOTE: this code won't run until we specify the terms on the right)

# Year1 <- projection_matrix %*% Abundance_year0  # matrix multiplication!
  

# First, build a simple age-structured projection matrix called pop_matrix

pop_matrix <- matrix(     # 
  c(
    0.25,     1.5,   1.5,
    0.4,   0,     0,
    0,     0.75,   0
  )
  ,nrow=3,ncol=3,byrow=T
)
pop_matrix    # print to the console to check!


# Then we specify initial abundances for the three age classes

init_abund <- c(1000,0,0)    # initial abundance vector
init_abund    # print to the console to check!


# Now we can run the code for real

# project year-1 abundance:

Year1 <- pop_matrix %*% init_abund  # matrix multiplication in R uses the symbol '%*%'
Year1


# Project year-2 abundance

Year2 <- pop_matrix %*% Year1  # matrix multiplication!
Year2


# Multi-year projection code ----------------------------

#  You may want to modify this code for the examples below:


# Set key parameters -----------------------

n_years <- 20                                            # set the number of years to project
pop_matrix <- matrix(     # 
  c(
    0.25,     1.5,   1.5,
    0.4,   0,     0,
    0,     0.75,   0
  )
  ,nrow=3,ncol=3,byrow=T
)
init_abund <- c(1000,0,0)                                # initial abundance vector
age_structured <- TRUE          # set to TRUE for Leslie matrix and FALSE for Lefkovitch 


# Use a FOR loop for multi-year projection  -------------

   # NOTE: the code below can be re-used without modification:

all_years = 0:n_years
n_stages = nrow(pop_matrix)

Nmat <- matrix(0,nrow=n_stages,ncol=length(all_years))     # build a storage array for all stages and all years!
Nmat[,1] <- init_abund  # set the year 0 abundance                                    
for(t in 2:(n_years+1)){   # loop through all years
  Nmat[,t] <-  pop_matrix %*% Nmat[,t-1]
}
plot(1,1,pch="",ylim=c(0,max(Nmat)),xlim=c(0,length(all_years)),xlab="Years",ylab="Abundance",xaxt="n")  # set up blank plot
cols <- rainbow(ncol(pop_matrix))    # set up colors to use
for(s in 1:ncol(pop_matrix)){
  points(Nmat[s,],col=cols[s],type="l",lwd=2)     # plot out each life stage abundance, one at a time
}
axis(1,at=seq(1,n_years+1),labels = seq(0,n_years))   # label the axis
if(age_structured){
  leg <-  paste("Age",seq(1,(ncol(pop_matrix))))
}else{
  leg <- paste("Stage",seq(1,ncol(pop_matrix))) 
}
legend("topleft",col=cols,lwd=rep(2,ncol(pop_matrix)),legend=leg,bty="n")  # put a legend on the plot


# Use 'popbio' package to compute lambda and SSD -----------

# Use the following line of code if you haven't installed 'popbio' yet. Once you've installed it, you can delete the line or comment this line out by adding a pound sign before the "i" in "install.packages"

# install.packages("popbio")   # uncomment this line to run - you only need to do this once

# Use the 'popbio' package to compute lambda (NOTE: you first have to install the popbio package! You only have to install the package once...)

library(popbio)   # load the 'popbio' package in R

lambda(pop_matrix)


# Use the 'popbio' package to compute the stable stage distribution!

stable.stage(pop_matrix)


# Construct a four-age matrix:

pop_matrix <- matrix(     # 
  c(
    0,     2.5,   1.2,    0.5,   
    0.3,     0,     0,      0,    
    0,       0.8,  0,      0,   
    0,       0,     0.55,   0  
  )
  ,nrow=4,ncol=4,byrow=T
)
pop_matrix

stmat <- read.csv("stage_matrix1.csv")
stmat <- as.matrix(stmat[,-1])
rownames(stmat) <- colnames(stmat)
stmat

# lambda(stmat) 

