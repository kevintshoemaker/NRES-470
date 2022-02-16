
############################################################
####                                                    ####  
####  NRES 470, Lab 4                                   ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Matrix population models                          ####
############################################################



########
# Syntax for projecting abundance using a transition matrix (NOTE: this code won't run until we specify the terms on the right)

Year1 <- projection_matrix %*% Abundance_year0  # matrix multiplication!
  

######
# First, build a simple projection matrix

projection_matrix <- matrix(
  c(
    0,     1.2,   3.1,
    0.4,   0,     0,
    0,     0.75,   0
  )
  ,nrow=3,ncol=3,byrow=T
)

projection_matrix


######
# Then we specify initial abundances for the three age classes

Abundance_year0 <- c(1000,0,0)
Abundance_year0


######
# Now we can run the code for real

Year1 <- projection_matrix %*% Abundance_year0  # matrix multiplication!
Year1


########
# Project another year

Year2 <- projection_matrix %*% Year1  # matrix multiplication!
Year2


##########
# Use a FOR loop to project many years into the future

nYears <- 20                                            # set the number of years to project
TMat <- projection_matrix                               # define the projection matrix
InitAbund <- Abundance_year0                            # define the initial abundance

  ## NOTE: the code below can be re-used without modification:
allYears <- matrix(0,nrow=nrow(TMat),ncol=nYears+1)     # build a storage array for all abundances!
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
legend("topleft",col=cols,lwd=rep(2,ncol(TMat)),legend=paste("Stage ",seq(1:ncol(TMat))))  # put a legend on the plot


############
# Use the 'popbio' package to compute lambda (NOTE: you first have to install the popbio package! You only have to install the package once...)

library(popbio)

lambda(projection_matrix)


#############
# Use this code if you haven't installed 'popbio' yet. Once you've installed it, you can delete the line or comment this line out by adding a pound sign before the "i" in "install.packages"

install.packages("popbio")



###### 
# Use the 'popbio' package to compute the stable age distribution!

stable.stage(projection_matrix)

stmat <- read.csv("stage_matrix1.csv")
stmat <- as.matrix(stmat[,-1])
rownames(stmat) <- colnames(stmat)
stmat

# lambda(stmat) 

