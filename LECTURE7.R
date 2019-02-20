
############################################################
####                                                    ####  
####  NRES 470, Lecture 7                               ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Matrix population models                          ####
############################################################



#########
# Project the population at time 1

Year1 <- teasel_matrix %*% Initial_teasel   # note: the '%*%' denotes 'matrix multiplication' in R. We'll go through this more later.     
Year1
  

#########
# Project the population at time 2

thisYear <- Year1
nextYear <- teasel_matrix %*% thisYear
nextYear  # now we get the (age structured) population size at time 2!     


########
# Use a for loop to project the population dynamics for the next 10 years!

nYears <- 10
tenYears <- matrix(0,nrow=6,ncol=nYears+1)
rownames(tenYears) <- rownames(Initial_teasel)
colnames(tenYears) <- seq(0,10)
tenYears[,1] <- Initial_teasel 

for(t in 2:(nYears+1)){
  tenYears[,t] <-  teasel_matrix %*% tenYears[,t-1]
}

tenYears



###########
# Use the transition matrix to compute Lambda, or the finite rate of population growth!

Lambda <- as.numeric(round(eigen(teasel_matrix)$values[1],2))
Lambda



library(popbio)      # or... it's easier to use the 'popbio' library in R!
lambda(teasel_matrix)


##########
# Compute stable age distribution from the transition matrix!

SAD <- abs(as.numeric(round(eigen(teasel_matrix)$vectors[,1],3)))
SAD/sum(SAD)      # stable age distribution as a percentage of the total population


library(popbio)    # ... and it's even easier if we use the 'popbio' package...
stable.stage(teasel_matrix)



###################
# In class demo: convert an insightmaker model to a matrix projection model


###########
# First, we specify a blank transition matrix

TMat <- matrix(0,nrow=3,ncol=3)     # blank matrix with 3 rows and 3 columns
stagenames <- c("Juveniles","Subadults","Adults")  # name the rows and columns
rownames(TMat) <- stagenames
colnames(TMat) <- stagenames
TMat


#####
# fill in the top left element of the matrix

TMat[1,1] <- 0.1
TMat


#####
# update the second row, first column

TMat[2,1] <- 0.3
TMat


#####
# and keep filling it in...

TMat[,1] <- c(0.1,0.3,0)
TMat[,2] <- c(0,0.4,0.1)
TMat[,3] <- c(4,0,0.85)
TMat


######
# specify initial abundance vector

InitAbund <- c(40,0,0)
names(InitAbund) <- colnames(TMat)
InitAbund


#######
# Run the model for 40 years (using for loop)

nYears <- 40
allYears <- matrix(0,nrow=nrow(TMat),ncol=nYears+1)
rownames(allYears) <- rownames(TMat)
colnames(allYears) <- seq(0,nYears)
allYears[,1] <- InitAbund 

for(t in 2:(nYears+1)){
  allYears[,t] <-  TMat %*% allYears[,t-1]
}

allYears


#####
# and plot out the results!

plot(1,1,pch="",ylim=c(0,50),xlim=c(0,nYears+1),xlab="Years",ylab="Abundance",xaxt="n")
cols <- rainbow(3)
for(s in 1:3){
  points(allYears[s,],col=cols[s],type="l",lwd=2)
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))
legend("topleft",col=cols,lwd=rep(2,3),legend=rownames(allYears))

