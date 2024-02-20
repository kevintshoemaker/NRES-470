
#  NRES 470, Lecture 7  --------------------          
#   Kevin Shoemaker                                   
#   University of Nevada, Reno                        
#   Matrix population models                   


# Teasel example -------------------------------

# Teasel example from Gotelli: summarizing a complex life history!

teasel <- read.csv("teaselmatrix1.csv", header=T)      # read in the teasel transition matrix from Gotelli
teasel <- teasel[,-1]                                  # remove the row names
teasel_matrix <- as.matrix(teasel)                     # convert to a matrix (from a data frame)
colnames(teasel_matrix) <- names(teasel)               # assign row and column names
rownames(teasel_matrix) <- names(teasel)
teasel_matrix                                          # print the matrix


# Summarize initial age-structured abundance as a matrix with one column

Initial_teasel <- matrix(c(1000,1500,200,300,600,25),ncol=1)         # initial population size (population vector; matrix with 1 column!)
rownames(Initial_teasel) <- rownames(teasel_matrix)                  # add row and column names
colnames(Initial_teasel) <- "Abundance"
Initial_teasel


# Project the population at time 1

Year1 <- teasel_matrix %*% Initial_teasel   # note: the '%*%' denotes 'matrix multiplication' in R. We'll go through this more later.     
Year1
  

# Project the population at time 2

thisYear <- Year1
nextYear <- teasel_matrix %*% thisYear
nextYear  # now we get the (age structured) population size at time 2!     


# Use a FOR loop to project the population dynamics for the next 10 years!

nYears <- 10
tenYears <- matrix(0,nrow=6,ncol=nYears+1)          # initialize storage array for recording age structured abundances for the next 10 years. 
rownames(tenYears) <- rownames(Initial_teasel)      # assign row and column names
colnames(tenYears) <- seq(0,10)
tenYears[,1] <- Initial_teasel                      # initialize the simulated abundances

# run the for loop!

for(t in 2:(nYears+1)){    # here we use 't' as our looping variable, but we could choose any name we want
  tenYears[,t] <-  teasel_matrix %*% tenYears[,t-1]     # perform matrix multiplication for each year of the simulation!
}

tenYears


# Matrix "tricks" for population ecology ---------------------------

# Use the transition matrix to compute Lambda, or the finite rate of population growth!

library(popbio)      # load the 'popbio' package in R!
Lambda <- lambda(teasel_matrix) 
Lambda

#   as.numeric(round(eigen(teasel_matrix)$values[1],2))  # this is an alternative method- if you don't want to use the 'popbio' package


# Compute stable age distribution from the transition matrix!

library(popbio)    # ... and it's even easier if we use the 'popbio' package...
SAD <- stable.stage(teasel_matrix)  
SAD      # stable age distribution as a percentage of the total population


# #abs(as.numeric(round(eigen(teasel_matrix)$vectors[,1],3)))  # alternative- doesn't use 'popbio'
# SAD/sum(SAD)


# Demo -------------------------
# In class demo: convert an insightmaker model to a matrix projection model

# First, we specify a blank transition matrix

TMat <- matrix(0,nrow=3,ncol=3)                    # create a blank matrix with 3 rows and 3 columns
stagenames <- c("Yearlings","Subadults","Adults")  # name the rows and columns
rownames(TMat) <- stagenames
colnames(TMat) <- stagenames
TMat                                               # now we have an all-zero transition matrix.


# fill in the top left element of the matrix

TMat[1,1] <- 0    # yearlings have birth rate of zero, so also a fecundity of zero!
TMat


# update the second row, first column (yearling survival, or transition from ylng to sub)

TMat[2,1] <- 0.3


# and the survival of adults (transition from adult to adult)

TMat[3,3] <- 0.85    # 85% survival


# transition from sub to sub (survive and stay subadult) 

TMat[2,2] <- 0.6-0.1    # 50% of subadults survive and stay juvenile


# transition from subadult to adult

TMat[3,2]  <- 0.1   # 10% of juveniles survive and transition to adult. 


# fill in the bottom left element of the matrix

TMat[3,1] <- 0    # yearlings (col 1) can't become adults (row 3) next year!
TMat


# fill in the bottom left element of the matrix

TMat[2,3] <- 0    # adults (col 3) can't become subadults (row 2) next year!
TMat


TMat[1,2] <- 3 * 0.3      # subadult fecundity term: birth rate multiplied by first-year survival

TMat[1,3] <- 7 * 0.3   # subadult fecundity term: birth rate multiplied by first-year survival

TMat

# alternatively, you can fill in the matrix like this:

TMat[,1] <- c(0,0.3,0)              # fill in the entire first column of the transition matrix
TMat[,2] <- c(3*0.3,0.5,0.1)        # fill in the entire second column of the transition matrix
TMat[,3] <- c(7*0.3,0,0.85)         # fill in the entire third column of the transition matrix
TMat


# specify initial abundance vector

InitAbund <- c(40,0,0)
names(InitAbund) <- colnames(TMat)
InitAbund


# Run the model for 50 years (using for loop)

nYears <- 50
allYears <- matrix(0,nrow=nrow(TMat),ncol=nYears+1)
rownames(allYears) <- rownames(TMat)
colnames(allYears) <- seq(0,nYears)
allYears[,1] <- InitAbund 

for(t in 2:(nYears+1)){
  allYears[,t] <-  TMat %*% allYears[,t-1]    # matrix multiplication!
}

allYears


# and plot out the results!

plot(1,1,pch="",ylim=c(0,100),xlim=c(0,nYears+1),xlab="Years",ylab="Abundance",xaxt="n")
cols <- rainbow(3)
for(s in 1:3){
  points(allYears[s,],col=cols[s],type="l",lwd=2)
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))
legend("topleft",col=cols,lwd=rep(2,3),legend=rownames(allYears),bty="n")

