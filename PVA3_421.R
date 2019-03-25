
############################################################
####                                                    ####  
####  NRES 421, PVA                                     ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Loggerhead example                                ####
############################################################



##### specify projection matrix!

projection_matrix <- matrix(
  c(
    0,     0,      0,      4.665,      61.896,
    0.675, 0.703,  0,      0,          0,
    0,     0.047,  0.657,  0,          0,
    0,     0,      0.019,  0.682,      0,
    0,     0,      0,      0.061,      0.809
  )
  ,nrow=5,ncol=5,byrow=T
)

stagenames <- c("Hatchling","Small Juv","Large Juv","Subadult","Adult")
rownames(projection_matrix) <- stagenames
colnames(projection_matrix) <- stagenames
projection_matrix


Abundance_year0 <- c(2000,500,300,300,20)   # vector of initial abundances
Abundance_year0


# Run the matrix projection model!

nYears <- 100                                            # set the number of years to project
TMat <- projection_matrix                               # define the projection matrix
InitAbund <- Abundance_year0                            # define the initial abundance

  ## NOTE: the code below can be re-used without modification:
allYears <- matrix(0,nrow=nrow(TMat),ncol=nYears+1)     # build a storage array for all abundances!
allYears[,1] <- InitAbund  # set the year 0 abundance                                    
for(t in 2:(nYears+1)){   # loop through all years
  allYears[,t] <-  TMat %*% allYears[,t-1]
}
plot(1,1,pch="",ylim=c(0,max(allYears)),xlim=c(0,nYears+1),xlab="Years",ylab="Abundance",xaxt="n")  # set up blank plot
cols <- rainbow(5)    # set up colors to use
for(s in 1:5){
  points(allYears[s,],col=cols[s],type="l",lwd=2)     # plot out each life stage abundance, one at a time
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   # label the axis
legend("topright",col=cols,lwd=rep(2,3),legend=rownames(TMat),bty="n")  # put a legend on the plot


Year1 <- projection_matrix %*% Abundance_year0  # matrix multiplication!
  

projection_matrix <- matrix(
  c(
    0,     1.2,   3.1,
    0.4,   0,     0,
    0,     0.75,   0
  )
  ,nrow=3,ncol=3,byrow=T
)

projection_matrix

Abundance_year0 <- c(1000,0,0)
Abundance_year0
Year1 <- projection_matrix %*% Abundance_year0  # matrix multiplication!
Year1
Year2 <- projection_matrix %*% Year1  # matrix multiplication!
Year2
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
cols <- rainbow(3)    # set up colors to use
for(s in 1:3){
  points(allYears[s,],col=cols[s],type="l",lwd=2)     # plot out each life stage abundance, one at a time
}
axis(1,at=seq(1,nYears+1),labels = seq(0,nYears))   # label the axis
legend("topleft",col=cols,lwd=rep(2,3),legend=paste("Stage ",seq(1:nrow(TMat))))  # put a legend on the plot
