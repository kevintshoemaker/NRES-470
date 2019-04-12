
############################################################
####                                                    ####  
####  NRES 470/670, Lecture 15                          ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Parameter estimation                              ####
############################################################



###########
# Cormack-Jolly-Seber (CJS) model in R
###########

library(mra)      # install the 'mra' package if you haven't already done this!
data("dipper.data")
head(dipper.data,10)


data(dipper.histories)      # load data
ct <- as.factor( paste("T",1:ncol(dipper.histories), sep=""))      # specify which years were modeled
attr(ct,"nan")<-nrow(dipper.histories)  # assign the proper number of individuals

######
# Run the model!

dipper.cjs <- F.cjs.estim( ~tvar(ct,drop=c(1,2)), ~tvar(ct,drop=c(1,6,7)), dipper.histories )

#####
# Display the results

cat("survival estimates!\n")
dipper.cjs$s.hat[1,] 

cat("\n\n detection probability estimates!\n")
dipper.cjs$p.hat[1,] 


#######
# Compute the mean survival rate for the population

mean(dipper.cjs$s.hat[1,],na.rm=T)


#######
# Compute the environmental variablility in annual survival rates. 

sd(dipper.cjs$s.hat[1,],na.rm=T)

