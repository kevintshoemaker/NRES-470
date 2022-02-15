
############################################################
####                                                    ####  
####  NRES 470/670, lab 7                               ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Parameter estimation                              ####
############################################################



##########
# CLOSED POPULATION MODELS
##########

# Note: I recommend doing these lab activities in EXCEL, but you're more than welcome to try to do it in R (instead of handing in your work as an excel spreadsheet, just hand in your R code!)

###########
# First, load up the CSV file (you need to download it first)

# setwd()     # remember to set your working directory before you read in the data! 

head(read.csv("simple_closed.csv"))



##########
# OPEN POPULATION MODELS
##########

library(marked)    # remember to install the 'marked' package if you haven't already done this

#?crm  # to get help on the main parameter estimation function in "marked"

# library(help="marked")

# vignette("markedVignette","marked")

##########
# load data!

data(dipper)
dipper.male <- dipper[which(dipper$sex=="Male"),]   # extract only the males, to compare with the Mark example


#############
# Process data

dipper.proc=process.data(dipper.male,model="cjs",begin.time=1)  # Helper function- process the data for CJS model
dipper.ddl=make.design.data(dipper.proc)    # another helper function- process data!

##########
# Fit models

########
# fit time-varying cjs model

mod.Phit.pt <- crm(dipper.proc,dipper.ddl,model.parameters=list(Phi=list(formula=~time),p=list(formula=~time)),method="Nelder-Mead",hessian = T)

mod.Phit.pt   # print out model
mod.Phit.pt$results$AIC       # extract AIC

########
# fit time-invariant cjs model

mod.Phidot.pdot <- crm(dipper.proc,dipper.ddl,model.parameters = list(Phi=list(formula=~1),p=list(formula=~1)),method="Nelder-Mead",hessian = TRUE)

mod.Phidot.pdot
mod.Phidot.pdot$results$AIC

###########
# compare all models with AIC
###########

######
# Set up models to run (must have either "Phi." or "p." in the name)
Phi.dot <- list(formula=~1)       
Phi.time <- list(formula=~time)
p.dot <- list(formula=~1)
p.time <- list(formula=~time)

cml=create.model.list(c("Phi","p"))    # create list of all models to run

######
# Run all models

allmodels <- crm.wrapper(cml,data=dipper.proc, ddl=dipper.ddl,external=FALSE,accumulate=FALSE,method="Nelder-Mead",hessian=TRUE)
allmodels

#######
# get parameter estimates and confidence intervals for best model

allmodels[[1]]

#######
# make predictions and plot them. 

predict(allmodels[[1]])$Phi

Phi_by_year <- predict(allmodels[[3]])$Phi    # predict Phi for all years (based on the best Phi(t) model)

library(Hmisc)   #load Hmisc package- has a nice error bar function
plot(1:nrow(Phi_by_year),Phi_by_year$estimate,xlab="Year",ylab="Survival",ylim=c(0,1),main="Variability in Survival, dipper demo")
errbar(1:nrow(Phi_by_year),Phi_by_year$estimate,Phi_by_year$ucl,Phi_by_year$lcl,add=T)




