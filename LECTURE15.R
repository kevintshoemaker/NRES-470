
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

library(marked)      # install the 'marked' package if you haven't already done this!
data("dipper")
head(dipper,10)


##########
# load data!

data(dipper)

#############
# Process data

dipper.proc=process.data(dipper,model="cjs",begin.time=1)  # Helper function- process the data for CJS model
dipper.ddl=make.design.data(dipper.proc)    # another helper function- process data!

##########
# Fit models

# fit time-varying cjs model

mod.Phit.pt <- crm(dipper.proc,dipper.ddl,model.parameters=list(Phi=list(formula=~time),p=list(formula=~time)),method="Nelder-Mead",hessian = T)

mod.Phit.pt   # print out model
mod.Phit.pt$results$AIC       # extract AIC

########
# fit time-invariant cjs model

mod.Phidot.pdot <- crm(dipper.proc,dipper.ddl,model.parameters = list(Phi=list(formula=~1),p=list(formula=~1)),method="Nelder-Mead",hessian = TRUE)

mod.Phidot.pdot
mod.Phidot.pdot$results$AIC

########
# fit sex-dependent cjs model

mod.Phisex.psex <- crm(dipper.proc,dipper.ddl,model.parameters = list(Phi=list(formula=~sex),p=list(formula=~sex)),method="Nelder-Mead",hessian = TRUE)

mod.Phisex.psex
mod.Phisex.psex$results$AIC

###########
# compare all models with AIC
###########

######
# Set up models to run (must have either "Phi." or "p." in the name)
Phi.dot <- list(formula=~1)       
Phi.time <- list(formula=~time)
Phi.sex <- list(formula=~sex)
Phi.timesex <- list(formula=~sex+time)
p.dot <- list(formula=~1)
p.time <- list(formula=~time)
p.sex <- list(formula=~sex)
p.timesex <- list(formula=~sex+time)

cml=create.model.list(c("Phi","p"))    # create list of all models to run

######
# Run all models

allmodels <- crm.wrapper(cml,data=dipper.proc, ddl=dipper.ddl,external=FALSE,accumulate=FALSE,method="Nelder-Mead",hessian=TRUE)

######
# AIC model selection

allmodels

#######
# get parameter estimates and confidence intervals for best model

allmodels[[1]]

allmodels[[11]]

#######
# make predictions and plot them. 

predict(allmodels[[1]])$Phi

Phi_by_year <- predict(allmodels[[11]])$Phi    # predict Phi for all years (based on the best Phi(t) model)

library(Hmisc)   #load Hmisc package- has a nice error bar function
plot(1:nrow(Phi_by_year),Phi_by_year$estimate,xlab="Year",ylab="Survival",ylim=c(0,1),main="Variability in Survival, dipper demo")
errbar(1:nrow(Phi_by_year),Phi_by_year$estimate,Phi_by_year$ucl,Phi_by_year$lcl,add=T)


#######
# Compute the mean survival rate for the population

mean(Phi_by_year$estimate)


#######
# Compute the environmental variablility in annual survival rates. 

sd(Phi_by_year$estimate)

