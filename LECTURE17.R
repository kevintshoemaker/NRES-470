
#  NRES 470/670, Lecture 17  ------------------------
#    University of Nevada, Reno           
#    Predator-prey    


# simple functional response

LVfuncresp <- function(V,alpha){
  alpha*V
}

curve(LVfuncresp(x,0.03),0,200,xlab="Victim abundance",ylab="Total prey eaten per predator",col="red",lwd=3)


# Numerical response

LVnumresp <- function(V,beta){
  beta*V
}

curve(LVnumresp(x,0.001),0,200,xlab="Victim abundance",ylab="Increase in r for predator population",col="blue",lwd=3)


# LOTKA VOLTERRA PREDATION EXAMPLE (on phase plane)

## Params

Alpha <- 0.001
Beta <- 0.001
InitPrey <- 100
InitPred <- 75
r <- 0.1
q <- 0.1
Nyears <- 100
step <- 0.1

System <- data.frame(prey = rep(InitPrey,(Nyears+1)*10),pred = InitPred)

doYear <- function(prevyear){
  n1 <- prevyear[1] + r*prevyear[1]*step - Alpha*prevyear[1]*prevyear[2]*step
  n2 <- prevyear[2] + Beta*prevyear[1]*prevyear[2]*step - q*prevyear[2]*step 
  return(c(n1,n2))
}

## Do simulation
for(i in 1:(Nyears*10+1)){
  System[1+i,] <- doYear(System[i,])
}




plot(1,1,pch="",ylim=c(0,200),xlim=c(0,200),xlab="prey",ylab="predators")
points(System[seq(1,1000,10),],col="green",type="p",pch=20,cex=0.85)
points(System[1,],col="blue",pch=20,cex=3)


# LOTKA VOLTERRA PREDATION EXAMPLE

## Params

Alpha <- 0.005
Beta <- 0.005
InitPrey <- 100
InitPred <- 75
r <- 0.2
q <- 0.1
Nyears <- 100
step <- 0.1

System <- data.frame(prey = rep(InitPrey,(Nyears+1)*10),pred = InitPred)

doYear <- function(prevyear){
  n1 <- prevyear[1] + r*prevyear[1]*step - Alpha*prevyear[1]*prevyear[2]*step
  n2 <- prevyear[2] + Beta*prevyear[1]*prevyear[2]*step - q*prevyear[2]*step 
  return(c(n1,n2))
}

## Do simulation
for(i in 1:(Nyears*10+1)){
  System[1+i,] <- doYear(System[i,])
}


plot(1,1,pch="",ylim=c(0,200),xlim=c(0,200),xlab="prey",ylab="predators")
points(System[seq(1,1000,10),],col="green",type="p",pch=20,cex=0.85)
points(System[1,],col="blue",pch=20,cex=3)


plot(1,1,pch="",ylim=c(0,200),xlim=c(0,200),xlab="Prey",ylab="Predators")
points(jitter(System[,1],200),jitter(System[,2],200),col="brown",pch=20,cex=0.3)
#abline(h=K2,v=K1,col="gray",lwd=2,lty=2)


## LOTKA VOLTERRA PREDATION EXAMPLE

## Params


InitN1 <- 120
InitN2 <- 25
System1 <- data.frame(n1 = rep(InitN1,(Nyears*10+1)),n2 = InitN2)
## Do simulation
for(i in 1:(Nyears*10+1)){
  System1[1+i,] <- doYear(System1[i,])
}

InitN1 <- 200
InitN2 <- 100
System2 <- data.frame(n1 = rep(InitN1,(Nyears*10+1)),n2 = InitN2)
## Do simulation
for(i in 1:(Nyears*10+1)){
  System2[1+i,] <- doYear(System2[i,])
}


InitN1 <- 50
InitN2 <- 200
System3 <- data.frame(n1 = rep(InitN1,(Nyears*10+1)),n2 = InitN2)
## Do simulation
for(i in 1:(Nyears*10+1)){
  System3[1+i,] <- doYear(System3[i,])
}



plot(1,1,pch="",ylim=c(0,400),xlim=c(0,400),xlab="species 1",ylab="species 2")
points(jitter(System[,1],500),jitter(System[,2],500),col="brown",pch=20,cex=0.3)
points(jitter(System1[,1],500),jitter(System1[,2],500),col="green",pch=20,cex=0.3)
points(jitter(System2[,1],500),jitter(System2[,2],500),col="red",pch=20,cex=0.3)
points(jitter(System3[,1],500),jitter(System3[,2],500),col="blue",pch=20,cex=0.3)
#abline(h=K2,v=K1,col="gray",lwd=2,lty=2)


plot(seq(1,100,length=nrow(System)), System[,1], xlab="Time", ylab="Abundance",type="l",col="orange",lwd=2,ylim=c(0,160),xlim=c(0,95))
points(seq(1,100,length=nrow(System)), System[,2], xlab="Time", ylab="Abundance",type="l",col="green",lwd=2,lty=2)
legend("top",lwd=c(2,2),lty=c(1,2),col=c("orange","green"),legend=c("prey","predator"),bty="n")



## SPECIFY MODEL AND INITIALIZE
#
## toggle switch function for phase arrow and nullcline plotting 

toggle = compiler::cmpfun(function(u,v,parms) {
  c( u*parms[1]-parms[2]*u*v, parms[3]*u*v-parms[4]*v )
})

fun=toggle ## Our generic name for the system of equations to look at! ;-)
#
## toggle switch function for computing solution trajectories with deSolve::ode()

#Toggle = as.ode.func(toggle)
#
## parameter values?

Alpha <- 0.005
Beta <- 0.005
r <- 0.2
q <- 0.1

parms=c(r,Alpha,Beta,q)

# toggle(100,100,parms)

xlim = c(5,200)
ylim = c(5,200)
new <- phasearrows.calc(toggle,xlim,ylim,resol=25,parms=parms)

plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="Prey",ylab="Predators")
phasearrows.draw(new)

#
## END MODEL SPECIFICATION AND INITIALIZATION
#######################################################################################


plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="Prey",ylab="Predators")
phasearrows.draw(new)
abline(h=r/Alpha,col="red",lwd=3)   # prey
abline(v=q/Beta,col="blue",lwd=3)   # pred


## SPECIFY MODEL AND INITIALIZE
#
## toggle switch function for phase arrow and nullcline plotting 

toggle = compiler::cmpfun(function(u,v,parms) {
  c( u*parms[1]-parms[2]*u*v - parms[3]*u^2, parms[4]*u*v-parms[5]*v )
})

fun=toggle ## Our generic name for the system of equations to look at! ;-)
#
## toggle switch function for computing solution trajectories with deSolve::ode()

#Toggle = as.ode.func(toggle)
#
## parameter values?

Alpha <- 0.001
Beta <- 0.005
r <- 0.2
q <- 0.1
c <- 0.008

parms=c(r,Alpha,c,Beta,q)

# toggle(100,100,parms)

xlim = c(5,200)
ylim = c(5,200)
new <- phasearrows.calc(toggle,xlim,ylim,resol=25,parms=parms)

plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="Prey",ylab="Predators")
phasearrows.draw(new)

#
## END MODEL SPECIFICATION AND INITIALIZATION
#######################################################################################


plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="Prey",ylab="Predators")
phasearrows.draw(new)
abline((r/Alpha),-(c/Alpha),col="red",lwd=3)   # prey
abline(v=q/Beta,col="blue",lwd=3)   # pred


## Params

Alpha <- 0.001
Beta <- 0.005
r <- 0.2
q <- 0.1
c <- 0.008
InitPrey <- 100
InitPred <- 75
Nyears <- 100
step <- 0.1

System <- data.frame(prey = rep(InitPrey,(Nyears+1)*10),pred = InitPred)

doYear <- function(prevyear){
  n1 <- prevyear[1] + r*prevyear[1]*step - Alpha*prevyear[1]*prevyear[2]*step - c*prevyear[1]^2*step
  n2 <- prevyear[2] + Beta*prevyear[1]*prevyear[2]*step - q*prevyear[2]*step 
  return(c(n1,n2))
}

## Do simulation
for(i in 1:(Nyears*10+1)){
  System[1+i,] <- doYear(System[i,])
}

##### LOTKA VOLTERRA PREDATION EXAMPLE

## Params


InitN1 <- 100
InitN2 <- 200
System1 <- data.frame(n1 = rep(InitN1,(Nyears*10+1)),n2 = InitN2)
## Do simulation
for(i in 1:(Nyears*10+1)){
  System1[1+i,] <- doYear(System1[i,])
}

InitN1 <- 100
InitN2 <- 100
System2 <- data.frame(n1 = rep(InitN1,(Nyears*10+1)),n2 = InitN2)
## Do simulation
for(i in 1:(Nyears*10+1)){
  System2[1+i,] <- doYear(System2[i,])
}


InitN1 <- 50
InitN2 <- 20
System3 <- data.frame(n1 = rep(InitN1,(Nyears*10+1)),n2 = InitN2)
## Do simulation
for(i in 1:(Nyears*10+1)){
  System3[1+i,] <- doYear(System3[i,])
}

plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="Prey",ylab="Predators")
phasearrows.draw(new)
abline((r/Alpha),-(c/Alpha),col="red",lwd=3)   # prey
abline(v=q/Beta,col="blue",lwd=3)   # pred
points(jitter(System[,1],500),jitter(System[,2],500),col="brown",pch=20,cex=0.3)
points(jitter(System1[,1],500),jitter(System1[,2],500),col="green",pch=20,cex=0.3)
points(jitter(System2[,1],500),jitter(System2[,2],500),col="red",pch=20,cex=0.3)
points(jitter(System3[,1],500),jitter(System3[,2],500),col="blue",pch=20,cex=0.3)


plot(seq(1,100,length=nrow(System)), System[,1], xlab="Time", ylab="Abundance",type="l",col="orange",lwd=2,ylim=c(0,160),xlim=c(0,95))
points(seq(1,100,length=nrow(System)), System[,2], xlab="Time", ylab="Abundance",type="l",col="green",lwd=2,lty=2)
legend("top",lwd=c(2,2),lty=c(1,2),col=c("orange","green"),legend=c("prey","predator"),bty="n")


TypeIfuncresp <- function(V,alpha){
  alpha*V
}

curve(TypeIfuncresp(x,0.1),0,500,xlab="Victim abundance",ylab="Total prey eaten per predator",col="red",lwd=3)


TypeIIfuncresp <- function(V,alpha,h){
  (alpha*V)/(1+alpha*h*V)
}

curve(TypeIIfuncresp(x,0.1,0.1),0,500,xlab="Victim abundance",ylab="Total prey eaten per predator",col="red",lwd=3)


TypeIIIfuncresp <- function(V,alpha,h,k){
  (alpha*V^k)/(1+alpha*h*V^k)
}

curve(TypeIIIfuncresp(x,0.0005,0.1,1.6),0,1000,xlab="Victim abundance",ylab="Total prey eaten per predator",col="red",lwd=3)

