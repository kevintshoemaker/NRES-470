
#  NRES 470/670, Lecture 16  
#     University of Nevada, Reno                
#     Competition                    



## LOTKA VOLTERRA COMPETITION EXAMPLE  -----------------------

## Params

Alpha <- 1.1
Beta <- 0.5
InitN1 <- 100
InitN2 <- 300
K1 <- 1000
K2 <- 450
Rmax1 <- 0.05
Rmax2 <- 0.3
Nyears <- 1000

System <- data.frame(n1 = rep(InitN1,(Nyears+1)),n2 = InitN2)

doYear <- function(prevyear){
  n1 <- prevyear[1] + prevyear[1] * Rmax1 * (1-((prevyear[1]+Alpha*prevyear[2])/(K1)))
  n2 <- prevyear[2] + prevyear[2] * Rmax2 * (1-((prevyear[2]+Beta*prevyear[1])/(K2)))
  return(c(n1,n2))
}

## Do simulation
for(i in 1:(Nyears+1)){
  System[1+i,] <- doYear(System[i,])
}



# visualize the initial abundances on the phase plane

plot(1,1,pch="",ylim=c(0,K2*1.5),xlim=c(0,K1*1.5),xlab="species 1",ylab="species 2")
points(System[1,],col="green",pch=20,cex=2)


# and the first 5 years...

plot(1,1,pch="",ylim=c(0,K2*1.5),xlim=c(0,K1*1.5),xlab="species 1",ylab="species 2")
points(System[1:6,],col="green",pch=20,cex=2)


# and many years!
plot(1,1,pch="",ylim=c(0,K2*1.5),xlim=c(0,K1*1.5),xlab="species 1",ylab="species 2")
points(System,col="green",pch=20,cex=2)


## LOTKA VOLTERRA COMPETITION EXAMPLE #2 ----------------------------

## Params

Alpha <- 0.3
Beta <- 0.2
InitN1 <- 100
InitN2 <- 300
K1 <- 1000
K2 <- 450
Rmax1 <- 0.05
Rmax2 <- 0.3
Nyears <- 1000

System <- data.frame(n1 = rep(InitN1,(Nyears+1)),n2 = InitN2)

doYear <- function(prevyear){
  n1 <- prevyear[1] + prevyear[1] * Rmax1 * (1-((prevyear[1]+Alpha*prevyear[2])/(K1)))
  n2 <- prevyear[2] + prevyear[2] * Rmax2 * (1-((prevyear[2]+Beta*prevyear[1])/(K2)))
  return(c(n1,n2))
}

## Do simulation
for(i in 1:(Nyears+1)){
  System[1+i,] <- doYear(System[i,])
}




# visualize on the phase plane

plot(1,1,pch="",ylim=c(0,K2*1.5),xlim=c(0,K1*1.5),xlab="species 1",ylab="species 2")
points(jitter(System[,1],500),jitter(System[,2],500),col="brown",pch=20,cex=0.3)
abline(h=K2,v=K1,col="gray",lwd=2,lty=2)


## LOTKA VOLTERRA COMPETITION EXAMPLE #3: multiple starting points   --------------------

## Params


InitN1 <- 1200
InitN2 <- 25
System1 <- data.frame(n1 = rep(InitN1,(Nyears+1)),n2 = InitN2)
## Do simulation
for(i in 1:(Nyears+1)){
  System1[1+i,] <- doYear(System1[i,])
}

InitN1 <- 500
InitN2 <- 100
System2 <- data.frame(n1 = rep(InitN1,(Nyears+1)),n2 = InitN2)
## Do simulation
for(i in 1:(Nyears+1)){
  System2[1+i,] <- doYear(System2[i,])
}


InitN1 <- 800
InitN2 <- 600
System3 <- data.frame(n1 = rep(InitN1,(Nyears+1)),n2 = InitN2)
## Do simulation
for(i in 1:(Nyears+1)){
  System3[1+i,] <- doYear(System3[i,])
}



# visualize on the phase plane

plot(1,1,pch="",ylim=c(0,K2*1.5),xlim=c(0,K1*1.5),xlab="species 1",ylab="species 2")
points(jitter(System[,1],500),jitter(System[,2],500),col="brown",pch=20,cex=0.3)
points(jitter(System1[,1],500),jitter(System1[,2],500),col="green",pch=20,cex=0.3)
points(jitter(System2[,1],500),jitter(System2[,2],500),col="red",pch=20,cex=0.3)
points(jitter(System3[,1],500),jitter(System3[,2],500),col="blue",pch=20,cex=0.3)

abline(h=K2,v=K1,col="gray",lwd=2,lty=2)


# Visualize phase plane with arrows!  --------------------

## SPECIFY MODEL AND INITIALIZE
#
## toggle switch function for phase arrow and nullcline plotting 

toggle = compiler::cmpfun(function(u,v,parms) {
  c( u*parms[1]*(1-(u+(parms[2]*v))/parms[3]), v*parms[4]*(1-(v+(parms[5]*u))/parms[6]) )
})

fun=toggle ## Our generic name for the system of equations to look at! ;-)
#
## toggle switch function for computing solution trajectories with deSolve::ode()

#Toggle = as.ode.func(toggle)
#
## parameter values?

Rmax1 <- 0.05
Alpha <- 0.3
K1 <- 1000
Rmax2 <- 0.3
Beta <- 0.2
K2 <- 450

parms=c(Rmax1,Alpha,K1,Rmax2,Beta,K2)

# toggle(100,100,parms)

xlim = c(5,2000)
ylim = c(5,1000)
new <- phasearrows.calc(toggle,xlim,ylim,resol=25,parms=parms)

plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="N1",ylab="N2")
phasearrows.draw(new)

#
## END MODEL SPECIFICATION AND INITIALIZATION
#######################################################################################


### example with phase-plane arrows

plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="N1",ylab="N2")
phasearrows.draw(new)
abline(K1/Alpha,-(K1/Alpha)/K1,col="red",lwd=3)   # species 1
abline(K2,-K2/(K2/Beta),col="blue",lwd=3)   # species 1


# Another example

Rmax1 <- 0.2
Alpha <- 1.1
K1 <- 1000
Rmax2 <- 0.2
Beta <- 0.9
K2 <- 500

parms=c(Rmax1,Alpha,K1,Rmax2,Beta,K2)

# toggle(100,100,parms)

xlim = c(5,1500)
ylim = c(5,1000)
new <- phasearrows.calc(toggle,xlim,ylim,resol=25,parms=parms)

plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="N1",ylab="N2")
phasearrows.draw(new)
abline(K1/Alpha,-(K1/Alpha)/K1,col="red",lwd=3)   # species 1
abline(K2,-K2/(K2/Beta),col="blue",lwd=3)   # species 2


# And another example!

Rmax1 <- 0.5
Alpha <- 1.05
K1 <- 890
Rmax2 <- 0.2
Beta <- 0.5
K2 <- 890

parms=c(Rmax1,Alpha,K1,Rmax2,Beta,K2)

# toggle(100,100,parms)

xlim = c(5,1500)
ylim = c(5,1000)
new <- phasearrows.calc(toggle,xlim,ylim,resol=25,parms=parms)

plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="N1",ylab="N2")
phasearrows.draw(new)
abline(K1/Alpha,-(K1/Alpha)/K1,col="red",lwd=3)   # species 1
abline(K2,-K2/(K2/Beta),col="blue",lwd=3)   # species 2


# And another!

Alpha <- 0.3
Beta <- 0.2
K1 <- 1000
K2 <- 450
Rmax1 <- 0.05
Rmax2 <- 0.3
Nyears <- 1000
ylim=c(0,K2*1.5)
xlim=c(0,K1*1.5)
plot(1,1,pch="",ylim=ylim,xlim=xlim,xlab="species 1",ylab="species 2")
points(jitter(System[,1],500),jitter(System[,2],500),col="brown",pch=20,cex=0.4)
points(jitter(System1[,1],500),jitter(System1[,2],500),col="green",pch=20,cex=0.4)
points(jitter(System2[,1],500),jitter(System2[,2],500),col="red",pch=20,cex=0.4)
points(jitter(System3[,1],500),jitter(System3[,2],500),col="blue",pch=20,cex=0.4)

parms=c(Rmax1,Alpha,K1,Rmax2,Beta,K2)
xlim = c(5,1500)
ylim = c(5,1000)
new <- phasearrows.calc(toggle,xlim,ylim,resol=15,parms=parms)
phasearrows.draw(new)
abline(h=K2,v=K1,col="gray",lwd=2,lty=2)
abline(K1/Alpha,-(K1/Alpha)/K1,col="red",lwd=2)   # species 1
abline(K2,-K2/(K2/Beta),col="blue",lwd=2)   # species 2


# And finally...

Rmax1 <- 0.2
Alpha <- 1.5
K1 <- 1000
Rmax2 <- 0.2
Beta <- 2
K2 <- 1500

parms=c(Rmax1,Alpha,K1,Rmax2,Beta,K2)

# toggle(100,100,parms)

xlim = c(5,1500)
ylim = c(5,1000)
new <- phasearrows.calc(toggle,xlim,ylim,resol=25,parms=parms)

plot(1,1,pch="",xlim=xlim,ylim=ylim,xlab="N1",ylab="N2")
phasearrows.draw(new)
abline(K1/Alpha,-(K1/Alpha)/K1,col="red",lwd=3)   # species 1
abline(K2,-K2/(K2/Beta),col="blue",lwd=3)   # species 2

