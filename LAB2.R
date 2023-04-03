
Density <- seq(0,15000,10)  # create a sequence of numbers from 0 to 15000, representing a range of population densities

## CONSTANTS

b_max <- 0.9   # maximum reproduction (at low densities)
d_min <- 0.2  # minimum mortality (at low densities)

a <- 0.00009    # D-D terms
c <- 0.00006

b <- b_max - a*Density
d <- d_min + c*Density

K <- (b_max-d_min)/(a+c)   # compute carrying capacity

plot(Density,b,type="l",col="green",lwd=2,ylim=c(0,2),main="Density-Dependence!",xlim=c(0,15000),ylab="Vital rate (b or d)")
points(Density,d,type="l",col="red",lwd=2)
legend("topleft",col=c("green","red"),lty=c(1,1),legend=c("per-capita birth rate","per-capita mortality"),bty="n")

  

