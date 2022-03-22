

######## D-D demo

# variable definitions

bmax <- 0.7
dmin <- 0.3

a <- 0.001
c <- 0.0005


#########
# visualize density dependence

N <- seq(10,500,by=10)

b <- bmax - a*N
d <- dmin + c*N

plot(N,b,type="l",col="green",ylim=c(0,1),lwd=2)
lines(N,d,col="red",lwd=2)

legend("topright",col=c("green","red"),lwd=2,legend=c("births","deaths"))


popsize=500
for(i in 1:100){
  print(all(rbinom(popsize,1,0.75)))
}


red = 20
blue = 20
for(i in 1:100){
  red = rpois(1,red)
  blue = rpois(1,blue)
  cat(paste("red is :",red,"\n"))
  cat(paste("blue is :",blue,"\n"))
}




