


lam = 1
sd = 0.04
N0 = 20


png("pvaplot1.png",5,4,res = 400,units = "in")

{
N <- numeric(21)
N[1] <- N0
i=2
for(i in 2:21){
  thislam <- rnorm(1,lam,sd)
  N[i] <- rpois(1,lambda=N[i-1]*thislam) 
}
N
years <- 0:20

plot(years,N,type="l",col=rgb(0.2,0.2,0.2,0.8),lwd=2,ylim=c(0,100))


for(r in 1:20){
  N <- numeric(21)
  N[1] <- N0
  for(i in 2:21){
    thislam <- rnorm(1,lam,sd)
    N[i] <- rpois(1,lambda=N[i-1]*thislam) 
  }
 lines(years,N,type="l",col=rgb(0.2,0.2,0.2,0.8),lwd=2)
}

}

dev.off()



