
x <- c(1,rep(2:20,each=2))
y = numeric(length(x)) 
y[1] = 30
for(i in 2:length(x)){
  if(i%%2){
    y[i] = y[i-1]*1.07
  }else{
    y[i] <- y[i-1]
  }
}
#y
plot(x,y,type="l",ylab="Abundance N",xlab="Time (years)",lwd=2,col="brown")


