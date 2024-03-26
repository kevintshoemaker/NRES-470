

Tmat <- matrix(c(0.08,0.5,0.82,
                 0.35, 0.1, 0,
                 0, 0.65, 0.8),byrow=T,nrow=3)
Tmat

library(popbio)

lambda(Tmat)
ssd <- stable.stage(Tmat)

round(ssd*100)


c=1000
A=1:10
(c/2)*A^2
