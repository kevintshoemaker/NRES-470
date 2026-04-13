


# hello -------------------------------------------------------------------


geom = function(a,p){
  sum(a * p^(0:1000))
}
geom(2,0.654)
2/(1-0.654)



# pop model ---------------------------------------------------------------




## globals  ---------

n_years = 50
r = 0.10   # 10% growth
N_0 = 100  # initial abundance


## set up ------

all_years = 0:n_years   # years 

lambda = 1 + r   # define lambda

N = numeric(length(all_years))  # initialize abundance vector

pop_dat = data.frame(
  Year = all_years,
  N = N
)

## run population simulation ---------


pop_dat$N[1] = N_0
for(i in 1:n_years){   # replicate across years
  pop_dat$N[i+1] = N[i] * lambda   # do the math!
}

# or

pop_dat$N = N_0 * lambda^all_years

pop_dat$type = "Discrete"

pop_dat2 = pop_dat
pop_dat2$type = "Continuous"
  
pop_dat2$N = N_0 * exp(r*all_years)

pop_dat <- rbind(pop_dat,pop_dat2)

## plot the population -------

library(ggplot2)

ggplot(pop_dat, aes(x=Year,y=N,col=type)) +
  geom_path(lwd=2) +
  theme_classic() 










