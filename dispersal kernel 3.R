# Dispersal kernel for wind dispersed herb ####
library(LaplacesDemon)
library(tidyverse)
options(scipen = 999)

# Basic kernels ####
# Conyza
wind <- as.data.frame(rcauchy(n=1000,
															location=0.00000249,scale=2.7658))
colnames(wind) <- c("Distance")

wind <- wind %>% filter(Distance>0)
hist(wind$Distance, breaks = 50)

# Trillium 
animal <- as.data.frame(rpe(n=1000,sigma=188.36,kappa=0.7341))
colnames(animal) <- c("Distance")

animal <- animal %>% filter(Distance>0)
hist(animal$Distance, breaks = 50)

# Mixture ####
p.wind <- 0.5
p.animal <- 1-p.wind

n <- 1000
mix.sims.out <- rep(0,n)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind){
    #pick wind kernel:
    mix.sims.out[i] <- rcauchy(n=1,
															location=0.00000249,scale=2.7658)
  }else{
    #pick animal kernel: 
    mix.sims.out[i] <- rpe(n=1,sigma=188.36,kappa=0.7341)
  }
}

mix.sims.out <- as.data.frame(mix.sims.out)
colnames(mix.sims.out) <- c("Distance")
mix.sims.out <- mix.sims.out %>% filter(Distance>0)
hist(mix.sims.out$Distance, breaks = 50)



# Summary statistics ####
# delta median by proportion 

median(wind$Distance)
mean(wind$Distance)
quantile(wind$Distance, probs=c(.5, .95), na.rm = T)

median(mix.sims.out$Distance)
mean(mix.sims.out$Distance)
quantile(mix.sims.out$Distance, probs=c(.5, .95), na.rm = T)



