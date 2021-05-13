library(LaplacesDemon)
# Exponential power distribution
# sigma = alpha = scale
# kappa = beta = kurtosis

# Dispersal kernel for wind dispersed herb ####
sigma.wind <- 0.000047
kappa.wind <- 0.2336
mu.wind <- (sigma.wind)*((gamma(3/kappa.wind))/(gamma(2/kappa.wind)))


x <- seq(0,1000)
wind.pdf <- dpe(x=x,mu=mu.wind, sigma=sigma.wind, kappa=kappa.wind,log = TRUE)
plot(trial1.pdf,log='x')

# Dispersal kernel for animal dispersed seeds ####

sigma.animal <- 0.00000001
kappa.animal <- 0.1339
mu.animal <- (sigma.animal)*((gamma(3/kappa.animal))/(gamma(2/kappa.animal)))

x <- seq(0,1000)
trial2.pdf <- dpe(x=x,mu=mu.animal, sigma=sigma.animal, kappa=kappa.animal,log = TRUE)
plot(trial2.pdf,log='x')

# Simulating dispersal distances from the wind kernel ####
n <- 10000
wind.sims.out <- rep(0,n)
for(i in 1:n){
	wind.sims.out[i] <- rpe(n=1, mu=mu.wind, 
    									 sigma=sigma.wind, kappa=kappa.wind)
}

hist(wind.sims.out)

# Simulating dispersal distances from the mixture of kernels ####
p.wind <- 0.90
p.animal <- 1-p.wind

n <- 10000
mix.sims.out <- rep(0,n)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind){
    #pick wind kernel:
    mix.sims.out[i] <- rpe(n=1, mu=mu.wind, 
    									 sigma=sigma.wind, kappa=kappa.wind)
  }else{
    #pick animal kernel: 
    mix.sims.out[i] <- rpe(n=1, mu=mu.animal, 
    									 sigma=sigma.animal, kappa=kappa.animal)
  }
}

hist(mix.sims.out)
# Simulating with different probabilities ####
p.wind <- c(0.50, 0.70, 0.90)
p.animal <- 1-p.wind

for (j in 1:length(p.wind)){
	mix.sims.out <- rep(0,n)
	p1 <- p.wind[j]
		for(i in 1:n){
  
			U <- runif(n=1)
  		if(U <= p1){
  		# pick wind kernel:
  	 	mix.sims.out[i] <- rpe(n=1, mu=mu.wind, 
    									 sigma=sigma.wind, kappa=kappa.wind)
  		}else{
  	 	# pick animal kernel: 
    	mix.sims.out[i] <- rpe(n=1, mu=mu.animal, 
    									 sigma=sigma.animal, kappa=kappa.animal)
  	}
	}
	hist(mix.sims.out)
}
