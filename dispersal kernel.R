# 3 March 2020
# Simulating dispersal kernels

library(gnorm)
library(tidyverse)

x <- seq(from=0.1, to=3, by=0.01) # distances

plot(x, dgnorm(x, mean, alpha, beta), 
		 ylim=c(0,2), type="l", main="Dispersal Distance",
     ylab="Probability", xlab = "Distance (m)", col="red")

d <- read.csv("Plantdispersaldata.csv")
options(scipen = 999)

alpha <- 0.0030
beta <- 0.3454
mean <- a_sigma*(gamma((3/b_kappa))/gamma((2/b_kappa)))

sim.dat <- rgnorm(10000, mean, alpha, beta)
sim.dat[ sim.dat<0 ] <- 0


plot(sim.dat)
bins <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,
					1.2,1.3,1.4,1.5,1.6,1.7)
sim.bin <- vector()

for(i in 1:length(bins)){
	sim.bin[i] <- length(which(sim.dat >= bins[i] & sim.dat < bins[i]+0.1))
}

sim.bin.df <- as.data.frame(cbind(bins,sim.bin))

ggplot(d = sim.bin.df, aes(x = bins, y = log(sim.bin, base = 10)))+
	geom_point()
