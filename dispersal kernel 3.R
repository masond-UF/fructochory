# Dispersal kernel for wind dispersed herb ####
library(LaplacesDemon)
library(tidyverse)
options(scipen = 999)

# Basic kernels ####
# Conyza
set.seed(14356)

wind <- as.data.frame(rcauchy(n=10000,
															location=0.00000249,scale=2.7658))
colnames(wind) <- c("Distance")

wind <- wind %>% filter(Distance>=0)
hist(wind$Distance, breaks = 100)

wind$Kernel <- "Wind only"
filter(wind, Distance>=100)

# Trillium 	
animal <- as.data.frame(rpe(n=10000,sigma=188.36,kappa=0.7341))
colnames(animal) <- c("Distance")

animal <- animal %>% filter(Distance>0)
hist(animal$Distance, breaks = 50)

# Mixture test ####
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

# 1% probability of animal ####
p.wind <- c(0.99, 0.95, 0.90, 0.80, 0.70)
p.animal <- 1-p.wind

n <- 10000
mix.sims.out <- rep(0,n)
set.seed(4)

for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[1]){
    #pick wind kernel:
    mix.sims.out[i] <- rcauchy(n=1,
															location=0.00000249,scale=2.7658)
  }else{
    #pick animal kernel: 
    mix.sims.out[i] <- rpe(n=1,sigma=188.36,kappa=0.7341)
  }
}

mix.sims.out.1 <- as.data.frame(mix.sims.out)
colnames(mix.sims.out.1) <- c("Distance")
mix.sims.out.1 <- mix.sims.out.1 %>% filter(Distance>=0)
hist(mix.sims.out.1$Distance, breaks = 50)

median(mix.sims.out.1$Distance)-median(wind$Distance) # 0.05361729
quantile(mix.sims.out.1$Distance, probs=c(.95))-quantile(wind$Distance, probs=c(.95)) # 12.13406 
 
filter(mix.sims.out.1, Distance>=100)

# 5% probability of animal ####
mix.sims.out <- rep(0,n)
set.seed(302)

for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[2]){
    #pick wind kernel:
    mix.sims.out[i] <- rcauchy(n=1,
															location=0.00000249,scale=2.7658)
  }else{
    #pick animal kernel: 
    mix.sims.out[i] <- rpe(n=1,sigma=188.36,kappa=0.7341)
  }
}

mix.sims.out.5 <- as.data.frame(mix.sims.out)
colnames(mix.sims.out.5) <- c("Distance")
mix.sims.out.5 <- mix.sims.out.5 %>% filter(Distance>=0)
mix.sims.out.5$Kernel <- "5% Exocarpochory"
hist(mix.sims.out.5$Distance, breaks = 50)

median(mix.sims.out.5$Distance)-median(wind$Distance) # 0.287181
quantile(mix.sims.out.5$Distance, probs=c(.95))-quantile(wind$Distance, probs=c(.95)) # 52.16673  
 
filter(mix.sims.out.5, Distance>=100)

# 10% probability of animal ####
mix.sims.out <- rep(0,n)
set.seed(49)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[3]){
    #pick wind kernel:
    mix.sims.out[i] <- rcauchy(n=1,
															location=0.00000249,scale=2.7658)
  }else{
    #pick animal kernel: 
    mix.sims.out[i] <- rpe(n=1,sigma=188.36,kappa=0.7341)
  }
}

mix.sims.out.10 <- as.data.frame(mix.sims.out)
colnames(mix.sims.out.10) <- c("Distance")
mix.sims.out.10 <- mix.sims.out.10 %>% filter(Distance>=0)
hist(mix.sims.out.10$Distance, breaks = 50)

mix.sims.out.10$Kernel <- "10% Exocarpochory"
median(mix.sims.out.10$Distance)-median(wind$Distance) #  0.5404275
quantile(mix.sims.out.10$Distance, probs=c(.95))-quantile(wind$Distance, probs=c(0.95)) # 150.6252   
filter(mix.sims.out.10, Distance>=100)

# 20% probability of animal ####
mix.sims.out <- rep(0,n)
set.seed(501)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[4]){
    #pick wind kernel:
    mix.sims.out[i] <- rcauchy(n=1,
															location=0.00000249,scale=2.7658)
  }else{
    #pick animal kernel: 
    mix.sims.out[i] <- rpe(n=1,sigma=188.36,kappa=0.7341)
  }
}

mix.sims.out.20 <- as.data.frame(mix.sims.out)
colnames(mix.sims.out.20) <- c("Distance")
mix.sims.out.20 <- mix.sims.out.20 %>% filter(Distance>=0)
hist(mix.sims.out.20$Distance, breaks = 50)

median(mix.sims.out.20$Distance)-median(wind$Distance) # 1.337396
quantile(mix.sims.out.20$Distance, probs=c(.95))-quantile(wind$Distance, probs=c(.95)) # 272.4912 
filter(mix.sims.out.20, Distance>=100)

# 30% probability of animal ####
mix.sims.out <- rep(0,n)
set.seed(000109)
for(i in 1:n){
  
  U <- runif(n=1)
  if(U <= p.wind[5]){
    #pick wind kernel:
    mix.sims.out[i] <- rcauchy(n=1,
															location=0.00000249,scale=2.7658)
  }else{
    #pick animal kernel: 
    mix.sims.out[i] <- rpe(n=1,sigma=188.36,kappa=0.7341)
  }
}

mix.sims.out.30 <- as.data.frame(mix.sims.out)
colnames(mix.sims.out.30) <- c("Distance")
mix.sims.out.30 <- mix.sims.out.30 %>% filter(Distance>=0)
mix.sims.out.30$Kernel <- "30% Exocarpochory"
hist(mix.sims.out.30$Distance, breaks = 50)

median(mix.sims.out.30$Distance)-median(wind$Distance) # 2.5146
quantile(mix.sims.out.30$Distance, probs=c(.95))-quantile(wind$Distance, probs=c(.95)) # 405.3121 
filter(mix.sims.out.30, Distance>=100)

# Change in percentiles ####
df <- data.frame(Probability = c(1,5,10,20,30),
								 Delta_median = c(0.05361729,0.287181,0.5404275, 1.337396,2.5146),
								 Delta_95 = c(12.13406,52.16673,150.6252, 272.4912,405.3121))

df$Probability <- as.factor(df$Probability)

df_long <- pivot_longer(df, cols = c("Delta_median",
																		 "Delta_95"), names_to = "Parameter")

library(plyr)
df_long$Parameter <- plyr::revalue(df_long$Parameter, c("Delta_median"="50th percentile", 
												 "Delta_95"="95th percentile"))

limits <- data.frame(Parameter = c("50th percentile", "95th percentile"),
																	 y_min = c(0,0), y_max = c(3,450))

fifty <- filter(df_long, Parameter=='50th percentile')
fifty$y_min <- 0
fifty$y_max <- 3
ninety <- filter(df_long, Parameter=='95th percentile')
ninety$y_min <- 0
ninety$y_max <- 450

df_long <- rbind(fifty,ninety)


ggplot(df_long, aes(x = Probability, y = value))+
			 		geom_point(size=10)+
					ylab("Increase in dispersal distance (m)")+
					theme_bw()+
					xlab("Probability of Exocarpochory (%)")+
					theme(text = element_text(size = 22))+
					theme(axis.title.y = element_text(vjust = 2))+
					scale_x_discrete(breaks=c("1","5","10", "20", "30"))+
					facet_wrap(~Parameter, scales = "free_y", nrow = 2)+
					geom_blank(aes(y = y_min))+
				  geom_blank(aes(y = y_max))
# Histogram with different probabilitiess ####

comb.kernel <- rbind(wind,mix.sims.out.30)

ggplot(comb.kernel, aes(x=log(Distance+1), 
												color=Kernel, fill=Kernel)) +
  geom_histogram(alpha=0.1, position="identity",
  							 bins = 30)+
	scale_color_manual(values = c("#E69F00", "#56B4E9"))+
	ylim(0,800)+
	xlim(0,8)+
	xlab("Distance (m)")+
	ylab("Frequency")+
	theme_classic()+
	theme(legend.position="top")+
	scale_x_continuous(breaks = c(0,2.397895,
																4.615121,6.621406,
																8.517393),
										 labels = c("0","10","100",
										 					 "750","5000"))


log(11)
log(20)
log(751)


				 