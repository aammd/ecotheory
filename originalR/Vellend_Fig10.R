# Fig 4.10 - Colonization-competition tradeoff model in 2 patches
# parameters set for the 2nd panel from the left in Fig 4.10
# init.1 & m can be set to reproduce each panel

J <- 1000 
init.1 <- 0.1*J 
num.patch <- 2 
m <- 0.1
num.years <- 500 
year <- 2 

COM <- matrix(nrow=J, ncol=num.patch)
COM[1:init.1,] <- 1; COM[(init.1+1):J,] <- 2 

fit.ratio.avg <- vector(length=num.patch)
fit.ratio.avg[] <- c(1.2,1.2)
fit.ratio.m <- 1/5
freq.dep <- vector(length=num.patch)
freq.dep[] <- 0

freq.1.mat <- matrix(nrow = num.years, ncol = num.patch)
freq.1.mat[1,] <- init.1/J 

for (i in 1:(J*num.patch*(num.years-1))) {

patch <- sample(1:num.patch,1)

if (runif(1) < m) {
	freq.1.meta <- sum(COM==1)/(J*num.patch)
	Pr.1 <- fit.ratio.m*freq.1.meta/(fit.ratio.m*freq.1.meta + (1-freq.1.meta))
} else { 
	freq.1 <- sum(COM[,patch]==1)/J; freq.2 <- 1 - freq.1
	fit.ratio <- exp(freq.dep[patch]*(freq.1-0.5) + log(fit.ratio.avg[patch]))
	Pr.1 <-  fit.ratio*freq.1/(fit.ratio*freq.1 + freq.2)
}
	
COM[ceiling(J*runif(1)),patch] <- sample(c(1,2), 1, prob=c(Pr.1,1-Pr.1)) 

if (i %% (J*num.patch) == 0) {
freq.1.mat[year,] <- colSums(COM==1)/J
year <- year + 1 
}
} 

plot(1:num.years, rowMeans(freq.1.mat), type="l", xlab="Time", 
ylab="Frequency of species 1", ylim=c(0,1))