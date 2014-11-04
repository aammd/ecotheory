# Code for frequency-dependent selection in multiple independent communities
# Some parameters come outside first simulation loop; others are inside

num.sims <- 20 # number of independent simulations to run
num.years <- 50
freq.1.mat <- matrix(nrow = num.sims, ncol = num.years)

for (j in 1:num.sims) {

J <- 100
init.1 <- 0.8*J
COM <- vector(length = J)
COM[1:init.1] <- 1; COM[(init.1+1):J] <- 2

year <- 2

fit.ratio.avg <- 1
freq.dep <- 0.4

freq.1.mat[j,1] <- sum(COM==1)/J

for (i in 1:(J*(num.years-1))) {

freq.1 <- sum(COM==1)/J; freq.2 <- 1 - freq.1
fit.ratio <- exp(freq.dep*(freq.1-0.5) + log(fit.ratio.avg))
Pr.1 <- fit.ratio*freq.1/(fit.ratio*freq.1 + freq.2)
COM[ceiling(J*runif(1))] <- sample(c(1,2), 1, prob=c(Pr.1,1-Pr.1))
      
if (i %% J == 0) {
	freq.1.mat[j,year] <- sum(COM==1)/J
	year <- year + 1
}
}
}

plot(1:num.years, freq.1.mat[1,], type="l", xlab="Time", 
ylab="Frequency of species 1", ylim=c(0,1))
for (i in 2:(num.sims-1)) {
lines(1:num.years,freq.1.mat[i,], type="l", ylim=c(0,1))
}
