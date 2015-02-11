# Code for temporally fluctuating selection

num.sims <- 5 # number of independent simulations to run
num.years <- 50
freq.1.mat <- matrix(nrow = num.sims, ncol = num.years)

for (j in 1:num.sims) {
  
  J <- 4000
  init.1 <- 0.5*J
  COM <- vector(length = J)
  COM[1:init.1] <- 1; COM[(init.1+1):J] <- 2
  
  year <- 2
  
  # set fitness ratios in each year
  f20 <- c(1.1,1.1,1.1,1.1,1.1,1.1,1.1,1.1,1.1,1.1,1/1.1,1/1.1,1/1.1,1/1.1,1/1.1,1/1.1,1/1.1,1/1.1,1/1.1,1/1.1)
  fit.ratios <- f20
  for (x in 1:(num.years/20)) {
    fit.ratios <- c(fit.ratios,f20)
  }
  
  freq.dep <- 0
  
  freq.1.mat[j,1] <- sum(COM==1)/J
  
  for (i in 1:(J*(num.years-1))) {
    
    freq.1 <- sum(COM==1)/J; freq.2 <- 1 - freq.1
    fit.ratio <- fit.ratios[year]
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
