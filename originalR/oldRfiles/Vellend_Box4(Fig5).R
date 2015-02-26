# Online Box 4
# R code to produce Fig. 4.5 (delayed negative frequency-dependent selection)
# "delayed" = fitness is fixed for an entire "year", so there are two time loops

num.sims <- 2 # code doesn't work with num.sims = 1, so I ran 2 to get 1
num.years <- 50
freq.1.mat <- matrix(nrow = num.sims, ncol = num.years)

for (j in 1:num.sims) {
  
  J <- 500
  init.1 <- 0.1*J
  COM <- vector(length = J)
  COM[1:init.1] <- 1; COM[(init.1+1):J] <- 2
  
  year <- 2
  
  fit.ratio.avg <- 1
  freq.dep <- -20
  
  freq.1.mat[j,1] <- sum(COM==1)/J
  
  for (i in 1:(num.years-1)) {
    
    freq.1 <- sum(COM==1)/J; freq.2 <- 1 - freq.1
    fit.ratio <- exp(freq.dep*(freq.1-0.5) + log(fit.ratio.avg))
    Pr.1 <- fit.ratio*freq.1/(fit.ratio*freq.1 + freq.2)
    
    for (k in 1:J) {
      COM[ceiling(J*runif(1))] <- sample(c(1,2), 1, prob=c(Pr.1,1-Pr.1))
    }
    
    freq.1.mat[j,year] <- sum(COM==1)/J
    year <- year + 1
  }
}

plot(1:num.years, freq.1.mat[1,], type="l", xlab="Time", 
     ylab="Frequency of species 1", ylim=c(0,1))

