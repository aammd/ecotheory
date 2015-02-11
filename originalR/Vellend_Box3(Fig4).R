# Online Box 3
# R code to produce Fig. 4.4 (temporally fluctuating selection)

## specify the number of simulations, the number of years, and a matrix for output
num.sims <- 5 # number of independent simulations to run
num.years <- 50
freq.1.mat <- matrix(nrow = num.sims, ncol = num.years)

## start a loop for each of num.sims independent simulations
for (j in 1:num.sims) {

  ## specify parameters and initial conditions
  J <- 4000
  init.1 <- 0.5*J

  COM <- vector(length = J)
  COM[1:init.1] <- 1; COM[(init.1+1):J] <- 2 
  year <- 2

  freq.dep <- 0

  freq.1.mat[j,1] <- sum(COM==1)/J 

  # set fitness ratios in each sequence of 20 years
  f20 <- vector(length = 20); f20[1:10] <- 1.1; f20[11:20] <- 1/1.1
  fit.ratios <- f20
  for (x in 1:(num.years/20)) {
    fit.ratios <- c(fit.ratios,f20)
  }
 
  ## run simulation
  for (i in 1:(J*(num.years-1))) {
    
    freq.1 <- sum(COM==1)/J; freq.2 <- 1 - freq.1
    fit.ratio <- fit.ratios[year]
    Pr.1 <- fit.ratio*freq.1/(fit.ratio*freq.1 + freq.2) # note absence of freq.dep parameter
    COM[ceiling(J*runif(1))] <- sample(c(1,2), 1, prob=c(Pr.1,1-Pr.1))

    ## record data     
    if (i %% J == 0) {
      freq.1.mat[j,year] <- sum(COM==1)/J
      year <- year + 1
    }
  }
}

## graph the results
plot(1:num.years, freq.1.mat[1,], type="l", xlab="Time", 
     ylab="Frequency of species 1", ylim=c(0,1))
for (i in 2:(num.sims-1)) {
  lines(1:num.years,freq.1.mat[i,], type="l", ylim=c(0,1))
}
