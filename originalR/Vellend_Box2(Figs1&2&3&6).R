# R code to produce Figures 4.1, 4.2, 4.3 and 4.6
# Dynamics of 2 species under constant and/or frequency-dependent selection
# Note that some parameters are outside the simulation loop; others are inside (defined anew for each simulation)

# For Fig. 4.1 (neutral model): num.sims = 20, num.years = 50, fit.ratio.avg = 1, freq.dep = 0, init.1 = 0.5*J, J varies among panels
# For Fig. 4.2 (constant selection): as for Fig. 4.1 except fit.ratio.avg = 1.1
# For Fig. 4.3 (negative frequency-dependent selection): as for Fig. 4.1 except fit.ratio.avg, freq.dep and J vary among panels
# For Fig. 4.6 (positive frequency-dependent selection): as for Fig. 4.1 except freq.dep = 0.4, J = 100 and init.1 varies among panels

## specify the number of simulations, the number of years, and a matrix for output
num.sims <- 20 
num.years <- 50
freq.1.mat <- matrix(nrow = num.sims, ncol = num.years)

## start a loop for each of j independent simulations
for (j in 1:num.sims) {
  
  ## specify parameters and initial conditions
  J <- 100
  init.1 <- 0.5*J  
  
  COM <- vector(length = J)
  COM[1:init.1] <- 1; COM[(init.1+1):J] <- 2
  year <- 2
  
  fit.ratio.avg <- 1
  freq.dep <- 0
  
  ## record data (frequency of species 1) for year 1
  freq.1.mat[j,1] <- sum(COM==1)/J
  
  ## run simulation
  for (i in 1:(J*(num.years-1))) {
    
    freq.1 <- sum(COM==1)/J; freq.2 <- 1 - freq.1
    fit.ratio <- exp(freq.dep*(freq.1-0.5) + log(fit.ratio.avg))
    Pr.1 <- fit.ratio*freq.1/(fit.ratio*freq.1 + freq.2)
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
