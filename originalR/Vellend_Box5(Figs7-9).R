# Box 2 - N-patch model with drift and selection
# For Fig 4.7 set num.patch = 10, fit.ratio.avg[] = 1, freq.dep[] = 0, m depends on panel
# For Fig 4.8 set num.patch = 2, J = 1000, fit.ratio.avg[] <- c(1.2,(1/1.2)), m depends on panel
# For Fig 4.9 set num.years = 100, num.patch = 2, J = 1000, fit.ratio.avg[] <- c(1.5,(1/1.1)), m depends on panel

J <- 100 
init.1 <- J/2 
num.patch <- 2 
m <- 0.1
num.years <- 50 
year <- 2 

COM <- matrix(nrow=J, ncol=num.patch)
COM[1:init.1,] <- 1; COM[(init.1+1):J,] <- 2 

fit.ratio.avg <- vector(length=num.patch)
fit.ratio.avg[] <- 1
freq.dep <- vector(length=num.patch)
freq.dep[] <- 0

freq.1.mat <- matrix(nrow = num.years, ncol = num.patch)
freq.1.mat[1,] <- init.1/J 

for (i in 1:(J*num.patch*(num.years-1))) {
  
  patch <- sample(1:num.patch,1)
  
  if (runif(1) < m) {
    Pr.1 <- sum(COM==1)/(J*num.patch)
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

plot(1:num.years, freq.1.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 1", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.1.mat[,i], type="l", lty=2, ylim=c(0,1))
}
