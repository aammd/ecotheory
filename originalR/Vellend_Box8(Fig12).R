# Online Box 8
# R code to produce Fig. 4.12
# The Theory of Island Biogeography
# Code produces one curve in Fig. 4.12
# Need to alter the input csv file and m to produce the others

## Import data on metacommunity relative abundance distribution
## produced by Online Box 7
metacom.data <- read.csv("c:/temp/RAD1.csv", header = TRUE)

## specify parameters, initial conditions, and output matrix
num.years <- 1000
J.min <- 2 # J on the smallest 'island'
num.J <- 12 # the number of islands (J values)
SR.meta = dim(metacom.data)[1] # metacommunity species richness

m <- 0.01

SR.out <- matrix(nrow = (num.years+1), ncol = num.J); SR.out[1,] <- 1

## run simulations (one for each island)
for (j in 1:num.J) {
	J <- J.min**j
	COM <- vector(length = J); COM[] <- 1
	time <- 2

## run individual simulation
for (i in 1:(J*num.years)) {
	dead.indiv <- ceiling(J*runif(1)) 

      ## immigration
	if (runif(1) < m) {
      	COM[dead.indiv] <- sample(metacom.data[,1], size = 1, prob = metacom.data[,2])
	} else {

      ## local reproduction
	      COM[dead.indiv] <- COM[ceiling(J*runif(1))]
	}

## record data
if (i %% J == 0) {
	COM.hist <- hist(COM, c(1:SR.meta), plot=FALSE)
	abund.vec <- COM.hist$counts[COM.hist$counts>0]
	SR.out[time,j] <- sum(abund.vec>0)
	time <- time + 1 
	} 
} }

## graph the results (the average of the final 600 years of 'data')
plot(log(J.min**c(1:num.J)), log(colMeans(SR.out[400:1001,])), type="l", 
xlab="log(Local community size, J)", ylab="log(Number of species)", ylim=c(0,5))
