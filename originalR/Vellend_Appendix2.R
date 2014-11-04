# The Theory of Island Biogeography
# The parameter values here produce the curve in Fig 4.12 panel (a), m = 0.01
# Alter the input csv file and m to produce the others

metacom.data <- read.csv("c:/temp/RAD1.csv", header = TRUE)

J.min <- 2
num.J <- 12
SR.meta = dim(metacom.data)[1]
m <- 0.01
num.years <- 1000

SR.out <- matrix(nrow = (num.years+1), ncol = num.J); SR.out[1,] <- 1

for (j in 1:num.J) {
	J <- J.min**j
	COM <- vector(length = J); COM[] <- 1
	time <- 2

for (i in 1:(J*num.years)) {
	dead.indiv <- ceiling(J*runif(1)) 

	if (runif(1) < m) {
      	COM[dead.indiv] <- sample(metacom.data[,1], size = 1, prob = metacom.data[,2])
	} else {
	      COM[dead.indiv] <- COM[ceiling(J*runif(1))]
	}

if (i %% J == 0) {
	COM.hist <- hist(COM, c(1:SR.meta), plot=FALSE)
	abund.vec <- COM.hist$counts[COM.hist$counts>0]
	SR.out[time,j] <- sum(abund.vec>0)
	time <- time + 1 
	} 
} }

# plot the average of the final 600 years of 'data'
plot(log(J.min**c(1:num.J)), log(colMeans(SR.out[400:1001,])), type="l", 
xlab="log(Local community size, J)", ylab="log(Number of species)", ylim=c(0,5))
