# Neutral model in 1 large community with speciation
# Produces one scenario in Fig 4.11; need to alter nu to generate others
# This code takes a while to run (~20 minutes or more)
# More than half the code is post-simulation calculations in order to generate graphics

J <- 10000 
nu <- 5e-04 
sp <- 1 
COM <- vector(length = J); COM[] <- sp 
num.years <- 10000 
year <- 2 

COM.out <- matrix(nrow = (num.years/100+1), ncol = J); COM.out[1,] <- COM 

for (i in 1:(J*(num.years))) {
	dead.indiv <- ceiling(J*runif(1))

if (runif(1) < nu) {
	COM[dead.indiv] <- sp+1
	sp <- sp + 1
} else {
	COM[dead.indiv] <- COM[ceiling(J*runif(1))]
}

if (i %% (J*100) == 0) {
	COM.out[year,] <- COM
	year <- year+ 1 	
} } 

################################
# a bunch of calculations to allow plotting relative abundance distributions
# we first need the number of species (SR) at each time, to avoid outputting zero abundances

SR.out <- vector(length = dim(COM.out)[1])

for (t in 1:dim(COM.out)[1]) {
	COM.hist <- hist(COM.out[t,],c(1:max(COM.out[t,])),plot=FALSE)
	abund.vec <- COM.hist$counts[COM.hist$counts>0]
	SR.out[t] <- sum(abund.vec>0)
}

abund.out <- matrix(nrow = (num.years/100+1), ncol = max(SR.out))
abund.out[] <- 0

for (t in 1:dim(COM.out)[1]) {
	COM.hist <- hist(COM.out[t,],c(1:max(COM.out[t,])),plot=FALSE)
	abund.vec <- COM.hist$counts[COM.hist$counts>0]
	abund.out[t,1:SR.out[t]] <- abund.vec
}

abund.out <- abund.out/J

final.rad <- sort(abund.out[(num.years/100+1),1:SR.out[(num.years/100+1)]],decreasing=TRUE)

# write the final relative abundance distribution to a file for use in island biography model
write.csv(final.rad, file = "c:/temp/RAD1.csv")

plot(log(final.rad),type="l",
xlab="Species rank order", ylab="log(Relative abundance)")

# some code to overlay results from previous time steps (should look similar if simulation has run long enough)
lines(log(sort(abund.out[(num.years/100+1)-1,1:SR.out[(num.years/100+1)-1]],decreasing=TRUE)),type="l",col="red")
lines(log(sort(abund.out[(num.years/100+1)-2,1:SR.out[(num.years/100+1)-2]],decreasing=TRUE)),type="l",col="blue")
lines(log(sort(abund.out[(num.years/100+1)-3,1:SR.out[(num.years/100+1)-3]],decreasing=TRUE)),type="l",col="green")
lines(log(sort(abund.out[(num.years/100+1)-4,1:SR.out[(num.years/100+1)-4]],decreasing=TRUE)),type="l",col="pink")
lines(log(sort(abund.out[(num.years/100+1)-5,1:SR.out[(num.years/100+1)-5]],decreasing=TRUE)),type="l",col="grey")
