# Online Box 7
# R code to produce Fig. 4.11
# Neutral model in 1 large community with speciation
# Code produces one scenario in Fig 4.11; need to alter nu to generate others
# Note 1: this code takes a while to run (~20 minutes or more)
# Note 2: More than half the code is post-simulation calculations in order to generate graphics

## specify parameters, initial conditions, and output vector
num.years <- 10000 
J <- 10000 
sp <- 1 # a counter to keep track of added species
COM <- vector(length = J); COM[] <- sp 

nu <- 1e-04 # speciation rate

year <- 2 

COM.out <- matrix(nrow = (num.years/100+1), ncol = J); COM.out[1,] <- COM 

## run simulation
for (i in 1:(J*(num.years))) {
  dead.indiv <- ceiling(J*runif(1))
  
  ## speciation (replace dead individual with new species)
  if (runif(1) < nu) {
    COM[dead.indiv] <- sp+1
    sp <- sp + 1
  } else {

  ## not speciation (replace dead individual from community)
    COM[dead.indiv] <- COM[ceiling(J*runif(1))]
  }
  
  ## record data
  if (i %% (J*100) == 0) {
    COM.out[year,] <- COM
    year <- year+ 1 	
  } } 

################################
# Calculations to allow plotting relative abundance distributions

## First calculate the number of species (SR) at each time
## we need this to avoid outputting zero abundances

SR.out <- vector(length = dim(COM.out)[1])

for (t in 1:dim(COM.out)[1]) {
  COM.hist <- hist(COM.out[t,],c(1:max(COM.out[t,])),plot=FALSE)
  abund.vec <- COM.hist$counts[COM.hist$counts>0]
  SR.out[t] <- sum(abund.vec>0)
}

## Calculate species abundances
abund.out <- matrix(nrow = (num.years/100+1), ncol = max(SR.out))
abund.out[] <- 0

for (t in 1:dim(COM.out)[1]) {
  COM.hist <- hist(COM.out[t,],c(1:max(COM.out[t,])),plot=FALSE)
  abund.vec <- COM.hist$counts[COM.hist$counts>0]
  abund.out[t,1:SR.out[t]] <- abund.vec
}

abund.out <- abund.out/J

## The final rank-order of relative abundances
final.rad <- sort(abund.out[(num.years/100+1),1:SR.out[(num.years/100+1)]],decreasing=TRUE)

## Write the final relative abundance distribution to a file for use in Online Box 8 (island biogeography model)
## you will need to create/set the directory on your own
## data for different speciation rates need to be saved separately and combined later to produce Fig. 4.11
write.csv(final.rad, file = "c:/temp/RAD1.csv")

## graph the results
plot(log(final.rad),type="l",
     xlab="Species rank order", ylab="log(Relative abundance)")
