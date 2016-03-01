##requires JAGS library
##Using JAGS for creating Bayesian model to forecast Online volume

library(R.utils)
library(rstan)
library(R2jags)
library(manipulate)
library(MCMCpack)
library(R2WinBUGS)

##Read in historical data
dat<- read.csv("data.csv", header=T, sep=',')

## Write out a full probability model for JAGS
jags.lm <- function() {
for (i in 1:N) {
Online[i] ~ dnorm(mu[i], tau)
mu[i] <- b[1] + b[2]*Mobile[i] + b[3]*Calls[i] + b[4]*Text[i]
}
# priors for missing X values for each channel: historical mean, precision (1/variance)
Mobile[1] ~ dnorm(64942, 2.15)
Calls[1] ~ dnorm(1682831, 4.06)
Text[1] ~ dnorm(14228, 2.15)
# non-informative priors on b's
for (j in 1:4) {
b[j] ~ dnorm(0, 0.001)
}
# conditional variance of y given x
tau <- pow(sd, -2)
sd ~ dunif(0, 100)
}

write.model(jags.lm, "jagslm.txt")



## Fit the model
attachLocally(dat)
N <- nrow(dat)
inits <- function() { list(b=c(50, rnorm(3))) }
jagsfit <- jags.parallel(data=c("Online", "Mobile",
"Calls", "Text", "N"),
inits=inits,
parameters.to.save=c("b", "sd", "Online",
"Mobile", "Calls", "Text "),
model.file="jagslm.txt",
n.chains=3,
n.iter=10000)


## Look at the model output, check convergence.
plot(jagsfit)
traceplot(jagsfit, mfrow=c(1,1), "Online")


## Check model output - values of jags.mod for predictions
jags.mod <- jagsfit$BUGSoutput
jags.mod$mean$b                  # regression coefficients
jags.mod$mean$Online[1]  # forecast of Feb 2016 Online values


## Probability that Online volume will exceed this historical mean of 874479
## That's the proportion of simulated draws greater than 874479
dim(jags.mod$sims.list$Online)
hist(jags.mod$sims.list$Online[,1], breaks=50, col="steelblue",
xlab="Predicted value of Online Visits", main="Bayesian Model of Volume Prediction")

table(jags.mod$sims.list$Online[,1]>874479) / jags.mod$n.sims

## Answer is False 1  – Will not exceed historical average