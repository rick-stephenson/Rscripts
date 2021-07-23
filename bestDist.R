## function to take a vector of data and
## run a bunch of fits and then print the
## gof results in a table
## intput is a vector of values

## temporary input for testing:
##in.vec <- PulData$length

bestDist <- function(in.vec) {

suppressWarnings(fn <- fitdist(in.vec, distr = "norm"))
suppressWarnings(fl <- fitdist(in.vec, distr = "lnorm"))
suppressWarnings(fw <- fitdist(in.vec, distr = "weibull"))

bfit <- gofstat(list(fn, fl, fw), fitnames = c("Normal", "lognormal", "weibull"))

print(bfit)
print("ad test")
print(bfit$adtest)
print("cvm test")
print(bfit$cvmtest)
print("kstest")
print(bfit$kstest)

}

