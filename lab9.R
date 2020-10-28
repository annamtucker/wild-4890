# stochastic projections
# feb 2018


#### parametric uncertainty ####
# birth rate and death rate are constant from year to year and the same for all individuals
# but we are uncertain about what the true birth and/or death rates are

# set the number of years to project and initial population size
nyrs = 20
N.init = 10

# set value for birth and death rate
b = 0.3
d = 0.2

# calculate r
r = b-d

# set up projection
N = numeric(nyrs)
N[1] = N.init

# project the population 
for(t in 2:nyrs){
  N[t] = N[t-1] * exp(r)
}

# plot the projection
plot(N ~ c(1:nyrs), pch = 16, cex = 2, xlab = "Year")
lines(N ~ c(1:nyrs), lwd = 2)

# look at projected values for population size
N


# project population under a range of possible values
min.b = 0.15
max.b = 0.35

birth.rate = seq(min.b, max.b, 0.01)
birth.rate

# set up projection 
N = matrix(nrow = nyrs, ncol = length(birth.rate))
N[1,] = N.init

for(i in 1:length(seq(bs))){
  
  r = birth.rate[i]-d
  
  for(t in 2:nyrs){
    N[t,i] = N[t-1,i] * exp(r)
  }
  
}

matplot(N, type ="l", lwd = 2, xlab = "Year", ylab = "N",
        "Parametric uncertainty only")



#### demographic stochasticity ####

# birth and death rate are constant from year to year and we can measure them perfectly,
# but there is individual variation so we treat them as probabilities, not rates

# set number of replications, number of years, and initial population size
reps = 50
nyrs = 50
N.init = 10

# set birth rate and death rate
b = 0.4
d = 0.3

# set up projection
N = B = D = matrix(nrow = nyrs, ncol = reps)
N[1,] = N.init

for(r in 1:reps){
  
  for(t in 2:nyrs){
    B[t-1,r] = rpois(1, N[t-1,r]*b)
    D[t-1,r] = rbinom(1, N[t-1,r], d)
    N[t,r] = N[t-1,r] + B[t-1,r] - D[t-1,r]
  }
}

matplot(N, type = "l", lwd = 2, xlab = "Year", ylab = "N",
        main = "Demographic stochasticity only")




#### environmental stochasticity ####

# birth and death rate are the same for all individuals and we can measure them perfectly,
# but they vary from year to year

# set number of replications, number of years, and initial population size
reps = 50
nyrs = 50
N.init = 10

# set mean birth rate and sd of birth rate
mean.b = 0.4
sd.b = 0.4

# plot possible values for b
hist(rnorm(10000, mean.b, sd.b), breaks = 100, col = "gray", main = "",
     xlab = "Possible value of b", ylab = "Probability", freq = F)

# set mean death rate and sd of death rate
mean.d = 0.3
sd.d = 0.3

# plot possible values for d
hist(rnorm(10000, mean.d, sd.d), breaks = 100, col = "gray", main = "",
     xlab = "Possible value of d", ylab = "Probability", freq = F)


# set up and run the projection
b = abs(rnorm(nyrs, mean.b, sd.b))
d = abs(rnorm(nyrs, mean.d, sd.b))
d = d/max(d)

N = B = D = matrix(nrow = nyrs, ncol = reps)
N[1,] = N.init

for(r in 1:reps){
  
  for(t in 2:nyrs){
    B[t-1,r] = rpois(1, N[t-1,r]*b[t])
    D[t-1,r] = rbinom(1, N[t-1,r], d[t])
    N[t,r] = N[t-1,r] + B[t-1,r] - D[t-1,r]
  }
}

matplot(N, type = "l", lwd = 2, xlab = "Year", ylab = "N",
        main = "Environmental stochasticity only")

#### parametric, demographic, and environmental stochasticity ####

# birth and death rates vary from year to year, we cannot measure them perfectly, 
# and we treat them as probabilities not rates to account for individual variation
# (for simplicity we only consider parametric uncertainty for birth rate)

# set number of replications, number of years, and initial population size
reps = 1000
nyrs = 20
N.init = 10

# set minimum and maximum average birth rates
min.b = 0.1
max.b = 0.5

# set average death rate
mean.d = 0.3

# set among-year standard deviation for birth and death rates
sd.b = 0.4
sd.d = 0.3


# set up and run the projection
d = abs(rnorm(nyrs, mean.d, sd.b))
d = d/max(d)

N = B = D = matrix(nrow = nyrs, ncol = reps)
N[1,] = N.init

for(r in 1:reps){
  
  # draw a value for the average birth rate
  mean.b = runif(1, min.b, max.b)
  b = abs(rnorm(nyrs, mean.b, sd.b))
  
  for(t in 2:nyrs){
    B[t-1,r] = rpois(1, N[t-1,r]*b[t])
    D[t-1,r] = rbinom(1, N[t-1,r], d[t])
    N[t,r] = N[t-1,r] + B[t-1,r] - D[t-1,r]
  }
}

matplot(N, type = "l", lwd = 2, xlab = "Year", ylab = "N",
        main = "Parametric, demographic, and environmental stochasticity")



#### from estimation to projection ####

# read in data
dat = read.csv("marked_dat.csv")
dat

# calculate Nhat and variance using the Lincoln-Petersen estimator
dat$Nhat = (((dat$M+1)*(dat$C+1))/(dat$R+1))-1
dat$var = ((dat$M+1)*(dat$C+1)*(dat$M-dat$R)*(dat$C-dat$R))/(((dat$R+1)^2)*(dat$R+2))
dat$conf.low = dat$Nhat - 1.96*sqrt(dat$var)
dat$conf.high = dat$Nhat + 1.96*sqrt(dat$var)

# plot estimates of N over time
par(mfrow = c(1,1))
plot(dat$Nhat ~ dat$Year, pch = 16, cex = 1.5, xlab = "Time", ylab = "Estimated population size", 
     ylim = c(min(dat$conf.low), max(dat$conf.high)))
lines(dat$Nhat ~ dat$Year, lwd = 1, lty = 2)
arrows(x0 = dat$Year, x1 = dat$Year, y0 = dat$conf.low, y1 = dat$conf.high, 
       angle = 90, code = 3, length = 0.04, lwd =2)


# calculate lambda for each time step
# (need to add the NA on the end so it is the correct length)
dat$lambda = c(dat$Nhat[2:nrow(dat)]/dat$Nhat[1:(nrow(dat)-1)], NA)
dat

# we can also calculate a variance on our estimate of lambda based on variance in estimates of N
# first calculate the coefficent of variation (cv) for Nhat estimates
cv0 = c(sqrt(dat$var[1:(nrow(dat)-1)])/dat$Nhat[1:(nrow(dat)-1)], 0)
cv1 = c(0, sqrt(dat$var[2:nrow(dat)])/dat$Nhat[2:nrow(dat)])

dat$lambda_var = dat$lambda^2 * (cv0 + cv1)

# plot estimates of lambda over time
plot(dat$lambda ~ dat$Year, pch = 16, cex = 1.5, xlab = "Time", ylab = "Estimated population growth rate", 
     ylim = c(0.5, 2.1))
lines(dat$lambda ~ dat$Year, lwd = 1, lty = 2)
arrows(x0 = dat$Year, x1 = dat$Year, y0 = dat$lambda - dat$lambda_var, y1 = dat$lambda + dat$lambda_var, 
       angle = 90, code = 3, length = 0.04, lwd =2)
abline(h = 1, lty = 3)


# find the average lambda over this time period - need to use geometric mean
# the arithmetic mean will produce an incorrect estimate of average lambda

# function to calculate geometric mean
geo_mean = function(x){
  prod(x, na.rm = T)^(1/(length(x)-1))
}

mean_lambda = geo_mean(dat$lambda)

# add line to plot at the mean value
abline(h = mean_lambda)

# calculate standard deviation of lambda
sd_lambda = sd(dat$lambda, na.rm = T)



# next we'll use estimates of mean lambda and variation in lambda 
# to project population growth into the future

# visualize distribution of possible growth rates
x = rnorm(100000, mean_lambda, sd_lambda)
x
hist(x, breaks = 50, col = "gray", main = "Distribution of possible annual growth rates", 
     xlab = "Possible values")


# start with estimate from final year as initial population size
# project 20 years into the future
nyrs = 20
N = numeric(nyrs)
N[1] = dat$Nhat[15]

# draw a different value for lambda each year
lambda = rnorm(t, mean_lambda, sd_lambda)
lambda

# projection population forward
for(i in 2:nyrs){
  N[i] = N[i-1]*lambda[i-1]
}

# plot outcome
plot(N ~ c(1:nyrs), pch = 16, cex = 1, xlab = "Years in the future", ylab = "Population size")
lines(N ~ c(1:nyrs), lwd = 2, lty = 2)


# repeat this projection 1000 times
reps = 1000
N = matrix(nrow = nyrs, ncol = reps)
N[1,] = dat$Nhat[15]

for(r in 1:reps){
  
  # draw a different value for lambda each year
  lambda = rnorm(nyrs, mean_lambda, sd_lambda)
  
  # projection population forward
  for(i in 2:nyrs){
    N[i,r] = N[i-1,r]*lambda[i-1]
  }
}

matplot(N, type = "l", xlab = "Years in the future", ylab = "Population size", lwd = 2,
        main = "All replicates")


# calculate the 95% quantiles and median outcome and plot projection
med = round(apply(N, 1, median))
lci = round(apply(N, 1, quantile, probs = 0.025))
uci = round(apply(N, 1, quantile, probs = 0.975))

Nhat = c(dat$Nhat, rep(NA, nyrs))
pred.N = c(dat$Nhat, med)
conf.low = c(dat$conf.low, rep(NA, nyrs))
conf.high = c(dat$conf.high, rep(NA, nyrs))
pred.lci = c(rep(NA, 15), lci)
pred.uci = c(rep(NA, 15), uci)
year = c(1:(15+nyrs))

# plot estimates of N over time
plot(Nhat ~ year, pch = 16, cex = 1.5, xlab = "Time", ylab = "Estimated population size", 
     ylim = c(min(lci, na.rm = T), max(uci, na.rm = T)))
lines(pred.N ~ year, lwd = 2)
arrows(x0 = year, x1 = year, y0 = conf.low, y1 = conf.high, 
       angle = 90, code = 3, length = 0.04, lwd =2)
lines(pred.lci ~ year, lwd = 2, lty = 2)
lines(pred.uci ~ year, lwd = 2, lty = 2)


# probabiilty of extinction (use threshold = 1 for extinction to account for non-rounding)
threshold = 10

mean(N[nyrs,] < threshold)

# add line to plot for threshold
abline(h = threshold, col = "red", lwd =2)








######## simulate aruba rattlesnake data #####

lambda = 0.99
sd.lam = 0.01

hist(rnorm(100000, lambda, sd.lam))

t = 15
N = numeric(t)
N[1] = 250

M = C = R = numeric(t)
M[1] = NA
C[1] = NA
R[1] = NA
p = 0.4

lam = rnorm(t, lambda, sd.lam)

for(i in 2:t){
  N[i] = round(N[i-1]*lam[i])
  
  M[i] = rbinom(1, N[i], p)
  C[i] = rbinom(1, N[i], p)
  R[i] = rbinom(1, N[i], p^2)
}

M[1] = rbinom(1, N[1], p)
C[1] = rbinom(1, N[1], p)
R[1] = rbinom(1, N[1], p^2)

snakes = data.frame(Year = c(1:t)+2001,
                    Marked = M, 
                    Captured = C,
                    Recaptured = R)


plot(N ~ c(1:t))

snakes$Nhat = (((snakes$Marked+1)*(snakes$Captured+1))/(snakes$R+1))-1  
snakes$lambda = c(snakes$Nhat[2:t]/snakes$Nhat[1:(t-1)], NA)


plot(snakes$Nhat ~ c(1:t))
lines(N ~ c(1:t))

write.csv(snakes, "snakes.csv", row.names = F)


# function to calculate geometric mean
geo_mean = function(x){
  prod(x, na.rm = T)^(1/(length(x)-1))
}

mean_lambda = geo_mean(snakes$lambda)

# calculate standard deviation of lambda
sd_lambda = sd(snakes$lambda, na.rm = T)

mean_lambda 
sd_lambda

# next we'll use estimates of mean lambda and variation in lambda 
# to project population growth into the future

# visualize distribution of possible growth rates
x = rnorm(100000, mean_lambda, sd_lambda)
x
hist(x, breaks = 50, col = "gray", main = "Distribution of possible annual growth rates", 
     xlab = "Possible values")

