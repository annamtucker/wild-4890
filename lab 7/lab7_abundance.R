###############################################
# Analyzing abundance of wildlife populations
# WILD 4890 Lab 7
# March 2018
###############################################


#### Part 1: Two-sample mark-recapture studies (Lincoln-Petersen estimator) ####


M = 41     # number captured and marked in first sampling period
C = 38    # total number captured in second sampling period
R = 14      # number of recaptures (previously-marked) in second samplng period

# 2-sample Lincoln-Petersen population size estimate
Nhat = (((M+1)*(C+1))/(R+1))-1           # Lincoln-Petersen estimator adjusted for small sample size

# estimate variance in estimate of population size and compute confidence intervals
var = ((M+1)*(C+1)*(M-R)*(C-R))/(((R+1)^2)*(R+2))
se = sqrt(var)
conf.low = Nhat - 1.96*se
conf.high = Nhat + 1.96*se

# coefficient of variation 
cv = (se/Nhat)*100

# print results
print(c("Nhat" = Nhat, "conf.low" = conf.low, "conf.high" = conf.high, "cv" = cv))

# detection probability
p1 = R/C
p2 = R/M

p1
p2


# expected precision
exp_cv = function(N, p){
  (1-p)/(p*sqrt(N))*100
}

exp_cv(N = 400, p= 0.3)



#### Part 2: Population projections with environmental stochasticity ###


# estimate abundance and population growth rate over several years

# read in data
dat = read.csv("marked_dat.csv")

# calculate Nhat and variance using same equations as above
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

# look at estimates
dat

# calculate lambda for each time step (need to add the NA on the end so it is the correct length)
dat$lambda = c(dat$Nhat[2:nrow(dat)]/dat$Nhat[1:(nrow(dat)-1)], NA)

# we can also calculate a variance on our estimate of lambda based on variance in estimates of N
# first calculate the coefficent of variation (cv) for Nhat estimates
cv0 = c(sqrt(dat$var[ 1:(nrow(dat)-1)])/dat$Nhat[1:(nrow(dat)-1)], 0)
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

# add line to plot 
abline(h = mean_lambda)

# calculate standard deviation of lambda
sd_lambda = sd(dat$lambda, na.rm = T)

### lambda projection ###

# use estimates of mean lambda and variation in lambda to project population growth into the future

# visualize distribution of possible growth rates
x = rnorm(100000, mean_lambda, sd_lambda)
hist(x, breaks = 50, col = "gray", main = "Possible values of lambda", xlab = "lambda")

# start with estimate from final year as starting population size
# project 20 years into the future
t = 25
N = numeric(t)
N[1] = dat$Nhat[15]

# draw a different value for lambda each year
lambda = rnorm(t, mean_lambda, sd_lambda)

# projection population forward
for(i in 2:t){
  N[i] = N[i-1]*lambda[i-1]
}

# plot outcome
plot(N ~ c(1:t), pch = 16, cex = 1, xlab = "Time", ylab = "Population size")
lines(N ~ c(1:t), lwd = 2, lty = 2)



# repeat this projection 1000 times
reps = 1000
N = matrix(nrow = t, ncol = reps)
N[1,] = dat$Nhat[15]

for(r in 1:reps){
  
  # draw a different value for lambda each year
  lambda = rnorm(t, mean_lambda, sd_lambda)
  
  # projection population forward
  for(i in 2:t){
    N[i,r] = N[i-1,r]*lambda[i-1]
  }
}

matplot(N, type = "l", xlab = "Time", ylab = "Population size")

# calculate the 95% quantiles and median outcome and plot 
med = apply(N, 1, median)
lci = apply(N, 1, quantile, probs = 0.025)
uci = apply(N, 1, quantile, probs = 0.975)

to.plot = dat[,c(1,5,7,8)]
to.plot = rbind(to.plot, data.frame(Year = c(16:40),
                                   Nhat = rep(NA, t),
                                   conf.low = rep(NA, t),
                                   conf.high = rep(NA, t)))
to.plot$pred = c(rep(NA, 15), med)
to.plot$lci = c(rep(NA, 15), lci)
to.plot$uci = c(rep(NA, 15), uci)


# plot estimates of N over time
plot(to.plot$Nhat ~ to.plot$Year, pch = 16, cex = 1.5, xlab = "Time", ylab = "Estimated population size", 
     ylim = c(min(to.plot$lci, na.rm = T), max(to.plot$uci, na.rm = T)))
lines(to.plot$Nhat ~ to.plot$Year, lwd = 1, lty = 2)
arrows(x0 = to.plot$Year, x1 = to.plot$Year, y0 = to.plot$conf.low, y1 = to.plot$conf.high, 
       angle = 90, code = 3, length = 0.04, lwd =2)
lines(to.plot$pred ~ to.plot$Year, lwd = 2)
lines(to.plot$lci ~ to.plot$Year, lwd = 2, lty = 2)
lines(to.plot$uci ~ to.plot$Year, lwd = 2, lty = 2)
legend(2, 65, lty = c(1,2), lwd = c(2,2), c("Median", "95% quantiles"))





# we can use our replicates to determine the probabilities of the population falling above or below a threshold
# when you ask R a logical question, it returns a TRUE or FALSE (called boolean values)
# R treats these values like numbers, where TRUE = 1, and FALSE = 0
# we can use this to easily find the proportion of replications in which certain conditions are met

# in what proportion of replications is N = 10 at final year?

# all values of N at year 20 (see all numbers in the console)
N[t,]
hist(N[t,], breaks = 50, col = "gray", main = "", xlab = "Population size in final year")
abline(v = 10, col = "red", lwd = 2)

# for each replication, is N < 10 at year 20? (see all the TRUEs and FALSEs in the console)
N[t,] < 10

# what proportion of replications was N < 10 at year 20?
# this is equivalent to the probability that N will be less than 10 in 20 years
mean(N[t,] < 10)

# what is the probability of the population going extinct? (N = 0)
# it works better to ask if N < 1 since we have decimal places
mean(N[t,] < 1)




### Part 3: Estimating population trend from counts ###


# read in file of count data
dat = read.csv("count_dat.csv")

# plot counts over time
plot(dat$count ~ dat$time, pch = 16, cex = 1.5, xlab = "Time", ylab = "Count")

# as you can see there are several years with missing data (no counts)
# this makes it hard to estimate average annual lambda
# instead we can use regression to estimate growth rate (r) on the log scale
# we'll fit the equation: log(N) = log(N0) + r*t  (takes the form of a straight line)
# the intercept will equal log(N0) and the slope equals the estimate of r

# first need to log-transform the counts
dat$logN = log(dat$count)

summary(mod <- lm(logN ~ time, data = dat))

### Part 4: Population projections with parametric uncertainty ###

# get predicted values and confidence limits
pred = predict.lm(mod, interval = "confidence", newdata = data.frame(time = c(1:25)))

# plot population size over time on the log scale and add predicted line
par(mfrow = c(1,2))
plot(dat$logN ~ dat$time, pch = 16, cex = 1.5, xlab = "Time", ylab = "Log population size", 
     main = "Log scale")
lines(pred[,1], lwd = 2)  
lines(pred[,2], lwd = 2, lty = 2)
lines(pred[,3], lwd = 2, lty = 2)

# we can also back-transform the estimates to look at them on the log scale
predN = exp(pred)

plot(dat$count ~ dat$time, pch = 16, cex = 1.5, xlab = "Time", ylab = "Population size",
     main = "Natural scale")
lines(predN[,1], lwd = 2)
lines(predN[,2], lwd = 2, lty = 2)
lines(predN[,3], lwd = 2, lty = 2)

# we cannot transform r to get unbiased estimates of lambda, however we can
# use estimates of r to project the population into the future, assuming r stays the same
# this method assumes that r is constant across years
# it does not account for environmental stochasticity or year-to-year variation in r

# mean and precision of r estimate
mean_r = 0.085
se_r = 0.009
sd_r = se_r*sqrt(nrow(dat))
cv_r = (sd_r/mean_r) * 100

print(c("mean" = mean_r, "sd" = sd_r, "cv"= cv_r))

# visualize variation in r
x = rnorm(100000, mean_r, sd_r)
par(mfrow = c(1,1))
hist(x, breaks = 50, col= "gray", main = "Uncertainty in estimate of r", xlab = "r")


# using this model, we're assuming that r does not change between years
# we can project this model into the future using the estimated mean
pred.new = predict.lm(mod, interval = "confidence", newdata = data.frame(time = c(1:50)))

# plot population size over time on the log scale and add predicted line
plot(dat$logN ~ dat$time, pch = 16, cex = 1.5, xlab = "Time", ylab = "Log population size",
     xlim = c(1,50), ylim = c(min(pred.new[,2]), max(pred.new[,3])))
lines(pred.new[,1], lwd = 2)
lines(pred.new[,2], lwd = 2, lty = 2)
lines(pred.new[,3], lwd = 2, lty = 2)

# but this assumes that our estimate of mean r is correct
# we known the precision our our estimate, so it's better to incorporate that uncertainty
# in the prediction process -- this is called parametric uncertainty 

# projection is the same as above, but using continuous time model with r
# instead of discrete time model with lambda
# we'll also project on the log scale for consistency with how it was modeled

# repeat this projection 1000 times
reps = 1000
t = 25

N = matrix(nrow = t, ncol = reps)
N[1,] = dat$logN[25]

for(r in 1:reps){
  
  # draw a different value for lambda each year
  real_r = rnorm(1, mean_r, sd_r)
  
  # projection population forward
  for(i in 2:t){
    N[i,r] = N[i-1,r] + real_r
  }
}

matplot(N, type = "l", xlab = "Time", ylab = "Population size")

# summarize and plot with observed counts
med = apply(N, 1, median)
lci = apply(N, 1, quantile, probs = 0.025)
uci = apply(N, 1, quantile, probs = 0.975)


plot(c(dat$logN, rep(NA, 25)) ~ c(1:50), pch = 16, cex = 1.5, xlab = "Time", ylab = "Log population size",
     ylim = c(2, max(uci)), main = "Log scale")
lines(c(pred.new[,1], rep(NA, 25)), lwd = 2, col = "blue")
lines(pred.new[,2], lwd = 2, lty = 2, col = "blue")
lines(pred.new[,3], lwd = 2, lty = 2, col = "blue")
lines(c(rep(NA, 25), med), lwd = 2)
lines(c(rep(NA, 25), lci), lwd = 2, lty = 2)
lines(c(rep(NA, 25), uci), lwd = 2, lty = 2)
legend(1, 8.5,lty = c(1,1), lwd = c(2,2), col = c("blue", "black"), c("Deterministic", "Stochastic"))


plot(c(dat$count, rep(NA, 25)) ~ c(1:50), pch = 16, cex = 1.5, xlab = "Time", ylab = "Log population size",
     ylim = c(2, max(exp(uci))), main = "Natural scale")
lines(c(exp(pred.new[,1]), rep(NA, 25)), lwd = 2, col = "blue")
lines(exp(pred.new[,2]), lwd = 2, lty = 2, col = "blue")
lines(exp(pred.new[,3]), lwd = 2, lty = 2, col = "blue")
lines(c(rep(NA, 25), exp(med)), lwd = 2)
lines(c(rep(NA, 25), exp(lci)), lwd = 2, lty = 2)
lines(c(rep(NA, 25), exp(uci)), lwd = 2, lty = 2)
legend(1, 5250,lty = c(1,1), lwd = c(2,2), col = c("blue", "black"), c("Deterministic", "Stochastic"))


# look at population projections in the final year
par(mfrow = c(1,1))
hist(exp(N[25,]), breaks = 50, col = "gray", main = "Population size in final year", xlab = "N")


# what is the probability of extinction at year 50?
mean(exp(N[25,]) < 1)

# what is the probability of more than 1000 individuals at year 50?
mean(exp(N[25,]) > 1000)


