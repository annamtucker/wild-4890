
# Analyzing abundance of wildlife populations
# WILD 4890 Lab 7
# March 2018


#### Part 1: Unmarked individuals #####


### Canonical population estimator ####

N = 50
p = 0.6
n.counts = 10

C = rbinom(n.counts, N, p) # simulate counts
C   # look at counts

hist(C, breaks = 10, col = "gray", main = paste(n.counts, "counts, N =", N, ", p =", p),
     xlab = "Number of individuals counted")

Nhat = C/p



### Estimating trend from population indices ####

# manually enter counts of active wood duck nests
year = c(1:10)
index = c(10, 7, 8, 10, 10, 12, 14, 12, 13, 18)

# plot counts over time
plot(index ~ year, pch = 16, cex = 1.5, xlab = "Time", ylab = "Count")

# we can use a generalized linear model to estimate growth rate (r) on the log scale
# we'll fit the equation: log(N) = log(N0) + r*t  (takes the form of a straight line)
# the intercept will equal log(N0) and the slope ("year") equals the estimate of r
# use a Poisson error structure since we are modeling count data


# fit the generalized linear regression model
mod = glm(index ~ year, family = "poisson")
summary(mod)

# get predicted values and confidence limits
pred = predict.lm(mod, interval = "confidence", newdata = data.frame(year = year))

# plot population size over time on the log scale and add predicted line
# dashed lines are the 95% confidence intervals
par(mfrow = c(1,2))
plot(log(index) ~ year, pch = 16, cex = 1.5, xlab = "Time", ylab = "Log index", main = "Log scale")
lines(pred[,1], lwd = 2)  
lines(pred[,2], lwd = 2, lty = 2)
lines(pred[,3], lwd = 2, lty = 2)

# we can also back-transform the estimates to look at them on the natural scale
pred_nat = exp(pred)

plot(index ~ year, pch = 16, cex = 1.5, xlab = "Time", ylab = "Population index",
     main = "Natural scale")
lines(pred_nat[,1], lwd = 2)
lines(pred_nat[,2], lwd = 2, lty = 2)
lines(pred_nat[,3], lwd = 2, lty = 2)


# convert r to lambda
r = -0.01          # change value of r here
lambda = exp(r)
lambda




#### Part 2: Marked individuals ####

M = 41     # number captured and marked in first sampling period
C = 38     # total number captured in second sampling period
R = 14    # number of recaptures (previously-marked) in second samplng period

# 2-sample Lincoln-Petersen population size estimate ####
Nhat = (((M+1)*(C+1))/(R+1))-1           # Lincoln-Petersen estimator adjusted for small sample size

# estimate variance in estimate of population size and compute confidence intervals
var = ((M+1)*(C+1)*(M-R)*(C-R))/(((R+1)^2)*(R+2))
se = sqrt(var)
conf.low = Nhat - 1.96*se
conf.high = Nhat + 1.96*se

# coefficient of variation  ####
cv = (se/Nhat)*100

# print results
print(c("Nhat" = Nhat, "conf.low" = conf.low, "conf.high" = conf.high, "cv" = cv))

# detection probability ####
p1 = R/C
p2 = R/M

p1
p2


# expected precision ####
p = 0.4
N = 40

exp_cv = (1-p)/(p*sqrt(N))*100
exp_cv



