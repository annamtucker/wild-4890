# survival estimation
# mar 2018


### known fate (Kaplan-Meier)

# simulating known-fate data

N = 25    # total number of individuals tracked
s = 0.9   # daily survival probability
c = 0.1   # daily censorship (emigration) probability
days = 10 # number of days

# create a matrix to track the fate of each individual (rows) over time (columns)
# 1 = alive, 0 = dead, NA = censored

pop = matrix(0, nrow = N, ncol = days)
pop[,1] = ifelse(rbinom(1, 1, s) == 1, ifelse(rbinom(1, 1, c) == 1, NA, 1))
pop

# loop across each individual
for(i in 1:N){
  
  #loop through time
  for(t in 2:days){
    
    # the rbinom() part is the coin flip to see if the individual survives
    # we multiply by the previous time step because an individual can only survive if it was alive yesterday
    
    alive.yesterday = pop[i,t-1] == 1
    dead.yesterday = pop[i,t-1] == "D"
    censored.yesterday = pop[i,t-1] == "C"
    
    survived.today = rbinom(1, 1, s) == 1
    censored.today = rbinom(1, 1, c) == 1
    
    # "|" means "or"
    pop[i,t] = ifelse(censored.yesterday | dead.yesterday, NA, 
                      ifelse(survived.today, ifelse(censored.today, "C", 1), "D"))
  }
}

# look at the population
pop

# next we calculate the conditional survival probability for each day
# create a data frame to keep all information together

daily = data.frame(day = c(1:days))

# number at risk on each day - aka the number alive (count up all the 1s)
count.at.risk = function(x) length(which(x == "1"))
n.risk = apply(pop, 2, count.at.risk)
n.risk

daily$n.risk = n.risk
daily

# number of deaths on each day 
count.dead = function(x) length(which(x == "D"))
n.deaths = apply(pop, 2, count.dead)
n.deaths

daily$n.deaths = n.deaths
daily

# number censored on each day
count.censored = function(x) length(which(x == "C"))
n.censored = apply(pop, 2, count.censored)

daily$n.censored = n.censored
daily

# conditional probability - see equation in Donovan Welden pg 317
daily$cond.surv = 1 - (daily$n.deaths/daily$n.risk)
daily


# unconditional probability - use the cumprod() function to find the cumulative product
daily$uncond.surv = cumprod(daily$cond.surv)
daily


# expected daily survival - based on the value we used to simulate the data
daily$expected = s^daily$day
daily


# observed daily survival - based on analysis of simulated data
daily$observed = daily$uncond.surv^(1/daily$day)
daily

# plot the Kaplan-Meier survival curve
plot(data = daily, expected~day, pch = 16, cex = 1.5, ylim = c(0,1), col = "gray60", xlab = "Day", 
     ylab = "Survival probability")
lines(data = daily, expected~day, lwd = 2, col = "gray60")
points(data = daily, uncond.surv~day, pch = 18, cex = 2, col = "dodgerblue4")
lines(data = daily, uncond.surv~day, col = "dodgerblue4", lwd = 2)
points(data = daily, cond.surv~day, col = "palegreen4", pch = 20, cex = 2)
lines(data = daily, cond.surv~day, col = "palegreen4", lwd = 2)
legend(1, 0.25, c("Expected", "Unconditional", "Condiitonal"), 
       lty = c(1,1,1), lwd = c(2,2,2), pch = c(16, 18,20), col = c("gray60", "dodgerblue4", "palegreen4"))



### independent
# analyzing known fate data
bobwhite = read.csv("bobwhite.csv")
bobwhite

# 
# bobwhite = data.frame(week = c(1:6),
#                       total = c(45, 50, 47, 46, 44, 36),
#                       n.censored = c(0, 0, 0, 2, 5, 0),
#                       n.added = c(5, 0, 0, 0, 0, 0),
#                       n.deaths = c(0, 3, 1, 2, 1, 1))
# 
# write.csv(bobwhite, "bobwhite.csv", row.names = F)

# first need to calculate the number at risk
bobwhite$n.risk = bobwhite$total - bobwhite$n.censored

# conditional (interval) survival probability - see equation in Donovan Welden pg 317
bobwhite$Si = 1 - (bobwhite$n.deaths/bobwhite$n.risk)
bobwhite

# unconditional (cumulative) survival probability - use the cumprod() function to find the cumulative product
bobwhite$Sc = cumprod(bobwhite$Si)
bobwhite


# variance around cumulative probability
bobwhite$var = (bobwhite$Sc * bobwhite$Sc * (1-bobwhite$Sc))/bobwhite$n.risk
bobwhite

# calculate 95% confidence intervals ( = estimate +/- 1.96*SE)
bobwhite$lci = bobwhite$Sc - 1.96*sqrt(bobwhite$var)
bobwhite$uci = bobwhite$Sc + 1.96*sqrt(bobwhite$var)


# plot cumulative and daily survival
plot(data = bobwhite, Sc ~ week, pch = 18, cex = 2, col = "dodgerblue4", ylim = c(0,1), main = "",
     ylab = "Survival probability", xlab = "Day")
lines(data = bobwhite, Sc ~ week, lwd = 2, col = "dodgerblue4")
arrows(x0 = bobwhite$week, y0 = bobwhite$lci, x1 = bobwhite$week, y1 = bobwhite$uci, 
       length=0.05, angle=90, code=3,lwd = 2, col = "dodgerblue4")
points(data = bobwhite, Si ~ week, pch = 20, cex = 2, col = "goldenrod")
lines(data = bobwhite, Si ~ week, lwd = 2, lty = 2, col = "goldenrod")
legend(1, 0.2, c("Conditional (interval)", "Unconditional (cumulative)"),
       lty = c(2, 1), lwd = c(2,2), col = c("goldenrod", "dodgerblue4"), pch = c(20, 18))





### imperfect detection

N = 25
s = 0.9
p = 0.6









# only need to run this the first time you use this code on a new computer
install.packages("RMark")

# run this every time you use this script
library(RMark)



