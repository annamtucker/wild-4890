# wild 4890 lab 8
# survival estimation
# mar 2018


### part 1 - known fate (Kaplan-Meier) ####

# read in the data - make sure your working directory is set to the correct folder!
bobwhite = read.csv("bobwhite.csv")
bobwhite

# interval (conditional) survival probability - equation in notes
bobwhite$Si = 1 - (bobwhite$deaths/bobwhite$atrisk)
bobwhite

# cumulative (unconditional) survival probability - equation in notes
# use the cumprod() function to find the cumulative product of interval probabilities
bobwhite$Sc = cumprod(bobwhite$Si)
bobwhite

# variance around cumulative probability - equation in notes
bobwhite$var = (bobwhite$Sc^2 * (1-bobwhite$Sc))/bobwhite$atrisk
bobwhite

# calculate 95% confidence intervals around Sc( = estimate +/- 1.96*SE) 
# SE = square root of var - use sqrt() function
bobwhite$lci = bobwhite$Sc - 1.96*sqrt(bobwhite$var)
bobwhite$uci = bobwhite$Sc + 1.96*sqrt(bobwhite$var)
bobwhite

# plot cumulative survival probabilities
plot(data = bobwhite, Sc ~ week, pch = 18, cex = 2, col = "dodgerblue4", ylim = c(0,1.25), main = "",
     ylab = "Survival probability", xlab = "Day")
lines(data = bobwhite, Sc ~ week, lwd = 2, col = "dodgerblue4")
arrows(x0 = bobwhite$week, y0 = bobwhite$lci, x1 = bobwhite$week, y1 = bobwhite$uci, 
       length=0.05, angle=90, code=3,lwd = 2, col = "dodgerblue4")
points(data = bobwhite, Si ~ week, pch = 20, cex = 2, col = "goldenrod")
lines(data = bobwhite, Si ~ week, lwd = 2, lty = 2, col = "goldenrod")
legend(1, 0.2, c("Interval (conditional)", "Cumulative (unconditional)"),
       lty = c(2, 1), lwd = c(2,2), col = c("goldenrod", "dodgerblue4"), pch = c(20, 18))


# how does survival differ between study area or groups?
# manually enter data
n.risk = c(34, 25)        # number at risk for each group/study area
n.survived = c(18, 8)    # number survived in each group/study area

# chi-square test
prop.test(x = n.survived, n = n.risk)

# foxes
s1 = 18/34
s2 = 8/25

se1 = sqrt((s1*(1-s1))/34)
se2 = sqrt((s2*(1-s2))/25)

### part 2 - imperfect detection (CJS) ####

# only need to run this the first time you use this code on a new computer
install.packages("RMark")

# run this every time you use this script
library(RMark)

# load dipper data
data(dipper)
head(dipper)

# fit model with time-constant survival and detection probability
model1 = mark(dipper)
summary(model1)
model1$results$real

# fit time-varying survival model

# these two lines of code process the data for the model function
dipper.process = process.data(dipper, model = "CJS", begin.time = 1980)
dipper.ddl = make.design.data(dipper.process)

# these two lines of code define the models for survival (phi) and detection (p)
phi.t = list(formula = ~time)
p.dot = list(formula = ~1)

# fit the model and display the results
model2 = mark(dipper.process, dipper.ddl, model.parameters = list(Phi = phi.t, p = p.dot))
summary(model2)
model2$results$real

# compare AIC of the two models
print(c("time-constant survival AIC" = model1$results$AICc,
        "time-varying survival AIC" = model2$results$AICc))

# effect of floods on survival
dipper.ddl$Phi$flood = 0
dipper.ddl$Phi$flood[dipper.ddl$Phi$time == 1981 | dipper.ddl$Phi$time == 1982] = 1

phi.flood = list(formula = ~flood)

model3 = mark(dipper.process, dipper.ddl, model.parameters = list(Phi = phi.flood, p = p.dot))
exp(-0.5599)

# extract model estimates
# t1980 is a non-flood year, t1981 is a flood year
model3$results$real

# compare all models
all.results = collect.models(type = "CJS")
all.results

# odds of survival in a flood year
exp(model3$results$beta[2,])


# plot survival estimates and 95% CI for each year
years = c(1980:1985)
flood = c(0, 1, 1, 0, 0, 0)
ests = plogis(model3$results$beta[1,1] + flood*model3$results$beta[2,1])
lci =  plogis(qlogis(ests) - 1.96*(flood*model3$results$beta[2,2] + (1-flood)*model3$results$beta[1,2]))
uci =  plogis(qlogis(ests) + 1.96*(flood*model3$results$beta[2,2] + (1-flood)*model3$results$beta[1,2]))

# make plot (run all lines of code below)
plot(ests[flood == 0] ~ years[flood == 0], pch = 18, cex = 2, col = "dodgerblue3",main = "", ylim = c(0.3, 0.7),
     ylab = "Survival probability", xlab = "Year")
lines(ests ~ years, lwd = 2, lty = 2, col = "gray50")
arrows(x0 = years[flood == 0], y0 = lci[flood == 0], x1 = years[flood == 0], y1 = uci[flood == 0], 
       length=0.05, angle=90, code=3,lwd = 2, col = "dodgerblue3")
points(ests[flood==1] ~ years[flood==1], pch = 20, cex = 3, col = "sienna2")
arrows(x0 = years[flood == 1], y0 = lci[flood == 1], x1 = years[flood == 1], y1 = uci[flood == 1], 
       length=0.05, angle=90, code=3,lwd = 2, col = "sienna2")
