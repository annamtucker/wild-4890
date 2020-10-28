# lotka-volterra predator-prey dynamics
# 21 jan 2018

require(tidyverse)
require(cowplot)

# parameters

R = 0.2 #prey growth rate
q = 0.08 #pred starvation rate
f = 0.006 #pred conversion efficiency
a = 0.010 #attack rate

R = 0.3 #prey growth rate
q = 0.05 #pred starvation rate
f = 0.001 #pred conversion efficiency
a = 0.06 #attack rate

t = 100
C = rep(NA, t)
C[1] = 5
V = rep(NA, t)
V[1] = 1000

for(i in 2:t){
  V[i] = ifelse(V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1] >0, V[i-1] + R*V[i-1] - a*C[i-1]*V[i-1],0)
  C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1] > 0, C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1], 0)
}

#par(mfrow = c(1,2))
#plot(V ~ c(1:t), type="l", lwd = 2)
#plot(C ~ c(1:t), type = "l", lwd = 2, col = "red")

dat = data.frame(V, C, Time = c(1:length(V)))
dat %>%
  mutate(Cplot = C*100) %>% 
  select(V, Cplot, Time) %>% 
  gather(type, N, 1:2) %>% 
  ggplot(aes(x = Time, y = N, col = type)) +
  scale_y_continuous("Prey population size", 
                     sec.axis = sec_axis(~./100, name = "Predator population size")) +
  geom_line(lwd = 2) +
  scale_color_discrete(labels = c("Predator", "Prey"), name = "") +
  theme(legend.position = "top")



ggplot(dat, aes(x = V, y = C)) +
  geom_hline(yintercept = input$R/input$a, lty = 2, lwd = 1) +
  geom_vline(xintercept = input$q/(input$a*input$f), lty = 2, lwd = 1) +
  geom_path(lwd = 2, arrow = arrow(type = "closed"))+
  xlab("Prey population size") +
  ylab("Predator population size") +
  theme(axis.title = element_text(size = 14),
        legend.position = "top") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5)))


ggplot(dat, aes(x = V, y = C)) +
  geom_path(lwd = 2, arrow = arrow(type = "closed"))+
  geom_point(size = 4, alpha = 0.5) +
  xlab("Prey population size") +
  ylab("Predator population size")



# estimate LV parameters for lynx and hare
dat = read.csv("lynx_hare.csv")
par(mfrow = c(1,2))
plot(hare ~ year, data = dat, type="l", lwd = 2)
plot(lynx ~ year, data = dat, type = "l", lwd = 2, col = "red")

lvmod = function(theta, V, C, nyears){
  
  R = exp(theta[1])
  q = plogis(theta[2])
  f = plogis(theta[3])
  a = plogis(theta[4])
  sigmaV = exp(theta[5])
  sigmaC = exp(theta[6])
  
  for(i in 1:(nyears-1)){
    Vpred[i+1] = Vpred[i] + R*Vpred[i] - a*Cpred[i]*Vpred[i]
    Cpred[i+1] = Cpred[i] + a*f*Vpred[i]*Cpred[i] - q*Cpred[i]
  }
  
  -sum(dnorm(V, Vpred, sigmaV, log = T), dnorm(C, Cpred, sigmaC, log = T))
}

theta = c(log(1), qlogis(0.1), qlogis(0.01), qlogis(0.01), log(1000), log(100))

fit = optim(par = theta, fn = lvmod, method = "BFGS",
            V = dat$hare, C = dat$lynx, nyears = nrow(dat))


require(jagsUI)

lv = cat(file = "lvmod.txt", "
model{

  R ~ dunif(0, 5)
  q ~ dunif(0, 1)
  f ~ dunif(0, 1)
  a ~ dunif(0, 1)
  
  sigmaV ~ dunif(0, 20)
  tauV <- pow(sigmaV, -2)

  #sigmaC ~ dunif(0, 3)
  #tauC <- pow(sigmaC, -2)

  Vpred[1] ~ dunif(20000, 350000)
  Cpred[1] ~ dunif(1500, 6000)
  
  for(i in 2:(nyears-1)){
    Vpred[i] = Vpred[i-1] + R*Vpred[i-1] - a*Cpred[i-1]*Vpred[i-1]
    Cpred[i] = Cpred[i-1] + a*f*Vpred[i-1]*Cpred[i-1] - q*Cpred[i-1]
  }
  
  for(i in 1:(nyears-1)){
    V[i] ~ dnorm(Vpred[i], tauV)
    #C[i] ~ dnorm(Cpred[i], tauC)
  }
}
")

jagsdat = list(V = dat$hare[1:50]/1000,
               C = dat$lynx[1:50]/1000,
               nyears = nrow(dat[1:50,]))

parms = c("R", "q", "f", "a", "sigmaV", "sigmaC")

lv = jags(jagsdat, inits = NULL, parms, "lvmod.txt", parallel = T,
          n.chains = 3, n.iter = 10000, n.burnin = 1000, n.adapt = 1000, n.thin = 1)



## simulate data


R = 0.45 #prey growth rate
q = 0.25 #pred starvation rate
f = 0.03 #pred conversion efficiency
a = 0.01 #attack rate
Kv = 5000

t = 100
C = numeric(t)
C[1] = 20
V = numeric(t)
V[1] = 1000

for(i in 2:t){
  V[i] = ifelse(V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1] > 0, 
                V[i-1] + R*V[i-1]*((Kv-V[i-1])/Kv) - a*C[i-1]*V[i-1], 0)
  C[i] = ifelse(C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1] > 0, 
                C[i-1] + a*f*V[i-1]*C[i-1] - q*C[i-1], 0)
}

par(mfrow = c(1,2))
plot(V ~ c(1:t), type="l", lwd = 2)
plot(C ~ c(1:t), type = "l", lwd = 2, col = "red")

# add stochasticity

Vobs = rnorm(t, V, sd = 300)
Vobs[which(Vobs<0)] = 150
Cobs = rnorm(t, C, sd = 10)
Cobs[which(Cobs<0)] = 5

par(mfrow = c(1,2))
plot(Vobs ~ c(1:t))
lines(V ~ c(1:t), type="l", lwd = 2)

plot(Cobs ~ c(1:t))
lines(C ~c(1:t), type = "l", lwd = 2)

dat = data.frame(pred = round(Cobs), prey = round(Vobs), year = c(1890:1989))

