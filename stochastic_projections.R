# stochastic projections
# feb 2018

require(tidyverse)
require(cowplot)

nyrs = 50
N.init = 10

# parametric uncertainty

uncertain.b = F
min.b = 0.2
max.b = 0.5
input.b = 0.4
b = ifelse(uncertain.b, runif(1, min.b, max.b), input.b)

uncertain.d = F
min.d = 0.2
max.d = 0.5
input.d = 0.3
d = ifelse(uncertain.d, runif(1, min.d, max.d), input.d)

R = b-d

N = numeric(nyrs)
N[1] = N.init

for(t in 2:nyrs){
  N[t] = N[t-1] + N[t-1]*R
}

ggplot(data.frame(Year = c(1:nyrs), N = N), aes(x = Year, y = N)) +
  geom_point(size= 2) +
  geom_line()

# demographic and environmental stochasticity

reps = 50
nyrs = 50
N.init = 10

uncertain.b = T
mean.b = 0.4
sd.b = 0.4
input.b = 0.4

if(uncertain.b){b = abs(rnorm(nyrs, mean.b, sd.b))} else b = rep(input.b, nyrs)

uncertain.d = T
mean.d = 0.3
sd.d = 0.3
input.d = 0.3

if(uncertain.d){d = abs(rnorm(nyrs, mean.d, sd.b))} else d = rep(input.d, nyrs)
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

ggplot(data.frame(Year = rep(c(1:nyrs), reps), 
                  N = c(N),
                  rep = rep(c(1:reps), each = nyrs)), 
       aes(x = Year, y = N, col = as.character(rep))) +
  geom_point(size= 2) +
  geom_line() +
  theme(legend.position = "none")


# minimum viable population

reps = 500
nyrs = 50
N.init = 250
threshold = 0

uncertain.b = T
mean.b = 0.7
sd.b = 0.15
input.b = 0.4

if(uncertain.b){b = abs(rnorm(nyrs, mean.b, sd.b))} else b = rep(input.b, nyrs)

uncertain.d = F
mean.d = 0.3
sd.d = 0.3
input.d = 0.69

if(uncertain.d){d = abs(rnorm(nyrs, mean.d, sd.b))} else d = rep(input.d, nyrs)
if(uncertain.d){d = d/max(d)}

N = B = D = matrix(nrow = nyrs, ncol = reps)
N[1,] = N.init

for(r in 1:reps){
  
  for(t in 2:nyrs){
    B[t-1,r] = rpois(1, N[t-1,r]*b[t])
    D[t-1,r] = rbinom(1, N[t-1,r], d[t])
    N[t,r] = N[t-1,r] + B[t-1,r] - D[t-1,r]
  }
}
df = data.frame(Year = rep(c(1:nyrs), reps), 
                N = c(N),
                rep = rep(c(1:reps), each = nyrs)) 

ggplot(df, aes(x = Year, y = N, col = as.character(rep))) +
  geom_line() +
  theme(legend.position = "none") +
  ggtitle("All replicates") +
  ylab("Population size")

df %>% 
  group_by(Year) %>% 
  summarize(median = median(N),
            lci = quantile(N, 0.025), 
            uci = quantile(N, 0.975)) %>% 
  ggplot(aes(x = Year)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), col = "gray90", alpha = 0.5) +
  geom_line(aes(y = median), lwd = 1) +
  geom_hline(yintercept = threshold, lty = 2) +
  ylab("Population size") +
  ggtitle("Median and 95% quantiles")

p.ext = 1-mean(N[nyrs,] > threshold)  
p.ext
