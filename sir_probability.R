# basic probabilities

require(ggplot2)
require(cowplot)

# one die
one = data.frame(probs = rep(1/6, 6),
                 val = c(1:6))

ggplot(one, aes(x = val, y = probs)) +
  geom_histogram(stat = "identity", binwidth = 1) +
  scale_x_continuous(name = "Number", breaks = seq(1,6,1)) +
  ylab("Frequency") +
  ylim(0,0.5)

# two dice
two = data.frame(val = c(2:12),
                 probs = c(1/36, 2/36, 3/36, 4/36, 5/36, 6/36,
                           5/36, 4/36, 3/36, 2/36, 1/36))

ggplot(two, aes(x = val, y = probs, fill = as.character(val))) +
  geom_histogram(stat = "identity", binwidth = 1, alpha= 0.6,col = "black") +
  scale_x_continuous(name = "Number", breaks = seq(1,12,1)) +
  scale_fill_manual(values = c("purple", "purple4", "gray30","red", "orange", 
                               "yellow", "yellowgreen", "darkgreen", "lightskyblue",
                               "blue", "navyblue")) +
  ylab("Frequency") +
  ylim(0,0.2) +
  theme(legend.position = "none")


# SIR model

beta = 0.787
alpha = 0
m = 0.2
t = 10
N =250
I.init = 3

# deterministic
S = rep(0, t)
I = rep(0, t)
R = rep(0, t)

S[1] = (N-I.init)/N
I[1] = I.init/N

for(i in 2:t){
  S[i] = S[i-1] - S[i-1]*I[i-1]*beta
  I[i] = I[i-1] + S[i-1]*I[i-1]*beta - alpha*I[i-1]
  R[i] = R[i-1] + alpha*I[i-1]
}

df =data.frame(N = c(S*N, I*N, R*N),
               time = rep(1:t, 3),
               type = rep(c("S", "I", "R"), each = t))

ggplot(df,aes(x = time, y = N, col = type)) +
  geom_line(lwd = 2) +
  xlab("Time") +
  ylab("Number in each state") +
  scale_color_discrete(name = "", labels = c("infected", "recovered", "susceptible")) +
  theme(legend.position = "top")

# stochastic
reps = 1000

out = data.frame()

for(r in 1:reps){
  
  S = rep(0, t)
  I = rep(0, t)
  R = rep(0, t)
  
  S[1] = N-I.init
  I[1] = I.init
  
  Ntot = rep(NA, t)
  Ntot[1] = N
  
  for(i in 2:t){
    lambda = ifelse(0.5-0.001*Ntot[i-1] < 0, 0, 0.5-0.001*Ntot[i-1])
    
    new.i = rbinom(1, round(S[i-1]*(I[i-1]/Ntot[i-1])), beta)
    new.r = rbinom(1, I[i-1], alpha)
    new.s = ifelse(S[i-1] > 0, sum(rpois(S[i-1], lambda)), 0)
    dead.i = rbinom(1, I[i-1], m)

    S[i] = S[i-1] - new.i + new.s
    I[i] = I[i-1] + new.i - new.r - dead.i
    R[i] = R[i-1] + new.r
    
    Ntot[i] = S[i] + I[i] + R[i]
  }

  df =data.frame(N = c(S, I, R),
                 time = rep(1:t, 3),
                 type = rep(c("S", "I", "R"), each = t), 
                 rep = r)
  
  out = bind_rows(out, df)
}

striplabs = c("I" = "Infected", 
              "R" = "Recovered", 
              "S" = "Susceptible")

out %>% 
  filter(type != "R") %>% 
  group_by(time, type) %>% 
  summarize(avg = mean(N),
            min = min(N),
            max = max(N),
            lci = quantile(N, 0.025), 
            uci = quantile(N, 0.975)) %>% 
  mutate(type = striplabs[type]) %>% 
  ggplot(aes(x = time, fill = type)) +
  geom_line(aes(y = lci), lty = 2, lwd = 1) +
  geom_line(aes(y = uci), lty = 2, lwd = 1) +
  geom_line(aes(y = avg), lwd = 2) +
  facet_wrap(~type, scales= "free") +
  scale_y_continuous(breaks = seq(0, 500, 50)) +
  scale_color_discrete(name = "", labels = c("infected", "recovered", "susceptible")) +
  theme(legend.position = "none",
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "white")) +
  xlab("Time") +
  ylab("Number in each state") 

  
