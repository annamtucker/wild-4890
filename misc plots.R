
### Miscellaneous plots for presentations

require(tidyverse)
require(cowplot)

# 1 - exponential growth
N0 = 100
r = 0.1
t = c(1:50)

toplot = data.frame(t = rep(t, length(r)),
                    r = rep(r, each = length(t)))
toplot %>%
  mutate(N = N0 * exp(r*t)) %>%
  ggplot(aes(x = t, y = N)) +
  geom_line(lwd = 2) +
  xlab("Time") +
  ylab("Population size") +
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 34))


# 2 - logistic 
N0 = 100
K = 300
r = 0.1
t = c(1:75)

toplot = data.frame(t = t,
                    N = N0)
for(i in 2:nrow(toplot)){
  toplot$N[i] = toplot$N[i-1]*exp(r*((K-toplot$N[i-1])/K))
}

ggplot(toplot, aes(x = t, y = N)) +
  geom_line(lwd = 2) +
  xlab("Time") +
  ylab("Population size") +
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 34))



# 3 - predator-prey




# 4 - age-structured


# 5 - stage-structured


# 6 - SIR model