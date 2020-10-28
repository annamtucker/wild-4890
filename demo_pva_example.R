# BIOL 521:           PVA by Stochastic Leslie Matrix Projection
# 			Multiple matrices approach
#			Scott Creel 9/30/2011

# INTRO

# Demographic PVA with Mountain Golden Heather
# from example in Morris & Doak 2002, Quantitative Conservation Biology
# Chapter 6.  See their Table 6.7 for data, which come from
# Frost 1990 and Gross et al. 1998.
# Uses a data set for Hudsonia montana, or mountain golden heather. This is a
# plant now restricted to a small portion of N. Carolina (in Pisgah
# National Forest.  It is listed as threatened under the ESA, but is considered
# G1 by NatureServe (G = all global populations threatened, 1 = critically 
# endangered.  As you've seen in other examples (e.g. Grizzly bears), changes 
# in USFWS listing often take many years.

# The original causes of decline are many (including fire suppression
# and direct losses to human activities.  The current range is so restricted
# that the species is at risk of extinction simply due to small population
# size.  Thus a PVA is of interest to establish the magnitude of extinction
# risk.  A demographic PVA is of interest so that elasticities (together with
# information on the observed variance in demographic parameteres) can inform
# responses.   

# ANNOTATED CODE

# Input the stored data and examine it.  It is a series of 4 transition (Leslie) matrices, 
# one matrix for each year from 1985 to 1988 each matrix is 6x6 because 
# there are 6 STAGE classes.  Some elements other than the top row (stage
# specific fertility) and subdiagonal(stage specific probability of transition
# to the next stage) are non-zero in these matrices.  These elements represent
# the probability of surviving but staying in the same stage class, or surviving and
# passing through more than one stage in a single year. In an
# AGE structured matrix, the 'probability of transition to the next stage' is
# the same thing as survival. In a stage structured matrix, survival is 
# decomposed into survivors that change stages and survivors that stay in the
# same stage

library(popbio)  	# load the popbio package
data(hudsonia)  	# load the Hudsonia data file included with popbio
hudsonia	    	# inspect the data - this is ALWAYS a good idea


### Set up an initial age structure or N0 vector (named 'n' below)

# this N0 vector is realistic for a plant, i.e. highly biased to young
# individuals.  Anything that has a large number of offspring that are
# each likely to die -- like seeds -- will have a very flat age pyramid
# i.e. a type III survivorship curve.  Recall that this species' life 
# history has 'recursive' loops, in which a survivor to remains in the 
# same stage class from one year to the next. These explain some of the off-subdiagonal
# elements discussed above.  With recursive loops, the stage structure does
# NOT have to show fewer individuals in each successive stage of the life
# history.  For example, individuals stay in the medium stage a long time (probability
# of staying the stage from one year to the next ranges from 0.44 to 0.65.
# So, even if the rate iof individuals enetering the medium stage is low,
# a fair number accumulate. 

n<-c(4264, 3,30,16,25,5)
names(n)<-c("seed",  "seedlings", "tiny", "small", "medium" , "large")
n


### 1000 replicates of projection for 50 years 
# with equal and unequal probabilities of selecting each matrix --
# that is, weighting how likely it is that the population will
# experience the demographic conditions captured in each year's
# transition matrix
#
# stoch.projection is a function defined in the popbio package.  

# As with any function, you can type ?stoch.projection in R to see the arguments for the
# function and its outputs.  You can do the same in RStudio by
# putting the cursor within the function and hitting the F1 key.
# In this example, notice in the code below that there
# is no argument for the number of years projected.  50 is the default, so if you
# omit "tmax = [any number]", you get projection over 50 years.


reps = 1000
x.eq<-stoch.projection(hudsonia, n, nreps=reps) 
# 1000 iterations of projection over 50 years, where
# each of the 4 matrices has equal probability of occuring

x.uneq<-stoch.projection(hudsonia, n, nreps=reps, prob=c(.2,.2,.2,.4)) 
#the same, but last matrix now more likely than the others

# Plot the frequency distribution of the population size in year 50 for the 1000
# replicates of each simulation, with a dashed vertical line indicating
# N0, initial population size.  Because this is a stochastic projection, you
# won't get identical results each time.  If we increase the number of replicates
# (5000 is the maximum allowed by the stoch.projection function), the results
# will be more stable.  

# Using a small number of replicates and comparing the differences
# among runs is actually an indirect method of examining ENVIRONMENTAL STOCHASTICITY.


par(mfrow =c(2,1))  # set up the plot area to display two plots, one above the other (2 rows, 1 column)

#equal weight for each Leslie matrix

hist(apply(x.eq, 1, sum), xlim=c(0,5000), ylim=c(0,200), col="green", 
     breaks=seq(0,5000, 100), xlab="Final population size at t=50", 
     main='Projection of stochastic growth for Hudsonia
     using equal and unequal probabilities of drawing each matrix')

## add initial pop size to the plot
sum(n)
abline(v=sum(n), lty=3)


#unequal weight 

hist(apply(x.uneq, 1, sum), xlim=c(0,5000), ylim=c(0,200), col = rgb(0, 0, 1, 0.2), 
     breaks=seq(0,10000, 100),xlab="Final population size at t=50", main = '' )


## add initial pop size to the plot
sum(n)
abline(v=sum(n), lty=3)

### Extensions - once you have run the model, do the following:
# 1. Calculate the probability of pseudo-exinction, defined as dropping 
# 	below ext.threshold, initially set to 500 individuals in this example.


N.final = apply(x.eq, 1,sum)  # sum the age classes to give N at final time step for each rep
ext.threshold = 500           # set the definition of pseudoextinction 
ext = 0                       # set the counter for number of extinctions to zero before tallying extinctions

# loop to tally up the number of cases where N.final is < ext.threshold
for (i in 1:length(N.final))
{if (N.final[i] < ext.threshold) ext = ext+1
}
ext  # number of cases where N.final is < ext.threshold
prob.ext = ext/reps
prob.ext  # proportion of cases where N.final is < ext.threshold, i.e. Prob{pseudoextinction}

# 2.  Repeat step one for several different extinction thresholds.  Is the relationship
#     between prob.ext and ext.threshold linear?

N.final = apply(x.eq, 1,sum)  #could use x.eq or x.uneq

ext.threshold = 0
increment = 100
ext=matrix(0,10,1)
prob.ext=matrix(0,10,1)
for (i in 1:10)
{ext.threshold = (ext.threshold + increment)
ext[i] = 0  
for (j in 1:length(N.final))
{if (N.final[j] < ext.threshold) ext = ext+1
}
ext[i] 
prob.ext[i] = ext[i]/reps
prob.ext[i] 
}
ext
prob.ext
thresholds = matrix(seq(0,900, by = 100),10,1)


par(mfrow = c(1,1))  #reset the plot area to show one plot

plot(thresholds, prob.ext, xlab = 'Pseudo-extinction threshold', 
     ylab = 'Probability of Extinction')


# fit linear model

lin.mod=lm(prob.ext~thresholds)
# get coefficient estimates (intercept and slope)from linear model
out=matrix(NA,1,2)
out[1:2]=coef(lin.mod)[1:2]

# add fitted model to plot 
abline(a = out[,1], b = out[,2], lty = 1)




# 2. Incorporate density dependence and evaluate the effect on population growth and 
#	pseudo-extinction.  This simulates the effect of systematic (not stochastic) changes
# 	in environmental conditions.

# 	?stoch.projection will let you examine all of the arguments of this function. 
#  	Nmax is one of the arguments, and it allows you to add density dependence
#  	to the projection.  This is 'ceiling'density dependence, the simplest possible version 
#  	of DD growth, where the population grows exponentially until it hits the ceiling 
#  	defined by Nmax, and then abruptly drops to r <= 0 or lambda  <= 1.

# 3. Examine the effect of different initial age structures and population sizes
# 	on the probability of pseudo-extinction.  This lets you consider how the 
#	demographic circumstances affect demographic stochasticity and thus extinction risk.
#