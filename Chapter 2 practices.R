## Practices chapter 2

##2M1

#Compute and plot the grid approximate posterior distribution for each of the following. Assume uniform p 

library(pacman)
p_load(rethinking, tidyverse)

### QUADRATIC APPROXIMATION
globe.qa <- map(
  alist(
    w ~ dbinom(9,p) , # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ) ,
  data=list(w=6) )
# display summary of quadratic approximation
precis( globe.qa )


###First through third - just change the values in the dbinom function to be hits and total observations 

###GRID APPROXIMATION
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior - uniform prior 
prior <- rep( 1 , 20 )

# compute likelihood at each value in grid
likelihood <- dbinom(5 , size=7 , prob=p_grid )
#Syntax og dbinom: dbinom(hits, total, grid size)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)


plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )


