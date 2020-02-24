###Chapter 3 - practices 

#Code for doing the practices
n <- 1001
n_success <- 6
n_trials  <- 9

d <-
  tibble(p_grid     = seq(from = 0, to = 1, length.out = n),
         # note we're still using a flat uniform prior
         prior      = 1) %>% 
  mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
  mutate(posterior  = (likelihood * prior) / sum(likelihood * prior))
)

#How much of the posterior probability lies below p = 0.2?
samples <-
  d %>% 
  sample_n(size = 10000, weight = posterior, replace = T)

d %>% 
  filter(p_grid < 0.2) %>% 
  summarise(sum = sum(posterior))
#0.085 % of the posterior probability for proportion of water lies below 0.2 

#How much of the posterior probability lies above p = 0.8?
d %>% 
  filter(p_grid > 0.8) %>% 
  summarise(sum = sum(posterior))
#12 % of the posterior probability for proportion of water lies above 0.8

#How much of the posterior probability lies between p = 0.2 and p = 0.8?
d %>% 
  filter(p_grid > 0.2 & p_grid < 0.8) %>% 
  summarise(sum(posterior))
#87.7 % of the posterior probability lies between 0.2 and 0.8
#Checking whether they sum to 100 %
87.7+12+0.0851
#Almost, makes sense with some rounding mistakes

#20 % of the posterior probability lies below which value of p?
q_20 <- quantile(samples$p_grid, prob = 0.2)
q_20
#Value of p that 20 % of the data lies below = 0.516

#20 % of the posterior probability lies above which value of p?
#Since the 20 above percent starts at the 80 % quantile, this is what I calculate
q_80 <- quantile(samples$p_grid, prob = 0.8)
q_80
#Value of p that 20 % of the data lies above = 0.759

#Which values of p contain the narrowest interval equal to 66% of the posterior probability?
#Which means find the 66 % HPDI
mode_hdi(samples$p_grid, .width = 0.66)
#the values between 0.516 and 0.786 contains the narrowest interval equal to 66 % of post prob 

#Not quite sure what the difference between question 3E6 and 3E7 is - maybe the answer i just gave is 3E7



###MEDIUM
#Suppose the globe tossing data had turned out to be 8 water in 15 tosses. 
#Construct the posterior distribution, using grid approximation and a plat prior

n <- 1001
n_success <- 8
n_trials  <- 15

#Constructing the posterior distribution
medium <-
  tibble(p_grid     = seq(from = 0, to = 1, length.out = n),
         # note we're still using a flat uniform prior
         prior      = 1) %>% 
  mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
  mutate(posterior  = (likelihood * prior) / sum(likelihood * prior))
)

#Drawing 10.000 samples (question 3M2)
samples_medium <-
  medium %>% 
  sample_n(size = 10000, weight = posterior, replace = T)

#Calculating 90 % HPDI for p (question 3M2)
mode_hdi(samples_medium$p_grid, .width = 0.9)
#HPDIs are p = 0.335 and p = 0.716

#Question 3M3
#Construct a posterior predictive check for this model and data. 
#This means simulate the distribution of samples, 
#averaging over the posterior uncertainty in p. 
#What is the probability of observing 8 water in 15 tosses?
n_success
n_trials
medium <- medium %>% mutate(likelihood = dbinom(n_success, size = n_trials, prob = p_grid)) %>% 
  mutate(posterior = (likelihood*prior)/sum(posterior))

samples_medium <-
  medium %>% 
  sample_n(size = 10000, weight = posterior, replace = T)

#Just trying this, not sure
check <- tibble(draws = rbinom(10000, size = 15, prob = samples_medium))

