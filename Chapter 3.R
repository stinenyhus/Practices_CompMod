###Chapter 3 ###

#Vampire or not

PrPV <- 0.95 #True positives
PrPM <- 0.01 #False positives
PrV <- 0.001 #Prior probability of positive 
#Positive test - what is the probability that it is true?
#Calculate average probability of positive result
PrP <- PrPV*PrV+PrPM* (1-PrV)

PrVP <- (PrPV * PrV)/ PrP
PrVP

###DOING IT IN TIDYVERSE
library(pacman)
p_load(tidyverse)
#Making a tibble with the values

vampire <- tibble(pr_positive_vampire = 0.95,
                  pr_positive_mortal = 0.01,
                  pr_vampire = 0.001) %>% 
  mutate(pr_positive = pr_positive_vampire * pr_vampire + pr_positive_mortal * (1-pr_vampire)) %>% 
  mutate(pr_vampire_positive = pr_positive_vampire * pr_vampire /pr_positive)

vampire



###Generating samples###

#Number of grid points
n <- 10000
n_succes <-  6
n_trials <-  9

data <- tibble(p_grid = seq(from = 0, to = 1, length.out = n),
               prior = 1 #Flat, uniform prior
               ) %>% 
  mutate(likelihood = dbinom(n_succes, size = n_trials, prob = p_grid)) %>% 
  mutate(posterior = (likelihood * prior) /sum(likelihood*prior))

data


#Drawing the samples
n_samples <- 1e4

set.seed(3)
samples <- data %>% dplyr::sample_n(size = n_samples, weight = posterior, replace = TRUE)
samples
glimpse(samples)

#Need to make an x-axis with the number of samples
samples %>% mutate(sample_number = 1:n()) %>% 
  ggplot(aes(sample_number, p_grid)) +
  geom_line(size =1/10)+
  labs(x = "Sample number", y = "Proportion of water")

#Making density plot
samples %>% ggplot(aes(p_grid))+
  geom_density(fill = "black")+
  coord_cartesian(xlim = 0:1)+
  xlab("Proportion of water")


#Intervals of defined boundaries
#Prob that water is less than 0.5

data %>% filter(p_grid<0.5) %>% #Taking only values below 0.5
  summarise(sum= sum(posterior))
#Value = 0.172 --> probability that water is less than 0.5 of Earth is approx 17 % 

#Using samples
samples %>% filter(p_grid<0.5) %>% 
  summarise(sum = n()/1e4)


#Finding values between 0.5 and 0.75 
samples %>% filter(p_grid >0.5 & p_grid<0.75) %>% 
  summarise(sum =n()/1e4)

#Check how to make the plots in the tidyverse-rewriting

#Check quantiles
q_80 <- quantile(samples$p_grid, prob = 0.8)
q_80

samples %>% 
  select(p_grid) %>% 
  pull() %>% 
  quantile(prob = .8)

#10th and 90th quantiles
q_10_90 <- quantile(samples$p_grid, prob = c(0.1, 0.9))
q_10_90

#Plot quantiles as in the book: tidyverse rewriting 

#3 water in 3 throws
n_succes <- 3
n_trials <- 3

data$p_grid <- seq(from = 0, to = 1, length.out = 1000)
data <- data %>% mutate(likelihood = dbinom(n_succes, size = n_trials, prob = p_grid)) %>% 
  mutate(posterior = (likelihood*prior)/sum(posterior))

set.seed(3)

#New data samples
samples <-
  data %>% 
  sample_n(size = n_samples, weight = posterior, replace = T)

#10 and 90 percentile intervals using rethinking package

q_10_90_2 <- rethinking::PI(samples$p_grid, prob = 0.5)
q_10_90_2

p_load(tidybayes)
#MAybe use this package to compute HPDI --> returns numeric vector to use in ggplot 

rethinking::HPDI(samples$p_grid, prob = .5)

#Find the maximum a posteriori value (MAP)

data %>% 
  arrange(desc(posterior)) %>% 
  select(posterior) %>% 
  top_n(n = 1)
