##Chapter 8

library(pacman)
p_load(rethinking, tidyverse, brms)

num_weeks <- 1e5
positions <- rep(0,num_weeks)
current <- 10
for ( i in 1:num_weeks ) {
  # record current position
  positions[i] <- current
  # flip coin to generate proposal
  proposal <- current + sample( c(-1,1) , size=1 )
  # now make sure he loops around the archipelago
  if ( proposal < 1 ) proposal <- 10
  if ( proposal > 10 ) proposal <- 1
  # move?
  prob_move <- proposal/current
  current <- ifelse( runif(1) < prob_move , proposal , current )
}

tibble(week = 1:1e5,
       island = positions) %>% 
  
  ggplot(aes(week, island))+
  geom_point()+
  scale_x_continuous(breaks = seq(from = 0, to = 100, by = 20))+
  coord_cartesian(xlim = 0:100,
                  ylim = 1:10)


tibble(week = 1:1e5,
       island = positions) %>% 
  mutate(island = factor(island)) %>% 
  ggplot(aes(island))+
  geom_bar()


##HMC

data(rugged)

#make outcome variable log
rugged <- rugged %>% mutate(log_gdp = log(rgdppc_2000))

#Include only complete cases
rugged_clean <- rugged %>% drop_na(rgdppc_2000)

#Fit the model using HMC (WHAT - what is the difference? Seems like the exact same model)

model1 <- brm(data =rugged_clean, family = gaussian,
              log_gdp ~1 + rugged + cont_africa + rugged:cont_africa,
              prior = c(prior(normal(0,100), class = Intercept),
                        prior(normal(1,10), class = b),
                        prior(cauchy(0,2), class = sigma)))

print(model1)
#Sampling using more cores

model1_4cores <- update(model1, cores = 4)
print(model1_4cores)

#sampling using more chains
model1_4chains <- update(model1, chains = 4)
print(model1_4chains)
#changing chains and cores does not make a difference now because the model is rather simple 

##Visualization

posterior <- posterior_samples(model1)

#Making a pairs plot
pairs(model1)
pairs(posterior)

#Get cooler pair plots
p_load(GGally)

posterior %>% select(b_Intercept:sigma) %>% 
  ggpairs()
#Can be customized even more, see Kurz for example --> the output is a ggplot object 


#Getting IC's requires calling them directly and preferably saving them into the model (as has been done so far)
model1 <- add_criterion(model1, c("waic", "loo"))
l <- loo_compare(model1, model1_4chains, criterion = "loo")
print(l, simplify =F)

#Checking the chain with the trace plots produced by plot()
plot(model1)

brms::stancode(model1)


#When MCMC goes wrong
model2 <- brm(data = list(y = c(-1, 1)), #Only two observations
    family = gaussian,
    y ~ 1,
    prior = c(prior(uniform(-1e10, 1e10), class = Intercept), #Very bad, uniform priors
              prior(uniform(0, 1e10), class = sigma)), #Very bad, uniform priors
    inits = list(list(Intercept = 0, sigma = 1),
                 list(Intercept = 0, sigma = 1)),
    iter = 4000, warmup = 1000, chains = 2,
    seed = 8)


plot(model2) #See that the chains look horrible --> due to flat priors and only 2 observations

#Use weakly informing priors instead

model3 <-
  brm(data = list(y = c(-1, 1)), 
      family = gaussian,
      y ~ 1,
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(cauchy(0, 1), class = sigma)),
      inits = list(list(Intercept = 0, sigma = 1),
                   list(Intercept = 0, sigma = 1)),
      iter = 4000, warmup = 1000, chains = 2,
      seed = 8)

plot(model3) #Now the trace plots look much better 


#Simulate 100 data points from a gaussian distribution, with mean = 0 and sd = 1
y <- rnorm(100, mean = 0, sd = 1)

model4 <-
  brm(data = list(y           = y,
                  intercept_1 = 1,
                  intercept_2 = 1), 
      family = gaussian,
      y ~ 0 + intercept_1 + intercept_2,
      prior = c(prior(uniform(-1e10, 1e10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      inits = list(list(intercept_1 = 0, intercept_2 = 0, sigma = 1),
                   list(intercept_1 = 0, intercept_2 = 0, sigma = 1)),
      iter = 4000, warmup = 1000, chains = 2,
      seed = 8)

plot(model4)

model5 <- brm(data = list(y= y,
                  intercept_1 = 1,
                  intercept_2 = 1),
      family = gaussian,
      y ~ 0 + intercept_1 + intercept_2,
      prior = c(prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      inits = list(list(intercept_1 = 0, intercept_2 = 0, sigma = 1),
                   list(intercept_1 = 0, intercept_2 = 0, sigma = 1)),
      iter = 4000, warmup = 1000, chains = 2,
      seed = 8)

plot(model5)

