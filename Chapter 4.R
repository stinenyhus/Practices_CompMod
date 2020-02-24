###Chapter 4

pos <- replicate( 1000 , sum( runif(16,-1,1) ) ) %>% as_tibble()



ggplot(pos, aes(x = value))+
  geom_histogram()

set.seed(4)

prod(1+runif(12, 0, 0.1))

#Replicating 1000 times using purrr
p_load(tidyverse)
set.seed(4)
growth <- tibble(growth = map_dbl(1:10000, ~ prod(1 + runif(12,0,0.1))))

ggplot(growth, aes(growth))+
  geom_density()


#Loading weird anthropology data
library(rethinking)
data(Howell1)
data <- Howell1

#Removing rethinking again and using brms instead
rm(Howell1)
detach(package:rethinking, unload = T)
library(brms)

#Making dataframe with adults only
adults <- filter(data, age >=18)

ggplot(adults, aes(height))+
  geom_density()

#Height - setting likelihood and priors for mean and standard deviation
#Plotting shape of the prior for the mean
ggplot(data = tibble(x = seq(100, 250, by = 0.1)),
       aes(x= x, y = dnorm(x, mean = 178, sd = 20)))+
  geom_line()+
  xlab("mean")+
  ylab("density")

#Plotting shape of prior for standard deviation
#Uniform between 0 and 50 --> the prior probability that sd is between 0 and 50 is the same for all values

ggplot(data = tibble(x = seq(-10, 60, 0.1)),
       aes(x = x, y = dunif(x, min = 0, max = 50)))+
  geom_line()+
  scale_y_continuous(NULL, breaks = NULL)


#Simulating from both priors to get a prior probability of the distribution of heights 
set.seed(4)
tibble(sample_mu    = rnorm(n=10000, mean = 178,      sd = 20),
       sample_sigma = runif(n=10000,  min = 0, max = 50)) %>% 
  mutate(x = rnorm(n=10000, mean = sample_mu, sd = sample_sigma)) %>% 
  ggplot(aes(x=x))+
  geom_density(fill = "black", size = 0)+
  scale_y_continuous(NULL, breaks = NULL)


#Grid approximations
n <- 200

###You dont have to understand the next bit 
d_grid <- tibble(mu = seq(140, 160, length.out = n),
                 sigma = seq(4, 9, length.out = n)) %>% 
  expand(mu, sigma)
head(d_grid)

#making grid function
grid_function <- function(mu, sigma){
  dnorm(adults$height, mean = mu, sd = sigma, log = T) %>% 
    sum()
}

d_grid <- d_grid %>% 
  mutate(log_likelihood = map2(mu, sigma, grid_function)) %>% 
  unnest() %>% 
  mutate(prior_mu = dnorm(mu, mean = 178, sd = 20, log = T),
         prior_sigma = dunif(sigma, min = 0, max = 50, log = T)) %>% 
  mutate(product = log_likelihood + prior_mu + prior_sigma) %>% 
  mutate(probability = exp(product - max(product)))

head(d_grid)

#So now we have approximated the posterior distribution with grid approximation 
###Begint to understand again from here 

set.seed(4)

d_grid_samples <- 
  d_grid %>% 
  sample_n(size = 10000, replace = T, weight = probability) #sample_n() draws samples, in this case 10000
                                                            #from the d_grid tibble 

#Plotting the samples

d_grid_samples %>% 
  ggplot(aes(x = mu, y = sigma)) + 
  geom_point(size = .9, alpha = 1/15) 

#Plot densities of mu and sigma at once

d_grid_samples %>% 
  select(mu, sigma) %>% 
  gather() %>% 
  
  ggplot(aes(x = value))+
  geom_density(size = 0)+
  scale_y_continuous(NULL, breaks = NULL)+
  facet_wrap(~key, scales = "free")


#Quadratic approximation
p_load(brms)
b4.1 <- 
  brm(data = adults, family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(uniform(0, 50), class = sigma)),
      iter = 31000, warmup = 30000, chains = 4, cores = 4,
      seed = 4)
####DID NOT WORK
#Problems with the quadratic approximation 


#4.4 Adding a predictor

#Plotting weight against height 
ggplot(adults, aes(weight, height))+
  geom_point(size = 2)


#Fitting the model
#This makes only little sense to me
b4.3 <- 
  brm(data = adults, family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(156, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma)),
      iter = 41000, warmup = 40000, chains = 4, cores = 4,
      seed = 4)

plot(b4.3)
 
#Interpreting the model
posterior_summary(b4.3)[1:3,]

#Correlation matrix
posterior_samples(b4.3) %>%
  select(-lp__) %>%
  cor() %>%
  round(digits = 2)

#Centering the weigths
adults <- adults %>% mutate(weight_c = weight-mean(weight))

#Fit the model with meancentered weigths

b4.4 <-  brm(data = adults, family = gaussian,
             height ~ 1 + weight_c,
             prior = c(prior(normal(178, 100), class = Intercept),
                       prior(normal(0, 10), class = b),
                       prior(uniform(0, 50), class = sigma)),
             iter = 46000, warmup = 45000, chains = 4, cores = 4,
             seed = 4)

posterior_summary(b4.4)[1:3, ]

#Plotting 
adults %>% ggplot(aes(weight, y = height))+
  geom_abline(intercept = fixef(b4.3)[1],
              slope = fixef(b4.3)[2], size = 1.5)+
  geom_point(size = 2, alpha = 0.3)

#Adding uncertainty

#Extracting all posteriors 
post <- posterior_samples(b4.3)
post %>% slice(1:5)

#Trying adding more lines (random samples drawn from the posterior distribution)

#Four model directly copied

#First adding only 10 lines to show uncertainty 
n <- 10 

b.10 <- 
  brm(data = adults %>%
        slice(1:n),  # note our tricky use of `n` and `slice()`
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

#Now with 50
n <- 50

b.50 <- 
  brm(data = adults %>%
        slice(1:n), 
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

#Now with 150 lines
n <- 150

b.150 <- 
  brm(data = adults %>%
        slice(1:n), 
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

#Finally with 352 lines
n <- 352

b.352 <- 
  brm(data = adults %>%
        slice(1:n), 
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

#Putting chain into dataframes
post10  <- posterior_samples(b.10)
post50  <- posterior_samples(b.50)
post150 <- posterior_samples(b.150)
post352 <- posterior_samples(b.352)



#Plotting the four models individually and saving them 
p10 <- 
  ggplot(data =  adults[1:10 , ], 
         aes(x = weight, y = height)) +
  geom_abline(intercept = post10[1:20, 1], 
              slope     = post10[1:20, 2],
              size = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(adults$weight),
                  ylim = range(adults$height)) +
  labs(subtitle = "N = 10") +
  theme_bw() +
  theme(panel.grid = element_blank())

p50 <-
  ggplot(data =  adults[1:50 , ], 
         aes(x = weight, y = height)) +
  geom_abline(intercept = post50[1:20, 1], 
              slope     = post50[1:20, 2],
              size = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(adults$weight),
                  ylim = range(adults$height)) +
  labs(subtitle = "N = 50") +
  theme_bw() +
  theme(panel.grid = element_blank())

p150 <-
  ggplot(data =  adults[1:150 , ], 
         aes(x = weight, y = height)) +
  geom_abline(intercept = post150[1:20, 1], 
              slope     = post150[1:20, 2],
              size = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(adults$weight),
                  ylim = range(adults$height)) +
  labs(subtitle = "N = 150") +
  theme_bw() +
  theme(panel.grid = element_blank())

p352 <- 
  ggplot(data =  adults[1:352 , ], 
         aes(x = weight, y = height)) +
  geom_abline(intercept = post352[1:20, 1], 
              slope     = post352[1:20, 2],
              size = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(adults$weight),
                  ylim = range(adults$height)) +
  labs(subtitle = "N = 352") +
  theme_bw() +
  theme(panel.grid = element_blank())

p_load(ggpubr)
ggarrange(p10, p50, p150,p352)

#Add uncertainty shades around regression line 
#- make a lot of data point for the distribution of possible mean heights for a person who weighs 50 kg 
mu_at_50 <- post %>% transmute(mu_at_50 = b_Intercept+b_weight * 50)

#Plot the uncertainty expressed in the mean of the different possible mean heights 
ggplot(mu_at_50, aes(mu_at_50))+
  geom_density()

#Get the 89 and 95 % high probability density intervals
mean_hdi(mu_at_50[,1], .width = c(0.89, 0.95))

#Calculate all HPDI's
mu <- fitted(b4.3, summary=F)
mu

#Specify only some predictor values --> here, only weights between 25 and 70 kgs
weight_seq <- tibble(weight = seq(25, 70, by = 1)) 

mu <- fitted(b4.3, summary= F,
             newdata = weight_seq) %>% 
  as_tibble() %>% 
  #Namin the columns after the weight value used to compute them 
  set_names(25:70) %>% 
  mutate(iter = 1:n())
str(mu)

#Now we have a matrix where each column is a given weigth, and each row is a sample from the post distrib

#Converting the data from wide to long format
p_load(reshape2)
?melt()
melt_mu <- melt(mu, id = "iter", value.name = "height")

#Plot it
ggplot(melt_mu, aes(x=variable, heigth))+
  geom_point(melt_mu %>% filter(iter<101))

#Predict height instead of sampling from posterior distribution to also have sd's

pred_height <- predict(b4.3, newdata = weight_seq) %>% 
  as_tibble() %>% 
  bind_cols(weight_seq)
#This tibble contains simulated heights, not samples 

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
d2 <- 
  d2 %>%
  mutate(weight_c = weight - mean(weight))

mu_summary <-
  fitted(b4.3, 
         newdata = weight_seq) %>%
  as_tibble() %>%
  # let's tack on the `weight` values from `weight_seq`
  bind_cols(weight_seq)

d2 %>%
  ggplot(aes(x = weight)) +
  geom_ribbon(data = pred_height, 
              aes(ymin = Q2.5, ymax = Q97.5),
              fill = "grey83") +
  geom_smooth(data = mu_summary,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
              stat = "identity",
              fill = "grey70", color = "black", alpha = 1, size = 1/2) +
  geom_point(aes(y = height),
             color = "navyblue", shape = 1, size = 1.5, alpha = 2/3) +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  theme(text = element_text(family = "Times"),
        panel.grid = element_blank())



###POLYNOMIAL REGRESSION

#Standardizing the variable height
data <- data %>% mutate(weight_s = (weight - mean(weight))/sd(weight))

#plotting height on weight - both standardized and non- to see there is no difference
ggplot(data, aes(weight, height))+
  geom_point()

ggplot(data, aes(weight_s, height))+
  geom_point()

#Quadratic model
b4.5 <- 
  brm(data = data, family = gaussian,
      height ~ 1 + weight_s + I(weight_s^2),
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

print(b4.5)

#In order to plot the model
weight_seq_standardized <- tibble(weight_seq = seq(-2.5, 2.5, length.out = 30))

f_quad <- fitted(b4.5,
                 newdata = weight_seq_standardized) %>% 
  as_tibble() %>% 
  bind_cols(weight_seq_standardized)

p_quad <-
  predict(b4.5, 
          newdata = weight_seq) %>%
  as_tibble() %>%
  bind_cols(weight_seq)  
