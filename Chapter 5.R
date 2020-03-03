###Chapter 5 ###

#Load the data
library(pacman)
p_load(rethinking, tidyverse, brms)
data(WaffleDivorce)
d <- WaffleDivorce

#First standardizing the predictor variable we want to use
d$MedianAgeMarriage_s <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)

#Fitting the model
model_divorce_univ <- brm(data = d, family = gaussian,
                          Divorce ~ 1 + MedianAgeMarriage_s,
                          prior = c(prior(normal(10,10), class = Intercept),
                                    prior(normal(0,1), class = b),
                                    prior(uniform(0,10), class = sigma)),
                          iter =2000, warmup = 500, chains = 4, cores = 4,
                          seed = 5)

print(model_divorce_univ)

#When plotting, we want to plot the fitted values against some expected values
#We decide on values between -3 and 3.5 because?????

newdata <- tibble(MedianAgeMarriage_s = seq(-3, 3.5, length.out = 30)) # Same name in order to merge??

#Fit the values against those of the model
fitted <- 
  fitted(model_divorce_univ, newdata = newdata) %>% 
  as_tibble() %>% 
  bind_cols(newdata)

#Then plot it
ggplot(fitted, aes(MedianAgeMarriage_s, Estimate))+
  geom_smooth(aes(ymin = Q2.5, ymax = Q97.5), stat = "identity")+
  geom_point(data = d, aes( y =Divorce))


#Plotting mariage rate instead of mariage age 

#First standardizing the variable
d$Marriage_s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)

# Fitting the model
model_divorce_marrate <- brm(data = d, family = gaussian, 
                             Divorce ~ 1 + Marriage_s, 
                             prior = c(prior(normal(10,10), class = Intercept),
                                       prior(normal(0,1), class = b),
                                       prior(uniform(0,10), class = sigma)),
                             iter =2000, warmup = 500, chains = 4, cores = 4,
                             seed = 5)

print(model_divorce_marrate)

#Plotting it 
newdata <- tibble(Marriage_s = seq(from = -2.5, to = 3.5, length.out = 30))

#Fitting values against regression line
fitted <- fitted(model_divorce_marrate, newdata = newdata) %>% 
  as_tibble() %>% 
  bind_cols(newdata)

#Actally plotting 
ggplot(data = fitted, aes(x = Marriage_s, y = Estimate ))+
  geom_smooth(aes(ymin = Q2.5, ymax = Q97.5), stat = "identity")+
  geom_point(data = d, aes( y =Divorce))



#Fitting model with both predictors 

model_divorce_multi <- brm(data = d, family = gaussian,
                           Divorce ~ Marriage_s + MedianAgeMarriage_s, 
                           prior = c(prior(normal(0,10), class = Intercept),
                                     prior(normal(0,1), class = b), #Both predictors get the same prior
                                     prior(normal(0,10), class = sigma)),
                           iter =2000, warmup = 500, chains = 4, cores = 4,
                           seed = 5)

print(model_divorce_multi)

stanplot(model_divorce_multi)
#You can do this using the bayesplot package but it is usually not necessary
#It only allows you to do fancy things with colors and themes 
#You can also do it in tidybayes


###Predictor residual plots
#Marriage

model_pred_marr <- brm(data = d, family = gaussian,
                       Marriage_s~ 1 + MedianAgeMarriage_s,
                       prior = c(prior(normal(0,10), class = Intercept),
                                  prior(normal(0,1), class = b),
                                  prior(normal(0,10), class = sigma)),
                       iter = 2000, warmup = 500, chains = 4, cores = 4,
                       seed = 5)

print(model_pred_marr)
#Compute expected values using fitted()

fitted <- fitted(model_pred_marr) %>% 
  as_tibble() %>% 
  bind_cols(d)

#inspect
head(fitted)

#Plot regression and residuals
ggplot(fitted, aes(x = MedianAgeMarriage_s, y = Marriage_s))+
  geom_point()+
  geom_segment(aes(xend=MedianAgeMarriage_s, yend = Estimate)) + #These are the residual lines, dot to line 
  geom_line(aes(y=Estimate, color = "red"))+
  theme(legend.position = "none")


#Plot residuals against actual outcome values 

#Get the residuals in a dataframe
residuals <- residuals(model_pred_marr) %>% 
  as_tibble() %>% bind_cols(d)

#Plot
ggplot(residuals, aes(Estimate, Divorce))+
  stat_smooth(method = "lm", fullrange = T)+
  geom_vline(xintercept = 0, linetype = 2)+
  geom_point()

###The same could be done by making a model predicting MedianAgeMarriage instead 



###Counterfactual plots ###

#New data 
newdata <- tibble(Marriage_s = seq(-3,3, length.out = 30),
                  MedianAgeMarriage_s = mean(d$MedianAgeMarriage_s))

#Fit it to the model using both predictors 
countfact1 <- fitted(model_divorce_multi, newdata = newdata) %>% as_tibble() %>% 
  rename(f_ll = Q2.5, f_ul = Q97.5) %>% 
  bind_cols(predict(model_divorce_multi, newdata = newdata) %>% 
  as_tibble() %>% transmute(p_ll = Q2.5, p_ul = Q97.5), newdata)

#Plot it

ggplot(countfact1, aes(Marriage_s, Estimate))+
  geom_ribbon(aes(ymin = p_ll, ymax = p_ul), alpha = 1/8)+ 
  geom_smooth(aes(ymin = f_ll, ymax = f_ul), stat = "identity", alpha = 1/2)

#Same kind of plot could be made by reversing the two predictors



###Posterior prediction plots ###

fitted <- fitted(model_divorce_multi) %>% as_tibble() %>% bind_cols(d)

ggplot(fitted, aes(Divorce, Estimate))+
  geom_abline()+
  geom_point()+
  geom_linerange(aes(ymin=Q2.5, ymax=Q97.5), size = 0.3)+#95 % intervals
  geom_linerange(aes(ymin =Estimate-Est.Error, ymax = Estimate + Est.Error), size = 1)


#Making the residual plot
#Do not!!!! understand this, just copy paste

residuals(model_divorce_multi) %>% 
  as_tibble() %>% 
  rename(f_ll = Q2.5,
         f_ul = Q97.5) %>% 
  bind_cols(
    predict(model_divorce_multi) %>% 
      as_tibble() %>% 
      transmute(p_ll = Q2.5,
                p_ul = Q97.5),
    d) %>% 
  # here we put our `predict()` intervals into a deviance metric
  mutate(p_ll = Divorce - p_ll,
         p_ul = Divorce - p_ul) %>% 

  ggplot(aes(x = reorder(Loc, Estimate), y = Estimate)) +
  geom_hline(yintercept = 0, size = 1/2, 
             color = "firebrick4", alpha = 1/10) +
  geom_pointrange(aes(ymin = f_ll, ymax = f_ul),
                  size = 2/5, shape = 20, color = "firebrick4") + 
  geom_segment(aes(y    = Estimate - Est.Error, 
                   yend = Estimate + Est.Error,
                   x    = Loc, 
                   xend = Loc),
               size = 1, color = "firebrick4") +
  geom_segment(aes(y    = p_ll, 
                   yend = p_ul,
                   x    = Loc, 
                   xend = Loc),
               size = 3, color = "firebrick4", alpha = 1/10) +
  labs(x = NULL, y = NULL) +
  coord_flip(ylim = c(-6, 5)) +
  theme_bw() +
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0))



#MASKED RELATIONSHIPS - MILK ###

#load data
data("milk")
d <- milk

#Remove rethinking and load brms
rm(milk)
detach(package:rethinking, unload = T)
library(brms)

#Inspect data using some  weird function
d %>% select(kcal.per.g, mass, neocortex.perc) %>% pairs()

#First model: bivariate regression model
model_milk_bi <- brm(data = d, family = gaussian,
                     kcal.per.g ~ 1 + neocortex.perc,
                     prior = c(prior(normal(0,100), class = Intercept),
                               prior(normal(0,1), class = b),
                               prior(cauchy(0,1), class = sigma)),  #Still have NO idea what cauchy is
                     iter = 2000, warmup = 500, chains = 4, cores = 4,
                     seed = 5)

#Something about removing NA's - do not understand the _s
dcc <- 
  d %>%
  drop_na(ends_with("_s")) 

print(model_milk_bi, digits = 3)

min(dcc$neocortex.perc)
max(dcc$neocortex.perc)

#Change in neocortex size from smallest to biggest --> change in kcal pr g
fixef(model_milk_bi)[2] * (76 - 55) #--> only 0.097 --> less than 0.1 kilocalories 

#Plotting this

#Making new data
nd <- tibble(neocortex.perc = 54:80)
neo_fit <- fitted(model_milk_bi, newdata = nd, 
                  probs = c(.025, .975, .25, .75)) %>%
  as_tibble() %>% bind_cols(nd)

#plot it

ggplot(neo_fit, aes(neocortex.perc, Estimate))+
  geom_ribbon(aes(ymin = Q2.5, ymax=Q97.5))+
  geom_smooth(aes(ymin = Q25, ymax=Q75), stat = "identity", alpha = 1/5)+
  geom_point(data = dcc, aes(y = kcal.per.g), size = 2)


#Take the log of body mass
dcc <-
  dcc %>%
  mutate(log_mass = log(mass))

#Predicting kcal per g using female body mass
model_milk_mass <- brm(data = dcc, family = gaussian,
                       kcal.per.g~1+log_mass,
                       prior = c(prior(normal(0,100), class = Intercept),
                                 prior(normal(0,1), class = b),
                                 prior(uniform(0,1), class = sigma)),
                       iter = 2000, warmup = 500, chains = 4, cores = 4,
                       control = list(adapt_delta = 0.9),
                       seed = 5)

print(model_milk_mass, digits = 3)

#Plot the new model with log body mass
nd <- tibble(log_mass = seq(-2.5, 2, length.out = 30))
logmass_fit <- fitted(model_milk_mass, newdata = nd, 
                  probs = c(.025, .975, .25, .75)) %>%
  as_tibble() %>% bind_cols(nd)

#plot it

ggplot(logmass_fit, aes(log_mass, Estimate))+
  geom_ribbon(aes(ymin = Q2.5, ymax=Q97.5))+
  geom_smooth(aes(ymin = Q25, ymax=Q75), stat = "identity", alpha = 1/5)+
  geom_point(data = dcc, aes(y = kcal.per.g), size = 2)+
  coord_cartesian(xlim = range(dcc$log_mass))


#Both predictors at the same time 

model_milk_multi <- brm(data = dcc, family = gaussian,
                        kcal.per.g~1+neocortex.perc+log_mass,
                        prior = c(prior(normal(0,100), class = Intercept),
                                  prior(normal(0,1), class = b), #Same prior for both 
                                  prior(uniform(0,1), class = sigma)),
                        iter = 4000, warmup = 2000, chains = 4, cores = 4,
                        control = list(adapt_delta = 0.999),
                        seed = 5)

print(model_milk_multi, digits =3)                  

#Plotting intervals for predicted mean kilocalories --> counterfactual plots 
nd <- 
  tibble(neocortex.perc = 54:80 %>% as.double(),
         log_mass       = mean(dcc$log_mass))

countfact_neo <- model_milk_multi %>% fitted(newdata = nd,  probs = c(.025, .975, .25, .75)) %>%
  as_tibble() %>%
  bind_cols(nd)

#Plot it
ggplot(countfact_neo, aes(neocortex.perc, Estimate))+
  geom_ribbon(aes(ymin = Q2.5, ymax=Q97.5), alpha = 1/5)+
  geom_smooth(aes(ymin = Q25, ymax =Q75), stat = "identity", alpha = 1/3)+
  geom_point(data = dcc, aes(y=kcal.per.g))

#With log mass instead of neocortex
nd <- 
  tibble(log_mass = seq(-2.5, 5, length.out = 30),
         neocortex.perc = mean(dcc$neocortex.perc))

countfact_logmass <- model_milk_multi %>% fitted(newdata = nd, probs = c(.025, .975, .25, .75)) %>%
  as_tibble() %>%
  bind_cols(nd)

#Plot it
ggplot(countfact_logmass, aes(log_mass, Estimate))+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 1/5)+
  geom_smooth(aes(ymin = Q25, ymax =Q75), stat = "identity", alpha = 1/3)+
  geom_point(data = dcc, aes(y=kcal.per.g))


##Multicollinearity##

#Simulate leg data
n <- 100
set.seed(5)

data <- tibble(height = rnorm(n, mean = 10, sd = 2),
               leg_prop = runif(n, min =0.4, max = 0.5)) %>% 
  mutate(leg_left=leg_prop * height + rnorm(n, mean = 0, sd = 0.02),
         leg_right = leg_prop * height + rnorm(n, mean = 0, sd = 0.02))

#See correlations between leg lengths
data %>% select(leg_left:leg_right) %>% cor() %>% round(digits = 4)
#Strong correlation!! 

#Plot it 

ggplot(data, aes(leg_left, leg_right))+
  geom_point()

#We model it now to see that the result is not as expected
multi_col <- brm(data = data, family = gaussian, 
                 height ~ 1 + leg_left + leg_right,
                 prior = c(prior(normal(10,100), class = Intercept),
                           prior(normal(2,10), class = b),
                           prior(uniform(0,10), class = sigma)),
                 iter = 2000, warmup = 500, chains = 4, cores = 4,
                 seed = 5)

#Printing the result
print(multi_col)

#Plotting it to see
stanplot(multi_col)

pairs(multi_col, pars = parnames(b5.8)[2:3])



#Multicollinearity with real data --> milk
#Model kcal per g by percentage fat and percentage lactose 

library(rethinking)
data(milk)
d <- milk
rm(milk)
detach(package:rethinking, unload = TRUE)
library(brms)

model_milk_fat <- 
  brm(data = d, family = gaussian,
      kcal.per.g ~ 1 + perc.fat,
      prior = c(prior(normal(.6, 10), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 500, chains = 4, cores = 4,
      seed = 5)

#Using update function to have them both in the same element but as two seperate models
model_milk_lact <- 
  update(model_milk_fat,
         newdata = d,
         formula = kcal.per.g ~ 1 + perc.lactose)

#Compare coefficients from the two models 
posterior_summary(model_milk_fat) %>% round(digits = 3)
posterior_summary(model_milk_lact) %>% round(digits = 3)
#They seem to affect the data in opposite directions

#Both predictors in the same model
model_milk_fat_lact <- 
  update(model_milk_lact,
         newdata = d,
         formula = kcal.per.g ~ 1 + perc.fat + perc.lactose)

#Inspect result
posterior_summary(model_milk_fat_lact) %>% round(digits = 3)

#The two variables contain much of the same information 

#PAGE 149, TOP