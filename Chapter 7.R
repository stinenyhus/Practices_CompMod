###Chapter 7 ###

library(pacman)
p_load(rethinking, brms, tidyverse)

data(rugged)
d <- rugged

d <- d %>% mutate(log_gdp = log(rgdppc_2000))

#Get only complete cases
dd <- d %>% filter(complete.cases(rgdppc_2000))

#Split data by continents

Africa <- dd %>% filter(cont_africa==1)
non_Africa <- dd %>% filter(cont_africa==0)

#Fit first model - African nations

model1 <- brm(data = Africa, family = gaussian,
              log_gdp ~ 1+ rugged, 
              prior = c(prior(normal(8,100), class = Intercept),
                        prior(normal(0,1), class = b),
                        prior(uniform(0,10), class = sigma)),
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              seed = 7)

#Fit second model - non-Africa nations

model2 <- update(model1, newdata = non_Africa)

#Wrangling to plot
nd <- tibble(rugged = seq(0,6.3, length.out = 30)) #Values for length = min and max from the original variable 
summary(Africa$rugged)

fitted_model1 <- fitted(model1, newdata = nd) %>% as_tibble() %>% bind_cols(nd)
fitted_model2 <- fitted(model2, newdata = nd) %>% as_tibble() %>% bind_cols(nd)

#Stacking data from the two models 
full <- full_join(fitted_model1, fitted_model2) %>% 
  mutate(cont_africa =rep(c("Africa", "not Africa"), each = 30))

dd %>% mutate(cont_africa = ifelse(cont_africa ==1, "Africa", "not Africa")) %>% 

ggplot(aes(x=rugged))+
  geom_smooth(data = full, 
              aes(y=Estimate, ymin=Q2.5, ymax = Q97.5,
                  fill = cont_africa, color = cont_africa), 
              stat = "identity")+
  geom_point(aes(y=log_gdp, color = cont_africa))+
  facet_wrap(~cont_africa)


#Fit model on entire dataset
model3 <- update(model1, newdata = dd)

#Update model to use the dummy variable for being Africa or not
model4 <- update(model3, newdata = dd, formula = log_gdp ~ 1 + rugged + cont_africa)
model4
#Get information criteria for the two models
model3 <- add_criterion(model3, c("loo", "waic"))
model4 <- add_criterion(model4, c("loo", "waic"))

#Compare models, first by loo then by waic
loo_compare(model3, model4, criterion = "loo")
loo_compare(model3, model4, criterion = "waic")

#Get the model weights
model_weights(model3, model4, weights = "waic") %>% round(digits = 3)
  #Almsot all weight is put on the second model, it seems to be far better

#Plotting posterior predictions 
nd <- tibble(rugged = seq(0,6.3, length.out = 30) %>% rep(.,times = 2), cont_africa =rep(0:1, each = 30))

fitted_model4 <- fitted(model4, newdata = nd) %>% 
  as_tibble() %>% bind_cols(nd) %>% mutate(cont_africa = ifelse(cont_africa ==1, "Africa", "not Africa"))


dd %>% mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa")) %>% 
ggplot(aes(x=rugged))+
  geom_smooth(data = fitted_model4, aes(y=Estimate, ymin=Q2.5, ymax = Q97.5,
                  fill = cont_africa, color = cont_africa), 
              stat = "identity")+
  geom_point(aes(y=log_gdp, color = cont_africa))

#Fit interaction model

model5 <- update(model4, formula = log_gdp ~ 1 + rugged*cont_africa)
model5
#Compare with loo
model5 <- add_criterion(model5, c("loo", "waic"))

l <- loo_compare(model3, model4, model5, criterion = "loo")
print(l, simplify =F)

#Plotting interaction model
fitted_model5 <-
  fitted(model5, newdata = nd) %>%  # we can use the same `nd` data from last time
  as_tibble() %>%
  bind_cols(nd) %>%
  mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa"))

dd %>% mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa")) %>% 
  ggplot(aes(x=rugged, color = cont_africa))+
  geom_smooth(data = fitted_model5, aes(y=Estimate, ymin=Q2.5, ymax = Q97.5,
                                        fill = cont_africa), 
              stat = "identity")+
  geom_point(aes(y=log_gdp))+
  facet_wrap(~cont_africa)

#Get the model estimates
posterior_summary(model5)


#Get the posterior samples
post <- posterior_samples(model5)

post %>% transmute(gamma_Africa = b_rugged + `b_rugged:cont_africa`,
                   gamma_notAfrica = b_rugged) %>% 
  gather(key, value) %>% group_by(key) %>% summarise(mean = mean(value))

#Plotting it 
post %>% transmute(gamma_Africa = b_rugged + `b_rugged:cont_africa`,
                   gamma_notAfrica = b_rugged) %>% 
  gather(key, value) %>% 
  
  ggplot(aes(x = value, group = key, color = key, fill = key))+
  geom_density()


#Prob that slope within Africa is less than slope outside Africa?
post %>%
  mutate(gamma_Africa    = b_rugged + `b_rugged:cont_africa`,
         gamma_notAfrica = b_rugged) %>% 
  mutate(diff = gamma_Africa -gamma_notAfrica) %>%
  summarise(Proportion_of_the_difference_below_0 = sum(diff < 0) / length(diff))
#Highly implausible that slope within Africa is less than slope outside Africa 
#I.e. it is highly implausible that that effect of ruggedness on log-GDP is lower in Africa than outside 


#SYMMETRY OF LINEAR INTERACTION#

#Africa depending on ruggedness

#New data to use in fitted()

nd <- tibble(rugged = rep(range(dd$rugged), times = 2),
             cont_africa = rep(0:1, each = 2))
nd

#Fit
fitted_model5_2 <- fitted(model5, newdata = nd) %>% 
  as_tibble() %>% 
  bind_cols(nd) %>% 
  mutate(ox=rep(c(-0.5,0.5), times = 2))

#Adding to dd
dd %>% mutate(ox =ifelse(rugged > median(rugged), 0.5, -0.5),
              cont_africa = cont_africa+ox) %>% 
  select(cont_africa, everything()) %>% 
  
  ggplot(aes(x=cont_africa, color = factor(ox)))+
  geom_smooth(data = fitted_model5_2,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5, 
                  fill = factor(ox), linetype = factor(ox)),
              stat = "identity")+
  geom_point(aes(y=log_gdp))+
  scale_x_continuous("Continent", breaks = 0:1, 
                     labels = c("other", "Africa"))


#Continuous interactions

data(tulips)
data <- tulips

#Model without interactions
model6 <-
  brm(data = data, family = gaussian,
      blooms ~ 1 + water + shade,
      prior = c(prior(normal(0, 100), class = Intercept),
                prior(normal(0, 100), class = b),
                prior(uniform(0, 100), class = sigma)),
      iter = 2000, warmup = 1000, cores = 4, chains = 4,
      seed = 7)

#Warnings about divergent transitions makes us increase adapt_delta (???)
model6 <-
  update(model6,
         prior = c(prior(normal(0, 100), class = Intercept),
                   prior(normal(0, 100), class = b),
                   prior(cauchy(0, 10), class = sigma)),
         control = list(adapt_delta = 0.9),
         seed = 7)

#Making the model with interactions
model7 <- update(model6, 
                 formula = blooms ~ 1 + water + shade + water:shade)

#Get posterior estimates
posterior_summary(model6) %>% round(digits = 3)
posterior_summary(model7) %>% round(digits = 3)

#Compare with information criteria
model6 <- add_criterion(model6, "waic")
model7 <- add_criterion(model7, "waic")

w <- loo_compare(model6, model7, criterion = "waic")
print(w, simplify = F)

#Center and re-estimation 
data <- data %>% mutate(shade_c = shade - mean(shade),
                        water_c = water - mean(water))

#Fit model with centered variables
model8 <- brm(data = data, family = gaussian,
              blooms ~ 1 + water_c + shade_c,
              prior = c(prior(normal(130, 100), class = Intercept),
                        prior(normal(0, 100), class = b),
                        prior(cauchy(0, 10), class = sigma)),
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = 0.9),
              seed = 7)

#With interaction
model9 <- update(model8, formula= blooms ~1+water_c+shade_c+water_c:shade_c)

posterior_summary(model8) %>% round(digits =2)
posterior_summary(model9) %>% round(digits=2)

#How to make triptych plots

#What to loop through - the values that the predictor we want to keep constant should have at the points we wanna show
shade_seq <- -1:1
shade_seq

for(w in -1:1) {
  # define the subset of the original data
  dt <- data[data$water_c == w, ]
  # defining our new data
  nd <- tibble(water_c = w, shade_c = shade_seq)
  # smaple from the posterior distribution of the model in question
  f <- 
    fitted(model9, newdata = nd) %>%
    as_tibble() %>%
    bind_cols(nd)
  
  #Now specifying the plot we want
  fig <- ggplot()+
    geom_smooth(data = f, 
                aes(x= shade_c, y = Estimate, ymin = Q2.5, ymax =Q97.5),
                stat = "identity")+
    geom_point(data = dt, aes(shade_c, blooms))
  
  plot(fig)
}
