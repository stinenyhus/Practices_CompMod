##Chapter 8 - practices###

library(pacman)
p_load(rethinking, tidyverse, brms)

#8M1
data(rugged)

data <- rugged %>% mutate(log_gdp = log(rgdppc_2000))

rugged <- data %>% drop_na(rgdppc_2000)

rugged_model <- brm(data = rugged, family = gaussian,
                    log_gdp ~1 + rugged + cont_africa + rugged:cont_africa,
                    prior = c(prior(normal(0,100), class = Intercept),
                              prior(normal(1,10), class = b),
                              prior(uniform(0,10), class = sigma)))

#Sampling from posterior
posterior1 <- posterior_samples(rugged_model)

#Making enw data to predict on
nd <- tibble(rugged = seq(0,6.3, length.out = 30) %>% rep(.,times = 2), cont_africa =rep(0:1, each = 30))

#Fitting model and binding with new data
fitted_rugged1 <- fitted(rugged_model, newdata = nd) %>% 
  as_tibble() %>% bind_cols(nd) %>% mutate(cont_africa = ifelse(cont_africa ==1, "Africa", "not Africa"))

#Plotting - the uniform prior
uniform <- rugged %>% mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa")) %>% 
  ggplot(aes(x=rugged))+
  geom_smooth(data = fitted_rugged1, aes(y=Estimate, ymin=Q2.5, ymax = Q97.5,
                                        fill = cont_africa, color = cont_africa), 
              stat = "identity")+
  geom_point(aes(y=log_gdp, color = cont_africa))

#Fitting model with exponential prior
rugged_model2 <- brm(data = rugged, family = gaussian,
                    log_gdp ~1 + rugged + cont_africa + rugged:cont_africa,
                    prior = c(prior(normal(0,100), class = Intercept),
                              prior(normal(1,10), class = b),
                              prior(exponential(1), class = sigma)))

#Sampling from posterior
posterior2 <- posterior_samples(rugged_model2)

#Making enw data to predict on
nd <- tibble(rugged = seq(0,6.3, length.out = 30) %>% rep(.,times = 2), cont_africa =rep(0:1, each = 30))

#Fitting model and binding with new data
fitted_rugged2 <- fitted(rugged_model2, newdata = nd) %>% 
  as_tibble() %>% bind_cols(nd) %>% mutate(cont_africa = ifelse(cont_africa ==1, "Africa", "not Africa"))

#Plotting - the exponential prior
exponential <- rugged %>% mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa")) %>% 
  ggplot(aes(x=rugged))+
  geom_smooth(data = fitted_rugged2, aes(y=Estimate, ymin=Q2.5, ymax = Q97.5,
                                         fill = cont_africa, color = cont_africa), 
              stat = "identity")+
  geom_point(aes(y=log_gdp, color = cont_africa))

p_load(ggpubr)

ggarrange(uniform, exponential)


#There seems to be little or no difference between the models - the data overwhelms the prior 







#8M2

#Changing the prior to be cauchy and increasingly stronger

#Cauchy(0,15)
rugged_model3 <- brm(data = rugged, family = gaussian,
                    log_gdp ~1 + rugged + cont_africa + rugged:cont_africa,
                    prior = c(prior(normal(0,100), class = Intercept),
                              prior(normal(1,10), class = b),
                              prior(cauchy(0,15), class = sigma)))

model3_plot <- pp_check(rugged_model3)
model3_plot

#Cauchy(0,5)
rugged_model4 <- brm(data = rugged, family = gaussian,
                     log_gdp ~1 + rugged + cont_africa + rugged:cont_africa,
                     prior = c(prior(normal(0,100), class = Intercept),
                               prior(normal(1,10), class = b),
                               prior(cauchy(0,5), class = sigma)))

model4_plot <- pp_check(rugged_model4)

#Cauchy(0,1)
rugged_model5 <- brm(data = rugged, family = gaussian,
                     log_gdp ~1 + rugged + cont_africa + rugged:cont_africa,
                     prior = c(prior(normal(0,100), class = Intercept),
                               prior(normal(1,10), class = b),
                               prior(cauchy(0,1), class = sigma)))

model5_plot <- pp_check(rugged_model5)


ggarrange(model3_plot, model4_plot, model5_plot)