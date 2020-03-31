##Chapter 7 - practices ##

library(pacman)
p_load(rethinking, tidyverse, brms)

data("tulips")

tulips
data <- tulips
summary(tulips$bed)

#Making three dummy variables, one for each case in bed 
tulips <- tulips %>% mutate(bed_a = ifelse(bed == "a", 1,0),
                            bed_b = ifelse(bed == "b", 1,0),
                            bed_c = ifelse(bed == "c", 1,0))

model1 <- brm(data = tulips, family = gaussian,
              blooms ~ 1 + water:shade + bed_a + bed_b + bed_c,
              prior = c(prior(normal(130, 100), class = Intercept),
                        prior(normal(0, 100), class = b),
                        prior(normal(0, 10), class = sigma)),
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              control = list(adapt_delta = 0.9),
              seed = 7)


model3 <- update(model1, 
                 formula = blooms~1+water+shade+water:shade)

model2 <- update(model1, 
                 formula = blooms~1+water+shade+water:shade + bed_a + bed_b + bed_c)
model2
model3
posterior_summary(model3)
posterior_summary(model2)

model2 <- add_criterion(model2, "waic")
model3 <- add_criterion(model3, "waic")

comp <- loo_compare(model2,model3, criterion = "waic")
#We find that model 2 is the best - the one with the bed-variables, though it is not much better (due to large errors?)
print(comp, simplify = F)


#7H3
data(rugged)

rugged <- rugged %>% mutate(log_gdp = log(rgdppc_2000))

#Get only complete cases
rugged_data_all <- rugged %>% filter(complete.cases(rgdppc_2000))

#Leave Seychelles out the analysis
rugged_data <- subset(rugged_data, country != "Seychelles")

#Making the model wo Sey for pp_check
no_Sey <- brm(data = rugged_data, family = gaussian, 
              log_gdp ~ 1 + cont_africa + rugged + cont_africa:rugged,
              prior = c(prior(normal(8.5,1), class = Intercept),
                        prior(normal(0,1), class = b),
                        prior(normal(0,3), class = sigma)),
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              sample_prior = "only",
              control = list(adapt_delta = 0.99),
              seed = 7)

#Prior predictive checks
pp_check(no_Sey)

#Making model with Sey for pp_check
with_Sey <- update(no_Sey, newdata = rugged_data_all)

#Prior predictive checks

pp_check(with_Sey)

#Making the real model wo Sey
no_Sey_real <- brm(data = rugged_data, family = gaussian, 
              log_gdp ~ 1 + cont_africa + rugged + cont_africa:rugged,
              prior = c(prior(normal(8.5,1), class = Intercept),
                        prior(normal(0,1), class = b),
                        prior(normal(0,3), class = sigma)),
              iter = 2000, warmup = 1000, chains = 4, cores = 4,
              sample_prior = "only",
              control = list(adapt_delta = 0.99),
              seed = 7)

pp_check(no_Sey_real)

#Making real model with Sey
with_Sey_real <- update(no_Sey_real, newdata = rugged_data_all)

#Comparing the models 
no_Sey_real
with_Sey_real



#7H4
p_load(rethinking, tidyverse, brms)
data(nettle)

#Creating outcome variable of interest
nettle$lang.per.cap <- nettle$num.lang / nettle$k.pop

#Taking logaritm of this
nettle$lang.per.cap.log <- log(nettle$lang.per.cap)

#Making model
lang_div <- brm(data=nettle, family = gaussian,
                lang.per.cap.log~ 1 + mean.growing.season + sd.growing.season + mean.growing.season:sd.growing.season,
                prior= c(prior(normal(-5,2), class = Intercept),
                         prior(normal(2,0.1), class = b),
                         prior(normal(0.6,0.1), class = sigma)),
                iter = 2000, warmup = 1000, chains = 4, cores = 4,
                control = list(adapt_delta = 0.99))

lang_div

