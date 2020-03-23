###CHapter 6 ###

library(pacman)
p_load(tidyverse, rcartocolor, ggrepel)

d <- 
  tibble(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"), 
         brain   = c(438, 452, 612, 521, 752, 871, 1350), 
         mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5))

d

carto_pal(7, "BurgYl")


#Plotting brain size against body mass
d %>% ggplot(aes(mass, brain, label = species))+
  geom_point()+
  geom_text_repel()

#Fitting first model
model1 <- lm(brain ~ mass, data = d)

summary(model1)

#Computing R2
1-var(resid(model1))/var(d$brain)

#Fitting increasingly complex models
model2 <- lm(brain ~ mass + I(mass^2), data = d)
model3 <- lm(brain ~ mass + I(mass^2) + I(mass^3), data = d)
model4 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4), data = d)
model5 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5), data = d)
model6 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5) + I(mass^6), data = d)

#Plotting all six models
plot <- d %>% ggplot(aes(mass, brain))+
  geom_point()

p1 <- plot+stat_smooth(method ="lm", fullrange = TRUE, level=.89, formula = y~x)
p1

p2 <- plot+stat_smooth(method ="lm", fullrange = TRUE, level=.89, formula = y~ poly(x,2))
p2                       

p3 <- plot+stat_smooth(method ="lm", fullrange = TRUE, level=.89, formula = y~ poly(x,3))
p3

p4 <- plot+stat_smooth(method ="lm", fullrange = TRUE, level=.89, formula = y~ poly(x,4))
p4

p5 <- plot+stat_smooth(method ="lm", fullrange = TRUE, level=.89, formula = y~ poly(x,5))
p5

p6 <- plot+stat_smooth(method ="lm", fullrange = TRUE, level=.89, formula = y~ poly(x,6))
p6

p_load(ggpubr)

ggarrange(p1,p2,p3,p4,p5,p6)

#Investigating underfitting
model7 <- lm(brain ~ 1, data = d)

#Plotting it
p7 <- plot + stat_smooth(method = "lm", fullrange = T, level=.89, formula = y~1)
p7

model1

#Computing loglikelihood of already fitted model
-2*logLik(model1)

#Simulating change in deviance with overfitting 

p_load(rethinking)

n <- 20
kseq <- 1:5
n_sim   <- 1e3
n_cores <- 1

#Copy-pasting code that simulates 20 cases in both training and test sets 
dev_20 <-
  sapply(kseq, function(k) {
    print(k);
    r <- mcreplicate(n_sim, sim.train.test(N = n, k = k),
                     mc.cores = n_cores);
    c(mean(r[1, ]), mean(r[2, ]), sd(r[1, ]), sd(r[2, ]))
  })

n       <- 100
dev_100 <- 
  sapply(kseq, function(k) {
    print(k);
    r <- mcreplicate(n_sim, sim.train.test(N = n, k = k), 
                     mc.cores = n_cores);
    c(mean(r[1, ]), mean(r[2, ]), sd(r[1, ]), sd(r[2, ]))
  })

#Still just copy-pastying code I really do not understand
dev_tibble <-
  dev_20 %>% 
  as_tibble() %>% 
  bind_rows(
    dev_100 %>%
      as_tibble()
  ) %>% 
  mutate(n         = rep(c("n = 20", "n = 100"), each = 4),
         statistic = rep(c("mean", "sd"), each = 2) %>% rep(., times = 2),
         sample    = rep(c("in", "out"), times = 2) %>% rep(., times = 2)) %>% 
  gather(n_par, value, -n, -statistic, -sample) %>% 
  spread(key = statistic, value = value) %>% 
  mutate(n     = factor(n, levels = c("n = 20", "n = 100")),
         n_par = str_remove(n_par, "V") %>% as.double()) %>% 
  mutate(n_par = ifelse(sample == "in", n_par - .075, n_par + .075))

head(dev_tibble)

dev_text <-
  dev_tibble %>% 
  filter(n_par > 1.5, 
         n_par < 2.5) %>% 
  mutate(n_par = ifelse(sample == "in", n_par - .2, n_par + .28))

#Plotting un-understood code
dev_tibble %>% 
  ggplot(aes(x     = n_par, y = mean,
             ymin  = mean - sd, ymax = mean + sd,
             group = sample,
             color = sample, 
             fill  = sample)) +
  geom_pointrange(shape = 21) +
  facet_wrap(~n, scale = "free_y")



###Regularization ###

#Skeptical priors
tibble(x=seq(-3.5,3.5, by = 0.1)) %>% 
  ggplot(aes(x = x, ymin = 0))+
  geom_ribbon(aes(ymax = dnorm(x, mean = 0, sd = 0.2)),fill = "Red")+
  geom_ribbon(aes(ymax = dnorm(x, mean = 0, sd = 0.5)), fill = "Pink", alpha = 1/1.5)+
  geom_ribbon(aes(ymax = dnorm(x, mean = 0, sd = 1)), fill = "Purple", alpha = 1/3)




####Using information criteria###

#Model comparison
data(milk)
data <- milk %>% drop_na(ends_with("_s"))
#Always remove incomplete cases yourself because model comparison requires that 
#They should be fit to exactly the same observations 

#Scaling neocortex variable
data <- data %>% mutate(neocortex = neocortex.perc/100)
dim(data)

#Fitting models of four different complexities 
inits <- list(Intercept = mean(data$kcal.per.g),
              sigma = sd(data$kcal.per.g)) #Making a list with priors???

inits_list <- list(inits, inits, inits, inits) #Making four of them, one for each model???

p_load(brms)
model11 <- brm(data = data, family = gaussian, 
               kcal.per.g ~ 1, #Intercept only model
               prior = c(prior(uniform(-1000,1000), class = Intercept),
                         prior(uniform(0,100), class = sigma)),
               iter = 2000, warmup = 1000, chains = 4, cores = 4,
               inits = inits_list,
               seed = 6)

inits <- list(Intercept = mean(data$kcal.per.g),
              neocortex = 0,
              sigma     = sd(data$kcal.per.g))
inits_list <-list(inits, inits, inits, inits)

model12 <- 
  brm(data = data, family = gaussian,
      kcal.per.g ~ 1 + neocortex,
      prior = c(prior(uniform(-1000, 1000), class = Intercept),
                prior(uniform(-1000, 1000), class = b),
                prior(uniform(0, 100), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      inits = inits_list,
      seed = 6)


inits <- list(Intercept   = mean(data$kcal.per.g),
              `log(mass)` = 0,
              sigma       = sd(data$kcal.per.g))
inits_list <-list(inits, inits, inits, inits)

model13 <-
  update(model12, 
         newdata = data,
         formula = kcal.per.g ~ 1 + log(mass),
         inits   = inits_list)

inits <- list(Intercept   = mean(data$kcal.per.g),
              neocortex   = 0,
              `log(mass)` = 0,
              sigma       = sd(data$kcal.per.g))
inits_list <-list(inits, inits, inits, inits)

model14 <- 
  update(model13, 
         newdata = data,
         formula = kcal.per.g ~ 1 + neocortex + log(mass),
         inits   = inits_list)

waic(model11)
waic(model12)
waic(model13)
waic(model14)


milk_models <- rethinking::compare(model11, model12, model13, model14)


###Model averaging

#New data for the functions to come

nd <- tibble(neocortex = seq(.5, .8, length.out = 30),
             mass = 4.5)
nd

find the fitted from the last model

f <- fitted(model14, newdata = nd) %>% 
  as_tibble() %>% 
  bind_cols(nd)
f

#Get the model average from the four models 
av <- pp_average(model11, model12, model13, model14,
                 weights = "waic",
                 method = "fitted",
                 newdata = nd) %>% 
  as_tibble() %>% 
  bind_cols(nd)
av

#Plot the single best model against the averaged models

ggplot(av, aes(neocortex, Estimate))+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), alpha = 1/4)+
  geom_line()+
  geom_ribbon(data = f, aes(ymin = Q2.5, ymax = Q97.5), linetype = 2)+
  geom_line(data = f, linetype = 2)+
  geom_point(data = data, aes(y =kcal.per.g))
