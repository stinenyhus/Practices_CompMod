###Practices chapter 5###
library(pacman)
p_load(tidyverse)

#5M1 - Jacobs code! Simulating some spurious correlation 

n <- 100                             # number of cases

set.seed(5)                          # setting the seed makes the results reproducible
d <- 
  tibble(x_real = rnorm(n),          # x_real as Gaussian with mean 0 and SD 1 (i.e., the defaults)
         x_spur = rnorm(n, x_real),  # x_spur as Gaussian with mean = x_real
         y =      rnorm(n, x_real))  # y as Gaussian with mean = x_real

pairs(d, col = "firebrick4")

#5M2 - simulating masked relationships
n   <- 100   # number of cases
rho <- .7    # correlation between x_pos and x_neg

set.seed(5)  # setting the seed makes the results reproducible
d <- 
  tibble(x_pos = rnorm(n),                                # x_pos as a standard Gaussian
         x_neg = rnorm(n, rho * x_pos, sqrt(1 - rho^2)),  # x_neg correlated with x_pos
         y     = rnorm(n, x_pos - x_neg))

pairs(d, col = "firebrick4")

##5M4 - divorce rate 
mormons <- read.csv("Mormons.csv", sep = ";")

library(pacman)
p_load(rethinking, tidyverse)
data("WaffleDivorce")
data <- WaffleDivorce

length(unique(mormons$state))
length(unique(data$Location))
unique(mormons$state) == unique(data$Location)

mormons <- mormons[-4,] #Removing Nevada because it is not in rethinking data

#Removing the % sign from the column
mormons$perc_mor <- as.numeric(gsub("\\%", "", mormons$perc_mor))

mormons$state <- as.character(mormons$state)
mormons <- mormons[order(mormons$state),] # Ordering alphabetically

data$LDS_perc <- mormons$perc_mor


#Standardizing
data$Marriage_s <- (data$Marriage - mean(data$Marriage))/sd(data$Marriage)
data$LDS_perc_stand <- (data$LDS_perc-mean(data$LDS_perc))/sd(data$LDS_perc)
data$MedianAgeMarriage_s <- (data$MedianAgeMarriage - mean(data$MedianAgeMarriage)) / sd(data$MedianAgeMarriage)

p_load(brms)
model_mormon_marry <- brm(data = data, family = gaussian,
                          Divorce ~ 1 + MedianAgeMarriage_s + Marriage_s + LDS_perc_stand,
                          prior = c(prior(normal(10,10), class = Intercept),
                                    prior(normal(0,1), class = b),
                                    prior(uniform(0,10), class = sigma)),
                          iter =2000, warmup = 500, chains = 4, cores = 4,
                          seed = 5)

print(model_mormon_marry)

ggplot(filter(data, LDS_perc>=2), aes(Location, LDS_perc))+
  geom_point()+
  geom_line()

#Fox hard
data(foxes)
foxes <- foxes

p_load(brms)
weight_terr <- brm(weight ~ 1 + area, data= foxes, family = gaussian,
                   prior = c(prior(normal(4,2), class = Intercept),
                             prior(normal(0.5, 0.2), class = b),
                             prior(normal(1, 0.5), class = sigma)),
                   iter =2000, warmup = 500, chains = 4, cores = 4,
                   seed = 5)

print(weight_terr)
