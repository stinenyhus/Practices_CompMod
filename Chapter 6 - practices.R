###Chapter 6 - practices ###


#6E2

#Coin that lands on heads 70 % of the time


((0.7)*log(0.7))+((0.3)*log(0.3)) #0.61 --> how much uncertainty there is in the system 
#The higher the entropy, the more uncertain 
#The entropy in a system is highest when all possible outcomes have the same probaiblity

((0.99)*log(0.99))+((0.01)*log(0.01))

#6E3

((0.2)*log(0.2))+((0.25)*log(0.25))+((0.25)*log(0.25))+((0.3)*log(0.3))
#The entropy of this foursided die is 1.37

#6E4
((1/3)*log(1/3))*3
#The entropy of this die is 1.099



###HARD QUESTIONS
library(pacman)
library(rethinking)
data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
set.seed( 1000 )
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[ i , ]
d2 <- d[ -i , ]

summary(d1$height)

d1$age2 <- d1$age^2
d1$age3 <- d1$age^3
d1$age4 <- d1$age^4
d1$age5 <- d1$age^5
d1$age6 <- d1$age^6

p_load(tidyverse, brms)
model1 <- brm(data = d1, family = gaussian,
              height ~ 1+age,
              prior = c(prior(normal(130,30), class = Intercept),
                        prior(normal(0,50), class = b),
                        prior(normal(30,10), class = sigma)))

model2 <- update(model1,
                 newdata = d1,
             formula. = height ~ 1 + age + age2)

model3 <- update(model1,
                 newdata = d1,
             formula = height ~ 1 + age + age2 + age3)

model4 <- update(model1,
                 newdata = d1,
             formula = height ~ 1 + age + age2 + age3 + age4)

model5 <- update(model1,
                 newdata = d1,
             formula = height ~ 1 + age + age2 + age3 + age4 + age5)

model6 <- update(model1,
                 newdata = d1,
             formula = height ~ 1 + age + age2 + age3 + age4 + age5 + age6)

model1 <- add_criterion(model1, "waic")
model2 <- add_criterion(model2, "waic")
model3 <- add_criterion(model3, "waic")
model4 <- add_criterion(model4, "waic")
model5 <- add_criterion(model5, "waic")
model6 <- add_criterion(model6, "waic")

waics <- loo_compare(model1, model2, model3, model4, model5, model6, criterion = "waic")
print(waics, simplify = F)

weights <- model_weights(model1, model2, model3, model4, model5, model6, weights = "waic")
weights %>% round(digits = 5)

