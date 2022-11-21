# A brief conceptual demonstration of maximum likelihood estimation. 
library(tidyverse)

splat <- c(0,0,1,0,1,1,1,0,1,0,1,1,1,0,0,0,1,1,1,1)

# what is the probability of being splatted? 

# let's just take a stab in the dark. prob of splatted is 0.4. 
# this is my "model"
mod_par <- 0.4

# prob of each observation, given n
ifelse(splat == 1, mod_par, 1-mod_par)

# the obs are independent, so we can multiply them to get P(all data | model)
# this is the likelihood! 
prod(ifelse(splat == 1, mod_par, 1-mod_par))

# it's a tiny number. and it's computationally difficult (lots of multiplication)
# instead of likelihood, take the log-likelihood
log(ifelse(splat == 1, mod_par, 1-mod_par))
# benefits = bigger range
# can use addition instead of multiplication. log(3) + log(4) = log(3*4) = log(12)
sum(log(ifelse(splat == 1, mod_par, 1-mod_par)))

# let's try a different model: 
mod_par <- 0.5
sum(log(ifelse(splat == 1, mod_par, 1-mod_par)))

mod_par <- 0.55
sum(log(ifelse(splat == 1, mod_par, 1-mod_par)))

mod_par <- 0.6
sum(log(ifelse(splat == 1, mod_par, 1-mod_par)))

mod_par <- 0.65
sum(log(ifelse(splat == 1, mod_par, 1-mod_par)))


# let's do it for various models
possible_models <- seq(0,1,.05)
# this is a for loop! 
# it "loops" over a set of things (in this case my "models")
# as it goes through the set, it calculates the log-likelihood of our splat data, given that model
# it will then add that to an object called ll, which i create in the environment first
ll <- c()
for(mod in possible_models){
  modll = sum(log(ifelse(splat == 1, mod, 1-mod)))
  ll <- c(ll, modll)
}

plot(x = some_models, y = ll, type=c("both"))

# it looks like it peaks at the value of 0.6
mod_par <- 0.6
sum(log(ifelse(splat == 1, mod_par, 1-mod_par)))

logLik(glm(splat ~ 1, family = binomial))


# often we talk instead about -2*LL (and we sometimes call it deviance)
plot(x = some_models, y = -2*ll, type=c("both"))

deviance(glm(splat ~ 1, family = binomial))
