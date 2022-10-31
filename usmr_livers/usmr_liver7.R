library(tidyverse)

# make some data
spooky_sample <- tibble(
  spookiness = rnorm(30, 12, 4),
  treats = (2*1) + (1*spookiness) + rnorm(30, 0, 5)
)

# fit the model
spooky_mod <- lm(treats ~ spookiness, data = spooky_sample)
# look at the model
summary(spooky_mod)

# visualise
ggplot(spooky_sample, aes(x=spookiness,y=treats)) + 
  geom_point() + 
  geom_smooth(method=lm)


# simulate to show variability
simspook <- function(){
  sim = tibble(
    spookiness = rnorm(30, 12, 4),
    treats = (2*1) + (1*spookiness) + rnorm(30, 0, 5)
  )
  simmod = lm(treats ~ spookiness, data = sim)
  return(coef(simmod))
}

res <- replicate(1000, simspook())
res2 <- tibble(
  int = res[1,],
  slope = res[2,]
)

ggplot(spooky_sample, aes(x=spookiness,y=treats)) + 
  geom_point()+
  geom_smooth(method=lm) + 
  geom_abline(data=res2, aes(intercept=int, slope=slope), alpha=.1)


# simulate to show the null 
simspook <- function(){
  sim = tibble(
    spookiness = rnorm(30, 12, 4),
    treats = (2*1) + (1*spookiness) + rnorm(30, 0, 5)
  )
  simmod = lm(treats ~ spookiness, data = sim)
  return(coef(simmod))
}

res <- replicate(1000, simspook())
res2 <- tibble(
  int = res[1,],
  slope = res[2,]
)

ggplot(spooky_sample, aes(x=spookiness,y=treats)) + 
  geom_point()+
  geom_smooth(method=lm) + 
  geom_abline(data=res2, aes(intercept=int, slope=slope), alpha=.1)
