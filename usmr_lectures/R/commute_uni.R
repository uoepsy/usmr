set.seed(nchar("i like trains and not cars"))
commute_uni <- tibble(
  distance = round(15.03+rchisq(300, 6)*4.86),
  EV = sample(c(0,1),300,replace=T,prob=c(.6,.4)),
  Zenv_cons = round(rnorm(300),2),
  lp = scale(.03*distance + .4*Zenv_cons - .3*Zenv_cons*EV)[,1],
  commute = rbinom(300, 1, plogis(lp))
) %>%
  mutate(commute = ifelse(commute==1,"train","drive")) %>% select(-lp)

# From a small survey, the Uni HR and Transport dept know that staff 
# who live further away tend to be slightly more likely to take the train than to drive

# they want to know if people's environmental conscientiousness is associated with taking the train
# and they suspect that this may depend on whether people have an electric vehicle or not  
load(url("https://uoepsy.github.io/usmr/2232/lectures/R/commute_uni.RData"))

