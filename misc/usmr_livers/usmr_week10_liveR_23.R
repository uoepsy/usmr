## Week 9, Alien Songs
library(tidyverse)
load("https://uoepsy.github.io/usmr/2324/lectures/R/singers2.RData")













singers |> ggplot(aes(x=quality,y=SPLATTED,colour=song)) +
  ylab("p(SPLATTED)") +
  geom_jitter(size=3,width=0,height=.2,alpha=.3) +
  geom_smooth(method="glm",method.args=list(family=binomial)) +
  scale_y_continuous(breaks=seq(0,1,by=.2))
