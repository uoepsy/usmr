#https://uoepsy.github.io/wayback/dapr1slides/dapR1_lec11_Bootstrap_CIs.html#4



# samples and populations
# 


library(tidyverse)
library(janitor)
set.seed(777)
the_pop <- read_csv("https://uoepsy.github.io/data/usmrsurvey2.csv")

my_sample <- sample_n(the_pop, 40)

ggplot(my_sample, aes(x=internal_control,y=conscientiousness))+
  geom_point()+
  geom_smooth(method="lm")

my_mod <- lm(conscientiousness ~ internal_control, my_sample)
summary(my_mod)




simstudies <- function(){
  anothersample = sample_n(the_pop, 40)
  anothermodel = lm(conscientiousness ~ internal_control,anothersample)
  return(coef(anothermodel))
}

manystudies <- t(replicate(1e4, simstudies()))
manystudies <- as_tibble(manystudies)
manystudies <- clean_names(manystudies)
ggplot(the_pop, aes(x=internal_control,y=conscientiousness))+
  geom_point(alpha=.3)+
  geom_abline(data = manystudies, 
              aes(intercept=intercept, slope = internal_control),alpha=.1)

sd(manystudies$intercept)
sd(manystudies$internal_control)
summary(my_mod)$coefficients
