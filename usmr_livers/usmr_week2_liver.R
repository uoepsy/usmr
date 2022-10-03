
# We've seen in central limit theorem the demonstration of what happens when we repeatedly sample.  

# in practice, we only have one sample. 

movies <- read.csv("https://uoepsy.github.io/data/hollywoodsample2.csv")

movies$Genre <- as.factor(movies$Genre)
movies$Movie <- as.factor(movies$Movie)

# it might contain higher than population average ratings, it might contain lower than population average ratings
# what do we do?

summary(movies)
view(movies)

mean(movies$RottenTomatoes)
sd(movies$RottenTomatoes)/sqrt(nrow(movies))

# we'll see more of this in the readings and exercises, but i'd like to show a little demonstration
library(tidyverse)
movies %>% 
  summarise(
    mRT = mean(RottenTomatoes),
    sRT = sd(RottenTomatoes),
    n_obs = n(),
    se = sRT / sqrt(n_obs)
  )

ggplot(movies, aes(RottenTomatoes)) + geom_histogram(colour='black', binwidth=5)



## Normal distributions 
# 68% of values lie within 1 standard deviation of the mean.
# 95% of values lie within 1.96 standard deviations of the mean.
# 99.7% of values lie within 3 standard deviations of the mean.


# Of the samples of n=50 we *could* take, we would expect
# 95% of their means to fall between:  
47.04 - (1.96 * 3.69)
47.04 + (1.96 * 3.69)
