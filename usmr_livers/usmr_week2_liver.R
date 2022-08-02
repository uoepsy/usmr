
# here's our population
pop <- sample(c(120, 185, 195), 200000, replace = TRUE)
# the population:
hist(pop)


# function which samples n people from pop
# and calculates the mean height
newsamplemean <- function(n=50){
  mean(sample(pop, size = n))
}

# we get a new sample each time
newsamplemean()
newsamplemean()
newsamplemean()

# imagine that we do it 1000 times
replicate(1000,newsamplemean())

# the means of all the different samples are centered on the population mean
hist(replicate(1000,newsamplemean()))


movies <- read.csv("https://uoepsy.github.io/data/hollywoodsample2.csv")
# we only have one sample. 
# it might contain higher than population average ratings, it might contain lower than population average ratings
# what do we do?

mean(movies$RottenTomatoes)
sd(movies$RottenTomatoes)/sqrt(nrow(movies))

curve(dnorm(x, mean = 47.04, sd = 3.69), from = 30, to = 70)
