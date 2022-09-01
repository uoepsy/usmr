
# here's our population
pop <- rnorm(10000, 170, 12)
# pop <- runif(10000, 120, 210)

# the population:
hist(pop)

# function which samples n people from pop
# and calculates the mean height
newsamplemean <- function(n=20){
  mean(sample(pop, size = n))
}

# we get a new sample each time
newsamplemean()
newsamplemean()
newsamplemean()

# imagine that we do it 5000 times
sMeans <- replicate(5000, newsamplemean())

# the means of all the different samples are centered on the population mean
hist(sMeans)
plot(density(sMeans))

curve(dnorm(x, mean = 170, sd = 12/sqrt(20)), 
      add = TRUE, col = "red")

# CLT!
# if you try and estimate the mean of a population by sampling repeatedly, it doesn't really matter what shape the distribution of the population has, you're going to get a normal distribution of the sample means.  