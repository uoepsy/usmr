# Our current best guesses for the population:  
# heights have an average of 168, sd of 9.1
# 52% of people have brown eyes

# these lines of code will 'take a sample' from the population defined above:
heights <- rnorm(n = 228, mean = 168, sd = 9.1)
browneyes <- sample(c(0,1), size = 228, replace = TRUE, prob = c(.48, .52))

# each time we run it, we get a different sample
# and different statistics
mean(heights)
mean(browneyes)
sum(browneyes==1)/228


# 1 possible sample:
rnorm(n = 228, mean = 168, sd = 9.1)
# mean height of 1 possible sample:
mean(rnorm(n = 228, mean = 168, sd = 9.1))
# mean heights of 1000 possible samples:
replicate(1000, mean(rnorm(n = 228, mean = 168, sd = 9.1)))

# 1 possible sample:
sample(c(0,1), size = 228, replace = TRUE, prob = c(.48, .52))
# prop browneyes of 1 possible sample:
mean(sample(c(0,1), size = 228, replace = TRUE, prob = c(.48, .52)))
# prop browneyes of 1000 possible samples:
replicate(1000, mean(sample(c(0,1), size = 228, replace = TRUE, prob = c(.48, .52))))

# these are "sampling distributions"
m_heights <- replicate(1000, mean(rnorm(n = 228, mean = 168, sd = 9.1)))
p_breyes <- replicate(1000, mean(sample(c(0,1), size = 228, replace = TRUE, prob = c(.48, .52))))
                    
# hist to visualise
hist(m_heights)
hist(p_breyes)
# sd to quantify (gives us the "standard error")
sd(m_heights)
sd(p_breyes)

