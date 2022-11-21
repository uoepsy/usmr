library(tidyverse)
library(sjPlot)
load("R/reading.Rdata")


# releveling
m1 <- lm(R_AGE ~ hrs_wk * method, data = reading)
summary(m1)

reading <- reading %>% 
  mutate(
    method = fct_relevel(factor(method), "word"),
    hrs_wkC = hrs_wk - mean(hrs_wk)
  )

m1a <- lm(R_AGE ~ hrs_wk * method, data = reading)
summary(m1a)

#recentering
m1b <- lm(R_AGE ~ hrs_wkC * method, data = reading)
summary(m1b)

plot_model(m1a, type="int")
plot_model(m1b, type="int")


# For toys learning to read via the standard "phonics" method, there was no significant association between the amount of practise and reading age. At the average amount of practice (?? hours per week), learning by word was associated with ?? lower reading ages than learning by the phonics method. Crucially, the significant interaction between practise and method indicates that the extent to which practice infuences reading ages is different for these methods, with learners of the word method having reduced effects of extra hours of weekly reading practice (??)? This is visualised in Figure 1. 


