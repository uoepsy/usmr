library(tidyverse)
load(url("https://uoepsy.github.io/usmr/2223/lectures/R/reading.Rdata"))

mod.mm <- lm(R_AGE ~ age + hrs_wk + method + hrs_wk:method, data=reading)

summary(mod.mm)

# intercept is one point
p_coef_int <- 
  tibble(
    age = 0, 
    hrs_wk = 0,
    method = "phonics"
  )

predict(mod.mm, newdata = p_coef_int)
coef(mod.mm)



# coefs are comparing two points
p_coef_age <- 
  tibble(
    age = c(0,1), 
    hrs_wk = c(0,0), # change these to whatever, just keep them the equal
    method = c("phonics","phonics") # change these to whatever, just keep them equal
  )

predict(mod.mm, newdata = p_coef_age)
coef(mod.mm)

diff(predict(mod.mm, newdata = p_coef_age))



p_coef_hw <- 
  tibble(
    age = c(0,0), # change these to whatever, just keep them the equal
    hrs_wk = c(0,1), 
    method = c("phonics","phonics") # *has* to be phonics
  )

predict(mod.mm, newdata = p_coef_hw)
coef(mod.mm)
diff(predict(mod.mm, newdata = p_coef_hw))



p_coef_meth <- 
  tibble(
    age = c(0,0), # change these to whatever, just keep them the equal
    hrs_wk = c(0,0), # *has* to be zero
    method = c("phonics","word") 
  )

predict(mod.mm, newdata = p_coef_meth)
coef(mod.mm)
diff(predict(mod.mm, newdata = p_coef_meth))




p_coef_hwmeth <- 
  tibble(
    age = c(0,0,0,0), # change these to whatever, just keep them the equal
    hrs_wk = c(0,1,0,1), 
    method = c("phonics","phonics","word","word") 
  )


predict(mod.mm, newdata = p_coef_hwmeth)

pp <- predict(mod.mm, newdata = p_coef_hwmeth)
pp
(pp[4] - pp[3]) - (pp[2] - pp[1])

coef(mod.mm)




