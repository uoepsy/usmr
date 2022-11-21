library(tidyverse)
library(plotly)
source("https://uoepsy.github.io/usmr/2223/lectures/usmr_3Dplots.R")

head(bdat)

## our original model
m1 <- lm(brain_vol ~ weekly_actv, data = bdat)

summary(m1)

ggplot(bdat, aes(x=weekly_actv, y=brain_vol)) +
  geom_point()+
  geom_smooth(method=lm)


## legos and playmos have different brain vols.. 
ggplot(bdat, aes(x=weekly_actv, y=brain_vol, col=group)) +
  geom_point()

# intuition about the slope? 
# when we take into account lego/playmo differences, 
# is weekly actv slope shallower or steeper?  

# fit it and see:  
m2 <- lm(brain_vol ~ group + weekly_actv, data = bdat)
summary(m2)

coef(m1)
coef(m2)

library(sjPlot)
library(patchwork)
plot_model(m1, type="eff", terms=c("weekly_actv"), show.data = TRUE) + 
plot_model(m2, type="eff", terms=c("weekly_actv"), show.data = TRUE)
# Two lines is actually a model in 3 dimensions (although it might not feel like it). 


# Easier to see this with 2 continuous predictors:  
ggplot(bdat, aes(x=weekly_actv, y=brain_vol, col=hydration)) +
  geom_point(size=3)

# or 3d:
plt1_cloud

# our initial model, m1:
plt2a_surfcloud

# our new model:  
m2 <- lm(brain_vol ~ hydration + weekly_actv, data = bdat)
summary(m2)
plt2_surfcloud

# what does this mean for how we interpret our coefficients? 

# the increase in y for a one unit increase in x1 when…

# ... holding x2 constant.
# ... controlling for differences in x2.
# ... partialling out the effects of x2.
# ... holding x2 equal.
# ... accounting for effects of x2.

summary(m2)$coefficients

plot_model(m2, type="eff", terms=c("weekly_actv", "hydration [1]"))


# more than 2 predictors = more than 3 dimensions
m3 <- lm(brain_vol ~ group + hydration + weekly_actv, data = bdat)

mr<-lm(brain_vol ~ group, data = bdat)
mf<-lm(brain_vol ~ group + hydration + weekly_actv, data = bdat)
anova(mr,mf)


y ~ x1 + z2
y ~ x1 + x2 + x3 + x4 + z2


# person 1
# lego, drinks 3L water a week, does 10 hrs activity a week
coef(m3)
# person 2
# lego, drinks 3L water a week, does 11 hrs activity a week



