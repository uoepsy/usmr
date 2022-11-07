library(tidyverse)
library(plotly)
source("C:/Users/jking34/Desktop/uoepsy/usmr/usmr_livers/usmr_liver8_plots.R")

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
plot_model(m2, type="eff", terms=c("weekly_actv","group"), show.data = TRUE)
# Two lines is actually a model in 3 dimensions (although it might not feel like it). 


# Easier to see this with 2 continuous predictors:  
ggplot(bdat, aes(x=weekly_actv, y=brain_vol, col=hydration)) +
  geom_point()

# or 3d:
plt1_cloud

# our initial model, m1:
plt2a_surfcloud

# our 
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
