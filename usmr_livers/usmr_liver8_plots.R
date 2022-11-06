library(tidyverse)
library(plotly)
set.seed(99)
xmat <- tibble(
  group = sample(c("playmo","lego"), 50, replace = TRUE),
  weekly_actv = 15.3 + scale((group=="playmo")*1.1 + rnorm(50, 0, 1))[,1]*3,
  hydration = 4.4 + scale(weekly_actv*.2 + rnorm(50, 0, 1))[,1]*1.6,
  brain_vol = 80.6 + .3*weekly_actv - 2*(group=="lego") + 1*hydration + rnorm(50, 0, 1)
)
bdat <- xmat

plt1_cloud <- plot_ly(bdat, x=~weekly_actv, y=~hydration, marker=list(colorscale='Plasma',color=~weekly_actv),z=~brain_vol, type="scatter3d",mode="markers") 


steps=100
ll <- with(bdat, seq(min(hydration),max(hydration),length=steps))
ye <- with(bdat, seq(min(weekly_actv),max(weekly_actv),length=steps))

mod2 <- lm(brain_vol ~ weekly_actv + hydration, data = bdat)
acr <- matrix(predict(mod2, expand.grid(hydration = ll, weekly_actv = ye)), steps, steps)

plt2_surfcloud <- plot_ly(bdat, 
                          x=~weekly_actv, y=~hydration, z=~brain_vol, 
                          type="scatter3d",mode="markers") %>%
  add_trace(x = ye, y = ll, z = acr, 
            type="surface",colorscale="Viridis")

mod2a <- lm(brain_vol ~ weekly_actv, data = bdat)
acr <- matrix(predict(mod2a, expand.grid(hydration = ll, weekly_actv = ye)), steps, steps)

plt2a_surfcloud <- plot_ly(bdat, 
                          x=~weekly_actv, y=~hydration, z=~brain_vol, 
                          type="scatter3d",mode="markers") %>%
  add_trace(x = ye, y = ll, z = acr, 
            type="surface",colorscale="Viridis")



rm(list=ls()[!grepl("plt|bdat",ls())])
