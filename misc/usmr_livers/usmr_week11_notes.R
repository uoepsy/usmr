mallow <- 
  df %>% 
  mutate(
    visibility = factor(visibility),
    taken = factor(taken, levels=c("0","1")),
    agemonths = ifelse(agemonths==0, NA, agemonths),
    ageyears = floor(agemonths/12)
  ) %>%
  filter(!is.na(sweetrating), !is.na(taken), !is.na(agemonths))

mallowtake <- mallow
mallowtime <- mallow %>% filter(!is.na(time))


mm <- glm(taken ~ sweetrating + agemonths*visibility, data = mallowtake, 
          family=binomial)

mallowtake <- mallowtake %>%
  mutate(
    agemonthsC = agemonths - (3*12),
    ageyearsC = ageyears - 3
  )

mm <- glm(taken ~ sweetrating + agemonthsC*visibility, data = mallowtake, 
    family=binomial)

plot(mm,which=4)

summary(mm)
sjPlot::plot_model(mm, type="int")

resmm <- data.frame(
  round(exp(coef(mm)), 2),
  round(exp(confint(mm)), 2)
)
resmm
names(resmm) <- c("est","lw","up")
resmm

# After accounting for children's liking of sweets, older children were more likely to wait for the larger reward when the immediate reward is hidden, with each month of age being associated with decreased odds of taking the small reward (OR: `r resmm[2,1]`, 95% CI [`r resmm[2,2]`,`r resmm[2,3]`]). For 3 year old children, the visibility of the smaller reward (visible vs hidden) was associated with `r round(resmm[3,1])` times the odds of taking it over the delayed, but larger reward  (OR: `r resmm[3,1]`, 95% CI [`r resmm[3,2]`,`r resmm[3,3]`]). A significant interaction between age and visibility was present, with the effects of visibility of small reward reducing as children age (OR: `r resmm[4,1]`, 95% CI [`r resmm[4,2]`,`r resmm[4,3]`]). This relationship is visualised in Figure 1 (see Table 1 for full model results).

```{r echo = FALSE, message = FALSE, fig.cap="Figure 1. - Interaction between age and visibility on probability of taking reward"}
# there are better ways to make Rmarkdown make nice labels "fig 1" etc, but sometimes it's just easier to do it manually
plot_model(mm, type = "int") + 
  scale_x_continuous("Age (months)", breaks=(0:4)*12, labels=(3:7)*12) +
  labs(title="Predicted probability of taking smaller reward",
       y = "p(takes small reward)")
```



## if you want to play with cat*cat (to help with understanding), 
# why not make a continuous variable discrete?  
mallowtake <- mallowtake %>% mutate(above4 = factor(ageyears>4))

mm2 <- glm(taken ~ sweetrating + above4*visibility, data = mallowtake, 
          family=binomial)
summary(mm2)
sjPlot::plot_model(mm2, type="int") + geom_line()
