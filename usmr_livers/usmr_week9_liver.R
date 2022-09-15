library(tidyverse)

usmrsurv2 <- read_csv("https://uoepsy.github.io/data/usmrsurvey2.csv")
names(usmrsurv2)
names(usmrsurv2)[9:14]<-c("E","A","C","ES","I","LOC")

# week 7 & 8:
m78 <- lm(LOC ~ ES + optimism, data = usmrsurv2)
# week 9:
m9 <- lm(LOC ~ ES + optimism + ES:optimism, data = usmrsurv2)
sjPlot::plot_model(m9, type="pred", terms=c("ES","optimism [0,10,20,30,40,50,60,70,80,90,100]"))

# week 10
# binary outcomes
usmrsurv2$isMorning <- factor(usmrsurv2$ampm == "Morning person")
usmrsurv2$isCat <- factor(usmrsurv2$catdog == "cat")
usmrsurv2$gavePseudo <- factor(!is.na(usmrsurv2$pseudonym))

glm(isCat~optimism + E,usmrsurv2,family=binomial)




# Let's pretend (for now) people who prefer cats to dogs are more intelligent. 
set.seed(93)
xmat = tibble(
  iq = rnorm(150)
)
lp = cbind(1,as.matrix(xmat)) %*% c(.5,.1)
xmat %>% mutate(
  isCat = rbinom(150,1,prob=lp),
  iq = 100+(iq*15)
) -> df

glm(isCat ~ iq,df,family=binomial) %>% summary

