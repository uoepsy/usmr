library(tidyverse)
library(sjPlot)
# plot_model(model, type="pred") or type ="int" (for interactions)
# also tab_model()


# okay. forget about the fact that last week we had this as a "population", and then sampled 20 from it. 
# in real life, we only have a sample, and we make inferences from that to the population
usmrsurv2 <- read_csv("https://uoepsy.github.io/data/usmrsurvey2.csv")

names(usmrsurv2)
names(usmrsurv2)[9:14]<-c("E","A","C","ES","I","LOC")

# # categorical predictors:
# m1 <- lm(optimism ~ catdog, data=usmrsurv2)
# summary(m1)
# plot_model(m1, type = "pred")

# influence etc
m2 <- lm(LOC ~ ES, data=usmrsurv2)
summary(m2)
plot(m2)
summary(influence.measures(m2))
ggplot(usmrsurv2, aes(x=ES, y=LOC))+
  geom_point()+
  geom_smooth(method=lm)+
#  geom_point(data=usmrsurv2[4,],col="red")+
  geom_point(data=usmrsurv2 %>% filter(pseudonym=='Martin'),col='green')

influence.measures(m2)

# multiple reg
m3 <- lm(LOC ~ ES, data=usmrsurv2)
summary(m3)


# interactions
m4 <- lm(LOC ~ ES + optimism + ES:optimism, data=usmrsurv2)
plot_model(m4, type="pred", terms=c("ES","optimism [30,60,90]"))
summary(m4)

m5 <- lm(LOC ~ ES*ampm, data=usmrsurv2)
plot_model(m5, type="int")
m6 <- lm(LOC ~ ampm*catdog, data=usmrsurv2)
plot_model(m6, type="int")



usrmsurv2 <- usmrsurv2 %>% rename(pitchfluc = optimism, city = gender)
tibble(
  city = rep(letters[1:3], each=10),
  age = rnorm(30),
  pitchf = c(rnorm(10,.3,.3),rnorm(10,.1,.3),rnorm(10,.6,.3))
) %>%
  ggplot(.,aes(x=age,y=pitchf,col=city))+
  geom_point()+
  geom_smooth(method=lm,se=F)
  



