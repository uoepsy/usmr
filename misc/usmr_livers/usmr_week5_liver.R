nobelchoc <- read_csv("https://uoepsy.github.io/data/nobelchoc.csv")
library(gghighlight)
ggplot(nobelchoc, aes(x=chocolate, y=laureates10_million))+
  geom_point()+
  gghighlight(label_key = country)+
  theme_minimal()


with(nobelchoc, cor(chocolate, laureates10_million))

handheight <- read_csv('https://uoepsy.github.io/data/handheight.csv')

handheight %>% ggplot(aes(x=height,y=handspan)) +
  geom_point()
