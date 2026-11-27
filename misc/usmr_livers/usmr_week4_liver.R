library(tidyverse)
classdata <- read_csv("https://uoepsy.github.io/data/surveydata_allcourse22.csv") 
classdata <- classdata %>% 
  filter(!is.na(birthmonth)) %>%
  mutate(isWinterBaby=(birthmonth %in%
                         c('nov','dec','jan','feb','mar')))

# suggestions:
### Is preference for dog/cat independent of being a winter/non winter baby?
t = table(classdata$catdog,classdata$isWinterBaby)
rowSums(t) %o% colSums(t) / sum(t)


plot(table(classdata$catdog,classdata$isWinterBaby))
chisq.test(table(classdata$catdog,classdata$isWinterBaby))

### looks a bit complicated for class?
# can we use something from the threewords descriptions?
classdata %>% mutate(
  id = 1:n(),
  words = strsplit(threewords, split=" ")
) %>% unnest(words) %>%
  mutate(
    words = tolower(gsub("[[:punct:] ]+","",words))
  ) %>%
  count(words) %>% arrange(desc(n))


# is cat/dog preference independent of whether people identify as "curious"? 
classdata <- 
  classdata %>% mutate(
    iscurious = grepl("curious",tolower(threewords))
  )
table(classdata$catdog, classdata$iscurious)
chisq.test(classdata$catdog, classdata$iscurious)

#### ----------- ####
#### T TEST ---- ####

do_test <- function(diff=5, size=20) {
  # create sample
  heights = rnorm(size, 165, 12)
  
  # create second sample with 165+diff mean height
  tall.heights = rnorm(size, 165+diff, 12)
  
  t.test(heights,tall.heights,paired = FALSE)$p.value < .05
  
}

sum(replicate(1000,do_test(diff=10,size=25))) / 1000
