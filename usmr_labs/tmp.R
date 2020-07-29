params <-
list(SHOW_SOLS = TRUE, TOGGLE = FALSE)

## ----eval=FALSE-----------------------------------------------------------------------------
## # show the dimensions of the data
## dim(somedata)
## 
## table(somedata$somevariable)


## ----eval=FALSE-----------------------------------------------------------------------------
## barplot(table(somedata$somevariable))


## -------------------------------------------------------------------------------------------
# Don't worry about what all these functions do, 
# it's just an example -
round(mean(log(cumsum(diff(1:10)))))


## -------------------------------------------------------------------------------------------
library(tidyverse)
starwars2 <- read_csv("https://uoepsy.github.io/data/starwars2.csv")

starwars2 %>%
    summary()


## ----eval=FALSE-----------------------------------------------------------------------------
## barplot(table(starwars2$species))


## -------------------------------------------------------------------------------------------
starwars2$species %>%
    table() %>%
    barplot()


## ----eval=FALSE-----------------------------------------------------------------------------
## # again, don't worry about all these functions,
## # just notice the difference in the two styles.
## round(mean(log(cumsum(diff(1:10)))))


## ----eval=FALSE-----------------------------------------------------------------------------
## 1:10 %>%
##     diff() %>%
##     cumsum() %>%
##     log() %>%
##     mean() %>%
##     round()


## ----eval=FALSE-----------------------------------------------------------------------------
## # take the data
## # %>%
## # mutate it, such that there is a variable called "newvariable", which
## # has the values of a variable called "oldvariable" multiplied by two.
## data %>%
##   mutate(
##     newvariable = oldvariable * 2
##   )


## -------------------------------------------------------------------------------------------
data.frame(
  x = rnorm(100)
) %>%
  mutate(
    y = x^2
  )


## -------------------------------------------------------------------------------------------
wechsler <- read_csv("https://uoepsy.github.io/data/wechsler.csv")
summary(wechsler)


## -------------------------------------------------------------------------------------------
# take the "wechsler" dataframe %>%
# count() the values in the "iq" variable (creates an "n" column), and
# from there, arrange() the data so that the "n" column is descending - desc()
wechsler %>%
    count(iq) %>%
    arrange(desc(n))


## -------------------------------------------------------------------------------------------
median(wechsler$iq)


## -------------------------------------------------------------------------------------------
# get the values in the "iq" variable from the "wechsler" dataframe, and
# sum them all together. Then divide this by 120
sum(wechsler$iq)/120


## -------------------------------------------------------------------------------------------
mean(wechsler$iq)


## ----eval=FALSE-----------------------------------------------------------------------------
## # take the data %>%
## # summarise() it, such that there is a value called "summary_value", which
## # is the sum() of "variable1" column, and a value called
## # "summary_value2" which is the mean() of the "variable2" column.
## data %>%
##   summarise(
##     summary_value = sum(variable1),
##     summary_value2 = mean(variable2)
##   )


## -------------------------------------------------------------------------------------------
# take the "wechsler" dataframe %>%
# summarise() it, such that there is a value called "mean_iq", which
# is the mean() of the "iq" variable, and a value called 
# "mean_age" which is the mean() of the "age" variable. 
wechsler %>%
    summarise(
        mean_iq = mean(iq),
        mean_age = mean(age)
    )


## -------------------------------------------------------------------------------------------
IQR(wechsler$age)


## -------------------------------------------------------------------------------------------
# take the "wechsler" dataframe %>%
# summarise() it, such that there is a value called "median_age", which
# is the median() of the "age" variable, and a value called "iqr_age", which
# is the IQR() of the "age" variable.
wechsler %>% 
  summarise(
    median_age = median(age),
    iqr_age = IQR(age)
  )


## -------------------------------------------------------------------------------------------
wechsler %>%
  summarise(
    variance_iq = var(iq)
  )


## -------------------------------------------------------------------------------------------
wechsler %>%
  summarise(
    variance_iq = var(iq),
    sd_iq = sd(iq)
  )


## -------------------------------------------------------------------------------------------
# Notice, we put age on the x axis, making the box plot vertical. 
# If we had set aes(y = age) instead, then it would simply be rotated 90 degrees 
ggplot(data = wechsler, aes(x = age)) +
  geom_boxplot()


## -------------------------------------------------------------------------------------------
# make a ggplot with the "wechsler" data. 
# on the x axis put the possible values in the "iq" variable,
# add a histogram geom (will add bars representing the count 
# in each bin of the variable on the x-axis)
ggplot(data = wechsler, aes(x = iq)) + 
  geom_histogram()


## -------------------------------------------------------------------------------------------
ggplot(data = wechsler, aes(x = iq)) + 
  geom_histogram(binwidth = 5)


## -------------------------------------------------------------------------------------------
wechsler %>% 
  summarise(
    mean_test1 = mean(test1),
    sd_test1 = sd(test1),
    mean_test2 = mean(test2),
    sd_test2 = sd(test2)
  )


## -------------------------------------------------------------------------------------------
ggplot(data = wechsler, aes(x = iq)) + 
  geom_density()+
  xlim(50,150)


## -------------------------------------------------------------------------------------------
stroopdata %>%
  summarise(
    min_time = min(matching),
    max_time = max(matching),
    mean_time = mean(matching),
    median_time = median(matching)
  )


## -------------------------------------------------------------------------------------------
stroopdata %>%
  summarise(
    min_time = min(mismatching),
    max_time = max(mismatching),
    mean_time = mean(mismatching),
    median_time = median(mismatching)
  )


## ----eval=FALSE-----------------------------------------------------------------------------
## stroopdata <-
##   stroopdata %>%
##   mutate(
##     ?? = ??
##   )


## -------------------------------------------------------------------------------------------
stroopdata <- 
  stroopdata %>%
  mutate(
    stroop_effect = mismatching - matching
  )

# and print it out:
stroopdata


## -------------------------------------------------------------------------------------------
ggplot(data = stroopdata, aes(x = stroop_effect)) +
  geom_histogram()


## ----out.width="50%"------------------------------------------------------------------------
ggplot(data = stroopdata, aes(x = stroop_effect)) +
  geom_histogram(binwidth = 2)


## -------------------------------------------------------------------------------------------
ggplot(data = stroopdata, aes(x = stroop_effect)) +
  geom_density()


## ----eval=FALSE-----------------------------------------------------------------------------
## ggplot(data = stroopdata, aes(x = stroop_effect)) +
##   geom_histogram() +
##   geom_vline(xintercept = 0)


## -------------------------------------------------------------------------------------------
stroopdata %>% 
  summarise(
    mean_diff = mean(stroop_effect)
  )


## -------------------------------------------------------------------------------------------
ggplot(data = stroopdata, aes(x = stroop_effect)) +
  geom_histogram() +
  geom_vline(xintercept = 7.9)


## -------------------------------------------------------------------------------------------
stroopdata %>%
  summarise(
    min_age = min(age),
    max_age = max(age),
    median_age = median(age),
    iqr_age = IQR(age)
  )


## -------------------------------------------------------------------------------------------
stroopdata %>%
  summarise(
    min_height = min(height),
    max_height = max(height),
    mean_height = mean(height),
    sd_height = sd(height)
  )


## -------------------------------------------------------------------------------------------
stroopdata %>%
  count(practice)


## -------------------------------------------------------------------------------------------
# In the stroopdata dataframe, give me all the rows for which the
# condition stroopdata$practice=="no" is TRUE, and give me all the columns.
# assign this as an object with the name "no_practice"
no_practice <- stroopdata[stroopdata$practice == "no", ]

#and the same but for practice == "yes"
practice <- stroopdata[stroopdata$practice == "yes", ]



## ----eval=FALSE-----------------------------------------------------------------------------
## # take the data
## # and filter it to only the rows where the "variable1" column is
## # equal to "value1".
## data %>%
##   filter(variable1 == value1)


## ----eval=FALSE-----------------------------------------------------------------------------
## # take the data
## # and select the "variable1" and "variable2" columns
## data %>%
##   select(variable1, variable2)


## -------------------------------------------------------------------------------------------
no_practice <- stroopdata %>% filter(practice == "no")

practice <- stroopdata %>% filter(practice == "yes")

participants <- stroopdata %>% filter(age >= 40) %>% select(id, age, height)

