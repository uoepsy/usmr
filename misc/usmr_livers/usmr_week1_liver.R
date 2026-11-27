
### 
# in the console
###

# basic calculator
4+5
5-32
4*5

# environment
x<-3
x
y<-3*x
x<-5
x
y

## scripts! 
# v important, because can keep track of sequential lines of code to get us from start to finish


movies <- read.csv("https://uoepsy.github.io/data/hollywoodsample2.csv")

# indexing
movies[34,]
movies$RottenTomatoes
movies$RottenTomatoes[34]

movies$Genre=="Comedy"
movies$RottenTomatoes[movies$Genre=="Comedy"]

mean(movies$RottenTomatoes[movies$Genre=="Comedy"])

mean(movies$RottenTomatoes[movies$Genre=="Drama"])
