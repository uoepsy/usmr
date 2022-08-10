library(tidyverse)
library(googlesheets4)
newsurv <- read_sheet("14FA9d52K5NxyN160_9NU202YO_6pCoYGS4rvGXH8oq0")
prev <- read_csv("~/Desktop/uoepsy/data/surveydata_allcourse22.csv")


newsurv <- newsurv[2:9]
names(newsurv) <- c("pseudonym","birthmonth","ampm","height","eyecolour","catdog","sleeprating","threewords")
newsurv$course = "usmr"
newsurv$year = 2022

updated <- bind_rows(prev,newsurv)
write_csv(updated,"~/Desktop/uoepsy/data/surveydata_allcourse22.csv")
