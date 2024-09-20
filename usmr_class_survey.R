library(tidyverse)
library(googlesheets4)

ff = read_sheet("1LSOjInPdiFbZR2tmoOSWF7-GZhvRThr2eq2n0fJBXJo")
names(ff)[c(3,5,9,10,14)] <- c("birthmonth","height","eyecolour","catdog","threewords")
names(ff)[c(4,6,7,8,11,12,13)] <- c("ampm","phone_unlocks","caffeine","caffeine_type","sleeprating","procrastination","multitasking")
ff <- ff[,c(3:14)]
ff = ff |> mutate(
  course="usmr",year=2024,
  birthmonth = map(birthmonth, ~which(month.name == .)),
  birthmonth = map_chr(birthmonth, ~ifelse(length(.)==0,NA,unlist(tolower(month.abb[.])))),
  eyecolour = tolower(eyecolour),
  catdog = tolower(catdog)
)
df=read_csv("https://uoepsy.github.io/data/surveydata_historical.csv")

ff = ff[,c(names(df)[names(df) %in% names(ff)], names(ff)[!names(ff) %in% names(df)])]

write_csv(ff, "../../data/usmr2024.csv")
