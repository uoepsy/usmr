library(tidyverse)
library(googlesheets4)

ff = read_sheet("1LSOjInPdiFbZR2tmoOSWF7-GZhvRThr2eq2n0fJBXJo")
names(ff)[c(3,5,9,10,14)] <- c("birthmonth","height","eyecolour","catdog","threewords")
names(ff)[c(4,6,7,8,11,12,13)] <- c("ampm","phone_unlocks","caffeine","caffeine_type","sleeprating","procrastination","multitasking")

ff = ff[substr(ff$Timestamp,1,4)=="2026",]

ff$procrastination = as.numeric(unlist(ff$procrastination))

ff = ff |> mutate(
  course="usmr",year=2026,
  birthmonth = map(birthmonth, ~which(month.name == .)),
  birthmonth = map_chr(birthmonth, ~ifelse(length(.)==0,NA,unlist(tolower(month.abb[.])))),
  eyecolour = tolower(eyecolour),
  catdog = tolower(catdog),
  pseudonym = `OPTIONAL: As we will be using this data during our work in the course, you may want to be able to find yourself in the dataset. If you choose to do this, we suggest that you provide some form of pseudonym to preserve anonymity.`
)


df=read_csv("https://uoepsy.github.io/data/usmr26survey_historical.csv")

ff = ff[,c(names(df)[names(df) %in% names(ff)],"pseudonym")]

write_csv(ff, file="../../data/usmr2026.csv")
