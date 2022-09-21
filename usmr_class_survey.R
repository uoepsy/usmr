library(tidyverse)
library(googlesheets4)
newsurv <- read_sheet("14FA9d52K5NxyN160_9NU202YO_6pCoYGS4rvGXH8oq0")
prev <- read_csv("C:/Users/jking34/Desktop/uoepsy/data/surveydata_allcourse22.csv")
uprev <- read_csv("https://uoepsy.github.io/data/usmrsurvey2.csv")


ipip <- read_csv("C:/Users/jking34/Desktop/ipip.csv")
ipip$construct = as.numeric(gsub("\\(|\\-|\\+|\\)","",ipip$scoring))
ipip$rev = ifelse(grepl("-",ipip$scoring),1,0)

names(newsurv)[16:65]<-ipip$q
names(newsurv)[66:71]<-paste0("loc",1:6)

newsurv <- newsurv %>% mutate(across(16:71,
                          ~as.numeric(case_when(
                            . == "Very Inaccurate" ~ "1",
                            . == "Moderately Inaccurate" ~ "2",
                            . == "Neither Accurate nor Inaccurate" ~ "3",
                            . == "Moderately Accurate" ~ "4",
                            . == "Very Accurate" ~ "5",
                            TRUE ~ .
                          ))))

newsurv$extraversion <- rowSums(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==1 & ipip$rev==0]]) +  
  (rowSums(apply(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==1 & ipip$rev==1]], 2, function(x) 6-x)))
newsurv$agreeableness <- rowSums(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==2 & ipip$rev==0]]) +  
  (rowSums(apply(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==2 & ipip$rev==1]], 2, function(x) 6-x)))
newsurv$conscientiousness <- rowSums(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==3 & ipip$rev==0]]) +  
  (rowSums(apply(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==3 & ipip$rev==1]], 2, function(x) 6-x)))
newsurv$emot_stability <- rowSums(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==4 & ipip$rev==0]]) +  
  (rowSums(apply(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==4 & ipip$rev==1]], 2, function(x) 6-x)))
newsurv$imagination <- rowSums(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==5 & ipip$rev==0]]) +  
  (rowSums(apply(newsurv[, names(newsurv) %in% ipip$q[ipip$construct==5 & ipip$rev==1]], 2, function(x) 6-x)))

newsurv$loc <- rowSums(newsurv[,"loc2"]) + 
  rowSums(apply(newsurv[,paste0("loc",c(1,3:6))], 2, function(x) 6-x))

newsurv <- newsurv[,-c(16:71)]

names(newsurv)[1:18] <- 
  c("tstamp","pseudonym","birthmonth","ampm","height","eyecolour","catdog","sleeprating","threewords",
    "feelings_stat","feelings_prog","software_stat","course_hopes","course_concerns","rm","spirituality","rm2","clarify")

newsurv$course = "usmr"
newsurv$year = 2022
newsurv$eyecolour <- tolower(newsurv$eyecolour)
newsurv$birthmonth <- sapply(newsurv$birthmonth, function(x) tolower(month.abb[which(x == month.name)]))
names(newsurv)

newsurv <- newsurv[c(2:8,16,9,19:26)]
newsurv$pseudonym[lengths(newsurv$pseudonym) == 0] <- NA
newsurv$pseudonym <- unlist(newsurv$pseudonym, use.names = FALSE)


newsurv %>% select_if(is.numeric) %>%
  psych::pairs.panels()


prev <- prev %>% filter(year!=2022)

updated <- bind_rows(prev, newsurv) %>%
  arrange(desc(year),desc(course)) 
write_csv(updated,"C:/Users/jking34/Desktop/uoepsy/data/surveydata_allcourse22.csv")

#updated <- bind_rows(prev,newsurv) %>% arrange(desc(year),desc(course))
#write_csv(updated,"C:/Users/jking34/Desktop/uoepsy/data/surveydata_allcourse22.csv")
updated %>% filter(year==2022)