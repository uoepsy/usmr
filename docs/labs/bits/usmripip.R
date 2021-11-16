library(tidyverse)
library(googlesheets4)
read_csv("~/Desktop/test.csv") %>%
  mutate(
    reverse = grepl("-",score),
    dom = gsub("\\+|\\-|\\(|\\)","",score, perl=T)
  ) %>% filter(!is.na(score)) %>%
  transmute(item = varname, reverse = reverse, domain = dom) -> ipip

usmr <- read_sheet("1xe1EeBQdz0rjNN2J-SnXjpbpoDqt6VPszwIwbRQ4l18") %>% janitor::clean_names() %>%
  mutate(id=1:n()) %>% filter(id!=1)

names(usmr) <- gsub("describe_yourself_as_you_generally_are_now_not_as_you_wish_to_be_in_the_future_describe_yourself_as_you_honestly_see_yourself_in_relation_to_other_people_you_know_of_the_same_age_and_sex_as_you_are_responses_are_anonymous_please_indicate_the_accuracy_for_each_statement_in_how_it_describes_you_", "", names(usmr))


usmr %>% select(id, ipip$item) %>% 
  pivot_longer(-id, names_to="item", values_to="resp") %>% 
  mutate(
    resp = recode(resp, 
                  "Very Inaccurate" = 1,
                  "Moderately Inaccurate" = 2,
                  "Neither Accurate nor Inaccurate" = 3,
                  "Moderately Accurate" = 4,
                  "Very Accurate" = 5
                  )
  ) %>% left_join(., ipip) %>%
  mutate(resp = ifelse(reverse==TRUE, 6-resp, resp)) %>%
  group_by(id, domain) %>%
  summarise(
    score = sum(resp)
  ) %>% mutate(
    domain = recode(domain, 
                    `1` = "E",
                    `2` = "A",
                    `3` = "C",
                    `4` = "ES",
                    `5` = "I",
                    `loc` = "LOC"
                    )
  ) -> usmr_ipip


usmr <- usmr %>% select(-ipip$item, -timestamp,-x57)
names(usmr)<-c("catdog","gender","height","pseudonym","optimism","spirituality","ampm","id")

cleandat <- left_join(usmr, usmr_ipip %>% pivot_wider(names_from=domain,values_from=score))
psych::pairs.panels(cleandat)

cleandat %>% 
  #mutate(LOC = 30-LOC) %>% 
  #mutate(across(c(LOC,E,A,ES,C,I), ~scale(.)[,1])) %>%
  print -> cleandat

lm(LOC~.,cleandat %>% select(-id,-pseudonym,-gender)) %>% summary
lm(optimism~.,cleandat %>% select(-id,-pseudonym,-gender)) %>% summary
lm(spirituality~.,cleandat %>% select(-id,-pseudonym,-gender)) %>% summary
glm(I(catdog=="dog") ~ .,cleandat %>% select(-id,-pseudonym,-gender), family=binomial) %>% summary
glm(I(ampm == "Evening person") ~ .,cleandat %>% select(-id,-pseudonym,-gender), family=binomial) %>% summary


lm(LOC~C+E+A+ES+I,cleandat %>% select(-id,-pseudonym,-gender)) %>% summary

lm(optimism~.,cleandat %>% select(-id,-pseudonym,-gender)) %>% summary
ggplot(cleandat,aes(x=LOC,y=C))+
  geom_point()+geom_text(data=cleandat %>% filter(str_detect(pseudonym,"hound")), aes(label=pseudonym))+
  geom_smooth(method="lm")


cleandat %>% rename(
  conscientiousness = C,
  extraversion = E,
  agreeableness = A,
  imagination = I,
  emotional_stability = ES,
  internal_control = LOC
) %>% relocate(id,pseudonym) -> df


#write.csv(df, "~/Desktop/uoepsy/data/usmrsurvey2.csv", row.names=F)
