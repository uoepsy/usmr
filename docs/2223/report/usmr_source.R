get_my_data <- function(exam_num = NULL){
  if(is.null(exam_num) | is.na(as.numeric(gsub("[^\\d]+", "", exam_num, perl=TRUE)))){
    print("PLEASE SUPPLY YOUR EXAM NUMBER")
  } else {
    set.seed(exam_num)
    require(tidyverse)
    N = 120
    pairdf <- tibble(
      age = rdunif(N,3, 40),
      born = sample(c("wild","captv"),N,TRUE),
      species = rep(c("macaque","bonobo","capuchin"), e=N/3),
      dominance = round(scale(.6*(born=="wild") + rnorm(N))[,1],3),
      confidence = round(scale(1.7*(species=="bonobo")+rnorm(N))[,1],3)
    )
    coefs = c(0, -4, 0, 4, -2, 2, -.6, .4)
    
    y = model.matrix(lm(rnorm(N) ~ confidence + dominance + born + species + age + born:age, pairdf)) %*% coefs + 
      rnorm(N,0,6)
    y = rnorm(1, 12, 1) + (scale(y)[,1]*rnorm(1,4,.1))
    y = round(y, 1)
    pairdf$time_to_food = pmax(1,y)
    
    coefs2 = c(0, 9, -5, -3, 1, 3, 15)
    pp = model.matrix(lm(rnorm(N) ~ confidence + dominance + born + species + age, 
                         pairdf %>% mutate_if(is.numeric,~scale(.)[,1]))) %*% coefs2
    pairdf$share = rbinom(N, 1, prob = plogis(scale(pp)))
    
    names <- read_csv("https://uoepsy.github.io/data/names_scientists.csv")
    pairdf$pid = sample(names$name, nrow(pairdf))
    
    taskdata <<- pairdf %>% select(pid, species, age, time_to_food, share)
    monkeydata <<- pairdf %>% select(pid, species, born, dominance, confidence) %>% sample_n(n())
  }
}
