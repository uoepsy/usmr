get_my_data <- function(exam_num = NULL){
  if(is.null(exam_num) | is.na(as.numeric(gsub("[^\\d]+", "", exam_num, perl=TRUE)))){
    print("PLEASE SUPPLY YOUR EXAM NUMBER")
  } else {
    set.seed(as.numeric(gsub("[^\\d]+", "", exam_num, perl=TRUE)))
    if(!require(tidyverse)){
      install.packages("tidyverse")
      library(tidyverse)
    }
  
    N = 200
    
    Xmat = tibble(
      workingstyle = sample(c("office","hybrid","remote"), N, replace = TRUE),
      project = sample(c("team","solo"), N, replace = TRUE),
      age = (project=="solo")*5 + round(runif(N, 24, 69)),
      years_in_role = pmax(0,round(-8 + (age)*.3+ rchisq(N,4)))
    ) %>%
      mutate(
        years_in_role = case_when(
          age-18 < years_in_role ~ age-18,
          TRUE ~ years_in_role
        )
      ) 
  
    mm = model.matrix(lm(rnorm(N)~age + years_in_role + workingstyle*project, Xmat))
    #dimnames(mm)[[2]]
    y = mm %*% c(0,.2,0,-3.5,-1,2,0,-2.5) + rnorm(N,0,2)
    
    mm = model.matrix(lm(rnorm(N)~years_in_role + workingstyle + project, Xmat))
    # dimnames(mm)[[2]]
    ybinl = mm %*% c(0,.7,-1.4,-1,-1.5)
    
    df = bind_cols(Xmat, jobsat=y[,1],manager_support = rbinom(N,1,plogis(scale(ybinl)[,1])))
    
    
    df$workingstyle[sample(which(df$workingstyle=="remote"),sample(1:5,1))] <- "rmote"
    df$age[sample(1:N,5)] <- NA
    df$project[sample(which(df$project=="team"),sample(0:20,1))] <- "Team"
    df$project[sample(which(df$project=="team"),2)] <- c("i work in a team", "i don't know")
    df$jobsat <- 45.245 +scale(df$jobsat)[,1]*12.753
    df$jobsat <- pmin(100, pmax(0, df$jobsat))
    df$jobsat <- round(df$jobsat, 2)
    pids = map_dbl(1:1e5, ~round(rnorm(1, 2e6, 1e3)))
    df$employeeid = sample(unique(pids),N)
    
    hrdata <<- df %>% select(employeeid, workingstyle, project, years_in_role, age)
    survdata <<- df %>% select(employeeid, manager_support, jobsat) %>% sample_n(n())
  
  }
}



