get_my_data <- function(group_name = NULL, individual = FALSE){
  
  load(url("https://uoepsy.github.io/usmr/2324/misc/pptnames.rdata"))
  
  groups_2425 = c("asymptotic_arias","bivariate_buglers","chisquare_crooners","data_dancers","error_earworms","f_statistic_funkadelics","gaussian_groovers","histogram_harmonizers","inference_improvisers","jitterplot_jammers","kurtosis_kazoos","logistic_lyricalists","mean_medley_makers","normality_notes","outlier_orchestra","p_value_percussionists","quantile_quartet","standard_deviation_serenaders","t_test_troubadours","y_hat_yodelers")
  
  .groupseed <- function(gname){
    gseed = gsub("[\\d]+", "", gname, perl=TRUE) |> 
      strsplit(split="") |> 
      unlist() |> 
      sapply(X=_, function(c) which(letters==c)) |>
      unlist() |> sum()
    #cat(gseed^3)
    set.seed(gseed^3)
  }
  
  if(is.null(group_name)){
    stop(paste0("\nPlease supply a valid group name.\n\nIf you are completing the assignment individually, then use your exam number and set individual = TRUE.\n\navailable group names are:\n\n", paste0(paste0('"',groups_2324, '"'),collapse=' , ')))
  }
  
  if(individual){
    if(is.na(as.numeric(gsub("[^\\d]+", "", group_name, perl=TRUE)))){
      stop("\nPlease supply a valid exam number")
    } else {
      set.seed(as.numeric(gsub("[^\\d]+", "", group_name, perl=TRUE)))
    }
  } else {
    if(!group_name %in% groups_2425){
      stop(paste0("\nPlease supply a valid group name.\n\nIf you are completing the assignment individually, then use your exam number and set individual = TRUE.\n\navailable group names are:\n\n", paste0(paste0('"',groups_2425, '"'),collapse=' , ')))
    } else {
      .groupseed(gname = group_name)
    }
  }
  
  if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  init <- tibble(
    instrument = rep(c(letters,paste0(letters,letters))[1:40],e=10),
    family = rep(c("percussion","woodwind","brass","strings"), e=100),
    bpm = unlist(lapply(c(130,95,120,80), \(x) round(rnorm(100,x,20)/5)*5)),
    listenerage = round(runif(400,18,80)),
    listenermusician = rbinom(400,1,prob=plogis( -1*(family=="percussion") ))
  )
  xmat <- model.matrix(rnorm(400) ~ listenermusician + listenerage + scale(bpm) * family,
                       data = init)
  dimnames(xmat)[[2]]
  bs <- c(3, 2, 0, -.7, -6, 3, 3, -.5, .6, 0)
  
  df <- data.frame(
    init, EVS = xmat %*% bs + rnorm(400,0,2)
  )
  xmatb <- model.matrix(rnorm(400) ~ scale(EVS) + listenermusician + scale(listenerage) + scale(bpm) + family, data = df)
  dimnames(xmatb)[[2]]
  bbs <- c(0,2,2,0,0,-6,-1,-2)
  
  df$enjoyed = rbinom(400, 1, plogis( xmatb %*% bbs))
  #df <- df |> mutate(across(c(family,listenermusician,enjoyed), factor))
  df$pptname = sample(pptnames,size=nrow(df))
  orchestra <<- df |> 
    transmute(
      pptname, age=listenerage, musician=listenermusician, instrument,
      bpm, EVS,enjoyed)
}


 