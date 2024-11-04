get_my_data <- function(group_name = NULL, individual = FALSE){
  
  # load(url("https://uoepsy.github.io/usmr/2324/misc/pptnames.rdata"))
  load("~/Desktop/uoepsy/usmr/docs/2324/misc/pptnames.rdata")
  infl = c("Sigmund Freud","Beatrix Potter","Stephen Jay Gould")
  pptnames = pptnames[!pptnames %in% intersect(pptnames, infl)]
  
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
    family = sample(c("percussion","woodwind","brass","strings"),400,T,prob=c(.1,.3,.3,.3)),
    bpm = unlist(lapply(c(130,95,120,80), \(x) round(rnorm(100,x,20)/5)*5)),
    listenerage = round(runif(400,18,80)),
    listenermusician = rbinom(400,1,prob=plogis( -1*(family=="percussion") ))
  )
  xmat <- model.matrix(rnorm(400) ~ listenermusician + listenerage + scale(bpm) * family,
                       data = init)
  dimnames(xmat)[[2]]
  bs <- c(3, 2, 0, -.7, -6, 3, 3, -.8, .7, 0)
  
  df <- data.frame(
    init, ERS = xmat %*% bs + rnorm(400,0,2)
  )
  xmatb <- model.matrix(rnorm(400) ~ scale(ERS) + listenermusician + scale(listenerage) + scale(bpm) + family, data = df)
  dimnames(xmatb)[[2]]
  bbs <- c(0,2,2,0,0,-6,-1,-2)
  
  df$enjoyed = rbinom(400, 1, plogis( xmatb %*% bbs))
  #df <- df |> mutate(across(c(family,listenermusician,enjoyed), factor))
  df$pptname = sample(pptnames,size=nrow(df))
  
  df$instrument <- NA
  df$instrument[df$family=="woodwind"] <-
    sample(c("Flute","Oboe","Clarinet","Bassoon","Piccolo"), 
           table(df$family)['woodwind'], T)
  df$instrument[df$family=="strings"] <-
    sample(c("Violin","Viola","Cello","Double Bass"), 
           table(df$family)['strings'], T)
  df$instrument[df$family=="percussion"] <-
    sample(c("Timpani","Snare Drum","Bass Drum","Cymbals"), 
           table(df$family)['percussion'], T, prob=c(.3,.3,.3,.1))
  df$instrument[df$family=="brass"] <-
    sample(c("Trumpet","French Horn","Trombone","Tuba","Euphonium"), 
           table(df$family)['brass'], T)
  
  df$instrument[sample(1:400,1)] <- "Theramin"
  df$listenerage[sample(1:400,4)] <- -99
  df$instrument[df$instrument=="Piccolo"] <- 
    sample(c("Piccollo","Piccolo"), length(df$instrument[df$instrument=="Piccolo"]), T, 
           prob=c(.1,.9))
  df$instrument[df$instrument=="French Horn"] <- 
    sample(c("French Hron","French Horn"), length(df$instrument[df$instrument=="French Horn"]), T, 
           prob=c(.1,.9))
  df$listenermusician[1] <- "i don't know"

  inf2 <- tibble(
    pptname = infl,
    listenerage = round(runif(3,40,90)),
    listenermusician = c("0","1","0"),
    instrument = c("Cello","Timpani","Clarinet"),
    bpm = c(40,185,50),
    ERS = c(18.455, 15.341, -1.903),
    enjoyed = c(0,1,1)
  )
  
  df <- bind_rows(df,inf2)
  df <- sample_n(df, nrow(df))
  rownames(df) <- 1:nrow(df)
  
  orchestra <<- df |> 
    transmute(
      pptname, age=listenerage, 
      musician=ifelse(listenermusician=="0","non-musician",
                      ifelse(listenermusician=="1","musician",listenermusician)), 
      instrument,
      bpm, ERS = round(ERS,2), 
      enjoyed = ifelse(enjoyed==1,"enjoyed","not_enjoyed"))
}





 