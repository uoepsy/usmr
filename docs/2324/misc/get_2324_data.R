get_my_data <- function(group_name = NULL, individual = FALSE){
  
  # load(url("https://uoepsy.github.io/usmr/2324/misc/pptnames.rdata"))
  load("../docs/2324/misc/pptnames.rdata")
  
  groups_2324 = c("aggregating_anteaters","analytical_aardvarks","calculating_chinchillas","data_dingoes","diverging_dragonflies","evaluating_elephants","gaussian_gannets","histogram_hedgehogs","hypothesizing_hamsters","inferring_iguanas","linear_lemurs","modeling_meerkats","normal_narwhals","numbercrunching_newts","observing_otters","plotting_pumas","quantifying_quokkas","regression_rhinos","sampling_seahorses","trending_turtles","zscore_zebras")
  
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
    stop('\nPLEASE SUPPLY YOUR GROUP NAME.\nIf you are completing the assignment individually, then use your exam number and set individual=TRUE')
  }
  
  if(individual){
    if(is.na(as.numeric(gsub("[^\\d]+", "", group_name, perl=TRUE)))){
      stop("\nPlease supply a valid exam number")
    } else {
      set.seed(as.numeric(gsub("[^\\d]+", "", group_name, perl=TRUE)))
    }
  } else {
    if(!group_name %in% groups_2324){
      stop(paste0("\nPlease supply a valid group name.\n\nIf you are completing the assignment individually, then use your exam number and set individual = TRUE.\n\navailable group names are:\n\n", paste0(paste0('"',groups_2324, '"'),collapse=' , ')))
    } else {
      .groupseed(gname = group_name)
    }
  }
  
  if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  N=232
  
  age = round(runif(N,14,59))
  os = sample(c("android","apple"),N,replace=TRUE)
  freq_emu = rpois(N, lambda = exp(scale(age*-1 + (os=="apple")*1)))
  em_cat = sample(c("loudly crying face","slightly smiling face","thumbs up"),N,replace=TRUE)
  # model.matrix(lm(rnorm(N)~freq_emu + age * em_cat)) |> colnames()
  ERS = model.matrix(lm(rnorm(N)~freq_emu + scale(age) * em_cat)) %*% 
    c(0,1,-1,0,0,4,4) + 
    rnorm(N,0,4)
  
  # model.matrix(lm(rnorm(N)~freq_emu + scale(age) + em_cat)) |> colnames()
  EMI_lp = model.matrix(lm(rnorm(N)~freq_emu + scale(age) + em_cat)) %*% 
    c(-1,-4,3,6,2)
  EMI = rbinom(N,1,prob=plogis(scale(EMI_lp)))
  # EMI = ifelse(EMI == 0, "??","??") make this flipped (so they have to recode)
  
  
  data.frame(
    name = sample(pptnames, N),
    age,opsys=os,freq_emu,em_cat=factor(em_cat),ERS,EMI
  )
}


 