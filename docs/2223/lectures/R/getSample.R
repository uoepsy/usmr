set.seed(993)
give_me_a_sample <- function(size = 50){ 
  dat = as.data.frame(MASS::mvrnorm(size, mu=c(90,13), matrix(c(13,7,7,8),nrow=2))) 
  names(dat) = c("brain_vol","weekly_actv") 
  dat %>%  
    mutate( 
      brain_vol = pmax(0,pmin(100,round(brain_vol, 2))),  
      weekly_actv = pmax(0, round(weekly_actv)) 
    ) 
} 