get_my_data <- function(group_name){
  tryCatch(
    suppressWarnings(load(url(paste0("https://uoepsy.github.io/usmr/2526/misc/",tolower(group_name),".RData")))),
    
    error=function(e) cat(paste0(c("No Data Found. Are you sure you used the correct group name?\nGroup names should have an underscore between the words. Choose from these:  ", paste0(c("binomial_bears","calculating_chinchillas","data_dingoes","estimating_elephants","frequency_foxes","gaussian_gannets","histogram_hedgehogs","inferring_iguanas","jitterplot_jaguars","kurtosis_kangaroos","modellling_meerkats","normal_narwhals","outlier_owls","regression_rhinos","sampling_seahorses","t_test_turtles","univariate_umbrella_bird","variability_voles","winsorizing_wolves","y_hat_yaks","z_score_zebras"),collapse="\n")), 
           "\n\n\nIf you cannot make this work with your exam number, please use get_my_data(\"UNKNOWN\")"))
  )
  
    pilotA <<- pilotA
    pilotB <<- pilotB
    pilotC <<- pilotC
    nudges <<- nudges
    followup <<- followup
}


