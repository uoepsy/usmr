# get my data function.. 
get_my_data <- function(group_name){
  tryCatch(
    suppressWarnings(load(url(paste0("https://uoepsy.github.io/usmr/2627/misc/",tolower(group_name),".RData")))),
    
    error=function(e) cat(paste0(c("No Data Found. Are you sure you used the correct group name?\nGroup names should have an underscore between the words. Choose from these:  ", paste0(
      c("analytical_adders","binomial_buzzards","calculating_capercaillies","estimating_eagles","frequency_fulmars","histogram_hares","jitterplot_jays","kurtosis_kites","likelihood_lapwings","outlier_otters","parameter_ptarmigans","quantile_quails","regression_ravens","standard-error_salmon","two-tailed_terns","univariate_unicorns","variance_voles","winsorizing_wildcats"),
      collapse="\n")), "\n\n\nIf you have been given permission to work on this individually, please use your exam number, and if that doesn't work please let us know."))
  )
  
  pilotA <<- pilotA
  pilotB <<- pilotB
  pilotC <<- pilotC
  risset <<- risset
  followup <<- followup
}
