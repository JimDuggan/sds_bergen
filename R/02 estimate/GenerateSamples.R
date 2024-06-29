library(cmdstanr)
library(dplyr)
library(posterior)
library(readsdr)
library(tidyr)
library(stringr)
library(readr)
library(openxlsx)
library(lubridate)
library(glue)
library(purrr)
library(readsdr)

#---------------------------------------------------------------------------------
get_meas_model <- function(target){
  indicators <- c("Cases","Hospitalisations","Deaths")
  mods <- c("C ~ neg_binomial_2(net_flow(TC), phi1)",
            "H ~ neg_binomial_2(net_flow(TH), phi2)",
            "D ~ neg_binomial_2(net_flow(TD), phi3)")
  
  l_out <- vector(mode="list",length=length(target))
  for(i in seq_along(target)){
    l_out[[i]] <- mods[which(indicators==target[i])]
  }
  l_out
}

#---------------------------------------------------------------------------------
get_priors <- function(){
  priors <-    list(sd_prior(par_name  = "CF", 
                                 dist      = "beta", 
                                 dist_pars = c(2, 2)),
                    sd_prior(par_name  = "HF", 
                             dist      = "beta", 
                             dist_pars = c(2, 2)),
                    sd_prior(par_name  = "DF", 
                             dist      = "beta", 
                             dist_pars = c(2, 2)),
                    sd_prior(par_name = "Beta_Param", 
                                 dist     = "lognormal", 
                                 dist_pars = c(0,1)))
  priors
}

#---------------------------------------------------------------------------------
get_stan_d <- function(indicators, stocks, data){
  stan_d <- list(n_obs      = nrow(data),
                 x0         = stocks,
                 C          = data$Cases,
                 H          = data$Hospitalisations,
                 D          = data$Deaths,
                 t0         = 0,
                 ts         = 1:nrow(data))
  if(!("Cases" %in% indicators)) stan_d$C <- NULL
  if(!("Hospitalisations" %in% indicators)) stan_d$H <- NULL
  if(!("Deaths" %in% indicators)) stan_d$D <- NULL
  
  stan_d
}

#---------------------------------------------------------------------------------
fit_model <- function(XMILE_FILE,
                      STAN_FILE,
                      Exper,
                      Indicators,
                      meas_model,
                      data,
                      chains = 4,
                      parallel_chains = 4,
                      iter_warmup = 1000,
                      iter_sampling = 1000,
                      refresh         = 100,
                      save_warmup     = FALSE,
                      NEW_STAN_FILE = TRUE)
  {
  
  start_time <- lubridate::as_datetime(Sys.time())
  
  print(glue("Reading XMILE file {XMILE_FILE}..."))
  mdl        <- read_xmile(XMILE_FILE)
  
  
  # Check the stocks
  sd_stocks(mdl)
  
  # Check the auxiliaries
  sd_constants(mdl) |> 
    mutate(value = format(value, scientific = FALSE))
  
  # Get the target parameters to be fitted
  print(glue("Getting targets..."))
  priors <- get_priors()
  
  print(map_chr(priors,"par_name"))

  # Create the stan file from XMILE and params
  stan_file <- sd_Bayes(filepath         = XMILE_FILE,
                        meas_mdl         = meas_model,
                        estimated_params = priors)
  
  # Write the stan file
  if(NEW_STAN_FILE == TRUE){
     print(glue("Writing stan file to {STAN_FILE}..."))
     write_file(stan_file, file = STAN_FILE)
  }
  
  # Create the stan data object
  stan_d <- get_stan_d(Indicators,
                       sd_stocks(mdl)$init_value,
                       data)
  # Create model object
  mod           <- cmdstan_model(STAN_FILE)
  
  print(glue("Calling sampler at time {lubridate::as_datetime(Sys.time())}..."))
  # Generate fit
  fit <- mod$sample(data              = stan_d,
                    chains            = chains,
                    parallel_chains   = parallel_chains,
                    iter_warmup       = iter_warmup,
                    iter_sampling     = iter_sampling,
                    refresh           = refresh,
                    save_warmup       = save_warmup)
  
  print(glue("Sampling completed at time {lubridate::as_datetime(Sys.time())}..."))
  
  
  fit
  
}


