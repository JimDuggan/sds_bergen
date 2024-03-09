library(purrr)
library(glue)
library(lubridate)

source("R/02 estimate/Header.R")
get_stamp <- function(sep=" ")
{
  lubridate::ymd_hms(Sys.time()) %>%
    str_replace("UTC","") %>%
    str_trim() %>%
    str_replace(" ",sep)
}

# Get the data
epi <- get_data(config$G_DATA)

# Configure factorial design
exp <- configure_exp(config$EPOCHS,
                     config$MEAS_MODELS)

# Prepare readsdr stan measure models
exp <- exp %>%
        mutate(FitData=map(Epoch,~filter_data(epi,.x)),
               MM_readsdr=map(Indicators,~get_meas_model(.x)))

config$RUN_INFO <- vector(mode="list",length = nrow(exp))


# Run stan fits for each experiment, store in new column
exp <- exp %>%
        mutate(StanFit=pmap(list(ExpNumber,IndCode,Indicators,FitData,MM_readsdr),~{
                           t1 <- Sys.time()
                           start_time <- get_stamp()
                           f <- fit_model(XMILE_FILE = config$G_MODEL,
                                          STAN_FILE  = str_replace(config$G_STAN_MODEL,"SEIRH.stan",paste0("SEIRH_",..2,".stan")),
                                          Exper      = ..1,
                                          Indicators = ..3,
                                          data       = ..4,
                                          meas_model = ..5)
                           diagnostic      <- f$cmdstan_diagnose()
                           diagnostic_summ <- f$diagnostic_summary()
                           finish_time <- get_stamp()
                           config$RUN_INFO[[..1]] <<- list(ExpNo=..1,
                                                           Indicators=..2,
                                                           Obs=nrow(..3),
                                                           Start_Time=start_time,
                                                           Finish_Time=finish_time,
                                                           Duration=Sys.time()-t1,
                                                           Diagnostic=diagnostic,
                                                           Diagnostic_Summary=diagnostic_summ)
                           f
        }))


# Save in RDS file
STAMP <- get_stamp("#")

fits <- prepare_data1(exp,config)

rds_file  <- glue("data/estimates/{config$DESC}_{STAMP}_EP{length(config$EPOCHS)}_MM{length(config$MEAS_MODELS)}_FITS.rds")


saveRDS(fits,rds_file)




