source("R/02 estimate/GenerateSamples.R")
source("R/02 estimate/Data.R")
source("R/02 estimate/PrepareData.R")

G_DESC <- "TEST"


config <- list(G_DATA        = "data/SEIRH_Beta.xlsx",
               G_MODEL       = "models/SEIRH_Beta.stmx",
               G_STAN_MODEL  = "models/stan/SEIRH.stan",
               EPOCHS        = c("Epoch1","Epoch2","Epoch3"),
               MEAS_MODELS   = list(c("Cases","Hospitalisations","Deaths"),
                                    c("Cases","Hospitalisations"),
                                    c("Cases","Deaths"),
                                    c("Hospitalisations","Deaths"),
                                    c("Cases"),
                                    c("Hospitalisations"),
                                    c("Deaths")),
               DESC          = G_DESC)


config <- list(G_DATA        = "data/SEIRH_Beta.xlsx",
               G_MODEL       = "models/SEIRH_Beta.stmx",
               G_STAN_MODEL  = "models/stan/SEIRH.stan",
               EPOCHS        = c("Epoch1","Epoch2","Epoch3"),
               MEAS_MODELS   = list(c("Cases","Hospitalisations","Deaths")),
               DESC          = G_DESC)

