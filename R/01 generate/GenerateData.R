library(cmdstanr)
library(dplyr)
library(ggplot2)
library(posterior)
library(readr)
library(readsdr)
library(tidyr)
library(openxlsx)

# Load the Stella file containing the model.
model_path <- "models/SEIRH.stmx"
mdl        <- read_xmile(model_path)

# Check the stocks
sd_stocks(mdl)

# Check the auxiliaries
sd_constants(mdl) |> 
  mutate(value = format(value, scientific = FALSE))

# Specify synthetic data model
meas_mdl <- list("cases ~ neg_binomial_2(net_flow(TC), 10)",
                 "hosp ~ neg_binomial_2(net_flow(TH), 20)",
                 "deaths ~ neg_binomial_2(net_flow(TD), 40)")

# Generate synthetic data based on SD model
syn <- sd_measurements(n_meas       = 1,
                            ds_inputs    = mdl$deSolve_components,
                            meas_model   = meas_mdl,
                            start_time   = 0,
                            stop_time    = 100,
                            timestep     = 1/8,
                            integ_method = "euler") %>%
       as_tibble()

# Plot the synthetic data
ggplot(syn, aes(time, measurement,colour=var_name)) +
  geom_point(colour = "#7B92DC") +geom_line()+
  facet_wrap(~var_name,scales = "free",ncol=1)+
  labs(x = "Day", y = "People per day") +
  theme_classic()

wide_syn <- syn %>%
             select(-iter) %>%
             pivot_wider(names_from = "var_name",
                         values_from = "measurement") %>%
             rename(Time=time,
                    Cases=cases,
                    Hospitalisations=hosp,
                    Deaths=deaths)

write.xlsx(wide_syn,"data/SEIRH_Beta.xlsx")

