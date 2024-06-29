library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(cmdstanr)
library(posterior)
library(tidybayes)
library(tidyr)

source("R/04 Poisson Example/Get Pairwise.R")

N      <- 1000
LAMBDA <- 100


cases_syn <- rpois(N,LAMBDA)

hist(cases_syn)

r_mod <- cmdstan_model(here("R/04 Poisson Example/stan", 
                            "model1.stan"))

data <- list(
  N = N,
  cases = cases_syn
)

fit <- r_mod$sample(data = data, parallel_chains = 4)
fit

posterior_df <- as_draws_df(fit$draws())

p <- posterior_df %>%
       select(lambda,log_lik) %>% 
       get_pair_wise("Inference Results")

pars_long <- posterior_df[, c("prior_lambda","lambda")] |>
  mutate(iter = row_number()) |> 
  pivot_longer(-iter, names_to = "par")

p_pars <- ggplot(pars_long, aes(value)) +
  geom_histogram(colour = "white", fill = "grey60", alpha = 0.75) +
  facet_wrap(vars(par), scales = "free") +
  geom_vline(xintercept = LAMBDA,colour="red")+
  theme_classic()




