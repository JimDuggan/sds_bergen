library(ggplot2)

tidy_TS <- function(TS){
  td <- TS %>%
          pivot_longer(-c(SN,Chain),names_to = "Variable",values_to = "Value") %>%
          mutate(Time=as.integer(stringr::str_extract(Variable, "\\d+")),
                 Variable=str_extract(Variable,pattern = "sim\\_.")) %>%
          select(SN,Chain,Time,Variable,Value) %>%
          mutate(Variable=case_when(
            Variable == "sim_C" ~ "Cases",
            Variable == "sim_H" ~ "Hospitalisations",
            Variable == "sim_D" ~ "Deaths",
            TRUE ~ "Undefined"
          ))
}


prepare_data1 <- function(exp,config){
  # res <- readRDS(file)


  fits <- exp %>%
    mutate(Divergent=map_dbl(StanFit,
                             ~sum(apply(posterior::subset_draws(.x$sampler_diagnostics(), variable = "divergent__"), 2, sum))),
           
           StanDraws=map(StanFit,
                         ~as_draws_df(.x$draws()) %>%
                           mutate(SN=1:nrow(.)) %>%
                           select(SN,everything()) %>%
                           as_tibble()),
           
           Params=map(StanDraws,~{
             select(.x,dplyr::matches("CF"),
                    dplyr::matches("HF"),
                    dplyr::matches("DF"),
                    dplyr::matches("Beta_Param"),
                    dplyr::matches("inv_phi1"),
                    dplyr::matches("inv_phi2"),
                    dplyr::matches("inv_phi3"),
                    dplyr::matches(".chain"),
                    dplyr::matches(".iteration")) %>%
              mutate(SN=1:nrow(.)) %>%
              rename(Chain=.chain,
                     Iteration=.iteration) %>%
              select(SN,Chain,Iteration,everything()) %>%
            as_tibble()
           }),
           
           TS=map(StanDraws,~{
             select(.x,SN,
                    dplyr::matches(".chain"),               
                    dplyr::matches("sim_C"),
                    dplyr::matches("sim_H"),
                    dplyr::matches("sim_D")) %>%
            rename(Chain=.chain) %>%
            tidy_TS(.) %>%
            as_tibble()
           }),
           
           RunTime=as.numeric(map(config$RUN_INFO,"Duration")))
  
  select(fits,-c(StanFit,StanDraws))
  
}



