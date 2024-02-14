library(readxl)
library(tidyr)
library(dplyr)
library(purrr)

get_data <- function(file){
  case_data <- read_xlsx(file)
  
  # Assuming 100 observations
  case_data <- case_data %>%
                 mutate(Epoch1=c(rep(TRUE,33),rep(FALSE,67)),
                        Epoch2=c(rep(TRUE,67),rep(FALSE,33)),
                        Epoch3=rep(TRUE,100))
}


filter_data <- function(epi,Epoch="Epoch3"){
  filter(epi,!!as.name(as.character(Epoch))==TRUE)
}

shorten_ind <- function(x){
  x <- str_replace(x,"Cases","C") %>%
       str_replace("Hospitalisations","H") %>%
       str_replace("Deaths","D")
  paste(x,collapse = "")
}

configure_exp <- function(epochs, meas_models){
  o <- expand.grid(epochs,meas_models) %>%
         as_tibble() %>%
        rename(Epoch=Var1,
               Indicators=Var2) %>%
        mutate(ExpNumber=1:nrow(.))
  
  o <- o %>%  
        mutate(IndCode=map_chr(Indicators,~{
          shorten_ind(.x)}),
          NIndicators=map_dbl(Indicators,~length(.x)),
          EpIndCode=paste0(Epoch,"-",IndCode)) %>%
          select(ExpNumber,Epoch,IndCode, EpIndCode,NIndicators,everything())

}