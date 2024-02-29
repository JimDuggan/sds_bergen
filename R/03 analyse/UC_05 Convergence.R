library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplotify)

p_convs <- fits %>%
            select(ExpNumber,Epoch,IndCode,EpIndCode,Params) %>%
            mutate(IndCode=factor(IndCode,levels=rev(c("CHD","CH","CD","HD","C","H","D")))) %>%
            unnest(cols = "Params") %>%
            pivot_longer(names_to  = "Parameter",
                         values_to = "Value",
                        -c(SN,Chain,Iteration,ExpNumber,Epoch,IndCode,EpIndCode))

# Focus on the parameters

convg <- p_convs %>%
           filter(Parameter %in% c("CF","HF","DF","Beta_Param"))

plots_conv <- map(unique(convg$Parameter),~{
  temp <-   filter(convg,Parameter == .x)
  ggplot(temp,aes(x=Iteration,y=Value,colour=Chain))+
    geom_line()+
    facet_wrap(~EpIndCode)+
    labs(subtitle = paste0("Parameter= ",.x))
})

pl1 <- ggarrange(plotlist = plots_conv)



