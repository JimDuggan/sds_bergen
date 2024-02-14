# Generate plots for the TS and show fits
# Would be useful to have a facet_grid of Experiments~Indicator

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(purrr)

get_plot <- function(d,syn,data_var,epoch){
  ggplot(d,aes(x=Time,y=Mean))+
    geom_ribbon(aes(ymin=QL,ymax=QU),alpha=0.15,fill = "grey80", alpha = 0.5)+
    geom_line()+
    geom_point(data=syn,mapping=aes_string(y=data_var),size=0.3)+
    facet_wrap(~Indicators,scales="free_y")+
    theme_classic()+
    labs(y=data_var, subtitle = paste0("DS=",epoch))
}

syn_data <- fits %>%
             select(ExpNumber,Epoch,FitData) %>%
             unnest(cols = "FitData")

sim_data  <- fits %>%
             select(ExpNumber,Epoch,IndCode,TS) %>%
             unnest(cols = "TS")

STS <- sim_data %>%
       group_by(ExpNumber,Variable,Time) %>%
       summarise(Epoch=first(Epoch),
                 Indicators=first(IndCode),
                 Mean=mean(Value),
                 QL=quantile(Value,0.025),
                 QU=quantile(Value,0.975))


targets <- fits %>% 
             select(Epoch,Indicators) %>% 
             unnest(cols = Indicators)

plots <- map2(targets$Epoch,targets$Indicators,~{
  get_plot(filter(STS,Variable==.y,Epoch==.x),
           filter(syn_data,Epoch==.x),
           .y,
           .x)
})

p1 <- ggarrange(plotlist = plots)


