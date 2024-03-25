library(dplyr)
library(tidyr)
library(ggpubr)
library(purrr)

params <- fits %>%
  select(ExpNumber,Epoch,IndCode,EpIndCode,Params) %>%
  mutate(IndCode=factor(IndCode,levels=rev(c("CHD","CH","CD","HD","C","H","D")))) %>%
  unnest(cols = "Params") %>%
  pivot_longer(names_to  = "Parameter",
               values_to = "Value",
               -c(SN,Chain,Iteration,ExpNumber,Epoch,IndCode,EpIndCode)) %>%
  filter(Parameter %in% c("Beta_Param","CF","DF","HF"))


p_pars_ep3 <- ggplot(filter(params,Epoch=="Epoch3"), aes(Value)) +
  geom_histogram(colour = "white", fill = "grey60", alpha = 0.75) +
  facet_grid(IndCode~Parameter, scales = "free") +
  theme_classic()

p_pars_ep2 <- ggplot(filter(params,Epoch=="Epoch2"), aes(Value)) +
  geom_histogram(colour = "white", fill = "grey60", alpha = 0.75) +
  facet_grid(IndCode~Parameter, scales = "free") +
  theme_classic()

p_pars_ep1 <- ggplot(filter(params,Epoch=="Epoch1"), aes(Value)) +
  geom_histogram(colour = "white", fill = "grey60", alpha = 0.75) +
  facet_grid(IndCode~Parameter, scales = "free") +
  theme_classic()



quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  # r <- quantile(x, probs=c(0.025, 0.5, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  # names(r) <- c("ymin", "middle","ymax")
  r
}

true_vals <- tibble(
  Parameter=c("CF","DF","HF","Beta_Param"),
  Value=c(.6,.1,.1,1.0)
)

ep1<-ggplot(filter(params,Epoch=="Epoch1"), aes(x=IndCode, y=Value,fill=IndCode)) +
  guides(fill=FALSE) +
  coord_flip() +
  stat_summary(fun.data = quantiles_95, geom="boxplot",width=0.3)+
  geom_hline(data=true_vals,aes(yintercept = Value),linetype="dashed")+
  facet_wrap(~Parameter,scales="free_x")+
  labs(subtitle = "Data source Epoch1")

ep2<-ggplot(filter(params,Epoch=="Epoch2"), aes(x=IndCode, y=Value,fill=IndCode)) +
  guides(fill=FALSE) +
  coord_flip() +
  stat_summary(fun.data = quantiles_95, geom="boxplot",width=0.3)+
  geom_hline(data=true_vals,aes(yintercept = Value),linetype="dashed")+
  facet_wrap(~Parameter,scales="free_x")+
  labs(subtitle = "Data source Epoch2")

ep3<-ggplot(filter(params,Epoch=="Epoch3"), aes(x=IndCode, y=Value,fill=IndCode)) +
  guides(fill=FALSE) +
  coord_flip() +
  stat_summary(fun.data = quantiles_95, geom="boxplot",width=0.3)+
  geom_hline(data=true_vals,aes(yintercept = Value),linetype="dashed")+
  facet_wrap(~Parameter,scales="free_x")+
  labs(subtitle = "Data source Epoch3")

ep4<-ggplot(params, aes(x=IndCode, y=Value,colour=Epoch)) +
  guides(fill=FALSE) +
  coord_flip() +
  geom_boxplot()+
  # stat_summary(fun.data = quantiles_95, geom="boxplot",width=0.3)+
  geom_hline(data=true_vals,aes(yintercept = Value))+
  facet_wrap(~Parameter,scales="free_x")+
  labs(subtitle = "Exploring parameter distributions across three epochs")+
  theme(legend.position = "top")


#Extra ones...
den1 <-ggplot(filter(params,Parameter=="Beta_Param",Epoch=="Epoch1"), aes(x=Value,colour=IndCode,fill=IndCode)) +
       geom_density(alpha = 0.1)+
   labs(subtitle = "Exploring parameter distributions across three epochs")+
  theme(legend.position = "bottom")

TV_Beta <- 1.0
p_beta <- map(c("Epoch1","Epoch2","Epoch3"),~{
  ggplot(filter(params,Parameter=="Beta_Param",Epoch==.x), aes(x=Value,colour=IndCode,fill=IndCode)) +
    geom_density(alpha = 0.1)+
    geom_vline(xintercept = TV_Beta,linetype="dashed")+
    labs(subtitle = paste0("Beta_Param for ",.x))+
    theme(legend.position = "top",
          legend.text = element_text(size=8))
})

TV_CF <- 0.6
p_CF <- map(c("Epoch1","Epoch2","Epoch3"),~{
  ggplot(filter(params,Parameter=="CF",Epoch==.x), aes(x=Value,colour=IndCode,fill=IndCode)) +
    geom_density(alpha = 0.1)+
    geom_vline(xintercept = TV_CF,linetype="dashed")+
    labs(subtitle = paste0("Clinical Fraction for ",.x))+
    theme(legend.position = "top",
          legend.text = element_text(size=8))
})

TV_HF <- 0.1
p_HF <- map(c("Epoch1","Epoch2","Epoch3"),~{
  ggplot(filter(params,Parameter=="HF",Epoch==.x), aes(x=Value,colour=IndCode,fill=IndCode)) +
    geom_density(alpha = 0.1)+
    geom_vline(xintercept = TV_HF,linetype="dashed")+
    labs(subtitle = paste0("Hospitalisation Fraction for ",.x))+
    theme(legend.position = "top",
          legend.text = element_text(size=8))
})

TV_DF <- 0.1
p_DF <- map(c("Epoch1","Epoch2","Epoch3"),~{
  ggplot(filter(params,Parameter=="DF",Epoch==.x), aes(x=Value,colour=IndCode,fill=IndCode)) +
    geom_density(alpha = 0.1)+
    geom_vline(xintercept = TV_DF,linetype="dashed")+
    labs(subtitle = paste0("Death Fraction for ",.x))+
    theme(legend.position = "top",
          legend.text = element_text(size=8))
})

p_all_beta <- ggarrange(plotlist = p_beta,ncol=3)

p_all_CF <- ggarrange(plotlist = p_CF,ncol=3)

p_all_HF <- ggarrange(plotlist = p_HF,ncol=3)

p_all_DF <- ggarrange(plotlist = p_DF,ncol=3)




