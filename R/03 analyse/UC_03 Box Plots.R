library(dplyr)
library(tidyr)

params <- fits %>%
  select(ExpNumber,Epoch,IndCode,EpIndCode,Params) %>%
  mutate(IndCode=factor(IndCode,levels=rev(c("CHD","CH","CD","HD","C","H","D")))) %>%
  unnest(cols = "Params") %>%
  pivot_longer(names_to  = "Parameter",
               values_to = "Value",
               -c(SN,ExpNumber,Epoch,IndCode,EpIndCode))


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
  Parameter=c("CF","DF","HF","Beta_Param","inv_phi1","inv_phi2","inv_phi3"),
  Value=c(.6,.1,.1,1.0,1/10.0,1/20.0,1/40.0)
)

ep1<-ggplot(filter(params,Epoch=="Epoch1"), aes(x=IndCode, y=Value,fill=IndCode)) +
  guides(fill=FALSE) +
  coord_flip() +
  stat_summary(fun.data = quantiles_95, geom="boxplot",width=0.3)+
  geom_hline(data=true_vals,aes(yintercept = Value),linetype="dashed")+
  facet_wrap(~Parameter,scales="free_x")

ep2<-ggplot(filter(params,Epoch=="Epoch2"), aes(x=IndCode, y=Value,fill=IndCode)) +
  guides(fill=FALSE) +
  coord_flip() +
  stat_summary(fun.data = quantiles_95, geom="boxplot",width=0.3)+
  geom_hline(data=true_vals,aes(yintercept = Value),linetype="dashed")+
  facet_wrap(~Parameter,scales="free_x")

ep3<-ggplot(filter(params,Epoch=="Epoch3"), aes(x=IndCode, y=Value,fill=IndCode)) +
  guides(fill=FALSE) +
  coord_flip() +
  stat_summary(fun.data = quantiles_95, geom="boxplot",width=0.3)+
  geom_hline(data=true_vals,aes(yintercept = Value),linetype="dashed")+
  facet_wrap(~Parameter,scales="free_x")


