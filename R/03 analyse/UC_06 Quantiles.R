library(dplyr)
library(tidyr)
library(ggpubr)
library(gridExtra)

params <- fits %>%
  select(ExpNumber,Epoch,IndCode,EpIndCode,Params) %>%
  mutate(IndCode=factor(IndCode,levels=rev(c("CHD","CH","CD","HD","C","H","D")))) %>%
  unnest(cols = "Params") %>%
  pivot_longer(names_to  = "Parameter",
               values_to = "Value",
               -c(SN,Chain,Iteration,ExpNumber,Epoch,IndCode,EpIndCode)) %>%
  filter(Parameter %in% c("Beta_Param","CF","DF","HF"))


q <- params %>%
      group_by(EpIndCode, Parameter) %>%
      summarise(Q_0.025=round(quantile(Value,0.025),3),
                Median=round(median(Value),3),
                Mean=round(mean(Value),3),
                Q_0.975=round(quantile(Value,0.975),3),
                Q95Range=round(Q_0.975-Q_0.025,3)) %>%
      ungroup() %>%
      arrange(Parameter,Q95Range)

png("paper/diagrams/quants_beta_param.png", width=600,height=480,bg = "white")
grid.table(filter(q,Parameter=="Beta_Param"))
dev.off()

png("paper/diagrams/quants_CF_param.png", width=600,height=480,bg = "white")
grid.table(filter(q,Parameter=="CF"))
dev.off()

png("paper/diagrams/quants_HF_param.png", width=600,height=480,bg = "white")
grid.table(filter(q,Parameter=="HF"))
dev.off()

png("paper/diagrams/quants_DF_param.png", width=600,height=480,bg = "white")
grid.table(filter(q,Parameter=="DF"))
dev.off()

q2 <- params %>%
  group_by(Epoch,IndCode, Parameter) %>%
  summarise(Q_0.025=round(quantile(Value,0.025),3),
            Median=round(median(Value),3),
            Mean=round(mean(Value),3),
            Q_0.975=round(quantile(Value,0.975),3),
            Q95Range=round(Q_0.975-Q_0.025,3)) %>%
  ungroup() %>%
  select(Epoch,IndCode,Parameter,Median,Q95Range) %>%
  arrange(Parameter,Q95Range)

r_q2_epoch <- q2 %>%
               group_by(Parameter,Epoch) %>%
               mutate(RankEpoch=rank(Q95Range)) %>%
               arrange(Parameter,Epoch,RankEpoch) %>%
               ungroup()

av_rank_epoch <- r_q2_epoch %>%
                   group_by(Epoch,Parameter) %>%
                   summarise(MinRank=min(RankEpoch),
                             MaxRank=max(RankEpoch),
                             MedRank=median(RankEpoch))
                  


r_q2_ind <- q2 %>%
  group_by(Parameter,IndCode) %>%
  mutate(RankIndCode=rank(Q95Range)) %>%
  arrange(Parameter,IndCode)

av_rank_ind <- r_q2_ind %>%
  group_by(IndCode,Parameter) %>%
  summarise(MinRank=min(RankIndCode),
            MaxRank=max(RankIndCode),
            MedRank=median(RankIndCode))


