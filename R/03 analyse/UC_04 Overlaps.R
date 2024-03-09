library(overlapping)
library(purrr)
library(dplyr)
library(tidyr)
library(ggpubr)

all_params <- fits %>%
               select(EpIndCode,Params) %>%
               unnest(cols = "Params")

# Prepare the data for pair-wise analysis for each parameter

prep <- map_df(list("CF","HF","DF","Beta_Param"),~{
  x1 <- all_params %>%
          select(EpIndCode,SN,dplyr::matches(.x))

  cf <- pivot_wider(x1,names_from=EpIndCode,
                       values_from = .x) %>%
        mutate(Param=.x) %>%
        select(Param,everything())
  cf
}) %>% group_by(Param) %>% nest()

overlaps <- prep %>%
              mutate(Overlap=map(data,~{
                d <- select(.x,-SN)
                exps <- names(d)
                cbs <- combn(exps,2)
                inputs <- list(From=cbs[1,],
                               To=cbs[2,])

                over <- map2_df(inputs$From,inputs$To,~{
                  v1 <- d[,.x] %>% pull()
                  v2 <- d[,.y] %>% pull()
                  ol <- overlap(list(v1,v2))$OV
                  tibble(From=.x,To=.y,Overlap=ol)
                })
                
                over
              })) 

oa <- overlaps %>% 
       select(Param,Overlap) %>% 
       unnest(cols="Overlap") %>%
       mutate(From=factor(From,levels=fits$EpIndCode),
              To=factor(To,levels=fits$EpIndCode))


s_oa <- oa %>%
          group_by(From,To) %>%
          summarise(Median=median(Overlap),
                    Mean=round(mean(Overlap),2),
                    Min=min(Overlap),
                    Max=max(Overlap))

p <- ggplot(s_oa,aes(x=To,y=From,fill=Mean))+geom_tile()+
  scale_fill_gradient2(midpoint = 0.5,mid="grey70",limits=c(0,1))+
  geom_text(aes(To, From, label=Mean), colour = "white", check_overlap = TRUE)+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,hjust = 1),
        panel.background = element_rect(fill="white"),
        panel.grid=element_line(colour="blue",linetype=3,linewidth = 0.3))+
  labs(fill="Mean overlap estimate")


sp_oa <- oa %>%
  group_by(From,To,Param) %>%
  summarise(Median=median(Overlap),
            Mean=round(mean(Overlap),2),
            Min=min(Overlap),
            Max=max(Overlap))

target <- "HF"
p <- ggplot(filter(sp_oa,Param==target),aes(x=To,y=From,fill=Mean))+geom_tile()+
  scale_fill_gradient2(midpoint = 0.5,mid="grey70",limits=c(0,1))+
  geom_text(aes(To, From, label=Mean), size=3,colour = "white", check_overlap = TRUE)+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45,hjust = 1),
        panel.background = element_rect(fill="white"),
        panel.grid=element_line(colour="grey",linetype=3,linewidth = 0.3))+
  labs(fill="Mean overlap estimate",
       subtitle=paste0("Parameter = ",target))


# Plot histograms to show the overlaps
plots <- prep %>%
          mutate(Plots=map2(Param,data,~{
            set.seed(100)
            d <- select(.y,-SN)
            rn <- sample(1:ncol(d),4)
            d <- d %>% select(rn)
            plots <- final.plot(as.list(d),pairs=T)+labs(subtitle=.x)
          }))

p1 <- ggarrange(plotlist = plots$Plots)




