library(dplyr)
library(tidyr)

test <- fits$Params[[1]] %>%
         select(SN,CF,R0) %>%
         pivot_longer(-SN)

params <- fits %>%
            select(ExpNumber,Epoch,IndCode,EpIndCode,Params) %>%
            unnest(cols = "Params") %>%
            pivot_longer(names_to  = "Parameter",
                         values_to = "Value",
                         -c(SN,ExpNumber,Epoch,IndCode,EpIndCode)) 

p <- ggplot(params,aes(x=IndCode,y=Value,colour=Epoch))+
       geom_violin()+
       facet_wrap(Epoch~Parameter,scales = "free")
            

# p <- ggplot(test,aes(x=name,y=value,fill=name))
# 
# p + geom_violin(alpha=0.2) +
#      geom_boxplot(width=.1,fill="black",outlier.color = NA)+
#   stat_summary(fun.y=median,geom="point",fill="white",shape=21,size=2.5)
