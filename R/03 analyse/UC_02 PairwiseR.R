library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplotify)

source("R/03 analyse/Get Pairwise.R")

res <- pmap(list(fits$Epoch,fits$IndCode,fits$Params),~{
   get_pair_wise(..3[,-(1:3)],paste0("Ep ",..1," D=",..2))
})




cors <- pmap(list(fits$Epoch,fits$IndCode,fits$Params),~{
  tb <- cor(..3[,-1]) %>% 
         as_tibble() %>% 
         mutate(From=colnames(.),Epoch=..1,IndCode=..2) %>%
         select(Epoch,IndCode,From,everything())
  
  tb_l <- tb %>% pivot_longer(names_to = "To", values_to= "Cor",
                              -c(Epoch,IndCode,From))
}) %>% dplyr::bind_rows() %>% filter(From!=To)

cors1 <- pmap(list(fits$Epoch,fits$IndCode,fits$Params),~{
  m <- cor(..3[,-1]) 
  m[upper.tri(m,diag = TRUE)] <- NA
  
  tb <- m %>% as_tibble() %>% 
    mutate(From=colnames(.),Epoch=..1,IndCode=..2) %>%
    select(Epoch,IndCode,From,everything())

  tb_l <- tb %>% 
            pivot_longer(names_to = "To", 
                         values_to= "Cor",
                         -c(Epoch,IndCode,From)) %>%
          filter(!is.na(Cor))
}) %>% dplyr::bind_rows()

sum1 <- cors1 %>%
          group_by(From,To) %>%
          summarise(MinCor=min(Cor,na.rm = T),
                    MeanCor=mean(Cor,na.rm = T),
                    MedCor=median(Cor,na.rm = T),
                    MaxCor=max(Cor,na.rm = T),
                    RangeCor=MaxCor-MinCor,
                    SDCor=sd(Cor),
                    IndexMax=which.max(Cor),
                    IndexMin=which.min(Cor),
                    EpochMin=Epoch[IndexMin],
                    IndCodeMin=IndCode[IndexMin],
                    IndCodeMax=IndCode[IndexMax],
                    NumCors=n(),
                    FractPos=sum(Cor>=0)/NumCors,
                    FractNeg=sum(Cor<0)/NumCors)%>%
         select(-c(IndexMax,IndexMin)) %>%
         arrange(desc(NumCors))

sum2 <- cors1 %>%
         group_by(From,To,Epoch) %>%
         summarise(MinCor=min(Cor,na.rm = T),
                   MeanCor=mean(Cor,na.rm = T),
                   MedCor=median(Cor,na.rm = T),
                   MaxCor=max(Cor,na.rm = T),
                   RangeCor=MaxCor-MinCor,
                   SDCor=sd(Cor),
                   IndexMax=which.max(Cor),
                   IndexMin=which.min(Cor),
                   EpochMin=Epoch[IndexMin],
                   IndCodeMin=IndCode[IndexMin],
                   IndCodeMax=IndCode[IndexMax],
                   NumCors=n(),
                   FractPos=sum(Cor>=0)/NumCors,
                   FractNeg=sum(Cor<0)/NumCors)%>%
  select(-c(IndexMax,IndexMin)) %>%
  arrange(desc(NumCors))

sum3 <- cors1 %>%
         group_by(From,To,IndCode) %>%
         summarise(MinCor=min(Cor,na.rm = T),
                   MeanCor=mean(Cor,na.rm = T),
                   MedCor=median(Cor,na.rm = T),
                   MaxCor=max(Cor,na.rm = T),
                   RangeCor=MaxCor-MinCor,
                   SDCor=sd(Cor),
                   IndexMax=which.max(Cor),
                   IndexMin=which.min(Cor),
                   EpochMin=Epoch[IndexMin],
                   EpochMax=Epoch[IndexMax],
                   IndCodeMin=IndCode[IndexMin],
                   IndCodeMax=IndCode[IndexMax],
                   NumCors=n(),
                   FractPos=sum(Cor>=0)/NumCors,
                   FractNeg=sum(Cor<0)/NumCors)%>%
        select(-c(IndexMax,IndexMin)) %>%
        arrange(desc(NumCors))




