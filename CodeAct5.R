library(tidyverse)
library(VIM)
library(mice)
library(mvoutlier)
library(ade4)

set.seed(197273)
data_dragon <- read_csv("dragons.csv")

#==============================================================================
#Identification des données manquantes
matrixplot(data_dragon %>%
             select(-c(ID, Species)))

plot(aggr(data_dragon %>%
            select(-c(ID, Species))))

row_missing <- data_dragon %>%
  filter(apply(., 1, function(x) any(is.na(x))))

dragons_mice <- mice(data_dragon, method = "rf")
dragons_imp <- complete(dragons_mice)

nrow(row_missing) / nrow(data_dragon)

dragons_imp[!complete.cases(data_dragon), ]

#==============================================================================
#Données aberrantes

boxplot(data_dragon %>%
          select(-c(ID, Species)))
is_out <- sign2(dragons_imp %>%
                  select(-Species), qcrit = 0.975)$wfinal01
sum(is_out == 0) / length(is_out)

aberr <- sign2(dragons_imp %>%
                 select(-Species), qcrit = 0.975)
aberr
plot(dragons_imp %>%
       select(-ID, -Species), col = is_out + 2)

dragon_sans_out <- dragons_imp[-which(is_out == 0), ]

dragon_sans_out[!complete.cases(dragons_imp), ]

#==============================================================================
#Analyse discirminante linéaire

dragon_pca <- dudi.pca(df = dragon_sans_out %>% select(-c(ID, Species)),
                     scannf = FALSE, # ne pas générer de graphique
                     scale = TRUE)

dragon_lda <- discrimin(dudi = dragon_pca,
                      fac = as.factor(dragon_sans_out$Species),
                      scannf = FALSE)
plot(dragon_lda)

source("https://raw.githubusercontent.com/essicolo/AgFun/master/plotDA_gg.R")
plotDA(scores = dragon_lda$li,
       loadings = dragon_lda$fa,
       fac = as.factor(dragon_sans_out$Species),
       level=0.95,
       facname = "Species",
       propLoadings = 1) 

dragon_pca$c1
dragon_lda
