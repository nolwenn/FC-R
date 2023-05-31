## Formation R
## Nolwenn Le Meur
## 30 mai 2023

# load library
library(tidyverse)
library(ggplot2)
library(skimr)

# lecture file
ccam20 <- read.csv("Open_ccam_20.csv", header=T, sep=";", na.strings = ".", dec=",")
# overview
summary(ccam20)


sd(ccam20$nb_actes)/sqrt(nrow(ccam20))
sd(ccam20$nb_actes)


#--------------------------WORDCLOUD -------------------------

# source : http://www.sthda.com/french/wiki/text-mining-et-nuage-de-mots-avec-le-logiciel-r-5-etapes-simples-a-savoir
library("wordcloud")
dat<- ccam20 %>% group_by(acte) %>% 
  summarise("n" = sum(nb_actes)) 

set.seed(256)
wordcloud(words = dat$acte, freq = dat$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#--------------------------FORESTPLOT-----------------------------

#source:  https://nightingalehealth.github.io/ggforestplot/index.html
#devtools::install_github("NightingaleHealth/ggforestplot")
library(forestplot)

resglm2 <- data.frame(summary(glm2)$coef)
names(resglm2) <- c("estimate","se", "z", "pvalue")
resglm2$name <- rownames(resglm2)

ggforestplot::forestplot(
  df = resglm2,
  estimate = estimate,
  logodds = FALSE,
  pvalue = pvalue,
  colour = NULL,
  shape = NULL,
  title = "Forest plot",
  xlab = "Odds ratio for incident death"
)