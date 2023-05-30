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

#------------------------ TD I - II --------------------------------

## ----Ia------------------------------------------------------------
## Lecture des fichiers
ccam20 <- read.csv("Open_ccam_20.csv", header=T, sep=";", na.strings = ".", dec=",")
## avec la library data.table
# library(data.table)
# ccam20 <- fread("Open_ccam_20.csv")


## ----Ib------------------------------------------------------------
## EDA ecploratory data analysis
library(skimr)
skim(ccam20)


## ----Ic, echo=T----------------------------------------------------
## region et département
ccam20$reg <- as.factor(ccam20$reg)
ccam20$dep <- as.factor(ccam20$dep)
# acte
ccam20$acte <- substr(ccam20$acte, 1, 7)
ccam20$acte <- as.factor(ccam20$acte)
#ccam$dep2 <- substr(ccam20$FinessGeo, 1, 2)

# FINESS
ccam20$FinessGeo<- ifelse(nchar(ccam20$FinessGeo)==8, 
                        paste("0",ccam20$FinessGeo,sep=""), ccam20$FinessGeo)


## ----Id, message=FALSE---------------------------------------------
# lecture de codes CCAM pour les actes endovasculaires d'intérêt
ccam_endo <- read.csv("liste_ccam2020.csv", sep=",")


## ----ccam selection, echo=T, message=FALSE-------------------------
# Sélection des actes endovasculaires dans la base ccam20
endo <- ccam20[ccam20$acte%in%ccam_endo$CDC_ACT, ]


## ----ccam selection2, message=FALSE--------------------------------
# endo <- ccam20[ccam20$acte%in%ccam_endo[,1], ]

## Avec la fonction subset
# endo <- subset(ccam20, ccam20$acte%in%ccam_endo$CDC_ACT)

## Avec la fonction filter de la librairie dplyr 
library(dplyr)
endo <- ccam20 %>% filter(acte %in% ccam_endo$CDC_ACT)
endo <- dplyr::filter(ccam20, acte %in% ccam_endo$CDC_ACT)

## ----Ie, eval=F----------------------------------------------------
## save(endo, file="endovasculaire_20.Rdata" )


## ---- eval=F-------------------------------------------------------
## load("endovasculaire_20.Rdata")


## ----II1a, message=FALSE-------------------------------------------
# Affiche d'un premier niveau de résumé statistique
summary(endo$dms_globale)
# charger la library epiDisplay et ses fonctions
library(epiDisplay)
# calcul de intervalle de confiance sur la DMS
endoci <- ci(endo$dms_globale)
endoci


## ----II1b----------------------------------------------------------
# utilisation de la fonction by pour l'affichage de résumés stat par région
by(endo$dms_globale, endo$reg, summary)
by(endo$dms_globale, endo$reg, mean, na.rm=T)
# avec dplyr
# endo %>% group_by(reg) %>% summarise("Moyenne"= mean(dms_globale, na.rm=T))


## ----II1d, echo=TRUE-----------------------------------------------
endo$Top <- ifelse(endo$dms_globale > endoci$upper95ci, 1, 0)


## ----II1dres-------------------------------------------------------
#endo$Top <- ifelse(endo$dms_globale > endoci$upper95ci, 1, 0)
#créez une variable binaire *Top* dans le table *endo* avec la modalité 1 lorsque #l'établissement a une dms supérieure à la borne supérieure de l'intervalle de confiance #de la moyenne  nationale de la dms et 0 autrement. 

# Autre méthode par indexation
#endo$Top <- 0
#endo$Top[endo$dms_globale > endoci$upper95ci] <- 1

## Si pas accès à epiDisplay et utilisation de la moyenne des DMS
# endo$Top <- ifelse(endo$dms_globale > mean(endo$dms_globale, na.rm=TRUE), 1, 0)
## ou
# endo$Top[endo$dms_globale > mean(endo$dms_globale, na.rm=T)] <- 1
## ou
# endo[endo$dms_globale > mean(endo$dms_globale, na.rm=T), "Top"] <- 1


## ----II1e----------------------------------------------------------
## Si 1 acte - 1 hôpital par rapport à la dms nationale
table(endo$Top, endo$reg)

## Si n actes par hôpital
meanbyfiness <- endo %>% group_by(FinessGeo, reg) %>% 
  summarise("mean_etab"= mean(dms_globale, na.rm=T))
## ou on converse la table d'origine et ajoute d'un colonne mean_etab
## où la moyenne est répété pour chaque établissement
meanbyfiness2 <- endo %>% group_by(FinessGeo) %>% 
  mutate("mean_etab"= mean(dms_globale, na.rm=T))
## si que des NAs dans dms_globale pour 1 hôpital 
# meanbyfiness <- meanbyfiness[meanbyfiness$mean!="NaN",]

# ligne unique par établissement unique
# library base R (default) dmsFiness2 <- unique(meanbyfiness2[,c("FinessGeo", "reg", "mean_etab")])
# méthode dplyr
dmsFiness <- distinct(meanbyfiness2[,c("FinessGeo", "reg","mean_etab")])

# nouveau calcul de la dms national et de l'intervalle de confiance
cinat<- ci(meanbyfiness$mean_etab)
# nouvelle variable TOP
meanbyfiness$Top <- ifelse(meanbyfiness$mean_etab > cinat$upper95ci, 1, 0)
table(meanbyfiness$Top, meanbyfiness$reg)


## ----II1f----------------------------------------------------------
## proportion par région
round(prop.table(table(endo$Top, endo$reg), margin = 2)*100,2)


## ----II2a-c, echo=FALSE--------------------------------------------
## Lire le fichier finess
finess <- read.csv("etalab_stock_et_20201231.csv", header=T, sep=";")

## Transformation de la variable mft en type factor
finess$mft <- as.factor(finess$mft)

## Si problème longueur code finess à 8 et non 9 (premier 0 non lu)
##summary(nchar(endo$FinessGeo))
##endo$FinessGeo<- ifelse(nchar(endo$FinessGeo)==8, 
##                       paste("0",endo$FinessGeo,sep=""), endo$FinessGeo)
##summary(nchar(endo$FinessGeo))


## ----II2d, echo=TRUE-----------------------------------------------
## Jointure entre base avec clé de jointure de nom différent
## Si les 2 clés de jointures ont le même nom pas besoin de by.x et by.y mais juste by 
endofiness <- merge(endo, finess[, c("nofinesset", "mft")], by.x="FinessGeo",
                    by.y="nofinesset", all.x=TRUE, all.y=FALSE)
## colnames(endofiness)


## ----II2e-correc, eval=F-------------------------------------------
## ## Avec dplyr
## endofiness <- left_join(endo, finess[, c("nofinesset", "mft")],
##                         by=c("FinessGeo"=="nofinesset"))


## ----II2f, echo=TRUE, results=FALSE--------------------------------
# useNA = "always pour visualiser si il y a de NA
table(endofiness$mft, useNA = "always")
levels(endofiness$mft)
endofiness$mft <- droplevels(endofiness$mft)
table(endofiness$mft, useNA = "always")
endofiness$mft <- factor(endofiness$mft, 
                            labels=c("public", "public","non lucratif", "privé",
                                     "non lucratif", "privé",
                                     "public","privé","indéterminé"))


## ----II2f-correc---------------------------------------------------
# Table de contingence de la variable mode de paiement
# Si table de contingence retourne des modalités à 0 on les retire en les forçant à NA
# Table de contingence de la variable mode de paiement sans les O
# Ajouter les labels (étiquettes) à la variable mft


## ----II2g----------------------------------------------------------
## Proportion avec total colonne=100
prop.table(table(endofiness$Top, endofiness$mft), 2)*100
## Proportion avec total ligne=100
prop.table(table(endofiness$Top, endofiness$mft), 1)*100

