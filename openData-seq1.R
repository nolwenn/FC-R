

## ----Ia-------------------------------------------------------------------------------------------------
## Lecture des fichiers
ccam20 <- read.csv("Open_ccam_20.csv", header=T, sep=";", na.strings = ".", dec=",")
## avec la library data.table
# library(data.table)
# ccam20 <- fread("Open_ccam_20.csv")


## ----Ib-------------------------------------------------------------------------------------------------
library(skimr)
skim(ccam20)


## ----Ic, echo=T-----------------------------------------------------------------------------------------
## region et département
ccam20$reg <- as.factor(ccam20$reg)
ccam20$dep <- as.factor(ccam20$dep)
# acte
ccam20$acte <- substr(ccam20$acte, 1, 7)
ccam20$acte <- as.factor(ccam20$acte)

# FINESS
ccam20$FinessGeo<- ifelse(nchar(ccam20$FinessGeo)==8, 
                        paste("0",ccam20$FinessGeo,sep=""), ccam20$FinessGeo)


## ----Id, message=FALSE----------------------------------------------------------------------------------
# lecture de codes CCAM pour les actes endovasculaires d'intérêt
ccam_endo <- read.csv("liste_ccam2020.csv", sep=",")


## ----ccam selection, echo=T, message=FALSE--------------------------------------------------------------
# Sélection des actes endovasculaires dans la base ccam20
endo <- ccam20[ccam20$acte%in%ccam_endo$CDC_ACT, ]


## ----ccam selection2, message=FALSE---------------------------------------------------------------------
# endo <- ccam20[ccam20$acte%in%ccam_endo[,1], ]

## Avec la fonction subset
# endo <- subset(ccam20, ccam20$acte%in%ccam_endo$CDC_ACT)

## Avec la fonction filter de la librairie dplyr 
library(dplyr)
endo <- ccam20 %>% filter(acte%in%ccam_endo$CDC_ACT)


## ----Ie, eval=F-----------------------------------------------------------------------------------------
## save(endo, file="endovasculaire_20.Rdata" )


## ---- eval=F--------------------------------------------------------------------------------------------
## load("endovasculaire_20.Rdata")


## ----II1a, message=FALSE--------------------------------------------------------------------------------
# Affiche d'un premier niveau de résumé statistique
summary(endo$dms_globale)
# charger la library epiDisplay et ses fonctions
library(epiDisplay)
# calcul de intervalle de confiance sur la DMS
endoci <- ci(endo$dms_globale)
endoci


## ----II1b-----------------------------------------------------------------------------------------------
# utilisation de la fonction by pour l'affichage de résumés stat par région
by(endo$dms_globale, endo$reg, summary)
# avec dplyr
# endo %>% group_by(reg) %>% summarise("Moyenne"= mean(dms_globale, na.rm=T))


## ----II1d, echo=TRUE------------------------------------------------------------------------------------
endo$Top <- ifelse(endo$dms_globale > endoci$upper95ci, 1, 0)


## ----II1dres--------------------------------------------------------------------------------------------
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


## ----II1e-----------------------------------------------------------------------------------------------
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


## ----II1f-----------------------------------------------------------------------------------------------
## proportion par région
round(prop.table(table(endo$Top, endo$reg), margin = 2)*100,2)


## ----II2a-c, echo=FALSE---------------------------------------------------------------------------------
## Lire le fichier finess
finess <- read.csv("etalab_stock_et_20201231.csv", header=T, sep=";")

## Transformation de la variable mft en type factor
finess$mft <- as.factor(finess$mft)

## Si problème longueur code finess à 8 et non 9 (premier 0 non lu)
##summary(nchar(endo$FinessGeo))
##endo$FinessGeo<- ifelse(nchar(endo$FinessGeo)==8, 
##                       paste("0",endo$FinessGeo,sep=""), endo$FinessGeo)
##summary(nchar(endo$FinessGeo))


## ----II2d, echo=TRUE------------------------------------------------------------------------------------
## Jointure entre base avec clé de jointure de nom différent
## Si les 2 clés de jointures ont le même nom pas besoin de by.x et by.y mais juste by 
endofiness <- merge(endo, finess[, c("nofinesset", "mft")], by.x="FinessGeo",by.y="nofinesset", all.x=TRUE, all.y=FALSE)
## colnames(endofiness)


## ----II2e-correc, eval=F--------------------------------------------------------------------------------
## ## Avec dplyr
## endofiness <- left_join(endo, finess[, c("nofinesset", "mft")],
##                         by=c("FinessGeo"=="nofinesset"))


## ----II2f, echo=TRUE, results=FALSE---------------------------------------------------------------------
# useNA = "always pour visualiser si il y a de NA
table(endofiness$mft, useNA = "always")
levels(endofiness$mft)
endofiness$mft <- droplevels(endofiness$mft)
table(endofiness$mft, useNA = "always")
endofiness$mft <- factor(endofiness$mft, 
                            labels=c("public", "public","non lucratif", "privé",
                                     "non lucratif", "privé",
                                     "public","privé","indéterminé"))


## ----II2f-correc----------------------------------------------------------------------------------------
# Table de contingence de la variable mode de paiement
# Si table de contingence retourne des modalités à 0 on les retire en les forçant à NA
# Table de contingence de la variable mode de paiement sans les O
# Ajouter les labels (étiquettes) à la variable mft


## ----II2g-----------------------------------------------------------------------------------------------
## Proportion avec total colonne=100
prop.table(table(endofiness$Top, endofiness$mft), 2)*100
## Proportion avec total ligne=100
prop.table(table(endofiness$Top, endofiness$mft), 1)*100


## ----II3a-----------------------------------------------------------------------------------------------
# Création de vecteur de 0
endofiness$Ambu <- 0
# Remplacement des 0 par 1 si dans colonne nb_sej_0_nuit j'ai une valeur (pas de na !is.na() )
endofiness$Ambu[!is.na(endofiness$nb_sej_0_nuit)] <- 1

## Si je n'ai pas de NA ( !is.na() ) alors 1 sinon 0
# endofiness$Ambu <- ifelse(!is.na(endofiness$nb_sej_0_nuit), 1, 0)
## Si j'ai des NA alors 0 sinon 1 (avec étiquette)
# endofiness$Ambu <- ifelse(is.na(endofiness$nb_sej_0_nuit), "pas ambu", "ambu")
#endofiness$Ambu <- as.factor(endofiness$Ambu)


## ----II3b-----------------------------------------------------------------------------------------------
endofiness$Top <- factor(endofiness$Top, levels=c(0,1),
                            labels=c("dms", "sup dms"))
tabTopAmbu <- table("Dépassement"=endofiness$Top,
                    "Ambulatoire"=endofiness$Ambu)
tabTopAmbu
# 1 tiers des établissements qui ne font pas de l'ambulatoire
# depasse la DMS pour au moins 1 acte contre 15% pour les autres
prop.table(tabTopAmbu, 2)*100


## ----III1a----------------------------------------------------------------------------------------------
chisq.test(tabTopAmbu)
chisq.test(endofiness$Top, endofiness$Ambu)

fisher.test(tabTopAmbu)
fisher.test(endofiness$Top, endofiness$Ambu)


## ----III1b----------------------------------------------------------------------------------------------
summary(endofiness$nb_actes)
med_acte <- median(endofiness$nb_actes)
endofiness$nbactes_eleve <- ifelse(endofiness$nb_actes > median(endofiness$nb_actes), 1, 0)
#endofiness$nbactes_eleve <- ifelse(endo$nb_actes > med_acte, 1, 0)


## ----III1c----------------------------------------------------------------------------------------------
# par exemple
by(endofiness$dms_globale, endofiness$nbactes_eleve, epiDisplay::ci)
endofiness %>% group_by(nbactes_eleve) %>% summarise(mean(dms_globale))


## ----III1d----------------------------------------------------------------------------------------------
by(endofiness$dms_globale, endofiness$nbactes_eleve, shapiro.test)
wilcox.test(endofiness$dms_globale ~ endofiness$nbactes_eleve)


## ----III2a----------------------------------------------------------------------------------------------
by(endofiness$dms_globale,  endofiness$mft, mean)


## ----III2b----------------------------------------------------------------------------------------------
# Analyse non paramétrique
kruskal.test(dms_globale ~ mft, data=endofiness)
boxplot(dms_globale ~ mft, data=endofiness)
library(rstatix)
dunn_test(dms_globale ~ mft, data=endofiness)

# ANOVA -  Tukey HSD test même si méthodologiquement très limite
#aovmft <- aov(dms_globale ~ mft, data=endofiness)

# vérification a posteriori de la validité de l'ANOVA
#plot(aovmft)
#summary(aovmft)
#TukeyHSD(aovmft)


## ----III2b bis, echo=F----------------------------------------------------------------------------------
# Si on retire les indéterminés
endofiness2 <- endofiness %>% filter(mft !="indéterminé")
levels(endofiness2$mft)[4] <- NA

# Analyse non paramétrique
kruskal.test(dms_globale ~ mft, data=endofiness2)
boxplot(dms_globale ~ mft, data=endofiness2)
dunn_test(dms_globale ~ mft, data=endofiness2)



## ----III3a----------------------------------------------------------------------------------------------
## Changement de catégorie de référence pour la variable mft
#endofiness$mft <- relevel(endofiness$mft, ref="privé")
m1 <- glm(Top ~  nb_actes + mft + Ambu, data=endofiness, family = binomial())
summary(m1)


## ----III3b----------------------------------------------------------------------------------------------
logistic.display(m1, simplified = T)


## ----IV1a, message=FALSE--------------------------------------------------------------------------------
boxplot(endo$dms_globale ~ endo$reg, xlab="région", ylab="DMS (en jours)")


## ----IV1b, message=FALSE--------------------------------------------------------------------------------
library(ggplot2)
ggplot(endo, aes(reg,dms_globale)) + 
  geom_boxplot(aes(group=reg)) + xlab("Regions") + ylab("DMS (jours)")


## ----IV1c-----------------------------------------------------------------------------------------------
ggplot(endo, aes(x=nb_actes,  y=dms_globale)) + geom_point() 


## ----IV1d-----------------------------------------------------------------------------------------------
library(RColorBrewer)
ggplot(endofiness, aes(x=nb_actes,  y=dms_globale, colour=mft)) +
   geom_point(alpha=0.6) + scale_color_brewer(palette="Set2") + 
   xlim(3,100) + labs(title="DMS et actes", y="DMS (jours)", x="nombres d'actes", colour="Statut juridique")


## ----IV1e-----------------------------------------------------------------------------------------------
p <- ggplot(endofiness, aes(x=dms_globale, y=reg, fill=mft)) + geom_boxplot(aes(group=reg)) 
p + facet_wrap(.~ mft)


p <- ggplot(endofiness, aes(y=dms_globale, x=mft)) + geom_boxplot(aes(group=mft)) 
p + facet_wrap(.~ reg) +  theme(axis.text.x = element_text(angle = 45))


## ----IV1f-----------------------------------------------------------------------------------------------
ggplot(endofiness, aes(dms_globale, group=as.factor(mft))) + 
  geom_density(aes(fill=mft, alpha = 0.5)) 


## ----barplot--------------------------------------------------------------------------------------------
library(scales)  # pour l'échelle en pourcentage
prop_mft<- endofiness %>% select(FinessGeo, mft) %>% distinct(FinessGeo, mft) %>% count(mft) %>% mutate(percentage= n/sum(n))

ggplot(prop_mft, aes(x = mft, y = percentage)) + geom_bar(stat = "identity")+ scale_y_continuous(labels=scales::percent) 


## ----IV2a-----------------------------------------------------------------------------------------------
endofiness$dep <- factor(endofiness$dep)
dmsdep <-  by(endofiness$dms_globale, endofiness$dep, mean, na.rm=T)
dmsdep <- as.vector(dmsdep)
dmsdep <- data.frame("dep"=levels(endofiness$dep), "dms"=dmsdep)

# Avec dplyr
dmsdep <- endofiness %>% group_by(dep) %>% summarise("dms" = mean(dms_globale, na.rm=T))
dmsdep$dep <- ifelse(nchar(as.character(dmsdep$dep))==2, as.character(dmsdep$dep),
                     paste("0", as.character(dmsdep$dep), sep=""))
dmsdep$dep <- factor(dmsdep$dep)


## ----IV2b-----------------------------------------------------------------------------------------------
library(ggmap)
library(maps)
france <- map_data("france")


## ----IV2c-----------------------------------------------------------------------------------------------
dep2reg <- read.csv("dep2reg.csv", header=T, sep=",")


## ----IV2d-----------------------------------------------------------------------------------------------
dmsdep <-  merge(dmsdep, dep2reg, by ="dep", all.y=T)
dmsdep2map <- merge(france, dmsdep, by.x="region", by.y="label_dep", all.x=T)


## ----IV2e, echo=TRUE, eval=TRUE-------------------------------------------------------------------------
dmsdep2map <- dmsdep2map[order(dmsdep2map$order), ]
ggplot(data = dmsdep2map) + 
   geom_polygon(aes(x = long, y = lat, fill = dms, group = group)) + 
  scale_fill_gradientn(colours =heat.colors(7, alpha=0.8, rev = T))
   coord_fixed(1.3)

