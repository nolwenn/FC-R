## ----include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=T, echo=T)
solution <- F
library(tidyverse)
library(ggplot2)
library(R.utils)
library(data.table)


## ----correction, eval=T, echo=F, include=F----------------------------------
#Color Format
colFmt = function(x,color){
  outputFormat = knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(outputFormat == 'latex')
    paste("\\textcolor{",color,"}{",x,"}",sep="")
  else if(outputFormat == 'html')
    paste("<font color='",color,"'>",x,"</font>",sep="")
  else
    x
}


## ----lecture1, eval=F-------------------------------------------------------
## med2021_csv <- read.csv("../openMedic/OPEN_MEDIC_2021.CSV", header=T,  dec = ",",  sep=";")


## ----lecture2, eval=T, message=FALSE, cache=TRUE----------------------------
library(data.table)
med2021 <- fread("../openMedic/OPEN_MEDIC_2021.CSV", header=T, dec = ",")
## Attention code CIP13 en 64byte
## transfo en caractère pour l'obtenir en entier
med2021$CIP13 <- as.character(med2021$CIP13)
med2021$BEN_REG <- factor(med2021$BEN_REG)


## ----rm lecture1, eval=F----------------------------------------------------
## rm(med2021_csv)


## ---- eval=F----------------------------------------------------------------
## # BSE lue comme une chaîne de caractère
## ## conversion nombre avec délimitation des milliers en point (.)
## # elimination des points
## med2021$BSE2 <- gsub("[.]", "", med2021$BSE)
## ## conversion des décimales avec virgule (syst FR) par des points (syst en)
## med2021$BSE2  <- gsub(",", "\\.", med2021$BSE2)
## # conversion de caractères en numérique possible
## med2021$BSE2 <- as.numeric(med2021$BSE2)
## 
## #med2021$sexe <- factor(med2021$sexe, levels=c(1,2,9),
## #                       labels=c("MASCULIN","FEMININ","INCONNU"))


## ---------------------------------------------------------------------------
summary(med2021)


## ---------------------------------------------------------------------------
med2021 <- med2021 %>% filter(BOITES > 0)


## ----age2factor-------------------------------------------------------------
med2021$age <- factor(med2021$age, levels= c( 0, 20, 60, 99), 
                      labels=c("0-19 ANS","20-59 ANS","60 ANS ET +","AGE INCONNU"))


## ----age plot---------------------------------------------------------------
library(dplyr)
#command 1
ggplot(med2021, aes(age)) + geom_bar(stat="count")
#command 2
tabage<- med2021 %>% count(age)
ggplot(tabage, aes(x=age, y=n)) + geom_bar(stat="identity")



## ----age2region absolute, echo=TRUE-----------------------------------------
# Frequence absolue
med2021 %>% group_by(BEN_REG) %>% count(age)



## ----age2region relative----------------------------------------------------
# Frequence relative
freqRegAge<- med2021 %>% group_by(BEN_REG) %>%
  count(age) %>% 
  mutate("freq" = round(n /sum(n)*100,2))

# Bonus: Par région, nombre de classe ATC5 unique - éventail de prescription médicamenteuse en région
med2021 %>% group_by(BEN_REG) %>% summarise("ATC5"=length(unique(ATC5)))


## ----age2sexe---------------------------------------------------------------
med2021$sexe <- factor(med2021$sexe, labels=c("MASCULIN","FEMININ","INCONNU")) 
# ATTENTION: ne pas re-executer si déjà fait plus haut
#table(med2021$AGE, med2021$sexe)
#prop.table(table(med2021$AGE, med2021$sexe),1)*100
med2021 %>% group_by(sexe) %>% count(age) %>% mutate(freq = n / sum(n)*100)


## ----sexe2age---------------------------------------------------------------
med2021 %>% group_by(age) %>% count(sexe) %>% mutate(freq = n / sum(n)*100)


## ----bzh--------------------------------------------------------------------
#bzh<- med2021[med2021$BEN_REG == 53, ]
bzh <- med2021 %>% filter(BEN_REG == 53) 


## ----n05b-------------------------------------------------------------------
#bzhn05b <- bzh[bzh$ATC3 == "N05B", ]
bzhn05b <- bzh %>% filter(ATC3 == "N05B") 


## ----gp---------------------------------------------------------------------
#bzhn05bg<- bzhn05b[bzhn05b$PSP_SPE == 1,]
bzhn05bg <- bzhn05b %>% filter(PSP_SPE == 1) 


## ----pop--------------------------------------------------------------------
#bzhn05bg<- med2021[med2021$BEN_REG == 53 & med2021$ATC3 == "N05B" & med2021$PSP_SPE == 1, ]
bzhn05bg <- med2021 %>% filter(BEN_REG == 53 & ATC3 == "N05B" & PSP_SPE == 1) 


## ----by---------------------------------------------------------------------
#by(bzhn05bg$BOITES, list(bzhn05bg$sexe, bzhn05bg$AGE), sum)
bzhn05bg_count <-  bzhn05bg %>% group_by(age, sexe) %>% summarise("boites"=sum(BOITES)) %>% mutate("pourcentage"=boites/sum(boites)*100)
bzhn05bg_count


## ----skim, eval=F-----------------------------------------------------------
## library(gtsummary)
## bzhn05bg <- droplevels(bzhn05bg)
## bzhn05bg %>% select(age, sexe, BOITES) %>%
##   tbl_summary(by=age)
## 
## med2021 %>% select(age, sexe, BOITES) %>%
##   tbl_summary(by=age,
##               statistic = all_continuous() ~"{mean} ({sd})") %>% add_ci()


## ----plot-------------------------------------------------------------------
library(ggplot2)
#ggplot(data = bzhn05bg_count, aes(x=AGE, y=boites, fill=sexe)) + geom_bar(stat="identity")
# Variable sexe avec Femme en référence pour correspondance de colour
bzhn05bg_count$sexe <- relevel(bzhn05bg_count$sexe, ref = "FEMININ")
p <- ggplot(data = bzhn05bg_count, aes(x=age, y=boites, fill=sexe))
p + geom_bar(stat="identity") + scale_fill_brewer(palette="YlOrRd") + facet_wrap(.~ sexe)
#ggplot(data = bzhn05bg_count, aes(x=sexe, y=boites, fill=AGE)) + #geom_bar(stat="identity") 


## ----plot2------------------------------------------------------------------
library(ggplot2)
ggplot(data = bzhn05bg_count, aes(x=age, y=boites, fill=sexe)) + 
  geom_bar(stat="identity", position = "dodge") 
#ggplot(data = freq_anxio, aes(x=AGE, y=boites, fill=sexe)) + geom_bar(stat="identity", position = "dodge")


## ---------------------------------------------------------------------------
ggplot(data = bzhn05bg_count, aes(x=age, y=pourcentage, fill=sexe)) +
  geom_bar(stat="identity")  + 
  scale_fill_brewer(palette="YlOrRd")


## ---- eval=F, echo=TRUE-----------------------------------------------------
## openMed <- c()
## for(i in 2018:2021){
##     filename2open <- paste("../openMedic/OPEN_MEDIC_", i, ".CSV",sep="")
##     temp <- fread(filename2open, header=T, dec = ",")
##     temp <- temp[, -seq(2,12,2)]
##     if(i==2019){
##       colnames(temp)[10] <- "sexe"
##     }
##     temp <- cbind(temp,"ANNEE"=i)
##   openMed <- rbind(openMed, temp)
## }
## # verification
## # table(openMed$ANNE)


## ---- eval=FALSE, echo=TRUE-------------------------------------------------
## openMed53 <- openMed %>% filter(BEN_REG == 53 & ATC3 == "N05B" & PSP_SPE%in%c(1,17))
## openMed53$age <- factor(openMed53$age,
##                         labels=c("0-19 ANS","20-59 ANS","60 ANS ET +"))
## openMed53$sexe <- factor(openMed53$sexe, labels=c("Homme","Femme"))
## # save(openMed53, file="openMed53.Rdata")


## ---- eval=F----------------------------------------------------------------
## load("openMed53.Rdata")
## evolution <- openMed53  %>%
##   group_by(age, ANNEE, sexe, PSP_SPE) %>%
##   summarise("boites"=sum(BOITES))
## # Avec data.table
## ev2 <- openMed53[ , .(Nb=sum(BOITES), Moy=mean(BOITES)), by=list(age, ANNEE, sexe, PSP_SPE)]


## ---- eval=F----------------------------------------------------------------
## evolution$ANNEE <- as.Date(as.character(evolution$ANNEE), "%Y")
## ggplot(data = evolution, aes(y=boites, x=ANNEE,color=age, fill=age)) + geom_line() + facet_grid(.~sexe*PSP_SPE)


## ----damir1-----------------------------------------------------------------
m1 <- fread("../openDamir/A202101.csv.gz", nrows=10)


## ----damir------------------------------------------------------------------
library(data.table)
#namesvar <- fread("A202101.csv",nrow=1)
#colnames(namesvar)
m1 <- fread("../openDamir/A202101.csv.gz", 
            select = c("AGE_BEN_SNDS", "BEN_RES_REG", "BEN_SEX_COD", 
                                         "ETE_CAT_SNDS", "ETE_REG_COD", "PRS_ACT_QTE", 
                                      "PRS_NAT", "SOI_ANN", "SOI_MOI"))


## ----hospi, echo=FALSE------------------------------------------------------
library(dplyr)
hosp <- m1 %>% filter(ETE_CAT_SNDS%in%c("1101","1102"))


## ----echo, echo=FALSE-------------------------------------------------------
hospEcho <- hosp %>% filter(PRS_NAT==1324, SOI_ANN==2021)


## ----compte, echo=FALSE-----------------------------------------------------
hospEcho %>% group_by(ETE_REG_COD) %>% summarise("nbActe"=sum(PRS_ACT_QTE))


## ---- eval=FALSE, echo=TRUE-------------------------------------------------
## hecho <- c()
## filepath <- c("../openDamir/A202101.csv.gz",
##               "../openDamir/A202102.csv.gz", "../openDamir/A202103.csv.gz")
## for (i in 1:3){
##   m <- fread(filepath[i], select = c("AGE_BEN_SNDS", "BEN_RES_REG", "BEN_SEX_COD",
##                                          "ETE_CAT_SNDS", "ETE_REG_COD", "PRS_ACT_QTE",
##                                      "PRS_NAT","SOI_ANN","SOI_MOI"))
##   hosp <- m %>% filter(ETE_CAT_SNDS%in%c("1101","1102"))
##   temp <- hosp %>% filter(PRS_NAT==1324, SOI_ANN==2021)
##   hecho <- rbind(hecho, temp)
## }
## hecho$SOI_MOI <- factor(hecho$SOI_MOI, labels=c("JAN", "FEV", "MAR"))
## hecho$BEN_SEX_COD <- factor(hecho$BEN_SEX_COD, labels=c("Hommes", "Femmes"))
## save(hecho, file="hecho.RData")


## ---------------------------------------------------------------------------
load("hecho.RData")


## ---------------------------------------------------------------------------
library(knitr)
library(tidyr)
restab1<- hecho %>% group_by(ETE_REG_COD, SOI_MOI) %>% summarise("NB"=sum(PRS_ACT_QTE))
#bonus
restab1 <- spread(restab1, SOI_MOI, NB)
colnames(restab1) <- c("Région", "Janvier","Février","Mars")
kable(restab1, caption="Nombre d'échographies par région lors du premier trimestre 2021")


## ---------------------------------------------------------------------------
hecho %>% group_by(ETE_REG_COD, SOI_MOI, BEN_SEX_COD) %>% summarise("NB"=sum(PRS_ACT_QTE))


## ---------------------------------------------------------------------------
library(ggplot2)
echoRegSexe <- hecho %>% group_by(ETE_REG_COD, SOI_MOI, BEN_SEX_COD) %>% summarise("NB"=sum(PRS_ACT_QTE))

ggplot(echoRegSexe, aes(y=NB, x=SOI_MOI, fill=BEN_SEX_COD)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(.~ETE_REG_COD) +
  ylab("echography volume")

#p <- ggplot(echoRegSexe, aes(y=NB, x=SOI_MOI, fill=BEN_SEX_COD)) + #geom_bar(stat="identity", position="dodge") 
#p +  facet_wrap(.~ETE_REG_COD) +ylab("echography volume")


## ----popreg-----------------------------------------------------------------
# lecture du fichier et calcul pour de la population par sexe et par région 
popreg<- read.csv("pop-reg.csv", header=T, sep=",")

genderpop <- popreg %>% filter(pop%in%c("Hommes", "Femmes")) %>% group_by(region, pop) %>% summarise("population"=sum(size))

## Prise en compte de l'outre-mer en global
genderpop$region <- factor(genderpop$region)
levels(genderpop$region)[1:5] <- "5"
genderpop <- genderpop %>% group_by(region, pop) %>% summarise("population"=sum(population))

# levels(genderpop) <- c("Hommes", "Femmes")
# jointure avec fichier de consommation
echoRegSexe$ETE_REG_COD <- as.factor(echoRegSexe$ETE_REG_COD)
echoRegSexe <- left_join(echoRegSexe, genderpop, by=c("ETE_REG_COD"="region", "BEN_SEX_COD"="pop"))
echoRate <- echoRegSexe %>% mutate(rate= NB/population*10000)

# éliminer la région 5 (outre-mer) non référencée dans le fichier pop
echoRate <- echoRate %>% filter(ETE_REG_COD!=5)

# par region
ggplot(echoRate, aes(y=rate, x=BEN_SEX_COD, fill=SOI_MOI)) + geom_bar(stat="identity", position="dodge") + facet_wrap(.~as.factor(ETE_REG_COD)) + ylab("rate per 10 000 inhabitants")

# par sexe et mois de soins
ggplot(echoRate, aes(y=rate, x=as.factor(ETE_REG_COD), fill=BEN_SEX_COD)) + geom_bar(stat="identity", position="dodge") + facet_wrap(.~BEN_SEX_COD+SOI_MOI) + ylab("rate per 10 000 inhabitants") + xlab("Régions")


## ---------------------------------------------------------------------------
library(sf)
library(mapsf)
#library(RColorBrewer)
# import ING shape files as an sf object
FrMap <- st_read(dsn="~/Documents/Projets/01_EHESP/04_Projets/00_BasesDonnees/ADMIN-EXPRESS-COG_1-1__SHP__FRA_2018-04-03/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2018-03-28/ADE-COG_1-1_SHP_LAMB93_FR/.",  layer="REGION", quiet = TRUE)

echoRate2 <- left_join(echoRate, FrMap, by=c("ETE_REG_COD"="INSEE_REG"))

sf <- st_set_geometry(echoRate2, echoRate2$geometry)
sfHJ <- sf %>% filter(BEN_SEX_COD=="Hommes", SOI_MOI=="JAN")
#-----------------

plot(st_geometry(sfHJ), col = NA, border = NA, bg = NA)
# main plot
mf_map(
  x = sfHJ,
  var = "rate",
  type = "choro",
  breaks = "equal",
  pal =  mf_get_pal(n = 5, pal = "viridis"),
  border = "grey40",
  lwd = 0.2,
  leg_pos = "right",
  leg_title = "",
  add = TRUE
)
# layout
mf_layout(title = "2021",
            credits = "Sources: SNIIRAM")


