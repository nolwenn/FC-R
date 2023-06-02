library(tidyr)
library(dplyr)
library(readxl)
dat<- read_excel("exemple PMSI HAD Thomas FAUCHIER.xlsx")

colnames(dat)[3:11] <- paste("D", 1:9, sep="")

datlong <- gather(dat[,-2], key="niveau diag", value="diag",
            D1,D2,D3,D4,D5,D6,D7,D8,D9)
datlong <- datlong[!is.na(datlong$diag), ]

datcount <- datlong %>% group_by(`Secteur géographique`) %>% count(diag)

#------------------------------------------
library("wordcloud")
set.seed(256)
for(i in unique(datcount$`Secteur géographique`)){
  id <- which(datcount$`Secteur géographique`==i)
  wordcloud(words = datcount$diag[id], freq = datcount$n[id], min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}


#------------------------------------------
# library
library(treemap)
#library(d3treeR)

# Build Dataset
datcount$family <- substr(datcount$diag, 1,1)
datcount$group <- substr(datcount$diag, 1,2)
datcount$subgroup <- substr(datcount$diag, 1,3)

# treemap
for(i in unique(datcount$`Secteur géographique`)){
  id <- which(datcount$`Secteur géographique`==i)
  treemap(datcount[id,],
        index=c("family","subgroup"),
        vSize="n",
        type="index"
 ) 
 # inter <- d3tree2( p ,  rootname = "General" )
}
#------------------------------------------

library(gtsummary)
tbl_summary(datcount[, c(1,4)], 
            by=`Secteur géographique`)

#------------------------------------------

library(ggplot2)
datcount2 <- datcount %>% group_by(`Secteur géographique`, family) %>% summarise(n= sum(n))
ggplot(datcount2, aes(y=n, x=`Secteur géographique`, fill=family)) + geom_bar(stat="identity")
