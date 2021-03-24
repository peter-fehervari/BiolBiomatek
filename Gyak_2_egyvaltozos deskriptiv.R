
######### Deskriptív statisztika #########

#### Egy folytonos változó jellemzése #####
# Dataset: falcon.csv


## Csomagok 
library(tidyverse)
library(RcmdrMisc)

## Adatbeolvasás:

# base R megoldás példa

mydata <- read.table("falcon.csv", 
                     h=T,
                     sep=",")

class(mydata)
str(mydata)
# readR megoldás példa

mydata_tbl <- read_delim("falcon.csv", delim = ",")
class(mydata_tbl)
str(mydata_tbl)


#mindkettő generikus, van speciális esetük is read.csv, read_csv2 read_ csv, read_tsv. 
# bővebben itt : https://readr.tidyverse.org

## helyzeti mutatók

mydata %>% 
  summarise(atlag = mean(BodyMass),
            median = median(BodyMass))

# Módusz egy kicsit nehezebb eset mert nincs ilyen base függvény(!)
# kell csinálni egyet:

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Most már lehet használni
mydata %>% 
  summarise(atlag = mean(BodyMass),
            median = median(BodyMass),
            modusz = Mode(BodyMass))

# kvantiliseket is lehet

quantile(mydata$BodyMass)
quantile(mydata$BodyMass,probs = 0.25)


 mydata %>% 
  summarise(atlag = mean(BodyMass),
          median = median(BodyMass),
          modusz = Mode(BodyMass),
          elso_kvartilis = quantile(BodyMass,probs = 0.25)
)

 
# a ha nem kell a dplyr akkor a legegyszerubb megoldas talan ez:

numSummary(mydata$BodyMass)

## szóródási mutatók

mydata %>% 
  summarise(sd = sd(BodyMass),
            IQR = IQR(BodyMass),
            MAD = mad(BodyMass),
  )
# a terjedelem trükösebb:

range(mydata$BodyMass)

range(mydata$BodyMass)[2]

mydata %>% 
  summarise(sd = sd(BodyMass),
            IQR = IQR(BodyMass),
            MAD = mad(BodyMass),
            minimum = min(BodyMass),
            maximum = max(BodyMass)
  )
## Csoprtosítás
# dplyr előnye hogy az egyszerű bonyolult a bonyolult viszont egyszerűbb


mydata %>% 
  group_by(Sex) %>% 
  summarise(atlag = mean(BodyMass),
            median = median(BodyMass),
            modusz = Mode(BodyMass)
  )

# több csoport kombinációval
mydata %>% 
  group_by(Sex,Area) %>% 
  summarise(atlag = mean(BodyMass),
            median = median(BodyMass),
            modusz = Mode(BodyMass)
  ) 

# több csoport és több változóval:

mydata %>% 
  group_by(Sex,Area) %>% 
  summarise(across(c(BodyMass,WBone),
            list(atlag = mean,
                 median = median))) 

# részletesebben itt: https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-colwise/


## Ábrák egy változóra

# histogram
hist(mydata$BodyMass)

mydata %>% 
  ggplot(aes(x=BodyMass))+
  geom_histogram(fill="grey75")

hist(mydata$BodyMass,
     breaks = 30)

# simított hisztogram
plot(density(mydata$BodyMass))

mydata %>% 
  ggplot(aes(x=BodyMass))+
  geom_density(fill="grey75")

# a hisztogram és a simított hisztogram együtt

mydata %>% 
  ggplot(aes(x=BodyMass,
             y=..density..))+
  geom_histogram(fill="black")+
  geom_density(fill="grey75",
               alpha=0.7)


mydata %>% 
  ggplot(aes(x=BodyMass,
             y=..density..))+
  geom_histogram(bins=15,
                 fill="black")+
  geom_density(fill="grey75",
               alpha=0.7,
               adjust=0.5)

# qqplot
qqnorm(mydata$BodyMass)
qqline(mydata$BodyMass)

mydata %>% 
  ggplot(aes(sample=BodyMass))+
  geom_qq()+
  geom_qq_line()
             
#kombinálva is gyakran szokták használni

mydata %>% 
  ggplot(aes(sample=BodyMass))+
  geom_qq()+
  geom_qq_line() -> histo

mydata %>% 
  ggplot(aes(x=BodyMass,
             y=..density..))+
  geom_histogram(fill="black")+
  geom_density(fill="grey75",
               alpha=0.7)-> qq


library(patchwork)
histo+(qq+coord_flip())

# boxplot, violinplot

with(mydata, boxplot(BodyMass,
                     ylab="BodyMass"))

bplot <- with(mydata,boxplot.stats(BodyMass))

mydata %>% ggplot(aes(y=BodyMass))+
  geom_boxplot(notch = F)

mydata %>% ggplot(aes(y=BodyMass,
                      x=Sex))+
  geom_boxplot(notch = T)

#violinplot
mydata %>% ggplot(aes(y=BodyMass,
                      x=Sex))+
  geom_violin(fill="grey75")+
  geom_boxplot(notch = F,
               width = 0.1)

## származtatott változók készítése, transzformálás standardizálás studentizálás rang transzformáció

# xydollárvalami egyenlővel
# mutate vs transmutate
# scale
# rank
with(mydata,rank(BodyMass))

#### Egy kategoriális változó jellemzése #####
#Dataset: falcon.csv

# kontingencia tábla
# prop. table
# mosaicplot
