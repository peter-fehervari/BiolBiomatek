
######### Deskriptív statisztika #########

#### Egy folytonos változó jellemzése #####
# Dataset: falcon.csv

## Csomagok 
library(tidyverse)
library(RcmdrMisc)

## Adatbeolvasás:

# base R megoldás példa

mydata <- read.table(file = "falcon.csv", 
                     h = T,
                     sep = ",",
                     dec = ".")

class(mydata)
str(mydata)

# readR megoldás példa

mydata_tbl <- read_delim("falcon.csv", delim = ",")
class(mydata_tbl)
str(mydata_tbl)

mydata
mydata_tbl

# mindkettő generikus, van speciális esetük is read.csv, read_csv2 read_ csv, read_tsv. 
# bővebben itt : https://readr.tidyverse.org

## helyzeti mutatók
mean(mydata$BodyMass)
median(mydata$BodyMass)

mydata %>% 
  summarise(atlag = mean(BodyMass),
            median = median(BodyMass))

# Módusz egy kicsit nehezebb eset mert nincs ilyen base függvény(!)
# kell csinálni egyet:

mean(mydata$BodyMass)

my_function <- function(x){
  sd(x)/sqrt(length(x))
}

my_function(mydata$BodyMass)

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

quantile(mydata$BodyMass,probs = c(0.1,0.25,0.3))


mydata %>% 
  summarise(atlag = mean(BodyMass),
            median = median(BodyMass),
            modusz = Mode(BodyMass),
            elso_kvartilis = quantile(BodyMass,probs = 0.25)
  )


# a ha nem kell a dplyr akkor a legegyszerubb megoldas talan ez:

numSummary(mydata$BodyMass)

numSummary(mydata$BodyMass,groups = mydata$Sex)

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

### H.F. Megcsinálni [] hivatkozásokkal

# több csoport kombinációval
mydata %>% 
  group_by(Area,Sex) %>% 
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

summary(mydata)

mydata$Sex_fact <- factor(mydata$Sex)

summary(mydata)

# részletesebben itt: https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-colwise/

## Ábrák egy változóra

# stripchart

stripchart(mydata$BodyMass)

stripchart(mydata$BodyMass,
           pch = 20)

stripchart(mydata$BodyMass,
           method = "stack",
           pch = 1)

stripchart(mydata$BodyMass~mydata$Sex,
           method = "jitter",
           pch = 1,
           xlab="Tomeg")

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
  geom_qq_line()+
  scale_y_continuous(limits = c(110,210))-> histo

mydata %>% 
  ggplot(aes(x=BodyMass,
             y=..density..))+
  geom_histogram(fill="black")+
  geom_density(fill="grey75",
               alpha=0.7)+
  scale_x_continuous(limits = c(110,210))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) -> qq


library(patchwork)
histo+(qq+coord_flip())

# boxplot, violinplot

with(mydata, boxplot(BodyMass,
                     ylab="BodyMass"))

boxplot(mydata$BodyMass,
        ylab="BodyMass")

bplot2 <- boxplot(mydata$BodyMass,
                  ylab="BodyMass")


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

## származtatott változók készítése

# BMI vércse módra

mydata$BMI <- mydata$BodyMass/mydata$WingLength^2

# Testtömeg-testtömeg átlaga

mydata$valami <- mydata$BodyMass-mean(mydata$BodyMass)

hist(mydata$valami)
hist(mydata$BodyMass)

par(mfrow=c(2,1))
hist(mydata$valami)
hist(mydata$BodyMass)
par(mfrow=c(1,1))


mydata %>% mutate(BMI = BodyMass/WingLength^2) -> mydata

## transzformálás 

hist(mydata$WingLength)

mydata$LogWingLength <- log(mydata$WingLength)
mydata$sqrtWingLength <- sqrt(mydata$WingLength)

hist(mydata$LogWingLength)
hist(mydata$sqrtWingLength)

mydata %>% 
  mutate(LogWingLength = log(WingLength)) -> mydata

## standardizálás studentizálás centrálás

mydata$M_Center_BodyMass <- scale(mydata$BodyMass, scale = FALSE)
mydata$Student_BodyMass <- scale(mydata$BodyMass, scale = TRUE)

mydata %>% 
  ggplot(aes(x = M_Center_BodyMass))+
  geom_density()

mydata %>% 
  ggplot(aes(x = Student_BodyMass))+
  geom_density()

## rang transzformáció

mydata$Rank_BodyMass <- rank(mydata$BodyMass)

#### Egy kategoriális változó jellemzése #####
#Dataset: falcon.csv

# kontingencia tábla
table(mydata$Sex)
table(mydata$Sex,mydata$Area)

#HF hogyan kell dplyr-el

# kontingencia tábla

prop.table(table(mydata$Sex))

round(prop.table(table(mydata$Sex)),2)

prop.table(table(mydata$Sex,mydata$Area), margin = 1)
prop.table(table(mydata$Sex,mydata$Area), margin = 2)
prop.table(table(mydata$Sex,mydata$Area), margin = c(1,2))

#HF hogyan kell dplyr-el??


##Ábrázolás

#kör diagram
pie(table(mydata$Sex))
pie(table(mydata$Area))
pie(table(mydata$Colony))

#oszlopdiagram
barplot(table(mydata$Sex))

mosaicplot(table(mydata$Sex,mydata$Area))
