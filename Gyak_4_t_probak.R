

## Csomagok 
library(tidyverse)
library(RcmdrMisc)
library(patchwork)
library(car)
## Adatbeolvasás:

mydata <- read.table(file = "falcon.csv", 
                     h = T,
                     sep = ",",
                     dec = ".")

class(mydata)
str(mydata)

####### Egy mintás t-próba #########

# Kazakhsztánban a 18-20 napos fiókák átlag szárnyhossza = 142.3 mm
# Eltére-e ez a kardoskúti vércsékétől? 
# Tartható-e az az álláspont, hogy a két populáció várható értéke ugyanaz?
# 1. lépés deskriptív és exploratív hogy értsük mit csinálunk.

numSummary(mydata$WingLength)

# A qqplot kombinációs ábra
mydata %>% 
  ggplot(aes(sample=WingLength))+
  geom_qq()+
  geom_qq_line()-> histo

mydata %>% 
  ggplot(aes(x=BodyMass,
             y=..density..))+
  geom_histogram(fill="black")+
  geom_density(fill="grey75",
               alpha=0.7)+
#  scale_x_continuous(limits = c(110,175))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) -> qq

histo+(qq+coord_flip())

# Mi legyen a null hipotézis, és az ellen hipotézis?
# mik a t-próba feltételei?
# Teljesülnek itt?


t.test(mydata$WingLength,
       mu = 142.3)

str(mydata)

# FELADAT:
# Tartható-e az az álláspont, 
#hogy a felkarcsont hosszának várható értéke 60.5 mm?


######### független mintás t-próba #######

# Eltér-e a hímek és a tojók szárnyhossz várható értéke?

# D&E

numSummary(mydata$WingLength,
           groups = mydata$Sex)

# Violin plotok, gyak_2 ből
mydata %>% ggplot(aes(y=WingLength,
                      x=Sex))+
  geom_violin(fill="grey75")+
  geom_boxplot(notch = F,
               width = 0.1)

# # A qqplot kombinációs ábra ivaronként!
mydata %>% 
  ggplot(aes(sample = WingLength,
             color = Sex))+
  geom_qq()+
  geom_qq_line()-> histo
mydata %>% 
  ggplot(aes(x=WingLength,
             y=..density..,
             fill = Sex))+
  geom_histogram()+
  geom_density(alpha=0.7)+
  #  scale_x_continuous(limits = c(110,175))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) -> qq

histo+(qq+coord_flip())+plot_layout(guides = "collect")

# két mintás t-próba feltételei teljesülnek?

t.test(mydata$WingLength~mydata$Sex)

### Eltér-e a hímek és tojók szárnyhosszának szórása?

# milyen teszteket ismertek?
# mik a feltételeik?

var.test(mydata$WingLength~mydata$Sex)

leveneTest(mydata$WingLength~mydata$Sex)


##### Páros t-próba #####

# Eltér-e a lilék jobb és bal csűd hosszának várható értéke?
# Fluktuáló asszimetria

lile <- read.table("lile.csv",
                   h = T,
                   sep = ",")

str(lile)

# E&D
plot(density(lile$J_CSUD))
points(density(lile$B_CSUD),
       type = "l",
       col = "tomato")

# Páros t-próba feltételei?

lile$Csud_kul <- lile$J_CSUD-lile$B_CSUD

numSummary(lile$Csud_kul)

hist(lile$Csud_kul)

# A két teszt indentikus
t.test(lile$Csud_kul,mu=0)

t.test(lile$J_CSUD,lile$B_CSUD, 
       paired = T)
