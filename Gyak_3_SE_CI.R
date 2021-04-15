#### Csomagok #####
library(tidyverse)
library(gmodels)
library(RcmdrMisc)
#### Adatbeolvasás:#####

mydata <- read.table(file = "falcon.csv", 
                     h = T,
                     sep = ",",
                     dec = ".")

######Intervallum becslések ###########


# SE fg. készítés mert nincs az r-ben

se <- function(vec){
  vec_sd <- sd(vec,
               na.rm = T)
  n <- length(vec)
  return(vec_sd/sqrt(n))
}

se(mydata$WingLength)
ci(mydata$WingLength)

# HF átlagra vonatkozó KI függvény konstruálása az előadásban vett kód alapján



#CI populációs arányra: p +- z_krit*sqrt(p(1-p)/n)

# függvényként (ezt később használni fogjuk a mintaelemszám becslésnél)
ci_prop <- function(p,n,alpha){
  ci_upper <- p+qnorm(1-(alpha/2))*sqrt(p*(1-p)/n)
  ci_lower <- p-qnorm(1-(alpha/2))*sqrt(p*(1-p)/n)
  return(c(ci_lower,ci_upper))
}

mydata %>% 
  group_by(Sex) %>% 
  tally

ci_prop(121/268,121+268,0.05)

# Jellemző ábrázolási módok:

plotMeans(response = mydata$BodyMass,
          factor1 = mydata$Sex,
          error.bars = "conf.int")


plotMeans(response = mydata$BodyMass,
          factor1 = mydata$Sex,
          factor2 = mydata$Area,
          error.bars = "conf.int")


# ggplot 

mydata %>% ggplot(aes(x=Sex,
                      y=BodyMass))+
  geom_boxplot(notch = T)

# notch szélesség: 1.58 * IQR / sqrt(n) 
# durva közelítése a KI-nek ezért csak óvatosan

# mean with errorbars plot
# kell egy táblázat amiben benne vannak a csoport átlagok,
# a CI alsó/felső és az SE alsó/felső
mydata %>% 
  group_by(Sex) %>% 
  summarise(Atlag = mean(BodyMass),
            f_CI = ci(BodyMass)[2],
            u_CI = ci(BodyMass)[3]) -> CI_summary

# errorbars
CI_summary %>% ggplot(aes(x = Sex,
                          y = Atlag,
                          colour= Sex)) +
  geom_errorbar(aes(ymin = u_CI,
                    ymax = f_CI,
                    width = 0.2)) +
  geom_point(size=3)+
  theme_bw()

# pointrange
CI_summary %>% ggplot(aes(x = Sex,
                          y = Atlag,
                          colour= Sex)) +
  geom_pointrange(aes(ymin = u_CI,
                      ymax = f_CI)) +
  geom_point(size=3)+
  theme_bw()

#Ivar és Év szerinti bontásban
mydata %>% 
  group_by(Sex,Year) %>% 
  summarise(Atlag = mean(BodyMass),
            f_CI = ci(BodyMass)[2],
            u_CI = ci(BodyMass)[3]) -> CI_summary_2

CI_summary_2 %>% ggplot(aes(x = Year,
                            y = Atlag,
                            colour= Sex)) +
  geom_errorbar(aes(ymin = u_CI,
                    ymax = f_CI,
                    width = 0.2)) +
  geom_point(size=3)+
  theme_bw()

# kicsit szebb ábra:
# 
CI_summary_2 %>% ggplot(aes(x = Year,
                            y = Atlag,
                            colour= Sex)) +
  geom_errorbar(aes(ymin = u_CI,
                    ymax = f_CI,
                    width = 0.2),
                position = position_dodge(0.2)) +
  geom_point(aes(shape=Sex),
             size=3,
             position = position_dodge(0.2))+
  geom_line(position = position_dodge(0.2))+
  theme_bw()
