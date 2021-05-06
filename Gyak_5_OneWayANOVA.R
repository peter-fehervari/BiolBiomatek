#### Liibraries ####

library(tidyverse)
library(car)
library(emmeans)
library(sjPlot)

#Adatbeolvasás

mydata <- read.table(file = "falcon.csv", 
                     h = T,
                     sep = ",",
                     dec = ".")
str(mydata)

mydata$Year <- factor(mydata$Year)

str(mydata)

# Egy utas ANOVA: WingLength ~ Year

mydata %>% 
  ggplot(aes(y=WingLength,
             x=Year))+
  geom_boxplot()

mydata %>% 
  group_by(Year) %>% 
  tally()

# 
mod1 <- lm(WingLength~Year,data=mydata)

class(mod1)
str(mod1)

plot(mod1)

par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

anova(mod1)

mod1_post <- emmeans(mod1, 
                     specs = pairwise~Year)
mod1_post

# two way ANOVA

with(mydata,RcmdrMisc::plotMeans(WingLength,Year,Sex))
with(mydata,interaction.plot(Year,Sex,WingLength))

mydata %>% 
  group_by(Year,Sex) %>% 
  tally()

mod2 <- lm(WingLength~Year+Sex,data=mydata)

par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))

anova(mod2)

mod1_post <- emmeans(mod2... # Itt folytatni


