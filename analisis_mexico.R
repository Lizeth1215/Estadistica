library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)

pobrezaQ <- read.csv("pobrezaQ.csv", header=TRUE)

str(pobrezaQ)
## NO TIENE SENTIDO ESTA GRÁFICA
ggplot(pobrezaQ, aes(x=ent, fil=sexo)) + 
  theme_bw() +
  geom_bar() +
  labs( y="Frecuencia",
        x="Entidades federativas",
        title="Equisde")

pobrezaQ$sexo <- as.factor(pobrezaQ$sexo)
pobrezaQ$ent <- as.factor(pobrezaQ$ent)

class(pobrezaQ$sexo)
class(pobrezaQ$ent)

## NO TIENE SENTIDO ESTA GRÁFICA
ggplot(pobrezaQ, aes(x=ent, fill=sexo)) + 
  geom_bar() +
  labs( y="Numero de personas",
        x="Entidad federativa",
        title="Genero de persona por entidad")


ggplot(titanic, aes(x=Sex, fill=Survived)) +
  theme_bw() +
  geom_bar() +
  labs( y="Number of Passengers",
        title="Titanic Survival Rate by Gender by Passenger Class")+ 
  facet_wrap(~Pclass)

ggplot(pobrezaQ, aes(x=edad, fill=sexo))+
  geom_histogram(bins=30, colour="#1380A1") +
  labs(title="Survival Rate by Gender",
       y="Number of passengers",
       subtitle = "Distribution by age, gender and ticket class",
       caption="Author: Hnin")+
  theme_bw() +
  facet_grid(sexo~ent, scales="free")

ggplot(pobrezaQ, aes(x=edad, fill=no_pobv))+
  geom_histogram(bins=30, colour="#1380A1") +
  labs(title="Survival Rate by Gender",
       y="Number of passengers",
       subtitle = "Distribution by age, gender and ticket class",
       caption="Author: Hnin")+
  theme_bw() +
  facet_grid(sexo~ent, scales="free")

boxplot(pobreza)
