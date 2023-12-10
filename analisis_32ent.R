library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)

pobreza <-read.csv("pobreza_buena.csv", header=TRUE)

str(pobreza)

ggplot(pobreza, aes(x=ent, fill=sexo)) + 
  theme_bw() +
  geom_bar() +
  labs( y="Frecuencia",
        x="Entidades federativas",
        title="Cantidad de personas por entidad federativa")

pobreza$sexo <- as.factor(pobreza$sexo)
pobreza$ent <- as.factor(pobreza$ent)

class(pobreza$sexo)
class(pobreza$ent)

ggplot(pobreza, aes(x=ent, fill=sexo)) + 
  geom_bar() +
  labs( y="Numero de personas",
        x="Entidad federativa",
        title="Genero de persona por entidad")

ggplot(pobreza, aes(x=sexo, fill=ent)) +
  theme_bw() +
  geom_bar() +
  labs( y="Numero de personas",
        title="Genero de persona por entidad")+ 
  facet_wrap(~ent)

ggplot(pobreza, aes(x=edad, fill=sexo))+
  geom_histogram(bins=30, colour="#1380A1") +
  labs(title="Survival Rate by Gender",
       y="Number of passengers",
       subtitle = "Distribution by age, gender and ticket class",
       caption="Author: Hnin")+
  theme_bw() +
  facet_grid(sexo~ent, scales="free")
