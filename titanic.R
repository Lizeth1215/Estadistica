library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)

titanic <-read.csv("titanic.csv", header=TRUE)

str(titanic)

ggplot(titanic, aes(x=Pclass, fill=Survived)) + 
  theme_bw() +
  geom_bar() +
  labs( y="Number of Passengers",
        x="Passenger Class",
        title="Titanic Survival Rate by Passenger Class")

titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
class(titanic$Survived)
class(titanic$Pclass)

ggplot(titanic, aes(x=Pclass, fill=Survived)) + 
  geom_bar() +
  labs( y="Number of Passengers",
        x="Ticket Class",
        title="Titanic Survival Rate by Passenger Class")

ggplot(titanic, aes(x=Sex, fill=Survived)) +
  theme_bw() +
  geom_bar() +
  labs( y="Number of Passengers",
        title="Titanic Survival Rate by Gender by Passenger Class")+ 
  facet_wrap(~Pclass)

ggplot(titanic, aes(x=Age, fill=Survived))+
  geom_histogram(bins=20, colour="#1380A1") +
  labs(title="Survival Rate by Gender", y="Number of passengers", subtitle = "Distribution by age, gender and ticket class", caption="Author: Hnin")+
  theme_bw() +
  facet_grid(Sex~Pclass, scales="free")
