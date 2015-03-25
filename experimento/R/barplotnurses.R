library(reshape2)
library(ggplot2)
library(dplyr)

source("experimento/R/rii.R")

#Plotando facilitação para folhas por nurse

ggplot(rii_mean,aes(y=fol.rii.m,x=espnurse,fill=factor(target)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))+
geom_bar(stat="identity",position="dodge")


#Plotando facilitação para altura por nurse

ggplot(rii_mean,aes(y=alt.rii.m,x=espnurse,fill=factor(target)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))+
  geom_bar(stat="identity",position="dodge")

#Plotando facilitação para sobrevivencia

ggplot(rii_mean,aes(y=s.rii.m,x=espnurse,fill=factor(target)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))+
  geom_bar(stat="identity",position="dodge")
