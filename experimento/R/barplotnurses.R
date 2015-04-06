library(reshape2)
library(ggplot2)
library(dplyr)

source("experimento/R/rii.R")

#Plotando facilitação para folhas por nurse

sci<- c("C.vit", "A.col","S.mac", "P.mon", "P.gar", "A.cea", "E.num", "C.has", "P.mar", "C.lep",
          "H.imp","P.sti","L.fer","M.ten","C.bla", "C.lep","B.che","C. glaz", "A.pyr", "S.tub")

  ggplot(fol.rii.mean,aes(y=fol.rii.m,x=espnurse,fill=factor(target)))+
    scale_fill_grey(start = 0.3, end = .8, name="Target", labels=c("A.col","M.uru","P.gar"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))+
    ylab("Interaction Index [RII]")  +  xlab("Nurse species")+ 
    geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=fol.rii.m- se.fol.rii, ymax=fol.rii.m+ se.fol.rii), width=.3,
                position=position_dodge(.9))+
    scale_x_discrete(labels = sci)+
  scale_y_continuous(limits=c(-1,1))+
      theme(panel.background = element_rect(fill='white', colour='black'), 
            axis.text=element_text(colour="black"),
            legend.text=element_text(size=15))
            
        

#Plotando facilitação para altura por nurse

ggplot(rii_mean,aes(y=alt.rii.m,x=espnurse,fill=factor(target)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))+
  geom_bar(stat="identity",position="dodge")

#Plotando facilitação para sobrevivencia

ggplot(rii_mean,aes(y=s.rii.m,x=espnurse,fill=factor(target)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15))+
  geom_bar(stat="identity",position="dodge")

