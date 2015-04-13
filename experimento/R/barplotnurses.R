library(reshape2)
library(ggplot2)
library(dplyr)
source("experimento/R/RII_prop.R")

#Média dos tempos
rii.complete$medt <- rowSums(rii.complete[,-c(1:3)])/8
rii.complete$plot <-as.factor(rii.complete$plot)
str(rii.complete)

rii.tab <- summarise(group_by(rii.complete, espnurse, target),
          fol.nt.rii=mean(medt),
          fol.sd.rii=sd(medt),
          se.fol.rii=stderr(medt))

#Plotando facilitação para folhas por nurse, ordem das barras manual
  sci<- c("S.macr","A.colu", "C. glaz","P.moni", "P.gard",
          "E.numu", "P.marg", "L.ferr","B.chei", "P.stip",
          "C.lepr","C.viti","M.tenu","C.lept","C.blan", 
          "A.cear","C.hast","A.pyri", "S.tube","H.impe")

bar<-  ggplot(rii.tab,aes(y=fol.nt.rii,x=espnurse, fill=factor(target)))+
       scale_fill_grey(start = 0.3, end = .8, name="Target", labels=c("A.col","M.uru","P.gar"))+
       theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15),
             panel.background = element_rect(fill='white', colour='black'), 
             axis.text=element_text(colour="black"),
             legend.text=element_text(size=15))+
       ylab("Interaction Index [RII]")  +  xlab("Nurse species")+ 
         geom_bar(stat="identity",position="dodge")+
         geom_errorbar(aes(ymin=fol.nt.rii - se.fol.rii, ymax= fol.nt.rii+ se.fol.rii), width=.3,
         position=position_dodge(.9))+
    scale_x_discrete(limits=c ("burra", "angico","paubranco", "catanduva", "catingueira",
                               "favelinha","imbiratanha","juca","mororo","jubranca",
                               "mofumbo","algodao","jupreta","imburana","marmeleiro",
                               "cumaru","feijao","pereiro","umbu","ipe"),labels = sci)+
         scale_y_continuous(limits=c(-0.5,0.5))




        