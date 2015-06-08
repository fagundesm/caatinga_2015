library(reshape2)
library(ggplot2)
library(dplyr)
source("experimento/R/RII_prop.R")

#Plotando facilitação para folhas por nurse, ordem das barras manual
sci2<- c("C.vit","C.vit","C.vit","A.col","A.col","A.col","S.mac","S.mac","S.mac",
        "P.mon","P.mon","P.mon","P.gar","P.gar","P.gar","A.cea","A.cea","A.cea",
         "E.num","E.num","E.num","C.has","C.has","C.has","P.mar","P.mar","P.mar",
         "C.lept","C.lept","C.lept","H.imp","H.imp","H.imp","P.sti","P.sti","P.sti",
         "L.fer","L.fer","L.fer","M.ten","M.ten","M.ten","C.bla","C.bla","C.bla",
         "C.lepr","C.lepr","C.lepr","B.che","B.che","B.che","C.glaz","C.glaz","C.glaz",
         "A.pyr","A.pyr","A.pyr","S.tub","S.tub","S.tub")
rii.tab$abrev<-sci2

bar<-  ggplot(rii.tab,aes(y=fol.nt.rii,x=abrev, fill=factor(target)))+
       scale_fill_grey(start = 0.3, end = .8, name="Target", labels=c("A.col","M.uru","P.gar"))+
       theme(axis.text.x = element_text(angle = 90, hjust = 1, size=15),
             axis.text.y=element_text(size=15), 
             axis.title=element_text(size=20), 
             panel.background = element_rect(fill='white', colour='black'), 
             axis.text=element_text(colour="black"),
             legend.text=element_text(size=15))+
       ylab("Proportion of leaves [RII]")  +  xlab("Nurse species")+ 
         geom_bar(stat="identity",position="dodge")+
         geom_errorbar(aes(ymin=fol.nt.rii - se.fol.rii, ymax= fol.nt.rii+ se.fol.rii), width=.3,
         position=position_dodge(.9))+
   # guides(fill=FALSE)+
    #scale_x_discrete(limits=c ("burra", "angico","paubranco", "catanduva", "catingueira",
      #                         "favelinha","imbiratanha","juca","mororo","jubranca",
       #                        "mofumbo","algodao","jupreta","imburana","marmeleiro",
        #                       "cumaru","feijao","pereiro","umbu","ipe"),labels = sci)+
         scale_y_continuous(limits=c(-0.5,0.5))




        