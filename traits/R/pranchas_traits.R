source("traits/R/Traits_caatinga.R")
head(traits)
summary(traits)
####################################################################################################
##############################                 Correlações                  ###########################
#########################r###########################################################################

all<-with(traits, cor(cbind(cap_arm_mad, dens_mad, efet_agua_mad,
                           efet_agua_casca, cap_arm_casca, H_arvore_m, 
                           diam_medio, thick_casca, cont_agua_mad, cont_agua_casca)))

g8t <-with(traits, cor.test(cont_agua_mad, dens_mad))
g8 <- ggplot(traits,aes(y=cont_agua_mad, x=dens_mad)) +
  ylab("Wood Water content [%]")  +  xlab("Wood Density [g/cm³]") + 
  theme(axis.title.y = element_text(size = rel(0.8), angle = 90), 
  axis.title.x = element_text(size = rel(0.8)))+
  geom_point()+
  geom_smooth(method=lm)

grt <-with(traits, cor.test(cont_agua_casca, dens_casca))
gr <- ggplot(traits,aes(y=cont_agua_casca, x=dens_casca)) +
  ylab("Bark Water content [%]")  +  xlab("Bark Density [g/cm³]") + 
  theme(axis.title.y = element_text(size = rel(0.8), angle = 90), 
  axis.title.x = element_text(size = rel(0.8)))+
  geom_point()+
  geom_smooth(method=lm)

g7t <- with(traits, cor.test(thick_casca, dens_mad))
g7 <- ggplot(traits,aes( y=thick_casca, x=dens_mad))  +
  ylab("Bark thickness [mm]") + xlab("Wood density [g/cm³]") +
  theme(axis.title.y = element_text(size = rel(0.8), angle = 90), 
  axis.title.x = element_text(size = rel(0.8)))+
  geom_point()+ 
  geom_smooth(method=lm)

gxt <-with(traits, cor.test(cont_agua_casca,thick_casca))
gx <- ggplot(traits,aes(y=thick_casca, x=cont_agua_casca)) +
  ylab("Bark thickness [mm]") + xlab("Wood Water Content [%]")  +
  theme(axis.title.y = element_text(size = rel(0.8), angle = 90), 
  axis.title.x = element_text(size = rel(0.8)))+
  geom_point()+
  geom_smooth(method=lm)

g9t <-with(traits, cor.test(cont_agua_casca, dens_mad))
g9 <- ggplot(traits,aes(y=cont_agua_casca, x=dens_mad))+
  ylab("Bark water content [%]")  + xlab("Wood Density [g/cm³] ") +
  theme(axis.title.y = element_text(size = rel(0.8), angle = 90), 
  axis.title.x = element_text(size = rel(0.8)))+
  geom_point()+
  geom_smooth(method=lm)

g10t <-with(traits, cor.test(cont_agua_casca, cont_agua_mad))
g10 <- ggplot(traits,aes(y=cont_agua_casca, x=cont_agua_mad))  +
  ylab("Bark water content [%]") + xlab("Wood Water Content [%]") +
  theme(axis.title.y = element_text(size = rel(0.8), angle = 90), 
  axis.title.x = element_text(size = rel(0.8)))+
  geom_point()+
  geom_smooth(method=lm)


sheetcor <- grid.arrange(g8,gr, g7,gx,g9,g10, ncol=2)

####################################################################################################
#######################################      Gráficos     ##########################################
####################################################################################################
g3 <- ggplot(traits,aes(y=dens_mad, x=especie))+
  xlab("") + ylab("Wood Density [g/cm³]") +
  theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.6), angle = 90))+
  guides(fill=FALSE)+
  geom_boxplot(aes(fill=factor(traits$especie)))
  #g3 <- g3 + theme(plot.margin = unit(c(1,1,-1, 1),"cm"))

g6 <- ggplot(traits,aes(y=dens_casca, x= especie))+
  xlab("") + ylab("Bark Density [g/cm³]") +
  theme(axis.text.x = element_blank(),
  axis.title.y = element_text(size = rel(0.6), angle = 90))+
  guides(fill=FALSE)+
  geom_boxplot(aes(fill=factor(traits$especie)))
  #g6 <- g6 + theme(plot.margin = unit(c(1,1,-1, 1),"cm"))

g2 <- ggplot(traits,aes(y=cont_agua_mad, x= especie))+
   ylab("Wood Water content [%]") + xlab("") +
  theme(axis.text.x = element_blank(),
  axis.title.y = element_text(size = rel(0.6), angle = 90))+
  guides(fill=FALSE)+
  geom_boxplot(aes(fill=factor(traits$especie)))
  #g2 <- g2 + theme(plot.margin = unit(c(1,1,-1, 1),"cm"))

g5 <- ggplot(traits,aes(y=cont_agua_casca, x=especie))+
   ylab("Bark Water Content[%]") + xlab("Specie")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10),
  axis.title.y = element_text(size = rel(0.6), angle = 90), 
  axis.title.x = element_text(size = rel(1.0)))+
  guides(fill=FALSE)+
  geom_boxplot(aes(fill=factor(traits$spp)))
  #g5 <- g5 + theme(plot.margin = unit(c(1,1,-1, 1),"cm"))

sheetmad <- grid.arrange(g3,g6, g2,g5,g1,g4, ncol=3)

g1 <- ggplot(traits,aes(  y=cap_arm_mad, x= especie))+
  ylab("Wood Storage Capacity[%]") + xlab ("")+
  theme(axis.text.x = element_blank(),
  axis.title.y = element_text(size = rel(0.6), angle = 90))+
  guides(fill=FALSE)
+  geom_boxplot(aes(fill=factor(traits$especie)))
  #g1 <- g1 + theme(plot.margin = unit(c(1,1,-1, 1),"cm"))

g4 <- ggplot(traits,aes(y=cap_arm_casca, x=especie))+
  ylab("Bark Storage Capacity [%]") + xlab("") + 
  theme(axis.text.x = element_blank(),
  axis.title.y = element_text(size = rel(0.6), angle = 90))+
  guides(fill=FALSE)+
  geom_boxplot(aes(fill=factor(traits$especie)))
  #g4 <- g4 + theme(plot.margin = unit(c(1,1,-1, 1),"cm"))

g13 <- ggplot(traits,aes(y=diam_medio, x=especie))+
  xlab("") + ylab("Canopy diameter [m]") +
  theme(axis.text.x = element_blank(),
  axis.title.y = element_text(size = rel(0.6), angle = 90))+
  guides(fill=FALSE)+
  geom_boxplot(aes(fill=factor(traits$especie)))
  #g13 <- g13 + theme(plot.margin = unit(c(1,1,-1, 1),"cm"))

g14 <- ggplot(traits,aes(y=H_arvore_m, x=especie))+
  xlab("Species") + ylab("Tree Height [m]") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10),
  axis.title.y = element_text(size = rel(0.6), angle = 90), 
  axis.title.x = element_text(size = rel(1.0)))+
  guides(fill=FALSE)+
  geom_boxplot(aes(fill=factor(traits$especie)))
  #g14 <- g14 + theme(plot.margin = unit(c(1,1,-1, 1),"cm"))

sheetcanopy <- grid.arrange(g13, g14, ncol=1)

####################################################################################################
#######################################      histogramas    ##########################################
####################################################################################################
media <- traits.mean

dm  <- ggplot(media,aes (x= Mdens_mad))+
  xlab("Wood density [g/cm³]") + ylab("Frequency") +
  scale_x_continuous(breaks=seq(0.2, 1, 0.1))+
  geom_histogram(colour = "black", fill = "grey", binwidth = 0.05)

dc  <- ggplot(media,aes (x= Mdens_casca))+
  xlab("Bark density [g/cm³]") + ylab("Frequency") +
  scale_x_continuous(breaks=seq(0.2, 1, 0.1))+
  geom_histogram(colour = "black", fill = "grey", binwidth = 0.05)
  
cam <- ggplot(media,aes (x= Mcap_arm_mad))+
  xlab("Wood storage capacity") + ylab("Frequency") +
  scale_x_continuous(breaks=seq(0.2, 2.5, 0.5))+
  geom_histogram(colour = "black", fill = "grey", binwidth = 0.08) 

cac <- ggplot(media,aes (x= Mcap_arm_casca))+
  xlab("Bark storage capacity") + ylab("Frequency") +
  scale_x_continuous(breaks=seq(1, 3.5, 0.5))+
  geom_histogram(colour = "black", fill = "grey", binwidth = 0.08) 

coam<- ggplot(media,aes (x= Mcont_agua_mad))+
  xlab("Wood water content") + ylab("Frequency") +
  scale_x_continuous(breaks=seq(0.2, 2, 0.5))+
  geom_histogram(colour = "black", fill = "grey", binwidth = 0.08) 
  
coac<- ggplot(media,aes (x= Mcont_agua_casca))+
  xlab("Bark water content") + ylab("Frequency") +
  scale_x_continuous(breaks=seq(0.2, 3, 0.5))+
  geom_histogram(colour = "black", fill = "grey", binwidth = 0.08)
 
tc  <-  ggplot(media,aes (x= Mthick_casca))+
  xlab("Bark thickness") + ylab("Frequency") +
  scale_x_continuous(breaks=seq(0.2, 4.5, 0.8))+
  geom_histogram(colour = "black", fill = "grey", binwidth = 0.1)
  
sheethist<- grid.arrange (dm,dc,cam,cac,coam,coac,tc, ncol=2)
