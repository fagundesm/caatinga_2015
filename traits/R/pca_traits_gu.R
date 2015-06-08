library(reshape2)
source("traits/R/Traits_caatinga.R")
traits$H_arvore_m <- as.numeric(traits$H_arvore_m)
traits <- arrange(traits,spnurse,plot)
head(traits)
str(traits)
source("experimento/R/RII_prop.R")
#Média dos tempos
rii.complete$medt <- rowSums(rii.complete[,-c(1:3)])/8
rii.complete$plot <-as.factor(rii.complete$plot)
riimed <- rii.complete[,-c(4:11)] # tirando as colunas dos tempos e deixando só a média
dcasting <- dcast(riimed, plot + spnurse  ~ target, value.var= "medt") # arrumando a tabela pra poder fazer o pca
#sorting by spnuse pra ficar igual a tabela de traits
rii2 <- arrange(dcasting,spnurse)
rii <- rii2[c(1,4,5,7,9,10,13,14,15,17,19,20,21,22,24,27,28,30,31,34,35,36,38,40,41,42,43,46,48,49,51,54,55,
              57,58,59,61,62,64,66,67,69,72,73,74,76,77,79,81,82,84,86,88,89,93,94,95,96,97,99),]

tabela <- cbind(rii,traits[,-c(1:2)])

########################## PCA ##########################
reg2 <- select(tabela,spnurse, H_arvore_m , H_copa_m , diam_medio , dens_mad, 
                 cap_arm_mad, cont_agua_mad ,cont_agua_casca , thick_casca)
log.var <- log(reg2[,-1])
var.pca <- prcomp(log.var,center=T, sacale.=T)
print(var.pca)
plot(var.pca, type="l")
summary(var.pca)
biplot(var.pca)
str(var.pca)
pca<- var.pca$x[,1:4] # separando os eixos de pca 

##################### REGRESSÃO MULTIPLA COM TRAITS E TARGETS ##################
reg3 <- cbind(reg2,pca,rii[,3:5])
head(reg3)

pcaangico <- aov( angico ~ PC1 + PC2 + PC3 + Error(spnurse), reg3)
summary(pcaangico)

pcaCat <- aov( cating ~PC1 + PC2 + PC3 + Error(spnurse), reg3)
summary(pcaCat)

pcaaro <- aov( aroeira ~PC1 + PC2 + PC3 + Error(spnurse), reg3)
summary(pcaaro)
