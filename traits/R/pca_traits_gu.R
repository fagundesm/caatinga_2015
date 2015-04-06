
reg2 <- select(reg,especie, H_arvore_m , H_copa_m , diam_medio , dens_mad, 
                 cap_arm_mad, cont_agua_mad ,cont_agua_casca , thick_casca)
log.var <- log(reg2[,-1])

var.pca <- prcomp(log.var,center=T, sacale.=T)

print(var.pca)
plot(var.pca, type="l")
summary(var.pca)
biplot(var.pca)

str(var.pca)
pca<- var.pca$x[,1:4]
reg3 <- cbind(reg2,pca,reg[,29:31])
head(reg3)

pcaangico <- aov( angico~PC1 + PC2 + PC3 + Error(especie), reg3)
summary(pcaangico)

pcaCat <- aov( cating ~PC1 + PC2 + PC3 + Error(especie), reg3)
summary(pcaCat)

pcaaro <- aov( aroeira ~PC1 + PC2 + PC3 + Error(especie), reg3)
summary(pcaaro)
