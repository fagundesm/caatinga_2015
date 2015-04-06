library(lme4)

#Entrada de Dados
source("experimento/R/rii.R")

#separando somente as folhas
tab<- rii_times[,1:11]

#Reshapando a tabela para poder fazer a análise : )
tabela <- melt(tab, id.vars = c("plot", "espnurse", "target"),
               variable.name = "tempo", 
               value.name = "folha")
str(tabela)
head(tabela)
##################### modelo linear (pode ser uma anova) com random effect sem p
#Com log likelyhood ratio test: compara a diferença da deviance ( na tabela)
#entre o modelo completo (rand) e os modelos sem variaveis (rand1, rand2, rand3) como se fosse seleção de modelos
#mas em cada tabela ele me dá o P da variável (nurse, target ou nuser:target) sepadados 


rand <- lmer(folha ~ espnurse*target + (1|tempo/plot), REML=F, data=tabela)
rand1 <- update(rand, ~. - espnurse:target) # tirando interação nurse target
rand2 <- update(rand, ~. - espnurse - espnurse:target )
rand3 <- update(rand, ~. - target - espnurse:target)
summary(rand)

anova(rand) 
anova(rand,rand1) #Loglikelyhood ratio test nurse:target
anova(rand, rand2)#Loglikelyhood ratio test nurse
anova(rand, rand3)#Loglikelyhood ratio test target

plot(rand)


