library(lme4)
library(reshape2)
library(dplyr)
#Entrada de Dados
source("experimento/R/RII_prop.R")

tabela <- melt(rii.complete, id.vars = c("plot", "spnurse", "target"),
               variable.name = "tempo", 
               value.name = "folha")
str(tabela)
head(tabela)

##################### modelo linear (pode ser uma anova) com random effect sem p
#Com log likelyhood ratio test: compara a diferença da deviance ( na tabela)
#entre o modelo completo (rand) e os modelos sem variaveis (rand1, rand2, rand3) como se fosse seleção de modelos
#mas em cada tabela ele me dá o P da variável (nurse, target ou nuser:target) sepadados 


rand <- lmer(folha ~ spnurse*target + (1|tempo/plot), REML=F, data=tabela)
rand1 <- update(rand, ~. - spnurse:target) # tirando interação nurse target
rand2 <- update(rand, ~. - spnurse - spnurse:target )
rand3 <- update(rand, ~. - target - spnurse:target)
summary(rand)

anova(rand) 
anova(rand, rand1) #Loglikelyhood ratio test nurse:target
anova(rand, rand2)#Loglikelyhood ratio test nurse
anova(rand, rand3)#Loglikelyhood ratio test target

plot(rand)


