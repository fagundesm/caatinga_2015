library(reshape2)
library (dplyr)
###########################################RII CALCULADO COM DADOS BRUTOS ##############################
                                              # NÃO ESTAMOS TRABALHANDO #

stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

brutos <- read.csv("experimento/dados_brutos_experimento.csv", h=T )
brutos$plot <- as.factor(brutos$plot)
# Somando 1 em todas as células (celulas com valor -1 passam a ter valor 0 - plântula morta)
brutos[,-c(1:5)] <- brutos[,-c(1:5)] + 1  # Somando + 1 em todas as células
range(brutos[,-c(1:3)],na.rm=T)        


                                         ###### Indice Armas, 2004 #####
#Folhas
fp <- (brutos$NFP-brutos$FP)/(brutos$NFP+brutos$FP)
f1 <- (brutos$NF1-brutos$F1)/(brutos$NF1+brutos$F1)
f2 <- (brutos$NF2-brutos$F2)/(brutos$NF2+brutos$F2)
f3 <- (brutos$NF3-brutos$F3)/(brutos$NF3+brutos$F3)
f4 <- (brutos$NF4-brutos$F4)/(brutos$NF4+brutos$F4)
f6 <- (brutos$NF6-brutos$F6)/(brutos$NF6+brutos$F6) #problema
f7 <- (brutos$NF7-brutos$F7)/(brutos$NF7+brutos$F7)
f8 <- (brutos$NF8-brutos$F8)/(brutos$NF8+brutos$F8)
f9 <- (brutos$NF9-brutos$F9)/(brutos$NF9+brutos$F9)

### Matriz com os indices para cada tempo para folhas:
fol.rii <- data.frame(f1,f2,f3,f4,f6,f7,f8,f9)
fol.rii[fol.rii=="NaN"] <- 0

   #adicionando variaveis categoricas para poder usar o reshape
   fol_long <- data.frame(brutos[,c(1:3)], fol.rii)

   #usando o reshape para poder calcular as médias no pŕoximo comando
   fol_wide <- melt(fol_long, id.vars = c("plot", "espnurse", "target"),
                 variable.name = "tempo", value.name= "RII")

#Média dos tempos
fol_medt <- summarise(group_by(fol_wide, plot, espnurse,target),
                     fol.mt.rii=mean(RII),
                     fol.sd.rii=sd(RII),
                     se.folmed=stderr(RII))

#Média por nurse/target
fol.rii.mean <- summarise(group_by(fol_medt, espnurse,target),
                      fol.rii.m=mean(fol.mt.rii),
                      fol.rii.sd=sd(fol.mt.rii),
                     se.fol.rii =stderr(fol.mt.rii))

#################################################|*|###########################################

#altura
hp <- (brutos$NHP-brutos$HP)/(brutos$NHP+brutos$HP)
h1 <- (brutos$NH1-brutos$H1)/(brutos$NH1+brutos$H1)
h2 <- (brutos$NH2-brutos$H2)/(brutos$NH2+brutos$H2)
h3 <- (brutos$NH3-brutos$H3)/(brutos$NH3+brutos$H3)
h4 <- (brutos$NH4-brutos$H4)/(brutos$NH4+brutos$H4)
h6 <- (brutos$NH6-brutos$H6)/(brutos$NH6+brutos$H6) #problema
h7 <- (brutos$NH7-brutos$H7)/(brutos$NH7+brutos$H7)
h8 <- (brutos$NH8-brutos$H8)/(brutos$NH8+brutos$H8)
h9 <- (brutos$NH9-brutos$H9)/(brutos$NH9+brutos$H9)

### Matriz com os indices para cada tempo para folhas:
alt.rii <- data.frame(h1,h2,h3,h4,h6,h7,h8,h9)
alt.rii[alt.rii=="NaN"] <- 0 # quando dentro e fora são 0 ele me da NaN, então estou substituindo pelo valor real que é "0"

    #adicionando variaveis categoricas para poder calcular a média no prox comando
    alt_long <- data.frame(brutos[,c(1:3)], alt.rii)

    #usando o reshape para poder calcular as médias no pŕoximo comando
    alt_wide <- melt(alt_long, id.vars = c("plot", "espnurse", "target"),
                 variable.name = "tempo", value.name= "RII")

#Média para os tempos
alt_medt <- summarise(group_by(alt_wide, plot, espnurse,target),
                     alt.mt.rii=mean(RII),
                     alt.st.rii=sd(RII))
#Média por nurse/target
alt.rii.mean <- summarise(group_by(alt_medt, espnurse,target),
                          alt.rii.m=mean(alt.mt.rii),
                          alt.rii.sd=sd(alt.mt.rii))


#################################################|*|###########################################
#Sobrevivencia

s <- (brutos$N_sob-brutos$sob)/(brutos$N_sob+brutos$sob)
s.rii <- data.frame(s)

#adicionando variaveis categoricas para poder calcular a média no prox comando
s_long <- data.frame(brutos[,c(1:3)], s)

#Média por nurse/target
s.rii.mean <- summarise(group_by(s_long, espnurse,target),
                          s.rii.m=mean(s),
                          s.rii.sd=sd(s))

#################################################|*|###########################################

                                       ### Tabelas Finais ##
#RII para cada tempo
rii_times <- cbind(brutos[ ,c(1:3)], fol.rii, alt.rii, s.rii)
head(rii_times)

#Média  dos tempos 
rii_mt <- cbind(brutos[ ,c(1:3)], fol_medt[ ,-c(1:3)], alt_medt[ ,-c(1:3)], s.rii)

#Média nurse/target juntando plots/nurse
rii_mean <- cbind (alt.rii.mean, fol.rii.mean[ ,-c(1:2)], s.rii.mean[ ,-c(1:2)])

head(rii_mean)
print("Objetos criados: rii_times = Indices para cada tempo | rii_mt = Média de todos os tempos |rii_mean = Média por nurse/target")

############################################ 


