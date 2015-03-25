library(dplyr) # manuseio de dataframes
library(ggplot2)
library(gridExtra) #fazer pranchas de graficos

madeira <- read.csv ("traits/dados_traits_madeira.csv", h=T)
madeira$ID <- as.factor(madeira$ID)

                                                  # Tabela com réplicas #
          ####################################################################################################
          #Calculo da densidade da madeira, capacidade de armazenamento e conteúdo de água e largura da casca#
          ####################################################################################################

#Densidade = massa seca/volume fresco# [g/cm3]
#Madeira
madeira$dens_mad <- madeira$peso_seco_mad/madeira$vol_mad
#Casca
madeira$dens_casca <-madeira$peso_seco_casca/madeira$vol_casca

#Storage capacity = (peso sat-peso seco)/ peso seco# [%] 
#Madeira
madeira$cap_arm_mad <- (madeira$peso_sat_mad-madeira$peso_seco_madeira)/madeira$peso_seco_madeira
#Casca
madeira$cap_arm_casca <- (madeira$peso_sat_casca-madeira$peso_seco_casca)/madeira$peso_seco_casca

#Water content = (peso fresco-peso seco)/peso seco #
# numerador = peso coletado da madeira que é água - Numerador peso da madeira coletada que é madeira de fato,
#isso resulta na porcentagem daquela massa de madeira total que é água.  
#Madeira
madeira$cont_agua_mad <- (madeira$peso_fresco_mad-madeira$peso_seco_madeira)/madeira$peso_seco_madeira
#Casca
madeira$cont_agua_casca <- (madeira$peso_fresco_casca-madeira$peso_seco_casca)/madeira$peso_seco_casca

#Water storaged percentage= porcentagem de água da capacidade de armazenamento (o quanto de água estava armazenado na seca)
#Madeira
madeira$efet_agua_mad <- madeira$cont_agua_mad/madeira$cap_arm_mad
#Casca
madeira$efet_agua_casca <- madeira$cont_agua_casca/madeira$cap_arm_casca

#Bark thickness= diam1+diam2/2 [mm]
madeira$thick_casca <- (madeira$diam_casca+madeira$diam2_casca)/2


#adicionar dados da copa
cop <- read.csv("traits/dados_traits_copa.csv")
cop <- arrange(cop, especie)
#pegando somente os 3 dados que existem pra madeira também
copa<- cop[c(1,4,5,7,9,10,13,14,15,17,19,20,21,22,24,27,28,30,31,34,35,36,38,40,41,42,43,46,48,49,51,54,55,
             57,58,59,61,62,64,66,67,69,72,73,74,76,77,79,81,82,84,86,88,89,93,94,95,96,97,99),]
copa<- arrange(copa, especie)

#Alinhando os ID e nurses
ord <- match (madeira$ID, copa$ID)
copa <- copa[ord,]
#Corfirmação de que deu certo o alinhamento
#madeira$ID==copa$ID
#madeira$especie==copa$especie

#juntado tabelas
traits <- cbind(madeira, copa[,-c(1:2)])
head(traits)

                                        #### Tabela com médias por espécie #### 
                            

traits.mean <- summarise(group_by(traits,especie),
                         Mdiam_madeira_mm=mean(diam_madeira_mm), 
                         Mvol_mad = mean(vol_mad),
                         Mpseco_mad=mean(peso_seco_madeira), 
                         Mpeso_fresco_mad=mean(peso_fresco_mad), 
                         Mpeso_sat_mad=mean(peso_sat_mad),
                         Mthick_casca=mean(thick_casca),
                         Mvol_casca=mean(vol_casca),
                         Mpeso_seco_casca=mean(peso_seco_casca),
                         Mpeso_fresco_casca=mean(peso_fresco_casca),
                         Mpeso_sat_casca=mean(peso_sat_casca), 
                         Mdens_mad=mean(dens_mad),
                         Mdens_casca=mean(dens_casca),
                         Mcap_arm_mad=mean(cap_arm_mad), 
                         Mcap_arm_casca=mean(cap_arm_casca),
                         Mcont_agua_mad=mean(cont_agua_mad), 
                         Mcont_agua_casca=mean(cont_agua_casca),
                         Mefet_agua_mad=mean(efet_agua_mad),
                         Mefet_agua_casca=mean(efet_agua_casca),
                         MH_arvore=mean(H_arvore_m),
                         MH_copa=mean(H_copa_m),                          
                         Mdiam_copa=mean(diam_medio))


print ("madeira= traits de madeira por réplica |cop=traits de copa com 5 rplicas |copa = traits de copa por 3 réplica |traits = copa e madeira por réplica |traits.mean = media de todos traits por especie")

