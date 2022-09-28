# TP2 - ANADI #

source("TP2_Data.R", encoding = "UTF-8")

## Carregar packages necessários
library(car)          # funções de correlação
library(corrplot)     # corrplot()
library(dplyr)        # summarize(), filter(), etc.
library(fastDummies)  # dummy_cols()
library(FNN)          # knn.reg()
library(neuralnet)    # neuralnet()
library(PerformanceAnalytics) # gráficos (hist, plot...)
library(rpart)        # árvore de regressão
library(rpart.plot)   # árvore de regressão - plot
library(stringr)      # str_trim()


#=========================================#
# 1. DATA EXPLORATION & TRANSFORMATION ####
#=========================================#

#================#
## 1.1. Geral ####
#================#

# Conteúdo está no TP2_Data.R

#==============================================#
## 1.2. Específico para o nº 4 do enunciado ####
#==============================================#
# Variável objetivo: Fidelização (coluna 5)
# Variável previsora: TarifaMensal (coluna 18)
#==============================================#

par(mfrow = c(1,2)); # Criar 1x3 plots
# Boxplots: TarifaMensal & Fidelização
boxplot(clientes_dataset$Fidelização, main = "Fidelização", ylab = "Meses")
boxplot(clientes_dataset$TarifaMensal, main = "TarifaMensal", ylab = "€")
# +/- simétrico, sem outliers
dev.off() # limpar todos os plots

# Histograma, diagrama de dispersão e correlação
chart.Correlation(clientes_dataset[,c(18,5)], histogram = T, method = "pearson")
# Histogramass: distribuição não é normal (assimétrico)
# Plot: valores muito dispersos, sem uma tendência
# Correlação moderadamente baixa (+0.25)


#==============================================#
## 1.3. Específico para o nº 5 do enunciado ####
#==============================================#
# Variável objetivo: TotalTarifas (coluna 19)
# Variáveis previsoras: restantes
#==============================================#

# Histograma, diagrama de dispersão e correlação - atributos previsores numéricos:
# Fidelização | TarifaMensal ~ TotalTarifas
chart.Correlation(clientes_dataset[,c(5,18,19)], histogram = T, method = "pearson")
# Histogramas: distribuição não é normal (+/- assimétrico à esquerda)
# Plots~TotalTarifas: tendência crescente (mais dispersão à medida que os valores aumentam)
# Forte correlação

# Plot de correlação de todos atributos com TotalTarifas
corrplot(cor(clientes_dataset_num)[19,-19, drop=F], cl.pos="n", tl.srt=70, 
         method = "number", col=colorRampPalette(c("red","darkgray","blue"))(100))

# Remover os que estão muito pouco correlacionados (<0.1) - gama valores: [-0.36; 0.83]
clientes_dataset_5<-subset(
  clientes_dataset, select=-c(Genero, Dependentes, MétododePagamento))
# [0; 0.06; -0.04] respetivamente

#---------------------------#
# Para o 5. c) -> neuralnet #
#---------------------------#

# Transformar variáveis categóricas (nº atributos > 2) com dummy_cols():
# ServiçoInternet e TipodeContrato 
dummy_data<-dummy_cols(clientes_dataset_5[,c(6,13)], remove_first_dummy = T)

# Juntar ao dataset
clientes_dataset_5c<-cbind(dummy_data[,3:6], clientes_dataset_5[,-c(6,13)])
rm(dummy_data) # remover data frame temporário

# Transformar data types para numérico
clientes_dataset_5c<-to_numeric(clientes_dataset_5c); str(clientes_dataset_5c)


#==============================================#
## 1.4. Específico para o nº 8 do enunciado ####
#==============================================#
# Variável objetivo: Ativo (coluna 20)
# Variáveis previsoras: restantes
#==============================================#

# Plot de correlação de todos atributos com Ativo
corrplot(cor(clientes_dataset_num)[20,-20, drop=F], cl.pos="n", tl.srt=70, 
         method = "number", col=colorRampPalette(c("red","darkgray","blue"))(100))

# Remover apenas os que estão muito pouco correlacionados (<0.03)
# Tendo em conta a gama de valores [-0.35; 0.34]
clientes_dataset_8<-subset(
  clientes_dataset, select=-c(Genero, ServiçoTelefónico))

# Uma maneira de verificar os que são para remover (opcional)
# correlations<-round(cor(clientes_dataset_num)[20,-20], digits = 2); (names(which(correlations < .03 & correlations > -.03)))

par(mfrow = c(1,3)); # Criar 1x3 plots
# Boxplots: Fidelização & TarifaMensal & TotalTarifas ~ Ativo
for (i in c(5,18,19)) {
  boxplot(clientes_dataset[,i] ~ Ativo, data = clientes_dataset, 
          main = names(clientes_dataset[i]), names = c("Não", "Sim"), 
          ylab = names(clientes_dataset[i]))
}
dev.off() # limpar todos os plots

# Substituição dos outliers significativos de TotalTarifas pelo valor do limite superior
# Extrair o valor máximo, sem considerar outliers
totalTarifas_max<-boxplot(filter(clientes_dataset_8, Ativo == "Sim")$TotalTarifas, 
                          plot=FALSE)$stats[5,]; totalTarifas_max
# Substituição dos registos
clientes_dataset_8$TotalTarifas<-replace(clientes_dataset_8[,17], 
                                clientes_dataset_8[,17] > totalTarifas_max, totalTarifas_max)
# Verificação
summary(filter(clientes_dataset_8, Ativo == "Sim")$TotalTarifas)
# Data frame para o b)/c)
clientes_dataset_8bc<-clientes_dataset_8

#-----------------------------------#
# Para o 8. a) -> Árvore de decisão #
#-----------------------------------#

# Redefinir os levels das variáveis para colocar os inativos em 1º
# Temos maior interesse em avaliar os inativos
clientes_dataset_8$Ativo<-factor(clientes_dataset_8$Ativo, levels = c("Não", "Sim"), 
                                   labels = c("N", "S"))

clientes_dataset_8[-18]<-to_numeric(clientes_dataset_8[-18]); str(clientes_dataset_8)

#-----------------------------------------#
# Para o 8. b)/c) -> neuralnet/k-vizinhos #
#-----------------------------------------#

# Transformar variáveis categóricas (nº atributos > 2) com dummy_cols():
# TipodeContrato
dummy_data<-dummy_cols(clientes_dataset_8bc[,c(6,13,15)], remove_first_dummy = T); str(dummy_data);

# Juntar ao dataset
clientes_dataset_8bc<-cbind(dummy_data[,4:10], clientes_dataset_8bc[,-c(6,13,15)])
rm(dummy_data) # remover data frame temporário

# Transformar data types para numérico
clientes_dataset_8bc<-to_numeric(clientes_dataset_8bc); str(clientes_dataset_8bc)

#===================#
# 2. REGRESSÃO   ####
#===================#

#==============================================#
## 4.... ####
#==============================================#
# Notas:
# Só o k-vizinho e redes neuronais precisam de dados numéricos - dummy
# 6./7. K-fold cross validation para os 3 modelos -> k=10
# Com os valores já se pode aplicar o teste estatístico pedido no 7.

#=======================#
# 3. CLASSIFICAÇÃO   ####
#=======================#

# Notas:
# 9. Matrix de confusão

#=============#
#     FIM     #
#=============#