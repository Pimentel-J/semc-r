# TP2 - ANADI #

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

# Limpar variáveis do ambiente
rm(list = ls())
source("functions.R")

# Carregar ficheiro
clientes_dataset<-read.csv("Clientes_DataSet.csv", fileEncoding="latin1")

# Dimensão/Sumário
head(clientes_dataset)
dim(clientes_dataset)
str(clientes_dataset)
# 21 variáveis (maioria do tipo 'chr')
## Categóricas: Género, Maior65, Ativo, etc.
## Numéricas: Fidelização, TarifaMensal, TotalTarifas

summary(clientes_dataset)
# A gama de valores TotalTarifas é bastante acentuada [18.8; 8684.8]

# Colunas de caracteres num data frame podem conter espaços em branco no início ou no final das strings
clientes_dataset %>% mutate_if(is.character, str_trim) -> clientes_dataset


# Remoção dos 11 NA's em "TotalTarifas"
clientes_dataset<-na.omit(clientes_dataset)

# Remoção de ClienteID (é irrelevante para este caso)
length(unique(clientes_dataset$ClienteID)) == length(clientes_dataset$ClienteID)
# Não existe duplicação de clientes
clientes_dataset<-clientes_dataset[,-1]

# Renomear "Sem serviço telefónico" ou "Sem serviço internet" (é equivalente a "Não")
clientes_dataset<-replace(clientes_dataset, clientes_dataset == "Sem serviço telefónico" | 
                            clientes_dataset == "Sem serviço internet", "Não")

# Abreviar nome dos registos de MétododePagamento e ServiçoInternet
# (p.e. depois de criar dummies não é preciso renomear colunas, melhor leitura na árvore decisão)
clientes_dataset$MétododePagamento<-str_replace_all(clientes_dataset$MétododePagamento, 
                      c("Cheque Eletrónico" = "Cheque_Eletro", 
                      "Cheque por email" = "Cheque_Email", 
                      "Transferência Bancária \\(automatico\\)" = "Transferência",
                      "Cartão de Crédito \\(automatico\\)" = "Cartão"))
clientes_dataset$ServiçoInternet<-str_replace_all(clientes_dataset$ServiçoInternet, 
                                                  c("Fibra ótica" = "Fibra"))

# Renomear TipoServiço para um nome mais apropriado -> ServiçoTelefónico
names(clientes_dataset)[6]<-"ServiçoTelefónico"

# Nova verificação
str(clientes_dataset)
summary(clientes_dataset)

# Transformar data types para numérico
clientes_dataset_num<-to_numeric(clientes_dataset); str(clientes_dataset_num);

# Quantidade e percentagem de clientes ativos e inativos
AbsFreq<-table(clientes_dataset$Ativo)
PercFreq<-round(prop.table(table(clientes_dataset$Ativo)) * 100, digits = 2)
cbind(AbsFreq, PercFreq)
# % alta de inativos

# Plot de correlação com todos os atributos
corrplot(cor(clientes_dataset_num), type = "upper", order = "alphabet",
         tl.cex = 0.9, tl.srt=70)



#=============#
#     FIM     #
#=============#