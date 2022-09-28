# TP2 - ANADI #

#================================#
## Carregar packages necessários #
#================================#
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

# Limpar variáveis do ambiente
rm(list = ls())

#==========#
## Funções #
#==========#
### Função min-max para normalizar
minmaxnorm <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
### Função min-max para desnormalizar
minmaxdesnorm<-function(x, goal.attrib) {
  return (x * max(goal.attrib) - min(goal.attrib)) + min(goal.attrib)
}
### MAE
MAE <- function(test, prev) {
  if(length(test) == length(prev)){
    mean(abs(test-prev))
  }
}
### RMSE
RMSE <- function(test,predicted){
  if(length(test) == length(predicted)){
    sqrt(mean((test-predicted)^2))
  }
}
# Transformar data type para numérico
to_numeric<-function(df) {
  for (i in 1:length(df)) {
    ifelse(sapply(df[i], class) != "character", 
           df[,i]<-as.numeric(df[,i]), 
           df[,i]<-as.numeric(as.factor(df[,i]))-1)
  }
  return(df)
}
## Função para medir Acurracy Recall Precision F1
modelEvaluation<-function(tstlabels, predlabels) {
  # Testar se a qtd de valores é igual
  if(length(unique(tstlabels)) == length(unique(predlabels))) {
    
    # Matriz de Confusão (ou de Barras/Erros)
    #                   Previsões
    # Valores Reais | ^M+ | ^B- |
    #           M+  | TP  | FN  |
    #           B-  | FP  | TN  |
    # M+ & B- = dt.tst real | ^M+ & ^B- = previsões
    cfmatrix<-table(tstlabels, predlabels); cfmatrix
    # Tx. Acerto (accuracy) = TP+TN/TP+FP+FN+TN
    accuracy<-round(sum(cfmatrix[1], cfmatrix[4]) / sum(cfmatrix[1:4]), digits = 3)
    
    # Para detetar este problema:
    # Recall = TP / TP+FN (% de M's que o modelo consegue detetar)
    recall<-round(cfmatrix[1] / sum(cfmatrix[1], cfmatrix[3]), digits = 3) # aka sensitivity
    
    # Precision = TP / TP+FP (% de previsões do modelo corretas)
    precision<-round(cfmatrix[1] / sum(cfmatrix[1], cfmatrix[2]), digits = 3)
    
    # F1 = (2 * Precision * Recall) / (Precision + Recall) -> média harmónica de ambas
    # Quanto mais perto de 1, melhor
    fscore<-round((2 * (recall * precision))/(recall + precision), digits = 3)
    
    # Specificity - É o rácio entre as previsões dos que estão ativos sobre o total de ativos (oposto do recall)
    # Specificity = TN/(TN+FP)
    Specificity<-round(cfmatrix[4] / sum(cfmatrix[4], cfmatrix[3]), digits = 3) 
    
    data.frame(accuracy, recall, precision, Specificity, fscore)
  }}
# Alteração das colunas (sim/não) para int
str_to_int <- function(col) {
  as.integer(as.integer(as.factor(col))-1)
}

#======================================#
# DATA EXPLORATION & TRANSFORMATION ####
#======================================#

#===========#
## Geral ####
#===========#

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


#=========================================#
## Específico para o nº 4 do enunciado ####
#=========================================#
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


#================#
# REGRESSÃO   ####
#================#

#========================================================#
# 4. Modelo de Regressão Linear Simples - Fidelização ####
#========================================================#
dados<-clientes_dataset %>% select(Fidelização, TarifaMensal)

summary(dados)

# Critério Holdout 70% 30%
set.seed(1)
index <- sample(1:nrow(dados),0.7*nrow(dados))
dt.train <- dados[index,]
dt.tst <- dados[-index,]

## a) Apresente a função linear resultante ####
slm.model <- lm(Fidelização~TarifaMensal, data=dt.train)

slm.pred <- predict(slm.model, dt.tst)

summary(slm.model)
# Beta1 = 18.79078
# Beta2 = 0.20958
# R = 0.06581
# R-squared:  0.06581
# R^2 é a proporção da variância da Fidelização que 
# pode ser explicada pela tarifa mensal, 
# O valor obtido R^2, é de 6.5%. 

### Pressupostos ####
# Os resíduos seguem uma distribuição normal com média zero
shapiro.test(residuals(slm.model))
hist(residuals(slm.model))
summary(residuals(slm.model))
# p-val < alpha. Não se verifica a condição de normalidade
# A variância dos resíduos é constante (homocedasticidade).
mx = median(slm.model$residuals);
var.test(residuals(slm.model)[slm.model$residuals > mx], residuals(slm.model)[slm.model$residuals < mx])
# p-val < alpha. Não se verifica a condição de homocedasticidade

# Os resíduos são independentes.
# H0 : Os resíduos são independentes
durbinWatsonTest(slm.model)
# p-val .096 < alpha, rejeitamos H0. Para um nível de significância 95% podemos inferir que os residuos não são independentes
# Logo, A condição de independência de residuos não se verifica. 


## b) Visualize a reta correspondente ao modelo de regressão linear simples e o ####
# respetivo diagrama de dispersão.

par(mfrow=c(1,1))
plot(dados$Fidelização ~ dados$TarifaMensal, ylab="Fidelização", xlab="Tarifamensal")
abline(slm.model,col=c("red"))

## c) Calcule o erro médio absoluto (MAE) e raiz quadrada do erro médio (RMSE) do ####
#modelo sobre os 30% casos de teste.

ErMae <- round(MAE(dt.tst$Fidelização,slm.pred), digits=3); ErMae
# 20.934

ErRmse <- round(RMSE(dt.tst$Fidelização,slm.pred), digits=3); ErRmse
# 23.803

summary(dt.tst$Fidelização)
# erro grande, entre o primeiro e terceiro quartil, este modelo não é fiável

boxplot(dt.tst$Fidelização)

# O modelo de regressão linear simples apresenta um erro MAE e RMSE
# consideravel 20.934 (entre o segundo e terceiro quartil)

## Resumo pressupostos ...
# - Pressupostos da reg simples 
#   Falha - Os resíduos seguem uma distribuição normal com média zero
#   Falha - A variância dos resíduos é constante (homocedasticidade)
#   Falha - Os resíduos são independentes
#   Erro grande, 
rm(dados)

#===========================================================================#
# 5. Regressão Linear Múltipla, Árvore de Regressão e Rede Neuronal - TotalTarifas  ####
#===========================================================================#

#======================================#
## DATA EXPLORATION & TRANSFORMATION 5 #
#======================================#
# Variável objetivo: TotalTarifas (coluna 19)
# Variáveis previsoras: restantes
#=========================================#

# Histograma, diagrama de dispersão e correlação - atributos previsores numéricos:
# Fidelização | TarifaMensal ~ TotalTarifas
chart.Correlation(clientes_dataset[,c(5,18,19)], histogram = T, method = "pearson")
# Histogramas: distribuição não é normal (+/- assimétrico à esquerda)
# Plots~TotalTarifas: tendência crescente (mais dispersão à medida que os valores aumentam)
# Forte correlação

# Plot de correlação de todos atributos com TotalTarifas
corrplot(cor(clientes_dataset_num)[19,-19, drop=F], cl.pos="n", tl.srt=70, 
         method = "number", col=colorRampPalette(c("red","darkgray","blue"))(100))

# Remover os que estão pouco correlacionados (<0.1) com o atributo TotalTarifa
# Gama valores: [-0.36; 0.83]
clientes_dataset_5<-subset(
  clientes_dataset, select=-c(Genero, Dependentes, MétododePagamento))
# [0; 0.06; -0.04] respetivamente

clientes_dataset_num <- subset(
  clientes_dataset_num, select=-c(Genero, Dependentes, MétododePagamento))

# Definimos uma seed para estarmos a obter sempre o mesmo resultado aleatório
set.seed(75)

# é copiado o clientes_dataset_numerico
datasetTo5 <- clientes_dataset_num

index5a <- sample(1:nrow(datasetTo5), 0.7 * nrow(datasetTo5))

dt5a.train <- datasetTo5[index5a,]
dt5a.test <- datasetTo5[-index5a,]

## 5.a) Regressão linear múltipla ####

multipleLinearRegression <- lm( TotalTarifas~., data = dt5a.train )
# equação de regressão
multipleLinearRegression

summary(multipleLinearRegression)
# residual - erros que o modelo comete
# coefficients - maior o valor, maior importância para o modelo
# r-squared - varia entre 0-1, 90% da variavel objetivo é explicada por este modelo
# acima de 80%, significa que é aceitável

# criação do modelo usando os dados de Teste
mlr.model <- multipleLinearRegression
mlr.pred <- predict(mlr.model, dt5a.test)
summary(mlr.pred)

## 5.b) Árvore de regressão, usando a função rpart. Apresente a árvore de regressão obtida. ####

rpart.model <- rpart(TotalTarifas~., data=dt5a.train); rpart.model

# Visualizar a árvore de regressão 
# * significa que é uma folha
rpart.plot(rpart.model, digits = 3)

rpart.pred <- predict(rpart.model, dt5a.test); rpart.pred
rpart.plot(rpart.model, digits = 3)

## 5.c) Rede neuronal usando a função neuralnet, fazendo variar os parâmetros. Apresente a rede obtida ####

# Transformar variáveis categóricas (nº atributos > 2) com dummy_cols():
# ServiçoInternet e TipodeContrato 
dummy_data<-dummy_cols(clientes_dataset_5[,c(6,13)], remove_first_dummy = T)
str(dummy_data);

# Juntar ao dataset
clientes_dataset_5c <- cbind(dummy_data[,3:6], clientes_dataset_5[,-c(6,13)])
rm(dummy_data) # remover data frame temporário

# Transformar data types para numérico
clientes_dataset_5c <- to_numeric(clientes_dataset_5c); str(clientes_dataset_5c)

# normalização dos dados
datasetTo5.norm <- as.data.frame(apply(clientes_dataset_5c, 2, minmaxnorm))

# metodo Holdout (separação dos dados em 70%, 30%)
index5c <- sample(1:nrow(datasetTo5.norm), 0.7 * nrow(datasetTo5.norm))

dt5c.train <- datasetTo5.norm[index5c,]
dt5c.test <- datasetTo5.norm[-index5c,]

# Verificar normalização. Todos os valores têm de  estar entre 0-1
summary(dt5c.train$TotalTarifas)
summary(dt5c.test$TotalTarifas)


# Rede com 1 nó no nível interno
num_nodes <- 1
# Rede com outros parâmetros
# num_nodes <- 2
# num_nodes <- 3
# num_nodes <- c(3,2)
# No artigo consta a seguinte parametrização
# num_nodes <- c(5,3,2)

nnet <- neuralnet(TotalTarifas~., data = dt5c.train, hidden = num_nodes)
names(nnet)

nnet$weights
nnet$result.matrix
nnet$net.result

plot(nnet)

# Desnormalizar previsões
nn.pred <- compute(nnet, select(dt5c.test, -c("TotalTarifas")))
nn.pred.TotalTarifas <- minmaxdesnorm(nn.pred$net.result, clientes_dataset_5c$TotalTarifas)
test.TotalTarifas <- minmaxdesnorm(dt5a.test$TotalTarifas, clientes_dataset_5c$TotalTarifas)
head(nn.pred.TotalTarifas)

# Cálculo do MAE & RMSE
MAE(nn.pred.TotalTarifas, test.TotalTarifas)
RMSE(nn.pred.TotalTarifas, test.TotalTarifas)

summary(test.TotalTarifas)
# O erro é alto, pois está acima da média (perto do 3º quartil)

#==============================================#
# 6. Comparação dos resultados - MAE e RMSE ####
#==============================================#

# Regressão Linear Multipla - com estas equações, usando o modelo de previsão nos dados reais, conseguimos perceber se o modelo é bom ou não
MAE( dt5a.test$TotalTarifas, mlr.pred )
RMSE( dt5a.test$TotalTarifas, mlr.pred )

boxplot(clientes_dataset_5$TotalTarifas)
summary(dt5a.test$TotalTarifas)

# Árvore de regressão - o erro médio absoluto (MAE) e raiz quadrada do erro médio (RMSE) da árvore de regressão sobre o conjunto de teste
MAE( dt5a.test$TotalTarifas, rpart.pred )
RMSE( dt5a.test$TotalTarifas, rpart.pred )

boxplot(clientes_dataset_5$TotalTarifas)
summary(dt5a.test$TotalTarifas)

# Rede Neuronal
MAE( dt5c.test$TotalTarifas, nn.pred.TotalTarifas )
RMSE( dt5c.test$TotalTarifas, nn.pred.TotalTarifas )

boxplot(clientes_dataset_5$TotalTarifas)
summary(dt5c.test$TotalTarifas)


#==============================================================#
# 7. Significância Estatística dos Dois Melhores Modelos ####
#==============================================================#

# método de treino k-fold cross validation e a função anterior para obter as medidas de 
# avaliação de cada modelo; 
# Em relação ao método anterior (holdout), a vantagem do k-fold cross validation é
# que se faz todo o conjunto de validações dados

# data [x] [] []...
# data [] [x] []...
# data [] [] [x]...


k <- 10
folds <- sample(1:k, nrow(clientes_dataset_5), replace=TRUE)

table(folds)

cv.error <- matrix(nrow=k, ncol=2)

i<-1
for(i in 1:k) {
  
  # criacao de conjuntos com dados não normalizados
  train.cv <- clientes_dataset_5[folds != i,]
  test.cv <- clientes_dataset_5[folds == i,]
  
  mlr.model <- lm(TotalTarifas~., data = train.cv)
  mlr.pred <- predict(mlr.model, test.cv)
  
  rpart.model <- rpart(TotalTarifas~., data = train.cv)
  rpart.pred <- predict(rpart.model, test.cv)
  
  cv.error[i,] <- c(RMSE(mlr.pred, test.cv$TotalTarifas),
                    RMSE(rpart.pred, test.cv$TotalTarifas))
  
}

colnames(cv.error) <- c('mlr', 'rpart')
cv.error

apply(cv.error, 2, mean)
apply(cv.error, 2, sd)

# comparar se os 2 modelos s estatisticamente diferentes
diff <- cv.error[,1] - cv.error[,2]; diff

# Shapiro.test
# H0: A função distribuição das 2 amostras é idêntica
# H1: A função distribuição das 2 amostras não é idêntica
# Não rejeitamos H0, para um nível de significância de 5% podemos concluir que 
# os valores de accuracy seguem a distribuição normal

shapiro.test(diff)
# Podemos assumir que segue a distribuição normal, porque p_value = 0.7431 > 0.05

# paired t-test
t.test(cv.error[,1], cv.error[,2], paired=TRUE, alternative="two.sided")

# p-value = 2.746e-06 < alpha (5%)
# Para um nível de significância <= 5%, rejeita-se H0 (a favor de H1)
# Existe diferença significativa de eficiência entre os dois algoritmos
# Melhor desempenho: Árvore de Regressão

rm(dt5a.test, dt5a.train, dt5c.test, dt5c.train, datasetTo5, datasetTo5.norm, clientes_dataset_5, clientes_dataset_5c)

#====================#
# CLASSIFICAÇÃO   ####
#====================#

#===========================================================================#
# 8. Árvore de Decisão, Rede Neuronal e K-vizinhos-mais-próximos - Ativo ####
#===========================================================================#

#======================================#
## DATA EXPLORATION & TRANSFORMATION 8 #
#======================================#
# Variável objetivo: Ativo (coluna 20)
# Variáveis previsoras: restantes
#=========================================#

clientes_dataset_num<-to_numeric(clientes_dataset)

# Plot de correlação de todos atributos com Ativo
corrplot(cor(clientes_dataset_num)[20,-20, drop=F], cl.pos="n", tl.srt=70, 
         method = "number", col=colorRampPalette(c("red","darkgray","blue"))(100))

# Remover apenas os que estão muito pouco correlacionados (<0.03)
# Tendo em conta a gama de valores [-0.35; 0.34]
data<-select(clientes_dataset, -c(Genero, ServiçoTelefónico))

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
totalTarifas_max<-boxplot(filter(data, Ativo == "Sim")$TotalTarifas, 
                          plot=FALSE)$stats[5,]; totalTarifas_max
# Substituição dos registos
data$TotalTarifas<-replace(data[,17], 
                           data[,17] > totalTarifas_max, totalTarifas_max)
# Verificação
summary(filter(data, Ativo == "Sim")$TotalTarifas)


# Conversão para dummies

atributos_dummy <- c("ServiçoInternet", "TipodeContrato", "MétododePagamento")

dummy_data<-dummy_cols(clientes_dataset[,atributos_dummy], remove_first_dummy = T);

str(dummy_data)

data <- select(data, -atributos_dummy)

data <- cbind(dummy_data[4:10], data)

rm(dummy_data)


data[, c(8:10, 11:19, 22)] <- sapply(data[, c(8:10, 11:19, 22)], str_to_int )

data.norm <- as.data.frame(apply(data, 2, minmaxnorm));

data$Ativo <- factor(clientes_dataset$Ativo, levels = c("Não", "Sim"), labels = c("N", "S"))

data.norm$Ativo <- factor(clientes_dataset$Ativo, levels = c("Não", "Sim"), labels = c("N", "S"))

# verificação dos dados
str(data)
str(data.norm)

# Crit Holdout (70% - 30%)
set.seed(1)
index <- sample(1:nrow(data),0.7*nrow(data))

## Árvore de decisão ####

dt.train <- data[index,]
dt.tst <- data[-index,]

rpart.model <- rpart(Ativo~., data=dt.train)
rpart.pred <- predict(rpart.model, dt.tst, type='class'); rpart.pred

rpart.plot(rpart.model, digits = 3)

cfmatrix<-table(dt.tst$Ativo, rpart.pred); cfmatrix

#Specificity - É o rácio entre as previões dos que estão ativos sobre o total de ativos (oposto do recall)
# Specificity = TN/(TN+FP)
# cfmatrix[4]/(cfmatrix[4]+cfmatrix[3])

# necessário para Pergunta 11
DTREE_EVAL <- modelEvaluation(dt.tst$Ativo, rpart.pred); DTREE_EVAL

# RTree 79.3 accuracy


## Rede Neuronal ####

dt.train <- data.norm[index,]
dt.tst <- data.norm[-index,]

# dt.train$Ativo <- as.factor(unclass(dt.train$Ativo))
# dt.tst$Ativo <- as.factor(unclass(dt.tst$Ativo))

# Rede com 3 nós 
num_nodes<- 3

nnet <- neuralnet(Ativo~., data = dt.train, hidden = num_nodes)

names(nnet)

nnet$weights
nnet$result.matrix
nnet$net.result

plot(nnet)

nn.pred<-compute(nnet, select(dt.tst, -c("Ativo")))

nn.pred.net.result <- as.data.frame(nn.pred$net.result)[,1]; nn.pred.net.result

nn.pred.net.result <- as.integer(ifelse(nn.pred.net.result<.5, 0, 1))

modelEvaluation(dt.tst$Ativo, nn.pred.net.result)

cfmatrix<-table(dt.tst$Ativo, nn.pred.net.result); cfmatrix
#    N    S
#N  149 1385
#S  294  282

# Accuracy baixa, apenas 20.3% c/ 3 nodes
# Dificuldade em convergir 
# Mau modelo

## - K-vizinhos-mais-próximos ####

dt.train <- data.norm[index,]
dt.tst <- data.norm[-index,]


# remove atrib. obj.
train <- select(dt.train, -c(Ativo)); train
tst <- select(dt.tst, -c(Ativo)); train

train_labels <- data.norm[index, "Ativo"]
tst_labels <- data.norm[-index, "Ativo"]

k <- c()
accuracy_k<-data.frame(accuracy = double(), precision = double(), recall = double())
# encontrar K mais eficiente.. 
for (i in seq(1, 50, 2)) {
  # Só as variáveis previsoras
  knn.pred <- knn(train, tst, train_labels, k = i)
  
  accuracy_k <- rbind(accuracy_k, modelEvaluation(tst_labels, knn.pred))
  
  k <- c(k,i)
}

eval_k <- cbind(k, accuracy_k) 

plot(eval_k$k, eval_k$accuracy, ylab="Accuracy", xlab="Valor de K", main="Avaliação do Modelo em função do parametro K")

abline(v=45)

eval_k[eval_k$accuracy==max(eval_k$accuracy),] # K -> 45 regista o melhor accuracy

# Matriz de confusão para o melhor modelo.
knn.pred <- knn(train, tst, train_labels, k = 45)

cfmatrix<-table(tst_labels, knn.pred); cfmatrix

# necessário para Pergunta 11
KNN_EVAL <- modelEvaluation(tst_labels, knn.pred); KNN_EVAL

## 45 é o valor de K com melhor Accuracy 79.8%


## conclusão
# Árvore de decisão - 79.3%
# Rede neuronal - 20.3%
# KNN - 79.8%
# Árvore de decisão e KNN são os melhores modelos.


#===========================================================================#
# 9. Apuramento da média e do desvio padrão da taxa de acerto da previsão do atributo ####
#===========================================================================#

set.seed(10)
k<-10
# dividir os registos em 10 folds -> # gerar valores aleatórios entre 1 e nrow
folds<-sample(1:k, nrow(data), replace = T);
# tamanho de cada fold
table(folds)

accuracy <- matrix(nrow = k, ncol = 2)

for (t in 1:k) {
  
  # Árvore de decisão
  dt.train <- data[folds != t,]
  dt.tst <- data[folds == t,]
  
  rpart.model <- rpart(Ativo~., data=dt.train)
  rpart.pred <- predict(rpart.model, dt.tst, type='class');
  
  Arvore_Accuracy <- modelEvaluation(dt.tst$Ativo, rpart.pred)$accuracy
  
  # KNN
  dt.train <- data.norm[folds != t,]
  dt.tst <- data.norm[folds == t,]
  
  # remove atrib. obj.
  train <- select(dt.train, -c(Ativo));
  tst <- select(dt.tst, -c(Ativo));
  
  train_labels <- data.norm[folds != t, "Ativo"]
  tst_labels <- data.norm[folds == t, "Ativo"]
  
  knn.pred <- knn(train, tst, train_labels, k = i)
  
  KNN_Accuracy <- modelEvaluation(tst_labels, knn.pred)$accuracy
  
  accuracy[t, ] <- c(Arvore_Accuracy, 
                     KNN_Accuracy)
  
}
accuracy

cat("Mean =", round(apply(accuracy, 2, mean), digits = 3), "\nSD   =", 
    round(apply(accuracy, 2, sd), digits = 3))
# Mean = 0.787 0.791 
# SD   = 0.018 0.021


#=================================================================#
# 10. Significância Estatística dos Dois Melhores Modelos (5%) ####
#=================================================================#

# Comparação dos 2 melhores modelos: neuralnet e árvore de regressão
diff<-accuracy[,1] - accuracy[,2]
shapiro.test(diff)
# p-value = 0.81 > 5%

# Shapiro.test
# H0: A função distribuição das 2 amostras é idêntica
# H1: A função distribuição das 2 amostras não é idêntica
# Não rejeitamos H0, para um nível de significância de 5% podemos concluir que 
# os valores de accuracy seguem a distribuição normal

t.test(accuracy[,1], accuracy[,2], paired = T, alternative = "two.sided")

# p-value = 25.5% > alpha, Não rejeitamos H0, Os resultados não permitem concluir 
# que existe diferença significativa de eficiência entre os dois algoritmos.


#===========================================================================#
# 11. Comparação dos Resultados dos Modelos com Recurso aos Critérios Accuracy, Sensitivity, Specificity e F1 ####
#===========================================================================#

DTREE_EVAL

KNN_EVAL

# MODEL eval
# KNN
#     accuracy recall precision fscore
# 1    0.798  0.886     0.844  0.864


# Accuracy - Rácio entre o total de Previsões corretas sobre o Total de previsões
# Accuracy = (TP+TN)/(TP+FP+FN+TN)

#Sensitivity(ou Recall) - É o rácio entre as previsões dos que não estão ativos sobre o total dos que não estão ativos
# Recall = TP/(TP+FN)

#Specificity - É o rácio entre as previões dos que estão ativos sobre o total de ativos (oposto do recall)
# Specificity = TN/(TN+FP)


# F1 é a média harmonica entre a Precision e a Sensitivity, se o precision for 1 e o recall for 0, f1 é 0
# F1 Score = 2*(Recall * Precision) / (Recall + Precision)



#================#
#     FIM     ####
#================#