# TP1 - ANADI 21/22 #
# 4.1 - Análise do funcionamento dos servidores VPN do DEI

## Carregar packages necessárias
library(readxl)   # <- ler ficheiros
library(dplyr)    # <- funções: mutate, group_by, summarize, etc.
library(ggplot2)  # <- gerar gráficos
library(tidyr)    # <- alterar tabelas (pivoting)
library(car)      # <- funções de correlação
library(lubridate)# <- funções para manipular datas

#-###########-#
# EXERCÍCIO 1 #
#-###########-#

# 1. Responda às seguintes questões na forma que ache mais apropriada (use o conjunto completo de dados) ####

## Importação de dados (sessões VPN) ####
sessoes_vpn<-read.table("vpnsessionsfile.txt")

## Formatar data frame (remover colunas desnecessárias + renomear colunas)
sessoes_vpn<-sessoes_vpn[,-c(6,8)]
names(sessoes_vpn)[1:6]<-c("Servidor", "Protocolo", "Data", 
                           "Inicio", "Fim", 
                           "Duracao"); head(sessoes_vpn)

sessoes_vpn$Data<-as.Date(sessoes_vpn$Data) # converte o campo de chr para Date

# Adiciona uma coluna auxiliar com acesso = 1/falha = 0 (necessária para a resolução das seguintes alíneas)
sessoes_vpn<-sessoes_vpn %>% mutate(Acesso = ifelse(Duracao > 1, 1, 0));


## a) Nº de acessos de cada servidor ####

# Filtrar sessões com duração >1 minuto (acesso)
acessos_vpn<-filter(sessoes_vpn, Duracao > 1); head(acessos_vpn)

# Determinar o nº de acessos por servidor
num_acessos_servidor<-table(acessos_vpn$Servidor); num_acessos_servidor

# Extra: Gráfico de barras
graph<-barplot(num_acessos_servidor, col = topo.colors(5),
                 main = "Nº de Acessos p/ Servidor",
                 xlab = "Servidor", ylab = "Nº Acessos",
                 ylim = c(0, max(num_acessos_servidor)+4000)
); text(graph, num_acessos_servidor+750, num_acessos_servidor)


## b) Nº de falhas de cada servidor ####

# Filtrar sessões com duração =< 1 minuto (falha)
falhas_vpn<-filter(sessoes_vpn, Duracao < 2); head(falhas_vpn)

# Nº de falhas por servidor (quantos elementos tem cada nível)
num_falhas_servidor<-table(falhas_vpn$Servidor); num_falhas_servidor

# Extra: Gráfico de barras
graph<-barplot(num_falhas_servidor, col = topo.colors(5),
                 main = "Nº de Falhas p/ Servidor",
                 xlab = "Servidor", ylab = "Nº Falhas",
                 ylim = c(0, max(num_falhas_servidor)+1000)
); text(graph, num_falhas_servidor+250, num_falhas_servidor)


## c) Nº de vezes o servidor "X" usa o protocolo "Y" ####

utilizacao_servidor_protocolo<-sessoes_vpn %>%
  # adiciona uma coluna com o nº de utilizações de cada protocolo por servidor
  mutate(n_utilizacoes = 1) %>%
  # agrupa por servidor e protocolo
  group_by(Servidor, Protocolo) %>%
  # somar o nº de utilizações de cada protocolo por servidor
  summarize("N_Utilizacoes" = sum(n_utilizacoes)); utilizacao_servidor_protocolo

# Extra: dados apresentados de forma resumida (Linhas = Servidores / Colunas = Protocolos)
resumo_servidor_protocolo<-pivot_wider(utilizacao_servidor_protocolo, 
                   names_from = Protocolo, values_from = N_Utilizacoes, 
                   values_fill = list(N_Utilizacoes = 0)); resumo_servidor_protocolo


## d) Determinar médias, medianas e desvios padrão mensais de acessos de cada servidor (apenas meses completos) ####

# Identifica meses completos (para depois remover incompletos)
num_dias_por_mes<-acessos_vpn %>% 
  group_by(Mes = format(Data, "%Y-%m")) %>% 
  # apurar o nº de dias distintos por mês na data frame 'acessos_vpn'
  summarise(Num_Dias = n_distinct(Data)) %>%
  # Adicionar nº de dias do respetivo mês (paste para adicionar o dia e assim funcionar o lubridate)
  mutate(Total_Dias = unname(lubridate::days_in_month(as.Date(paste(Mes, "-01", sep = "")))))

# Identifica os meses completos (TRUE) e incompletos (FALSE)  
num_dias_por_mes<-num_dias_por_mes %>%
  mutate(Mes_Completo = Num_Dias == Total_Dias); num_dias_por_mes
# Meses incompletos: 2016-12, 2017-07, 2018-02

# Cálculo da média, mediana e desvio padrão
acessos_mensais <- 
  # filtra os dados por meses completos
  filter(acessos_vpn, !(format(Data, "%Y-%m") %in% c("2016-12", "2017-07", "2018-02"))) %>%
  # adiciona uma coluna com os meses
  mutate(Mes = format(Data, "%Y-%m")) %>%
  # agrupa por mês, servidor
  group_by(Mes, Servidor) %>%
  # adiciona uma coluna para auxiliar no cálculo (sum) seguinte
  mutate(Acesso = 1) %>% 
  # faz a soma dos acessos para cada servidor p/ mês
  summarise(acessos=sum(Acesso)) %>%
  # agrupa por servidor
  group_by(Servidor) %>%
  # calcula média, mediana e desvio padrão.
  summarise(Media = round(mean(acessos), digits = 2),
            Mediana = round(median(acessos), digits = 2), 
            Desvio_Padrao = round(sd(acessos), digits = 2)); acessos_mensais

# Podemos verificar, através dos valores do desvio padrão, que há uma maior dispersão dos dados (nº de acessos/mês)
# nos servidores vsrv16, vsrv17 e vsrv8 comparativamente com os servidores vsrv10 e vsrv11.


#-###########-#
# EXERCÍCIO 2 #
#-###########-#

# 2. Com os dados relativos ao mês de dezembro de 2017 (todos os servidores e todos os protocolos) ####
## a) Gráfico que nas abcissas representa o tempo e nas ordenadas o nº de falhas simultâneas e o nº de acessos simultâneos ####
# Nota: nº de acessos simultâneos determinado de forma matricial

# Sessões de dezembro de 2017
sessoes_vpn_dez2017 <- filter(sessoes_vpn, Data >= "2017-12-01", 
                              Data < "2018-01-01"); head(sessoes_vpn_dez2017)

# Data frame auxiliar: Servidor, Acesso/Falha, Login/Logout, Time, Sessions_Num, Date (string)
# Acesso = 1/Falha = 0 | Login = 1/Logout = -1) | Sessions_Num = nº de sessões ativas ]

# Tabelas com os logins, logouts e o respetivo tempo
login<-cbind(
  Acesso_Falha=sessoes_vpn_dez2017$Acesso, 
  Login_Logout=1, 
  Time=as.POSIXct(paste(sessoes_vpn_dez2017$Data, sessoes_vpn_dez2017$Inicio)))
logout<-cbind( 
  Acesso_Falha=sessoes_vpn_dez2017$Acesso, 
  Login_Logout=-1,
  # tempo inicial + duração
  Time=as.POSIXct(paste(sessoes_vpn_dez2017$Data, sessoes_vpn_dez2017$Inicio)) +
    (sessoes_vpn_dez2017$Duracao * 60))

# Data frame com os logins e logouts ordenados pelo tempo
logins_logouts<-data.frame(rbind(login,logout)) %>% 
  # agrupa e ordena por tempo
  group_by(Time) %>% arrange(Time)

# O número de acessos múltiplos pode agora facilmente ser calculado pela soma 
# acumulada pelo tipo (login(1) logout(-1))
# ex:
# tempos   t0     t0     t0  t1    t1 t1
# lig1 :   |-----------------------|
# lig2 :          |-------------------|
# lig3 :                 |---|
# cumsum:  1      2      3   2     1  0

# Data frame com logins/logouts e nº de acesso/falha simultâneo por minuto
sessoes_simultaneas<-logins_logouts %>%
  group_by(Acesso_Falha) %>%
  # adiciona colunas auxiliares com o nº de ligações simultâneas e com o tempo no formato YYYY-MM-DD HH:MM
  mutate(Sessions_Num = cumsum(Login_Logout), 
         Date = as.POSIXct.numeric(Time, origin = '1970-01-01 00:00:00'))

# Transformar o formato da data
sessoes_simultaneas$Date<-as.Date(sessoes_simultaneas$Date, "%Y-%m-%d", tz = "GMT")
# Remover a parte das 2 sessões que ultrapassam 31 de dezembro 2017
sessoes_simultaneas<-filter(sessoes_simultaneas, Date < "2018-01-01")

# Filtrar nº de falhas simultâneas por minuto
falhas_simultaneas_minuto<-filter(sessoes_simultaneas, Acesso_Falha == 0) %>% 
  group_by(Time) %>% top_n(1, Sessions_Num)
# Agrupar o nº de sessões/min. em dias
falhas_simultaneas_dia<-setNames(aggregate(falhas_simultaneas_minuto$Sessions_Num, 
                                  by = list(falhas_simultaneas_minuto$Date), 
                                  FUN = sum), c("Data", "Num_Sessoes")); head(falhas_simultaneas_dia)

# Gráfico com o nº de falhas simultâneas ao longo do tempo (dias)
ggplot(data=falhas_simultaneas_dia, aes(x=as.Date(Data, "%Y-%m-%d")))+
  geom_line(aes(y=Num_Sessoes,colour="Num_Sessoes"), size=1)+
  geom_point(aes(y=Num_Sessoes,colour="Num_Sessoes"))+
  geom_text(aes(label=Num_Sessoes, y=Num_Sessoes), hjust=0, vjust=-0.3)+
  scale_color_manual(name="Legenda", values = c("Num_Sessoes"="#56B4E9"))+
  labs(title="Nº de Falhas Simultâneas/Dia - Dez 2017", y="Nº Falhas", x="Dia")
#+scale_x_date(breaks = seq(as.Date("2017-12-01"), as.Date("2017-12-31"), by="5 days"))

# Filtrar nº de falhas simultâneas por minuto
acessos_simultaneos_minuto<-filter(sessoes_simultaneas, Acesso_Falha == 1) %>% 
  group_by(Time) %>% top_n(1, Sessions_Num)
# Agrupar o nº de sessões/min. em dias
acessos_simultaneos_dia<-
  setNames(aggregate(acessos_simultaneos_minuto$Sessions_Num, 
                     by = list(acessos_simultaneos_minuto$Date), 
                     FUN = sum), c("Data", "Num_Sessoes")); head(acessos_simultaneos_dia)

# Gráfico com o nº de acessos simultâneas ao longo do tempo (dias)
ggplot(data=acessos_simultaneos_dia, aes(x=as.Date(Data, "%Y-%m-%d")))+
  geom_line(aes(y=Num_Sessoes,colour="Num_Sessoes"), size=1)+
  geom_point(aes(y=Num_Sessoes,colour="Num_Sessoes"))+
  geom_text(aes(label=paste(format(round(Num_Sessoes / 1e3, 1), trim = TRUE), 
                            "k"), y=Num_Sessoes), 
            hjust=0.3, vjust=-0.5, check_overlap = T)+
  scale_color_manual(name="Legenda", values = c("Num_Sessoes"="#56B4E9"))+
  labs(title="Nº de Acessos Simultâneos/Dia - Dez 2017", 
       y="Nº Acessos", x="Dia")
#+scale_x_date(breaks = seq(as.Date("2017-12-01"), as.Date("2017-12-31"), by="5 days"))


## b) Diagrama de caixa de bigodes do nº diário de falhas simultâneas, para cada servidor ####
# (o número diário de falhas simultâneas é o número total de falhas simultâneas que ocorreram nesse dia). 
# Indique quantos outliers existem por servidor.

### TODO ###

## c) Verificação da correlação entre o nº de falhas simultâneas e o nº de acessos simultâneos, no dia 11-12-2017 das 12:00 às 14:00 ####
# Teste de Correlação Linear de Pearson

# Data frame com logins/logouts e nº de acesso/falha simultâneo por minuto
sessoes_simultaneas<-logins_logouts %>%
  group_by(Acesso_Falha) %>%
  # adiciona colunas auxiliares com o nº de ligações simultâneas e com o tempo no formato YYYY-MM-DD HH:MM
  mutate(Sessions_Num = cumsum(Login_Logout), 
         Date = as.POSIXct.numeric(Time, origin = '1970-01-01 00:00:00'))

# Nº de falhas simultâneas no dia 11 de dezembro de 2017 das 12h às 14h
falhas_dia_11<-filter(falhas_simultaneas_minuto, Time >= 
                        as.POSIXct("2017-12-11 12:00:00", 
                                   origin = '1970-01-01 00:00:00') & 
                        Time <= as.POSIXct("2017-12-11 14:00:00", 
                                           origin = '1970-01-01 00:00:00'))

# Nº de acessos simultâneos no dia 11 de dezembro de 2017 das 12h às 14h
acessos_dia_11<-filter(acessos_simultaneos_minuto, Time >= 
                        as.POSIXct("2017-12-11 12:00:00", 
                                   origin = '1970-01-01 00:00:00') & 
                        Time <= as.POSIXct("2017-12-11 14:00:00", 
                                           origin = '1970-01-01 00:00:00'))

falhas_dia_11<-setNames(aggregate(falhas_dia_11$Sessions_Num, by = list(falhas_dia_11$Time), FUN = sum), c("Time", "Sessions_Num"))
acessos_dia_11<-setNames(aggregate(acessos_dia_11$Sessions_Num, by = list(acessos_dia_11$Time), FUN = sum), c("Time", "Sessions_Num"))

# Remoção de colunas desnecessárias
falhas_dia_11<-falhas_dia_11[,-c(1,2,5)]; head(falhas_dia_11)
acessos_dia_11<-acessos_dia_11[,-c(1,2,5)]; head(acessos_dia_11)

# Por algum motivo perdemos o merge desta tabela
acessos_falhas_dia_11<-setNames(merge(acessos_dia_11, falhas_dia_11, by = "Time", all.x = T), c("Time", "Acessos", "Falhas"))

acessos_falhas_dia_11_sem_NA<-acessos_falhas_dia_11
acessos_falhas_dia_11_sem_NA[is.na(acessos_falhas_dia_11_sem_NA)]<-0

# Análise de pressupostos do teste de Pearson:
# P.1 - As variáveis devem ser contínuas e não existir outliers significativos
boxplot(acessos_falhas_dia_11$Acessos, acessos_falhas_dia_11$Falhas)
# Através da análise do diagrama de extremos e quartis (caixa de bigodes) confirma-se a inexistência de outliers

# P.2 - Deve existir uma relação linear entre as duas variáveis
# plot(acessos_falhas_dia_11$Acessos, acessos_falhas_dia_11$Falhas)
acessos_falhas_dia_11 <- setNames(left_join(acessos_dia_11, falhas_dia_11, by = "Time"), c("Time", "Acessos", "Falhas"));

ggplot(data=acessos_falhas_dia_11, aes(x=as.POSIXct.numeric(Time, origin = '1970-01-01 00:00:00')))+
  geom_point(aes(y=Acessos,colour="Acessos"))+
  geom_point(aes(y=Falhas,colour="Falhas"))
# Através da análise do diagrama de dispersão verifica-se a inexistência de uma relação linear
# Os pressupostos do teste de Pearson não são satisfatórios -> Teste de Kendall


## NOTA: com os novos valores corretos das falhas simultâneas, se o gráfico continuar disperso -> ñ é preciso verificar os outros 2 pressupostos ##
## Caso contrário, estão feitos mais abaixo.

# Nota: alternative = "greater" -> pretende-se estudar se o nº de Acessos está
# está associado positivamente com o nº de Falhas
# H0: tau = 0 | H1: tau != 0

cor.test(acessos_falhas_dia_11_sem_NA$Acessos, acessos_falhas_dia_11_sem_NA$Falhas, 
         alternative = "g", method = "kendall", exact = F)
# Os dados indiciam uma fraca correlação negativa (tau = -0.09) entre as sessões (acessos/falhas)
# p-value=0.71 > alfa = 0.05 -> não rejeitar H0 -> Com nível de sign. 5%, a correlação não é significativa
# tau= -0.09 -> Existe uma fraca associação negativa entre as variáveis

# NOTA: #
# a lower score on variable A is always associated with a higher score on variable B
# https://www.spss-tutorials.com/kendalls-tau/


### FIM 2. c) ###


### NOTA: os outros 2 pressupostos, caso seja necessário...

# p.3 - Variáveis devem ter aproximadamente uma distribuição normal
# Gráfico de quartis
qqnorm(acessos_falhas_dia_11$Acessos, xlab = "Quartis de distr. normal", 
       ylab = "Quartis amostrais", main = "Acessos")
qqline(acessos_falhas_dia_11$Acessos)
# Os dados estão aproximadamente sobre a linha -> não contraria a distribuição 
# dos valores de nº de acessos

# Gráfico de quartis
qqnorm(acessos_falhas_dia_11$Falhas, xlab = "Quartis de distr. normal", 
       ylab = "Quartis amostrais", main = "Falhas")
qqline(acessos_falhas_dia_11$Falhas)
# Atendendo à escala dos dados não podemos afirmar que estão muito longe da linha

#### Amostra com 42 (>30) -> posso usar o Shapiro em vez de Lilliefors?

shapiro.test(acessos_falhas_dia_11$Acessos) # Teste Shapiro - permite 
# verificar se os dados provêm de uma distribuição normal (H0)
# Hipót. alternativa (H1) -> não provém de uma distr. normal
# p-value=0.02 < alfa = 0.05 -> rejeitar H0
# Rejeitada a hip. da amostra ser proveniente de uma pop. normal -> o teste 
# de Pearson perde validade

# Se ambas as amostras fossem compatíveis c/ o teste de Normalidade
# p.4 - Homocedasticidade (variâncias iguais)
valores<-c(acessos_falhas_dia_11$Acessos, acessos_falhas_dia_11$Falhas)
grupos<-as.factor(c(rep(1, length(acessos_falhas_dia_11$Acessos)), 
                     rep(2, length(acessos_falhas_dia_11$Falhas))))

leveneTest(valores, grupos, center = mean) # teste à igualdade de variâncias das duas amostras
# p-value=0.052 !< alfa (0.05) -> não rejeitar H0
# Dados indiciam existência de Heterocedasticidade
# Os pressupostos do teste de Pearson não são satisfatórios

