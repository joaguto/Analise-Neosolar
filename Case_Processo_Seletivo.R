# Case processo seletivo Estágio NeoSolar
# Canditado João Augusto de Oliveira Jorge

##################################################

# Definição do problema:
# Os dados são coletados a partir de ligações e pesquisas de satisfação pós-atendimento.
# O gestor da área gostaria de saber:

# Qual ou quais são os problemas que devemos melhorar primeiro, e por quê? 
# Em quais países estamos com mais problemas?
# Quais são os problemas mais recorrentes?
# Qual canal de suporte mais usado?
# Qual a percepção dos clientes que ligam no call-center ?


# Definindo diretório de Trabalho  
setwd("C:/Users/JoaoA/Downloads/Neosolar")
getwd()
dir()

# Importando Dataset
library(readxl)
requests = read_excel("Dataset Case - Processo Seletivo Neosolar 2021 (1).xlsx", sheet = "Requests")
survey = read_excel("Dataset Case - Processo Seletivo Neosolar 2021 (1).xlsx", sheet = "Survey")


##################################

# Iniciando analise de dados
library(dplyr)

View(requests)
View(survey)

str(requests)
str(survey)

requests$`Service Request Id` <- as.character(requests$`Service Request Id`)
survey$`Service Request Id` <- as.character(survey$`Service Request Id`)

summary(requests)
summary(survey)


# Criando Data Frame unificado
str(df)
df= requests

# Verificando valores Vazios
Miss_Values = function(x){
  sapply(x, function(x) sum(is.na(x)))
}

Miss_Values(df)

# Verificando a % de valores Vazios
miss_values2 = function(x) {
  sapply(x, function(x) (sum(is.na(x)) /length(x)) *100 ) 
}

miss_values2(df)

# Como valores vazios são representam muito pouco em nosso data frame, optei por excluir esses dados
df = na.omit(df)

dim(df)
View(df)

# Removendo dados invalidos
df = df%>%
  filter(`Valid Data ?.x` == TRUE)
View(df)

# Removendo coluna Dados validos pois não será mais necessario
df$`Valid Data ?.x` = NULL

View(df)

# Renomeando colunas
df = df %>%
  rename(Pais = `Customer Country/Region.x`, Motivo = `Issue Code 1.y`, ID = `Service Request Id`,
         Canal_Suporte = `Support Channel.x`, tempo_Para_Fechar = `Time To Close.x`)
View(df)


dim(df)



# Salvando Dataframe
write.csv(df, "df.csv", sep = ',')
dir()



##################################################

library(ggplot2)
df = read.csv('df.csv', sep = ',')
View(df)
df$X = NULL

# Analise Exploratoria
summary(df)
unique(df$Motivo)
df$Motivo = as.factor(df$Motivo)

# Verificando distribuição de dados de tempo para fechar
hist(df$tempo_Para_Fechar)
boxplot(df$tempo_Para_Fechar)
summary(df$tempo_Para_Fechar)
?barplot


# Criando um dataset com tempo médio de fechamento e numero de pedidos por motivo
motivos = df %>%
  select(Motivo, tempo_Para_Fechar) %>%
  group_by(Motivo) %>%
  summarise(Media = mean(tempo_Para_Fechar),
            total = length(tempo_Para_Fechar)) %>%
  group_by(Motivo)

View(motivos)

# Removendo a primeira linha do Dataset
motivos = motivos[-1,]

media_motivos = arrange(motivos, desc(Media))
View(media_motivos)

media_motivos = media_motivos[0:5,]



# Vizualiando o gráfico com maior tempo de resuloção de problema:

g1 = ggplot(media_motivos, aes(x = Motivo, y = Media)) +
  geom_bar(stat = 'identity', aes(fill = Motivo)) +
  ggtitle('Tempo Médio para resolver cada problema') +
  ylab('Tempo médio') +
  xlab('Motivos') +
  theme_light() +
  geom_label(aes(label = round(Media, 2))) +
  theme(legend.position="none")

# Salvando primeiro arquivo
pdf("Tempo_Medio_Problema.pdf", width = 10, height = 6)
g1
dev.off()


#########################
# Quais paises tem mais problema

pais = df %>%
  select(Pais, Motivo, tempo_Para_Fechar) %>%
  group_by(Pais) %>%
  summarise(total = length(Motivo),
            media = mean(tempo_Para_Fechar))
View(pais)

# Removendo a primeira linha
pais = pais[-1,]

pais = arrange(pais, desc(total), desc(media))
View(pais)

pais2 = pais[0:5,]
pais[1, 2]/ sum(pais$total) * 100
pais[2, 2]/ sum(pais$total) * 100

ggplot(pais2, aes(x= total, fill = pais)) +
  geom_bar(width = 1)+
  coord_polar(theta = "x")

colors()
g2 <- pie(x = pais2$total, labels = pais2$Pais, main = 'Numero de total de Chamados por Paises', col = c('tomato', 'tan4', 'salmon', 'orange', 'lightyellow1'))
g3 = pie(x = pais2$media, labels = pais2$Pais, main = 'Tempo médio para concluir os Chamados por Paises', col = c('tomato', 'tan4', 'salmon', 'orange', 'lightyellow1'))


pdf('Por_Pais.pdf', width = 10, height = 6)
pie(x = pais2$total, labels = pais2$Pais, main = 'Numero de total de Chamados por Paises', col = c('tomato', 'tan4', 'salmon', 'orange', 'lightyellow1'))
pie(x = pais2$media, labels = pais2$Pais, main = 'Tempo médio para concluir os Chamados por Paises', col = c('tomato', 'tan4', 'salmon', 'orange', 'lightyellow1'))

dev.off()


######################

# Quais são os problemas mais recorrentes?
motivos = motivos[-1,]


total_motivos = arrange(motivos, desc(total))
View(total_motivos)
total_motivos = total_motivos[0:5,]

g4 = ggplot(total_motivos, aes(x = Motivo, y = total)) +
  geom_bar(stat = 'identity', aes(fill = Motivo)) +
  ggtitle('Problemas Mais Recorrentes') +
  ylab('N° de Pedidos') +
  xlab('Motivos') +
  theme_light() +
  geom_label(aes(label = total)) +
  theme(legend.position="none")


pdf('Numero_Problemas.pdf', width = 10, height = 6)
g4
dev.off()

#################################################
# Qual canal de suporte mais usado?


names(df)


suporte = df %>%
  select(Canal_Suporte, tempo_Para_Fechar) %>%
  group_by(Canal_Suporte) %>%
  summarise(Media = mean(tempo_Para_Fechar),
            total = length(tempo_Para_Fechar))

View(suporte)
suporte = arrange(suporte, desc(total))
suporte

10822/ (94021 + 10822 + 866) *100

# O canal mais utilizado é o Chat, representando 88% dos chamados.


################################################
# Qual a percepção dos clientes que ligam no call-center ?
df2 = survey

View(df2)
str(df2)
names(df2)
unique(df2$`QualityOfSupport 9pt`)
df2$`Is Issue Resolved` =  as.factor(df2$`Is Issue Resolved`)


Miss_Values = function(x){
  sapply(x, function(x) sum(is.na(x)))
}

Miss_Values(df2)


