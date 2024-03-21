install.packages(c("summarytools", "fdth", "worldcloud2", "tm", "ggplot2", "readxl", "readr", "dplyr", "stringr", "tidyverse"))

require(stringr)
require(summarytools)
require(fdth)
require(ggplot2)
require(readxl)
library(wordcloud2)
library(tm) # biblioteca de text mining para limpar o texto (wordcloud)
library(readr)
library(tidyverse)
library(dplyr)
#renomear o arquivo que foi baixado do portal de dados abertos brasileiro 

dadosRodovia <- demostrativo_acidentes_novadutra

#tratamento dos dados

#como os dados de 2022 estão incompletos (apenas 4 meses) eles serão desconsiderados na análise
dadosRodovia$data <- as.Date(dadosRodovia$data, format= "%d/%m/%Y")
dadosRodovia <- filter(dadosRodovia, data >= "2010-01-01", data <= "2021-12-31")

#o dataset original conta com celulas em branco representando 0. para as analises é necessário substituir todas as celulas em branco por 0
dadosRodovia[is.na(dadosRodovia)] <- 0

#criar uma nova coluna no dataframe com a soma de todos os envolvidos na ocorrencia = qtde_pessoas
dadosRodovia$qtde_pessoas = rowSums(dadosRodovia[,c("ilesos", "levemente_feridos", "moderadamente_feridos", "gravemente_feridos", "mortos")])

#criar uma nova coluna no dataframe usando o horario para determinar em qual periodo do dia a ocorrencia foi registrada
dadosRodovia$periodo = substring(dadosRodovia$horario, 0, 2)
dadosRodovia['periodo'][dadosRodovia['periodo'] == '00' | dadosRodovia['periodo'] == '01' | dadosRodovia['periodo'] == '02' | dadosRodovia['periodo'] == '03' | dadosRodovia['periodo'] == '04' | dadosRodovia['periodo'] == '05'] <- 'Madrugada (00h - 05h)'
dadosRodovia['periodo'][dadosRodovia['periodo'] == '06' | dadosRodovia['periodo'] == '07' | dadosRodovia['periodo'] == '08' | dadosRodovia['periodo'] == '09' | dadosRodovia['periodo'] == '10' | dadosRodovia['periodo'] == '11'] <- 'Manhã (06h - 11h)'
dadosRodovia['periodo'][dadosRodovia['periodo'] == '12' | dadosRodovia['periodo'] == '13' | dadosRodovia['periodo'] == '14' | dadosRodovia['periodo'] == '15' | dadosRodovia['periodo'] == '16' | dadosRodovia['periodo'] == '17'] <- 'Tarde (12h - 17h)'
dadosRodovia['periodo'][dadosRodovia['periodo'] == '18' | dadosRodovia['periodo'] == '19' | dadosRodovia['periodo'] == '20' | dadosRodovia['periodo'] == '21' | dadosRodovia['periodo'] == '22' | dadosRodovia['periodo'] == '23'] <- 'Noite (18h - 23h)'

#para a analise é necessário padronizar os dados, a seguir o campo tipo_de_ocorrencia é padronizado utilizando os parametros 'sem vitima' ou 'com vitima'
#a planilha original possui acentuação na palavra vítima e por conta disso é necessário uma correção

dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "sem v'tima", "sem vitima") 
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Acidente sem vitima", "sem vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Acidente sem vítima", "sem vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Atropelamento sem morte", "sem vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "AC03 - Acidente sem VITIMA", "sem vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Sem v'tima", "sem vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Sem vítima", "sem vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "sem vítima", "sem vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "AC04 - Atropelamento", "sem vitima")

dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "com vítima", "com vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "AC01 - Acidente com VITIMA FATAL", "com vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Acidente com morte", "com vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "AC02 - Acidente com VITIMA", "com vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Acidente com vitima", "com vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Acidente com vítima", "com vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "AC05 - Atropelamento Fatal", "com vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Atropelamento com morte", "com vitima")
dadosRodovia$tipo_de_ocorrencia = str_replace_all(dadosRodovia$tipo_de_ocorrencia, "Com vítima", "com vitima")

#elaborar o grafico de setores com a coluna trecho do dataframe 
cols <- c("yellow2","midnightblue")
pielabels<- paste(round(table(dadosRodovia$trecho)/length(dadosRodovia$trecho)*100), "%", sep="")
pie(round((table(dadosRodovia$trecho)/length(dadosRodovia$trecho)*100),2),labels=pielabels, cex=1.3, col=cols)
legend("bottomright", c("Rio de Janeiro","São Paulo"), cex = 1.1, fill = c("yellow2","midnightblue"))

#grafico de setores utilizando o campo periodo para encontrar em qual periodo mais aconteceram ocorrencias
cols <- c("red", "dodgerblue3", "green", "orange")
pielabels<- paste(round(table(dadosRodovia$periodo)/length(dadosRodovia$periodo)*100), "%", sep="")
pie(round((table(dadosRodovia$periodo)/length(dadosRodovia$periodo)*100),2),labels=pielabels, cex=1.3, col=cols)
legend("bottomright", c("Madrugada (00h - 05h)","Manhã (06h - 11h)", "Tarde (12h - 17h)", "Noite (18h - 23h)"), cex = 1.1, fill = c("red", "dodgerblue3", "green", "orange"))

#extrair o ano da coluna data que possui ano/mes/dia e elaborar o grafico de barras com quantidade de acidentes x ano
anosAcidentes = select(dadosRodovia, data) #extrai a coluna data do dataset
anosAcidentes = format(as.Date(anosAcidentes$data, format="%d/%m/%Y"),"%Y") # o formato de data é dia/mês/ano, para a primeira análise foi considerado apenas o ano
barplot(table(anosAcidentes), xlab = "Ano", ylab = "Quantidade de Acidentes", cex.lab=1.0, cex.names=1.0, cex.axis=1.0, col="steelblue2", ylim=c(0,13000))

#elaborar o grafico de barras com 2 variaveis qualitativas (tipo de ocorrencia x trecho)
percentData <- dadosRodovia %>% group_by(`trecho`) %>% count(`tipo_de_ocorrencia`) %>%  mutate(ratio=scales::percent(n/sum(n)))
ggplot(dadosRodovia, aes(x=factor(trecho),fill=factor(tipo_de_ocorrencia))) + 
  geom_bar(position="fill") + 
  geom_text(data=percentData, aes(y=n,label=ratio), position=position_fill(vjust=0.5))+
  xlab("Trecho") +
  ylab("Proporção de ocorrencias") + 
  scale_fill_manual(name="Tipo de Ocorrencia", values = c("darksalmon", "aquamarine3"))

#janela com vários gráficos de uma variável quantitativa (quantidade de carros envolvidos nas ocorrencias)
quantidadeCarros <- dadosRodovia$automovel
quantidadeCarros <- as.integer(quantidadeCarros)
quantidadeCarros <- sort(quantidadeCarros)

tab=fdt(quantidadeCarros, start=0,h=1,end=15) 
par(mfrow=c(1,3))
boxplot(quantidadeCarros, ylab = "Quantidade de Automoveis", cex.axis=1.6, cex.lab=1.6) 
points(mean(quantidadeCarros), pch=3)
plot(tab, type='rfph', xlab="Quantidade de Automoveis",ylab="% ocorrencias", cex.axis=1.6, cex.lab=1.6) 
par(new=TRUE)
plot(tab,type='rfpp', xlab="Quantidade de Automoveis",ylab="", col = "black",cex.axis=1.6, cex.lab=1.6)
plot(tab,type='cfpp', xlab="Quantidade de Automoveis",ylab="% acumulados", ylim=c(0,100), col = "black", cex.axis=1.6, cex.lab=1.6)

mean(quantidadeCarros) #média
median(quantidadeCarros) #mediana
sd(quantidadeCarros) #standard deviation = desvio padrão
(sd(quantidadeCarros)/mean(quantidadeCarros))*100 # coeficiente de variação

#diagrama de dispersao, spearman e pearson
par(mar=c(4,4,2,0), oma=c(0,0,0,10))
plot(dadosRodovia$onibus~dadosRodovia$qtde_pessoas, xlab = "Quantidade de pessoas envolvidas", ylab = "Quantidade de Onibus envolvidos", col = "blue")
abline(lm(dadosRodovia$onibus~dadosRodovia$qtde_pessoas), col= "red")

pearson <- cor(dadosRodovia$onibus, dadosRodovia$qtde_pessoas); pearson
spearman <- cor(dadosRodovia$onibus, dadosRodovia$qtde_pessoas, method="spearman"); spearman

#associação entre duas variaveis qualitativas (trecho x periodo)
percentData <- dadosRodovia %>% group_by(tipo_de_ocorrencia) %>% count(periodo) %>%  mutate(ratio=scales::percent(n/sum(n)))
ggplot(dadosRodovia, aes(x=factor(tipo_de_ocorrencia),fill=factor(periodo))) + 
  geom_bar(position="fill") + 
  geom_text(data=percentData, aes(y=n,label=ratio ), position=position_fill(vjust=0.5))+
  xlab("Tipo de Ocorrencia") +
  ylab("Proporção de Ocorrencias")+
  scale_fill_manual(name="Periodo", values = c("red", "dodgerblue3", "green", "orange"))

#criação de uma nuvem de palavras utilizando a coluna tipo_de_acidente
#fonte https://www.youtube.com/watch?v=0cToDzeDLRI&ab_channel=Dataslice

medium.corpus = Corpus(VectorSource(dadosRodovia$tipo_de_acidente))
medium.corpus = medium.corpus %>% 
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower))

tdm = TermDocumentMatrix(medium.corpus) %>%
  as.matrix()
words = sort(rowSums(tdm), decreasing = TRUE)
df = data.frame(word = names(words), freq = words)
wordcloud2(df, size = 1.0, minSize = 2, rotateRatio = 0)

#associação entre uma variavel quantitativa x uma variavel qualitativa
ggboxplot(dadosRodovia$automovel ~ dadosRodovia$periodo,xlab = "Periodo", ylab = "Quantidade de Automoveis")
points(1:nlevels(dadosRodovia$periodo), tapply(dadosRodovia$automovel, dadosRodovia$periodo, mean),  pch=3)
