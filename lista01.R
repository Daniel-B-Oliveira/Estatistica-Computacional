#Considere os dados a seguir, os quais constam no livro “Nocoes deProbabilidade
#e Estatıstica” (MAGALHAESe LIMA, 2004) e tambem estao disponibilizado
#no Teams.

dados <- read.csv("MagalhaesLima.csv", header = T)


summary(dados)

dados$Turma <- as.factor(dados$Turma)
dados$Sexo <- as.factor(dados$Sexo)
dados$Fuma <- as.factor(dados$Fuma)
dados$Toler <- as.factor(dados$Toler)
dados$OpCine <- as.factor(dados$OpCine)
dados$OpCine <- as.factor(dados$OpCine)
dados$OpTV <- as.factor(dados$OpTV)

#a) Mostre que nao existem dados ausentes
dados_ausentes <- sum(is.na(dados))
dados_ausentes

#b) Tanto no R quanto no Python, exclua a vari´avel Id do banco de dados.
dados <- dados[,-1]

#c) Apresente a meedia, mediana, valor mınimo e valor maximo, variancia,
#desvio padrao e coeficiente de variacao para todas as variaveis
#quantitativas.

dados_numricos <- subset(dados, select = sapply(dados, is.numeric))
dados_numricos

cv_func <- function(x){
  return(100*var(x)/mean(x))
}

medidas_resumo <- data.frame(
  media <- sapply(dados_numricos, mean),
  mediana <- sapply(dados_numricos, median),
  minimo <- sapply(dados_numricos, min),
  maximo <- sapply(dados_numricos, max),
  variacia <- sapply(dados_numricos, var),
  dp <- sapply(dados_numricos, sd),
  cv <- sapply(dados_numricos, cv_func)
)

medidas_resumo

#d) Apresente a media, mediana, valor mınimo e valor maximo, variancia, desvio
#padrao e coeficiente de variacao para todas as variaveis quantitativas,por
#turma. As turmas parecem similar quanto a media em todas as variaveis?
#E quanto a dispersao? Justifique.


medias_turma <- data.frame(
)


for(i in 1:ncol(dados_numricos)){
  print(names(dados_numricos)[i])
  #teste <- data.frame(tapply(dados_numricos[,i],dados$Turma, mean ))
  print(tapply(dados_numricos[,i], dados$Turma, mean))
  print(tapply(dados_numricos[,i],dados$Turma, min ))
  print(tapply(dados_numricos[,i],dados$Turma, max ))
  print(tapply(dados_numricos[,i],dados$Turma, var ))
  print(tapply(dados_numricos[,i],dados$Turma, sd ))
  print(tapply(dados_numricos[,i],dados$Turma, cv_func ))
  medias_turma <- rbind(medias_turma, data.frame(tapply(dados_numricos[,i],dados$Turma, mean )))
  
}

medidas_resumo <- function(x) {
  c(
    media = mean(x, na.rm = TRUE),
    mediana = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    variancia = var(x, na.rm = TRUE),
    desvio_padrao = sd(x, na.rm = TRUE),
    coef_var = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
  )
}

resultado <- aggregate(. ~ turma, data = , FUN = function(x) {
  medidas_resumo(x)
})








