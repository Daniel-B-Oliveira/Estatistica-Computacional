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

#b) Tanto no R quanto no Python, exclua a variavel Id do banco de dados.
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

coef <- function(x){
  return(100*var(x)/mean(x))
}

medidas_indexadas <- function(x,y){
  print(tapply(x, y, mean))
  print(tapply(x, y, median))
  print(tapply(x, y, min))
  print(tapply(x, y, max))
  print(tapply(x, y, var))
  print(tapply(x, y, sd))
  print(tapply(x, y, coef))
}

str(dados_numricos)

for(i in 1:ncol(dados_numricos)){
  print(names(dados_numricos)[i])
  medidas_indexadas(dados_numricos[,i], dados$Turma)
}


#Apresentão variações em relação as turmas

# e)Voce utilizaria o histograma para descrever graficamente todas as variaveis
# numericas deste banco de dados? Justifique.

for(i in 1:ncol(dados_numricos)){
  h <- hist(dados_numricos[,i], plot = FALSE)
  plot(h, xlab = names(dados_numricos)[i],
       main = "Histograma")
}

names(dados_numricos)
# Não, utilizaria apenas para variaveis continuas ou
# que nao estivessem tão dispersas



# f)Descreva todas as variaveis por meio de graficos observando o tipo de
# grafico mais adequado para cada uma.

#ruins em histograma: filhos, idade

dados_graficos1 <- dados[, c(-3, -6)]
dados_graficos2 <- dados[, c(3, 6)]

for(i in 1:ncol(dados_graficos1)){
  if(class(dados_graficos1[,i]) == "factor"){
    barplot(table(dados_graficos1[,i]),
            xlab =  names(dados_graficos1)[i],
            main = "Gráfico de barras")
  }else{
    h <- hist(dados_graficos1[,i], plot = FALSE)
    plot(h, xlab = names(dados_graficos1)[i],
         main = "Histograma")
  }
}

for(i in 1:ncol(dados_graficos2)){
  barplot(table(dados_graficos2[,i]),
          xlab =  names(dados_graficos2)[i],
          main = "Gráfico de barras")
}

# g)Descreva todas as variaveis pormeio de graficos observando o tipo de grafico
# mais adequado para cada uma, por turma. Opadrao das distribuicoes parece
# similar nas duas turmas? Justifique. (duvida)

dados_graficos3 <- dados_graficos2
dados_graficos3$Turma <- dados_graficos1$Turma

for(i in 1:ncol(dados_graficos1)){
  if(class(dados_graficos1[,i]) == "factor"){
    
    t1 <- table(dados_graficos1[dados_graficos1$Turma == "A",i])
    t2 <- table(dados_graficos1[dados_graficos1$Turma == "B",i])
    
    barplot(t1, xlab =  names(dados_graficos1)[i],
            ylim=c(0,max(max(t1),max(t2)))*1.3,
            ylab = "Frequencia", main = "Grafico de barras",
            density=30, col = "red", angle=45)
    
    barplot(t2, add=T, col="blue", density=30, angle=135)
    
    legend("top", c("A", "B"), fill=c("red","blue"),density=30,
           title="Turma", angle=c(45,135), title.font=2)
    
  }else{
    h1 <- hist(dados_graficos1[dados$Turma == "A",i], plot = FALSE)
    h2 <- hist(dados_graficos1[dados$Turma == "B",i], plot = FALSE)
    
    plot(h1,xlab=names(dados_graficos1)[i],
         ylim=c(0,max(max(h1$counts),max(h2$counts)))*1.3,
         ylab="Frequência",main="Histograma", 
         density=30,col="red", angle=45)
    
    plot(h2,add=T,col="blue",density=30,angle=135)
    
    legend("top",c("A","B"),fill=c("red","blue"),density=30,
           title="Turma",angle=c(45,135),title.font=2)
  }
}

str(dados_graficos3)

for(i in 1:ncol(dados_graficos3)){
  t1 <- table(dados_graficos3[dados_graficos3$Turma == "A",i])
  t2 <- table(dados_graficos3[dados_graficos3$Turma == "B",i])
  
  barplot-.(t1, xlab =  names(dados_graficos3)[i],
          ylim=c(0,max(max(t1),max(t2)))*1.3,
          xlim = c(0, max(max(t1), max(t2))),
          beside = T,
          ylab = "Frequencia", main = "Grafico de barras",
          density=30, col = "red", angle=45)
  
  barplot(t2, add=T, col="blue", density=30, angle=135)
  
  legend("top", c("A", "B"), fill=c("red","blue"),density=30,
         title="Turma", angle=c(45,135), title.font=2)
}


# Sabe-se que quanto maior a amostra menor serao erro de estimativa. Para
# ilustrar este fato, gere amostras de uma distribuicao normal com media 30 e
# desviopadrao 3 (µ=30 eσ=3 ) ,comecando com uma amostrade tamanhoum(n=1) e
# aumentando de uma em uma unidade, iterativamente, ate que o erro de estimativa
# seja menor que 0,001. Lembre-se que o estimador natural de µ e amedia amostral
# (¯ x) e portanto as amostras devem aumentar a cada iteracao ate que|¯ x−30|<0,00
# .Utilize a estrutura while no R.

n <- 1
dif <- 30

?abs
while(dif >= 0.001){
  dif <- abs(mean(rnorm(n, 30, 3)) - 30)
  n <- n+1
}

print(n)
print(dif)

#Questão 3 lista 1
n <- c(10,15,20,30,40,50,70,100,200)

for(i in 1:length(n)){
  dist_gama <- rgamma(n[i], 2, 10)
  hist(dist_gama)
  print(mean(dist_gama))
  
}

media_gama <- 2/10
abs(media_gama - mean(dist_gama))

#Questão 4

#a)
p_nao_ven <- 1 - pbeta(0.4, 4,2)
p_nao_ven
p_ven <- 1 - p_nao_ven
p_ven

#b
bp_nao_ven <- 1 - pbeta(0.4, 2,4)
bp_nao_ven
bp_ven <- 1 - bp_nao_ven
bp_ven

curve(dbeta(x, 4, 2))
curve(dbeta(x, 2, 4), add = T)
