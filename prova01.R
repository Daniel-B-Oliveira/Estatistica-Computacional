dados <- read.delim2("~/URSOS2.txt")

medidas_indexadas <- function(x,y){
  print(tapply(x, y, mean))
  print(tapply(x, y, var))
}

exercicio_5 = c('IDADE','COMPCAB', 'LARGCAB', 'PESC','COMPCOR', 'PEITO', 'PESO')

medidas_indexadas(dados$IDADE, dados$SEXO)
medidas_indexadas(dados$COMPCAB, dados$SEXO)
medidas_indexadas(dados$LARGCAB, dados$SEXO)
medidas_indexadas(dados$PESC, dados$SEXO)
medidas_indexadas(dados$COMPCOR, dados$SEXO)
medidas_indexadas(dados$PEITO, dados$SEXO)
medidas_indexadas(dados$PESO, dados$SEXO)

attach(dados)
var.test(COMPCOR~SEXO)
?var.test

t.test(COMPCOR, SEXO, paired = F)

#Questão 4

?dpois

#a)

pa <- ppois(4,1) - ppois(2,1)
pa

#b)
pb <- ppois(2,1) - ppois(1,1)
pb

#c)
mediana <- qpois(0.5, 1)
mediana

#d)

ns <- c(5,50,500,5000,50000)

varia <- vector()
media <- vector()

h <- 1

for(i in ns){
  amostra <- rpois(i, 1)
  varia[h] <- var(amostra)
  media[h] <- mean(amostra)
  h <- h + 1
}

media
varia
