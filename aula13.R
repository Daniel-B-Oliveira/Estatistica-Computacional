#Modelo de Regressão Linear bootstrap
set.seed(123)

x0 <- 11:20
y0 <- 4 + 5*x0 + rnorm(10,0,2)

xy <- data.frame(x0, y0)

B <- 5000

tboot <- matrix(NA, nrow=B, ncol = 3)

for(i in 1:B){
  s <- 1:10
  u <- sample(s, 10, replace = T)
  dados <- xy[u,]
  x <- dados[,1]
  y <- dados[,2]
  ajuste <- lm(y~x)
  tboot[i,] <- c(summary(ajuste)$coefficients[,1],
             summary(ajuste)$r.squared)
  }

b0 <- mean(tboot[,1]); b0
b1 <- mean(tboot[,2]); b1
r2 <- mean(tboot[,3]); r2

bo.IC <- quantile(tboot[,1], prob=c(0.025, 0.975)); bo.IC
bi.IC <- quantile(tboot[,2], prob=c(0.025, 0.975)); bi.IC
r2.IC <- quantile(tboot[,3], prob=c(0.025, 0.975)); r2.IC

#Regressão Múltipla

dados <- read.table("preco_frangos.txt", sep = "\t", dec = ",", header = T)
summary(dados)

#  Y é o consumo per cápita de frango (em libras); 
# X1 é a renda real per capita disponível; 
# X2 é o preço real do frango no varejo; 
# X3 é o preço real da carne de porco no varejo; 
# X4 é o preço real da carne de vaca no varejo e 
# X5 é o preço real composto pelos substitutos do frango
# (média ponderada)

#Geração das amostras bootstrap
set.seed(123)
B <- 2000

b <- matrix(NA, nrow = B, ncol = 6)
r2 <- vector()

for (i in 1:B){
  s <- 1:nrow(dados)
  u <- sample(s, replace = T)
  dados2 <- as.matrix(dados[u,])
  y <- dados2[,1]
  x1 <- dados2[,2]
  x2 <- dados2[,3]
  x3 <- dados2[,4]
  x4 <- dados2[,5]
  x5 <- dados2[,6]
  
  modelo <- lm(y ~x1+x2+x3+x4+x5)
  b[i,] <- summary(modelo)$coefficients[,1]
  r2[i] <- summary(modelo)$r.squared
}

colMeans(b)
bs <- mean(b)
R2 <- mean(r2); R2


b1.IC <- quantile(bs[1],prob=c(0.025, 0.975)); b1.IC

hist(b[,2])
p = length(b[b[,2]<0,2])/B; p

valorp_betal = 2*p
valorp_betal





