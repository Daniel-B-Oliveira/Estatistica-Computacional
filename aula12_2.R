#Média e variância via bootstrap

set.seed(117532)

dados <- rpois(20,4)
dados
hist(dados)

B <- 5000

media <- vector()
v <- vector()

# X(barra) ~ N(u, s^2/n)



for(i in 1:B){
  media[i] <- mean(sample(dados, replace = TRUE))
  v[i] <- var(sample(dados, replace = TRUE))
}

mean(media)
mean(v)

quantile(media, prob = c(0.05, 0.95))
quantile(v, prob = c(0.05, 0.95))
#Intervalo de bootstrap (90%)

#Correlação linear simples

set.seed(123)

x0 <- 11:20
y0 <- 4 + 5*x0 + rnorm(10,0,2)

x0 #Em geral são dados coletados/observados
y0 #Em geral são dados coletados/observados

r_boot = matrix(NA, nrow = B, ncol = 1)

xy <- data.frame(x0, y0)

for(i in 1:B){
  u <- sample(1:10, replace = TRUE)
  dados <- xy[u, ]
  x <- dados[, 1]
  y <- dados[, 2]
  r_boot[i, ] <- cor(x,y)
}

mean(r_boot)  #Estimativa pontual
var(r_boot)
sd(r_boot)
quantile(r_boot, probs = c(0.025, 0.975))

cor(x0,y0)


