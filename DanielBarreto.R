#Questão 1
f <- function(x){
  ifelse(x>0 & x<5, (3/125)*x^2,0)
}

#maximo é (3/125)*(5^2) = 3/5
n <- 1000
xc <- runif(n,0,5)
u <- runif(n,0,3/5)

curve(f,0,10)

plot(xc,u)

x <- xc[u < f(xc)]
hist(x, freq = F)
curve(f, add=T, lwd=3, col='red')

#Questão 2
estado0 <-c(1,0,0)

#0    1/2   1/2
#1/3  0     2/3
#1/3  2/3   0

v <- c(0,1/3,1/3, 1/2,0,2/3, 1/2, 2/3, 0)
M <- matrix(v,3,3)
M

ps <- matrix(NA, 13, 3)
ps[1,] <- estado0

for(i in 2:13){
  ps[i,] <- ps[i-1,]%*%M
}

ps

#QUesta 2 b
ps <- matrix(NA, 100, 3)
ps[1,] <- estado0

for(i in 2:100){
  ps[i,] <- ps[i-1,]%*%M
}

ps

