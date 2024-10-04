#Geracao de variaveis aleatórias

n <- 10000

#Método congruencial
#U_(i+1) = (a.U_i + c) mod m
#
#Seja U_0 = a = c = 7 e m = 10  (exemplo ruim)
#U_1 = (7x7 + 7) = 56 => 56 mod 10 = 6
#U_2 = (7x6 + 7) = 49 => 49 mod 10 = 9
#U_3 = (7x9 + 7) = 70 => 70 mod 10 = 0
#U_4 = (7x0 + 7) = 7 => 7 mod 10 = 7
#U_5 = (7x7 + 7) = 56 => 56 mod 10 = 6    Igual U_1

a <- 7^5
m <- 2^31 - 1

u <- vector()
u[1] <- 10

for(i in 1:n){
  u[i+1] <- ((a*u[i])%%m)
}

max(table(u))
hist(u)
hist(20*u/m)

#Função gcl = gedarado de congruência

#Método da tranformada inversa

#f(x) = le^(-lx)
#F(x) = 1 - e^(-lx)
#u = F(x) = 1 - e^(-lx) => x = -(ln(1-u))/l

n <- 1000000
u <- runif(n)
x <- -log(1-u)/2

par(mfrow=c(1,1))
hist(x, freq = F)
curve(dexp(x,2), add=T, col='red', lwd=3)


#Amostragem por rejeição

f <- function(x){
  ifelse(x<1, f <- x, f <- (2-x))
}

plot(f, xlim = c(0,2))

n <- 100000
x <- runif(n,0,2)
u <- runif(n,0,1)
    
plot(x,u)
plot(f, xlim = c(0,2), add=T, lwd=3, col='red')
  
  
  
  
w <- x[u<f(x)]
hist(w)
hist(w, freq=F, ylim = c(0,1))
plot(f, xlim=c(0,2), add=T, lwd=3)























