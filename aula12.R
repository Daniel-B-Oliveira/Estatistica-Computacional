f <- function(x){
  ifelse(x>=0 & x <= 1, 20*x*(1-x)^3,0)
}

u <- runif(100000,0,1)
mean(f(u))

g <- function(x){
  ifelse(x>=0 & x<=1, x*20*x*(1-x)^3,0)
}

u <- runif(100000,0,1)
esp <- mean(g(u))
esp
1/3       #Valor analítico

#Var(x) = E(x^2) - E(x)^2

h <- function(x){
  ifelse(x>=0 & x<=1, x^2*20*x*(1-x)^3,0)
}
u <- runif(100000,0,1)
esp.quad <- mean(h(u))

varia <- esp.quad - esp^2
varia
2/63      #valor analítico
