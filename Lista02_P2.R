#Questão 1

f <- function(x){
  ifelse(x>0 & x<1, (2/5)*(x+2),0)
}
curve(f,0,2)

f_acum <- function(x){
  ifelse(x<0, 0, ifelse(x>1, 1, 4/5*x + x^2/5))
}
curve(f_acum, 0, 2)

inv_f_acum <- function(x){
  return(sqrt(5*x+4)-2)
}

#a)
n <- 100000
u <- inv_f_acum(runif(n))

hist(u, freq = F, ylim = c(0,2), xlim = c(0,2))
plot(f, add=T, xlim = c(0,2), lwd=3, col='red')

integrate(f,0,1)
mean(f(runif(n)))
#b)#Valor Máximo da Função é f(1) = 6/5

xc <- runif(n)
u <- runif(n, 0, 6/5)

plot(xc,u)

x <- xc[u < f(xc)]

hist(x, freq = F)
curve(f, add=T, lwd=3, col='red')

#2
#a)
f_values <- f(runif(n))
mean(f_values)
integrate(f,0,1)

#b)
ef <- function(x){
  ifelse(x>0 & x<1, x*(2/5)*(x+2),0)
}
ef_values <- ef(runif(n))
mean(ef_values)
integrate(ef,0,1)

#3

g <- function(x){
  r <- exp(-x) / (1 + x^2)
  return(r)
}
plot(g,0,10)

g_v <- g(runif(n,0,10))

g_v

mean(g_v)*(10-0)










