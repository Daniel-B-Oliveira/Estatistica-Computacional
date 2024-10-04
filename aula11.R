#Seja x uma v.a. com f.d.p 
#
#f_x(X) =   {x, se 0<=x<=1
#           {2-x, se 1 <= x <= 2
#

#1/2 + [2t - t^2/2]|(x,1) = 1/2 + 2x - x^2/2 - 2 + 1/2 =
#= 2x - x^2/2 - 2 + 1 = 2x - x^2/2 - 1

#facum_x(X) = 
#           {0                se x < 0
#           {x^2/2,           se 0<=x<=1
#           {2x - x^2/2 - 1,  se 1 <= x <= 2
#           {1,               se x > 2


#Para 0 <= x <= 1
#
#u = x^2/2 => x = raiz(2u)

#Para 1 < x <= 2
#u = 2x - x^2/2 -1 => x^2/2 -2x + (1+u) = 0
#delta = 4 - 4*1/2*(1+u) = 4 - 2 - 2u = 2(1-u)
#x = [4 +- raiz(2(1-u))][2*1/2] = 2 +- raiz(2-2u)

f <- function(x){
  ifelse(x>=0,
         ifelse(x<=1,
                x,
                ifelse(x<=2,
                       2-x,
                       0)),
         0)
}

f_acum <- function(x){
  ifelse(x>=0,
         ifelse(x<=1,
                x^2/2,
                ifelse(x<=2,
                       2*x-x^2/2 - 1,
                       1)),
         0)
}

inv_f_acum <- function(u){
  ifelse(u<=1/2, sqrt(2*u),2 - sqrt(2-2*u))
}

n <- 100000
u <- inv_f_acum(runif(n))

hist(u, freq = F, ylim = c(0,1))
plot(f, add=T, xlim = c(0,2), lwd=3, col='red')

#Método da aceitação e rejeição (Gerar valores aleatorios)

g <- function(x){
  ifelse(x>=0 & x <= 1, 20*x*(1-x)^3,0)
}

curve(g)
integrate(g,0,1)
#g(1/4) é valor máximo

xc <- runif(n, 0, 1)
u<- runif(n,0,g(1/4))

plot(xc, u)

x <- xc[u < g(xc)]

hist(x, freq = F)
curve(g, add = T, lwd=3, col='red')

#Integração de Monte Carlo
#EX: mostre que integral(20x(1-x)^3,0,1) = 1
#solução:
#(b-a)E(w) = (b-a)integral(w/(b-a),a,b) ~= aproximado por média
#(1-0)E(20x(1-x)^3) = (1-0)integral(20x(1-x)^3,0,1)

u <- g(runif(n))
mean(u)
