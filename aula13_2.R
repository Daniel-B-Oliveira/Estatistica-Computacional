#Cadeia de Markov

#[p1 p2]([0,7 0,3])^n
#       ([0,1 0,9])

#P_0 = {0,45; 0,55}

0.1*0.45 + 0.7*0.55

0.9*0.45 + 0.3*0.55

#P_1 = {0,43 ; o,57}

0.1*0.57 + 0.7*0.43
0.9*0.57 + 0.3*0.43

f <- function(v){
  a <- 0.1*v[1] + 0.7*v[2]
  b <- 0.9*v[1] + 0.3*v[2]
  return(c(a,b))
}

v <- matrix(NA, 10, 2)
v[1,] <- c(0.45, 0.55)

for(i in 2:10){
  v[i,] <- f(v[i-1,])
}
v
#Professor
M <- matrix(c(0.7, 0.1, 0.3,0.9), 2, 2)
M

?matrix
iter = 10

estado0 <- c(0.55, 0.45)
estado1 <- estado0%*%M
estado1

p <- matrix(NA, iter, 2)
p[1,] <- estado0

for(i in 2:iter){
  p[i,] <- p[i-1,]%*%M
}
p

#Exercicio esquina

s <- rep(c(0,0.5),2)
r <- rep(c(s,rev(s)),2)

M <- matrix(r,4,4);M

estado0 <- c(0,1,0,0)
p <- matrix(NA, iter, 4)
p[1,] <- estado0

for(i in 2:iter){
  p[i,] <- p[i-1,]%*%M
}
p

plot.ts(p[,1])

#EXEMPLO

#[1.0 0.0 0.0 0.0 0.0]
#[0.5 0.0 0.5 0.0 0.0]
#[0.0 0.5 0.0 0.5 0.0]
#[0.0 0.0 0.5 0.0 0.5]
#[0.0 0.0 0.0 0.0 1.0]


?matrix
t <- c(1, 0, 0, 0, 0,
       0.5, 0, 0.5, 0, 0,
       0, 0.5, 0, 0.5, 0,
       0, 0, 0.5, 0, 0.5,
       0, 0, 0, 0, 1)

M <- matrix(t, 5, 5, TRUE)

estado0 <- c(0,0.5,0,0.5,0)

iter = 100
p <- matrix(NA, iter, 5)
p[1,] <- estado0

for(i in 2:iter){
  p[i,] <- round(p[i-1,]%*%M, 4)
};p




