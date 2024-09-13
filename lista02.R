#Suponha que o tempo necessario para atendimento de clientes
#em uma central de atendimento telefonico siga
#uma distribuicaoao normal de media de 8 minutos e desvio padrao de 2 minutos.
#
#a) Qual e a probabilidade de que um atendimento dure menos de 5 minutos?

pro_a <- pnorm(5,8,2)

#b) E mais do que 9,5 minutos?
pro_b <- 1-pnorm(9.5,8,2)

#c) E entre 7 e 10 minutos?
prob_c <- pnorm(10,8,2) - pnorm(7,8,2)

#d) 75% das chamadas telefonicas requerem pelo menos
#quanto tempo de atendimento?
?pnorm
quartil_d <- qnorm(0.25, 8,2)

#e) 75% das chamadas telefonicas requerem no maximo
#quanto tempo de atendimento?

quartil_e <- qnorm(0.75,8, 2)

#f) Qual e o tempo mediano de atendimento?
#O tempo e 8 minutos

prob_f <- pnorm(0.5, 8, 2)

#g) Gere amostras de tamanho 5 (n=5), 100 (n=100), 1000 (n=1000) e 10000 (n=10000)
#e cada caso calcule a media e o desvio padrao. O que voce pode afirmar sobre
#o erro amostral em cada caso?

n <- c(5,100, 1000, 10000)
ys <- vector()

for(i in 1:4)
  ys[i] <- rnorm(n[i],8,2)


y1 <- rnorm(5,8,2)
mean(y1)
sd(y1)  


#i) Construa (desenhe) o grafico ou a curva da distribuicao normal
#apresentada nesta questao e neste mesmo grafico insira (desenhe) tres pontos
#(x, f(x)) sendo um deles (µ, f(µ)). Edite o grafico para que estes pontos 
#fiquem bem destacados.

# z = (x - u)/dp , x = u + 1,96
?curve
?points

curve(dnorm(x,8,2), to = 0, from = 16)
points(8, dnorm(8,8,2), cex = 2, col = "Red", pch = 19)
points(2, dnorm(2,8,2), cex = 2, col = "Blue", pch = 19)
points(10, dnorm(10,8,2), cex = 2, col = "Green", pch = 19)



#Considere que a variavel T tenha distribuicao t de Student com v graus de
#liberdade. Em cada um dos casos a seguir, encontre a mediana, o primeiro e o
#terceiro quartis e o intervalo interquartil. Em qual caso o intervalo
#interquartil e menor? Qual o significado pratico disso?
  

?qt
#a) v = 2;
aq1 <- qt(0.25, 2)
aq2 <- qt(0.5, 2)
aq3 <- qt(0.75, 2)

adif <- aq3 - aq1

#b) v = 5;

bq1 <- qt(0.25, 5)
bq2 <- qt(0.5, 5)
bq3 <- qt(0.75, 5)

bdif <- bq3 - bq1

#c) v = 10;

cq1 <- qt(0.25, 10)
cq2 <- qt(0.5, 10)
cq3 <- qt(0.75, 10)

cdif <- cq3 - cq1

#d) v = 20;

dq1 <- qt(0.25, 20)
dq2 <- qt(0.5, 20)
dq3 <- qt(0.75, 20)

ddif <- dq3 - dq1

#e) v = 100.

eq1 <- qt(0.25, 100)
eq2 <- qt(0.5, 100)
eq3 <- qt(0.75, 100)

edif <- eq3 - eq1

v_interqualtil <- function(v)
  {
    return(qt(0.75, v) - qt(0.25, v))
  }

#Considere a distribui¸c˜ao normal padr˜ao e as distribui¸c˜oes t de Student
#para os graus de liberdade v = 2, v = 10, v = 20 e v = 100. Fa¸ca o gr´afico
#de todas em uma mesma figura. O que se pode afirmar da distribui¸c˜ao t de
#Student em rela¸c˜ao `a normal padr˜ao a medida que os graus de liberdade aumenta?

curve(dnorm(x), to = -4, from = 4, lwd = 4)
curve(dt(x,2), add = T, col = "BLue", lwd = 2, lty = 2)
curve(dt(x,10), add = T, col = "Red", lwd = 2, tly = 3)
curve(dt(x,10), add = T, col = "Green", lwd = 2, lty = 4)
curve(dt(x,20), add = T, col = "Purple", lwd = 2, lty = 5)
curve(dt(x,100), add = T, col = "Yellow", lwd = 2, lty = 6)

#Econtre a mediana para as distribuicoes contiınuas:
#a)N(µ = 8, σ2 = 4);
ma <- qnorm(0.5, 8, 2)

#b) Unif(10, 20);
mb <- qunif(0.5,10,20)

#c) Exp(4);
mc <- qexp(0.5, 4)

#d) Gama(4, 10);
md <- qgamma(0.5,4,10)
?qgamma

#e) Beta(4, 10);
me <- qbeta(0.5, 4, 10)

#Para cada uma das distribuicao apresentada na questao anterior)
#gere uma amostra de tamanho 5 calcule a m´edia e a variancia e compare estes
#valores amostrais com os valores populacionais. Repita o processo novamente
#por´em com amostra de tamanho 10000. Observe que no segundo caso
#(com n = 10000) as estimativas sao mais precisas.

#a)
ay_n5 <- rnorm(5, 8, 2)
ay_n10 <- rnorm(10000, 8, 2)

mean(ay_n5)
mean(ay_n10)

var(ay_n5)
var(ay_n10)

#b)

by_n5 <- runif(5, 10, 20)
by_n10 <- runif(10000, 10, 20)

mean(by_n5)
mean(by_n10)

var(by_n5)
var(by_n10)

var_uni <- 100/12

#c)

cy_n5 <- rexp(5,4)
cy_n10 <- rexp(10000,4)

mean(cy_n5)
mean(cy_n10)
meam_exp <- 1/4
meam_exp

var(cy_n5)
var(cy_n10)
var_exp <- 1/4**2
var_exp

#d)

dy_n5 <- rgamma(5,4,10)
dy_n10 <- rgamma(10000,4,10)

mean(dy_n5)
mean(dy_n10)
mean_gamma <- 4/10
mean_gamma

var(dy_n5)
var(dy_n10)
var_gamma <- 4/100
var_gamma

#e
ey_n5 <- rbeta()