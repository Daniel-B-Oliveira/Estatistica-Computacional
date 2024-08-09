
#############   AULA 1 - R

# Fontes

# https://cran.r-project.org/

# IDEs para R:
# R Studio => https://www.rstudio.com/
# Jupyter Notebook
# Vim
# RKWard
# StatET
# R Commander
# Architect
# Displayr


## O R:
R.version
citation()


## LEITURA FORTEMENTE SUGERIDA:
# https://bookdown.org/wevsena/livro_curso_r/




#############################
#######  O R COMO CALCULADORA


2+2

2-2

2*2

2/2

exp(-2)

exp(1)


log(exp(1))


x<-2 
z = 3
w='Brasil'
class(x);class(z);class(w)

x+z

x-z

x*z

x/z

x

print(x)
sqrt(x)

x^2

x^3

y<-c(1,5,3,2,0.5)
class(y)

prod(y)
sum(y)


# Entrada de dados e tipo
# de variável

k=c('Brasil','Argentina','Chile')
class(k)


k=c('Brasil','Argentina','Chile',2022)
class(k)


#### criando sequ?ncias de n?meros

w<-1:10
w

r=seq(1,10,by=0.5)
r

seq(1,10,l=19)

rep(1:10,2)

rep(1:3,c(2,2,2))

rep(1:3,rep(2,3))

rep(1:10,rep(2,10))


### opera??es com vetores

pesos<-c(60,72,57,90,95,72)
alturas<-c(1.75,1.80,1.65,1.9,1.74,1.91)

imc<-pesos/alturas^2


imc



### c?lculo de m?dias

mp<-sum(pesos)/length(pesos)

soma<-sum(pesos)
n<-length(pesos)

soma/n


# ou

mp<-mean(pesos)
mp

ma<-sum(alturas)/length(alturas)
ma
# ou

ma<-mean(alturas)
ma

median(pesos)  ## mediana
sort(pesos)    ## ordena

table(pesos)   ## moda ? o valor mais frequente


############# medidas de varia??o

####### amplitude

Ap<-max(pesos)-min(pesos)
Ap

Aa<-max(alturas)-min(alturas)
Aa

range(pesos)
range(alturas)





###### c?lculo de vari?ncias

n<-length(pesos)

s2p <- sum( (  pesos-mp  )^2) /(n-1)
s2p
# ou

s2p <- var(pesos)
s2p

s2a <- sum( (  alturas-ma  )^2) /(n-1)

# ou

s2a <- var(alturas)
s2a


###### c?lculo de desvios-padr?o

sp<-sqrt(s2p)
sp
# ou 

sd(pesos)



sa<-sqrt(s2a)
sa
# ou 

sd(alturas)



###### coeficientes de varia??o


cvp<-(sp/mp)*100

cvp


cva<-(sa/ma)*100

cva



### outras estat?sticas

resumo<-summary(pesos)

resumo

y<-rnorm(1000,60,4) # mil valores de uma normal com média 60 e desvio 4
y
hist(y)
summary(y)

mean(y)
sd(y)

table(pesos)


###### alguns gr?ficos



plot(pesos)

plot(alturas)

plot(alturas,pesos,pch=19)
lines(alturas,-46.34+67.35*alturas,lwd=2)



# suponha que os pesos venham de dois grupos 

r<-rep(1:2,c(3,3))
r
plot(r,pesos)


#editando os eixos

plot(r,pesos,axes=F)
axis(1,c(1,2))
axis(2)
box()


# renomeando o eixo x
plot(r,pesos,axes=F,xlab="Grupos")
axis(1,c(1,2))
axis(2)
box()



# ou box-plot


boxplot(r,pesos)

## ou se r ? fator

rf<-factor(r)
plot(rf,pesos)


## outro exemplo de fator e boxplot

trat<-c("A","A","A","B","B","B")
boxplot(alturas~trat)



##### adicionando pontos e texto a um gr?fico

f<-function(x) -46.34+67.35*x

alturas<-c(1.75,1.80,1.65,1.9,1.74,1.91)
pesos<-c(60,72,57,90,95,72)


plot(f,xlim=c(min(alturas),max(alturas)))

plot(f,xlim=c(min(alturas),max(alturas)), lwd=2)
points(alturas,pesos,pch=17)    # acrescenta pontos a um plot




plot(f,xlim=c(min(alturas),max(alturas)), ylim=c(50,100), lwd=2)
## xlim e ylim delimita o intervalo para os eixos x e y respectivamente
points(alturas,pesos,pch=17)    # acrescenta pontos a um plot
text(1.7,90,expression(hat(y)*"=-46,34+67,35x")) # acrescenta texto a um plot



#aumentando o tamanho dos caractes do texto

plot(f,xlim=c(min(alturas),max(alturas)), ylim=c(50,100), lwd=2)
points(alturas,pesos,pch=17)   
text(1.7,90,expression(hat(y)*"=-46,34+67,35x"),cex=1.2) 
## cex=1.2 aumenta o tamanho dos caractes


### mais de uma linha de texto
plot(f,xlim=c(min(alturas),max(alturas)), ylim=c(50,100), lwd=2)
points(alturas,pesos,pch=17)   
text(c(1.7,1.7),c(90,85),c(expression(hat(y)*"=-46,34+67,35x"),
                           expression(R^2*"=19,18%")),cex=1.2) 


### renomeando os eixos
plot(f,xlim=c(min(alturas),max(alturas)), ylim=c(50,100), lwd=2, 
     xlab="Altura",ylab="Peso",cex.lab=1.2)

points(alturas,pesos,pch=17)   
text(c(1.7,1.7),c(90,85),c(expression(hat(y)*"=-46,34+67,35x"),expression(R^2*"=19,18%")),cex=1.2) 


### gr?fico de pontos unidimensional - destribui??o de frequ?ncia

stripchart(pesos,at=0.2,method="stack",pch=19,offset = 1)
?stripchart
y<-round(rnorm(100,60,2)) 
# gera e arredonda para o inteiro mais pr?ximo,
# 30 valores de uma normal com m?dia 60 e desvio padr?o 2

stripchart(y,at=0,method="stack",pch=19,offset = 1)

table(y)

x<-rnorm(30,60,2)
stripchart(x,at=0.2,method="stack",pch=19,offset = 1)

table(x)

z<-rnorm(30,65,4)

stripchart(x,at=0.2,method="stack",pch=19,xlim=c(min(x),max(x)))
text(mean(x), 0.3, "Dados x")

stripchart(z,at=1.2,method="stack",pch=19,add=TRUE)
text(mean(z), 1.3, "Dados z")



### Importando banco de dados  ..... próxima aula

# .....






