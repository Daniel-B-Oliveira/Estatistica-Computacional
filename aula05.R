

## DISCRETAS

# Uniforme

rm(list=ls())

x=1:5
d=dunif(x,1,5) # densidades/probabilidades

plot(d, type="h", lwd=4,cex = 1.5,xlab='x',ylab='P(x)', main='Uniforme')

Fx=punif(x,1,5) # probabilidades acumuladas
Fx

y=runif(100000,1,5)
hist(y)



# Bernoulli

?rbinom

x=0:1
d=dbinom(x,size=1,prob=0.3)
d

plot(d,type='h',lwd=4,axes=FALSE,xlab='x',ylab='p(x)',main='Binom(1;0.3) ou Bernoulli(0.3)')
axis(1,1:2,c("Nao", "Sim"))
box()

y=rbinom(3,size=1,prob=0.3)
y


# Aplicação da Bernoulli:
# Verificação da probabilidade frequentista

y=vector()
p=vector()

for(i in 1:5000){
  y[i]<-rbinom(1,size=1,prob=0.3)
  p[i]= sum(y[1:i])/i
}

plot(p,type='l') 
abline(h=0.3,col='red',lwd=2)

# OU

y=vector()
p=vector()
y[1]<-rbinom(1,size=1,prob=0.3)
p[1]=2
i=1
dif=2

while(dif > 10^(-4)){
  i<-i+1
  y[i]<-rbinom(1,size=1,prob=0.3)
  p[i]= sum(y[1:i])/i
  dif=abs(p[i]-p[i-1])
}

?abs

par(mfrow=c(1,1))
plot(p,type='l') 
abline(h=0.3,col='red',lwd=2)

length(p)

?par

# Aplicação:
# Verificação empírica do Teorema do Limite Central 
# Distribuição da proporção amostral

v=vector()

n=50  # observe a distribuição/grafico com n=10, 30 e 50 

for(i in 1:5000){
  v[i]=mean(rbinom(n,size=1,prob=0.3))
}

hist(v) 

# Binomial

x=0:10

par(mfrow=c(2,2))
d=dbinom(x,size=10,prob=0.1)
plot(d,type='h',lwd=3,xlab='x',ylab='p(x)')
d=dbinom(x,size=10,prob=0.9)
plot(d,type='h',lwd=3)
d=dbinom(x,size=10,prob=0.5)
plot(d,type='h',lwd=3)
d=dbinom(x,size=10,prob=0.6)
plot(d,type='h',lwd=3)


?pbinom
d=dbinom(x,size=10,prob=0.1)
d
Fx=pbinom(x,size=10,prob=0.1)
?pbinom
Fx

# Poisson

?rpois

x=0:100

par(mfrow=c(2,2))
d=dpois(x,lambda=0.5)
plot(d,type='h',lwd=3,xlab='x',ylab='p(x)')
d=dpois(x,lambda=3)
plot(d,type='h',lwd=3,xlab='x',ylab='p(x)')
d=dpois(x,lambda=20)
plot(d,type='h',lwd=3,xlab='x',ylab='p(x)')
d=dpois(x,lambda=50)
plot(d,type='h',lwd=3,xlab='x',ylab='p(x)')


## Algumas Distribuições contínuas

# Exponencial

?rexp

par(mfrow=c(2,2))
curve(dexp(x,0.1),type='l',lwd=2,col=1,xlim=c(0,10))
curve(dexp(x,0.7),type='l',lwd=2,col=1,xlim=c(0,10))
curve(dexp(x,7),type='l',lwd=2,col=2)
curve(dexp(x,10),type='l',lwd=2,col=2)

ppois(0, lambda = 2)
ppois(1, lambda = 2)
dpois(0, lambda = 2) + dpois(1, lambda = 2)

?ppois

# gerando amostras

y=rexp(100000,0.1)
mean(y)


# cálculo de probabilidade

pexp(4,0.7)-pexp(2,0.7)

# Ilustração
par(mfrow=c(1,1))
curve(dexp(x,0.7),type='l',lwd=2,col=1,xlim=c(0,10))
abline(h=0,lwd=2)
polygon(x = c(2, seq(2, 4, length.out = 100), 4),
        y = c(0, dexp(seq(2, 4, length.out = 100),0.7), 0),
        col = "red")


# normal


?rnorm

par(mfrow=c(1,1))
curve(dnorm(x,0,1),type='l',lwd=2,col=1,xlim=c(-5,5),ylim=c(0,0.9))
curve(dnorm(x,0,0.5),type='l',lwd=2,col='red',add=T)
curve(dnorm(x,0,2),type='l',lwd=2,col='blue',add=T)
curve(dnorm(x,0,3),type='l',lwd=2,col='green',add=T)


par(mfrow=c(1,1))
curve(dnorm(x,0,1),type='l',lwd=2,col=1,xlim=c(-5,7),ylim=c(0,0.9))
curve(dnorm(x,-2,0.5),type='l',lwd=2,col='red',add=T)
curve(dnorm(x,2,2),type='l',lwd=2,col='blue',add=T)
curve(dnorm(x,0,3),type='l',lwd=2,col='green',add=T)


# cálculo de probabilidade

# normal padrão

pnorm(1.96,mean=0,sd=1)
pnorm(1.96)

pnorm(-1.96) 
1-pnorm(1.96) 
1-pnorm(-1.96) 

pnorm(1.96)-pnorm(-1.96) 

# Ilustração
par(mfrow=c(1,1))
curve(dnorm(x),type='l',lwd=2,col=1,xlim=c(-4,4))
abline(h=0,lwd=2)
polygon(x = c(-1.96, seq(-1.96, 1.96, length.out = 100), 1.96),
        y = c(0, dnorm(seq(-1.96, 1.96, length.out = 100)), 0),
        col = "red")



# cálculo de probabilidade

# com média e variância qualquer


pnorm(8,mean=5,sd=2)
pnorm(8,5,2)

pnorm(6,5,2) 


pnorm(8,5,2)-pnorm(6,5,2) 

# Ilustração
par(mfrow=c(1,1))
curve(dnorm(x,5,2),type='l',lwd=2,col=1,xlim=c(-2,12))
abline(h=0,lwd=2)
polygon(x = c(6, seq(6, 8, length.out = 100), 8),
        y = c(0, dnorm(seq(6, 8, length.out = 100),5,2), 0),
        col = "red")



## Outra verificação empírica do Teorema do Limite Central

## A distribuição da média amostral é
## normal ou aproximadamente normal
## com média igual a média da população
## e variância igual a variância da população
## dividida por n (tamanho da amostra)


# amostras de uma poisson
medias=vector()
par(mfrow=c(2,3))
n=c(3,5,7,10,15,30)

for(j in 1:length(n))
{
  for(i in 1:5000){
    medias[i] = mean(rpois(n[j],1)) # a medida que a amostra aumenta 'melhora'
  }
  m=round(mean(medias),digits=4)
  var=round(var(medias),digits=4)
  hist(medias,main=paste0('n=',n[j],', m=',m, ', v=',var),prob=TRUE)
  lines(density(medias),col='red',lwd=2)
}

1 # média
1/30 # variância

