dados <- read.table("HarryPotter.txt", header = T, dec = ',')

?attach
#Ler as colunas individualmente
attach(dados)

?t.test
mean(dados$Legibilidade)

t.test(Legibilidade, mu = 4, alternative = "greater")
#Ha evidencias de que a media e maior dq 4, a 5% de significancia

t.test(Legibilidade)

?confint
?lm
confint(lm(Legibilidade~1))

#Verificacao de normalidade
shapiro.test(Legibilidade)
#p-value > 0.05

dados <- read.table("Usinagem.eixos.txt", header = T, dec = ",")
attach(dados)
show(dados)
var.test(Medida~Sist.Usinagem)
#Comparar variancia

#Verificar normalidade
shapiro.test(Medida[Sist.Usinagem=="1"])
shapiro.test(Medida[Sist.Usinagem=="2"])
#p-value's superiores a 0,05

#Grafico
boxplot(Medida~Sist.Usinagem)


t.test(Medida~Sist.Usinagem)
t.test(Medida~Sist.Usinagem, alternative = "g") #1_a maior que 2_a
t.test(Medida~Sist.Usinagem, alternative = "l") #1_a menor que 2_a

dados <- read.table("SAUDE.txt", h=T, dec=",")

attach(dados)

var.test(PULSO~SEXO)

shapiro.test(PULSO[SEXO=="F"])
shapiro.test(PULSO[SEXO=="M"])

t.test(PULSO~SEXO, var.equal = TRUE)

tapply(PULSO, SEXO, mean)
tapply(PULSO, SEXO, var)

boxplot(PULSO~SEXO)

dados <- read.table("TEMPERATURAS-JANEIRO.txt", h=T, dec="T")
attach(dados)
names(dados)

d = Max.Real-Max.Prev.1dia
t.test(Max.Real,Max.Prev.1dia, paired = T)    #Correlacao eh mais adequada

library(nortest)
xb <- mean(d)
sx <- sd(d)