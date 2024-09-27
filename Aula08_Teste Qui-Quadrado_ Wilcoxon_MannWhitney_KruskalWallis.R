# Teste de Qui-Quadradao para aderência e
# independência

# Teste de Wilcoxon (amostra única e duas amostras)

# Teste de Kruskal-Wallis

library(titanic)

d1=as.data.frame(titanic_train)
d2=as.data.frame(titanic_test) # não tem a col. sobreviveu

View(d1)
colnames(d1)

?chisq.test

## Teste Qui-Quadrado para aderência

# H0: A proporção de passageiros
# nas diferentes classes é a mesma.

teste1=chisq.test(table(d1$Pclass))

teste1

teste1$observed
teste1$expected

# H0: A proporção de passageiros
# na classe 1 é 0,3, na classe 2 é 0,25
# e na classe 3 é 0,45.

teste2=chisq.test(table(d1$Pclass),p=c(0.3,0.25,0.45))

teste2

teste2$observed
teste2$expected

#É um teste para categorias

## Teste Qui-Quadrado para associação/independência/homogeneidade

teste3=chisq.test(table(d1$Survived,d1$Pclass))

teste3

teste3$observed
teste3$expected

barplot(teste3$observed,beside=T,
        legend.text = c("Não","Sim"),
        args.legend=list(x="topleft"),ylim=c(0,450))
box()

## Teste de wilcoxon (uma amostra)
#Quando rejeita a hipótese de normalidade

wilcox.test(d1$Age,mu=50,alternative="l")
hist(d1$Age)

# poderia usar o teste t????

shapiro.test(d1$Age)
# Não poderia

## Teste de wilcoxon (duas amostras porém pareados)

peso_antes = c(75, 82, 75, 67)
peso_depois = c(74, 77, 76, 65)

#Dados pareados, o teste trabalha com a diferença
wilcox.test(peso_antes, peso_depois,alternative='greater',paired=T)

## Teste de wilcoxon ou Mann-Whitney 
## (duas amostras não pereadas - independentes)

wilcox.test(d1$Age[d1$Sex=="male"],d1$Age[d1$Sex=="female"])

boxplot(d1$Age~d1$Sex)

# poderia usar o teste t????
shapiro.test(d1$Age[d1$Sex=="male"])
# Não poderia
shapiro.test(d1$Age[d1$Sex=="female"])

# Mais um teste para normalidade inclusive
#Verifica além de normalidade
y=na.omit(d1$Age[d1$Sex=="female"])
ks.test(y,"pnorm",mean(y),sd(y))

x=na.omit(d1$Age[d1$Sex=="male"])
ks.test(x,"pnorm",mean(x),sd(x))

# Teste de Kruskal-Wallis para comparar
# as classes quanto à idade

shapiro.test(d1$Age[d1$Pclass==1])
shapiro.test(d1$Age[d1$Pclass==2])
shapiro.test(d1$Age[d1$Pclass==3])

kruskal.test(d1$Age ~ d1$Pclass)
boxplot(d1$Age ~ d1$Pclass)
