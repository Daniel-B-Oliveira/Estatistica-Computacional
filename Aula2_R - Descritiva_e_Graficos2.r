rm(list=ls())

## Gráficos - Estatística Descritiva

#dados1 <- read_excel("UFU/2023.2/Estatística Computacional/telecom_users.xlsx")
#View(dados1)

#dados3 <- read.csv("~/UFU/2023.2/Estatística Computacional/telecom_users.csv",sep=";")
#View(dados3)

dados <- read.table("telecom_users.txt",sep=";",h=T)
View(dados)

## Verificando o bando de dados (dados faltantes/ausentes)

?summary.data.frame

summary(dados) # Tipo/classe, Medidas básicas, NAs
head(dados)

class(dados$TotalGasto)
mode(dados$TotalGasto)

?apply

NAs <- apply(dados,2, function(x) is.na(x)) # Classifica em NA ou não (FALSE/TRUE)
colSums(NAs) # Conta a quantidade de NAs por coluna

dim(dados)
names(dados)

# Excluindo colunas indesejadas
?subset
names(dados)
head(dados)

#dados2=subset(dados2,select=c(-X,-IDCliente,-Codigo))
dados2 <- subset(dados, select=c(-IDCliente,-Codigo))


names(dados2)
summary(dados2)
#View(dados2)


# Verificado NAs em dados2
NAs=apply(dados2,2, function(x) is.na(x))
colSums(NAs)


dados3=na.omit(dados2) # Elimina linhas com NAs

dim(dados)
dim(dados2)
dim(dados3)  # Se todos os elementos de uma coluna for NA, 
# na.omit retornará um data frame com 0 linhas. Porquê ????


tab <- table(dados3$Churn)
barplot(tab)

dados4 <- subset(dados3,dados3$Churn=="Nao" | dados3$Churn=="Sim")# seleciona churn não e churn sim

# Gráfico de setores

tab=table(dados4$Churn)
tab

p = round(100*tab/sum(tab),digits=1)
p

names(p)

pie(p,col=c("blue","red"),main="Churn")
legend("topright",c("Não","Sim"),fill = c("blue","red"))
box()
?legend

# Forma de pagamento
#Jamovi (Pesquisar)
names(dados4)
tab=table(dados4$FormaPagamento)
tab

p = round(100*tab/sum(tab),digits=1)
p

nomes=c("B. Eletrônico","B. Impresso","C. Crédito","D. Automático")
pie(p,col=c("blue","red","green","gray"),main="Forma de Pagamento",labels=nomes)
legend("topleft",nomes,fill = c("blue","red","green","gray"),cex=0.7)
box()



## Medidas resumo de uma variável indexada por outra(s)

?list
tapply(dados4$TotalGasto,dados4$Churn,mean)
tapply(dados4$TotalGasto,dados4$Churn,var)

tapply(dados4$MesesComoCliente,list(dados4$Churn,dados4$FormaPagamento),mean)
tapply(dados4$MesesComoCliente,list(dados4$Churn,dados4$FormaPagamento),var)

tapply(dados4$MesesComoCliente,list(dados4$Churn,dados4$FormaPagamento),min)
tapply(dados4$MesesComoCliente,list(dados4$Churn,dados4$FormaPagamento),max)


tapply(dados4$MesesComoCliente,list(dados4$Churn,dados4$FormaPagamento,dados4$Genero),mean)
tapply(dados4$MesesComoCliente,list(dados4$Churn,dados4$FormaPagamento,dados4$Genero),var)

barplot(table(dados4$Dependentes))

# Elimnado linha com Dependentes desconhecido
dados5=subset(dados4,dados4$Dependentes=="Nao" | dados4$Dependentes=="Sim")# Apenas Não e Sim 

dim(dados)
dim(dados2)
dim(dados3)
dim(dados4)
dim(dados5)

# Com if para mudar o tipo de gráfico conforme tipo de variável
# Fazer uma única (sem laço)

head(dados5)

i=1
  if(class(dados5[,i])=="numeric") {hist(dados5[,i],xlab=names(dados5)[i],
                                         ylab="Frequência",main="Histograma")}
  if(class(dados5[,i])=="integer") {hist(dados5[,i],xlab=names(dados5)[i],
                                       ylab="Frequência",main="Histograma")}
  if(class(dados5[,i])=="character") {barplot(table(dados5[,i]),xlab=names(dados5)[i],
                                              ylab="Frequência",main="")}
# Editando - cores, tamanho do eixo y e box 
# Editar uma única (Fazer)

# Editando todas
for(i in 1:ncol(dados5)){
  if(class(dados5[,i])=="numeric") {h=hist(dados5[,i],plot=F)
  plot(h,xlab=names(dados5)[i],
       ylab="Frequência",main="Histograma", col=1:length(h$mids),
       ylim=c(0,max(h$counts*1.3)))
  box()  } else
  if (class(dados5[,i])=="integer") {
    h=hist(dados5[,i],plot=F)
    plot(h,xlab=names(dados5)[i],
         ylab="Frequência",main="Histograma", col=1:length(h$mids),
         ylim=c(0,max(h$counts*1.3)))
    box() } else {
    barplot(table(dados5[,i]),xlab=names(dados5)[i],
            ylab="Frequência",main="",col=1:length(table(dados5[,i])),
            ylim=c(1,max(table(dados5[,i]))*1.3))
    box()}
  }




# Exportanto as figuras

# Exportar uma única figura (Fazer)


# Exportando
for(i in 1:ncol(dados5)){
  png(filename = paste0("C:/Users/User/Documents/UFU/2024.1/Estatística Computacional/Figuras/",names(dados5)[i],".png"))
  if(class(dados5[,i])=="numeric") {h=hist(dados5[,i],plot=F)
  plot(h,xlab=names(dados5)[i],
       ylab="Frequência",main="Histograma", col=1:length(h$mids),
       ylim=c(0,max(h$counts*1.3)))
  box()  } else
    if (class(dados5[,i])=="integer") {
      h=hist(dados5[,i],plot=F)
      plot(h,xlab=names(dados5)[i],
           ylab="Frequência",main="Histograma", col=1:length(h$mids),
           ylim=c(0,max(h$counts*1.3)))
      box() } else {
        barplot(table(dados6[,i]),xlab=names(dados5)[i],
                ylab="Frequência",main="",col=1:length(table(dados5[,i])),
                ylim=c(1,max(table(dados5[,i]))*1.3))
        box()}
  dev.off()
  }



#################################
# Gráficos por Churns (sim/nao)



# Editando uma única variável por categoria de churn

i=19 # Total gasto - HISOGRAMA
h1=hist(dados5[dados5$Churn=="Nao",i],plot=F)
h2=hist(dados5[dados5$Churn=="Sim",i],plot=F)
plot(h1,xlab=names(dados5)[i], ylim=c(1,max(max(h1$counts),max(h2$counts)))*1.3,
     ylab="Frequência",main="Histograma", 
     density=30,col="red", angle=45)
plot(h2,add=T,col="blue",density=30,angle=135)
legend("top",c("Não","Sim"),fill=c("red","blue"),density=30,
       title="Churn",angle=c(45,135),title.font=2)
box()



i=17 # Forma de pagamento
tab=table(dados5$Churn,dados5[,i])
barplot(tab,xlab=names(dados5)[i], 
        ylab="Frequência",main="",col=c("blue","red"),
        ylim=c(1,max(tab)*2.3),legend=T, cex.names=0.7,
        args.legend=list(title="Churn",title.font=2))
box()


# Para todas as colunas

for(i in 1:ncol(dados5)){
  if(class(dados5[,i])=="numeric") {h1=hist(dados5[dados5$Churn=="Nao",i],plot=F)
  h2=hist(dados5[dados5$Churn=="Sim",i],plot=F)
  plot(h1,xlab=names(dados5)[i], ylim=c(1,max(max(h1$counts),max(h2$counts)))*1.3,
       ylab="Frequência",main="Histograma", 
       density=30,col="red", angle=45)
  plot(h2,add=T,col="blue",density=30,angle=135)
  legend("top",c("Não","Sim"),fill=c("red","blue"),density=30,
         title="Churn",angle=c(45,135),title.font=2)
  box()
  }else{
    tab=table(dados5$Churn,dados5[,i])
    barplot(tab,xlab=names(dados5)[i], 
            ylab="Frequência",main="",col=c("blue","red"),
            ylim=c(1,max(tab)*2.3),legend=T, cex.names=0.7,
            args.legend=list(title="Churn",title.font=2))
    box()
  }
}


# Exportanto

for(i in 1:ncol(dados5)){
png(filename = paste0("C:/Users/User/Documents/UFU/2024.1/Estatística Computacional/Figuras2/",names(dados5)[i],".png"))
  if(class(dados5[,i])=="numeric") {h1=hist(dados5[dados5$Churn=="Nao",i],plot=F)
  h2=hist(dados5[dados5$Churn=="Sim",i],plot=F)
  plot(h1,xlab=names(dados5)[i], ylim=c(1,max(max(h1$counts),max(h2$counts)))*1.3,
       ylab="Frequência",main="Histograma", 
       density=30,col="red", angle=45)
  plot(h2,add=T,col="blue",density=30,angle=135)
  legend("top",c("Não","Sim"),fill=c("red","blue"),density=30,
         title="Churn",angle=c(45,135),title.font=2)
  box()
  }else{
    tab=table(dados5$Churn,dados5[,i])
    barplot(tab,xlab=names(dados5)[i], 
            ylab="Frequência",main="",col=c("blue","red"),
            ylim=c(1,max(tab)*2.3),legend=T, cex.names=0.7,
            args.legend=list(title="Churn",title.font=2))
    box()
  }
  dev.off()
}


## ???????????? Oberve o gráfico de meses como cliente

