#QUestão 7
#Teste as hipóteses da 1ª, 2ª e 3ª Questão desta lista utilizando o teste de
#Wilcoxon ou de Mann-Whitney (testes não paramétricos - não necessitam da
#suposição de normalidade). Realize estes testes no R. Apresente uma conclusão.

#1)Utilize o banco de dados Usinagem.eixos e teste a hipótese de que a média geral
# (independente do tipo de usinagem) da variável medida é diferente de 20
# ( H0:µ=20 vs  Ha:µ≠20 ) a 5% de significância.

usinagem <- read.table("Usinagem.eixos.txt", sep="\t", header = T, dec = ",")

usinagem$Sist.Usinagem <- as.factor(usinagem$Sist.Usinagem)
str(usinagem)

sistema1 <- usinagem[usinagem$Sist.Usinagem == "1",]$Medida
sistema2 <- usinagem[usinagem$Sist.Usinagem == "2",]$Medida

?wilcox.test

wilcox.test(sistema1, mu=20)
wilcox.test(sistema2, mu=20)
wilcox.test(usinagem$Medida, mu=20)

#2)Utilize o banco de dados telecom users e teste a hipótese de que mulheres não
#diferem, em média, quanto à variável total gasto. Utilize 5% de significância. Apresente uma conclusão.

telecom <- read.table("telecom_users.txt", sep=";", header = T)

str(telecom)
telecom$Genero <- as.factor(telecom$Genero)

feminino <- telecom[telecom$Genero == "Feminino",]$TotalGasto
masculino <- telecom[telecom$Genero == "Masculino",]$TotalGasto

wilcox.test(feminino, masculino)


#3)Um estudo com o objetivo de avaliar a efetividade de uma dieta combinada com um
#programa de exercícios físicos na redução do nível de colesterol foi realizado
#e os dados são como a seguir:
#Utilize 10% de significância.A dieta foi significativamente eficiente para a redução dos níveis de colesterol? Justifique.

dieta <- read.table("dieta_q3_l1_p2.txt", TRUE,";")

wilcox.test(dieta$Antes, dieta$Depois, alternative="greater", paired = T, exact = F)

#Questão 8
# A hipótese de que o total gasto é o mesmo nas diferentes formas de pagamento
# deve ser rejeitada? Realize o teste apropriado para responder a essa questão,
# supondo que os pressupostos necessários para o uso de testes paramétricos não
# sejam atendidos. Use 5% de significância. Realize este teste tanto utilizando
# o R quanto o Python. Apresente uma conclusão.


str(telecom)
telecom$FormaPagamento <- as.factor(telecom$FormaPagamento)

kruskal.test(telecom$TotalGasto ~ telecom$FormaPagamento)





