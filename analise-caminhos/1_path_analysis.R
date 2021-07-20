# Header----
# Apostila Exercícios de Estatística - William Magnusson | INPA
# Aula Prática 13 - com modificações de Josué Raizer | UFGD
# ANÁLISE DE CAMINHOS (PATH ANALYSIS)----

## Para fazer este exercício vamos usar os dados de lagostins que estão disponíveis
## em nossa sala virtual e você deve ter salvo na sua pasta da disciplina. 

### Importe esses dados:
lagostins <- read.table("lagostins.txt", header = T)
head(lagostins)

# Para calcular o 'b' padronizado, vamos transformar os dados usando a função 
# 'decostand( )' do vegan: 
# Anote os valores do coeficiente 'b' em cada análise.

if(!"vegan" %in% installed.packages()) install.packages("vegan") #para instalar o vegan se necessário
library(vegan) #habilitar o vegan
d.pad <- decostand(lagostins, "standardize") #padronizar os dados de lagostins
d.pad

# Primeiro uma regressão múltipla para os efeitos diretos sobre os lagostins:
lm(d.pad$lagostins ~ d.pad$poluicao + d.pad$peixes + d.pad$fitoplancton)
# Depois os efeitos diretos da poluição sobre os peixes 
lm(d.pad$peixes~d.pad$poluicao)
# e sobre o fitoplâncton
lm(d.pad$fitoplancton~d.pad$poluicao)

## Podemos fazer o diagrama de setas...
par(mfrow = c(1,1), mar = rep(.1, 4))
plot(-2:12, -2:12, type = "n", xaxt="n", yaxt="n", xlab = "", ylab = "", bty = "n")
text(1, 5, adj = 1, "Poluição")
text(5, 1, adj = .5, "Fitoplâncton")
text(5, 10, adj = .5, "Peixes")
text(10, 5, adj = .5, "Lagostins")
arrows(1, 5.5, 4, 9)
arrows(1, 4.5, 4, 2)
arrows(6, 2, 9, 4)
arrows(6, 9, 9, 6)
arrows(1.5, 5, 9, 5)

#Desafio----
## Escreva os comandos para colocar os valores de 'b' junto as setas.
## DICA: use a função text() como nos comandos acima. Essa função serve
## para inserir texto em gráficos.
## Exemplo:
text(1, 7.5, "-0,657")


#ALERTA
#Esse exemplo de análise de caminhos serve para entendermos o processo e os conceitos de efeitos 
#diretos, indiretos e gerais. Entretanto, quando quisermos fazer análise de caminhos ("Path 
#Analysis"), vamos precisar considerar vários pontos, tal como a forma das relações entre as 
#variáveis. 
#Nesse sentido, sugiro começar estudando uma boa introducão ao tema em
#Introduction to Path Analysis in R
#https://www.kdnuggets.com/2018/09/introducing-path-analysis-using-r.html

#O script "2_structural_equation_model.R" mostro como usar
#o pacote 'lavaan' para geral nossa modelagem de equações estruturais e 
#o pacote 'semPlot'para gerar a representação gráfica.

#Lembre-se, uma busca rápida no Google sobre a análise que você quer fazer em R geralmente traz
#as respostas que você está precisando, principalmente se a busca for em inglês. Tente entrar com
#"Path Analysis in R" ou "Structural Equation Model" no Google.









