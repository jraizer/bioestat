# Header----
# Bioestatística | FCBA | UFGD
# Introdução às análises multivariadas: NMDS e PCoA
# Professor Josué Raizer

# A gente pode dizer que um dos principais objetivos das
# análises multivariadas é reduzir a dimensionalidade em 
# conjuntos de dados multivariados. 
# Um exemplo de dados multivariados é o conjunto de espécies 
# numa comunidade biológica. Nesse caso cada espécie é uma dimensão.
# Digamos que alguém contabilizou a quantidade de cada espécie de
# aves em 10 locais de uma região. Se ele quiser avaliar se essa
# comunidade de aves é afetada por algum fator ambiental (ou alguns),
# não pode simplesmente avaliar o que acontece com cada espécie, 
# precisa saber como o conjunto de espécie varia entre suas observações.
# Resumidamente, num modelo multivariado temos muitas variáveis 'Y'.
# Assumindo um modelo linear:

# Y1, Y2,..., Yn = a + b(X) 

# Vamos pensar nisso com um exemplo hipotético.

# Pacotes necessários----
if(!"vegan" %in% installed.packages()) install.packages("vegan")
library(vegan)
citation("vegan")

# Dados----
## Abundância de seis espécies de plantas em 10 locais
plantas <- read.table("planta.txt", header = T)
plantas

## Quantidade de chuva e altitude nesses 10 locais
ambientais <- read.table("ambiente.txt", header = T)
ambientais

# Ordenação direta----
## Uma das principais classes de análises multivariadas são as ordenações.
## Como o próprio nome diz, numa ordenação a gente coloca as coisas em ordem.

## Podemos fazer isso em função de um gradiente externo à comunidade.
## Por exemplo, vamos ordenar as amostras de plantas pela altitude:

plantas[order(ambientais$altitude), ]

plot(ambientais$altitude, plantas$sp_A,
     cex = 2, pch = 19)

for(i in 1:6){
  Sys.sleep(2)
  plot(ambientais$altitude, plantas[, i], type = "p", pch = 19, cex = 2.5,
       ylab = paste("sp_", LETTERS[i]),
       xlab = "Altitude")
}

## A gente pode usar uma função para gerar os gráficos juntos e em ordem
## criada pelo Cristian Dambros (https://pt.csdambros.com/)
source("poncho.R")

poncho(plantas, gradient = ambientais$altitude, 
       xlab = "Ordenação das observações", 
       ylab = "Abundância", 
       gradlab = "Altitude", 
       cex.species = 1.25, 
       cex.lab = 1.25, 
       cex.gradient = 1)

## O problema com as ordenações diretas é que enxergamos um 
## padrão na estrutura do conjunto de dados associado à variável externa,
## mas não sabemos se esse padrão é relevante. 
## Para extrair um padrão na estrutura dos dados independentemente 
## das variáveis externas à comunidade, a gente usa ordenações indiretas.


# Ordenações indiretas----
## Existem várias técnicas para ordenar as amostras usando somente
## as informações do próprio conjunto de dados da comunidade.
## Todas elas partem das diferenças em espécie entre cada par de observações.

## Para diferenciar as observações (neste caso, locais) em função dos
## atributos dessas observações (no caso, abundâncias de cada espécie)
## temos vários índices que medem essas diferenças (chamadas de distâncias), 
## cada um com suas propriedades.

## Distâncias----
# Para diferenciar duas observações, podemos simplesmente
# somar as diferença entre cada atributo. Vejamos.


sp1 <- c(loc_A = 1, loc_B = 5)
# Qual a diferença em abundância da sp1 entre os locais A e B?
abs(sp1["loc_A"] - sp1["loc_B"])

sp2 <- c(loc_A = 5, loc_B = 4)
# Qual a diferença em abundância da sp2 entre os locais A e B?
abs(sp2["loc_A"] - sp2["loc_B"])


# Agora podemos somar essas diferenças...
abs(sp1["loc_A"] - sp1["loc_B"]) + abs(sp2["loc_A"] - sp2["loc_B"])


# Graficamente
plot(sp1, sp2, xlim = c(0, 6), ylim = c(3, 6))
text(sp1 + .1, sp2 + .1, c("A", "B"))
segments(sp1[2], sp2[2], sp1[1], sp2[2])
segments(sp1[1], sp2[1], sp1[1], sp2[2])

# Essa é a distância Manhattan ou City Block (quarteirão).
# Reparou que a menor distância entre A e B é a hipotenusa 
# do triângulo formado?

segments(sp1[1], sp2[1], sp1[2], sp2[2], lty = 2)

# Lembra o Teorema de Pitagoras: 
# o quadrado da hipotenusa é igual a soma dos quadrados dos catetos

diff(sp1) ^ 2 + diff(sp2) ^ 2 #soma dos quadrados dos catetos = hipotenusa ao quadrado

sqrt(diff(sp1) ^ 2 + diff(sp2) ^ 2) #raiz quadrada da hipotenusa
# Essa é a distância Euclidiana (a menor distância entre dois pontos)

# Vamos calcular as distâncias Manhattan e Euclidiana 
# para os dados das plantas com a função vegdist do pacote vegan:

vegdist(plantas, "man")
round(vegdist(plantas, "euc"), 2)

# Veja as opções de índices de distância que a função
# vegdist tem:
?vegdist

## Análise não-métrica de escalas multidimensionais (NMDS)----
# Esse é um método de ordenação indireta por iteração (permutação)
# que arranja os objetos (observações) num espaço de poucas dimensões 
# a partir das distâncias calculadas.

# Imagine três objetos (observações), por exemplo locais: A, B e C
# Digamos que a distância quanto aos atributos (p.ex. espécies) 
# entre eles seja:

# A - B = 5
# A - C = 10
# B - C = 5

# Como podemos arrajar esses objetos em uma dimensão (uma linha)?

# A----B----C

# E se a distância entre A e C for 9?


# O que o NMDS faz é exatamente buscar o melhor arranjo nesse número
# limitado de dimensões.

# Para fazer o NMDS usamos a função metaMDS do vegan. 
# Vejamos para as plantas.
metaMDS(plantas, distance = "bray", k = 1, autotransform = F)

nmds <- metaMDS(plantas, k = 1, autotransform = F)
nmds

scores(nmds)
plantas[order(scores(nmds)), ]
poncho(plantas, gradient = scores(nmds),
       xlab = "Ordenação das observações", 
       ylab = "Abundância", 
       gradlab = "NMDS", 
       cex.species = 1.25, 
       cex.lab = 1.25, 
       cex.gradient = 1)

# Vamos entender o que aconteceu. Por padrão (default) a função metaMDS
# calculou distâncias Bray-Curtis entre cada par de locais amostrados
# quanto às plantas. Esse índice varia entre 0 (totalmente iguais) e
# 1 (totalmente diferentes)
round(vegdist(plantas, "bray"), 2)

# A partir dessas distâncias o NMDS arranjou os locais buscando a menor diferença
# entre essas distâncias e as distâncias finais obtidas. Para saber o quanto
# esse resultado se aproximou das distâncias originais, usamos o 
# diagrama de Shepard:
stressplot(nmds)

# A distância Bray-Curtis funciona bem para recuperar distâncias
# entre amostras das abundâncias de espécies, principalmente 
# usando abundâncias relativas.
# Para transformar nossos dados podemos usar a função decostand do vegan:
round(decostand(plantas, "total"), 2) #abundâncias relativas 
abund_rel <- decostand(plantas, "total")

nmds <- metaMDS(abund_rel, k = 1, autotransform = F)
stressplot(nmds)
poncho(plantas, gradient = scores(nmds),
       xlab = "Ordenação das observações", 
       ylab = "Abundância", 
       gradlab = "NMDS", 
       cex.species = 1.25, 
       cex.lab = 1.25, 
       cex.gradient = 1)

# Podemos optar por arranjar os objetos em mais dimensões 
# (mais do que 3 geralmente não faz sentido). 
# Vejamos com 2 dimensões:
nmds_2d <- metaMDS(abund_rel, k = 2, autotransform = F)
stressplot(nmds_2d)
plot(nmds_2d, type = "t")


# Também podemos escolher qualquer índice de distância;
nmds_euc <- metaMDS(abund_rel, distance = "euc", k = 1, autotransform = F)
poncho(plantas, gradient = scores(nmds_euc),
       xlab = "Ordenação das observações", 
       ylab = "Abundância", 
       gradlab = "NMDS_euc", 
       cex.species = 1.25, 
       cex.lab = 1.25, 
       cex.gradient = 1)

# Vamos comparar a diferença com o resultado obtido a partir de Bray-Curtis
poncho(plantas, gradient = scores(nmds),
       xlab = "Ordenação das observações", 
       ylab = "Abundância", 
       gradlab = "NMDS", 
       cex.species = 1.25, 
       cex.lab = 1.25, 
       cex.gradient = 1)


## Análise de coordenadas principais (PCoA)----
# Esse é um método de ordenação indireta por álgebra de matrizes
# que extrai eixos do espaço multidimensional. Ela é derivada
# de um método chamado de Análise de Componentes Principais (PCA).
# Gower em 1966 "enganou" a PCA, que aproxima os objetos usando
# distâncias Euclidianas, para poder usar qualquer índice de distância.
# É um processo de regressão do maior eixo entre todas as dimensões.
# Quando executamos a PCoA ela extrai todos os eixos possíveis
# que tem a vantagem de serem independentes (ortogonais) uns dos outros.
# Isso significa que podemos analisar cada eixo separadamente! Mas 
# tem a desvantagem de extrair somente as relações lineares entre atributos.

# Vejamos com as plantas usando a função cmdscale. 
# Neste caso precisamos informar as distâncias:
cmdscale(vegdist(abund_rel, "bray")) #por padrão mostra os dois primeiros eixos

pcoa <- cmdscale(vegdist(abund_rel, "bray")) 
poncho(plantas, gradient = pcoa[, 1],
       xlab = "Ordenação das observações", 
       ylab = "Abundância", 
       gradlab = "PCoA", 
       cex.species = 1.25, 
       cex.lab = 1.25, 
       cex.gradient = 1)

pcoa_euc <- cmdscale(vegdist(abund_rel, "euc")) 
poncho(plantas, gradient = pcoa_euc[, 1],
       xlab = "Ordenação das observações", 
       ylab = "Abundância", 
       gradlab = "PCoA_euc", 
       cex.species = 1.25, 
       cex.lab = 1.25, 
       cex.gradient = 1)

pcoa <- cmdscale(vegdist(abund_rel, "bray"), eig = T) 
pcoa
plot(pcoa$points, type = "n", xlim = c(-.5, .6))
text(pcoa$points[, 1], pcoa$points[, 2], rownames(plantas))

wascores(pcoa$points, abund_rel) #correlações das espécies com os eixos (loadings)
loads <- wascores(pcoa$points, abund_rel)
arrows(0, 0, loads[, 1], loads[, 2], length = .1, col = "blue")
text(loads[, 1] * 1.1, loads[, 2] * 1.1, rownames(loads), col = "blue")

# Autovalores (eigen values) indicam a variância recuperada por cada eixo
pcoa$eig #equivale ao comprimento de cada eixo (proporcional à variância)
sum(pcoa$eig[which(pcoa$eig > 0)]) #variância total
var_total <- sum(pcoa$eig[which(pcoa$eig > 0)])
pcoa$eig[which(pcoa$eig > 0)] / var_total
round((pcoa$eig[which(pcoa$eig > 0)] / var_total) * 100, 2)

# Quantos eixos usar? 
# Para responder a essa pergunta precisamos pensar no balanço 
# entre o aumento de explicação e a dificuldade de interpretação 
# com o aumento do número de eixos.
poncho(plantas, gradient = pcoa$points[, 1],
       xlab = "Ordenação das observações", 
       ylab = "Abundância", 
       gradlab = "PCoA", 
       cex.species = 1.25, 
       cex.lab = 1.25, 
       cex.gradient = 1)

## Testes de hipótese----

# Se julgamos que o principal padrão na estrutura desses dados
# está bem representado em uma dimensão (primeiro eixo), os testes
# são os mesmos que já vimos.

plot(ambientais$altitude, pcoa$points[, 1])
abline(lm(pcoa$points[, 1] ~ ambientais$altitude))

summary(lm(pcoa$points[, 1] ~ ambientais$altitude))

# Entretanto, se precisamos usar mais eixos, precisamos usar
# análise de variância multivariada (MANOVA):
plot(pcoa$points, cex = ambientais$altitude/50) #pontos maiores, maiores altitudes

summary(manova(pcoa$points[, 1:2] ~ ambientais$altitude + ambientais$chuva))

# Agora assumimos que a altitude afetou significativamente o
# principal padrão na estrutura da comunidades de plantas.

## Para pensar: O que diferencia ordenação direta e indireta?
