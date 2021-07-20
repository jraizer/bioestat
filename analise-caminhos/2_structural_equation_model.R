# Header----
# 

# Path analysis----
## Para fazer nossa análise de caminhos (path analysis) ou 
## modelo de equações estruturadas vamos usar os seguintes pacotes:
if(!"lavaan" %in% installed.packages()) install.packages("lavaan")
library(lavaan) #para fazer o modelo de equações estruturadas (SEM em inglês)
citation("lavaan")
if(!"semPlot" %in% installed.packages()) install.packages("semPlot") 
library(semPlot) #para representação gráfica do SEM
citation("semPlot")

## Vamos fazer novamente a análise de caminhos para os dados
## lagostins
lagostins

## Primeiro explicitamos nossas funções (equações) expressas no
## diagrama de setas.
path_model <- 
  "lagostins ~ peixes + poluicao + fitoplancton; 
    peixes ~ poluicao;
      fitoplancton ~ poluicao" #estes são os modelos para SEM

#Ajustamos o modelo com a função cfa
fit <- cfa(path_model, data = lagostins)
summary(fit) 
fitted(fit)

#A função semPaths() é uma maneira de gerar rapidamente a 
#representação gráfica com os coeficientes padronizados.
semPaths(fit)

semPaths(fit, 'std', layout = 'circle', residuals = F, 
         sizeMan = 15, theme = "gray",
         edge.label.cex = 2, shapeMan = "circle", 
         nodeLabels = c("Lagostins", "Peixes", 
                       "Fitoplâncton", "Poluição"))



jpeg("path.jpg", width = 15, height = 15, units = "cm", res = 300)
semPaths(fit, 'std', layout = 'circle', residuals = F, 
         sizeMan = 15, theme = "gray",
         edge.label.cex = 2, shapeMan = "circle", 
         nodeLabels = c("Lagostins", "Peixes", 
                        "Fitoplâncton", "Poluição"))
dev.off()


