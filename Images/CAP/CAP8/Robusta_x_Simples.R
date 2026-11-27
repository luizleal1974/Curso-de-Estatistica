setwd("F:\\Consultoria_Estatistica\\PPGMT\\MEAM")

# Carregar pacotes
library(openxlsx)
library(plotly)

# Dados
dados = read.xlsx("MEAM.xlsx", sheet = 11)
x = dados[,1]

# Estatistica Robusta
Rotulos = c("Media", "Desvio padrao")
Simples = c(mean(x), sd(x))
r = MASS::hubers(x)
Robusta = c(r$mu, r$s)
names(Simples) = names(Robusta) = Rotulos
round(data.frame(Simples, Robusta), digits = 2)
