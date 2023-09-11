# Importando bibliotecas
library(data.table)
# Criando dataframe base de dados
base <- fread(input = paste0("mobile.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",") 

# Pegando uma base aleatória:
amostra <- base[sample(nrow(base), 400), ]

# Média
media_batery_power <- mean(amostra$battery_power)
print(media_batery_power)
media_ram <- mean(amostra$ram)
print(media_ram)
# Mediana
mediana_batery_porwer <- median(amostra$battery_power)
print(media_batery_power)
mediana_ram <- median(amostra$ram)
print(mediana_ram)
# Percentis Batery Power
percentil_5_batery_power <- quantile(amostra$battery_power, 0.05)
percentil_25_batery_power <- quantile(amostra$battery_power, 0.25)
percentil_75_batery_power <- quantile(amostra$battery_power, 0.75)
percentil_95_batery_power <- quantile(amostra$battery_power, 0.95)
print(percentil_5_batery_power, percentil_25_batery_power, percentil_75_batery_power, percentil_95_batery_power)
# Percentil Ram
percentil_5_batery_ram <- quantile(amostra$ram, 0.05)
percentil_25_batery_ram <- quantile(amostra$ram, 0.25)
percentil_75_batery_ram <- quantile(amostra$ram, 0.75)
percentil_95_batery_ram <- quantile(amostra$ram, 0.95)

# Mínimo e máximo
minimo_batery_power <- min(amostra$battery_power)
maximo_batery_power <- max(amostra$battery_power)
minimo_ram <- min(amostra$ram)
maximo_ram <- max(amostra$ram)

#Criando variáveis
medias <- c(media_batery_power, media_ram)
medianas <- c(mediana_batery_porwer, mediana_ram)
variaveis <- c('Batery Power', 'Ram')

# Gráfico de média
barplot(medias, names.arg = variaveis, beside = True, col = 'lightblue', main = "Média")

#Gráficos de medianas
barplot(medianas, names.arg = variaveis, beside = True, col = 'lightblue', main = "Medianas")

# Variáveis com os percentis
percentis_batery_power <- c(percentil_5_batery_power, percentil_25_batery_power, percentil_75_batery_power, percentil_95_batery_power)
percentis_ram <- c(percentil_5_batery_ram, percentil_25_batery_ram, percentil_75_batery_ram, percentil_95_batery_ram)

# Variáveis
variaveis <- rep(c("batery_power", "ram"), each = 4)
valores <- c(percentis_batery_power, percentis_ram)

# Boxplot Batery Power
boxplot(percentis_batery_power, col = c('lightblue', 'lightgreen'), main = "Percentis Batery Power")

# Boxplot Ram
boxplot(percentis_ram, col = c('lightblue', 'lightgreen'), main = "Percentis Ram")

install.packages(dplyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(psych)

# Fazendo para Price Range
amostra2 <- base[sample(nrow(base), 400), ]
amostra2$price_range <- recode(amostra2$price_range, `0`='0:BAIXO', `1`='1:MEDIO', `2`='2:CARO', `3`='3:MUITO-CARO')

# Tabela de faixa de preço
price.tabela <- table(amostra2$price_range, useNA = 'ifany')
porc.price.tabela <- round(prop.table(price.tabela) * 100, 1)
print(porc.price.tabela)
price.tabela <- data.frame(price.tabela, porc.price.tabela)
price.tabela <- price.tabela[, -3]
colnames(price.tabela) <- c('Faixa_Preco', 'Frequencia', 'Porcentagem')
price.tabela

# Gráfico da tabela de faixa de preço
barplot(
  height = price.tabela$Frequencia,
  names = price.tabela$Faixa_Preco,
  col=rgb(0.36, 0.12, 0.64, 1),
  xlab = 'Faixa de Preço',
  ylab = 'Frequência',
  main = ''
)

percentil_5_price_range <- quantile(porc.price.tabela, 0.05)
print(percentil_5_price_range)


# Médias Price range
media_price_range <- mean(amostra2$price_range)
print(price_range)
mediana_price_range <- median(amostra2$price_range)

# Percentis Price range
percentil_5_price_range <- quantile(amostra$price_range, 0.05)
print(percentil_5_price_range)
percentil_25_price_range <- quantile(amostra$price_range, 0.25)
percentil_75_price_range <- quantile(amostra$price_range, 0.75)
percentil_95_price_range <- quantile(amostra$price_range, 0.95)

# Variável
percentis_price_range <- c(percentil_5_price_range, percentil_25_price_range, percentil_75_price_range, percentil_95_price_range)

boxplot(amostra$battery_power)


