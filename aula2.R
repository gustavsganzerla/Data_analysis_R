install.packages("rio")
library(rio)


#######
casas <- import("https://raw.githubusercontent.com/gustavsganzerla/Data_analysis_R/main/newhousing.csv")
table(casas$bedrooms, casas$bathrooms, casas$stories, casas$mainroad)

unique(casas$bedrooms)
unique(casas$bathrooms)

#######plot
###correlacao entre variaveis
cor(casas$price, casas$area, method = "spearman")

media_area <- mean(casas$area)

###gráfico de dispersão (scatterplot)
plot(x = casas$area, y = casas$price, 
     xlab = "Preço", ylab = "Área",
     main = "Correlação de valor e área",
     col = "cyan2")
abline(mean(casas$price), 0, col = "red", lwd = 1.2,
       lty = 4)

###gráfico de barras
barplot(table(casas$bedrooms), xlab = "N de quartos",
        ylab = "Quantidade de casas",
        main = "Distribuição de quartos/casas",
        col = "green")

###histograma
hist(casas$stories)

###gráfico de linha

##agregar os preços das casas com diferentes numeros de quartos
aggregate(casas$price~casas$bedrooms, FUN = mean)

aggregate(data = casas, price~bedrooms, FUN = mean)

plot(aggregate(casas$price~casas$bedrooms, FUN = mean),
     type = "b", col = "purple", ylab = "Valor", xlab = "Quartos")


#####
unique(casas$parking)

casa_0_estacionamento <- subset(casas, casas$parking==0)
casa_1_estacionamento <- subset(casas, casas$parking==1)
casa_2_estacionamento <- subset(casas, casas$parking==2)
casa_3_estacionamento <- subset(casas, casas$parking==3)

aggregate(casas$price~casas$parking, FUN = mean)

options(scipen=999)

boxplot(casa_1_estacionamento$price, casa_2_estacionamento$price,
        names = c("1 vaga", "2 vagas"))

##p < 0.05 ha diferenca
##teste de normalidade
shapiro.test(casa_1_estacionamento$price)
shapiro.test(casa_2_estacionamento$price)

##teste de comparacao de medias
t.test(casa_1_estacionamento$price, casa_2_estacionamento$price)
wilcox.test(casa_1_estacionamento$price, casa_2_estacionamento$price)


boxplot(casa_0_estacionamento$price, casa_1_estacionamento$price,
        casa_2_estacionamento$price, col = c("blue", "pink", "green"),
        names = c("0 vaga", "1 vaga", "2 vagas"))



###
carros <- import("https://raw.githubusercontent.com/gustavsganzerla/Data_analysis_R/main/CAR%20DETAILS%20FROM%20CAR%20DEKHO.csv")

aggregate(carros$selling_price~carros$dono, FUN = mean)



install.packages("dplyr")
library(dplyr)

# Adding column based on other column:
carros <- carros %>%
  mutate(dono = case_when(
    endsWith(owner, "First Owner") ~ 1,
    endsWith(owner, "Test Drive Car") ~ 1,
    endsWith(owner, "Second Owner") ~ 2,
    endsWith(owner, "Third Owner") ~ 3,
    endsWith(owner, "Fourth & Above Owner") ~ 4,
  ))

unique(carros$owner)

plot(aggregate(carros$selling_price~as.integer(carros$dono), FUN = mean),
     type = "b")


is.na(carros$dono)

carros <- na.omit(carros)

cor(carros$selling_price, carros$dono, method = "pearson")

lista = c("Caroline", "Renato")

sample(lista, 1)

modelo <- lm(carros$selling_price~carros$dono)
summary(modelo) 

library(stargazer)
stargazer(modelo, type = "text")
stargazer(modelo, type = "html", out = "~/Desktop/R/modelo.html")

plot(carros$selling_price~carros$dono)

