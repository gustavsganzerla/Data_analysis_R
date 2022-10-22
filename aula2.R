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

boxplot_r_base <- boxplot(casa_1_estacionamento$price, casa_2_estacionamento$price,
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

sample(lista, 2)

######
install.packages("ggplot2")
install.packages("ggpubr")

library(ggplot2)
library(ggpubr)

####dispersao

disp <- ggplot()+
  geom_point(data = casas, aes(x = area, y = price), color = "darkviolet")+
  xlab("Area")+
  ylab("Valor")+
  ggtitle("Dispersão")+
  theme_classic()+
  geom_hline(yintercept = mean(casas$price), color = "green", linetype = 4)+
  annotate("text", x = 2000, y = 10000000, label = "0.53")+
  stat_smooth(method = lm)

disp  

cor(casas$area, casas$price)






###barras
barras <- ggplot()+
  geom_bar(data = casas, aes(x = bedrooms), stat = "count", 
           color = "black", fill = "blue")+
  theme_classic()

barras

###boxplot
boxplot <- ggplot(data = casas, 
                  aes(x = as.factor(bedrooms), y = price, 
                      color = as.factor(bedrooms)))+
  geom_boxplot()+
  theme_classic()

boxplot

###linhas
linhas <- ggplot()+
  geom_line(data = carros, 
            aes(x = dono, y = selling_price))

#######regressão linear
modelo <- lm(data = casas, price~area)

summary(modelo)

library(stargazer)
stargazer(modelo, type = "html", out = "~/Desktop/R/modelo.html")

####mesclar dataframes
clientes1 <- read.csv("~/Desktop/R/clientes_1.csv")
clientes2 <- read.csv("~/Desktop/R/clientes2.csv")

colnames(clientes1)
colnames(clientes2)


###join
clientes_novo <- merge(x = clientes1, y = clientes2, 
                       by = "Clientes")

######
ggplot(data = casas, aes(x = area, y = price))+
  geom_point()+
  stat_smooth(method = lm, color = "yellow")


modelo <- lm(data = casas, price~area)

summary(modelo)
confint(modelo)

2387308 + 462 * 3290


install.packages("readxl")
library(readxl)

read_xlsx("")

####
voos <- read.csv("~/Desktop/R/flight_data_2018_to_2022.csv")

table <- data.frame(table(voos$Origin))
table

table2 <- subset(table, table$Freq>2000)



ggplot()+
  geom_bar(data = table2, aes(x = Var1, y = Freq), stat = "identity", color = "blue")+
  coord_flip()+
  theme_classic()+
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 10))


table(voos$Operating_Airline.)
