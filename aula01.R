####tipos de dados

numero <- 25.5 
texto <- "ola"
logico <- FALSE

is.numeric(numero)
is.character(numero)

is.numeric()

#####
lista <- c(1,2,3,4,5,6,7,8,9)

sum(lista)
mean(lista)

nomes <- c("Joao", "Carla", "Antonio", "Luiza")
is.character(lista)

lista_composta <- c(1,2,3, "joao", "carla")

cidades <- c("Caxias", "Bento", "Sao Marcos",
             "Lajeado")

cidades <- as.factor(cidades)

lista <- c("1", "2", "3")
mean(lista)

is.numeric(lista)
is.character(lista)

cidades <- as.numeric(cidades)
mean(lista)

print(cidades)

#####
#####
print(numero)
numero <- 10

numero_vezes_3 <- numero * 3

sqrt(numero_vezes_3)
numero_vezes_3 <- numero_vezes_3^2

print(numero)

###logica
numero > 10
numero < 10
numero == 30
numero != 30

numero <- 7

(numero < 10) & (numero > 5) & (numero == 7)
(numero == 6) | (numero < 10)

#####vetores
debito <- c(0, 10, 40, -50, 10)
credito <- c(10, 0, 0, 10, 10)
print(debito)

clientes <- c("Marcos", "Antonia", "Carla", "Juca", "Sergio")

length(debito)
length(credito)

##indexacao
clientes[c(1,4)]

is.character(clientes)

credito * debito

##matriz
matriz <- cbind(debito, credito)

###linha, coluna
matriz[2,]
matriz <- matriz*2
matriz

matriz <- t(matriz)

matriz <- cbind(matriz, clientes)

head(matriz)
tail(matriz)

###antes da virgula = linhas
###depois da virgula = colunas
matriz[,1]
matriz[4:5,2]
matriz[c(1,5),2]

matriz[,1] < 0 

####data frames
matriz <- as.data.frame(matriz)
matriz[,2]
matriz$credito
matriz$clientes[4]

matriz$debito <- as.numeric(matriz$debito)
matriz$credito <- as.numeric(matriz$credito)

sum(matriz$credito)
mean(matriz$credito)

matriz$debito < 0

matriz$debito <- matriz$debito*2

matriz[matriz$clientes=="Juca",1]


df <- as.data.frame(c(1,2,3,4,5))


df <- as.data.frame(df)

df$clientes <- c("oi", "ola", "aa", "aa", "bb")

write.csv(df, "~/Desktop/R/data_frame.csv")

######
rafael <- read.csv("~/Desktop/R/pacientes.csv")

View(rafael)

mean(rafael$idade)
mean(rafael$peso)
mean(rafael$filhos)

table(rafael$sexo)
table(rafael$cidade)

table(rafael$sexo, rafael$cidade)


###criar nova variavel
rafael$altura <- c(1.8, 1.7, 1.6, 1.9, 1.8, 1.7)
rafael$imc <- rafael$peso/rafael$altura^2

rafael$peso <- rafael$peso/0.4

###entrega

churn <- read.csv("~/Desktop/R/Churn_Modelling.csv")

##get to know the data

###subset

colnames(churn)
subset(churn, Gender =="Male" & IsActiveMember==1)

subset(churn, Gender =="Male" & IsActiveMember==1, select = c(Surname, Geography, Gender))
subset(churn, Gender =="Male" & IsActiveMember==1, select = -c(Tenure))

churn_male <- subset(churn, Gender =="Male" & IsActiveMember==1, select = c(Surname, Geography, Gender))

churn_male$nova_variavel <- 1:2867


##se apropriem do churn

####pacotes
#cran

install.packages("rio")
install.packages("stargazer")

library(rio)
library(stargazer)


aa <- import("https://raw.githubusercontent.com/gustavsganzerla/Data_analysis_R/main/googleplaystore.csv")


##traduzir colunas

mean(churn$CreditScore)
sd(churn$CreditScore)
summary(churn$CreditScore)

##passar um df subset
stargazer(churn_male)

stargazer(subset(churn, Gender =="Male" & IsActiveMember==1, select = c(Balance)),
          type = 'text')

stargazer(subset(churn, Gender =="Male" & IsActiveMember==1, select = c(Balance, Age)),
          type = 'text',
          out = "~/Desktop/R/summary.txt")

#######




housing <- read.csv("~/Desktop/R/newhousing.csv")
colnames(housing)

table(housing$bedrooms)
table(housing$bedrooms, housing$stories, dnn = c("Quartos", "Andares"))


####scatter plot
cor(housing$price, housing$area)

plot(housing$area, housing$price, xlab = "Area", ylab = "Valor", 
     main = "Relação entre valor e área", col = "green")
abline(mean(housing$price), 0, col = "blue", lty = 2, lwd = 2)


###histogram
housing$stories <- as.integer(housing$stories)
hist(housing$stories)
plot(density(housing$stories))

###barplot
barplot(table(housing$hotwaterheating))


plot(aggregate(housing$price~housing$bedrooms, FUN = mean), type = "b")


mainroad1 <- subset(housing, housing$mainroad==1)
mainroad0 <- subset(housing, housing$mainroad==0)

boxplot(mainroad0$price, mainroad1$price)

colnames(housing)

####ggplot

ggplot()+
  geom_boxplot(data = housing, aes(x = as.factor(bedrooms), y = price))

ggplot()+
  geom_point(data = housing, aes(x = area, y = price))

ggplot()+
  geom_bar(data = housing, aes(x = as.factor(bedrooms)), stat = "count")

ggplot()+
  geom_bar(data = housing, aes(x = as.factor(bedrooms), y = price), stat = "identity")

