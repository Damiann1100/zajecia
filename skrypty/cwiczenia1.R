#zad3
a <- 42
plot(1:10)

kwadrat <- function(x) { x^2 }
kwadrat(5)

#zad4
getwd()

#zad5
version
install.packages('ggplot2')
library(ggplot2)

install.packages('carData')
library(carData)
TitanicSurvival

#zad6
pi
obwod <- 2*pi*10
obwod

?log
log(100)
log(100, base = 10)

round(sqrt(2), 3)

27%/%4

5/0

#zad7
lista <- list(
  liczby = 1:10,
  miesiące = month.name[1:6],
  macierz = matrix(1:9, nrow = 3, ncol = 3),
  dane = data.frame(imie = c("Bartek", "Zbysiu", "Mateusz"), wiek = c(70, 24, 22))
)

lista$liczby[3]
lista$miesiące[2]
lista$macierz[, 2]
lista$dane$imie

#zad8
library(carData)
data(TitanicSurvival)

#Ile jest osób w tym zestawie danych?
nrow(TitanicSurvival)

#Ile jest kobiet, a ile mężczyzn?
table(TitanicSurvival$sex)

#Ile kobiet przeżyło? Ilu mężczyzn przeżyło?
table(TitanicSurvival$sex, TitanicSurvival$survived)

#Ile kobiet z pierwszej klasy miało powyżej 30 lat?
sum(TitanicSurvival$sex == "female" & TitanicSurvival$passengerClass == "1st" & TitanicSurvival$age > 30, na.rm = TRUE)

#zad9
x <- c(12, 15, 18, 20, 22, 25, 28, 30, 35, 40)

mean(x)
median(x)

sd(x)
var(x)

quantile(x, probs = c(0.25, 0.75))

iris
aggregate(
  cbind(Petal.Length, Petal.Width) ~ Species, 
  data = iris, 
  FUN = mean
)

#zad10
?iris