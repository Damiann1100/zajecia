install.packages("rio")
#Załaduj pierwszy arkusz z pliku studenci.xlsx.
library(rio)
studenci <- import("data/studenci.xlsx", sheet = 1)

#Załaduj plik koty_ptaki.csv ze strony http://biecek.pl/MOOC/dane/ (separatorem jest tutaj ;).
koty_ptaki <- read.csv("http://biecek.pl/MOOC/dane/koty_ptaki.csv", sep = ";")

#Załaduj obiekt koty_ptaki.rda ze strony http://biecek.pl/MOOC/dane/.
load(url("http://biecek.pl/MOOC/dane/koty_ptaki.rda"))

#Sprawdź, jakie dane są dostępne w pakiecie dplyr.
data(package = "dplyr")

#Pobierz przykładowy zbiór danych mtcars i zapisz go jako plik CSV w folderze data.
# Wczytanie wbudowanego zbioru danych mtcars do środowiska
data("mtcars")

write.csv(mtcars, file = "data/mtcars.csv")

#Załaduj zbiór danych airquality. Sprawdź, o czym jest ten zbiór danych, oraz jaka jest jego struktura. Ile wierszy i kolumn ma ramka danych? Wyświetl tylko nazwy kolumn. Oblicz podstawowe statystyki opisowe.
data("airquality")
str(airquality)
dim(airquality)
colnames(airquality)
summary(airquality)

#Wyświetl pierwsze 10 wartości kolumny Ozone.
head(airquality$Ozone, 10)

#Wybierz wszystkie wiersze, w których temperatura (Temp) jest większa niż 85°F.
airquality[airquality$Temp > 85, ]

#Wyświetl tylko kolumny Ozone i Temp dla dni z lipca (Month == 7).
airquality[airquality$Month == 7, c("Ozone", "Temp")]

#Sprawdź, ile jest braków (NA) w każdej kolumnie.
colSums(is.na(airquality))

#Usuń wszystkie wiersze z brakami i zapisz wynik do nowego obiektu.
airquality_clean <- na.omit(airquality)
airquality_clean

#Wartości brakujące w kolumnie Ozone zastąp średnią z tej kolumny.
airquality_imputed <- airquality
airquality_imputed$Ozone[is.na(airquality_imputed$Ozone)] <- mean(airquality_imputed$Ozone, na.rm = TRUE)

#Sprawdź ponownie, czy są jeszcze jakieś NA.
colSums(is.na(airquality_imputed))

#Na podstawie danych z pliku studenci.xlsx odpowiedz na następujące pytania:
  #Ile kobiet, a ilu mężczyzn studiuje Leśnictwo?
table(studenci$Płeć[studenci$`Kierunek studiów` == "Leśnictwo"])
  #Jaka jest średnia stypendium naukowego dla kobiet, a jaka dla mężczyzn?
tapply(studenci$`Stypendium naukowe`, studenci$Płeć, mean)
  #Ile kobiet studiuje Rolnictwo (użyj funkcji subset())?
nrow(subset(studenci, `Kierunek studiów` == "Rolnictwo" & Płeć == "kobieta"))
  #Ilu studentów Leśnictwa nie ma stypendium naukowego?
nrow(subset(studenci, `Kierunek studiów` == "Leśnictwo" & `Stypendium naukowe` == 0))

#Korzystając z obiektu koty_ptaki, spróbuj wykonać następujące polecenia:
  #Wyświetl zawartość zmiennej gatunek.
koty_ptaki$gatunek
  #Wyświetl drugi wiersz danych.
koty_ptaki[2, ]
  #Usuń drugi wiersz danych.
koty_ptaki_bez_drugiego <- koty_ptaki[-2, ]
  #Wyświetl drugi, trzeci i czwarty wiersz dla czterech pierwszych kolumn.
koty_ptaki[2:4, 1:4]
  #Znajdź podzbiór zwierząt, które osiągają prędkość większą niż 100 km/h.
koty_ptaki[koty_ptaki$predkosc > 100, ]

#Sprawdź bieżący katalog roboczy.
getwd()

#Wyświetl zawartość bieżącego katalogu.
dir()

#Utwórz nowy katalog do ćwiczeń (w bieżącej lokalizacji).
dir.create("cwiczenia")

#Zmień katalog roboczy na nowo utworzony.
setwd("cwiczenia")

#Sprawdź, czy ścieżka się zmieniła.
getwd()

#Utwórz kilka obiektów testowych: test1 <- "To jest test", test2 <- 1:10, test3 <- data.frame(x = 1:3, y = c("a", "b", "c")), a następnie zapisz całą przestrzeń roboczą pod nazwą cala_sesja.RData. Wyczyść całą przestrzeń roboczą, a następnie załaduj zapisaną sesję.
test1 <- "To jest test"
test2 <- 1:10
test3 <- data.frame(x = 1:3, y = c("a", "b", "c"))

save.image("cala_sesja.RData")

rm(list = ls())

load("cala_sesja.RData")