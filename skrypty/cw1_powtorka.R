#Zadanie 1. Oparatory i nazwy obiektów  

#Pewien eksperyment trwał 2026 dni. Oblicz, ile to pełnych tygodni i ile dni zostaje reszty. Użyj operatorów dzielenia całkowitego i reszty z dzielenia. 
# Liczba pełnych tygodni
tygodnie <- 2026 %/% 7

# Reszta dni
dni_reszta <- 2026 %% 7

tygodnie
dni_reszta

#Oblicz wartość wyrażenia exp(2) + log10(1000) - sqrt(81) i zaokrąglij wynik do dwóch miejsc po przecinku. 
wynik <- round(exp(2) + log10(1000) - sqrt(81), 2)
wynik

#Oblicz resztę z dzielenia wartości bezwzględnej liczby −17 przez 5. 
abs(-17) %% 5

#Które z poniższych nazw obiektów są w R niepoprawne i dlaczego? 2dane, _wynik, moje.dane, Moje_Dane, dane wejsciowe.
#2dane, _wynik, dane wejsciowe

#Zadanie 2. Wektory i indeksowanie  

#Wykonano 10 pomiarów długości (w mm). Utwórz wektor: 
pomiar <- c(12.4, 15.1, 9.8, 22.0, 18.3, 7.5, 25.6, 14.2, 19.9, 11.1) 

#Wyświetl wszystkie pomiary większe od 15 i policz, ile ich jest. 
wieksze_od_15 <- pomiar[pomiar > 15]
wieksze_od_15
liczba <- sum(pomiar > 15)
liczba

#Wyświetl pomiary stojące na pozycjach parzystych. Zbuduj indeks funkcją seq(), nie wypisuj pozycji ręcznie. 
parzyste_pozycje <- pomiar[seq(2, length(pomiar), by = 2)]
parzyste_pozycje

#Utwórz wektor bez_skrajnych, w którym nie ma pomiaru najmniejszego ani największego. Nie korzystaj z numerów pozycji – użyj warunku z funkcjami min() i max(). 
bez_skrajnych <- pomiar[pomiar != min(pomiar) & pomiar != max(pomiar)]
bez_skrajnych

#Posortuj pomiary malejąco. 
sort(pomiar, decreasing = TRUE)


#Sprawdź jednym poleceniem, czy wszystkie pomiary są większe od 5, i drugim, czy którykolwiek przekracza 25. 
# Czy wszystkie większe od 5?
all(pomiar > 5)

# Czy którykolwiek przekracza 25?
any(pomiar > 25)

#Wyświetl pomiary większe od średniej wszystkich pomiarów. 
pomiar[pomiar > mean(pomiar)]

#Wykonaj standaryzację: od każdego pomiaru odejmij średnią i podziel przez odchylenie standardowe. Wynik zapisz jako z, a następnie sprawdź jego średnią i odchylenie standardowe. Jakie wartości powinny wyjść? 
z <- (pomiar - mean(pomiar)) / sd(pomiar)
mean(z) # Powinno wyjść 0
sd(z)   # Powinno wyjść dokładnie 1
  
#Sprawdź, ile wartości ze zbioru c(9.8, 14.2, 30) występuje w wektorze pomiar. Użyj operatora %in%.
sum(c(9.8, 14.2, 30) %in% pomiar)

#Zadanie 3. Tworzenie wektorów funkcjami rep() i seq()   

#Każdy z poniższych wektorów utwórz jednym poleceniem, bez wypisywania wartości ręcznie. 

#"A" "A" "A" "B" "B" "B" "C" "C" "C" 
rep(c("A", "B", "C"), each = 3)

#1 2 3 1 2 3 1 2 3 
rep(1:3, times = 3)

#5 5 5 10 10  
rep(c(5, 10), times = c(3, 2))

#100 90 80 70 60 50 40 30 20 10 
seq(100, 10, by = -10)

#pięć liczb równo oddalonych od siebie, od 0 do 1 włącznie (użyj seq, który argument na to pozwala?) 
seq(0, 1, length.out = 5)

#2 2 4 4 6 6 – bez wypisywania liczb, łącząc rep() i seq()
rep(seq(2, 6, by = 2), each = 2)

#Zadanie 5. Listy  

#Utwórz listę student zawierającą: imię (tekst), wektor pięciu ocen c(4.5, 3.0, 5.0, 3.5, 4.0), informację, czy przedmiot został zaliczony (wartość logiczna), oraz macierz 2×3 z liczbami od 1 do 6. 
student <- list(
  imie = "Anna",
  oceny = c(4.5, 3.0, 5.0, 3.5, 4.0),
  zaliczony = TRUE,
  mat = matrix(1:6, nrow = 2)
)

#Wyświetl nazwy elementów listy oraz jej strukturę. 
names(student)
str(student)

#Wyświetl drugą i czwartą ocenę na dwa sposoby: przez nazwę elementu i przez jego numer. 
student$oceny[c(2, 4)]

#Dopisz do listy nowy element srednia zawierający średnią ocen. 
student$srednia <- mean(student$oceny)
student$srednia

#Popraw drugą ocenę na 4.0 i przelicz średnią. Ile teraz wynosi? 
student$oceny[2] <- 4.0
student$srednia <- mean(student$oceny)
student$srednia
  
#Wyświetl drugą kolumnę macierzy zapisanej w liście. 
student$mat[, 2]

#Sprawdź, ile elementów ma teraz lista, i policz, ile ocen jest większych lub równych 4.
length(student)
sum(student$oceny >= 4)

#Zadanie 6. Ramki danych: dane crabs  

#Załaduj pakiet MASS i dane crabs (pomiary krabów: sp – gatunek, sex – płeć, FL, RW, CL, CW, BD – wymiary w mm): 
library(MASS) 
data(crabs) 

#Sprawdź wymiary ramki danych i nazwy wszystkich kolumn. 
dim(crabs)
colnames(crabs)

#Zbuduj tabelę liczebności dla kombinacji gatunku i płci. Czy grupy są równoliczne? 
table(crabs$sp, crabs$sex)#tak

#Policz, jaki udział w danych ma każdy gatunek (proporcje, nie liczebności). 
prop.table(table(crabs$sp))

#Utwórz podzbiór: samce gatunku B o długości pancerza CL większej niż 30. Ile krabów spełnia ten warunek i jaka jest ich średnia szerokość pancerza CW? 
podzb <- crabs[crabs$sp == "B" & crabs$sex == "M" & crabs$CL > 30, ]
nrow(podzb)
mean(podzb$CW)
  
#Policz średnią szerokość pancerza CW osobno dla samic i samców. 
aggregate(CW ~ sex, data = crabs, mean)

#Oblicz odchylenie standardowe zmiennej CL.
sd(crabs$CL)