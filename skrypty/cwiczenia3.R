data(crabs)
?crabs

crabs %>%
  filter(FL>9, sp=='B') %>%
  dplyr::select(RW)

#cw 21.1
#Wybierz tylko samice krabów.
crabs %>% 
  filter(sex == "F")
#Wybierz kraby gatunku „O” z FL < 15.
crabs %>% 
  filter(sp == "O", FL < 15)
#Posortuj kraby według: gatunek → płeć → CW (rosnąco).
crabs %>% 
  arrange(sp, sex, CW)
#Utwórz kolumnę CW_squared (kwadrat CW).
crabs %>% 
  mutate(CW_squared = CW^2)
#Dla każdej płci oblicz: liczbę, średnią CW, medianę CW, odchylenie standardowe CW.
crabs %>% 
  group_by(sex) %>% 
  summarise(
    licznosc = n(),
    srednia_CW = mean(CW),
    mediana_CW = median(CW),
    odchylenie_std_CW = sd(CW)
  )
#Policz kraby według gatunku i płci, posortuj malejąco.
crabs %>% 
  count(sp, sex, sort = TRUE)
#Utwórz potok, który:
#filtruje kraby z FL > 15,
#tworzy nową kolumnę size_category („Duża”, jeśli CW > 30, „Mała” w przeciwnym razie),
#wybiera gatunek, płeć, CW i size_category,
#sortuje według gatunku, a następnie CW.
crabs %>% 
  filter(FL > 15) %>% 
  mutate(size_category = if_else(CW > 30, "Duża", "Mała")) %>% 
  select(sp, sex, CW, size_category) %>% 
  arrange(sp, CW)


#cw 21.2
install.packages('nycflights13')
library(nycflights13)
data(flights)
#Wybierz loty z JFK obsługiwane przez United Airlines z opóźnieniem > 60 min.
flights %>% 
  filter(origin == "JFK", carrier == "UA", dep_delay > 60)
#Utwórz pipeline, który:
#filtruje tylko loty ze stycznia,
#wybiera kolumny: carrier, flight, dep_delay, arr_delay,
#filtruje, gdy dep_delay jest dodatnie (tylko opóźnione wyloty),
#sortuje według dep_delay w kolejności malejącej,
flights %>% 
  filter(month == 1) %>% 
  select(carrier, flight, dep_delay, arr_delay) %>% 
  filter(dep_delay > 0) %>% 
  arrange(desc(dep_delay)) %>% 
  slice_head(n = 20)
#Oblicz średnie opóźnienie odlotu dla każdego przewoźnika.
flights %>% 
  group_by(carrier) %>% 
  summarise(srednie_dep_delay = mean(dep_delay, na.rm = TRUE))


#cw 21.3
#Stwórz ramkę danych df1 zawierającą kolumny group, value1 i value2:
#set.seed(123)
#df1 <- data.frame(
#  group = sample(c("A", "B", "C"), 100, replace = TRUE),
#  value1 = runif(100, min = 5, max = 20),
#  value2 = runif(100, min = 1, max = 100)
#)
#Użyj funkcji filter(), aby wybrać tylko te wiersze, w których value1 jest większe niż 10. Następnie użyj funkcji group_by() i summarise(), aby obliczyć średnią value2 dla każdej grupy group.
set.seed(123)
df1 <- data.frame(  
  group = sample(c("A", "B", "C"), 100, replace = TRUE),  
  value1 = runif(100, min = 5, max = 20),  
  value2 = runif(100, min = 1, max = 100)
)

df1 %>% 
  filter(value1 > 10) %>% 
  group_by(group) %>% 
  summarise(srednia_value2 = mean(value2))

#Stwórz ramkę danych df3 z kolumnami kategoria, podkategoria, cena i jakość:
#   set.seed(123)
#df3 <- data.frame(
#  kategoria = sample(c("Elektronika", "Gospodarstwo Domowe", "Odzież"),
#                     100, replace = TRUE),
#  podkategoria = sample(c("Telefony", "Sprzęt AGD", "Koszulki"),
#                        100, replace = TRUE),
#  cena = runif(100, min = 50, max = 5000),
#  jakość = sample(1:20, 100, replace = TRUE)
#)
#Użyj funkcji group_by(), aby pogrupować dane według zmiennych kategoria i podkategoria. Następnie użyj funkcji summarise() w połączeniu z across(), aby obliczyć średnią wartość zmiennych numerycznych (cena i jakość).
set.seed(123)
df3 <- data.frame(  
  kategoria = sample(c("Elektronika", "Gospodarstwo Domowe", "Odzież"), 100, replace = TRUE),  
  podkategoria = sample(c("Telefony", "Sprzęt AGD", "Koszulki"), 100, replace = TRUE),  
  cena = runif(100, min = 50, max = 5000),  
  jakość = sample(1:20, 100, replace = TRUE)
)

df3 %>% 
  group_by(kategoria, podkategoria) %>% 
  summarise(across(c(cena, jakość), mean), .groups = "drop")


#cw 21.5
#Dla danych TitanicSurvival z pakietu carData spróbuj uzupełnić zdania:
library(carData)
data("TitanicSurvival")
#Średni wiek ….. wraz z klasą – pasażerowie …….. klasy byli starsi.
TitanicSurvival %>% 
  group_by(passengerClass) %>% 
  summarise(sredni_wiek = mean(age, na.rm = TRUE))
#Kobiety miały znacznie …… wskaźnik przeżycia niż mężczyźni.
TitanicSurvival %>% 
  group_by(sex) %>% 
  summarise(przezycie_proc = mean(survived == "yes") * 100)
#Pasażerowie …. klasy mieli znacznie większą szansę na przeżycie niż ci z ….. klasy.
TitanicSurvival %>% 
  group_by(passengerClass) %>% 
  summarise(przezycie_proc = mean(survived == "yes") * 100)
#Najbardziej narażeni byli ….. z …… klasy w wieku dorosłym.
TitanicSurvival %>% 
  filter(age >= 18) %>%
  group_by(passengerClass, sex) %>% 
  summarise(przezycie = mean(survived == "yes")) %>% 
  arrange(przezycie)
#Największą przeżywalność odnotowano wśród ….. z ……. klasy w grupie dorosłych.
TitanicSurvival %>% 
  filter(age >= 18) %>%
  group_by(passengerClass, sex) %>% 
  summarise(przezycie = mean(survived == "yes")) %>% 
  arrange(desc(przezycie))

#cw 21.6
#Poniższe zadania pozwalają powtórzyć materiał z zajęć 2 i 3. Wykorzystaj zbiór danych mtcars, dostępny w bazowym R.
#Wczytanie i podstawowa eksploracja danych:
#wczytaj dane mtcars,
data(mtcars)
#wyświetl pierwsze 6 wierszy,
head(mtcars)
#sprawdź strukturę danych,
str(mtcars)
#sprawdź, ile jest wierszy w zbiorze danych,
nrow(mtcars)
#wyświetl podsumowanie statystyczne.
summary(mtcars)

#Funkcje bazowe R:
#policz, ile samochodów ma daną liczbę cylindrów,
table(mtcars$cyl)
#stwórz podzbiór danych zawierający tylko samochody z silnikiem V8 (8 cylindrów),
samochody_v8 <- mtcars[mtcars$cyl == 8, ]
#sprawdź, ile jest samochodów z silnikiem V8.
nrow(samochody_v8)

#Analiza z pakietem dplyr:
#pogrupuj dane ze względu na liczbę cylindrów oraz stwórz nowe zmienne: średnie zużycie paliwa, średnia moc i liczba aut,
mtcars %>% 
  group_by(cyl) %>% 
  summarise(
    srednie_mpg = mean(mpg),
    srednia_moc = mean(hp),
    liczba_aut = n()
  )
#przefiltruj dane, wyświetlając tylko samochody z mocą silnika > 200, i uporządkuj je malejąco według zużycia paliwa.
mtcars %>% 
  filter(hp > 200) %>% 
  arrange(desc(mpg))

#Wizualizacja danych (pakiet ggplot2):
library(ggplot2)
#zrób wykres pudełkowy zużycia paliwa w zależności od liczby cylindrów,
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot() +
  labs(title = "Zużycie paliwa a liczba cylindrów",
       x = "Liczba cylindrów",
       y = "Zużycie paliwa (mpg)",
       fill = "Cylindry") +
  theme_minimal()
#zrób wykres punktowy mocy względem zużycia paliwa.
ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
  geom_point(size = 3) +
  labs(title = "Zależność mocy od zużycia paliwa",
       x = "Zużycie paliwa (mpg - wyżej = oszczędniej)",
       y = "Moc (hp)",
       color = "Cylindry") +
  theme_minimal()

#Jaka jest średnia moc samochodu dla każdej liczby cylindrów?
mtcars %>% 
  group_by(cyl) %>% 
  summarise(srednia_moc_hp = mean(hp))

#Które samochody mają najlepsze, a które najgorsze zużycie paliwa?
mtcars %>% arrange(desc(mpg)) %>% head(1)
mtcars %>% arrange(mpg) %>% head(1)

#cw 21.4
install.packages('tidyr')
library(tidyr)


oceny <- tibble(  
  uczeń = c("Anna", "Bartek", "Celina", "Darek"),  
  przedmiot = c("Matematyka", "Matematyka", "Nauka", "Nauka"),  
  wynik = c(80, 90, 70, 85)
)

szerokie <- oceny %>% 
  spread(key = przedmiot, value = wynik)

print(szerokie)

szerokie %>% 
  gather(key = przedmiot, value = wynik, -uczeń)