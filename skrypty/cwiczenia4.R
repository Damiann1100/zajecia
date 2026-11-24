library(MASS)
library(ggplot2)
library(dplyr)
#cw21
#Stwórz wykres punktowy zależności między zmiennymi CL i FL ze zbioru crabs.
ggplot(crabs, aes(x = CL, y = FL)) +
  geom_point()
#Dodaj do wykresu mapowanie kolorów na podstawie gatunku.
ggplot(crabs, aes(x = CL, y = FL, color = sp)) +
  geom_point() +
  labs(color = "Gatunek")
#Utwórz histogram długości pancerza (CL) dla różnych gatunków.
ggplot(crabs, aes(x = CL, fill = sp)) +
  geom_histogram(position = "dodge", bins = 15, color = "black")
#Stwórz wykres pudełkowy długości pancerza dla różnych gatunków.
ggplot(crabs, aes(x = sp, y = CL, fill = sp)) +
  geom_boxplot()
#Stwórz złożony wykres, który pokaże relacje między wszystkimi zmiennymi numerycznymi w zbiorze crabs.

#Przygotuj wizualizację, która porówna rozmiary poszczególnych części ciała kraba w zależności od gatunku.
library(tidyr)
crabs.long <- crabs %>%
  pivot_longer(
    cols = FL:BD, 
    names_to = "cecha", 
    values_to = "rozmiar"
  )

ggplot(crabs.long, aes(x = cecha, y = rozmiar, fill = sp)) +
  geom_boxplot()
#Na podstawie danych birthwt z pakietu MASS utwórz wykres punktowy opisujący zależność pomiędzy wagą urodzeniową dziecka a wagą matki sprzed ciąży. Co możesz powiedzieć o tej zależności?
ggplot(birthwt, aes(x = lwt, y = bwt)) +
  geom_point()
#Korzystając z danych hla.long, policz reguły statystyki opisowej dla każdego markera: liczbę pomiarów, średnią i odchylenie standardowe ekspresji. Następnie funkcją pivot_wider() przedstaw same średnie w tabeli, w której wiersze odpowiadają markerom, a kolumny grupom (0 i 1). W którym markerze różnica między grupami jest największa?
install.packages('gap')
library(gap)
hla.long <- hla %>%  
  pivot_longer(    
    cols      = -c(id, y),    
    names_to  = "marker",     
    values_to = "ekspresja"   
  ) %>%  
  mutate(y = as.factor(y)) %>%  
  rename(grupa = y)

hla.long %>%
  group_by(marker, grupa) %>%
  summarise(
    n = n(),
    srednia = mean(ekspresja),
    sd = sd(ekspresja),
    .groups = "drop"
  )

tabela_srednich <- hla.long %>%
  group_by(marker, grupa) %>%
  summarise(srednia = mean(ekspresja), .groups = "drop") %>%
  pivot_wider(
    names_from = grupa,
    values_from = srednia
  )
tabela_srednich

tabela_srednich %>%
  mutate(roznica = abs(`0` - `1`)) %>%
  arrange(desc(roznica))
#Na podstawie danych z pliku Protein.txt (do pobrania poniżej) stwórz zbiór danych w formacie long i nazwij go protein.long.
protein.long <- protein %>%
  pivot_longer(
    cols = -Country,
    names_to = "zrodlo",
    values_to = "spozycie"
  )


#cw22
#Stwórz wykres punktowy zależności między CL i FL, gdzie kolor zależy od gatunku, przezroczystość jest ustawiona na 0,5, a kształt zależy od płci.
ggplot(crabs, aes(FL, CL, color = sp, shape = sex)) + geom_point(alpha = 0.5)
#Na podstawie danych hla.long wykonaj wykres punktowy zależności między markerami a ekspresją. Dodaj różne wielkości punktów w zależności od id. Zastanów się nad czytelnością wykresu i zmień go tak, aby był czytelny.
ggplot(hla.long, aes(marker, ekspresja)) + geom_point()
#Dla wykresu z zadania 7 z poprzedniego rozdziału (zależność wagi urodzeniowej dziecka od wagi matki sprzed ciąży) uwzględnij zmienne race i ht, korzystając z wybranych przez Ciebie estetyk.
ggplot(birthwt, aes(lwt, bwt, color = factor(race), shape = factor(ht))) + geom_point()
#Pobierz poniższy plik BodyTemperature.txt i załaduj go. Wykonaj wykres punktowy dla zmiennych HeartRate i Temperature z uwzględnieniem płci (zmienna Gender).
df <- read.table("data/BodyTemperature.txt", header = TRUE)
ggplot(df, aes(Temperature, HeartRate, color = Gender)) + geom_point()

#cw23
#Wykonaj wykres punktowy zależności między CL i FL. Podziel go na panele według gatunku i dodaj kolor zależny od płci.
ggplot(crabs, aes(x = FL, y = CL, color = sex)) +
  geom_point() +
  facet_wrap(~ sp)
#Na podstawie danych birthwt stwórz wykres zależności wagi matki sprzed ciąży i wagi urodzeniowej dziecka. Podziel go na panele według rasy i nadciśnienia oraz uwzględnij informacje o paleniu.
birthwt$race <- factor(birthwt$race)
birthwt$ht <- factor(birthwt$ht)
birthwt$smoke <- factor(birthwt$smoke)

ggplot(birthwt, aes(x = lwt, y = bwt, color = smoke)) +
  geom_point() +
  facet_grid(race ~ ht)
#Na podstawie danych hla.long wykonaj wykres punktowy, który będzie pokazywał zależność pomiędzy markerami a ekspresją dla każdej grupy oddzielnie. Z jakiej funkcji skorzystasz?
ggplot(hla.long, aes(x = marker, y = ekspresja)) +
  geom_point() +
  facet_wrap(~ grupa)
#Na podstawie danych BodyTemperature.txt wykonaj wykres punktowy dla zmiennych HeartRate i Temperature na dwóch oddzielnych wykresach zależnych od płci (zmienna Gender).
df <- read.table("data/BodyTemperature.txt", header = TRUE)

ggplot(df, aes(x = Temperature, y = HeartRate)) +
  geom_point() +
  
#cw24
install.packages('palmerpenguins')
library(palmerpenguins)

#Obejrzyj dane: sprawdź ich wymiary, nazwy kolumn i podsumowanie. Zajrzyj też do dokumentacji (?penguins). Ile pingwinów każdego gatunku zmierzono na poszczególnych wyspach?
dim(penguins)
names(penguins)
summary(penguins)
?penguins

table(penguins$species, penguins$island)
#Wykonaj wykres punktowy zależności długości płetwy (flipper_length_mm) od masy ciała (body_mass_g). Jaki widzisz związek między tymi cechami?
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point()
#Narysuj ten sam wykres, zastępując geom_point() funkcją geom_line(). Dlaczego taki wykres jest nieczytelny i do jakich danych lepiej nadaje się wykres liniowy?
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_line()
#Przy rysowaniu wykresów R wypisuje ostrzeżenie o usuniętych wierszach. Sprawdź, ile braków danych (NA) mają zmienne body_mass_g oraz sex, i wyjaśnij, skąd bierze się to ostrzeżenie.
sum(is.na(penguins$body_mass_g))
sum(is.na(penguins$sex))
#Zrób histogram masy ciała. Dobierz szerokość przedziału argumentem binwidth, a następnie ustaw kolor wypełnienia słupków i kolor ich obramowania. Porównaj wyniki dla binwidth = 50, 200 i 1000 — która wersja najlepiej pokazuje kształt rozkładu?
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black")

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200, fill = "blue", color = "black")

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black")
#200 najlepiej

#Zrób wykres słupkowy pokazujący liczbę pingwinów każdego gatunku. Czym różni się ten wykres od histogramu z poprzedniego zadania?
ggplot(penguins, aes(x = species, fill = species)) +
  geom_bar()
#Wróć do wykresu punktowego z zadania 2 i zmapuj kolor punktów na gatunek. Co nowego widać po dodaniu koloru?
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm, color = species)) +
  geom_point()
#Na tym samym wykresie zmapuj na gatunek jednocześnie kolor, rozmiar i kształt punktów. Oceń czytelność wyniku: czy kodowanie jednej zmiennej trzema estetykami naraz coś wnosi?
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm, 
                     color = species, size = species, shape = species)) +
  geom_point()
#Narysuj wykres punktowy, w którym wszystkie punkty są niebieskie i półprzezroczyste. Uwaga: to zadanie wymaga ustawienia koloru poza funkcją aes() — zastanów się dlaczego.
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(color = "blue", alpha = 0.5)
#Podziel wykres punktowy z zadania 7 na panele według wyspy. Czy na którejkolwiek wyspie występują wszystkie trzy gatunki? Porównaj wykres z tabelą z zadania 1.
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm, color = species)) +
  geom_point() +
  facet_wrap(~ island)
#Wykonaj wykres punktowy długości dzioba (bill_length_mm) względem jego wysokości (bill_depth_mm) z kolorem według gatunku i panelami według płci. Co zwraca uwagę w panelu z brakującą płcią?
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  facet_wrap(~ sex)
#Korzystając z poznanej funkcji pivot_longer(), przekształć cztery kolumny z pomiarami (bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) do formatu długiego, a następnie narysuj wykresy pudełkowe wartości w podziale na gatunki, z osobnym panelem dla każdej mierzonej cechy. Wskazówka: żeby każdy panel miał własną skalę osi pionowej, użyj argumentu scales = "free_y" w funkcji facet_wrap().
penguins_long <- pivot_longer(
  penguins,
  cols = c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g),
  names_to = "cecha",
  values_to = "wartosc"
)

ggplot(penguins_long, aes(x = species, y = wartosc, fill = species)) +
  geom_boxplot() +
  facet_wrap(~ cecha, scales = "free_y")