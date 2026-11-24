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