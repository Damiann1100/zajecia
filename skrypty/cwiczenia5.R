#cw26
#Korzystając ze zbioru danych BodyTemperature.txt (do pobrania poniżej), utwórz wykres punktowy zależności między temperaturą ciała a tętnem. Dodaj linię regresji loess.
library(ggplot2)
body_temp <- read.table("data/BodyTemperature.txt", header = TRUE)

ggplot(body_temp, aes(x = Temperature, y = HeartRate)) +
  geom_point() +
  geom_smooth(method = "loess")

#Korzystając ze zbioru danych birthwt z pakietu MASS, utwórz wykres punktowy zależności między masą urodzeniową dziecka a masą matki przed ciążą. Dla każdej grupy dzieci (niska i prawidłowa masa urodzeniowa) dodaj linie regresji lm. Co możesz powiedzieć o tych zależnościach?
library(MASS)
birthwt$low_f <- factor(birthwt$low, labels = c("Prawidłowa", "Niska"))

ggplot(birthwt, aes(x = lwt, y = bwt, color = factor(low))) +
  geom_point() +
  geom_smooth(method = "lm")

#cw27
#Na podstawie danych birthwt
#utwórz wykres słupkowy liczebności poszczególnych kategorii dla zmiennej race. Każda kategoria niech będzie wypełniona innym kolorem, a zarys słupków niech będzie niebieski;
library(dplyr)
ggplot(birthwt, aes(x = factor(race), fill = factor(race))) + 
  geom_bar(color = "blue")
#utwórz wykres słupkowy liczebności poszczególnych kategorii dla zmiennej race w zależności od informacji na temat palenia;
ggplot(birthwt, aes(x = factor(race), fill = factor(smoke))) + 
  geom_bar()
#utwórz wykres słupkowy mówiący o procentowym udziale poszczególnych kategorii dla zmiennej race w zależności od informacji na temat palenia;
ggplot(birthwt, aes(x = factor(race), fill = factor(smoke))) + 
  geom_bar(position = "fill")
#utwórz wykres słupkowy, który pozwoli odpowiedzieć na pytanie: dla której rasy jest najwięcej kobiet niepalących w trakcie ciąży?
ggplot(subset(birthwt, smoke == 0), aes(x = factor(race))) + 
  geom_bar()
#zagreguj dane tak, aby móc wykonać wykres liniowy, który będzie podawał liczbę matek niepalących i palących w zależności od rasy. Dodaj punkty o różnych kształtach w zależności od informacji o paleniu.
dane <- as.data.frame(table(birthwt$race, birthwt$smoke))
colnames(dane) <- c("race", "smoke", "l_matek")

ggplot(dane, aes(x = race, y = l_matek, group = smoke, shape = smoke)) +
  geom_line() + 
  geom_point()

#Na podstawie danych BodyTemperature.txt
bt <- read.table("data/BodyTemperature.txt", header = TRUE)
#wyznacz przedziały wiekowe 20 − 30 , 30 − 40 i 40 − 50 . Dla każdego pacjenta podaj informację, do jakiej grupy wiekowej należy;
#wyznacz średni puls w każdej grupie wiekowej oraz dla obu płci;
dane2 <- bt %>%
  mutate(wiek_kat = cut(Age, breaks = c(20, 30, 40, 50), include.lowest = TRUE)) %>%
  group_by(wiek_kat, Gender) %>%
  summarise(sr_puls = mean(HeartRate), .groups = "drop")

dane2
#utwórz wykres liniowy zależności między płcią a średnim pulsem z uwzględnieniem grup wiekowych;
#dodaj dla każdej grupy wiekowej inny kolor;
#niech linie będą przerywane, a ich grubość niech wynosi 0.7;
#dołącz punkty danych o wielkości 2 i różnych kształtach zależnych od grupy.
ggplot(dane2, aes(x = Gender, y = sr_puls, group = wiek_kat, color = wiek_kat, shape = wiek_kat)) +
  geom_line(linetype = "dashed", linewidth = 0.7) +
  geom_point(size = 2)

  #cw28
  #Na podstawie danych birthwt utwórz histogram częstości zmiennej lwt. Co możesz powiedzieć o tym wykresie?
  ggplot(birthwt, aes(x = lwt)) +
    geom_histogram()
  #Na podstawie danych z pliku Protein.txt (do pobrania poniżej) wykonaj histogram gęstości razem z wykresem gęstości zmiennej WhiteMeat (wskazówka: wysokość słupków wyrażamy jako gęstość zapisem aes(y = after_stat(density)) — tak jak na zajęciach 4). Co możesz powiedzieć o tym wykresie?
  protein <- read.table("data/Protein.txt", header = TRUE)
  
  ggplot(protein, aes(x = WhiteMeat)) +
    geom_histogram(aes(y = after_stat(density))) +
    geom_density()
  #Na podstawie danych BodyTemperature.txt utwórz wykres pudełkowy temperatury ciała oddzielnie dla mężczyzn i kobiet. Która grupa ma wyższe wartości? Która grupa ma większy rozrzut?
  bt <- read.table("data/BodyTemperature.txt", header = TRUE)
  
  ggplot(bt, aes(x = Gender, y = Temperature)) +
    geom_boxplot()
  #Na podstawie danych penguins utwórz wykres gęstości długości płetwy (flipper_length_mm) oddzielnie dla każdego gatunku (species), mapując gatunek na wypełnienie. Ustaw przezroczystość tak, aby wszystkie krzywe były widoczne. Czy rozkłady gatunków się pokrywają?
  library(palmerpenguins)
  
  ggplot(penguins, aes(x = flipper_length_mm, fill = species)) +
    geom_density(alpha = 0.5)
  #Porównaj różne sposoby przedstawienia rozkładu długości dzioba (bill_length_mm) w zależności od gatunku pingwina:
  #wykonaj histogram z podziałem na panele według gatunku (facet_wrap()), z panelami ułożonymi jeden pod drugim;
  ggplot(penguins, aes(x = bill_length_mm)) +
    geom_histogram() +
    facet_wrap(~ species, ncol = 1)
  #wykonaj wykres pudełkowy;
  ggplot(penguins, aes(x = species, y = bill_length_mm)) +
    geom_boxplot()
  #wykonaj wykres skrzypcowy;
  ggplot(penguins, aes(x = species, y = bill_length_mm)) +
    geom_violin()
  #na wykres skrzypcowy nałóż wąski wykres pudełkowy (width = 0.1) oraz odwróć osie za pomocą coord_flip().
  ggplot(penguins, aes(x = species, y = bill_length_mm)) +
    geom_violin() +
    geom_boxplot(width = 0.1) +
    coord_flip()
  
  #Na podstawie danych gapminder z pakietu gapminder wybierz obserwacje z roku 2007 i utwórz wykres pudełkowy oczekiwanej długości życia (lifeExp) dla poszczególnych kontynentów (continent). Pudełka ułóż poziomo i uporządkuj kontynenty według mediany długości życia (wskazówka: x = reorder(continent, lifeExp, median)). Na którym kontynencie zróżnicowanie długości życia jest największe?
  install.packages('gapminder')
  library(gapminder)
  
  gap2007 <- gapminder %>%
    filter(year == 2007)
  
  ggplot(gap2007, aes(x = reorder(continent, lifeExp, median), y = lifeExp)) +
    geom_boxplot() +
    coord_flip()
  
  #Na podstawie danych mtcars utwórz wykres punktowy zależności zużycia paliwa (mpg) od masy samochodu (wt). Kolor punktów uzależnij od liczby cylindrów (cyl, pamiętaj o zamianie na czynnik), dodaj linie regresji lm dla każdej grupy cylindrów, a całość podziel na panele według rodzaju skrzyni biegów (am). Czy zależność wygląda podobnie w obu panelach?
  ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ am)
  
  #Na podstawie danych gapminder:
  #wybierz dane dla Polski, Niemiec, Czech i Węgier i utwórz wykres liniowy z punktami pokazujący zmiany oczekiwanej długości życia w latach 1952–2007 (każdy kraj innym kolorem);
  gapminder %>%
    filter(country %in% c("Poland", "Germany", "Czech Republic", "Hungary")) %>%
    ggplot(aes(x = year, y = lifeExp, color = country)) +
    geom_line() +
    geom_point()
  #dla wszystkich krajów wylicz średnią oczekiwaną długość życia w kolejnych latach dla każdego kontynentu (group_by() i summarise()), a następnie przedstaw wynik na wykresie liniowym z punktami;
  gapminder %>%
    group_by(continent, year) %>%
    summarise(sr_lifeExp = mean(lifeExp), .groups = "drop") %>%
    ggplot(aes(x = year, y = sr_lifeExp, color = continent)) +
    geom_line() +
    geom_point()
  #utwórz wykres słupkowy liczby krajów na każdym kontynencie w roku 2007.
  gapminder %>%
    filter(year == 2007) %>%
    count(continent) %>%
    ggplot(aes(x = continent, y = n)) +
    geom_col()