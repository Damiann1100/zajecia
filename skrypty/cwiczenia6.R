#cw29
#Do wykresu słupkowego utworzonego na podstawie danych birthwt, zliczającego liczbę urodzonych dzieci z małą i dużą masą urodzeniową dla różnych ras:
#dodaj odpowiednie etykiety osi;
#zatytułuj wykres;
#podaj jako źródło pakiet MASS;
#dodaj tytuł legendy „Rasy”.
library(MASS)
library(ggplot2)

ggplot(birthwt, aes(x = factor(race), fill = factor(low))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Wykres masy urodzeniowej dzieci",
    x = "Rasa",
    y = "Liczba dzieci",
    fill = "Rasy",
    caption = "pakiet MASS"
  )

#Do wykresu punktowego utworzonego na podstawie danych BodyTemperature.txt, opisującego zależność między temperaturą a pulsem dla mężczyzn i kobiet:
#dodaj odpowiednie etykiety osi;
#zatytułuj wykres;
#podaj jako źródło adres strony http://extras.springer.com/2012/978-1-4614-1302-8/BodyTemperature.txt;
#dodaj tytuł legendy „Płeć”.

dane_temp <- read.table("data/BodyTemperature.txt", header = TRUE)

ggplot(dane_temp, aes(x = Temperature, y = HeartRate, color = Gender)) +
  geom_point() +
  labs(
    title = "Zależność temperatury od pulsu",
    x = "Temperatura",
    y = "Puls",
    color = "Płeć",
    caption = "http://extras.springer.com/2012/978-1-4614-1302-8/BodyTemperature.txt"
  )

#cw30
#Na wykresie z zadania 2 z poprzedniej sekcji:
#dodaj etykiety wieku dla kobiet, które są starsze niż 45 lat;
#w prawym dolnym rogu dodaj tekst mówiący, ile wynosi średni puls dla kobiet i mężczyzn.
#Na podstawie danych birthwt utwórz histogram częstości zmiennej lwt. Dodaj dwie pionowe linie: ciągłą, która oznacza wartość średnią, oraz przerywaną, która oznacza medianę.

library(dplyr)

starsze_kobiety <- dane_temp %>% filter(Gender == "F" & Age > 45)

sredni_puls <- dane_temp %>% 
  group_by(Gender) %>% 
  summarize(sr = mean(HeartRate))

tekst_puls <- paste("Średni puls — K:", round(sredni_puls$sr[sredni_puls$Gender == "F"], 1), 
                    "| M:", round(sredni_puls$sr[sredni_puls$Gender == "M"], 1))

etykieta_rozek <- data.frame(
  Temperature = max(dane_temp$Temperature),
  HeartRate = min(dane_temp$HeartRate),
  label = tekst_puls
)

ggplot(dane_temp, aes(x = Temperature, y = HeartRate, color = Gender)) +
  geom_point() +
  geom_text(aes(label = Age), data = starsze_kobiety, vjust = -0.5, show.legend = FALSE) +
  geom_text(aes(label = label), data = etykieta_rozek, color = "black", vjust = "bottom", hjust = "right", show.legend = FALSE) +
  labs(
    title = "Zależność temperatury od pulsu",
    x = "Temperatura",
    y = "Puls",
    color = "Płeć",
    caption = "http://extras.springer.com/2012/978-1-4614-1302-8/BodyTemperature.txt"
  )

#Na podstawie danych birthwt utwórz histogram częstości zmiennej lwt. Dodaj dwie pionowe linie: ciągłą, która oznacza wartość średnią, oraz przerywaną, która oznacza medianę.

ggplot(birthwt, aes(x = lwt)) +
  geom_histogram(binwidth = 10, color = "black", fill = "white") +
  geom_vline(aes(xintercept = mean(lwt)), color = "red", linetype = "solid", linewidth = 1) +
  geom_vline(aes(xintercept = median(lwt)), color = "blue", linetype = "dashed", linewidth = 1) +
  labs(
    x = "lwt",
    y = "Częstość"
  )

#cw31
#Na podstawie danych BodyTemperature.txt utwórz wykres punktowy przedstawiający puls w zależności od wieku dla kobiet i mężczyzn.
#Dodaj znaczniki danych na osi x od 20 do 50 co 4;
#w legendzie zmień oznaczenia na polskie nazwy;
#umieść legendę na dole.

dane_temp <- read.table("data/BodyTemperature.txt", header = TRUE)

ggplot(dane_temp, aes(x = Age, y = HeartRate, color = Gender)) +
  geom_point() +
  scale_x_continuous(breaks = seq(20, 50, by = 4)) +
  scale_color_discrete(
    name = "Płeć",
    labels = c("Kobieta", "Mężczyzna")
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = "Puls w zależności od wieku",
    x = "Wiek",
    y = "Puls"
  )

#Na podstawie danych birthwt utwórz wykres liniowy przedstawiający średnią wagę dzieci dla ras z uwzględnieniem informacji o nadciśnieniu.
#Zmień kolory dla nadciśnienia na czerwony, jeśli jest nadciśnienie, lub zielony, jeśli go nie ma;
#w legendzie zmień oznaczenia na polskie nazwy;
#umieść legendę na dole.

b_sum <- birthwt %>%
  group_by(race, ht) %>%
  summarize(srednia_waga = mean(bwt), .groups = "drop")

b_sum$race_f <- factor(b_sum$race)
b_sum$ht_f <- factor(b_sum$ht)

ggplot(b_sum, aes(x = race_f, y = srednia_waga, color = ht_f, group = ht_f)) +
  geom_line() +
  geom_point(size = 3) +
  scale_color_manual(
    name = "Nadciśnienie",
    values = c("0" = "green", "1" = "red"),
    labels = c("Brak", "Jest")
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = "Średnia waga dzieci a rasa i nadciśnienie",
    x = "Rasa",
    y = "Średnia waga (bwt)"
  )

#Na podstawie danych birthwt utwórz histogram gęstości dla zlogarytmowanych wartości zmiennej lwt.
ggplot(birthwt, aes(x = lwt)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15, color = "black", fill = "white") +
  scale_x_log10() +
  labs(
    title = "Histogram gęstości zlogarytmowanej zmiennej lwt",
    x = "lwt (skala logarytmiczna)",
    y = "Gęstość"
  )

#cw32 
#Na podstawie danych gapminder z roku 2007 przygotuj wykres zależności oczekiwanej długości życia (lifeExp) od PKB na mieszkańca (gdpPercap), dopracowując go krok po kroku...

library(palmerpenguins)

pingwiny <- penguins %>% filter(!is.na(sex))

ggplot(pingwiny, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot() +
  scale_fill_manual(
    name = "Płeć",
    values = c("female" = "#E69F00", "male" = "#56B4E9"),
    labels = c("Samica", "Samiec")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Masa ciała pingwinów w zależności od gatunku i płci",
    subtitle = "Samce osiągają wyraźnie większą masę niż samice we wszystkich analizovaných gatunkach",
    x = "Gatunek pingwina",
    y = "Masa ciała (g)",
    caption = "Źródło: pakiet palmerpenguins"
  )

ggsave("pingwiny_wykres.png", width = 8, height = 5, dpi = 300)