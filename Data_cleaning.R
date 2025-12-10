#Biblioteki 1
if(!require("dplyr")) install.packages("dplyr")
library(dplyr)
if(!require("naniar")) install.packages("naniar")
library(naniar)
if(!require("VIM")) install.packages("VIM")
library(VIM)
if(!require("tidyr")) install.packages("tidyr")
library(tidyr)
if(!require("randomForest")) install.packages("randomForest")
library(randomForest)
if (!require("validate")) {install.packages("validate")}
library(validate)
if (!require("leaflet")) {install.packages("leaflet")}
library(leaflet)
if (!require("scales")) {install.packages("scales")}
library(scales)
if (!require("ggplot2")) {install.packages("ggplot2")}
library(ggplot2)
if (!require("hexbin")) {install.packages("hexbin")}
library(hexbin)
if (!require("gganimate")) {install.packages("gganimate")}
library(gganimate)
if (!require("gifski")) {install.packages("gifski")}
library(gifski) 
if (!requireNamespace("ggmap", quietly = TRUE)) {
  install.packages("ggmap")
}

#Czyszczenie danych
#załaduj mi obiekt apartments_pl_2024_06.csv do R
apartments_data_2024_06 <- read.csv("https://raw.githubusercontent.com/Michu24600/GRUPA-Aa/refs/heads/main/apartments_pl_2024_06.csv?token=GHSAT0AAAAAADQZDZEVXOPVHWJX7EZUODPE2JZ34OQ")
View(apartments_data_2024_06)

#Sprawdzanie warunków

# 1. Definicja reguł walidacyjnych
rules_apartments <- validator(
  # Floor count >= Floor (Liczba pięter w budynku musi być >= piętro mieszkania)
  Logic_Floor = floorCount >= floor,
  
  # Square meters <= 20 m2 - Room <=3 (Dla mieszkań <= 20m2 liczba pokoi musi być <= 3)
  Logic_Small_Apt = if (squareMeters <= 20) rooms <= 3,
  
  # Floor count <=1 - elevator Yes (Zgodnie z poleceniem: jeśli 1 piętro lub mniej, nie może być windy)
  Logic_Elevator = if (floorCount <= 1) hasElevator == "no",
  
  # Cena <= 100000 >= 10000000 (Cena w przedziale 100 tys. - 10 mln)
  Range_Price = price >= 100000 & price <= 10000000,
  
  # Współrzędne czy faktycznie Polska (Bounding box)
  Geo_Poland_Lat = latitude >= 49.0 & latitude <= 54.9,
  Geo_Poland_Lon = longitude >= 14.1 & longitude <= 24.2,
  
  # Kolumna "squareMeters" < 300
  Limit_Area_Max = squareMeters < 300,
  
  # Kolumna "buildYear" < 2025
  Limit_Year_Max = buildYear < 2025,
  
  # Występują w pliku tylko wartości dodatnie (wybrane kolumny numeryczne)
  
  # Podstawowe dane numeryczne (ID, Cena, Pokoje, Metraż)
  Positive_Basic = id > 0 & price > 0 & rooms > 0 & squareMeters > 0,
  
  # Dane budynku (Rok, Piętra)
  # Uwaga: floor > 0 oznacza, że parter (0) zostanie uznany za błąd.
  Positive_Building = buildYear > 0 & floorCount > 0 & floor >= 0, 
  
  # Dane geograficzne i POI
  Positive_Geo_POI = latitude > 0 & longitude > 0 & poiCount >= 0,
  
  # Dystanse (wszystkie kolumny *Distance muszą być > 0)
  Positive_Distances = centreDistance > 0 & 
    schoolDistance > 0 & 
    clinicDistance > 0 & 
    postOfficeDistance > 0 & 
    kindergartenDistance > 0 & 
    restaurantDistance > 0 & 
    collegeDistance > 0 & 
    pharmacyDistance > 0
)

### WERSJA BARPLOT 1: PRZED CZYSZCZENIEM ###

# 2. Konfrontacja reguł z danymi (apartments_imputed_city_knn)
cf_apartments <- confront(apartments_data_2024_06, rules_apartments)

# 3. Podsumowanie wyników
print("Podsumowanie walidacji:")
summary(cf_apartments)

# 4. Wizualizacja
barplot(cf_apartments, main = "Naruszenia reguł (nowe zmienne)")



# Zakładam, że apartments_data_2024_06 jest już wczytane.

# Definiujemy kolumny dystansowe raz, żeby nie śmiecić w kodzie
distance_cols <- c(
  "collegeDistance", "clinicDistance", "restaurantDistance", 
  "pharmacyDistance", "postOfficeDistance", "kindergartenDistance", 
  "schoolDistance"
)
distance_vars_for_knn <- c("poiCount", "centreDistance")

# -----------------------------------------------------------------------------
# CZĘŚĆ 1: Główne czyszczenie
# -----------------------------------------------------------------------------

apartments_processed <- apartments_data_2024_06 %>%
  #Usuwamy diw kolumny
  select(-buildingMaterial, -condition) %>%
  
  #uzupełnianie braków kolumna piętro
  mutate(
    floor = if_else(is.na(floor), pmin(3, floorCount), floor)
  ) %>%
  
  #Windy (hasElevator)
  mutate(
    hasElevator = case_when(
      (is.na(hasElevator) | hasElevator == "") & floor > 4 ~ "yes",
      (is.na(hasElevator) | hasElevator == "") & floor <= 4 ~ "no",
      TRUE ~ hasElevator
    )
  ) %>%
  
  #Filtracja pustych floorCount 
  filter(!is.na(floorCount)) %>%
  
  #Imputacja dystansów średnią w grupach miast
  group_by(city) %>%
  mutate(
    across(
      .cols = all_of(distance_cols),
      .fns = ~ replace(., is.na(.), mean(., na.rm = TRUE))
    )
  ) %>%
  
  #Imputacja kNN dla buildYear
  group_modify(~ {
    imputed <- kNN(
      .x, 
      variable = "buildYear",
      dist_var = distance_vars_for_knn, 
      k = 5
    )
    imputed
  }) %>%
  ungroup() %>%
  
  # Sprzątanie po kNN
  mutate(
    buildYear = round(buildYear) 
  ) %>%
  select(-ends_with("_imp")) %>%
  
  # puste stringi na NA i faktoryzacja
  mutate(
    type = na_if(trimws(type), ""),
    type = as.factor(type)
  )

# -----------------------------------------------------------------------------
# Imputacja Random Forest (Type)
# -----------------------------------------------------------------------------

is_missing_type <- is.na(apartments_processed$type)

# Trenowanie modelu
train_data <- apartments_processed[!is_missing_type, ]
train_data$type <- droplevels(train_data$type)

model_rf <- randomForest(
  type ~ ., 
  data = train_data, 
  ntree = 500,
  na.action = na.omit 
)

# Predykcja tylko dla brakujących
predictions <- predict(model_rf, newdata = apartments_processed[is_missing_type, ])

# Wstawiamy predykcje z powrotem do głównego zbioru
apartments_processed$type[is_missing_type] <- predictions

# -----------------------------------------------------------------------------
# Czyszczenie ostatni krok
# -----------------------------------------------------------------------------

apartments_final <- apartments_processed %>%
  mutate(
    hasElevator = if_else(floorCount <= 1 & hasElevator == "yes", NA_character_, hasElevator)
  ) %>%
  filter(!is.na(hasElevator))

# Sprawdzamy wyniki - BEZ VIEW() W KODZIE PRODUKCYJNYM!
cat("Liczba wierszy po czyszczeniu:", nrow(apartments_final), "\n")
print(table(apartments_final$hasElevator))
#Wykres testowy

#Wizualizacja Missing values po wyczyszczeniu danych
gg_miss_var(apartments_final)
gg_miss_case(apartments_final)
gg_miss_fct(apartments_final, fct = floor)
gg_miss_upset(apartments_final, nsets = 7)



### WERSJA BARPLOT 2: PO CZYSZCZENIU ###

# 2. Konfrontacja reguł z danymi (apartments_imputed_city_knn)
cf_apartments_after_cleaning <- confront(apartments_final, rules_apartments)

# 3. Podsumowanie wyników
print("Podsumowanie walidacji:")
summary(cf_apartments_after_cleaning)

# 4. Wizualizacja
barplot(cf_apartments_after_cleaning, main = "Naruszenia reguł (nowe zmienne)")

#Przygotowanie palety kolorów.
paleta <- colorNumeric(
  palette = "magma", 
  domain = apartments_final$price,
  reverse = TRUE )

#---------------------------------------------------------------------------------------------------------
#mapa z każdym mieszkamniem jako kółkiem kolorowanym wg ceny
#---------------------------------------------------------------------------------------------------------
mapa <- apartments_final %>%
  sample_n(min(21240, nrow(apartments_final))) %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 6,
    color = ~paleta(price), 
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~paste0("Cena: ", price, " PLN"),
    label = ~as.character(price)
  ) %>%
  
  addLegend(
    "bottomright", 
    pal = paleta,      # I tu znowu używamy palety
    values = ~price,
    title = "Cena",
    opacity = 1
  )
mapa

# ---------------------------------------------------------------------------------------------------------
#Wykres z bombelkami dla średniej ceny w miastach przeskalowany
#---------------------------------------------------------------------------------------------------------
city_stats <- apartments_final %>%
  group_by(city) %>% 
  summarise(
    avg_price = mean(price, na.rm = TRUE),      # Średnia cena
    avg_lat   = mean(latitude, na.rm = TRUE),   # Środek miasta (szerokość)
    avg_lng   = mean(longitude, na.rm = TRUE),  # Środek miasta (długość)
    offer_count = n()                           
  )

city_stats_scaled <- apartments_final %>%
  group_by(city) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    avg_lat   = mean(latitude, na.rm = TRUE),
    avg_lng   = mean(longitude, na.rm = TRUE),
    offer_count = n()
  ) %>%
  mutate(
    scaled_radius = scales::rescale(sqrt(offer_count), to = c(5, 35))
  )

paleta_miasta <- colorNumeric(palette = "viridis", domain = city_stats_scaled$avg_price, reverse = TRUE)

mapa_pro <- city_stats_scaled %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~avg_lng,
    lat = ~avg_lat,
    radius = ~scaled_radius, 

    color = ~paleta_miasta(avg_price),
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~paste0("<b>Miasto:</b> ", city, "<br><b>Oferty:</b> ", offer_count),
    label = ~city
  ) %>%
  addLegend("bottomright", pal = paleta_miasta, values = ~avg_price, title = "Średnia cena")

mapa_pro

#-----------------------------------------------------------------------------------------
#Wykres słupkowy średniej ceny za m² w miastach
#-----------------------------------------------------------------------------------------
apartments_final %>%
  # 1. Obliczamy cenę za metr (jeśli nie ma) i grupujemy
  mutate(price_per_sqm = price / squareMeters) %>%
  group_by(city) %>%
  summarise(mean_price_sqm = mean(price_per_sqm, na.rm = TRUE)) %>%
  
  # 2. Rysujemy
  # reorder() sortuje miasta od najtańszego do najdroższego - kluczowe dla czytelności
  ggplot(aes(x = reorder(city, mean_price_sqm), y = mean_price_sqm)) +
  
  # geom_col() to funkcja do słupków, gdy mamy wyliczone wartości
  geom_col(fill = "steelblue") + 
  
  # Obracamy wykres, żeby nazwy miast się czytało normalnie
  coord_flip() +
  
  # Opisy i formatowanie
  labs(
    title = "Średnia cena za m² w miastach",
    x = "Miasto",
    y = "Cena za m² (PLN)"
  ) +
  theme_minimal()

#-----------------------------------------------------------------------------------------
#Wykres zależności ceny za m² od odległości do centrum w podziale na miasta
#-----------------------------------------------------------------------------------------
wykres_odleglosci <- apartments_final %>%
  mutate(price_per_sqm = price / squareMeters) %>%

  filter(price_per_sqm < 60000) %>%
  ggplot(aes(x = centreDistance, y = price_per_sqm)) +
  geom_point(alpha = 0.2, color = "darkblue", size = 1) +
  geom_smooth(method = "gam", color = "red", se = FALSE) + 
  
  facet_wrap(~city, scales = "free") +
  
  labs(
    title = "Czy im dalej od centrum, tym taniej?",
    subtitle = "Zależność ceny za m² od dystansu do centrum w podziale na miasta",
    x = "Odległość od centrum (km)",
    y = "Cena za m² (PLN)"
  ) +
  theme_bw()
wykres_odleglosci

#-----------------------------------------------------------------------------------------
#Wykres hexbin pokazujący zależność ceny za m² od liczby punktów POI
#-----------------------------------------------------------------------------------------

wykres_fajerwerki <- apartments_final %>%
  mutate(price_per_sqm = price / squareMeters) %>%
  filter(price_per_sqm < 70000) %>% 
  
  ggplot(aes(x = poiCount, y = price_per_sqm)) +
  
  geom_hex(bins = 70) +
  
  scale_fill_viridis_c(option = "plasma", name = "Liczba\nofert") +
  
  
  geom_smooth(method = "gam", color = "cyan", fill = "white", alpha = 0.2, size = 1.5) +
  

  labs(
    title = "Wpływ punktów usługowych (POI) na cenę metra kwadratowego",
    subtitle = "Analiza zagęszczenia ofert dla całego rynku (Hexbin Plot)",
    x = "Liczba punktów POI w zasięgu (poiCount)",
    y = "Cena za m² (PLN)"
  ) +
  
  theme_dark() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "white"),
    plot.subtitle = element_text(color = "#cccccc"),
    axis.text = element_text(color = "#eeeeee"),
    axis.title = element_text(color = "white"),
    legend.background = element_rect(fill = "#333333"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    plot.background = element_rect(fill = "#222222"), # Ciemne tło całego obrazka
    panel.grid.major = element_line(color = "#444444"), # Delikatne linie siatki
    panel.grid.minor = element_blank()
  )

wykres_fajerwerki

#-----------------------------------------------------------------------------------------
# --- KROK 1: Sprawdzenie silników (Instalacja pakietów) ---
required_packages <- c("ggplot2", "dplyr", "gganimate", "gifski", "transformr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Instaluję:", pkg))
    install.packages(pkg)
  }
}
# --- KROK 2: Przygotowanie paliwa (Dane) ---
# Odsiewamy błędy (lata z kosmosu typu rok 0 albo 2050)
anim_data <- apartments_final %>%
  mutate(price_per_sqm = price / squareMeters) %>%
  filter(
    buildYear >= 1970,          # Skupmy się na ostatnich 50 latach
    buildYear <= 2024,
    price_per_sqm < 60000,      # Wywalamy anomalie cenowe
    centreDistance <= 15        # Wywalamy totalne zadupia > 15km
  ) %>%
  # Ważne: buildYear musi być liczbą całkowitą (integer)
  mutate(buildYear = as.integer(buildYear))

# --- KROK 3: Start rakiety (Definicja wykresu) ---
animacja <- ggplot(anim_data, aes(x = centreDistance, y = price_per_sqm, size = squareMeters, color = city)) +
  
  # Rysujemy punkty
  geom_point(alpha = 0.7) +
  
  # Skale i kolory (viridis znowu, bo jest czytelny)
  scale_color_viridis_d(option = "turbo") + # "turbo" jest jaskrawe, idealne do czarnego tła
  scale_size(range = c(2, 8), guide = "none") + # Wielkość kropki to metraż
  
  # Ciemny motyw NASA
  theme_dark() +
  labs(
    title = "Ewolucja Rynku: Rok {frame_time}", # To się będzie zmieniać dynamicznie!
    subtitle = "Oś X: Dystans od centrum | Oś Y: Cena za m² | Wielkość: Metraż",
    x = "Dystans od centrum (km)",
    y = "Cena za m² (PLN)",
    color = "Miasto"
  ) +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    legend.background = element_rect(fill = "black"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "gray"),
    panel.grid.major = element_line(color = "#333333"),
    panel.grid.minor = element_blank()
  ) +
  
  # --- TU JEST MAGIA (gganimate) ---
  transition_time(buildYear) +   # Animuj po roku budowy
  shadow_wake(wake_length = 0.1, alpha = FALSE) # Zostaw "smugę" za punktami (efekt komety)

# --- KROK 4: Renderowanie (To potrwa chwilę!) ---
message("Renderuję animację... Idź po kawę, to zajmie około minuty.")

animate(
  animacja, 
  nframes = 200,    # Liczba klatek (im więcej tym płynniej, ale wolniej)
  fps = 20,         # Klatki na sekundę
  width = 800, 
  height = 600,
  renderer = gifski_renderer()
)


