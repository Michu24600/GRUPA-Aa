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
if (!require("gifski")) {install.packages("gifski")}
library(gifski) 
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes")
  library(shiny)
  library(shinythemes)
}

#Czyszczenie danych
#załaduj mi obiekt apartments_pl_2024_06.csv do R
apartments_data_2024_06 <- read.csv("https://raw.githubusercontent.com/Michu24600/GRUPA-Aa/refs/heads/main/apartments_pl_2024_06.csv?token=GHSAT0AAAAAADRBDFX5LY52EOMGLQEBVZYW2KC5HNQ")
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
# ==============================================================================
# SEKCJA WIZUALIZACJI
# ==============================================================================

# Ustawienie wspólnego, czytelnego stylu dla wszystkich wykresów
theme_set(theme_minimal())

# -----------------------------------------------------------------------------
# 1. Rozkład cen mieszkań (Histogram)
# -----------------------------------------------------------------------------
plot1 <- ggplot(apartments_final, aes(x = price)) +
  geom_histogram(bins = 50, fill = "#4E84C4", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = scales::label_number(suffix = " PLN", big.mark = " ")) +
  labs(
    title = "Rozkład cen mieszkań",
    subtitle = "Jak kształtuje się rozkład cen mieszkań w Polsce",
    x = "Cena (PLN)",
    y = "Liczba ofert"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))

print(plot1)

# -----------------------------------------------------------------------------
# 2. Rozkład cen względem liczby pokoi (Price vs Rooms - Boxplot)
# -----------------------------------------------------------------------------
# Rzutujemy 'rooms' na factor, aby traktować liczbę pokoi jako kategorię
plot2 <- ggplot(apartments_final, aes(x = as.factor(rooms), y = price, fill = as.factor(rooms))) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1, outlier.alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number(suffix = " PLN", big.mark = " ")) +
  scale_fill_brewer(palette = "Set3") + # Ładna paleta kolorów
  labs(
    title = "Cena wynajmu a liczba pokoi",
    subtitle = "Rozkład cen",
    x = "Liczba pokoi",
    y = "Cena (PLN)",
    fill = "Liczba pokoi"
  ) +
  theme(legend.position = "none") # Ukrywamy legendę, bo oś X wszystko wyjaśnia

print(plot2)

# -----------------------------------------------------------------------------
# 3. Typ budynku a rozkład cen (Violin Plot + Boxplot w środku)
# -----------------------------------------------------------------------------
# Używamy wykresu skrzypcowego (violin), żeby pokazać gęstość rozkładu, 
# i dodajemy w środku boxplot dla precyzji.
plot3 <- apartments_final %>%
  filter(!is.na(type)) %>% # Zabezpieczenie na wypadek braków w kolumnie type
  ggplot(aes(x = type, y = price, fill = type)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, color = "black", alpha = 0.9, outlier.shape = NA) +
  scale_y_continuous(labels = scales::label_number(suffix = " PLN", big.mark = " ")) +
  labs(
    title = "Typ budynku a ceny mieszkań",
    subtitle = "Porównanie rozkładu cen dla różnych typów zabudowy",
    x = "Typ budynku",
    y = "Cena (PLN)"
  ) +
  theme(legend.position = "none") +
  coord_flip() # Obracamy wykres poziomo dla lepszej czytelności etykiet

print(plot3)

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

#-----------------------------------------------------------------------------------------
# To jest do dopracowania, ale definitywnie zostaję
#-----------------------------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("💰 Kalkulator Ceny Mieszkania"),
  sidebarLayout(
    sidebarPanel(
      h3("Filtry Wyszukiwania"),
      helpText("Ustaw parametry, aby znaleźć swoje wymarzone M."),
      
      # SUWACZEK 1: Odległość od centrum
      sliderInput("range_centre", 
                  "Maksymalna odległość od centrum (km):",
                  min = 0, 
                  max = 15, 
                  value = 5, # Wartość startowa
                  step = 0.5),
      
      # SUWACZEK 2: Metraż (zakres od-do)
      sliderInput("range_sqm",
                  "Metraż (m²):",
                  min = 20,
                  max = 150,
                  value = c(40, 60)), # Zakres startowy
      
      # SUWACZEK 3: Rok budowy
      sliderInput("range_year",
                  "Rok budowy:",
                  min = 1950,
                  max = 2024,
                  value = c(2000, 2024),
                  sep = ""), # sep="" usuwa przecinek w roku (np. 2,024)
      
      hr(), # Pozioma kreska
      h4("Dodatki:"),
      checkboxInput("check_elevator", "Musi być winda (hasElevator)", FALSE)
    ),
    
    mainPanel(
      div(style = "text-align: center; padding: 20px;",
          h2("Szacowana Cena Ofertowa:"),
          uiOutput("price_box") 
      ),
      
      hr(),
      
      h4("Rozkład cen znalezionych ofert:"),
      plotOutput("distPlot"),
      
      br(),
      
      h4("Przykładowe oferty spełniające kryteria:"),
      tableOutput("resultsTable")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    
    data <- apartments_final %>%
      filter(
        centreDistance <= input$range_centre,
        squareMeters >= input$range_sqm[1] & squareMeters <= input$range_sqm[2],
        buildYear >= input$range_year[1] & buildYear <= input$range_year[2]
      )
    
    if (input$check_elevator) {
      data <- data %>% filter(hasElevator == "yes") # Sprawdź czy w danych masz "yes" czy TRUE
    }
    
    data
  })
  
  # Renderowanie Licznika Ceny (z fajerwerkami CSS)
  output$price_box <- renderUI({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      return(h3("Brak ofert spełniających kryteria!", style = "color: red;"))
    }
    
    avg_price <- mean(df$price, na.rm = TRUE)
    count_offers <- nrow(df)
    
    tagList(
      h1(paste(format(round(avg_price, 0), big.mark = " "), "PLN"), 
         style = "color: #2c3e50; font-weight: bold; font-size: 48px;"),
      p(paste("Średnia z", count_offers, "znalezionych ofert"), 
        style = "color: gray; font-size: 16px;")
    )
  })
  
  #Renderowanie wykresu
  output$distPlot <- renderPlot({
    df <- filtered_data()
    
    if (nrow(df) < 2) return(NULL) # Nie rysuj jak nie ma danych
    
    ggplot(df, aes(x = price)) +
      geom_histogram(fill = "#3498db", color = "white", bins = 30) +
      geom_vline(aes(xintercept = mean(price)), color = "red", linetype = "dashed", size = 1) +
      labs(x = "Cena (PLN)", y = "Liczba ofert", title = "Histogram cen w wybranym segmencie") +
      theme_minimal() +
      scale_x_continuous(labels = scales::comma)
  })
  
  # Renderowanie Tabeli
  output$resultsTable <- renderTable({
    filtered_data() %>%
      select(city, price, squareMeters, floor, buildYear, centreDistance) %>%
      arrange(price) %>% # Pokaż najtańsze
      head(5) %>%
      mutate(price = paste(format(price, big.mark=" "), "PLN")) # Formatowanie waluty
  })
}

shinyApp(ui = ui, server = server)

#-----------------------------------------------------------------------------------------
# Urbanistyczna ekspansja Gdańska
#-----------------------------------------------------------------------------------------

# 1. Załadowanie niezbędnych bibliotek
library(osmdata)
library(sf)
library(ggplot2)
library(gganimate)

# 2. Przygotowanie danych dla Gdańska
gdansk_growth <- apartments_final %>% 
  filter(tolower(city) == "gdansk") %>% 
  filter(!is.na(buildYear), !is.na(latitude), !is.na(longitude)) %>% 
  mutate(buildYear = as.numeric(buildYear)) # numeric pozwala na płynniejsze przejścia

# 3. Pobieranie konturów Gdańska
message("Pobieram granice Gdańska...")
gd_boundary <- opq(bbox = "Gdańsk") %>% 
  add_osm_feature(key = "admin_level", value = "8") %>% 
  osmdata_sf() %>% 
  .$osm_multipolygons

if(!is.null(gd_boundary)) { 
  gd_boundary <- gd_boundary %>% filter(name == "Gdańsk") 
}

# 4. Budowa wykresu
wykres_gdansk_plynny <- ggplot() +
  # Tło - wyraźne granice Gdańska
  {if (!is.null(gd_boundary)) geom_sf(
    data = gd_boundary, 
    fill = "#151515",   # Bardzo ciemny szary
    color = "#444444",  # Wyraźny grafitowy kontur
    size = 0.6
  )} +
  
  # Punkty inwestycji
  geom_point(
    data = gdansk_growth, 
    aes(x = longitude, y = latitude, group = id), 
    color = "#00ffff", 
    size = 1.2, 
    alpha = 0.9
  ) + 
  
  theme_void() + 
  labs( 
    title = "Płynny rozwój terytorialny Gdańska", 
    subtitle = "Rok: {round(frame_along, 0)}", # frame_along dla płynnego licznika lat
    caption = "Źródło: apartments_pl_2024_06" 
  ) + 
  theme( 
    plot.background = element_rect(fill = "black", color = "black"), 
    panel.background = element_rect(fill = "black", color = "black"), 
    plot.title = element_text(color = "white", size = 20, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(color = "#00ffff", size = 18, hjust = 0.5), 
    plot.margin = margin(20, 20, 20, 20) 
  ) + 
  
  # KLUCZ DO PŁYNNOŚCI: transition_reveal zamiast transition_time
  transition_reveal(buildYear) +
  
  # Dzięki transition_reveal punkty domyślnie zostają, ale shadow_mark zapewnia 
  # stabilność przy renderowaniu do GIFa
  shadow_mark(past = TRUE, future = FALSE) +
  
  ease_aes('linear') 

# 5. Renderowanie - duża liczba klatek zapewnia "filmową" płynność
message("Renderuję bardzo płynną animację (400 klatek)...") 
animate( 
  wykres_gdansk_plynny, 
  nframes = 400,    # Znacznie więcej klatek = płynniejszy ruch lat
  fps = 20,         # Więcej klatek na sekundę
  width = 800, height = 600, 
  renderer = gifski_renderer(loop = TRUE), 
  end_pause = 60 
)

# -----------------------------------------------------------------------------
# ANALIZA: "STARE VS NOWE" - Cena za m2 w zależności od dekady budowy
# -----------------------------------------------------------------------------

# 1. Przygotowanie danych (grupowanie w dekady)
price_by_decade <- apartments_final %>%
  filter(!is.na(buildYear), buildYear >= 1900, buildYear <= 2024) %>%
  mutate(
    decade = (buildYear %/% 10) * 10,  # Tworzymy dekady (np. 1974 -> 1970)
    price_per_sqm = price / squareMeters
  ) %>%
  group_by(decade) %>%
  summarise(
    avg_price_sqm = mean(price_per_sqm, na.rm = TRUE),
    n_offers = n()
  ) %>%
  filter(n_offers > 5) # Usuwamy dekady z bardzo małą liczbą ofert dla lepszej statystyki

# 2. Generowanie wykresu liniowo-punktowego
plot_decade_trend <- ggplot(price_by_decade, aes(x = decade, y = avg_price_sqm)) +
  # Linia trendu
  geom_line(color = "#2c3e50", size = 1.2, alpha = 0.7) +
  # Punkty - wielkość kropki zależy od liczby ofert
  geom_point(aes(size = n_offers), color = "#e74c3c", alpha = 0.8) +
  # Etykiety z wartością ceny nad punktami
  geom_text(aes(label = paste0(round(avg_price_sqm, 0))), 
            vjust = -1.5, size = 3, color = "#34495e") +
  
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
  scale_y_continuous(labels = scales::label_number(suffix = " PLN"),
                     expand = expansion(mult = c(0.1, 0.2))) + # Więcej miejsca na górze na etykiety
  
  labs(
    title = "Ewolucja wartości: Średnia cena za m² względem dekady budowy",
    subtitle = "Wielkość punktu oznacza liczbę dostępnych ofert z danej dekady",
    x = "Dekada budowy",
    y = "Średnia cena za m² (PLN)",
    size = "Liczba ofert"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# Wyświetlenie wykresu
print(plot_decade_trend)

# -----------------------------------------------------------------------------
# ANALIZA: "STARE VS NOWE" - Tylko dla Warszawy 
# -----------------------------------------------------------------------------

# 1. Przygotowanie danych dla Warszawy
warszawa_decade_trend <- apartments_final %>%
  # Dopasowanie do zapisu "warszawa" w Twoim pliku
  filter(tolower(city) == "warszawa") %>%
  filter(!is.na(buildYear), buildYear >= 1900, buildYear <= 2024) %>%
  mutate(
    decade = (buildYear %/% 10) * 10,
    price_per_sqm = price / squareMeters
  ) %>%
  group_by(decade) %>%
  summarise(
    avg_price_sqm = mean(price_per_sqm, na.rm = TRUE),
    n_offers = n()
  ) %>%
  filter(n_offers > 3) # Filtr, aby uniknąć przekłamań przy pojedynczych ofertach

# 2. Generowanie wykresu
plot_warszawa_trend <- ggplot(warszawa_decade_trend, aes(x = decade, y = avg_price_sqm)) +
  # Obszar pod linią (daje fajny efekt wizualny)
  geom_area(fill = "#4E84C4", alpha = 0.2) +
  # Główna linia
  geom_line(color = "#1a5276", size = 1.2) +
  # Punkty
  geom_point(aes(size = n_offers), color = "#1a5276") +
  # Etykiety z cenami
  geom_text(aes(label = paste0(round(avg_price_sqm, 0))), 
            vjust = -1.5, size = 3.5, fontface = "bold") +
  
  scale_x_continuous(breaks = seq(1900, 2020, by = 10)) +
  scale_y_continuous(labels = scales::label_number(suffix = " PLN"),
                     expand = expansion(mult = c(0.1, 0.3))) +
  
  labs(
    title = "Warszawski rynek na osi czasu",
    subtitle = "Średnia cena za m² w zależności od dekady budowy (tylko Warszawa)",
    x = "Dekada budowy",
    y = "Cena za m² (PLN)",
    size = "Liczba ofert w bazie"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

# Wyświetlenie wykresu
print(plot_warszawa_trend)

# -----------------------------------------------------------------------------
# ANALIZA: "PODATEK OD BRAKU WINDY" - Wszystkie badane miasta
# -----------------------------------------------------------------------------

# 1. Przygotowanie danych
# Filtrujemy piętra do 5 (najbardziej miarodajne porównanie)
elevator_global <- apartments_final %>%
  filter(!is.na(floor), !is.na(hasElevator)) %>%
  filter(floor <= 5) %>% 
  mutate(
    hasElevator = ifelse(hasElevator == "yes", "Z windą", "Bez windy"),
    price_per_sqm = price / squareMeters
  ) %>%
  group_by(floor, hasElevator) %>%
  summarise(
    avg_price_sqm = mean(price_per_sqm, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )

# 2. Generowanie wykresu
plot_elevator_global <- ggplot(elevator_global, aes(x = as.factor(floor), y = avg_price_sqm, fill = hasElevator)) +
  # Używamy słupków obok siebie (dodge)
  geom_col(position = position_dodge(0.8), width = 0.7, alpha = 0.85) +
  
  # Dodanie etykiet tekstowych z ceną na szczycie słupków
  geom_text(aes(label = paste0(round(avg_price_sqm, 0))), 
            position = position_dodge(0.8), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  
  # Kolory: Pomarańczowy dla braku windy (ostrzegawczy), Zielony dla windy (komfort)
  scale_fill_manual(values = c("Bez windy" = "#D35400", "Z windą" = "#27AE60")) +
  
  scale_y_continuous(labels = scales::label_number(suffix = " PLN"),
                     expand = expansion(mult = c(0, 0.2))) +
  
  labs(
    title = "Ekonomiczny wpływ braku windy",
    subtitle = "Średnia cena za m² na poszczególnych piętrach (wszystkie miasta)",
    x = "Piętro",
    y = "Średnia cena za m² (PLN)",
    fill = "Dostęp do windy"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )

# Wyświetlenie wykresu
print(plot_elevator_global)

# -----------------------------------------------------------------------------
# ANALIZA: STRUKTURA PROCENTOWA RYNKU (Treemap - udziały wewnątrz miast)
# -----------------------------------------------------------------------------

library(treemapify)
library(dplyr)
library(ggplot2)
library(scales)

# 1. Przygotowanie danych z obliczeniem procentów PER MIASTO
market_pct_per_city <- apartments_final %>%
  filter(!is.na(type), !is.na(city)) %>%
  group_by(city, type) %>%
  summarise(n_offers = n(), .groups = 'drop') %>%
  # KLUCZOWA ZMIANA: grupujemy po mieście, aby procenty sumowały się do 100% dla każdego miasta
  group_by(city) %>%
  mutate(pct_in_city = n_offers / sum(n_offers)) %>%
  ungroup()

# 2. Tworzenie wykresu
plot_structure_final <- ggplot(market_pct_per_city, aes(
  area = n_offers,           # Wielkość prostokąta zależy od liczby (Warszawa nadal większa)
  fill = type,               # Kolor zależy od typu budynku
  # Etykieta: Typ + Procent udziału w danym mieście
  label = paste0(type, "\n", label_percent(accuracy = 1)(pct_in_city)), 
  subgroup = city            # Grupowanie po miastach
)) +
  geom_treemap() +
  # Wyraźne granice miast
  geom_treemap_subgroup_border(colour = "white", size = 4) +
  # Nazwy miast
  geom_treemap_subgroup_text(place = "centre", grow = TRUE, 
                             alpha = 0.2, colour = "black", 
                             fontface = "bold") +
  # Podpisy typów wewnątrz
  geom_treemap_text(colour = "white", place = "topleft", 
                    reflow = TRUE, size = 11) +
  
  scale_fill_brewer(palette = "Set2") +
  
  labs(
    title = "Charakterystyka rynków lokalnych",
    subtitle = "Procenty pokazują udział danego typu zabudowy wewnątrz każdego miasta",
    fill = "Typ zabudowy",
    caption = "Suma procentów wewnątrz każdego miasta = 100% | Źródło: apartments_pl_2024_06"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

# Wyświetlenie wykresu
print(plot_structure_final)