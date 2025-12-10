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
