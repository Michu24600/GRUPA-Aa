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

#Czyszczenie danych
#załaduj mi obiekt apartments_pl_2024_06.csv do R
apartments_data_2024_06 <- read.csv("https://raw.githubusercontent.com/Michu24600/GRUPA-Aa/refs/heads/main/apartments_pl_2024_06.csv?token=GHSAT0AAAAAADRBDFX55IL6KLM6X63OKU7I2JZ3PUA")
View(apartments_data_2024_06)

#Usuwanie kolumn "buildingMaterial" i "condition"
apartments_2024_06 <- apartments_data_2024_06[, -which(names(apartments_data_2024_06) %in% c("buildingMaterial", "condition"))]
View(apartments_2024_06)

# Uzupełnienie braków danych (NA) w kolumnie 'floor' medianą (3), lub wartością floor count, jeżeli jest mniejsza od 3.
apartments_2024_06$floor[is.na(apartments_2024_06$floor)] <- 
  pmin(3, apartments_2024_06$floorCount[is.na(apartments_2024_06$floor)])
View(apartments_2024_06)
table(apartments_2024_06$floor)

#Uzupełnianie informacji na temat windy
apartments_2024_06_Winda <- apartments_2024_06 %>%
  mutate(
    hasElevator = case_when(
      # 1. Najpierw sprawdzamy warunki do UZUPEŁNIENIA (dla NA lub pustego tekstu)
      
      # Jeśli (jest NA LUB jest puste) I piętro > 4 -> wpisz "yes"
      (is.na(hasElevator) | hasElevator == "") & floor > 4 ~ "yes", 
      
      # Jeśli (jest NA LUB jest puste) I piętro <= 4 -> wpisz "no"
      (is.na(hasElevator) | hasElevator == "") & floor <= 4 ~ "no",  
      
      # 2. W każdym innym przypadku (czyli jak coś tam już było wpisane) -> zostaw to
      TRUE ~ hasElevator
    )
  )

View(apartments_2024_06_Winda)

#usuwamy dane zawierające NA w floorcount
warunek <- !is.na(apartments_2024_06_Winda$floorCount)
apartments_clean_floorcount <- apartments_2024_06_Winda[warunek, ]

#Braki w dystansach do ważnych punktów zastępujemy średnią odległością do danego punktu w danym mieście.
distance_cols <- c(
  "collegeDistance", "clinicDistance", "restaurantDistance", 
  "pharmacyDistance", "postOfficeDistance", "kindergartenDistance", 
  "schoolDistance"
)

apartments_imputed <- apartments_clean_floorcount %>%
  
  group_by(city) %>%
  
  mutate(
    across(
      .cols = all_of(distance_cols),
      .fns = ~ replace(., is.na(.), mean(., na.rm = TRUE))
    )
  ) %>%
  ungroup()
#Sprawdzamy co ma największą korelację z rokiem budowy
num_apartments_imputed <- apartments_imputed[sapply(apartments_imputed, is.numeric)]
cor_with_build <- cor(num_apartments_imputed, use = "complete.obs")["buildYear", ]
cor_sorted <- sort(abs(cor_with_build), decreasing = TRUE)
cor_sorted
#Wybieramy te zmienne które mają największą korelację z rokiem budowy

#Metoda imputacji dla roku budowy - kNN
distance_vars_for_knn <- c("poiCount","centreDistance")
apartments_imputed_city_knn <- apartments_imputed %>%
  group_by(city) %>%
  group_modify(~ {
    dane_grupy <- .x 
    
    imputed_data <- kNN(
      dane_grupy, 
      variable = "buildYear",
      dist_var = distance_vars_for_knn, 
      k = 5,
   )
    return(imputed_data)
  }) %>%
  
  ungroup() %>%
  mutate(
    buildYear = ifelse(
      buildYear_imp, 
      round(buildYear), 
      buildYear
    )
  ) %>%
  select(-ends_with("_imp"))

#robimy type modelem random forest

apartments_imputed_city_knn$type[trimws(apartments_imputed_city_knn$type) == ""] <- NA
apartments_imputed_city_knn$type <- as.factor(apartments_imputed_city_knn$type)
train <- apartments_imputed_city_knn[!is.na(apartments_imputed_city_knn$type), ]
test  <- apartments_imputed_city_knn[is.na(apartments_imputed_city_knn$type), ]
train$type <- droplevels(train$type)
print(levels(train$type))

model <- randomForest(
  type ~ ., 
  data = train,
  ntree = 500,
  na.action = na.omit 
)
pred <- predict(model, newdata = test)
apartments_imputed_city_knn$type[is.na(apartments_imputed_city_knn$type)] <- pred
print(table(apartments_imputed_city_knn$type))

#Czyszczenie i filtracja danych o windach 

#Zamiana wartości "yes" na NA w błędnych przypadkach
apartments_imputed_city_knn <- apartments_imputed_city_knn %>%
  mutate(hasElevator = ifelse(floorCount <= 1 & hasElevator == "yes", NA, hasElevator))

#Usunięcie wszystkich wierszy, gdzie hasElevator to NA
apartments_imputed_city_knn <- apartments_imputed_city_knn %>%
  filter(!is.na(hasElevator))

table(apartments_imputed_city_knn$hasElevator)
table(apartments_imputed_city_knn$floorCount)
table(apartments_imputed_city_knn$hasElevator[apartments_imputed_city_knn$floorCount == 1])
#Wykres testowy
gg_miss_var(apartments_imputed_city_knn)
View(apartments_imputed_city_knn)

#Notatka 
#Trzeba zrobić coś z pustymi wartościami TYPE
table(apartments_imputed_city_knn$type)

### WERSJA 1: PRZED CZYSZCZENIEM ###

# 2. Konfrontacja reguł z danymi (apartments_imputed_city_knn)
cf_apartments <- confront(apartments_data_2024_06, rules_apartments)

# 3. Podsumowanie wyników
print("Podsumowanie walidacji:")
summary(cf_apartments)

# 4. Wizualizacja
barplot(cf_apartments, main = "Naruszenia reguł (nowe zmienne)")

### WERSJA 2: PO CZYSZCZENIU ###

# 2. Konfrontacja reguł z danymi (apartments_imputed_city_knn)
cf_apartments_after_cleaning <- confront(apartments_imputed_city_knn, rules_apartments)

# 3. Podsumowanie wyników
print("Podsumowanie walidacji:")
summary(cf_apartments_after_cleaning)

# 4. Wizualizacja
barplot(cf_apartments_after_cleaning, main = "Naruszenia reguł (nowe zmienne)")
