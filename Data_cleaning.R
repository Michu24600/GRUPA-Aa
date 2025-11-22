#Biblioteki 1
install.packages("dplyr")
library(dplyr)
install.packages("naniar")
library(naniar)
install.packages("VIM")
library(VIM)
install.packages("tidyr")
library(tidyr)
#Czyszczenie danych
#załaduj mi obiekt apartments_pl_2024_06.csv do R
apartments_data_2024_06 <- read.csv("https://raw.githubusercontent.com/Michu24600/GRUPA-Aa/refs/heads/main/apartments_pl_2024_06.csv?token=GHSAT0AAAAAADPYNKLIN7SNQWERKDDYC3YM2JBYSJA")
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

#Wykres testowy
gg_miss_var(apartments_imputed_city_knn)
View(apartments_imputed_city_knn)


#Notatka 
#Dla NA w rok budowy przyjmujemy mediane, ewentualnie sprawdzić czy to się da jakoś po równo to podzielić
#Trzeba zrobić coś z pustymi wartościami TYPE

