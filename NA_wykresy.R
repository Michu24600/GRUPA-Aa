#Wizualizacja niepoprawności danych 
install.packages("naniar")
library(naniar)

#załaduj mi obiekt apartments_pl_2024_06.csv do R
apartments_data_2024_06 <- read.csv("https://raw.githubusercontent.com/Michu24600/GRUPA-Aa/refs/heads/main/apartments_pl_2024_06.csv?token=GHSAT0AAAAAADPYNKLJ4HBMTZVSYFIIDQ2G2JA37GA")
View(apartments_data_2024_06)

#Usuwanie kolumn "buildingMaterial" i "condition" - powodem usunięcia była znacznie podwyższona liczba brakujących wartości w tych kolumnach w porównaniu do innych kolumn w zbiorze danych.
apartments_2024_06 <- apartments_data_2024_06[, -which(names(apartments_data_2024_06) %in% c("buildingMaterial", "condition"))]
View(apartments_2024_06)

#Podstawowe informacje o danych

str(apartments_2024_06)
summary(apartments_2024_06)

#Liczba brakujących wartości w całej tabeli 

n_miss(apartments_2024_06)

#Liczba brakujących wartości w kolumnach

n_miss(apartments_2024_06$id)
n_miss(apartments_2024_06$city)
n_miss(apartments_2024_06$type)
n_miss(apartments_2024_06$squareMeters)
n_miss(apartments_2024_06$rooms)
n_miss(apartments_2024_06$floor)
n_miss(apartments_2024_06$floorCount)
n_miss(apartments_2024_06$price)

#Tabela podsumowująca liczby NA w tabeli
miss_summary <- miss_var_summary(apartments_2024_06)
miss_var_summary(apartments_2024_06)

#Tabela podsumowująca NA według przypadku (Obserwacji)
miss_case_table(apartments_2024_06)

#Wizualizacja brakujących danych 
vis_miss(apartments_2024_06)

#Wizualizacja część 2
gg_miss_var(apartments_2024_06)
gg_miss_case(apartments_2024_06)
gg_miss_fct(apartments_2024_06, fct = floor)
gg_miss_upset(apartments_2024_06, nsets = 7)
