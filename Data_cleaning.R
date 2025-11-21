#Biblioteki
library(dplyr)
#Czyszczenie danych
#załaduj mi obiekt apartments_pl_2023_08.csv do R
apartments_data_2024_06 <- read.csv("https://raw.githubusercontent.com/Michu24600/GRUPA-Aa/refs/heads/main/apartments_pl_2024_06.csv?token=GHSAT0AAAAAADPYNKLJ43PG4LS3FRO4S26K2JAUN5A")
View(apartments_data_2024_06)

#Usuwanie kolumn "buildingMaterial" i "condition"
apartments_2024_06 <- apartments_data_2024_06[, -which(names(apartments_data_2024_06) %in% c("buildingMaterial", "condition"))]
View(apartments_2024_06)
# Zakładamy, że 'apartments_2024_06' to Twoja ramka, a kolumny to 'hasElevator' i 'floors'
apartments_2024_06_Winda <- apartments_2024_06 %>%
  mutate(
    hasElevator = case_when(
      !is.na(hasElevator) ~ hasElevator,
      
      # Zamiast TRUE wpisz tekst, np. "tak", "yes" lub "TRUE" (w cudzysłowie)
      is.na(hasElevator) & floor > 4 ~ "yes", 
      
      # Zamiast FALSE wpisz tekst, np. "nie", "no" lub "FALSE" (w cudzysłowie)
      is.na(hasElevator) & floor <= 4 ~ "no"  
    ))
View(apartments_2024_06_Winda)
