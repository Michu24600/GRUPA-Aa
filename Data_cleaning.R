#Biblioteki
install.packages("dplyr")
library(dplyr)
install.packages("naniar")
library(naniar)
#Czyszczenie danych
#załaduj mi obiekt apartments_pl_2024_06.csv do R
apartments_data_2024_06 <- read.csv("https://raw.githubusercontent.com/Michu24600/GRUPA-Aa/refs/heads/main/apartments_pl_2024_06.csv?token=GHSAT0AAAAAADPYNKLIRXXGMVVI5RWLCM3O2JAWNRQ")
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

table(apartments_2024_06_Winda$floor)
any(apartments_2024_06_Winda$floorCount < 3 & apartments_2024_06_Winda$floor== 3)
apartments_2024_06_Winda[apartments_2024_06_Winda$floorCount < 3 & apartments_2024_06_Winda$floor == 3, ]
sum(!complete.cases(apartments_data_2024_06))
sum(!complete.cases(apartments_2024_06_Winda))
