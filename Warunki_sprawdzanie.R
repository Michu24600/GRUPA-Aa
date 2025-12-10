# Floor count >= Floor x
# Square meters <= 20 m2 - Room <=3 x
# Floor count <=1 - elevator Yes x
# Cena <= 100000 >= 10000000 x
# Współrzędne czy faktycznie Polska x
# Wartości na plusie
# Square meters <= 300 m2
# Rok budowy <= 2024

# Załadowanie wymaganego pakietu (jeśli nie jest załadowany)
if (!require("validate")) {install.packages("validate")}
library(validate)
colnames(apartments_imputed_city_knn)

# 1. Definicja reguł walidacyjnych
rules_apartments <- validator(
  # Floor count >= Floor (Liczba pięter w budynku musi być >= piętro mieszkania)
  Logic_Floor = floorCount >= floor,
  
  # Square meters <= 20 m2 - Room <=3 (Dla mieszkań <= 20m2 liczba pokoi musi być <= 3)
  Logic_Small_Apt = if (squareMeters <= 20) rooms <= 3,
  
  # Floor count <=1 - elevator Yes (Zgodnie z poleceniem: jeśli 1 piętro lub mniej, nie może być windy)
  Logic_Elevator = if (floorCount <= 1) hasElevator %in% c("yes", FALSE),
  
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
  Positive_Building = buildYear > 0 & floorCount > 0 & floor > 0, 
  
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

# 2. Konfrontacja reguł z danymi (apartments_imputed_city_knn)
cf_apartments <- confront(apartments_data_2024_06, rules_apartments)

# 3. Podsumowanie wyników
print("Podsumowanie walidacji:")
summary(cf_apartments)

# 4. Wizualizacja
barplot(cf_apartments, main = "Naruszenia reguł (nowe zmienne)")

#2 WERSJA PONIŻEJ

# 2. Konfrontacja reguł z danymi (apartments_imputed_city_knn)
cf_apartments_after_cleaning <- confront(apartments_imputed_city_knn, rules_apartments)

# 3. Podsumowanie wyników
print("Podsumowanie walidacji:")
summary(cf_apartments_after_cleaning)

# 4. Wizualizacja
barplot(cf_apartments_after_cleaning, main = "Naruszenia reguł (nowe zmienne)")