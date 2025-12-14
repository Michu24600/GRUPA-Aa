# 🏢 Analiza Rynku Mieszkaniowego w Polsce

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=RStudio&logoColor=white)
![Gdańsk Tech](https://img.shields.io/badge/Politechnika_Gdańska-red?style=for-the-badge&logo=school&logoColor=white)
![Status](https://img.shields.io/badge/Status-Active-success?style=for-the-badge)

> **Projekt realizowany w ramach przedmiotu:** > *Analiza Danych w R* > **Uczelnia:** Politechnika Gdańska (Gdańsk Tech)

---

## 🎯 Cel Projektu

Głównym celem projektu jest zbadanie **czynników wpływających na ceny mieszkań w Polsce** w roku 2024. 
Staramy się odpowiedzieć na pytania:
* Jak lokalizacja (odległość od centrum, miasto) wpływa na wycenę metra kwadratowego?
* Czy bliskość punktów usługowych (szkoły, restauracje) podbija cenę?
* Jaką premię cenową dają udogodnienia takie jak winda, ochrona czy miejsce parkingowe?
* Czy "Wielka Płyta" faktycznie jest tańsza od nowego budownictwa?

Analiza obejmuje czyszczenie danych, inżynierię cech, wizualizację (mapy, wykresy) oraz modelowanie statystyczne.

---

## 👥 Zespół

|  Imię |
| :--- | 
|  **Wiktor** |
| ️ **Michał** |
|  **Kacper** | 

---

## 📊 O Danych

Zbiór danych zawiera oferty sprzedaży mieszkań z czerwca 2024 roku.
**Źródło danych:** [https://www.kaggle.com/datasets/krzysztofjamroz/apartment-prices-in-poland/?select=apartments_pl_2023_08.csv]

### Słownik Zmiennych (Data Dictionary)

| Zmienna | Opis |
| :--- | :--- |
| `city` | Miasto, w którym znajduje się nieruchomość |
| `price` | **Cena ofertowa (PLN) - zmienna celu** |
| `squareMeters` | Powierzchnia mieszkania w m² |
| `rooms` | Liczba pokoi |
| `floor` / `floorCount` | Piętro mieszkania / Liczba pięter w budynku |
| `buildYear` | Rok budowy |
| `type` | Rodzaj zabudowy (np. kamienica, blok, apartamentowiec) |
| `latitude`, `longitude` | Współrzędne geograficzne |
| `centreDistance` | Odległość od centrum miasta (km) |
| `poiCount` | Liczba punktów POI w promieniu 500m |
| `*Distance` | Odległości do: szkół, klinik, poczty, przedszkoli, restauracji, uczelni, aptek |
| `has*` | Czy posiada: miejsce parkingowe, balkon, windę, ochronę, komórkę lokatorską |

---

## 🛠️ Wykorzystane Technologie i Pakiety

Projekt został zrealizowany w języku **R** przy użyciu następujących bibliotek:
**1. Przetwarzanie i manipulacja danymi (Data Wrangling):**
* `dplyr` – główny silnik do przetwarzania, filtracji i agregacji danych.
* `tidyr` – do porządkowania struktury danych.
* `stringr` – operacje na ciągach znaków (tekstach).

**2. Wizualizacja Danych (Data Viz):**
* `ggplot2` – tworzenie statycznych wykresów (histogramy, scatterploty).
* `hexbin` – wydajna wizualizacja dużych zbiorów danych (wykresy heksagonalne).
* `scales` – skalowanie wielkości punktów i formatowanie osi (np. waluty).

**3. Analiza Przestrzenna i Mapy (Geospatial):**
* `leaflet` – interaktywne mapy (HTML widgets).
* `ggspatial` – wizualizacja danych na podkładach mapowych (OpenStreetMap) w ggplot2.
* `prettymapr` – narzędzie pomocnicze do skal i ramek na mapach.

**4. Imputacja Danych (Handling Missing Values):**
* `VIM` – algorytm kNN (k-Najbliższych Sąsiadów) do uzupełniania braków (np. rok budowy).
* `randomForest` – zaawansowana imputacja brakujących zmiennych kategorycznych (np. typ budynku).

**5. Animacja:**
* `gganimate` – animowanie wykresów w czasie (historia rynku).
* `gifski` – renderer do generowania plików GIF.

