# 🏢 Analiza Rynku Mieszkaniowego w Polsce

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=RStudio&logoColor=white)
![Gdańsk Tech](https://img.shields.io/badge/Politechnika_Gdańska-red?style=for-the-badge&logo=school&logoColor=white)
![Status](https://img.shields.io/badge/Status-Finished-success?style=for-the-badge)

> **Projekt realizowany w ramach przedmiotu:** > *Analiza Danych w R* > **Uczelnia:** Politechnika Gdańska 

---

## Cel Projektu

Analiza czynników cenotwórczych na rynku mieszkań w Polsce (2024)

Głównym celem projektu jest zbadanie, co tak naprawdę wpływa na ceny nieruchomości w Polsce. Raport stanowi próbę odpowiedzi na kluczowe pytania inwestycyjne:

Jak silny jest wpływ lokalizacji (miasto, dystans do centrum) na wycenę?

Czy bliskość infrastruktury usługowej (POI) realnie podbija wartość metra kwadratowego?

Jaką premię cenową dają parametry techniczne (winda, rok budowy)?

Publikacja obejmuje kompletny proces analityczny: od czyszczenia danych i inżynierii cech, przez zaawansowaną wizualizację przestrzenną, aż po weryfikację hipotez za pomocą modelowania statystycznego.

---

## Zespół

|  Imię |
| :--- | 
|  **Wiktor** |
| ️ **Michał** |
|  **Kacper** | 

---

## O Danych

Zbiór danych zawiera oferty sprzedaży mieszkań z czerwca 2024 roku.
**Źródło danych:** [https://www.kaggle.com/datasets/krzysztofjamroz/apartment-prices-in-poland/?select=apartments_pl_2023_08.csv]
---
## Słownik Zmiennych (Data Dictionary)

Poniższa tabela przedstawia opis zmiennych dostępnych w analizowanym zbiorze danych:

| Nazwa Zmiennej | Opis |
| :--- | :--- |
| **id** | Unikalny identyfikator ogłoszenia |
| **city** | Miasto, w którym znajduje się nieruchomość |
| **price** | Cena ofertowa (PLN) |
| **squareMeters** | Powierzchnia mieszkania w m² |
| **rooms** | Liczba pokoi |
| **floor / floorCount** | Piętro mieszkania / Liczba pięter w budynku |
| **buildYear** | Rok budowy |
| **type** | Rodzaj zabudowy (kamienica, blok, apartamentowiec) |
| **ownership** | Forma własności |
| **lat / lon** | Współrzędne geograficzne |
| **centreDistance** | Odległość od centrum miasta (km) |
| **poiCount** | Liczba punktów usługowych w promieniu 500m |
| **\*Distance** | Odległości do: szkół, przychodni itp. |
| **has\*** | Czy ma udogodnienie (Winda, Balkon, Parking...)? |

---

## 🛠️ Wykorzystane Technologie i Pakiety
Projekt został zrealizowany w ekosystemie R z naciskiem na nowoczesne biblioteki do wizualizacji, analizy przestrzennej oraz automatycznego raportowania.

1. Przetwarzanie i Manipulacja Danych (Data Wrangling)
dplyr – fundament projektu: filtrowanie, mutowanie i agregacja danych.

tidyr – czyszczenie i formatowanie struktury danych (tidy data).

stringr – operacje na ciągach tekstowych.

2. Zaawansowana Wizualizacja (Data Viz)
ggplot2 – tworzenie warstwowych wykresów statycznych.

patchwork – łączenie wielu niezależnych wykresów w jedną kompozycję (np. mapy Gdańska i Warszawy obok siebie).

viridis – profesjonalne palety kolorystyczne przyjazne dla daltonistów (użyte w mapach ciepła).

hexbin – agregacja heksagonalna do analizy gęstości zabudowy.

scales – formatowanie walut i osi liczbowych.

3. Wnioskowanie Statystyczne (Statistical Inference)
ggstatsplot – automatyzacja testów statystycznych (ANOVA, t-test) połączona z wizualizacją wyników i parametrami (p-value, wielkość efektu).

rstatix – "pipe-friendly" obliczenia statystyczne.

car – weryfikacja założeń (Test Levene'a jednorodności wariancji).

report – automatyczne generowanie opisów wyników statystycznych w języku naturalnym.

4. Analiza Przestrzenna (Geospatial & GIS)
sf (Simple Features) – nowoczesny standard obsługi danych wektorowych i geometrii miast.

osmdata – pobieranie granic administracyjnych miast (API OpenStreetMap).

ggspatial – elementy kartograficzne na wykresach ggplot.

5.  Interaktywność i Dashboard (Bezserwerowe)
crosstalk – komunikacja między widgetami (suwaki filtrujące dane na żywo bez użycia Shiny Server).

plotly – interaktywne wykresy (zoom, tooltips) działające w przeglądarce.

DT – interaktywne tabele z możliwością przeszukiwania i sortowania.

6. Animacja i Raportowanie
gganimate & gifski – wizualizacja rozwoju tkanki miejskiej w czasie (Time-series animation).

rmarkdown & knitr – silnik generujący raport HTML.

bslib (Bootstrap 5) – nowoczesny motyw graficzny raportu z obsługą Dark Mode (tryb ciemny/jasny).
---

## Raport

**Raport Rpubs:** http://rpubs.com/KACCISDATA/1391200
---


