library(dplyr)
library(readxl)
library(countries) # potrzebne aby szybko wybrać z un_wpp tylko kraje, a nie np UE czy kontynent


un_wpp <- read.csv("WPP2024_Demographic_Indicators_Medium.csv.gz")
# Q5 - U5MR
# 2023
un_wpp |> 
  filter(Location %in% list_countries(), Time == '2023')|>
  select(Location, Time, IMR, Under5Deaths, u5mr=Q5) |>
  group_by(Location) -> x

# 1950
un_wpp |> 
  filter(Location %in% list_countries(), Time == '1950')|>
  select(Location, Time, IMR, Under5Deaths, u5mr=Q5) |>
  group_by(Location) -> y


unigme_total <- read_xlsx('UNIGME-2023-Total-U5MR-IMR-and-NMR-database.xlsx',3)


unigme_total |> rename(country = "Total under-five mortality rate (deaths per 1,000 live births)", survey_name='...3',
                       year='...4', reference_date = '...13', u5mr = '...14', standard_error = '...15') |> 
  select(country, survey_name, year, reference_date, u5mr, standard_error) |> 
  slice(-(1:2)) |> 
  filter(survey_name == 'Multiple Indicator Cluster Survey') |> group_by(country) |> 
  slice_max(n= 1,order_by=tibble(year,reference_date)) -> a

#Demographic and Health Survey
unigme_total |> rename(country = "Total under-five mortality rate (deaths per 1,000 live births)", survey_name='...3',
                       year='...4', reference_date = '...13', u5mr = '...14', standard_error = '...15') |> 
  select(country, survey_name, year, reference_date, u5mr, standard_error) |> 
  slice(-(1:2)) |> 
  filter(survey_name == 'Demographic and Health Survey') |> 
  group_by(country) |> 
  slice_max(n= 1,order_by=tibble(year,reference_date)) -> b

# TO - DO: narysować mapy świata dla x, a i b z wypisanym u5mr dla każdego kraju, a potem wybrac która lepsza
#          potem to samo dla y, żeby pokazać postęp na świecie
#          następnie pewnie skupienie się na Afryce np korzystając z danych dla subregions, wealth, sex-specific czy Ca-Code
