library(dplyr)
library(readxl)
library(countries) # potrzebne aby szybko wybrać z un_wpp tylko kraje, a nie np UE czy kontynent
library(ggplot2)
library(africamonitor) # potrzebne aby wybrać same kraje afrykańskie


un_wpp <- read.csv("WPP2024_Demographic_Indicators_Medium.csv.gz")
# Q5 - U5MR
# 2023
un_wpp_2023 <- un_wpp |> 
  filter(Location %in% list_countries(), Time == '2023')|>
  select(Location, Time, IMR, Under5Deaths, u5mr=Q5) |>
  group_by(Location)

# 1950
un_wpp_1950 <- un_wpp |> 
  filter(Location %in% list_countries(), Time == '1950')|>
  select(Location, Time, IMR, Under5Deaths, u5mr=Q5) |>
  group_by(Location)




unigme_total <- read_xlsx('UNIGME-2023-Total-U5MR-IMR-and-NMR-database.xlsx',3)

unigme_total_processed <- unigme_total |>
  rename(country = "Total under-five mortality rate (deaths per 1,000 live births)",survey_name='...3', year='...4',
         reference_date = '...13', u5mr = '...14', standard_error = '...15') |> 
  select(country, survey_name, year, reference_date, u5mr, standard_error) |> 
  slice(-(1:2)) 

#Multiple Indicator Cluster Survey - 85 unikalne wystapienia
ut_mul <- unigme_total_processed |> 
  filter(survey_name == 'Multiple Indicator Cluster Survey') |> group_by(country) |> 
  slice_max(n= 1,order_by=tibble(year,reference_date))

#Demographic and Health Survey - 93 unikalne wystapienia
ut_dem <- unigme_total_processed |> 
  filter(survey_name == 'Demographic and Health Survey') |> 
  group_by(country) |> 
  slice_max(n= 1,order_by=tibble(year,reference_date))


#VR Submitted to WHO/UNIGME 2023 version - 129 wystąpień
ut_vr <- unigme_total_processed |> 
  filter(survey_name == 'VR Submitted to WHO/UNIGME 2023 version') |> 
  group_by(country) |> 
  slice_max(n= 1,order_by=tibble(year,reference_date))

# TO - DO: narysować mapy świata dla x, a i b z wypisanym u5mr dla każdego kraju, a potem wybrac która lepsza
#          potem to samo dla y, żeby pokazać postęp na świecie
#          następnie pewnie skupienie się na Afryce np korzystając z danych dla subregions, wealth, sex-specific czy Ca-Code


#NIE czytać
# unigme_total |> rename(country = "Total under-five mortality rate (deaths per 1,000 live births)", survey_name='...3',
#                        year='...4', reference_date = '...13', u5mr = '...14', standard_error = '...15') |>
#   select(country, survey_name, year, reference_date, u5mr, standard_error) |>
#   slice(-(1:2)) |>
#  group_by(country,survey_name) |> count(survey_name) |> group_by(survey_name) |> count() |> arrange(desc(n)) 




unigme_wealth <- read_xlsx('UNIGME-2023-Wealth-quintile-U5MR-database.xlsx',3)

unigme_wealth_processed <- unigme_wealth |> 
  rename(country = "Total under-five mortality rate by wealth quintile (deaths per 1,000 live births)", survey_name='...3',
         year='...4', reference_date = '...13', wealth_group = '...14', u5mr = '...15', standard_error = '...16') |> 
  select(country, survey_name, year, reference_date, wealth_group, u5mr, standard_error) |> slice(-(1:2)) |>
  mutate(u5mr = as.numeric(u5mr))

#Multiple Indicator Cluster Survey
uw_mul <- unigme_wealth_processed |>
  filter(survey_name == 'Multiple Indicator Cluster Survey') |> 
  group_by(country) |> 
  slice_max(n= 1,order_by=tibble(year,reference_date)) #|> mutate(u5mr = round(u5mr,2))


#Demographic and Health Survey
uw_dem <- unigme_wealth_processed |> 
  filter(survey_name == 'Demographic and Health Survey') |> 
  group_by(country) |> 
  slice_max(n= 1,order_by=tibble(year,reference_date))

#VR Submitted to WHO/UNIGME 2023 version - nie istnieje dla unigme_wealth

ggplot(uw_mul, aes(wealth_group, u5mr, color = "u5mr")) +
  geom_boxplot() +
  scale_color_manual(name = "Legend", values = "black", 
                     labels = "*Under 5 Mortality Rate \n (per 1000)") +
  labs(y="*u5mr")

# Moim zdaniem ten wykres ma lepsze dane
#ggplot(uw_dem, aes(wealth_group, u5mr)) + # tylko Afryka: |> filter(country %in% am_countries$Country_ISO)
#  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 


ggplot(uw_dem |> filter(country %in% am_countries$Country_ISO), 
       aes(wealth_group, u5mr, color = "u5mr")) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_color_manual(
    name = "Legend",
    values = "black",
    labels = "*Under 5 Mortality Rate\n(per 1000, Africa only)"
  ) +
  labs(y="*u5mr")



causes_death <- read.csv('CA-CODE-2024-Under5.csv')

causes_death_processed <- causes_death |>
  rename(country = Geographic.area, year = TIME_PERIOD, mortality = OBS_VALUE) |> 
  filter(year == '2021', Unit.of.measure == "Deaths per 1,000 live births", is.finite(mortality) == TRUE) |> 
  select(country, Cause.of.death, year, mortality)

ggplot(causes_death_processed |> 
         filter(country %in% am_countries$Country_ISO) |> 
         slice(1:(14*5)) , 
       aes(x=mortality, y=country, fill = Cause.of.death)) +
  geom_bar(position = 'fill', stat="identity")


ggplot(causes_death_processed |> 
         filter(country %in% am_countries$Country_ISO) |> 
         slice(1:(14*5)) , 
       aes(x=mortality, y=country, fill = Cause.of.death)) +
  geom_bar(position = 'fill', stat="identity")



eu_countries <- c("Albania", "Andorra", "Armenia", "Austria", "Belarus", "Belgium", 
                  "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
                  "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                  "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
                  "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", 
                  "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", 
                  "Montenegro", "Netherlands", "North Macedonia", "Norway", 
                  "Poland", "Portugal", "Romania", "Russia", "San Marino", 
                  "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
                  "Switzerland", "Turkey", "Ukraine", "United Kingdom", 
                  "Vatican City")

africa_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
                      "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", 
                      "Chad", "Comoros", "Democratic Republic of the Congo", "Djibouti", 
                      "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", 
                      "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", 
                      "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
                      "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
                      "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Rwanda", 
                      "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", 
                      "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", 
                      "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")


causes_death_processed_mod <- causes_death_processed%>%
  mutate(continent = ifelse(country %in% eu_countries, "Europe",
                            ifelse(country %in% africa_countries, "Africa", "Rest of the world"))) %>%
  group_by(continent, Cause.of.death) %>%
  summarise(avg_mortalityU5 = mean(mortality))


#tab1 <- unigme_total_processed %>%
#  filter(country %in% eu_countries) %>%  
#  select(country, u5mr) %>%               
#  group_by(country) %>%                    
#  summarise(avg_u5mr = mean(as.numeric(u5mr), na.rm = TRUE), .groups = 'drop')



ggplot(causes_death_processed_mod, aes(x = continent,
                                       y = avg_mortalityU5,
                                       fill = Cause.of.death)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Under-5 Mortality by Continent and Cause of Death (per 1000)",
       x = "Continent", 
       y = "Average Under-5 Mortality (U5MR)",
       fill = "Cause of Death") +
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma")  # Użycie palety Viridis z większą liczbą kolorów

causes_death_processed_mod2 <- causes_death_processed%>%
  mutate(continent = ifelse(country %in% eu_countries, "Europe",
                            ifelse(country %in% africa_countries, "Africa", "Rest of world"))) %>%
  group_by(continent) %>%
  mutate(all_in_one = sum(mortality)) %>%
  ungroup() %>%
  mutate(of100=mortality/all_in_one) %>%
  select(continent, Cause.of.death, of100)


ggplot(causes_death_processed_mod2, aes(x = continent, y = of100, fill = Cause.of.death)) +
  geom_bar(stat = "identity", position = "fill") +  # Wykres z wypełnieniem procentowym
  scale_y_continuous(labels = scales::percent_format()) +  # Oś Y w formacie procentowym
  labs(title = "Proportional Mortality by Cause of Death and Continent",
       x = "Continent", 
       y = "Percentage of Total Mortality",
       fill = "Cause of Death") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Etykiety osi X pod kątem 45 stopni
  scale_fill_viridis_d(option = "plasma")  # Kolory Viridis dla przyczyn śmierci

