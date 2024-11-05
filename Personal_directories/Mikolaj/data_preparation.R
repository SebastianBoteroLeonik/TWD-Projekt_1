library(dplyr)
library(readxl)
library(countries) # potrzebne aby szybko wybrać z un_wpp tylko kraje, a nie np UE czy kontynent
library(ggplot2)
library(africamonitor) # potrzebne aby wybrać same kraje afrykańskie
library(ggridges) # dodatek do ggplot, potrzebne do wykresu korzystającego z unigme_by_sex_processed
library(countrycode) # potrzebne do dopasowania krajów do kontynentów dla wykresu korzystającego z unigme_by_sex_processed
library(forcats)


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

ggplot(uw_mul, aes(wealth_group, u5mr)) +
  geom_boxplot() # dodanie + ylim(0,100) zmienia wartości (np wartość mediany dla 1) na wykresie - dziwne, nwm czemu

# Moim zdaniem ten wykres ma lepsze dane
ggplot(uw_dem, aes(wealth_group, u5mr)) + # tylko Afryka: |> filter(country %in% am_countries$Country_ISO)
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) 



causes_death <- read.csv('CA-CODE-2024-Under5.csv')

causes_death_processed <- causes_death |>
  rename(country = Geographic.area, year = TIME_PERIOD, mortality = OBS_VALUE) |> 
  filter(year == '2021', Unit.of.measure == "Deaths per 1,000 live births", is.finite(mortality) == TRUE) |> 
  select(country, Cause.of.death, year, mortality) |>
  mutate(death_cause_type = case_when(
    Cause.of.death %in% c('Birth asphyxia/trauma', 'Prematurity', 'Congenital anomalies') ~ 'Birth-related conditions',
    #Cause.of.death == 'Tetanus' ~ 'Bacterial disease (Tetanus)',
    Cause.of.death %in% c('HIV/AIDS', 'Measles') ~ 'Viral diseases (HIV/AIDS, Measles)',
    Cause.of.death %in% c('Lower respiratory infections', 'Tuberculosis') ~ 'Respiratory diseases (Lower
  respiratory infections, Tuberculosis)',
    .default = Cause.of.death), country = case_when(country == "Central African Republic" ~ 'CAR',
                                                    country == 'Venezuela (Bolivarian Republic of)' ~ 'Venezuela',
                                                    .default = country)) |>
  arrange(desc(mortality))

temp <- causes_death_processed |>
  group_by(country) |>
  summarise(sum_mortality = sum(mortality))

causes_death_processed <- left_join(causes_death_processed, temp) |> arrange(desc(sum_mortality))

ggplot(causes_death_processed |> 
         filter(country %in% c('Niger', 'Nigeria', "Somalia", "Chad", "Sierra Leone", "CAR", 
                               "Venezuela", "China", "Poland", "Singapore", "Estonia","Norway")), #Benin, Japan, Guinea, UAE
        # filter(country %in% am_countries$Country_ISO) |> 
        # slice(1:(14*5)) , 
       aes(x = mortality, y = fct_inorder(country), fill = death_cause_type)) +
  geom_bar(position = 'fill', stat="identity", width = 0.65) +
  scale_fill_manual(values = c("dodgerblue2", "#E31A1C", 
                               "black",
                               "#6A3D9A", 
                               "skyblue2",
                               "#FF7F00", "green4",
                               "deeppink1", "palegreen2", 
                               "yellow3")) +
  scale_x_continuous(expand = c(0.02,0)) +
  theme(legend.text = element_text(size=7))

# TO - DO: poprawić wykresy na bardziej zjadliwe i skorzystać z danych dla subregions i sex-specific



unigme_by_sex <- read_xlsx("UNIGME-2023-Sex-specific-U5MR-and-IMR-database.xlsx",3)

unigme_by_sex_processed <- unigme_by_sex |>
  rename(country = "Sex-specific under-five mortality rate (deaths per 1,000 live births)",survey_name='...3', year='...4', 
         sex = '...6', reference_date = '...13', u5mr = '...14', standard_error = '...15') |> 
  select(country, survey_name, year, sex, reference_date, u5mr, standard_error) |> 
  slice(-(1:2)) 

unigme_by_sex_processed$continent = countrycode(sourcevar = as.data.frame(unigme_by_sex_processed)[, "country"],
                                                origin = "country.name",
                                                destination = "continent")

ggplot(unigme_by_sex_processed |>
         filter(survey_name == 'VR Submitted to WHO/UNIGME 2023 version', sex  %in% c('Female', 'Male'), u5mr > 0) |>
         group_by(country) |> 
         slice_max(n= 1,order_by=tibble(year,reference_date)),
       aes(x = as.numeric(u5mr), y = continent, fill = continent)) +
  stat_density_ridges(scale = 1.3, alpha = 0.7, quantile_lines = TRUE, quantiles = 4) +
  facet_wrap(~sex) +
  scale_x_sqrt() +
  theme_ridges() +
  theme(
    legend.position="none"
    #panel.spacing = unit(0.1, "lines"),
    #strip.text.x = element_text(size = 8)
  )




unigme_district <- read_xlsx('UN-IGME-2023-Subnational-U5MR-and-NMR-database.xlsx',3)

unigme_district_processed <- unigme_district |>
  filter(Indicator == 'Under-five mortality rate') |>
  rename(country = "Country.Name",survey_name='Series.Name', year='Series.Year', 
         district = 'Area.Name', reference_date = 'Reference.Date', u5mr = 'Estimates', 
         standard_error = 'Standard.Error.of.Estimates') |> 
  select(country, district, survey_name, year, reference_date, u5mr, standard_error) |> 
  group_by(country, district) |> 
  slice_max(n= 1,order_by=tibble(year,reference_date))


# TO - DO: poprawić/ dodać nowe wykresy, dla unigme_district mozna próbować skorzystać z http://www.gadm.org/ 
# aby miec mapę z podziałem na dystrykty
























#NIE CZYTAĆ
# world <- map_data("world")
# 
# fix_names <- function(x, replacements){
#   for (i in 1:nrow(replacements)) {
#     x <- replace(x, x == replacements[i, 1], replacements[i, 2])
#   }
#   x
# }
# 
# country_name_replacements <- matrix(
#   c( "United States of America (and dependencies)", "USA",
#      "Russian Federation", "Russia",
#      "Iran (Islamic Republic of)", "Iran",
#      "United Kingdom", "UK",
#      "Czechia", "Czech Republic",
#      "Venezuela (Bolivarian Republic of)", "Venezuela",
#      "Bolivia (Plurinational State of)", "Bolivia",
#      "Türkiye", "Turkey",
#      "Syrian Arab Republic", "Syria",
#      "Kosovo (under UNSC res. 1244)", "Kosovo",
#      "Republic of Moldova", "Moldova",
#      "Dem. People's Republic of Korea", "North Korea",
#      "Republic of Korea", "South Korea",
#      "Viet Nam", "Vietnam",
#      "Lao People's Democratic Republic", "Laos",
#      "Côte d'Ivoire", "Ivory Coast",
#      "Congo", "Republic of Congo",
#      "United Republic of Tanzania", "Tanzania",
#      "China, Taiwan Province of China", "Taiwan",
#      "Eswatini", "Swaziland",
#      "State of Palestine", "Palestine"), ncol = 2, byrow = TRUE)
# 
# un_wpp |> 
#   mutate(Location = fix_names(Location, country_name_replacements)) |>
#   filter(Location %in% world$region, Time == '1950')|>
#   select(region = Location, u5mr=Q5)|>
#   right_join(world) |>
#   ggplot(mapping = aes(x = long, y = lat, group = group)) +
#   coord_fixed(1.3)+
#   geom_polygon(aes(fill = u5mr)) +
#   scale_fill_distiller(palette ="RdBu", direction = -1) +
#   labs(fill = "Under 5 mortality rate")
# 
# un_wpp |> 
#   mutate(Location = fix_names(Location, country_name_replacements)) |>
#   filter(Location %in% world$region, Time == '2023')|>
#   select(region = Location, u5mr=Q5)|>
#   right_join(world) |>
#   ggplot(mapping = aes(x = long, y = lat, group = group)) +
#   coord_fixed(1.3)+
#   geom_polygon(aes(fill = u5mr)) +
#   scale_fill_distiller(palette ="RdBu", direction = -1) +
#   labs(fill = "Under 5 mortality rate")
# 
# 
# un_wpp |> 
#   mutate(Location = fix_names(Location, country_name_replacements)) |>
#   filter(Location %in% world$region, Time %in% c("1950", "2023"))|>
#   select(region = Location, u5mr=Q5, year=Time)|>
#   left_join(world) |>
#   ggplot(aes(x = long, y = lat, group = group)) +
#   coord_fixed(1.3) +
#   # coord_map() +
#   geom_polygon(aes(fill = u5mr), colour = "#333333", size = 0.2) +
#   # scale_fill_distiller(palette = "Reds", direction = 1) +
#   scale_fill_gradient2(low = "#ccffdd",
#                        mid = "#ccaa44",
#                        high = "#bb0000",
#                        midpoint = 250) +
#   # scale_fill_distiller(palette = "YlOrBr", direction = 1) +
#   # scale_fill_distiller(palette = "YlOrRd", direction = 1) +
#   labs(fill = "Under 5 mortality rate") +
#   facet_wrap(~year)
# # theme(panel.background = element_rect(fill = "#cccccc"),
# #       panel.grid.major = element_line(colour = "grey"),
# #       panel.grid.minor = element_line(colour = "grey"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(uw_mul, aes(wealth_group, u5mr, color = "u5mr")) +
#   geom_boxplot() +
#   scale_color_manual(name = "Legend", values = "black", 
#                      labels = "*Under 5 Mortality Rate \n (per 1000)") +
#   labs(y="*u5mr")
# 
# 
# 
# 
# ggplot(uw_dem |> filter(country %in% am_countries$Country_ISO), 
#        aes(wealth_group, u5mr, color = "u5mr")) +
#   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
#   scale_color_manual(
#     name = "Legend",
#     values = "black",
#     labels = "*Under 5 Mortality Rate\n(per 1000, Africa only)"
#   ) +
#   labs(y="*u5mr")
# 
# 
# 
# 
# 
# 
# 
# 
# eu_countries <- c("Albania", "Andorra", "Armenia", "Austria", "Belarus", "Belgium", 
#                   "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
#                   "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
#                   "Georgia", "Germany", "Greece", "Hungary", "Iceland", "Ireland", 
#                   "Italy", "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", 
#                   "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", 
#                   "Montenegro", "Netherlands", "North Macedonia", "Norway", 
#                   "Poland", "Portugal", "Romania", "Russia", "San Marino", 
#                   "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
#                   "Switzerland", "Turkey", "Ukraine", "United Kingdom", 
#                   "Vatican City")
# 
# africa_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
#                       "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", 
#                       "Chad", "Comoros", "Democratic Republic of the Congo", "Djibouti", 
#                       "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", 
#                       "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", 
#                       "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
#                       "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
#                       "Namibia", "Niger", "Nigeria", "Republic of the Congo", "Rwanda", 
#                       "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", 
#                       "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", 
#                       "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
# 
# 
# causes_death_processed_mod <- causes_death_processed%>%
#   mutate(continent = ifelse(country %in% eu_countries, "Europe",
#                             ifelse(country %in% africa_countries, "Africa", "Rest of the world"))) %>%
#   group_by(continent, Cause.of.death) %>%
#   summarise(avg_mortalityU5 = mean(mortality))
# 
# 
# #tab1 <- unigme_total_processed %>%
# #  filter(country %in% eu_countries) %>%  
# #  select(country, u5mr) %>%               
# #  group_by(country) %>%                    
# #  summarise(avg_u5mr = mean(as.numeric(u5mr), na.rm = TRUE), .groups = 'drop')
# 
# 
# 
# ggplot(causes_death_processed_mod, aes(x = continent,
#                                        y = avg_mortalityU5,
#                                        fill = Cause.of.death)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Average Under-5 Mortality by Continent and Cause of Death (per 1000)",
#        x = "Continent", 
#        y = "Average Under-5 Mortality (U5MR)",
#        fill = "Cause of Death") +
#   theme_minimal() +
#   scale_fill_viridis_d(option = "plasma")  # Użycie palety Viridis z większą liczbą kolorów
# 
# causes_death_processed_mod2 <- causes_death_processed%>%
#   mutate(continent = ifelse(country %in% eu_countries, "Europe",
#                             ifelse(country %in% africa_countries, "Africa", "Rest of world"))) %>%
#   group_by(continent) %>%
#   mutate(all_in_one = sum(mortality)) %>%
#   ungroup() %>%
#   mutate(of100=mortality/all_in_one) %>%
#   select(continent, Cause.of.death, of100)
# 
# 
# ggplot(causes_death_processed_mod2, aes(x = continent, y = of100, fill = Cause.of.death)) +
#   geom_bar(stat = "identity", position = "fill") +  # Wykres z wypełnieniem procentowym
#   scale_y_continuous(labels = scales::percent_format()) +  # Oś Y w formacie procentowym
#   labs(title = "Proportional Mortality by Cause of Death and Continent",
#        x = "Continent", 
#        y = "Percentage of Total Mortality",
#        fill = "Cause of Death") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Etykiety osi X pod kątem 45 stopni
#   scale_fill_viridis_d(option = "plasma")  # Kolory Viridis dla przyczyn śmierci

