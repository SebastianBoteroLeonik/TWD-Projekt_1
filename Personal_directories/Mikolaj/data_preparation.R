library(dplyr)
library(readxl)
library(countries) # potrzebne aby szybko wybrać z un_wpp tylko kraje, a nie np UE czy kontynent
library(ggplot2)
library(africamonitor) # potrzebne aby wybrać same kraje afrykańskie
library(ggridges) # dodatek do ggplot, potrzebne do wykresu korzystającego z unigme_by_sex_processed
library(countrycode) # potrzebne do dopasowania krajów do kontynentów dla wykresu korzystającego z unigme_by_sex_processed


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
  select(country, Cause.of.death, year, mortality)

ggplot(causes_death_processed |> 
        filter(country %in% am_countries$Country_ISO) |> 
        slice(1:(14*5)) , 
       aes(x=mortality, y=country, fill = Cause.of.death)) +
  geom_bar(position = 'fill', stat="identity")

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
         filter(survey_name == 'VR Submitted to WHO/UNIGME 2023 version', sex == 'Female') |>
         group_by(country) |> 
         slice_max(n= 1,order_by=tibble(year,reference_date)),
       aes(x = as.numeric(u5mr), y = continent, fill = continent)) +
  stat_density_ridges(scale = 1.5, alpha = 0.7) +
  scale_x_sqrt() +
  theme(
    legend.position="none",
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











