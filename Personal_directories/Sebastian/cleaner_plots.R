library(readxl)
library(ggplot2)
library(dplyr)

un_wpp <- read.csv("../Mikolaj/WPP2024_Demographic_Indicators_Medium.csv.gz")
unigme_total <- read_xlsx('../Mikolaj/UNIGME-2023-Total-U5MR-IMR-and-NMR-database.xlsx',3)
unigme_wealth <- read_xlsx('../Mikolaj/UNIGME-2023-Wealth-quintile-U5MR-database.xlsx',3)
causes_death <- read.csv('../Mikolaj/CA-CODE-2024-Under5.csv')

world <- map_data("world")

fix_names <- function(x, replacements){
  for (i in 1:nrow(replacements)) {
    x <- replace(x, x == replacements[i, 1], replacements[i, 2])
  }
  x
}

country_name_replacements <- matrix(
  c( "United States of America (and dependencies)", "USA",
     "Russian Federation", "Russia",
     "Iran (Islamic Republic of)", "Iran",
     "United Kingdom", "UK",
     "Czechia", "Czech Republic",
     "Venezuela (Bolivarian Republic of)", "Venezuela",
     "Bolivia (Plurinational State of)", "Bolivia",
     "Türkiye", "Turkey",
     "Syrian Arab Republic", "Syria",
     "Kosovo (under UNSC res. 1244)", "Kosovo",
     "Republic of Moldova", "Moldova",
     "Dem. People's Republic of Korea", "North Korea",
     "Republic of Korea", "South Korea",
     "Viet Nam", "Vietnam",
     "Lao People's Democratic Republic", "Laos",
     "Côte d'Ivoire", "Ivory Coast",
     "Congo", "Republic of Congo",
     "United Republic of Tanzania", "Tanzania",
     "China, Taiwan Province of China", "Taiwan",
     "Eswatini", "Swaziland",
     "State of Palestine", "Palestine"), ncol = 2, byrow = TRUE)

un_wpp |> 
  mutate(Location = fix_names(Location, country_name_replacements)) |>
  filter(Location %in% world$region, Time == '1950')|>
  select(region = Location, u5mr=Q5)|>
  right_join(world) |>
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3)+
  geom_polygon(aes(fill = u5mr)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) +
  labs(fill = "Under 5 mortality rate")

un_wpp |> 
  mutate(Location = fix_names(Location, country_name_replacements)) |>
  filter(Location %in% world$region, Time == '2023')|>
  select(region = Location, u5mr=Q5)|>
  right_join(world) |>
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3)+
  geom_polygon(aes(fill = u5mr)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) +
  labs(fill = "Under 5 mortality rate")


un_wpp |> 
  mutate(Location = fix_names(Location, country_name_replacements)) |>
  filter(Location %in% world$region, Time %in% c("1950", "2023"))|>
  select(region = Location, u5mr=Q5, year=Time)|>
  left_join(world) |>
  ggplot(aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  # coord_map() +
  geom_polygon(aes(fill = u5mr), colour = "#333333", size = 0.2) +
  # scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_fill_gradient2(low = "#ccffdd",
                       mid = "#ccaa44",
                       high = "#bb0000",
                       midpoint = 250) +
  # scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  # scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  labs(fill = "Under 5 mortality rate") +
  facet_wrap(~year)
  # theme(panel.background = element_rect(fill = "#cccccc"),
  #       panel.grid.major = element_line(colour = "grey"),
  #       panel.grid.minor = element_line(colour = "grey"))
