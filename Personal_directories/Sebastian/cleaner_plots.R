library(readxl)
library(ggplot2)
library(dplyr)

un_wpp <- read.csv("../Mikolaj/WPP2024_Demographic_Indicators_Medium.csv.gz")
unigme_total <- read.csv('UNIGME-2023-Total-U5MR-database.csv')
unigme_wealth <- read.csv('UNIGME-2023-Wealth-quintile-U5MR-database.csv',
                          sep = ";", dec = ",")
causes_death <- read.csv('../Mikolaj/CA-CODE-2024-Under5.csv')

world <- map_data("world")

world <- world |>
  filter(
    !(long < -130 & lat > -90 & lat < 50),
    !(long > 130 & lat > 0 & lat < 30),
    region != "Antarctica",
    region != "Fiji",
    region != "Kiribati",
    region != "Mauritius",
    region != "Faroe Islands",
    region != "Comoros",
    !(region == "Ecuador" & !is.na(subregion))
  )

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

map_plot <- un_wpp |> 
  mutate(Location = fix_names(Location, country_name_replacements)) |>
  filter(Location %in% world$region, Time %in% c("1950", "2023"))|>
  select(region = Location, u5mr=Q5, year=Time)|>
  left_join(world) |>
  ggplot(aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  # coord_map() +
  geom_polygon(aes(fill = u5mr), colour = "#333333", size = 0.2) +
  # scale_fill_distiller(palette = "PuBuGn", direction = 1) +
  # scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_fill_gradient2(low = "#ccffdd",
                       mid = "#ccaa44",
                       high = "#bb0000",
                       midpoint = 250) +
  # scale_fill_distiller(palette = "YlOrBr", direction = 1) +
  # scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  labs(fill = "Under 5 mortality rate") +
  facet_wrap(~year, ncol = 1) +
  labs( x = NULL,
        y = NULL) +
  theme_void() +
  theme(
    legend.text = element_text(size=5),
    legend.title = element_text(size=7),
    legend.key.size = unit(0.4, 'cm'),
    legend.position = "inside",
    legend.position.inside = c(0.18, 0.55),
    strip.text = element_text(size = 14,
                              face = "bold")
  )

map_plot

ggsave("mapa_porownawcza_1950_2023_u5mr.png",
       map_plot,
       height = 1656,
       width = 2880,
       units = "px",
       bg = "transparent")




# 
# x <- un_wpp |>
#   filter(Time == 1950) |>
#   select(Location, u5mr1950 = Q5)
# 
# un_wpp |>
#   filter(Time == 2023) |>
#   select(Location, u5mr2023 = Q5) |>
#   left_join(x) |>
#   filter(u5mr2023 > u5mr1950) |>
#   View()


#Demographic and Health Survey

unigme_wealth_processed <- unigme_wealth |>
  rename(survey_name = Series.Name,
         country = Country.Name,
         year = Series.Year,
         reference_date = Reference.Date,
         wealth_quintile = Wealth.Quintile,
         u5mr = Estimates) |>
  mutate(wealth_quintile = factor(wealth_quintile))

uw_dem <- unigme_wealth_processed |> 
  filter(survey_name == 'Demographic and Health Survey') |> 
  group_by(country) |> 
  slice_max(n= 1,order_by=tibble(year,reference_date))


# Moim zdaniem ten wykres ma lepsze dane
violin_wealth <- ggplot(uw_dem, aes(wealth_quintile, u5mr,
                   colour = wealth_quintile,
                   fill = wealth_quintile)) + # tylko Afryka: |> filter(country %in% am_countries$Country_ISO)
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_colour_brewer(palette = "Set1") +
  labs(y = "Deaths/1000 births",
       x = "Wealth quantile") +
  theme_minimal() +
  theme(legend.position = "none",
        rect = element_rect(fill = "transparent"),
        panel.grid = element_line(colour = "black"))

ggsave("violin_wealth_u5mr.png",
       violin_wealth,
       height = 1656,
       width = 2880,
       units = "px",
       bg = "transparent")
    # geom_dotplot(binwidth = "y",
  #              stackdir = "center",
  #              dotsize = 1)
  # stat_summary()


  