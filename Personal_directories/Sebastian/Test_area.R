table1 <- read.csv("Personal_directories/Sebastian/UNIGME-2022-SBR-database.csv")
table2 <- read.csv("../Sebastian/UNIGME-2023-Total-U5MR-IMR-and-NMR-database.csv")
table3 <- read.csv("../Sebastian/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_FULL.csv",
                   dec = ",")

library(DBI)
con <- dbConnect(RSQLite::SQLite(), ":memory:")

library(readxl)
library(countries)

to_numeric <- c(
  "Total.Population..as.of.1.January..thousands.", 
  "Total.Population..as.of.1.July..thousands.",
  "Male.Population..as.of.1.July..thousands.", 
  "Female.Population..as.of.1.July..thousands.",
  "Population.Density..as.of.1.July..persons.per.square.km.", 
  "Population.Sex.Ratio..as.of.1.July..males.per.100.females.", 
  "Median.Age..as.of.1.July..years.",
  "Natural.Change..Births.minus.Deaths..thousands.", 
  "Rate.of.Natural.Change..per.1.000.population.",
  "Population.Change..thousands.", 
  "Population.Growth.Rate..percentage.",
  "Population.Annual.Doubling.Time..years.", 
  "Births..thousands.",
  "Births.by.women.aged.15.to.19..thousands.", 
  "Crude.Birth.Rate..births.per.1.000.population.",
  "Total.Fertility.Rate..live.births.per.woman.", 
  "Net.Reproduction.Rate..surviving.daughters.per.woman.",
  "Mean.Age.Childbearing..years.", 
  "Sex.Ratio.at.Birth..males.per.100.female.births.",
  "Total.Deaths..thousands.", 
  "Male.Deaths..thousands.",
  "Female.Deaths..thousands.",
  "Crude.Death.Rate..deaths.per.1.000.population.", 
  "Life.Expectancy.at.Birth..both.sexes..years.",
  "Male.Life.Expectancy.at.Birth..years.", 
  "Female.Life.Expectancy.at.Birth..years.",
  "Life.Expectancy.at.Age.15..both.sexes..years.", 
  "Male.Life.Expectancy.at.Age.15..years.",
  "Female.Life.Expectancy.at.Age.15..years.", 
  "Life.Expectancy.at.Age.65..both.sexes..years.",
  "Male.Life.Expectancy.at.Age.65..years.", 
  "Female.Life.Expectancy.at.Age.65..years.",
  "Life.Expectancy.at.Age.80..both.sexes..years.", 
  "Male.Life.Expectancy.at.Age.80..years.",
  "Female.Life.Expectancy.at.Age.80..years.", 
  "Infant.Deaths..under.age.1..thousands.",
  "Infant.Mortality.Rate..infant.deaths.per.1.000.live.births.", 
  "Live.Births.Surviving.to.Age.1..thousands.",
  "Under.Five.Deaths..under.age.5..thousands.", 
  "Under.Five.Mortality..deaths.under.age.5.per.1.000.live.births.", 
  "Mortality.before.Age.40..both.sexes..deaths.under.age.40.per.1.000.live.births.", 
  "Male.Mortality.before.Age.40..deaths.under.age.40.per.1.000.male.live.births.", 
  "Female.Mortality.before.Age.40..deaths.under.age.40.per.1.000.female.live.births.", 
  "Mortality.before.Age.60..both.sexes..deaths.under.age.60.per.1.000.live.births.", 
  "Male.Mortality.before.Age.60..deaths.under.age.60.per.1.000.male.live.births.", 
  "Female.Mortality.before.Age.60..deaths.under.age.60.per.1.000.female.live.births.", 
  "Mortality.between.Age.15.and.50..both.sexes..deaths.under.age.50.per.1.000.alive.at.age.15.", 
  "Male.Mortality.between.Age.15.and.50..deaths.under.age.50.per.1.000.males.alive.at.age.15.", 
  "Female.Mortality.between.Age.15.and.50..deaths.under.age.50.per.1.000.females.alive.at.age.15.", 
  "Mortality.between.Age.15.and.60..both.sexes..deaths.under.age.60.per.1.000.alive.at.age.15.", 
  "Male.Mortality.between.Age.15.and.60..deaths.under.age.60.per.1.000.males.alive.at.age.15.", 
  "Female.Mortality.between.Age.15.and.60..deaths.under.age.60.per.1.000.females.alive.at.age.15.", 
  "Net.Number.of.Migrants..thousands.",
  "Net.Migration.Rate..per.1.000.population."
)

make_numeric <- function(x) {
  x <- gsub(",", ".", x)
  x <- gsub(" ", "", x)
  as.numeric(x)
}

for (col in to_numeric) {
  table3[col] <- make_numeric(table3[[col]])
}

table3 |>
  filter(Region..subregion..country.or.area.. %in% list_countries()) |>
  select(Region..subregion..country.or.area..,
         Total.Population..as.of.1.January..thousands.,
         Mean.Age.Childbearing..years.) |>
  View()

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
  filter(Location %in% world$region, Time == '2023')|>
  select(region = Location, u5mr=Q5)|>
  right_join(world)-> x

worldplot <-  x |>
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3)+
  geom_polygon(aes(fill = u5mr)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) +
  labs(fill = "Under 5 mortality rate")

worldplot

x |> filter(is.finite(u5mr) == FALSE) |> group_by(region, u5mr) |> summarise() |>
  filter(!region %in% c("Swaziland")) |>
left_join(world) |>
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = c(1))) +
  
x |> filter(is.finite(u5mr) == FALSE) |> group_by(region) |> summarise() |>
View()
