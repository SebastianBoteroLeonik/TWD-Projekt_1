table1 <- read.csv("Personal_directories/Sebastian/UNIGME-2022-SBR-database.csv")
table2 <- read.csv("Personal_directories/Sebastian/UNIGME-2023-Total-U5MR-IMR-and-NMR-database.csv")
library(DBI)
con <- dbConnect(RSQLite::SQLite(), ":memory:")
