# скрипт для !ПОЛНОЙ ОЧИСТКИ БАЗЫ ДАННЫХ!
library(RSQLite)
library(dplyr)

load("data/insights.RData")

insights_table <- insights_table %>% 
  mutate_all(list(~NA)) %>%
  slice(0)

sim_matrix <- matrix(NA, nrow = nrow(sim_matrix), ncol = ncol(sim_matrix))

save(sim_matrix, insights_table, file = "data/insights.RData")


con <- dbConnect(SQLite(), "data/users.db")

query <- "DELETE FROM users"
dbExecute(con, query)

dbDisconnect(con)

