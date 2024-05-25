library(dplyr)
library(rlang)
library(knitr)
library(kableExtra)
setwd("/users/yinyu/INFO201/a4-incarceration-assignment-qianyz6")
data <- read.csv("csv_files/us-prison-pop.csv")
# Define a function using NSE
summary_table_final <- function(data, column_name, year_column) {
  col_sym <- sym(column_name)
  year_sym <- sym(year_column)
  new_col_name <- paste0(column_name, "_per_year")
  new_col_summary <- paste0(column_name, "_summary")

  summary_data <- data %>%
    select(!!col_sym, !!year_sym) %>%
    mutate(!!col_sym := replace_na(!!col_sym, 0)) %>%
    group_by(!!year_sym) %>%
    summarise(!!new_col_name := sum(!!col_sym)) %>%
    ungroup() %>%
    select(!!sym(new_col_name))

  summary_info <- summary_data %>%
    summarise(
      mean = mean(!!sym(new_col_name)),
      sd = sd(!!sym(new_col_name)),
      min = min((!!sym(new_col_name))[!!sym(new_col_name) != 0]),
      median = median(!!sym(new_col_name)),
      max = max(!!sym(new_col_name))
    )

  summary_table <- summary_info %>%
    pivot_longer(cols = everything(), names_to = "values", values_to = new_col_summary)
  return(summary_table)
}

data_list <- list()

column_names <- c(
  "total_prison_pop", "female_prison_pop", "male_prison_pop",
  "aapi_prison_pop", "black_prison_pop", "latinx_prison_pop",
  "native_prison_pop", "white_prison_pop"
)
for (i in column_names) {
  data_list <- append(data_list, list(summary_table_final(data, i, "year")))
}

data_final <- data_list[[1]]

# Use a for loop to join all data frames
for (i in 2:length(data_list)) {
  data_final <- left_join(data_final, data_list[[i]], by = "values")
}

