# Plot the trend over time for different racial groups
setwd("/users/yinyu/INFO201/a4-incarceration-assignment-qianyz6")
data <- read.csv("csv_files/data_final_1.csv")
ggplot(data, aes(x = year)) +
  geom_line(aes(y = black_prison_pop_per_year, color = "Black")) +
  geom_line(aes(y = latinx_prison_pop_per_year, color = "Latinx")) +
  geom_line(aes(y = white_prison_pop_per_year, color = "White")) +
  geom_line(aes(y = aapi_prison_pop_per_year, color = "AAPI")) +
  geom_line(aes(y = native_prison_pop_per_year, color = "Native")) +
  labs(
    title = "Trends in Prison Population by Racial Group (1970-2020)",
    x = "Year",
    y = "Prison Population",
    color = "Racial Groups"
  ) +
  theme_minimal()
