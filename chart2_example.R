# Plot the relationship between total prison population and black prison population
ggplot(data, aes(x = total_prison_pop_per_year, y = black_prison_pop_per_year)) +
  geom_point(alpha=0.7) +
  labs(
    title = "Relationship between Total Prison Population and Black Prison Population (1970-2020)",
    x = "Total Prison Population per Year",
    y = "Black Prison Population per Year",
    color = "Legend"
  ) +
  theme_minimal()