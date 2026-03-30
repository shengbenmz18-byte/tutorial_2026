#NO1
library(ade4)
library(dplyr)

data(doubs)
str(doubs)

doubs$env |> 
  select(dfs, alt, oxy, pH) |>
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) |>
  rename(distance = dfs, oxygen = oxy) |>
  filter(alt > 200) |>
  arrange(desc(alt)) |>
  group_by(oxygen_category) |>
  summarise(mean_alt = mean(alt), 
            mean_pH = mean(pH), .groups = "drop")
  
#NO2
ggplot(data = doubs$env) +
  geom_point(mapping = aes(x = alt, y = oxy))
ggplot(data = doubs$env) +
  geom_point(mapping = aes(x = alt, y = oxy, color = dfs))













