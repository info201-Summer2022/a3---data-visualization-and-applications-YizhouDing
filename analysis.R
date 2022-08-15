library(dplyr)
library(tidyverse)
library(maps)
library(ggplot2)

database <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Summary

#US population
country_max_jail_pop <- database %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(county_name)

country_jail_female_ratio <- database %>%
  filter(year == max(year, na.rm = T)) %>%
  mutate(ratio = female_jail_pop / total_jail_pop) %>%
  summarise(final = mean(ratio, na.rm = T)) %>%
  pull(final)

country_jail_male_ratio <- database %>%
  filter(year == max(year, na.rm = T)) %>%
  mutate(ratio = male_jail_pop / total_jail_pop) %>%
  summarise(final = mean(ratio, na.rm = T)) %>%
  pull(final)

country_jail_black_ratio <- database %>%
  filter(year == max(year, na.rm = T)) %>%
  mutate(ratio = black_jail_pop/total_jail_pop) %>%
  summarise(final = mean(ratio, na.rm = T)) %>%
  pull(final)

country_jail_white_ratio <- database %>%
  filter(year == max(year, na.rm = T)) %>%
  mutate(ratio = white_jail_pop/total_jail_pop) %>%
  summarise(final = mean(ratio, na.rm = T)) %>%
  pull(final)

#WA population
WA_max_jail_pop_county <- database %>%
  filter(state == "WA") %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = T)) %>%
  pull(county_name)

WA_jail_female_ratio <- database %>%
  filter(state == "WA") %>%
  filter(county_name == WA_max_jail_pop_county) %>%
  filter(year == max(year, na.rm = T)) %>%
  mutate(ratio = female_jail_pop / total_jail_pop) %>%
  summarise(final = mean(ratio, na.rm = T)) %>%
  pull(final)

WA_jail_male_ratio <- database %>%
  filter(state == "WA") %>%
  filter(county_name == WA_max_jail_pop_county) %>%
  filter(year == max(year, na.rm = T)) %>%
  mutate(ratio = male_jail_pop / total_jail_pop) %>%
  summarise(final = mean(ratio, na.rm = T)) %>%
  pull(final)

WA_jail_black_ratio <- database %>%
  filter(state == "WA") %>%
  filter(county_name == WA_max_jail_pop_county) %>%
  filter(year == max(year, na.rm = T)) %>%
  mutate(ratio = black_jail_pop/total_jail_pop) %>%
  summarise(final = mean(ratio, na.rm = T)) %>%
  pull(final)

WA_jail_white_ratio <- database %>%
  filter(state == "WA") %>%
  filter(county_name == WA_max_jail_pop_county) %>%
  filter(year == max(year, na.rm = T)) %>%
  mutate(ratio = white_jail_pop/total_jail_pop) %>%
  summarise(final = mean(ratio, na.rm = T)) %>%
  pull(final)

#Charts 

#Trends of population (Scatter)
scatter1 <- database %>%
  filter(state == "WA") %>%
  group_by(year) %>%
  summarise(WA_jail_pop = sum(total_jail_pop)) %>%
  tail(-20)

scatter2 <- database %>%
  filter(state == "WA") %>%
  group_by(year) %>%
  summarise(WA_male_pop = sum(male_jail_pop)) %>%
  tail(-23)

scatter3 <- database %>%
  filter(state == "WA") %>%
  group_by(year) %>%
  summarise(WA_female_pop = sum(female_jail_pop)) %>%
  tail(-23)
  
scatter_plot <- ggplot()+
  geom_point(data = scatter1, aes(x = year, y = WA_jail_pop), color = "Purple") +
  geom_point(data = scatter2, aes(x = year, y = WA_male_pop), color = "Blue") +
  geom_point(data = scatter3, aes(x = year, y = WA_female_pop), color = "Red") +
  geom_smooth(method = 'loess', formula = 'y ~ x', aes(x = year, y = WA_jail_pop, color = "Whole population in jail"), scatter1)+
  geom_smooth(method = 'loess', formula = 'y ~ x', aes(x = year, y = WA_male_pop, color = "Male in jail"), scatter2 )+
  geom_smooth(method = 'loess', formula = 'y ~ x', aes(x = year, y = WA_female_pop, color = "Female in jail"), scatter3 )+
  labs(x = "Time in Year",
       y = "Jail Population",
       title = "Trend of total jail population in Washington"
  )

print(scatter_plot)

#The percentage of each region

pie_data <- database %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(total_jail_pop > 1000) %>%
  group_by(region)

pie <- ggplot(pie_data, mapping = aes(x = "", y = "", fill = region)) +
  geom_bar(width = 1, stat = "identity") +
  ggtitle("The percentage of each region") +
  coord_polar("y", start = 0)

print(pie)

#Maps

data_for_map <- database %>%
  filter(year == max(year, na.rm = T))

WA_map <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- WA_map %>%
  left_join(data_for_map, by = "fips") %>%
  filter(state == "WA")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = female_jail_pop),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#F8766D", high = "Black") +
  labs(title = paste0("Number of female population in jail"),
       x = "", y = "", fill = "female population in jail") +
  blank_theme

print(map)

