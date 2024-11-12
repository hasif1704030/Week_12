

#Bangladesh Map


library(sf)
library(ggplot2)
library(geodata)
library(RColorBrewer)


bangladesh_divisions <- geodata::gadm(country = "BGD", level = 1, path = tempdir())
bangladesh_divisions <- st_as_sf(bangladesh_divisions)


ggplot(data = bangladesh_divisions) +
  geom_sf(aes(fill = NAME_1), color = "white", lwd = 0.3) + 
  geom_sf_text(aes(label = NAME_1), size = 3, color = "black") +  
  labs(title = "Bangladesh",
       subtitle = "by Division") +
  scale_fill_manual(values = brewer.pal(length(unique(bangladesh_divisions$NAME_1)), "Set2")) +  
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


#Bangladesh location in the context of south asia

library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")

# Define South Asian countries
south_asia_countries <- c("Bangladesh", "India", "Pakistan", "Nepal", "Bhutan", "Sri Lanka", "Maldives", "Afghanistan")

# Filter to include only South Asia
south_asia <- world %>% dplyr::filter(name %in% south_asia_countries)

# Highlight Bangladesh
bangladesh <- south_asia %>% dplyr::filter(name == "Bangladesh")

ggplot(data = south_asia) +
  geom_sf(fill = "lightblue", color = "black") +  # Fill South Asian countries with light grey
  geom_sf(data = bangladesh, fill = "green2", color = "grey") +  # Highlight Bangladesh
  geom_sf_text(aes(label = name), size = 3, color = "black") +  # Add country names
  labs(title = "Bangladesh in the Context of South Asia") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )


#data input

library(dplyr)
library(readr)
library(scico)
library(viridis)

crop_data <- read_csv("C:/Users/hp/Desktop/UTRGV/Fall 2024/BIOL-6398 Data Science_for_Ecologists/Term paper Presentation/Crop_Yield_Changes.csv")

names(crop_data)[names(crop_data) == "Division"] <- "NAME_1"

bangladesh_divisions <- bangladesh_divisions %>%
  left_join(crop_data, by = "NAME_1")


# Kharif Crop Change Map



ggplot(data = bangladesh_divisions) +
  geom_sf(aes(fill = kharif_change), color = "grey") +
  scale_fill_gradient(low = "lightblue", high = "yellow", na.value = "grey80") +  # Purple to green gradient
  labs(title = "Kharif Crops Yield Change by Division (1991-2020)",
       fill = "Kharif crops yield (ton/acre) change") +
  geom_sf_text(aes(label = paste(NAME_1, "\n", round(kharif_change, 3))), 
               size = 2.5, color = "black", fontface = "bold") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


#Rabi crops


ggplot(data = bangladesh_divisions) +
  geom_sf(aes(fill = rabi_change), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "yellow", na.value = "grey80") +  # Purple to green gradient
  labs(title = "Rabi Crops Yield Change by Division (1991-2020)",
       fill = "Rabi crops yield (ton/acre) change") +
  geom_sf_text(aes(label = paste(NAME_1, "\n", round(rabi_change, 3))), 
               size = 2.5, color = "black", fontface = "bold") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

#Rainfed crops


ggplot(data = bangladesh_divisions) +
  geom_sf(aes(fill = rainfed_change), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "yellow", na.value = "grey80") +  # Purple to green gradient
  labs(title = "Rainfed Crops Yield Change by Division (1991-2020)",
       fill = "Rainfed crops yield (ton/acre) change") +
  geom_sf_text(aes(label = paste(NAME_1, "\n", round(rainfed_change, 3))), 
               size = 2.5, color = "black", fontface = "bold") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


#Irrigated crops


ggplot(data = bangladesh_divisions) +
  geom_sf(aes(fill = cash_change), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "yellow", na.value = "grey80") +  
  labs(title = "Irrigated Crops Yield Change by Division (1991-2020)",
       fill = "Irrigated crops yield (ton/acre) change") +
  geom_sf_text(aes(label = paste(NAME_1, "\n", round(cash_change, 3))), 
               size = 2.5, color = "black", fontface = "bold") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


#Line trend for humidity

climate_data <- read_csv("C:/Users/hp/Desktop/UTRGV/Fall 2024/BIOL-6398 Data Science_for_Ecologists/Term paper Presentation/crops cat_10.12.csv")

library(tidyverse)


climate_data$Year <- as.numeric(climate_data$Year)
climate_data$humidity <- as.numeric(climate_data$humidity)

ggplot(climate_data, aes(x = Year, y = humidity, color = Division, group = Division)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.1, alpha = 0.8, span = 0.2) +  
  scale_color_brewer(palette = "Set2") +          
  scale_y_continuous(limits = c(74, 86),         
                     breaks = seq(74, 86, by = 2)) + 
  labs(title = "Humidity Trend (1991-2020) Across Divisions", 
       x = "Year", y = "Humidity (%)") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),                
    legend.position = "right",                         
    plot.title = element_text(hjust = 0.1, face = "bold") 
  )


ggplot(climate_data, aes(x = Year, y = tmean, color = Division, group = Division)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.1, alpha = 0.8, span = 0.2) +  
  scale_color_brewer(palette = "Set1") +          
  labs(title = "Mean Temperature Trend (1991-2020) Across Divisions", 
       x = "Year", y = "Tmean (°C)") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),                
    legend.position = "right",                         
    plot.title = element_text(hjust = 0.1, face = "bold") 
  )

ggplot(climate_data, aes(x = Year, y = mild_hw, color = Division, group = Division)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.1, alpha = 0.8, span = 0.2) +  
  scale_color_brewer(palette = "Set1") +          
  labs(title = "Mild Heat Wave (1991-2020) Across Divisions", 
       x = "Year", y = "Mild Heat Wave (days)") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),                
    legend.position = "right",                         
    plot.title = element_text(hjust = 0.1, face = "bold") 
  )
