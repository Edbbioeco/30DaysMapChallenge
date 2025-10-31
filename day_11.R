# Desafio ----

# Mapa minimalísta

# Pacotes ----

library(geobr)

library(tidyverse)

# Dados ----

## Importando ----

br <- geobr::read_state(year = 2019)

## Visualizando ----

br

ggplot() +
  geom_sf(data = br)

# Mapa ----

ggplot() +
  geom_sf(data = br, color = "black", fill = "gray90") +
  labs(title = "Brazilian states") +
  theme_minimal() +
  theme(axis.text = element_text(color = "black", size = 15),
        plot.title = element_text(size = 15,
                                                 color = "black", hjust = 0.5))

ggsave(filename = "mapas/map_day11.png", height = 10, width = 12)
