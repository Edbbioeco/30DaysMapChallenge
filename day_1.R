# Desafio ----

# points

# Pacotes ----

library(rnaturalearth)

library(tidyverse)

library(dismo)

library(magrittr)

library(readxl)

# Dados ----

## Shapefile dos países ----

### Importando ----

paises <- rnaturalearth::countries110 |>
  dplyr::filter(CONTINENT |> stringr::str_detect("America"))

### Visualizando ----

paises

paises |> dplyr::glimpse()

ggplot() +
  geom_sf(data = paises)

### Lista dos continentes e subregiões ----

paises$SUBUNIT |> unique()

### Filtrando para a América Latina ----

america_latina <- paises |>
  dplyr::filter(SUBREGION %in% c("South America", "Central America", "Caribbean"))

america_latina

ggplot() +
  geom_sf(data = paises, color = "black", fill = "royalblue") +
  geom_sf(data = america_latina, color = "black", fill = "gold")

## Distribuição ----

### Lista de espécies ----

especies <- c(paste0("Caiman ", c("latirostris", "yacare", "crocodilus")),
              paste0("Paleosuchus ", c("palpebrosus", "trigonatus")),
              "Melanosuchus niger")

especies

### Importando ----

importar_ocorrencias <- function(especie, caminho){

  especie |>
    crayon::green() |>
    message()

  df <- readxl::read_xlsx(caminho)

  assign(paste0("ocorrencias_", especie |> stringr::word(2)),
         df,
         envir = globalenv())

  nome_arquivo <- paste0("ocorrencias_", especie |> stringr::word(2))

}

caminho <- list.files(pattern = ".xlsx")

caminho

especies %<>% factor(levels = c("Caiman crocodilus",
                                "Caiman latirostris",
                                "Melanosuchus niger",
                                "Paleosuchus palpebrosus",
                                "Paleosuchus trigonatus",
                                "Caiman yacare"))

especies

purrr::walk2(especies, caminho, importar_ocorrencias)

unido_ocorrencias <- ls(pattern = "ocorrencias_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

unido_ocorrencias

### Visualizando ----

ggplot() +
  geom_sf(data = paises, color = "black", fill = "royalblue") +
  geom_sf(data = america_latina, color = "black", fill = "gold") +
  geom_point(data = unido_ocorrencias, aes(lon, lat), size = 0.1) +
  facet_wrap(~Espécie, ncol = 2)

# Mapa ----

## Criando o shapefile das ocorrências ----

shp_ocorrencias <- unido_ocorrencias |>
  tidyr::drop_na() |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = america_latina |> sf::st_crs()) |>
  sf::st_intersection(america_latina)

shp_ocorrencias

## Mapa ----

ggplot() +
  geom_sf(data = paises,
          aes(fill = "North America"),
          color = "black", linewidth = 0.5) +
  geom_sf(data = america_latina,
          aes(fill = "Latin America"),
          color = "black", linewidth = 0.5) +
  geom_sf(data = shp_ocorrencias,
          aes(fill = "Occurence Point"),
          color = "black",
          shape = 21,
          size = 0.75) +
  coord_sf(xlim = c(-117.1278, -34.72999),
           ylim = c(-55.61183, 32.72083),
           label_graticule = "SWE") +
  facet_wrap(~Espécie) +
  scale_fill_manual(values = c("gray90",
                               "forestgreen",
                               "black"),
                    breaks = c("North America",
                               "Latin America",
                               "Occurence Point")) +
  scale_x_continuous(breaks = seq(-120, -34, 35)) +
  scale_y_continuous(breaks = seq(-55, 30, 20)) +
  labs(fill = NULL,
       color = NULL,
       title = "Caimaninae subfamily distribution along Latin America",
       subtitle = "Source: Global Biodiversity Information Facility (GBIF)") +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.position = "bottom",
        strip.text = element_text(size = 15, color = "black", face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = element_text(size = 15, color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 15,
                                     color = "black", hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapas/map_day1.png", height = 10, width = 12)
