# Pacotes ----

library(dismo)

library(tidyverse)

library(writexl)

# Dados ----

## Lista de espécies ----

especies <- c(paste0("Caiman ", c("latirostris", "yacare", "crocodilus")),
              paste0("Paleosuchus ", c("palpebrosus", "trigonatus")),
              "Melanosuchus niger")

especies

## Loop para baixar os dados de ocorrência ----

baixar_correncias <- function(especies){

  ocorrencias <- dismo::gbif(genus = especies |> stringr::word(1),
                             species = especies,
                             removeZeros = TRUE) |>
    dplyr::mutate(Espécie = especies) |>
    dplyr::select(lon, lat, Espécie)

  assign(paste0("ocorrencias_", especies |> stringr::word(2)),
         ocorrencias,
         envir = globalenv())

}

purrr::walk(especies, baixar_correncias)

ls(pattern = "ocorrencias_") |>
  mget(envir = globalenv())

## Loop para exportar os dataframe ----

exportar_ocorrencias <- function(especie, lista_correncias){

  especie |>
    crayon::green() |>
    message()

  lista_correncias |>
    tidyr::drop_na() |>
    dplyr::distinct(lon, lat, .keep_all = TRUE) |>
    writexl::write_xlsx(paste0("ocorrencias_",
                               especie |> stringr::word(2),
                               ".xlsx"))

}

lista_correncias <- ls(pattern = "ocorrencias_") |>
  mget(envir = globalenv())

lista_correncias

purrr::walk2(especies, lista_correncias, exportar_ocorrencias)
