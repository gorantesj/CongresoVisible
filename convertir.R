library(tidyverse)
library(readxl)
# Leer bases de datos
archivos <- list.files("~/Documents/Congreso Visible/",full.names = T)
leer <- safely(read_xlsx)
tablas <- archivos %>% map(~leer(.x))
tablas_res <- tablas %>% transpose() %>% pluck("result")

campos <- read_csv("campos.csv")

# funciones ---------------------------------------------------------------
crear_bases<- function(relaciones, tablas){
  relaciones <- relaciones %>% filter(grupo!="")
  # Tablas nuevas del grupo 1
  tablas_nuevas1 <- relaciones %>%
    filter(grupo==1) %>%
    pull(tabla_gp) %>%
    unique()
  map(tablas_nuevas1,
      ~convertir_grupo1(.x, tablas, relaciones))

  #
}

convertir_grupo1 <- function(tabla_nueva, tablas, relaciones){
  # Filtra las relaciones de la tabla nueva
  relaciones <- relaciones %>%
    filter(tabla_gp==tabla_nueva,
           grupo==1)
  # Separa las relaciones en tablas viejas (usualmente 1)
  tablas_viejas <- relaciones %>% split(.$num)
  # Para cada tabla vieja selecciona los campos viejos
  # Si transformar es igual a 1 le escribe el nombre de campo viejoa con el prefijo
  # transformar
  campos_nuevos <- tablas_viejas %>%
    map(~{
      res <- tablas[[unique(.x$num)]] %>%
          select(.x$campo)
      names(res) <- .x$campo_gp
      names(res)[which(.x$transformar == 1)] <- paste("transformar",
                                                      .x$campo[which(.x$transformar == 1)],
                                                      sep = "_")
      return(res)

    }
      ) %>%
    reduce(bind_cols)
  write_csv(x = campos_nuevos,
            glue::glue("Bases de datos nuevas/{tabla_nueva}.csv"))
}

convertir_grupo2 <- function(tabla_nueva, tablas, relaciones){
  # Filtra las relaciones de la tabla nueva
  relaciones <- relaciones %>%
    filter(tabla_gp==tabla_nueva,
           grupo==2)
  # Separa las relaciones en tablas viejas (usualmente 1)
  tablas_viejas <- relaciones %>% split(.$num)
  # Para cada tabla vieja selecciona los campos viejos
  # Si transformar es igual a 1 le escribe el nombre de campo viejoa con el prefijo
  # transformar
  campos_nuevos <- tablas_viejas %>%
    map(~{
      res <- tablas[[unique(.x$num)]] %>%
        select(.x$campo)
      names(res) <- .x$campo_gp
      names(res)[which(.x$transformar == 1)] <- paste("transformar",
                                                      .x$campo[which(.x$transformar == 1)],
                                                      sep = "_")
      return(res)

    }
    ) %>%
    reduce(bind_cols)
  browser()
  write_csv(x = campos_nuevos,
            glue::glue("Bases de datos nuevas/{tabla_nueva}.csv"))
}


# transformación ----------------------------------------------------------

crear_bases(campos, tablas_res)
convertir_grupo1("proyecto_leys", tablas_res,campos)
convertir_grupo2("proyecto_leys", tablas_res,campos)
