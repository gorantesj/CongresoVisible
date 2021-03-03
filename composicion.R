library(DBI)
library(tidyverse)
library(RMySQL)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible2",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306
)


# Composición ------------------------------------------------------------
# Objetivo: Mostrar la distribución de los partidos y sus miembros dentro del congreso
# Filtro: Legislatura


# Partidos -> Distribución de asientos-Senadores
tbl(con, "congresistas") %>% filter(activo == 1, es_senador == 1, cuatrienio_id == 1) %>%
  count(partido_id) %>%
  left_join(tbl(con,"partidos") %>% select(id,partido = nombre), by = c("partido_id" = "id")) %>% select(-partido_id) %>%
  show_query()

# Partidos -> Distribución de asientos-Cámara de Representantes
tbl(con, "congresistas") %>% filter(activo == 1, es_representante_camara == 1, cuatrienio_id == 1) %>% count(partido_id) %>%
  left_join(tbl(con,"partidos") %>% select(id,partido = nombre), by = c("partido_id" = "id")) %>% select(-partido_id) %>%
  show_query()

# Partidos -> Edad y sexo
tbl(con, "congresistas") %>% filter(activo == 1, cuatrienio_id == 1) %>%
  # left_join(tbl(con,"cuatrienios") %>% select(id,cuatrienio = nombre)) %>%
  mutate(años = round(DATEDIFF(CURDATE(),fechaNacimiento)/365)) %>%
  count(partido_id,genero_id,años) %>% left_join(tbl(con,"generos") %>% select(id,genero = nombre), by = c("genero_id" = "id")) %>%
  left_join(tbl(con,"partidos") %>% select(id,partido = nombre), by = c("partido_id" = "id")) %>% ungroup %>%
  select(genero,años,partido,n) %>%
  show_query()

# Congresistas -> Pirámide Poblacional de Edad y sexo -> Representantes a la Cámara

# Congresistas -> Pirámide Poblacional de Edad y sexo -> Senadores

# Congresistas -> Pirámide Poblacional de Edad y sexo -> Por partido

# Congresistas -> Cuatrienios en el Congreso -> Cuatrienios como Representantes

# Congresistas -> Cuatrienios en el Congreso -> Cuatrienios como Senadores

# Congresistas -> Georepresentación

# Congresistas -> Investigaciones
# "jalar" 3 tipos de investigaciones: silla vacía, renunció  a la curul y los que empiecen por Atención:
# pérdida de investidura.

# Congresistas -> Cantidad de PL presentados por sexo

# Congresistas -> Partidos Políticos

# Congresistas -> Circunscripción

# Congresistas -> Minoría
