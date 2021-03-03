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


# Congreso hoy ------------------------------------------------------------

#Proyectos de ley -> Número de PL en en trámite -> Proyectos en Cámara
tbl(con, "proyecto_leys") %>%
  filter(activo == 1 && cuatrienio_id == 1) %>%
  mutate(proyecto_ley_id = id) %>%
  select(proyecto_ley_id, numero_camara) %>%
  show_query()

#Proyectos de ley -> Número de PL en en trámite -> Proyectos en Senado
