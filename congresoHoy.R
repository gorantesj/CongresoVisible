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


#Proyectos de ley -> Número de PL en en trámite -> Proyectos en Senado


#Proyectos de ley -> Origen de la iniciativa -> Legislativa
tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1, estado_proyecto_ley_id == 1) %>%
  filter( iniciativa_id == 1) %>%
  tally() %>%
  show_query()

#Proyectos de ley -> Origen de la iniciativa -> Gubernamental
tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1, estado_proyecto_ley_id == 1) %>%
  filter( iniciativa_id == 2) %>%
  tally() %>%
  show_query()

#Proyectos de ley -> Origen de la iniciativa -> Mixta
tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1, estado_proyecto_ley_id == 1) %>%
  filter( iniciativa_id == 4) %>%
  tally() %>%
  show_query()

#Proyectos de ley -> Origen de la iniciativa -> Otros
tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1, estado_proyecto_ley_id == 1) %>%
  filter( iniciativa_id == 3) %>%
  tally() %>%
  show_query()


#Proyectos de ley -> Temas recurrentes
tbl(con, "proyecto_leys") %>%  count(tema_proyecto_ley_id)  %>%
  show_query()

#Proyectos de ley -> Estado de los Proyectos de Ley -> Cámara de representantes

#Proyectos de ley -> Estado de los Proyectos de Ley -> Senado de la república


#Proyectos de ley -> Total presentados por ministros -> Senado de la república


#Proyectos de ley -> Resumen de la legislatura en cifras -> Audiencias Públicas citadas

#Proyectos de ley -> Resumen de la legislatura en cifras -> Debates de Control Político citados

#Proyectos de ley -> Resumen de la legislatura en cifras -> Sentencias emitidas

#Proyectos de ley -> Resumen de la legislatura en cifras -> Objeciones presentadas

#Proyectos de ley -> Resumen de la legislatura en cifras -> Proyectos de Ley radicados








