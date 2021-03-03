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


#Proyectos de ley -> Actividad por Partido Políticos -> Total de autorías
tbl(con, "proyecto_ley_autors") %>% filter(activo == 1) %>%  left_join(
  tbl(con, "congresistas") %>% select(id, corporacion_id, partido_id) %>%
    left_join(
      tbl(con, "partidos") %>% filter(activo == 1) %>% select(id, partido = nombre, grupo = posicion_ideologica),
      by = c("partido_id" = "id")
    ) %>%
    left_join(
      tbl(con, "corporacions") %>% filter(activo == 1) %>% select(id, camara = nombre),
      by = c("corporacion_id" = "id")
    ) %>% select(id, partido, grupo, camara) ,
  by = c("congresista_id"="id")
) %>% count(partido) %>%  show_query()

#Gráfica
tbl(con, "proyecto_ley_autors") %>% filter(activo == 1) %>% count(congresista_id) %>% left_join(
  tbl(con, "congresistas") %>% select(id, corporacion_id, partido_id) %>%
    left_join(
      tbl(con, "partidos") %>% filter(activo == 1) %>% select(id, partido = nombre, grupo = posicion_ideologica),
      by = c("partido_id" = "id")
    ) %>%
    left_join(
      tbl(con, "corporacions") %>% filter(activo == 1) %>% select(id, camara = nombre),
      by = c("corporacion_id" = "id")
    ) %>% select(id, partido, grupo, camara) ,
  by = c("congresista_id"="id")
) %>%
  arrange(desc(n)) %>% collect() %>%
  hchart(hcaes(x = partido, y = n, group = camara), type = "bar") %>%
  hc_title(text = "Partidos con más autorías")


