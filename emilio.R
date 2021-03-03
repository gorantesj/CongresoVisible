library(DBI)
library(tidyverse)
library(RMySQL)
library(highcharter)
con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible2",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306, timeout = 200, timezone = "America/Mexico_City"
)

tbl(con, "proyecto_leys") %>% filter(id == 1) %>% select(iniciativa_id) %>% collect()
# este es para la gráfica tal
tbl(con, "proyecto_leys") %>% filter(id == 1) %>% select(iniciativa_id) %>% show_query()

dbListTables(con) %>% grep("part",x = .,value = T)
dbListTables(con) %>% grep("cita",x = .,value = T)
dbListTables(con) %>% grep("aut",x = .,value = T)
dbListTables(con) %>% grep("congresista",x = .,value = T)
dbListTables(con) %>% grep("comision",x = .,value = T)
dbListTables(con) %>% grep("corp",x = .,value = T)
dbListTables(con) %>% grep("proy",x = .,value = T)
dbListTables(con) %>% grep("tema",x = .,value = T)
dbListTables(con) %>% grep("legis",x = .,value = T)
dbListTables(con) %>% grep("cuatri",x = .,value = T)
dbListTables(con) %>% grep("gen",x = .,value = T)

tbl(con, "partidos") %>% view
tbl(con, "congreso_partido_sectores") %>% view
tbl(con, "res_votacion_partidos")
tbl(con, "congreso_congresistadepartidos")
tbl(con, "res_tipo_citados") %>% view

tbl(con, "congresistas")
tbl(con, "comisions") %>% view
tbl(con, "corporacions")
tbl(con, "citacion_citantes")
tbl(con, "legislaturas")



tbl(con, "citacions")
tbl(con, "tema_proyecto_leys") %>% pull(nombre)
tbl(con, "temas")

tbl(con, "proyecto_ley_autors") %>% count(congresista_id) %>% left_join(
  tbl(con, "congresistas") %>% select(id, corporacion_id, partido_id) %>%
    left_join(
      tbl(con, "partidos") %>% select(id, partido = nombre, grupo = posicion_ideologica),
      by = c("partido_id" = "id")
    ) %>%
    left_join(
      tbl(con, "corporacions") %>% select(id, camara = nombre),
      by = c("corporacion_id" = "id")
    ) %>% select(id, partido, grupo, camara) ,
  by = c("congresista_id"="id")
)


# Partidos políticos
tbl(con, "citacion_citantes") %>% count(congresista_id) %>% left_join(
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
  hc_title(text = "Partidos con más citaciones")


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


tbl(con,"proyecto_leys") %>% filter(activo == 1) %>% select(id, tema_proyecto_ley_id) %>%
  full_join(
    tbl(con, "proyecto_ley_autors") %>% filter(activo == 1) %>% select(id,congresista_id) %>% left_join(
      tbl(con, "congresistas") %>% select(id, partido_id) %>%
        left_join(
          tbl(con, "partidos") %>% filter(activo == 1) %>% select(id, partido = nombre, grupo = posicion_ideologica),
          by = c("partido_id" = "id")
        ) %>% select(id, partido) ,
      by = c("congresista_id"="id")
    ), by = c("id" = "proyecto_ley_id")
  ) %>% count(to = tema_proyecto_ley_id, from = partido) %>% collect()

highchart() %>%
  hc_chart(type = 'sankey') %>%
  hc_add_series(
    data = list(
      list(from = 'CD', to = 'Finanzas', weight = 10),
      list(from = 'CD', to = 'Salud', weight = 3),
      list(from = 'CR', to = 'Ecología', weight = 5),
      list(from = 'CR', to = 'Salud', weight = 3),
      list(from = 'CR', to = 'Seguridad', weight = 2),
      list(from = 'AV', to = 'Seguridad', weight = 5),
      list(from = 'AV', to = 'Salud', weight = 1)
    )
  ) %>%
  hc_plotOptions(sankey = list(borderRadius = 7, colorByPoint = T,
                               dataLabels = list(style = list(fontFamily = "Avenir next", fontSize = "16px")))) %>%
  hc_title(text = "Temas frecuentes por partido") %>%
  hc_chart(style = list(fontFamily = "Avenir next"
  )) %>%  hc_add_theme(hc_theme_538())


# Query congresistas ------------------------------------------------------
# sólo falta poner el filtro de legislatura
# no hay variable de legislatura para citaciones

tbl(con, "citacion_citantes") %>% select(id,congresista_id) %>% left_join(
  tbl(con, "congresistas") %>% select(id, nombre, corporacion_id, partido_id) %>%
    left_join(
      tbl(con, "partidos") %>% filter(activo == 1) %>% select(id, grupo = posicion_ideologica),
      by = c("partido_id" = "id")
    ) %>%
    left_join(
      tbl(con, "corporacions") %>% filter(activo == 1) %>% select(id, camara = nombre),
      by = c("corporacion_id" = "id")
    ) %>% select(id, nombre, grupo, camara),
  by = c("congresista_id"="id")
) %>% left_join(
  tbl(con, "citacions") %>% select(id, tema_principal_id) ,
  by = c("citacion_id" = "id")
) %>% filter( # filtro, puede cambiar
  camara == "Cámara de Representantes",
  tema_principal_id == "1"
)

# Queries partido político-----------------------------------------------------------------
#1. partidos con más citaciones
# sólo falta poner el filtro de legislatura
# no hay variable de legislatura para citaciones
tbl(con, "citacion_citantes") %>% count(congresista_id) %>% left_join(
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
) %>% filter( # filtro, puede cambiar
  camara == "Cámara de Representantes",
) %>%
  arrange(desc(n)) %>% show_query()

#2. partidos con más autorías
tbl(con, "proyecto_ley_autors") %>% filter(activo == 1) %>% select(proyecto_ley_id,congresista_id) %>% left_join(
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
) %>% left_join(tbl(con,"proyecto_leys") %>% select(id,legislatura_id), by = c("proyecto_ley_id" = "id")) %>%
  filter( # filtro, puede cambiar
  camara == "Cámara de Representantes",
  legislatura_id == 1
) %>% count(congresista_id) %>%
  arrange(desc(n)) %>% show_query()

#3. temas por partidos

tbl(con,"proyecto_leys") %>% filter(activo == 1) %>% select(id, tema_proyecto_ley_id, legislatura_id) %>%
  full_join(
    tbl(con, "proyecto_ley_autors") %>% filter(activo == 1) %>% select(id,congresista_id) %>% left_join(
      tbl(con, "congresistas") %>% select(id, corporacion_id, partido_id) %>%
        left_join(
          tbl(con, "partidos") %>% filter(activo == 1) %>% select(id, partido = nombre, grupo = posicion_ideologica),
          by = c("partido_id" = "id")
        ) %>%
        left_join(
          tbl(con, "corporacions") %>% filter(activo == 1) %>% select(id, camara = nombre),
          by = c("corporacion_id" = "id")
        ) %>% select(id, partido, camara) ,
      by = c("congresista_id"="id")
    ), by = c("id" = "proyecto_ley_id")
  ) %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    legislatura_id == 1
  ) %>%
  count(to = tema_proyecto_ley_id, from = partido) %>% show_query()


# Composición -------------------------------------------------------------

## Partidos
### Distribución asientos Senadores
tbl(con, "congresistas") %>% filter(activo == 1, es_senador == 1, cuatrienio_id == 1) %>%
  count(partido_id) %>%
  left_join(tbl(con,"partidos") %>% select(id,partido = nombre), by = c("partido_id" = "id")) %>% select(-partido_id) %>%
  show_query()
### Distribución asientos Representantes
tbl(con, "congresistas") %>% filter(activo == 1, es_representante_camara == 1, cuatrienio_id == 1) %>% count(partido_id) %>%
  left_join(tbl(con,"partidos") %>% select(id,partido = nombre), by = c("partido_id" = "id")) %>% select(-partido_id) %>%
  show_query()
### Sexo y Edad

tbl(con, "congresistas") %>% filter(activo == 1, cuatrienio_id == 1) %>%
  # left_join(tbl(con,"cuatrienios") %>% select(id,cuatrienio = nombre)) %>%
  mutate(años = round(DATEDIFF(CURDATE(),fechaNacimiento)/365)) %>%
  count(partido_id,genero_id,años) %>% left_join(tbl(con,"generos") %>% select(id,genero = nombre), by = c("genero_id" = "id")) %>%
  left_join(tbl(con,"partidos") %>% select(id,partido = nombre), by = c("partido_id" = "id")) %>% ungroup %>%
  select(genero,años,partido,n) %>% show_query()

## Congresistas
### Sexo

tbl(con, "congresistas") %>% filter(activo == 1, cuatrienio_id == 1) %>%
  # left_join(tbl(con,"cuatrienios") %>% select(id,cuatrienio = nombre)) %>%
  mutate(años = round(DATEDIFF(CURDATE(),fechaNacimiento)/365)) %>%
  count(años,genero_id) %>% left_join(tbl(con,"generos") %>% select(id,genero = nombre), by = c("genero_id" = "id")) %>%
  ungroup %>% select(genero,años,n) %>% show_query()
