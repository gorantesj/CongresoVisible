require(pacman)
p_load(tidyverse, RPostgreSQL, DBI, RMySQL,  RMariaDB)


con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306
)

dbListTables(con)
tbl(con, "proyecto_leys")

# Queries estatus del proyecto por fecha
tbl(con, "proyecto_leys") %>%
  count(fecha_radicacion, estado_proyecto_ley_id,camara_id, legislatura_id) %>%
  left_join(
    tbl(con, "corporacions") %>%
      #filter(activo == 1) %>%
      select(id, camara = nombre),
    by = c("camara_id" = "id")
  )  %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    legislatura_id == 1
  ) %>%
  arrange(estado_proyecto_ley_id, fecha) %>%
  show_query()


# Queries de proyectos de ley presentados por ministros

tbl(con,"proyecto_ley_autors") %>%
  left_join(
    tbl(con, "congresistas") %>%
      select(id, corporacion_id, partido_id) %>%
      left_join(
        tbl(con, "partidos") %>%
          filter(activo == 1) %>%
          select(id),
        by = c("partido_id" = "id")
      ) %>%
      left_join(
        tbl(con, "corporacions") %>%
          filter(activo == 1) %>%
          select(id, camara = nombre),
        by = c("corporacion_id" = "id")
      ) %>%
      select(id,  camara) ,
    by = c("congresista_id"="id")
  ) %>%
  left_join(tbl(con, "proyecto_leys") %>%
              select(id, legislatura_id),
            by = c("proyecto_ley_id" = "id")
  ) %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    legislatura_id == 1
  )%>%
  count(congresista_id) %>%
  show_query()



# Queries de tipo de proyecto

tbl(con,"proyecto_leys") %>%
  count(tipo_proyecto_id, camara_id, legislatura_id) %>%
  left_join(
    tbl(con, "corporacions") %>%
      #filter(activo == 1) %>%
      select(id, camara = nombre),
    by = c("camara_id" = "id")
  )  %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    legislatura_id == 1
  ) %>%
  arrange(estado_proyecto_ley_id, fecha) %>%
  show_query()







# Queries de proyectos radicados según sexo

tbl(con,"proyecto_ley_autors") %>%
  count(proyecto_ley_id, congresista_id) %>%
  left_join(
    tbl(con, "proyecto_leys") %>%
      #filter(activo == 1) %>%
      select(id, legislatura_id, tema_proyecto_ley_id, camara_id),
    by = c("proyecto_ley_id" = "id")
  )  %>%
  left_join(
    tbl(con, "corporacions") %>%
      select(id, camara = nombre),
    by = c("camara_id" = "id")
  ) %>%
  left_join(
    tbl(con, "congresistas") %>%
      select(id, genero_id),
    by = c("congresista_id" = "id")
  ) %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    legislatura_id == 1
  )%>%
  show_query()
