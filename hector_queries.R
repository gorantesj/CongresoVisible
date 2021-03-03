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
  count(fecha_radicacion, estado_proyecto_ley_id,camara_id, cuatrienio_id) %>%
  left_join(
    tbl(con, "corporacions") %>%
      #filter(activo == 1) %>%
      select(id, camara = nombre),
    by = c("camara_id" = "id")
  )  %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    cuatrienio_id == 1
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
              select(id, cuatrienio_id),
            by = c("proyecto_ley_id" = "id")
  ) %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    cuatrienio_id == 1
  )%>%
  count(congresista_id) %>%
  show_query()



# Queries de tipo de proyecto

tbl(con,"proyecto_leys") %>%
  count(tipo_proyecto_id, camara_id, cuatrienio_id) %>%
  left_join(
    tbl(con, "corporacions") %>%
      #filter(activo == 1) %>%
      select(id, camara = nombre),
    by = c("camara_id" = "id")
  )  %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    cuatrienio_id == 1
  ) %>%
  arrange(estado_proyecto_ley_id, fecha) %>%
  show_query()







# Queries de proyectos radicados según sexo

tbl(con,"proyecto_ley_autors") %>%
  count(proyecto_ley_id, congresista_id) %>%
  left_join(
    tbl(con, "proyecto_leys") %>%
      #filter(activo == 1) %>%
      select(id, cuatrienio_id, tema_proyecto_ley_id, camara_id),
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
    cuatrienio_id == 1
  )%>%
  show_query()



## Query proporción de mujeres---------------
tbl(con, "congresistas") %>%
  select(cuatrienio_id , genero_id, es_representante_camara, es_senador) %>%
  left_join(tbl(con, "cuatrienios") %>%
            select(id, fecha_inicio),
            by=c("cuatrienio_id"="id")) %>%
  left_join(tbl(con, "generos") %>%
            select(id, nombre),
            by=c("genero_id"="id")) %>%
  filter(es_representante_camara==1) %>%
  group_by(cuatrienio_id,fecha_inicio, genero_id) %>%
  summarise(total=n()) %>%
  mutate(totales=sum(total, na.rm=T),
         porc=round((total/totales)*100, 2)) %>%
  show_query()


#Query mediana de edad

  tbl(con, "congresistas") %>%
  select(cuatrienio_id , fechaNacimiento, es_representante_camara, es_senador) %>%
  left_join(tbl(con, "cuatrienios") %>%
            select(id, fecha_inicio),
            by=c("cuatrienio_id"="id")) %>%
  mutate(nac= round(DATEDIFF(CURDATE(), fechaNacimiento)/365),
         cuatri=round(DATEDIFF(CURDATE(), fecha_inicio)/365),
         edad=cuatri-nac) %>%
  filter(es_representante_camara==1) %>%
  group_by(fecha_inicio) %>%
  summarise(median_edad=median(edad, na.rm=T))%>%
    show_query()


  ## Query partidos con más representación---------------

  tbl(con, "congresistas") %>%
    select(cuatrienio_id, partido_id, es_representante_camara, es_senador) %>%
    left_join(tbl(con, "partidos") %>%
              select(id, nombre),
              by=c("partido_id"="id")) %>%
    left_join(tbl(con, "cuatrienios") %>%
                select(id, fecha_inicio),
              by=c("cuatrienio_id"="id")) %>%
    filter(es_representante_camara==1) %>%
    group_by(cuatrienio_id, fecha_inicio, nombre) %>%
    summarise(total=n())%>%
    show_query()

  ## Query total de citaciones---------------


  tbl(con, "citacions") %>%
    select(fecha_proposicion) %>%
    count(fecha_proposicion) %>%
    show_query()









