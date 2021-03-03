library(DBI)
library(tidyverse)
library(RMySQL)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306
)


dbListTables(con)

#Congreso hoy -> Proyectos de ley -> Total de ponencias
tbl(con, "proyecto_leys")%>%
  left_join( tbl(con, "legislaturas")%>% select(legislatura_id = id, nombre,
                                                fechaInicio, fechaFin),
             by = "legislatura_id") %>%
  select(proyecto_ley_id =id, iniciativa_id,cuatrienio_id,
         legislatura_id, nombre, fechaInicio, fechaFin) %>%
  left_join(tbl(con, "proyecto_ley_ponentes") %>%
              select(proyecto_ley_id, congresista_id,
                     activo  )) %>%
  filter(activo == 1, legislatura_id == 1) %>%
  count(legislatura_id, iniciativa_id) %>%  show_query()



#Congreso hoy -> Proyectos de ley -> Total de citaciones
tbl(con, "control_politicos") %>%  filter(activo ==1, legislatura_id == 1) %>%
  filter(activo ==1, cuatrienio_id ==1) %>%
  count(legislatura_id) %>%  show_query()

#Congreso hoy -> Proyectos de ley -> Total proyectos desechados

tbl(con, "proyecto_leys")%>%
  left_join( tbl(con, "legislaturas")%>%
               select(legislatura_id = id, nombre,  fechaInicio, fechaFin),
                                          by = "legislatura_id") %>%
  count(iniciativa_id,legislatura_id, proyecto_ley_estado_id ) %>%
  filter(activo ==1, cuatrienio_id ==1) %>%
  # filter(proyecto_ley_estado_id == "desechado") %>%
  %>% show_query()


#Congreso hoy -> Congresistas -> Con mayor número de autorías
tbl(con, "proyecto_ley_autors") %>% filter(activo ==1) %>%
  count(congresista_id) %>%
  arrange(desc(n))%>% %>% show_query()


# Composición -> Congrsistas -> Edad y Sexo
# Gráfica: pirámide
# filtros: cuatrienio, corporación
tbl(con, "congresistas") %>%
  left_join(tbl(con, "corporacions") %>%
              select(corporacion_id = id, corporacion = nombre),
            by = "corporacion_id") %>%
  left_join(tbl(con, "cuatrienios") %>%
              select(cuatrienio_id = id, cuatrienio = nombre),
            by = "cuatrienio_id") %>%
  left_join(tbl(con, "generos") %>%
              select(genero_id = id, genero = nombre),
            by = "genero_id") %>%
  select(id, nombre, corporacion, cuatrienio, genero,fechaNacimiento ) %>%
  # mutate(edad =today()-fechaNacimiento) %>%
  count(cuatrienio, corporacion, genero,fechaNacimiento, fechaNacimiento) %>%
  show_query()



# Composición -> Congresistas -> Años en el cargo
# Gráfica: Barras
# filtros: cuatrienio, corporación
tbl(con, "congresistas") %>%  mutate(congresista_id = id) %>%
  left_join(tbl(con, "corporacions") %>%
              select(corporacion_id = id, corporacion = nombre),
            by = "corporacion_id") %>%
  left_join(tbl(con, "cuatrienios") %>%
              select(cuatrienio_id = id, cuatrienio = nombre),
            by = "cuatrienio_id")%>%
  left_join(tbl(con, "congresista_trayectoria_publicas")%>%
              select(congresista_id , partido_id,  cargo, fecha),
            by = "congresista_id")%>%
  # group_by(nombre, cargo, fecha) %>%
  group_by(nombre) %>% summarise(fecha =min(fecha, na.rm = T)) %>%
  mutate(tiempo = today()- fecha) %>%
  filter(!is.na(fecha)) %>%
  show_query()


# congreso hoy -> Congresistas más activos -> Con mayor número de autorías de Proyectos de Ley
# -> Representantes a la Cámara
tbl(con, "proyecto_ley_autors") %>%  collect()


tbl(con, "congresistas") %>%  mutate(congresista_id = id) %>%
  left_join(tbl(con, "corporacions") %>%
              select(corporacion_id = id, corporacion = nombre),
            by = "corporacion_id")



# congreso hoy -> Congresistas más activos -> Con mayor número de autorías de Proyectos de Ley
# -> Senadores



# sandobox ----------------------------------------------------------------
tbl(con, "corporacions") %>%  collect()
tbl(con, "cuatrienios") %>%  collect()
tbl(con, "legislaturas") %>%  collect()

tbl(con, "generos") %>%  collect()
tbl(con, "grupo_edads") %>%  collect()
tbl(con, "grupo_edad_imagens") %>%  collect()

tbl(con, "congresistas") %>%  collect()
tbl(con, "bananas") %>%  collect()
tbl(con, "congresista_trayectoria_privadas") %>%  collect()


tbl(con, "estado_proyecto_leys") %>% collect() %>%  view()
tbl(con, "control_politicos") %>%  collect()
tbl(con, "estado_control_politicos") %>%  collect()
tbl(con, "res_tipo_citados") %>%  collect()
tbl(con, "tipo_citacions") %>%  collect()

# En esta bd debería estar
tbl(con, "control_politico_citados") %>% count(nombre) %>%  arrange(desc(n))%>% %>%  collect()


tbl(con, "proyecto_ley_ponentes") %>%
  left_join(tbl(con, "corporacion_miembros")%>% select(congresista_id, corporacion_id),
            by = "congresista_id") %>%
  collect()%>% filter(activo ==1, cuatrienio == 1)%>%  nrow()
tbl(con, "corporacions")%>% collect()
tbl(con, "legislaturas")%>% select(legislatura_id = id, nombre, fechaInicio, fechaFin) collect()

tbl(con, "congreso_cargos") collect()
tbl(con, "corporacion_miembros")  %>% select(congresista_id, corporacion_id)%>% collect()

left_join( tbl(con, "legislaturas")%>% select(legislatura_id = id, nombre,
                                              fechaInicio, fechaFin),
           by = "legislatura_id") %>%
  select(proyecto_ley_id =id, iniciativa_id,cuatrienio_id,
         legislatura_id, nombre, fechaInicio, fechaFin) %>%
  left_join(tbl(con, "proyecto_ley_estados") %>%
              select(proyecto_ley_id, estado_proyecto_ley_id, activo),
            by = "proyecto_ley_id" )%>%
  filter(activo ==1, cuatrienio_id ==1)%>%
  count(legislatura_id,iniciativa_id ,estado_proyecto_ley_id)%>% show_query()




