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
  port = 3306
)


# Proyectos de ley  ------------------------------------------------------------
#Sgrupar por camara
#Falta corporaciónid
#Proyectos de ley -> Número de PL en en trámite -> Proyectos en Cámara
#estado_actual_id, camara_id

tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1) %>%
  count(camara_id, estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%
  left_join(tbl(con, "corporacions") %>%
              select(camara_id = id, corporacion = nombre), by = "camara_id" ) %>%
  filter(estado %in% c(  "Acumulado",
                         "Aprobada Conciliación"   ,
                         "Aprobada conciliación en Cámara" ,
                         "Aprobada conciliación en Senado"  ,
                         "Aprobada Objeción"   ,
                         "Aprobado Cuarto Debate"  ,
                         "Aprobado Informe de Objeciones"  ,
                         "Aprobado Octavo Debate" ,
                         "Aprobado Primer Debate"  ,
                         "Aprobado Primer y Tercer Debate" ,
                         "Aprobado Quinto Debate"   ,
                         "Aprobado Segundo Debate"  ,
                         "Aprobado Séptimo Debate"   ,
                         "Aprobado Sexto Debate" ,
                         "Aprobado Tercer Debate" ,
                         "Audiencia Pública"   ,
                         "Comisión Accidental" ,
                         "Concepto Institucional" ,
                         "Corrección de Texto"  ,
                         "Devuelto al Congreso",
                         "En Conciliación" ,
                         "Enviado a  la Corte para Control",
                         "Enviado a Comisión para Primer Debate" ,
                         "Informe Legislativo a las Objeciones del Ejecutivo",
                         "Objeción Parcial del Ejecutivo" ,
                         "Objeción Total del Ejecutivo" ,
                         "Objeciones Presidenciales",
                         "Publicación"  ,
                         "Publicada Ponencia Cuarto Debate"   ,
                         "Publicada Ponencia Octavo Debate"  ,
                         "Publicada Ponencia Primer Debate"  ,
                         "Publicada ponencia primer y tercer debate"   ,
                         "Publicada Ponencia Quinto Debate" ,
                         "Publicada Ponencia Segundo Debate" ,
                         "Publicada Ponencia Séptimo Debate"  ,
                         "Publicada Ponencia Sexto Debate"      ,
                         "Publicada Ponencia Tercer Debate"      ,
                         "Radicado"       ,
                         "Revision Corte Constitucional",
                         "Solicitud Audiencia Pública" ,
                         "Texto Unificado" )) %>%
  group_by(corporacion) %>%
  summarise(n=sum(n)) %>% arrange(desc(n)) %>% show_query()


#Proyectos de ley -> Origen de la iniciativa
tbl(con, "proyecto_leys") %>%
  left_join(tbl(con, "iniciativas") %>%
              select(iniciativa_id= id, iniciativa = nombre) ) %>%
  count(iniciativa) %>%  arrange(desc(n)) %>%
  filter(iniciativa %in%c( "Legislativa ", "Gubernamental", "Mixta", "Otras entidades ")) %>%
  show_query()


#Filtro de lislatura
#Señalar la legislatura 1, por ejemplo, si esa contiene información
#Proyectos de ley -> Temas recurrentes
tbl(con, "proyecto_leys") %>%
  count(tema_principal_id) %>%
  left_join( tbl(con, "temas") %>%  select(tema_principal_id = id, tema = nombre)) %>%
  arrange(desc(n)) %>%  head(10) %>%  show_query()

#Proyectos de ley -> Estado de los Proyectos de Ley ->
tbl(con, "proyecto_leys") %>%
  count(camara_id, estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%
  left_join(tbl(con, "corporacions") %>%
              select(camara_id = id, corporacion = nombre), by = "camara_id" ) %>%
  show_query()


#Proyectos de ley -> Total presentados por ministros
tbl(con, "proyecto_leys") %>%
  # filter(legislatura_id ==24) %>%
  left_join(tbl(con, "iniciativas") %>%
              select(iniciativa_id= id, iniciativa = nombre) ) %>%
  mutate(iniciativa2  = case_when(iniciativa %in% c("Gubernamental", "Mixta")~ "Ministros",
                                  T~"Otros")) %>%
  count(iniciativa2, iniciativa) %>%  show_query()

# Valuebox "Resumen de la legislatura en cifras" --------------------------
#Proyectos de ley -> Resumen de la legislatura en cifras -> Audiencias Públicas citadas
tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1) %>%
  count(estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%
  filter(estado %in% c("Audiencia Pública" ,"Solicitud Audiencia Pública" )) %>%
  select(n) %>% summarise(n = sum(n)) %>%  show_query()

#Proyectos de ley -> Resumen de la legislatura en cifras ->
# Debates de Control Político citados


#Proyectos de ley -> Resumen de la legislatura en cifras -> Sentencias emitidas
tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1) %>%
  count(estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%
  filter(estado %in% c("Declarado Exequible Parcial", "Declarado Exequible Total",
                       "Declarado Inexequible Parcial", "Declarado Inexequible Total")) %>%
  select(n) %>% summarise(n = sum(n)) %>%  show_query()

#Proyectos de ley -> Resumen de la legislatura en cifras -> Objeciones presentadas
tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1) %>%
  count(estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%
  filter(estado %in% c("Objeción Parcial del Ejecutivo",
                       "Objeción Total del Ejecutivo", "Objeciones Presidenciales",
                       "Objetado Por Presidencia")) %>%
  select(n) %>%  summarise(n = sum(n))  %>%  show_query()

#Proyectos de ley -> Resumen de la legislatura en cifras -> Proyectos de Ley radicados
tbl(con, "proyecto_leys") %>%
  anti_join(tbl(con, "proyecto_ley_publicacions") %>%  select(-id),
            by = c("id" = "proyecto_ley_id") ) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%
  filter(estado %in% c("Radicado")) %>% count() %>%
  select(n)  %>%  show_query()


# Congresistas más activos ------------------------------------------------

#Proyectos de ley -> Congresistas más activos ->
# Con mayor número de autorías de Proyectos de Ley -> Representantes a la Cámara
tbl(con, "proyecto_ley_autors") %>%
  left_join(tbl(con, "congresistas") %>% select(congresista_id =id,
                                                congresista = nombre,
                                                corporacion_id),
            "congresista_id") %>%
  left_join( tbl(con, "corporacions")  %>% select(id, corporacion = nombre),
             by = c("corporacion_id" = "id")  ) %>%
  count(corporacion_id, congresista) %>%  arrange(desc(n)) %>%
  show_query()


#Proyectos de ley -> Congresistas más activos ->
# Con mayor número de autorías de Proyectos de Ley -> Senadores
tbl(con, "proyecto_ley_autors") %>%
  left_join(tbl(con, "congresistas") %>% select(autor_id =id,
                                                congresista = nombre,
                                                corporacion_id),
            "autor_id") %>%
  left_join( tbl(con, "corporacions")  %>% select(id, corporacion = nombre),
             by = c("corporacion_id" = "id")  ) %>%
  count(corporacion, autor_id) %>%  arrange(desc(n)) %>% head(10) %>%
  show_query()


#Proyectos de ley -> Congresistas más activos ->
# Con mayor número de citaciones de Debates de Control Político -> Senadores
tbl(con, "citacions")

tbl(con, "citados")
tbl(con, "control_politico_citados")
tbl(con, "control_politicos")




# Actividad por Partido Políticos -----------------------------------------

#Proyectos de ley -> Actividad por Partido Políticos ->
# Total de citaciones -> Representantes a la Cámara

#Proyectos de ley -> Actividad por Partido Políticos ->
# Total de citaciones -> Senadores

#Proyectos de ley -> Actividad por Partido Políticos ->
# Total de autorías -> Representantes a la Cámara
tbl(con, "proyecto_ley_autors")%>%
  left_join(tbl(con, "congresistas") %>% select(congresista_id =id,
                                                congresista = nombre,
                                                corporacion_id, partido_id),
            "congresista_id") %>%
  filter(corporacion_id == 1) %>%
  count(partido_id) %>%  show_query()


#Proyectos de ley -> Actividad por Partido Políticos ->
# Total de autorías -> Senadores
tbl(con, "proyecto_ley_autors")%>%
  left_join(tbl(con, "congresistas") %>% select(congresista_id =id,
                                                congresista = nombre,
                                                corporacion_id, partido_id),
            "congresista_id") %>%
  filter(corporacion_id == 2) %>%
  count(partido_id) %>%  show_query()


#Proyectos de ley -> Actividad por Partido Políticos ->
# Temas recurrentes -> Representantes a la Cámara
tbl(con,"proyecto_ley_autors") %>%
  count(proyecto_ley_id, congresista_id) %>%
  left_join(
    tbl(con, "proyecto_leys") %>%
      #filter(activo == 1) %>%
      select(id, cuatrienio_id, tema_proyecto_ley_id),
    by = c("proyecto_ley_id" = "id")
  )  %>%
  left_join(
    tbl(con, "congresistas") %>%
      select(id, partido_politico_id, corporacion_id),
    by = c("congresista_id" = "id")
  ) %>%
  left_join(
    tbl(con, "corporacions") %>%
      select(id, camara = nombre),
    by = c("corporacion_id" = "id")
  ) %>%
  left_join(
    tbl(con, "partidos") %>%
      select(id, partido = nombre),
    by = c("partido_politico_id" = "id")
  ) %>%
  left_join(
    tbl(con, "tema_proyecto_leys") %>%
      select(id, tema = nombre),
    by = c("tema_proyecto_ley_id" = "id")
  ) %>%
  filter(corporacion_id == 1) %>%
  count(partido, tema) %>%
  show_query()

#Proyectos de ley -> Actividad por Partido Políticos ->
# Temas recurrentes -> Senadores

tbl(con,"proyecto_ley_autors") %>%
  count(proyecto_ley_id, congresista_id) %>%
  left_join(
    tbl(con, "proyecto_leys") %>%
      #filter(activo == 1) %>%
      select(id, cuatrienio_id, tema_proyecto_ley_id),
    by = c("proyecto_ley_id" = "id")
  )  %>%
  left_join(
    tbl(con, "congresistas") %>%
      select(id, partido_politico_id, corporacion_id),
    by = c("congresista_id" = "id")
  ) %>%
  left_join(
    tbl(con, "corporacions") %>%
      select(id, camara = nombre),
    by = c("corporacion_id" = "id")
  ) %>%
  left_join(
    tbl(con, "partidos") %>%
      select(id, partido = nombre),
    by = c("partido_politico_id" = "id")
  ) %>%
  left_join(
    tbl(con, "tema_proyecto_leys") %>%
      select(id, tema = nombre),
    by = c("tema_proyecto_ley_id" = "id")
  ) %>%
  filter(corporacion_id ==2) %>%
  count(partido, tema) %>%
  show_query()






