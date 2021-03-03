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


# Proyectos de ley  ------------------------------------------------------------

#Proyectos de ley -> Número de PL en en trámite -> Proyectos en Cámara
#estado_actual_id, camara_id
tbl(con, "proyecto_leys") %>%
  filter(camara_id == 1) %>%
  # filter(activo == 1 && cuatrienio_id == 1) %>%
  count(estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%
  #esta lista puede cambiar
  filter(estado %in% c("Aprobada conciliación en Senado",
                       "Aprobado Primer Debate",
                       "Aprobado Segundo Debate",
                       "Aprobado Tercer Debate",
                       "Aprobado Cuarto Debate",
                       "Archivado por Vencimiento de Términos",
                       "Publicada Ponencia Primer Debate",
                       "Publicada Ponencia Segundo Debate",
                       "Publicada Ponencia Tercer Debate",
                       "Publicada Ponencia Cuarto Debate",
                       "Publicación",
                       "Comisión Accidental",
                       "Corrección de Texto",
                       "Acumulado",
                       "Concepto Institucional")) %>%
  summarise(n=sum(n)) %>%  show_query()

#Proyectos de ley -> Número de PL en en trámite -> Proyectos en Senado
tbl(con, "proyecto_leys") %>%
  filter(camara_id == 2) %>%
  # filter(activo == 1 && cuatrienio_id == 1) %>%
  count(estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%
  #esta lista puede cambiar
  filter(estado %in% c("Aprobada conciliación en Senado",
                       "Aprobado Primer Debate",
                       "Aprobado Segundo Debate",
                       "Aprobado Tercer Debate",
                       "Aprobado Cuarto Debate",
                       "Archivado por Vencimiento de Términos",
                       "Publicada Ponencia Primer Debate",
                       "Publicada Ponencia Segundo Debate",
                       "Publicada Ponencia Tercer Debate",
                       "Publicada Ponencia Cuarto Debate",
                       "Publicación",
                       "Comisión Accidental",
                       "Corrección de Texto",
                       "Acumulado",
                       "Concepto Institucional")) %>%
  summarise(n=sum(n)) %>%  show_query()

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
tbl(con, "proyecto_leys") %>%
  filter(camara_id == 1) %>%
  # filter(activo == 1 && cuatrienio_id == 1) %>%
  count(estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%  show_query()

#Proyectos de ley -> Estado de los Proyectos de Ley -> Senado de la república
tbl(con, "proyecto_leys") %>%
  filter(camara_id == 2) %>%
  # filter(activo == 1 && cuatrienio_id == 1) %>%
  count(estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) )%>%  show_query()


#Proyectos de ley -> Total presentados por ministros
tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1, estado_proyecto_ley_id == 1) %>%
  filter( iniciativa_id == 2) %>%
  tally() %>%
  show_query()

# Valuebox "Resumen de la legislatura en cifras" --------------------------
#Proyectos de ley -> Resumen de la legislatura en cifras -> Audiencias Públicas citadas
tbl(con, "proyecto_leys") %>%
  # filter(activo == 1 && cuatrienio_id == 1) %>%
  count(estado_actual_id) %>%
  left_join(tbl(con, "estado_proyecto_leys") %>%
              select(estado_actual_id = id, estado = nombre) ) %>%
  filter(estado %in% c("Audiencia Pública" ,"Solicitud Audiencia Pública" )) %>%
  select(n) %>% summarise(n = sum(n)) %>%  show_query()

#Proyectos de ley -> Resumen de la legislatura en cifras -> Debates de Control Político citados


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
  filter(estado %in% c("Radicado")) %>%
  select(n)  %>%  show_query()


# Congresistas más activos ------------------------------------------------

#Proyectos de ley -> Congresistas más activos ->
# Con mayor número de autorías de Proyectos de Ley -> Representantes a la Cámara

#Proyectos de ley -> Congresistas más activos ->
# Con mayor número de autorías de Proyectos de Ley -> Senadores

#Proyectos de ley -> Congresistas más activos ->
# Con mayor número de citaciones de Debates de Control Político-> Representantes a la Cámara

#Proyectos de ley -> Congresistas más activos ->
# Con mayor número de citaciones de Debates de Control Político -> Senadores



# Actividad por Partido Políticos -----------------------------------------

#Proyectos de ley -> Actividad por Partido Políticos ->
# Total de citaciones -> Representantes a la Cámara

#Proyectos de ley -> Actividad por Partido Políticos ->
# Total de citaciones -> Senadores

#Proyectos de ley -> Actividad por Partido Políticos ->
# Total de autorías -> Representantes a la Cámara

#Proyectos de ley -> Actividad por Partido Políticos ->
# Total de autorías -> Senadores

#Proyectos de ley -> Actividad por Partido Políticos ->
# Temas recurrentes -> Representantes a la Cámara

#Proyectos de ley -> Actividad por Partido Políticos ->
# Temas recurrentes -> Senadores






