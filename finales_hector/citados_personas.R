library(DBI)
library(tidyverse)
library(RMySQL)
library(highcharter)
library(here)
con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306, timeout = 200, timezone = "America/Mexico_City"
)

citantes <- read_csv(here("Para Jesús", "control_politico_citantes.csv"))

congre <-  tbl(con, "tipo_citacions") %>%
  collect() %>%
  mutate(persona_id=as.numeric(persona_id))


citacions <- read.csv(here("Para Jesús", "control_politicos.csv"), sep="|")

#  ver_personas <- citantes %>%
#                  select(id, control_politico_id, persona_id) %>%
#                  left_join(congre %>% select(congresista_id=id, persona_id),
#                            by="persona_id")
# prueba <- ver_personas %>%
#            filter(is.na(congresista_id))


personas <-  tbl(con, "personas") %>%
  collect() %>%
  mutate(id=as.numeric(id),
         created_at= as.character(created_at),
         updated_at=as.character(updated_at))


personas_citantes <- prueba$persona_id

personas_aver <- filter(personas, id == 5274)


# citados -----------------------------------------------------------------

citados_asiste <- tabla_res[[125]]
citados<- tabla_res[[124]]

cita <- citados %>%
  filter(!is.na(nombres))

# citacions <- read.csv(here("Para Jesús", "control_politicos.csv"), sep="|")%>%
#   select(id_control=id, control_politico_id=agenda_legislativa_actividad_id)

nombre_personas <- personas %>%
  mutate(nombre_final=paste(nombres, apellidos, sep=" ")) %>%
  select(nombre_final) %>%
  unique()
#OJO hay personas repetidas - distintas obs con distinta cantidad de columnas de info
# 91 personas repetidas
nombre_citados <- citados %>%
  filter(!is.na(nombres)) %>%
  select(nombre_final=nombres) %>%
  unique() %>%
  pull(nombre_final)

nombre_personas <- personas %>%
  mutate(nombre_final=paste(nombres, apellidos, sep=" ")) %>%
  select(nombre_final) %>%
  pull(nombre_final)


sum(!nombre_citados %in% nombre_personas)



# 284 citados repetidos
suma <- bind_rows(nombre_personas, nombre_citados)

suma <- suma %>%
  count(nombre_final)

personas_citados <- suma %>%
  filter(n>1) %>%
  select(nombres=nombre_final)

personas_citados <- suma %>%
  filter(n>1)
# Hay 69 registros en personas y en citados
# Total base personas 8,221
# Total base citados - sin NAs = 2, 836
# Personas distintas en el bind_row 10, 613

para_personas <- citados %>%
  filter(!is.na(nombres)) %>%
  unique() %>%
  anti_join(personas_citados)

# Desmadre, hablar con Gerardo, revisar caso de Alfonso Prada Gil -- Revisar en citados y
# en personas. Sale N veces.

# para_personas_final <- para_personas %>%
#   group_by(nombres) %>%
#   mutate(id_nuevo=cur_group_id()) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(-id,-entidad_id, -cargo_id, -old_id, -imagen) %>%
#   mutate(apellidos=NA, fechaNacimiento=NA, municipio_id_nacimiento=NA,
#          profesion_id=NA, genero_id=NA, fecha_fallecimiento=NA,
#          perfil_educativo=NA, grado_estudio_id=NA,activo=NA,
#          usercreated=NA, usermodifed=NA) %>%
#   select(id=id_nuevo, nombres, apellidos:usermodifed,
#          created_at, updated_at=edited_at) %>%
#   mutate(id=id+14660)
#
#
#
# personas <- bind_rows(personas, para_personas_final)
#
# write_excel_csv(personas, "Para Jesús/personas.csv")
# Preparar trayectoria ----------------------------------------------------

para_trayectoria <- citados %>%
  filter(!is.na(nombres)) %>%
  unique() %>%
  anti_join(personas_citados) %>%
  group_by(nombres) %>%
  mutate(id_nuevo=cur_group_id()) %>%
  ungroup() %>%
  select(id=id_nuevo, nombres, entidad_id, cargo_id, imagen, created_at,
         updated_at=edited_at)


# control_citados ---------------------------------------------------------
control_politico_citados <- read_csv("Para Jesús/control_politico_citados.csv")
citados <- tabla_res[[124]]
asiste_citados <- tabla_res[[125]]

para_personas <- citados %>%
  filter(!is.na(nombres)) %>%
  unique() %>%
  anti_join(personas_citados)

# Desmadre, hablar con Gerardo, revisar caso de Alfonso Prada Gil -- Revisar en citados y
# en personas. Sale N veces.

para_citados <- para_personas %>%
  group_by(nombres) %>%
  mutate(id_nuevo=cur_group_id()) %>%
  # slice(1) %>%
  ungroup() %>%
  rename("id_viejo"="id") %>%
  select(id_viejo, id_nuevo) %>%
  mutate(id_nuevo=id_nuevo+14660)


citados_viejos <- citados %>%
  filter(!is.na(nombres)) %>%
  unique()

para_citados_final <- left_join(citados_viejos, para_citados, by=c("id"="id_viejo"))


# NUEVO  ------------------------------------------------------------------

control_citados <- tabla_res[[125]] %>%
  mutate(persona_id=citado_id+14657,
         asistencia_id=if_else(asiste==T, 1,
                               if_else(asiste==F & delega_asistencia==T & excuso==F, 2,
                               if_else(asiste==F & delega_asistencia==F & excuso==T, 3,
                               if_else(asiste==F & delega_asistencia==T & excuso==T, 4, 5))))) %>%
  select(-asiste, -delega_asistencia, -excuso, citado_id) %>%
  rename(control_politico_id=citacion_id) %>%
  mutate(
    tipo_citado=1,
    activo=NA_character_,
    usercreated=NA_character_,
    usermodifed=NA_character_,
    created_at=NA_character_,
    updated_at=NA_character_)

citados <- inner_join(citados, control_citados, by=c("id"="citado_id"))

personas_join <- personas %>%
                 mutate(nombre_final=paste(nombres, apellidos, sep=" ")) %>%
                 select(nombres=nombre_final)

# citados <- tabla_res[[124]] %>% select(id, nombres)

ppc <- citados %>%
  filter(!is.na(nombres)) %>%
  inner_join(personas_join) %>%
  unique()


# Nueva sección -----------------------------------------------------------
control_politico <-  read.csv("Para Jesús/control_politicos.csv", sep="|") %>%
  select(control_id=id, agenda_legislativa_actividad_id)
#Sí están
citados_nueva <- tabla_res[[124]] %>%
           filter(!is.na(nombres),
                  nombres %in% personas_citados$nombre_final) %>%
          mutate(persona_id_1=id+14657) %>%
  # Se junta con personas por nombre
          inner_join(personas %>%
                       mutate(nombres=paste(nombres, apellidos, sep=" ")) %>%
                       select(persona_id_2=id, nombres), by="nombres") %>%
  # Se junta con control politico por id de persona
          select(nombres, persona_id_1, persona_id_2) %>%
          inner_join(control_politico_citados,
                     by=c("persona_id_1"="persona_id")) %>%
  left_join(control_politico, by=c("control_politico_id"="agenda_legislativa_actividad_id")) %>%
  select(-control_politico_id) %>%
  rename(control_politico_id=control_id)  %>%
  select(control_politico_id, persona_id=persona_id_2,
  asistencia_id, tipo_citado, activo, usercreated,
  usermodifed, created_at, updated_at)


#   mutate(id_nueva=paste(tipo_citado, id))
#
# citados_nueva %>%
#   nrow()
#
# citados_nueva%>%
#   pull(id_nueva) %>%
#   n_distinct()


#no están
citados_nueva_nestan <- tabla_res[[124]] %>%
  filter(!is.na(nombres),
         !nombres %in% personas_citados$nombre_final) %>%
  mutate(persona_id_1=id+14657) %>%
  select(nombres, persona_id_1) %>%
  inner_join(control_politico_citados, by=c("persona_id_1"="persona_id")) %>%
  group_by(persona_id_1) %>%
  mutate(persona_id=cur_group_id() + 14660) %>%
  ungroup() %>%
  left_join(control_politico, by=c("control_politico_id"="agenda_legislativa_actividad_id")) %>%
  select(-control_politico_id) %>%
  rename(control_politico_id=control_id) %>%
  select(control_politico_id, persona_id,
         asistencia_id, tipo_citado, activo, usercreated,
         usermodifed, created_at, updated_at)




citados_final <- bind_rows(citados_nueva, citados_nueva_nestan) %>%
                 arrange(desc(control_politico_id)) %>%
                 mutate(id=seq(1:nrow(.))) %>%
                 select(id, control_politico_id:updated_at)

write_excel_csv(citados_final, "Para Jesús/control_politico_citados.csv")


#  comprobacion -----------------------------------------------------------

citados_nueva_nestan_na <- tabla_res[[124]] %>%
  filter(is.na(nombres)) %>%
  mutate(persona_id_1=id+14657) %>%
  select(nombres, persona_id_1) %>%
  inner_join(control_politico_citados, by=c("persona_id_1"="persona_id")) %>%
  mutate(id_nueva=paste(tipo_citado, id)) %>%
  pull(id_nueva) %>%
  n_distinct()


control_politico_citados %>%
  count(id, tipo_citado) %>%
  arrange(desc(n)) %>%
  View()


# Para personas -----------------------------------------------------------

seva_personas<- tabla_res[[124]] %>%
  filter(!is.na(nombres),
         !nombres %in% personas_citados$nombre_final) %>%
  mutate(persona_id_1=id+14657) %>%
  select(nombres, persona_id_1) %>%
  inner_join(control_politico_citados, by=c("persona_id_1"="persona_id")) %>%
  group_by(persona_id_1) %>%
  mutate(persona_id=cur_group_id() + 14660) %>%
  ungroup() %>%
  select(nombres, id=persona_id, activo, usercreated,
         usermodifed, created_at, updated_at) %>%
  unique()



personas_final <- bind_rows(personas, seva_personas)


write_excel_csv(personas_final, "Para Jesús/personas.csv")


write_excel_csv(control_politico_citados, "Bases de datos nuevas/control_politico_citados_resguardo.csv")

ppf <- ppc %>%
  group_by(nombres) %>%
  mutate(id_nuevo=cur_group_id()) %>%
  slice(1) %>%
  ungroup() %>%
  # select(-id,-entidad_id, -cargo_id, -old_id, -imagen) %>%
  mutate(apellidos=NA, fechaNacimiento=NA, municipio_id_nacimiento=NA,
         profesion_id=NA, genero_id=NA, fecha_fallecimiento=NA,
         perfil_educativo=NA, grado_estudio_id=NA,activo=NA,
         usercreated=NA, usermodifed=NA) %>%
  # select(id=id_nuevo, nombres, apellidos:usermodifed,
  #        created_at, updated_at=edited_at) %>%
  mutate(id_nuevo=id_nuevo+14660)

personas_seva <- ppf %>%
                 select(id=id_nuevo,nombres, apellidos:grado_estudio_id,
                        activo:updated_at)

personas_seva <- bind_rows(personas, personas_seva)

write_excel_csv(personas, "Para Jesús/personas.csv")

nueva_persona <- ppf %>%
  select(persona_id, id_nuevo)

control_politico_citados_final_nestan <- control_politico_citados %>%
  anti_join(nueva_persona) %>%
  select(-persona_id) %>%
  rename(persona_id=id_nuevo)%>%
  left_join(control_politico, by=c("control_politico_id"="agenda_legislativa_actividad_id")) %>%
  select(-control_politico_id) %>%
  rename(control_politico_id=control_id) %>%
  select(id, control_politico_id, persona_id, asistencia_id, tipo_citado,
         activo, usercreated:updated_at)


nombre_personas <- personas %>%
  mutate(nombres=paste(nombres, apellidos, sep=" "))

nombre_personas <- left_join(personas_citados,nombre_personas%>% select(nombres, id)) %>%
  rename(id_nuevo=id)

c_siestan_p <- control_citados %>%
  left_join(tabla_res[[124]] %>% select(nombres,id ),
            by=c("citado_id"="id")) %>%
  select(persona_id, nombres) %>%
  left_join(nombre_personas, by="nombres")


control_politico_citados_final_siestan <- control_politico_citados %>%
  inner_join(c_siestan_p) %>%
  select(-persona_id) %>%
  rename(persona_id=id_nuevo)%>%
  left_join(control_politico, by=c("control_politico_id"="agenda_legislativa_actividad_id")) %>%
  select(-control_politico_id) %>%
  rename(control_politico_id=control_id) %>%
  select(id, control_politico_id, persona_id, asistencia_id, tipo_citado,
         activo, usercreated:updated_at)

gilprada <- filter(control_politico_citados_final_siestan, persona_id==1894 | persona_id==6736)



# Invitados ---------------------------------------------------------------

invi <- tabla_res[[127]] %>%
  mutate(persona_id=citado_id+14657,
         asistencia_id=if_else(asiste==T, 1,
                       if_else(asiste==F & delega_asistencia==T & excuso==F, 2,
                       if_else(asiste==F & delega_asistencia==F & excuso==T, 3,
                       if_else(asiste==F & delega_asistencia==T & excuso==T, 4, 5))))) %>%
  select(-asiste, -delega_asistencia, -excuso) %>%
  rename(control_politico_id=citacion_id) %>%
  mutate(tipo_citado=0,
         activo=NA_character_,
         usercreated=NA_character_,
         usermodifed=NA_character_,
         created_at=NA_character_,
         updated_at=NA_character_)
citados <- tabla_res[[124]] %>% select(id, nombres)
citados_invitados <- inner_join(citados, invi, by=c("id"="citado_id")) %>%
                     filter(!is.na(nombres))

nombre_personas <- personas %>%
  mutate(nombres=paste(nombres, apellidos, sep=" "))

nombre_personas <- left_join(citados_invitados,nombre_personas%>% select(nombres, id)) %>%
  rename(id_nuevo=id) %>%
  filter(!is.na(nombres)) %>%
  select(nombres,id_nuevo) %>%
  unique()


citados_invitados_siestan <- citados_invitados %>%
                             filter(!is.na(nombres)) %>%
                             inner_join(nombre_personas, by="nombres") %>%
                             unique()




# Personas vieja
# Citados sin nombre y agarrar sus id, convertir a id de personas + 14657

citados_prueba <- tabla_res[[124]] %>%
           # select(id, nombres) %>%
           filter(is.na(nombres)) %>%
  #+14657
           mutate(id_nuevo=id) %>%
           pull(id_nuevo)

prueba_na <- control_politico_citados %>%
             filter(persona_id %in% citados_prueba)

