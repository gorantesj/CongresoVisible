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

 congre <-  tbl(con, "congresistas") %>%
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

citados <- tabla_res[[124]]

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
                  unique()
# 284 citados repetidos
suma <- bind_rows(nombre_personas, nombre_citados)

suma <- suma %>%
         count(nombre_final)

personas_citados <- suma %>%
                    filter(n>1) %>%
                    select(nombres=nombre_final)
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

para_personas_final <- para_personas %>%
                 group_by(nombres) %>%
                 mutate(id_nuevo=cur_group_id()) %>%
                 slice(1) %>%
                 ungroup() %>%
                 select(-id,-entidad_id, -cargo_id, -old_id, -imagen) %>%
                 mutate(apellidos=NA, fechaNacimiento=NA, municipio_id_nacimiento=NA,
                        profesion_id=NA, genero_id=NA, fecha_fallecimiento=NA,
                        perfil_educativo=NA, grado_estudio_id=NA,activo=NA,
                        usercreated=NA, usermodifed=NA) %>%
                select(id=id_nuevo, nombres, apellidos:usermodifed,
                       created_at, updated_at=edited_at) %>%
                mutate(id=id+14660)



personas <- bind_rows(personas, para_personas_final)

write_excel_csv(personas, "Para Jesús/personas.csv")
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





