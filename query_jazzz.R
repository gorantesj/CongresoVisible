library(dbplyr)
library(tidyverse)
library(DBI)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible2",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306
)

###################################################################
#
# Composición >> Secciones >> Congresistas
#
##################################################################

# Congresistas -> Pirámide Poblacional de Edad y sexo -> Representantes a la Cámara
tbl(con, "congresistas") %>% filter(activo == 1, es_representante_camara == 1, cuatrienio_id == 1) %>%
  mutate(años = round(DATEDIFF(CURDATE(),fechaNacimiento)/365))  %>%
  count(genero_id,años) %>% left_join(tbl(con,"generos") %>%
  select(id, genero = nombre), by = c("genero_id" = "id")) %>%
  ungroup %>% select(genero, años, n) %>%
  # collect() %>%
  ggplot(aes(x = años, fill = genero, y = ifelse(sexo=="Masculino",-años, años)))+
  geom_bar(stat="identity") +
  coord_flip()+
  theme_minimal()

# Congresistas -> Pirámide Poblacional de Edad y sexo -> Senadores
tbl(con, "congresistas") %>% filter(activo == 1, es_senador == 1, cuatrienio_id == 1) %>%
  mutate(años = round(DATEDIFF(CURDATE(),fechaNacimiento)/365))  %>%
  count(genero_id,años) %>% left_join(tbl(con,"generos") %>%
  select(id, genero = nombre), by = c("genero_id" = "id")) %>%
  ungroup %>% select(genero, años, n) %>%
  show_query()
  # collect() %>%
  # ggplot(aes(x = años, fill = genero, y = ifelse(sexo=="Masculino",-n, n)))+
  # geom_bar(stat="identity") +
  # coord_flip()+
  # theme_minimal()

# Congresistas -> Pirámide Poblacional de Edad y sexo -> Por partido
tbl(con, "congresistas") %>% filter(activo == 1, cuatrienio_id == 1) %>%
  # left_join(tbl(con,"cuatrienios") %>% select(id,cuatrienio = nombre)) %>%
  mutate(años = round(DATEDIFF(CURDATE(),fechaNacimiento)/365)) %>%
  count(partido_id,genero_id,años) %>% left_join(tbl(con,"generos") %>%
  select(id,genero = nombre), by = c("genero_id" = "id")) %>%
  left_join(tbl(con,"partidos") %>% select(id, partido = nombre), by = c("partido_id" = "id")) %>% ungroup %>%
  select(genero, años, partido,n) %>%
  show_query()
  # collect() %>%
  # ggplot(aes(x = años, fill = genero, y = ifelse(sexo=="Masculino",-n, n)))+
  # geom_bar(stat="identity") +
  # coord_flip()+
  # theme_minimal()

# Congresistas -> Cuatrienios en el Congreso -> Cuatrienios como Representantes
tbl(con, "congresistas") %>% filter(activo == 1,  es_representante_camara == 1) %>%
  select(congresista = nombre, cuatrienio_id) %>%
  left_join(tbl(con, "cuatrienios") %>% select(cuatrienio_id = id, cuatrienio = nombre), by = "cuatrienio_id") %>%
  select(congresista, cuatrienio) %>% show_query()

# Congresistas -> Cuatrienios en el Congreso -> Cuatrienios como Senadores
tbl(con, "congresistas") %>% filter(activo == 1,  es_senador == 1) %>%
  select(congresista = nombre, cuatrienio_id) %>%
  left_join(tbl(con, "cuatrienios") %>% select(cuatrienio_id = id, cuatrienio = nombre), by = "cuatrienio_id") %>%
  select(congresista, cuatrienio) %>% show_query()

# Congresistas -> Georepresentación
tbl(con, "congresistas") %>% filter(activo == 1) %>%
select(congresista_id = id, congresista = nombre) %>%
left_join(
tbl(con, "res_campanias") %>%
select(congresista_id, departamento_id), by = "congresista_id") %>%
left_join(
tbl(con, "departamentos") %>% filter(activo == 1) %>%
select(departamento_id = id, departamento = nombre),
by = "departamento_id") %>%
show_query()

# Congresistas -> Cantidad de PL presentados por sexo
tbl(con, "proyecto_ley_autors") %>%
select(proyecto_ley_autors_id = id, proyecto_ley_id, congresista_id) %>%
left_join(
tbl(con, "proyecto_leys") %>% select(proyecto_ley_id = id, proyecto = titulo),
by = "proyecto_ley_id") %>%
left_join(
tbl(con, "congresistas") %>% filter(activo == 1) %>%
select(congresista_id = id, congresista = nombre, genero_id),
by = "congresista_id") %>%
left_join(
tbl(con,"generos") %>% select(genero_id = id, genero = nombre),
by = "genero_id") %>%
count(genero) %>% show_query()

# Congresistas -> Partidos Políticos
tbl(con, "congresistas") %>% filter(activo == 1) %>%
select(congresista_id = id, congresista = nombre, partido_id) %>%
left_join(
tbl(con, "partidos") %>% filter(activo == 1) %>%
select(partido_id = id, partido = nombre),
by = "partido_id") %>% show_query()

# Congresistas -> Circunscripción
tbl(con, "congresistas") %>% filter(activo == 1) %>%
select(congresista_id = id, congresista = nombre, circunscripcion_id) %>%
left_join(
tbl(con, "circunscripcions") %>% filter(activo == 1) %>%
select(circunscripcion_id = id, circunscripcion = nombre),
by = "circunscripcion_id") %>% show_query()

# Congresistas -> Minoría
# ?
