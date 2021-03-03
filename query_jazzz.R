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
  gather(genero, años, -n)

# Congresistas -> Pirámide Poblacional de Edad y sexo -> Senadores
tbl(con, "congresistas") %>% filter(activo == 1, es_senador == 1, cuatrienio_id == 1) %>%
  mutate(años = round(DATEDIFF(CURDATE(),fechaNacimiento)/365))  %>%
  count(genero_id,años) %>% left_join(tbl(con,"generos") %>%
                                        select(id, genero = nombre), by = c("genero_id" = "id")) %>%
  ungroup %>% select(genero, años, n)
