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
# Congreso hoy >> Secciones >> Proyectos de ley
#
##################################################################
# Proyectos de ley >> Número de PL en trámite >> Proyectos en Cámara
tbl(con, "proyecto_leys") %>%
  filter(activo == 1 && cuatrienio_id == 1) %>%
  tally() %>%
  show_query()

SELECT COUNT(*) FROM(
  SELECT `id`, `titulo`, `numero_camara`
FROM `proyecto_leys`
WHERE (`activo` = 1.0 AND `cuatrienio_id` = 1.0))

# [1] "id"                             "iniciativa_id"                  "legislatura_id"
# [4] "cuatrienio_id"                  "tipo_proyecto_id"               "titulo"
# [7] "numero_camara"                  "numero_senado"                  "tema_proyecto_ley_id"
# [10] "estado_proyecto_ley_id"         "sinopsis"                       "activo"
# [13] "usercreated"                    "usermodifed"                    "fecha_radicacion"
# [16] "tags"                           "old_id"                         "se_acumula_a_id"
# [19] "camara_id"                      "enlace_a_texto"                 "enlace_a_informe_derecho_justo"
# [22] "proyecto_ley_estado_id"         "alias"                          "importancia"
# [25] "destacado"                      "alcance"                        "norma"
# [28] "created_at"                     "updated_at"                     "proyecto_ley_id"

# Proyectos de ley >> Número de PL en trámite >> Proyectos en Senado
tbl(con, "proyecto_leys") %>%
  filter(activo == 1 && cuatrienio_id == 1) %>%
  mutate(proyecto_ley_id = id) %>%
  select(proyecto_ley_id, numero_senado) %>%
  show_query()
# <SQL>
#   SELECT `id` AS `proyecto_ley_id`, `numero_senado`
# FROM `proyecto_leys`
# WHERE (`activo` = 1.0 AND `cuatrienio_id` = 1.0)

# Proyectos de ley >> Origen de la iniciativa >> Legislativa
tbl(con, "proyecto_leys") %>%
  filter(activo == 1 && cuatrienio_id == 1) %>%
  mutate(proyecto_ley_id = id) %>%
  select(iniciativa_id, legislatura_id) %>%
  filter(legislatura_id == 1) %>%
  show_query()
# <SQL>
#   SELECT *
#   FROM (SELECT `iniciativa_id`, `legislatura_id`
#         FROM `proyecto_leys`
#         WHERE (`activo` = 1.0 AND `cuatrienio_id` = 1.0)) `q01`
# WHERE (`legislatura_id` = 1.0)

# Proyectos de ley >> Origen de la iniciativa >> Gubernamental
tbl(con, "proyecto_leys") %>%
  filter(activo == 1 && cuatrienio_id == 1) %>%
  mutate(proyecto_ley_id = id) %>%
  select(iniciativa_id, legislatura_id) %>%
  filter(camara_id == 1) %>%
  show_query()
# <SQL>
#   SELECT *
#   FROM (SELECT `iniciativa_id`, `legislatura_id`
#         FROM `proyecto_leys`
#         WHERE (`activo` = 1.0 AND `cuatrienio_id` = 1.0)) `q01`
# WHERE (`camara_id` = 1.0)

