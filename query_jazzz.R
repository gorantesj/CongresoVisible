library(dbplyr)
library(DBI)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306
)

x <- dbListTables(con)
grep(pattern = 'proyecto', x = x, value=TRUE, fixed=TRUE)

Z <- tbl(con, "cuatrienios") %>% collect()
# Cuántos hay en curso

  tbl(con, "estado_proyecto_leys") %>%
  mutate(estado_proyecto_ley_id = id, estado_proyecto_ley = nombre) %>%
  select(c(estado_proyecto_ley_id, estado_proyecto_ley)) %>%
left_join(tbl(con, "proyecto_ley_estados") %>% filter(activo == 1), by = "estado_proyecto_ley_id") %>%
  mutate(n = case_when( id == 1 | id == 2  ~ 1, is.na(id) ~ 0 )) %>%
  select(c(estado_proyecto_ley, n)) %>%
  mutate(cuantos = sum(n)) %>%
  show_query()

# <SQL>
#   SELECT `estado_proyecto_ley`, `n`, SUM(`n`) OVER () AS `cuantos`
# FROM (SELECT `estado_proyecto_ley`, CASE
#       WHEN (`id` = 1.0 OR `id` = 2.0) THEN (1.0)
#       WHEN (((`id`) IS NULL)) THEN (0.0)
#       END AS `n`
#       FROM (SELECT `LHS`.`estado_proyecto_ley_id` AS `estado_proyecto_ley_id`, `estado_proyecto_ley`, `id`, `proyecto_ley_id`, `fecha`, `gaceta_texto`, `gaceta_url`, `activo`, `usercreated`, `usermodifed`, `nota`, `old_id`, `camara_id`, `numero_titulos`, `numero_articulos`, `observaciones`, `orden`, `created_at`, `updated_at`
#             FROM (SELECT `id` AS `estado_proyecto_ley_id`, `nombre` AS `estado_proyecto_ley`
#                   FROM `estado_proyecto_leys`) `LHS`
#             LEFT JOIN (SELECT *
#                          FROM `proyecto_ley_estados`
#                        WHERE (`activo` = 1.0)) `RHS`
#             ON (`LHS`.`estado_proyecto_ley_id` = `RHS`.`estado_proyecto_ley_id`)
#       ) `q01`) `q02`

# Estatus

tbl(con, "estado_proyecto_leys") %>%
  mutate(estado_proyecto_ley_id = id, estado_proyecto_ley = nombre) %>%
  select(c(estado_proyecto_ley_id, estado_proyecto_ley)) %>%
  left_join(tbl(con, "proyecto_ley_estados") %>% filter(activo == 1), by = "estado_proyecto_ley_id") %>%
  mutate(n = case_when( id == 1 | id == 2  ~ 1, is.na(id) ~ 0 )) %>%
  select(c(estado_proyecto_ley, n)) %>%
  show_query()

# <SQL>
#   SELECT `estado_proyecto_ley`, CASE
# WHEN (`id` = 1.0 OR `id` = 2.0) THEN (1.0)
# WHEN (((`id`) IS NULL)) THEN (0.0)
# END AS `n`
# FROM (SELECT `LHS`.`estado_proyecto_ley_id` AS `estado_proyecto_ley_id`, `estado_proyecto_ley`, `id`,
#         `proyecto_ley_id`, `fecha`, `gaceta_texto`, `gaceta_url`, `activo`, `usercreated`, `usermodifed`,
#         `nota`, `old_id`, `camara_id`, `numero_titulos`, `numero_articulos`, `observaciones`, `orden`, `created_at`, `updated_at`
#       FROM (SELECT `id` AS `estado_proyecto_ley_id`, `nombre` AS `estado_proyecto_ley`
#             FROM `estado_proyecto_leys`) `LHS`
#       LEFT JOIN (SELECT *
#                    FROM `proyecto_ley_estados`
#                  WHERE (`activo` = 1.0)) `RHS`
#       ON (`LHS`.`estado_proyecto_ley_id` = `RHS`.`estado_proyecto_ley_id`)
# ) `q01`

### Iniciativa
grep(pattern = 'inicia', x = x, value=TRUE, fixed=TRUE)
grep(pattern = 'corporacion', x = x, value=TRUE, fixed=TRUE)


left_join(
  tbl(con, "iniciativas") %>%
  mutate(iniciativa_id = id, iniciativa = nombre) %>%
  filter(activo == 1) %>%
  select(c(iniciativa_id, iniciativa)) %>%
left_join(
  tbl(con, "proyecto_leys") %>%
  filter(activo == 1 && cuatrienio_id == 1) %>%
  mutate(proyecto_ley_id = id) %>%
  select(c(iniciativa_id, proyecto_ley_id)),
  by = "iniciativa_id"),
  tbl(con, "proyecto_ley_ponentes") %>%
  filter(activo == 1) %>%
  select(c(proyecto_ley_id, congresista_id)) %>%
left_join(
  tbl(con, "corporacion_miembros") %>%
  select(c(corporacion_id, congresista_id)),
  by = "congresista_id") %>%
left_join(
  tbl(con, "corporacions") %>%
  mutate(corporacion_id = id, corporacion = nombre) %>%
  select(c(corporacion_id, corporacion)),
  by = "corporacion_id"),
by = "proyecto_ley_id") %>%
show_query()

# <SQL>
#   SELECT `iniciativa_id`, `iniciativa`, `LHS`.`proyecto_ley_id` AS `proyecto_ley_id`, `congresista_id`, `corporacion_id`, `corporacion`
# FROM (SELECT `LHS`.`iniciativa_id` AS `iniciativa_id`, `iniciativa`, `proyecto_ley_id`
#       FROM (SELECT `iniciativa_id`, `iniciativa`
#             FROM (SELECT `id`, `nombre`, `activo`, `usercreated`, `usermodifed`, `created_at`, `updated_at`, `id` AS `iniciativa_id`, `nombre` AS `iniciativa`
#                   FROM `iniciativas`) `q01`
#             WHERE (`activo` = 1.0)) `LHS`
#       LEFT JOIN (SELECT `iniciativa_id`, `id` AS `proyecto_ley_id`
#                  FROM `proyecto_leys`
#                  WHERE (`activo` = 1.0)) `RHS`
#       ON (`LHS`.`iniciativa_id` = `RHS`.`iniciativa_id`)
# ) `LHS`
# LEFT JOIN (SELECT `proyecto_ley_id`, `congresista_id`, `LHS`.`corporacion_id` AS `corporacion_id`, `corporacion`
#            FROM (SELECT `proyecto_ley_id`, `LHS`.`congresista_id` AS `congresista_id`, `corporacion_id`
#                  FROM (SELECT `proyecto_ley_id`, `congresista_id`
#                        FROM `proyecto_ley_ponentes`
#                        WHERE (`activo` = 1.0)) `LHS`
#                  LEFT JOIN (SELECT `corporacion_id`, `congresista_id`
#                             FROM `corporacion_miembros`) `RHS`
#                  ON (`LHS`.`congresista_id` = `RHS`.`congresista_id`)
#            ) `LHS`
#            LEFT JOIN (SELECT `id` AS `corporacion_id`, `nombre` AS `corporacion`
#                       FROM `corporacions`) `RHS`
#            ON (`LHS`.`corporacion_id` = `RHS`.`corporacion_id`)
# ) `RHS`
# ON (`LHS`.`proyecto_ley_id` = `RHS`.`proyecto_ley_id`)


tbl(con, "tema_proyecto_leys") %>%
filter(activo == 1) %>%
mutate(n = 1) %>%
group_by(nombre) %>%
summarise(across(n, sum)) %>%
arrange(n) %>%
head(5) %>%
show_query()

# <SQL>
#   SELECT `nombre`, SUM(`n`) AS `n`
# FROM (SELECT `id`, `nombre`, `activo`, `usercreated`, `usermodifed`, `created_at`, `updated_at`, 1.0 AS `n`
#       FROM `tema_proyecto_leys`
#       WHERE (`activo` = 1.0)) `q01`
# GROUP BY `nombre`
# ORDER BY `n`
# LIMIT 5

## Años en el cargo
grep(pattern = 'candidato', x = x, value=TRUE, fixed=TRUE)

A <- tbl(con, "eleccion_candidatos") %>% collect()
B <- tbl(con, "cargo_integrantes") %>% collect()
C <- tbl(con, "comisions") %>% collect()

grep(pattern = 'ministerio', x = x, value=TRUE, fixed=TRUE)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306
)

D <- tbl(con, "comision_cargo_congresistas") %>% collect()
E <- tbl(con, "comision_tipo_congresistas") %>% collect()
G <- tbl(con, "congresista_datos_contactos") %>% collect()
H <- tbl(con, "congresista_imagens") %>% collect()
I <- tbl(con, "congresista_perfils") %>% collect()
J <- tbl(con, "congresista_trayectoria_privadas") %>% collect()
K <- tbl(con, "congresista_trayectoria_publicas") %>% collect()
M <- tbl(con, "congresistas") %>% collect()
N <- tbl(con, "congreso_personas") %>% collect()

tbl(con, "congresistas") %>%
  select(c(nombre, cuatrienio_id)) %>%
left_join(
  select(mutate(tbl(con, "cuatrienios"),
  cuatrienio_id = id),
  c(cuatrienio_id, fecha_inicio, fecha_fin)),
  by = "cuatrienio_id"
) %>%
group_by(nombre) %>%
mutate(
  nombre_congresista = nombre,
  fecha_inicio_2 =  year(fecha_inicio),
  fecha_fin_2 = year(fecha_fin),
  años_cargo = fecha_fin_2 - fecha_inicio_2
) %>%
select(
  nombre_congresista,
  años_cargo
) %>% show_query()

# <SQL>
#   SELECT `nombre_congresista`, `fecha_fin_2` - `fecha_inicio_2` AS `años_cargo`
# FROM (SELECT `nombre`, `cuatrienio_id`, `fecha_inicio`, `fecha_fin`, EXTRACT(year FROM `fecha_fin`) AS `fecha_fin_2`, EXTRACT(year FROM `fecha_inicio`) AS `fecha_inicio_2`, `nombre` AS `nombre_congresista`
#       FROM (SELECT `nombre`, `LHS`.`cuatrienio_id` AS `cuatrienio_id`, `fecha_inicio`, `fecha_fin`
#             FROM (SELECT `nombre`, `cuatrienio_id`
#                   FROM `congresistas`) `LHS`
#             LEFT JOIN (SELECT `id` AS `cuatrienio_id`, `fecha_inicio`, `fecha_fin`
#                        FROM `cuatrienios`) `RHS`
#             ON (`LHS`.`cuatrienio_id` = `RHS`.`cuatrienio_id`)
#       ) `q01`) `q02`

## Proporción de iniciativas por ministro

# left_join(
#   tbl(con, "iniciativas") %>%
#   mutate(iniciativa_id = id, iniciativa = nombre) %>%
#   filter(activo == 1) %>%
#   select(c(iniciativa_id, iniciativa)) %>%
# left_join(
#   tbl(con, "proyecto_leys") %>%
#   filter(activo == 1 && cuatrienio_id == 1) %>%
#   mutate(proyecto_ley_id = id),
# by = "iniciativa_id"),
#   tbl(con, "proyecto_ley_ponentes") %>%
#   filter(activo == 1),
# by = "proyecto_ley_id"
# ) is there no relation?

  tbl(con, "sentencias_ministerios") %>%
  select(-c(created_at, updated_at)) %>%
left_join(
  tbl(con, "sentencias_nombres_ministerios") %>%
  mutate(ministerios_id = id) %>%
  select(-c(created_at, updated_at, avatar, id)),
by = "ministerios_id") %>%
group_by(nombre) %>%
summarise(across(participacion, sum)) %>%
  show_query()

  # SELECT `nombre`, SUM(`participacion`) AS `participacion`
  # FROM (SELECT `id`, `LHS`.`ministerios_id` AS `ministerios_id`, `participacion`, `sentencia_id`, `nombre`
  #       FROM (SELECT `id`, `ministerios_id`, `participacion`, `sentencia_id`
  #             FROM `sentencias_ministerios`) `LHS`
  #       LEFT JOIN (SELECT `nombre`, `id` AS `ministerios_id`
  #                  FROM `sentencias_nombres_ministerios`) `RHS`
  #       ON (`LHS`.`ministerios_id` = `RHS`.`ministerios_id`)
  # ) `q01`
  # GROUP BY `nombre`

## Proporción de iniciativas sancionadas como ley de ministros
  grep(pattern = 'sentencias', x = x, value=TRUE, fixed=TRUE)

left_join(
  tbl(con, "sentencias_ministerios") %>%
  select(-c(id, created_at, updated_at, participacion)) %>%
left_join(
  tbl(con, "sentencias_nombres_ministerios") %>%
  mutate(ministerios_id = id) %>%
  select(-c(created_at, updated_at, avatar, id)),
by = "ministerios_id"),
tbl(con, "sentencias_generals") %>%
  mutate(sentencia_id = id) %>%
  select(sentencia_id, id_norma),
by = "sentencia_id" ) %>%
group_by(nombre)%>%
summarise(total=n()) %>%
mutate(totales=sum(total, na.rm=T),
porc=round((total/totales)*100, 2)) %>%
show_query()

# <SQL>
#   SELECT `nombre`, `total`, `totales`, ROUND((`total` / `totales`) * 100.0, 2) AS `porc`
# FROM (SELECT `nombre`, `total`, SUM(`total`) OVER () AS `totales`
#       FROM (SELECT `nombre`, COUNT(*) AS `total`
#             FROM (SELECT `ministerios_id`, `LHS`.`sentencia_id` AS `sentencia_id`, `nombre`, `id_norma`
#                   FROM (SELECT `LHS`.`ministerios_id` AS `ministerios_id`, `sentencia_id`, `nombre`
#                         FROM (SELECT `ministerios_id`, `sentencia_id`
#                               FROM `sentencias_ministerios`) `LHS`
#                         LEFT JOIN (SELECT `nombre`, `id` AS `ministerios_id`
#                                    FROM `sentencias_nombres_ministerios`) `RHS`
#                         ON (`LHS`.`ministerios_id` = `RHS`.`ministerios_id`)
#                   ) `LHS`
#                   LEFT JOIN (SELECT `id` AS `sentencia_id`, `id_norma`
#                              FROM `sentencias_generals`) `RHS`
#                   ON (`LHS`.`sentencia_id` = `RHS`.`sentencia_id`)
#             ) `q01`
#             GROUP BY `nombre`) `q02`) `q03`

## Citaciones de ministros
left_join(
tbl(con, "sentencias_ministerios") %>%
select(-c(id, created_at, updated_at, participacion)),
tbl(con, "sentencias_nombres_ministerios") %>%
mutate(ministerios_id = id) %>%
select(-c(created_at, updated_at, avatar, id)),
by = "ministerios_id")
# [1] "ministerios_id" "sentencia_id"   "nombre"

tbl(con, "sentencias_generals") %>%
  mutate(sentencia_id = id) %>%
  select(sentencia_id, partido_congresista_id)
# [1] "sentencia_id" "partido_congresista_id"


grep(pattern = 'cita', x = x, value=TRUE, fixed=TRUE)
#######
# [1] "cargo_citados"                   "citacion_citantes"               "citacion_otros_invitados"
# [4] "citacions"                       "citado_asistente_citacions"      "citados"
# [7] "control_politico_citado_imagens" "control_politico_citados"        "control_politico_citantes"
# [10] "entidad_citados"                 "invitado_asistente_citacions"    "res_tipo_citados"
# [13] "tipo_citacions"

A <- tbl(con, "cargo_citados") %>% collect()
# [1] "id"         "nombre"     "old_id"     "created_at" "updated_at"
B <- tbl(con, "citacions") %>% collect() !!
# [1] "id" "tipo_id" "tema_principal_id" "tema_secundario_id" "plenaria"
# [6] "tags" "fecha_proposicion" "old_id" "detalles" "gacetas"
# [11] "cuestionario" "numero_proposicion" "respuesta" "duplicado" "created_at"
# [16] "updated_at"
C <- tbl(con, "citacion_citantes") %>% collect() !!!!
# [1] "id" "citacion_id"    "congresista_id" "created_at"     "updated_at"
D <- tbl(con, "citacion_otros_invitados") %>% collect()
# [1] "id"  "citacion_id" "persona_id"  "created_at"  "updated_at"
E <- tbl(con, "citado_asistente_citacions") %>% collect()
# [1] "id" "citacion_id" "citado_id" "asiste" "delega_asistencia"
# [6] "excuso"            "created_at"        "updated_at"
G <- tbl(con, "citados") %>% collect()
# [1] "id" "nombres"    "entidad_id" "cargo_id"  "old_id" "imagen" "created_at" "updated_at"
H <- tbl(con, "control_politico_citado_imagens") %>% collect()
# [1] "id" "citado_id"  "imagen" "activo" "usercreated" "usermodifed" "created_at"  "updated_at"
I <- tbl(con, "control_politico_citados") %>% collect()
# [1] "id"                            "control_politico_id"           "comision_cargo_congresista_id"
# [4] "tipo_citacion_id"              "nombre"                        "activo"
# [7] "usercreated"                   "usermodifed"                   "created_at"
# [10] "updated_at"
K <- tbl(con, "control_politico_citantes") %>% collect()
# [1] "id" "control_politico_id" "congresista_id" "activo" "usercreated"
# [6] "usermodifed" "created_at"  "updated_at"
M <- tbl(con, "entidad_citados") %>% collect()
# [1] "id"         "nombre"     "old_id"     "created_at" "updated_at"
N <- tbl(con, "invitado_asistente_citacions") %>% collect()
# [1] "id" "citacion_id" "citado_id" "asiste" "delega_asistencia"
# [6] "excuso" "created_at" "updated_at"
L <- tbl(con, "res_tipo_citados") %>% collect()
# [1] "id"          "nombre"      "activo"      "usercreated" "usermodifed" "created_at"  "updated_at"
O <- tbl(con, "tipo_citacions") %>% collect()
# [1] "id"          "nombre"      "activo"      "usercreated" "usermodifed" "created_at"  "updated_at"

grep(pattern = 'congresista', x = x, value=TRUE, fixed=TRUE)

tbl(con, "congresistas") %>%
mutate(congresista_id = id, partido_congresista_id = partido_id) %>%
select(congresista_id, partido_congresista_id)

#####
left_join(
left_join(
tbl(con, "sentencias_ministerios") %>%
select(-c(id, created_at, updated_at, participacion)),
tbl(con, "sentencias_nombres_ministerios") %>%
mutate(ministerios_id = id) %>%
select(-c(created_at, updated_at, avatar, id)),
by = "ministerios_id") %>%
left_join(
tbl(con, "sentencias_generals") %>%
mutate(sentencia_id = id) %>%
select(sentencia_id, partido_congresista_id),
by = "sentencia_id"),
tbl(con, "congresistas") %>%
mutate(congresista_id = id, partido_congresista_id = partido_id) %>%
select(congresista_id, partido_congresista_id),
by = "partido_congresista_id"
) %>% left_join(
tbl(con, "citacion_citantes") %>%
select(citacion_id, congresista_id),
by = "congresista_id"
) %>% left_join(
  tbl(con, "citacions") %>%
  mutate(citacion_id = id) %>%
  select(-id),
  by = "citacion_id")
