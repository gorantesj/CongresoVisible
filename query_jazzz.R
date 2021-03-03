library(dbplyr)
library(DBI)

con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible2",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306
)

x <- dbListTables(con)
grep(pattern = 'proyecto', x = x, value=TRUE, fixed=TRUE)


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
  filter(activo == 1) %>%
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
