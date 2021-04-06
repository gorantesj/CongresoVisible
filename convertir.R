library(tidyverse)
library(readxl)
# Leer bases de datos
archivos <- list.files("Tablas anteriores/",full.names = T,all.files = F)
leer <- safely(read_xlsx)
tablas <- archivos %>% map(~leer(.x))
tablas_res <- tablas %>% transpose() %>% pluck("result")

# Leer campos
campos <- read_csv("paralafinaldeborah.csv")

# funciones ---------------------------------------------------------------
crear_bases<- function(relaciones, tablas){
  relaciones <- relaciones %>% filter(grupo!="")
  # Tablas nuevas del grupo 1
  tablas_nuevas1 <- relaciones %>%
    filter(grupo==1) %>%
    pull(tabla_gp) %>%
    unique()
  map(tablas_nuevas1,
      ~convertir_grupo1(.x, tablas, relaciones))
  # Tablas nuevas del grupo 2
  convertir_grupo22(tablas, relaciones)
  #
}

convertir_grupo1 <- function(tabla_nueva, tablas, relaciones){
  # Filtra las relaciones de la tabla nueva
  relaciones <- relaciones %>%
    filter(tabla_gp==tabla_nueva,
           grupo==1)
  # Separa las relaciones en tablas viejas (usualmente 1)
  tablas_viejas <- relaciones %>% split(.$num)
  # Para cada tabla vieja selecciona los campos viejos
  # Si transformar es igual a 1 le escribe el nombre de campo viejoa con el prefijo
  # transformar
  campos_nuevos <- tablas_viejas %>%
    map(~{
      # browser()
      res <- tablas[[unique(.x$num)]] %>%
        select(.x$campo)
      names(res) <- .x$campo_gp
      names(res)[which(.x$transformar == 1)] <- paste("transformar",
                                                      .x$campo[which(.x$transformar == 1)],
                                                      sep = "_")
      return(res)

    }
    ) %>%
    reduce(bind_cols)
  #browser()
  write_excel_csv(x = campos_nuevos,
                  glue::glue("Bases de datos nuevas/{tabla_nueva}.csv"),)
}

convertir_grupo22 <- function(tablas, relaciones){
  # Tablas viejas
  tablas_viejas <- relaciones %>%
    filter(transformar==2) %>%
    split(.$num)
  # Tablas
  tablas_viejas %>%
    map(~{
      tablas_nuevas <- tablas[[unique(.x$num)]]
      nombre_tabla <- unique(paste0("respaldo_",.x$seccion, .x$tabla))
      #browser()
      write_excel_csv(x = tablas_nuevas,
                      glue::glue("Bases de datos nuevas/{nombre_tabla}.csv"))
    })
}

graficar_relaciones <- function(carpeta="Base de datos/"){
  archivos <- list.files(carpeta)
  nodos <- tibble(id=stringr::str_replace(archivos,pattern = ".csv",replacement = ""))
  nodos <- nodos %>% mutate(label=id,
                            groups=if_ele)
  aristas <- map_df(archivos, ~
                      {
                        relaciones <- tibble(to=read_csv(paste(carpeta, .x, sep="/")) %>%
                                               select(ends_with("_id")) %>%
                                               names() %>%
                                               stringr::str_replace(string = .,
                                                                    pattern = "_id",
                                                                    replacement = "s"))
                        relaciones <- relaciones %>%
                          mutate(from=stringr::str_replace(.x,pattern = ".csv", replacement = ""))
                      }
  )
  return(list(nodos, aristas))
}



# Construir blog-------------------------------------------------------------------------

# Blogs
# Blog, posts, tipo_blogs, autor

posts <- tablas_res2[[18]] %>%
  inner_join(tablas_res[[15]], by=c("blog_id"="id")) %>%
  select(-id,-blog_id, -destacado.x, -destacado.y) %>%
  rename(`titulo_post`=titulo.x,
         `titulo_blog`=titulo.y)

posts <- inner_join(posts,
                    tablas_res[[10]] %>%
                      select(id,username,first_name,last_name,email),
                    by=c("autor_id"="id"))

posts <- posts %>% inner_join(tablas_res[[20]], by=c("tipo_blog_id"="id")) %>%
  mutate(autor=paste(first_name, last_name)) %>%
  select(fecha_publicacion,
         tipo_blog=nombre,
         titulo_blog,
         descripcion,
         contenido,
         titulo_post,
         autor,
         email_autor=email,
         esta_publicado:ee )
write_excel_csv(posts,"finales/para CV/posts.csv")

# Boletines
boletin <- tablas_res[[16]] %>% select(nombre, año=anio, objeto, archivo)
write_excel_csv(boletin,"finales/para CV/boletin.csv")
# Congresista -------------------------------------------------------------
# Para el final


# Mezclar con periodos
congresistas <- inner_join(tablas_res[[39]],
                           tablas_res[[53]] ,
                           by=c("persona_ptr_id"= "congresista_id"))

# Falta arreglar el periodo por congresista
congresistas <- congresistas %>%
  mutate(curul_id=NA_integer_) %>%
  select(id,
         persona_id=persona_ptr_id,
         corporacion_id=camara_id,
         cuatrienio_id=periodo_id,
         partido_id=partido_politico_id,
         curul_id,
         circunscripcion_id
  )
write_csv(congresistas,
          file = "finales/cambio en campos/congresistas.csv")


# Para CV
tablas_res[[39]] %>%
  filter(!(es_congresista|
             es_senador|
             es_representante_camara|
             ha_sido_congresista|
             ha_reemplazado|
             ha_sido_reemplazo)|is.na(es_congresista|
                                        es_senador|
                                        es_representante_camara|
                                        ha_sido_congresista|
                                        ha_reemplazado|
                                        ha_sido_reemplazo))

# Todos falso
#


# Cargo -------------------------------------------------------------------------
# Solamente las personas pueden tener cargos
# Hay que juntar cargo=sector_id, cargo, entidad
trayectoria_privada <- read_csv("Bases de datos nuevas/congresista_trayectoria_privadas.csv")
trayectoria_privada <- tablas_res[[33]] %>%
  full_join(tablas_res[[99]], by = c("sector_id"= "id")) %>%
  replace_na(replace = list(nombre="", cargo="", entidad="")) %>%
  mutate(cargo=paste(nombre, cargo, entidad)) %>%
  select(id, cargo, fecha_final) %>%
  inner_join(trayectoria_privada) %>%
  select(id, persona_id, cargo, fecha, fecha_final,created_at, updated_at)
write_csv(trayectoria_privada, file = "finales/cambio en campos/persona_trayectoria_privadas.csv")

# Cargo legislativos, catálogo para cargo camara y cargo comisiones

tablas_res[[34]] %>%
  select(id, nombre) %>%
  write_csv("finales/tablas nuevas/cargo_corporacions.csv")

# Cargo cámara
cargo_camara <- tablas_res[[46]] %>%
  rename(persona_id=congresista_id) %>%
  inner_join(congresistas %>%
               select(persona_id,
                      periodo_id=cuatrienio_id,
                      congresista_id=id)) %>%
  mutate(  activo=T,
           usercreated="",
           usermodifed=""
  ) %>%
  select(id,
         cuatrienio_id=periodo_id,
         corporacion_id=camara_id,
         activo,
         usercreated,
         usermodifed,
         created_at,
         updated_at=edited_at)
write_csv(cargo_camara, "finales/tablas nuevas/cargo_camaras.csv")

# Cargo comisiones
# Se agregaron manualmente

# Cargo político
# Para la migración hay que eliminar los cargos de cámaras
cargo_politico <- tablas_res[[36]] %>%
  mutate(activo=T, usercreated="", usermodifed="") %>%
  select(id,
         persona_id,
         partido_id,
         cargo,
         fecha=fecha_inicio,
         fecha_final,
         activo,
         usercreated,
         usermodifed,
         created_at,
         updated_at=edited_at
         )

congresista_partido <-  tablas_res[[41]] %>%
  mutate(id=id+max(cargo_politico$id),
         cargo="Congresista",
         activo=T,
         usercreated="",
         usermodifed=""
         ) %>%
  select(id,
         persona_id=congresista_id,
         partido_id,
         cargo,
         fecha=fecha_inicio,
         fecha_final=fecha_fin,
         activo,
         usercreated,
         usermodifed,
         created_at,
         updated_at=edited_at
         )
rbind(cargo_politico,
      congresista_partido) %>%
write_csv("finales/cambio en campos/persona_trayectoria_publicas.csv")



# Datos contactos ---------------------------------------------------------
comision_contacto <- tablas_res[[38]] %>% select(comision_id=id, correo:url) %>%
  tidyr::pivot_longer(cols = -comision_id,
                      names_to = "dato_contacto_id",
                      values_to = "cuenta") %>%
  na.omit() %>%
  mutate(id=row_number(),
         activo="",
         usercreated="",
         usermodified="",
         created_at="",
         updated_at="") %>%
  select(id,dato_contacto_id,comision_id, cuenta, activo:updated_at)
# Catálogo dato_contactos
datos_contacto <-comision_contacto %>%
  select(dato_contacto_id, activo:updated_at) %>%
  unique() %>%
  mutate(nombre=dato_contacto_id,
         id=as.numeric(as.factor(dato_contacto_id)),
         Tipo=if_else(dato_contacto_id=="url", 2, 1)) %>%
  select(id, nombre, Tipo, activo:updated_at)
write_excel_csv(datos_contacto,"finales/idénticas/dato_contactos.csv")

# Comisiones
comision_contacto <- comision_contacto %>% rename(nombre=dato_contacto_id) %>%
  inner_join(datos_contacto %>% select(nombre, dato_contacto_id=id),
             by="nombre") %>%
  select(id, dato_contacto_id, comision_id, cuenta,activo:updated_at)
write_csv(comision_contacto,file = "finales/idénticas/comision_datos_contactos.csv")

# Congresistas
congresista_contacto <- inner_join(tablas_res[[39]],
           congresistas,
           by=c("persona_ptr_id"= "persona_id")) %>%
  select(id, numero_oficina, telefono_oficina) %>%
  tidyr::pivot_longer(-id, values_to = "cuenta",names_to="dato_contacto_id") %>%
  filter(!is.na(cuenta)) %>%
  mutate(dato_contacto_id=case_when(dato_contacto_id=="numero_oficina"~2,
                                    dato_contacto_id=="telefono_oficina"~5
                                    ),
         activo=T,
         usercreate="",
         usermodified	="",
         created_at="",
         updated_at=""
         )
write_csv(comision_contacto,file = "finales/idénticas/congresista_datos_contactos.csv")


# transformación ----------------------------------------------------------
campos <- read_csv("paralafinal.csv")
crear_bases(campos %>% filter(num==47), tablas_res)

# Congresista -------------------------------------------------------------
tablas_res[[39]] %>% count()



# Partidos ----------------------------------------------------------------
partidos <- read_csv("Bases de datos nuevas/partidos.csv")

partidos <- partidos %>%  mutate(activo = case_when(activo == 1~T,
                                                    activo == 2~F),
                                 color = NA_character_,
                                 usercreated = NA_character_,
                                 usermodifed = NA_character_, created_at = NA_character_,
                                 updated_at = NA_character_) %>%
  select(id, nombre, resenaHistorica, lineamientos, lugar,
         fechaDeCreacion, estatutos, color, activo, usercreated,
         usermodifed, created_at, updated_at)
write_excel_csv(partidos, "FinalesDeborah/identicas/partidos.csv")

# cuatrienios -------------------------------------------------------------
cuatrienios <- read_csv("Bases de datos nuevas/cuatrienios.csv")
cuatrienios %>%  mutate(usercreated = NA_character_,
                        usermodifed = NA_character_) %>%
  write_excel_csv("FinalesDeborah/identicas/cuatrienios.csv")



# tipo_comisions ----------------------------------------------------------
tipo_comisions <- read_csv("Bases de datos nuevas/tipo_comisions.csv")

tipo_comisions <- tipo_comisions %>%
  mutate(activo = NA_character_,
         usercreated = NA_character_,
         usermodifed = NA_character_,
         created_at = NA_character_,
         updated_at = NA_character_) %>%
  select(id = transformar_id, nombre = transformar_nombre,
         activo,
         usercreated, usermodifed,
         created_at, updated_at)
write_excel_csv(tipo_comisions, "Bases de datos nuevas/tipo_comisions.csv")



# departamentos -----------------------------------------------------------
departamentos <- read_csv("Bases de datos nuevas/departamentos.csv")

departamentos %>%  mutate(activo = NA_character_,
                          usercreated = NA_character_, usermodifed= NA_character_,
                          created_at= NA_character_, updated_at= NA_character_) %>%
  write_excel_csv("Bases de datos nuevas/departamentos.csv")

# grupo_edads -----------------------------------------------------------
grupo_edads <- read_csv("Bases de datos nuevas/grupo_edads.csv")

grupo_edads %>%  mutate(activo = NA_character_,
                          usercreated = NA_character_, usermodifed= NA_character_,
                          created_at= NA_character_, updated_at= NA_character_) %>%
  select(id = transformar_id,
         edad_inicial = transformar_edad_minima , edad_final = transformar_edad_maxima,
         activo, usercreated, usermodifed, created_at, updated_at
         ) %>%
  write_excel_csv("Bases de datos nuevas/grupo_edads.csv")

# tema_proyecto_leys -----------------------------------------------------------
tema_proyecto_leys <- tablas_res[[100]] %>%  mutate(
                        usercreated = NA_character_, usermodifed= NA_character_,
                         updated_at= edited_at)
tema_proyecto_leys%<>%  select(-edited_at)
tema_proyecto_leys %>%
  write_excel_csv("Bases de datos nuevas/tema_proyecto_leys.csv")

# control_politico_citados -----------------------------------------------------------
control_politico_citados <- read_csv("Bases de datos nuevas/control_politico_citados.csv")

control_politico_citados %>%  mutate(activo = NA_character_,
                        usercreated = NA_character_, usermodifed= NA_character_) %>%


  select(id = transformar_id,
         edad_inicial = transformar_edad_minima , edad_final = transformar_edad_maxima,
         activo, usercreated, usermodifed, created_at, updated_at
  ) %>%
  write_excel_csv("Bases de datos nuevas/control_politico_citados.csv")


# transformación ----------------------------------------------------------
# Leer campos
campos <- read_csv("paralafinaldeborah.csv")

# crear_bases(campos, tablas_res)
crear_bases(campos %>%  filter(num == 124), tablas_res)


tablas_res[[124]]

# Sandbox -----------------------------------------------------------------


ja <- graficar_relaciones("Bases de datos nuevas")

visNetwork(ja[[1]], ja[[2]])%>%
  visEdges(arrows = 'from') %>%
  visOptions( nodesIdSelection = TRUE,highlightNearest = TRUE)

campos %>% group_by(num, seccion,  tabla) %>% count(grupo) %>%
  complete(grupo=c(1,2,3,NA),fill=list(n=0)) %>%
  pivot_wider(names_from = grupo, values_from = n,
              names_glue = "Grupo {grupo}") %>%
  rowwise() %>%
  mutate(`Número de campos`=sum(c_across(starts_with("Grupo "))),
         Observaciones=case_when(`Grupo 2`==`Número de campos`~"Tabla obsoleta",
                                 `Grupo 1`==`Número de campos`~"Tabla migrada al 100%")) %>%

  write_excel_csv("tablas_originales.csv")
