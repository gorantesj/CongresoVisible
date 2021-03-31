library(tidyverse)
library(readxl)
# Leer bases de datos
archivos <- list.files("Tablas anteriores",full.names = T,all.files = F)
leer <- safely(read_xlsx)
tabla <- archivos %>% map(~leer(.x))
tabla_res <- tabla %>% transpose() %>% pluck("result")

# Leer campos
campos <- read_csv("parafinalhector.csv")

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
posts <- tabla_res[[18]] %>%
  inner_join(tabla_res[[15]], by=c("blog_id"="id")) %>%
  select(-id,-blog_id, -destacado.x, -destacado.y) %>%
  rename(`titulo_post`=titulo.x,
         `titulo_blog`=titulo.y)

posts <- inner_join(posts,
           tabla_res[[10]] %>%
  select(id,username,first_name,last_name,email),
  by=c("autor_id"="id"))

posts <- posts %>% inner_join(tabla_res[[20]], by=c("tipo_blog_id"="id")) %>%
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
boletin <- tabla_res[[16]] %>% select(nombre, año=anio, objeto, archivo)
write_excel_csv(boletin,"finales/para CV/boletin.csv")
# Cargo -------------------------------------------------------------------------
# Solamente las personas pueden tener cargos
# Hay que juntar cargo=sector_id, cargo, entidad
trayectoria_privada <- read_csv("Bases de datos nuevas/congresista_trayectoria_privadas.csv")
trayectoria_privada <- tabla_res[[33]] %>%
  full_join(tabla_res[[99]], by = c("sector_id"= "id")) %>%
  replace_na(replace = list(nombre="", cargo="", entidad="")) %>%
  mutate(cargo=paste(nombre, cargo, entidad)) %>%
  select(id, cargo, fecha_final) %>%
  inner_join(trayectoria_privada) %>%
  select(id, persona_id, cargo, fecha, fecha_final,created_at, updated_at)
write_csv(trayectoria_privada, file = "finales/cambio en campos/persona_trayectoria_privadas.csv")
# Cargo cámara
tabla_res[[34]] %>%
  select(id, nombre) %>%
  write_csv("finales/tablas nuevas/cargo_corporacions.csv")

# Cargo comisiones
# Se agregaron manualmente

# Cargo político
# Para la migración hay que eliminar los cargos de cámaras
tabla_res[[36]] %>%
  filter(is.na(x = camara_id)) %>%
  select(id, persona_id, partido_id, fecha=fecha_inicio, fecha_final) %>%
  write_csv("finales/cambio en campos/persona_trayectoria_publicas.csv")


# Datos contactos ---------------------------------------------------------
comision_contacto <- tabla_res[[38]] %>% select(comision_id=id,
                            correo:url) %>%
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
comision_contacto %>% rename(nombre=dato_contacto_id) %>%
  inner_join(datos_contacto %>% select(nombre, dato_contacto_id=id),
             by="nombre") %>%
  select(id, dato_contacto_id, comision_id, cuenta,activo:updated_at) %>%
  write_csv(file = "finales/idénticas/comision_datos_contactos.csv")

# Congresista -------------------------------------------------------------
tabla_res[[39]] %>% count()
# Generos -------------------------------------------------------------

generos <- read_csv("Bases de datos nuevas/generos.csv")

generos %>%  mutate(activo=NA_character_,
                    usercreated=NA_character_,
                    usermodifed=NA_character_,
                    created_at=NA_character_,
                    updated_at=NA_character_) %>%
        write_excel_csv("Bases de datos nuevas/generos.csv")
# iniciativas -------------------------------------------------------------

iniciativas <- read_csv("Bases de datos nuevas/iniciativas.csv")

iniciativas %>%  mutate(activo=NA_character_,
                    usercreated=NA_character_,
                    usermodifed=NA_character_,
                    created_at=NA_character_,
                    updated_at=NA_character_) %>%
  write_excel_csv("Bases de datos nuevas/iniciativas.csv")
# Orden del día citación citantes  -------------------------------------------------------------


citaciones <- read_csv("Bases de datos nuevas/control_politico_citantes.csv")

citaciones %>%  mutate(activo=NA_character_,
                    usercreated=NA_character_,
                    usermodifed=NA_character_,
                    created_at=NA_character_,
                    updated_at=NA_character_) %>%
  write_excel_csv("Bases de datos nuevas/control_politico_citantes.csv")

# Control político -----------------------------------------------------------------

citacion <- tabla_res[[123]]

orden_dia <- tabla_res[[128]] %>%
             select(id, orden_del_dia_id)

citacion <- left_join(citacion, orden_dia, by=c("itemdeordendeldia_ptr_id"="id"))

orden_dia_citacion <- tabla_res[[131]] %>%
            select(id, cuatrienio_id)

citacion <- left_join(citacion, orden_dia_citacion,by=c("orden_del_dia_id"="id"))

orden_dia_comision <- tabla_res[[130]] %>%
           select(ordendeldia_id, comision_id)


citacion <- left_join(citacion, orden_dia_comision,by=c("orden_del_dia_id"="ordendeldia_id"))

citacion %>% mutate(id=row_number())
# transformación ----------------------------------------------------------

crear_bases(campos %>% filter(num==141), tabla_res)

tabla_res[[123]]
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
