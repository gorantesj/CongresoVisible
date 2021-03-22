library(tidyverse)
library(readxl)
# Leer bases de datos
archivos <- list.files("~/Documents/Congreso Visible/",full.names = T)
leer <- safely(read_xlsx)
tablas <- archivos %>% map(~leer(.x))
tablas_res <- tablas %>% transpose() %>% pluck("result")

# Leer campos
campos <- read_csv("paralafinal.csv")

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
  # browser()
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
posts <- tablas_res[[18]] %>%
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
         titulo_post,
         autor,
         email_autor=email,
         esta_publicado:ee )
write_excel_csv(posts,"finales/para CV/posts.csv")

# Boletines
boletin <- tablas_res[[16]] %>% select(nombre, año=anio, objeto, archivo)
write_excel_csv(boletin,"finales/para CV/boletin.csv")
# Cargo -------------------------------------------------------------------------
# Solamente las personas pueden tener cargos
# Hay que juntar cargo=sector_id, cargo, entidad
tablas_res[[33]] %>%
  full_join(tablas_res[[99]], by = c("sector_id"= "id")) %>%
  replace_na(replace = list(nombre="", cargo="", entidad="")) %>%
  mutate(cargo=paste(nombre, cargo, entidad))

# Cargo político
# Para la migración hay que eliminar los cargos de cámaras
tablas_res[[36]]



# Congresista -------------------------------------------------------------
tablas_res[[39]] %>% count(ha_ejercido_cargo_diferente)

# transformación ----------------------------------------------------------

crear_bases(campos, tablas_res)


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
