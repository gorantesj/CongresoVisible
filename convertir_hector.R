library(tidyverse)
library(readxl)
library(stringr)
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

controlpol <- tabla_res[[123]]%>%
  mutate(id=row_number(),
         activo=NA_character_,
         usercreated=NA_character_,
         usermodifed=NA_character_,
         created_at=NA_character_,
         updated_at=NA_character_,
         estado_control_politico_id=NA_character_)

orden_diaitem <- tabla_res[[128]] %>%
             select(id, proposito, orden_del_dia_id, realizado)

controlpol_orden <- left_join(controlpol, orden_diaitem, by=c("itemdeordendeldia_ptr_id"="id"))

orden_dia_cuatrienio <- tabla_res[[131]] %>%
            select(id, cuatrienio_id)

controlpol_orden <- left_join(controlpol_orden, orden_dia_citacion,by=c("orden_del_dia_id"="id"))

orden_dia_comision <- tabla_res[[130]] %>%
           select(ordendeldia_id, comision_id)


controlpol_orden <- left_join(controlpol_orden, orden_dia_comision,by=c("orden_del_dia_id"="ordendeldia_id"))

corpo <- tabla_res[[38]]

comi <- read_csv("finales/idénticas/comisions.csv") %>%
        select(id, corporacion_id)

comi2 <- read_csv("finales/idénticas/corporacions.csv")
controlpol_orden <- left_join(controlpol_orden, comi,by=c("comision_id"="id"))
controlpol_orden <- rename(controlpol_orden, agenda_legislativa_actividad_id=itemdeordendeldia_ptr_id,
                           tipo_control_politico_id=tipo_id,
                           titulo=proposito,
                           fecha=fecha_proposicion)
controlpol_orden <- select(controlpol_orden, id, cuatrienio_id, comision_id, estado_control_politico_id, titulo, fecha, activo:updated_at,
                           tema_principal_id:tags, detalles, gacetas, numero_proposicion)%>%
  write_excel_csv("finales_hector/cambios en campos/control_politicos.csv")
# proyecto ley autor -------------------------------------------------------------


proyecto <- read_csv("Bases de datos nuevas/proyecto_ley_autor_otros.csv")

proyecto %>%  mutate(activo=NA_character_,
                       usermodifed=NA_character_,
                       usercreated=NA_character_) %>%
  write_excel_csv("finales_hector/idénticas/proyector_ley_autor_otros.csv")

# catalogo grado estudios----------

grad <- read_csv("Bases de datos nuevas/personas.csv") %>%
  select(grado_estudios) %>%
  unique() %>%
  filter(!is.na(grado_estudios)) %>%
  arrange(grado_estudios) %>%
  mutate(id=row_number()) %>%
  rename(nombre=grado_estudios) %>%
  mutate(activo=NA_character_,
         usercreated=NA_character_,
         usermodifed=NA_character_,
         created_at=NA_character_,
         updated_at=NA_character_) %>%
  mutate(nombre_final=if_else(nombre=="B", "Bachillerato",
                      if_else(nombre=="BI", "Bachillerato incompleto",
                      if_else(nombre=="D", "Doctorado",
                      if_else(nombre=="E", "Especialización",
                      if_else(nombre=="M", "Maestría",
                      if_else(nombre=="NR" | nombre=="NS" | nombre=="O"| nombre=="Po", nombre,
                      if_else(nombre=="P", "Primaria",
                      if_else(nombre=="PI", "Primaria incompleta",
                      if_else(nombre=="T", "Técnica",
                      if_else(nombre=="TE", "Tecnológica",
                      if_else(nombre=="TEI", "Tecnológica incompleta",
                      if_else(nombre=="TI", "Técnica incompleta",
                      if_else(nombre=="U", "Universitaria", "Universitaria Incompleta")))))))))))))) %>%
  select(-nombre) %>%
  select(id, nombre_final, activo:updated_at) %>%
  rename(nombre=nombre_final) %>%
  write_excel_csv("finales_hector/tablas nuevas/grado_estudios.csv")
# personas --------------------

personas <- read_csv("finales_hector/tablas nuevas/personas.csv")
grado <- read_csv("finales_hector/tablas nuevas/grado_estudios.csv") %>%
         select(nombre, id) %>%
         rename(grado_estudio_id=id)
#Limpiar nombres y apellidos de NAs

personasfinal <- personas %>%
            filter(!is.na(nombres)) %>%
            filter(!is.na(apellidos))

#Quitar símbolos
personasfinal <- mutate(personasfinal, nombres=stringr::str_replace_all(nombres, pattern = "[^[:alnum:][:space:]]", replacement = ""))

#Quitar números
personasfinal <- mutate(personasfinal, nombres=stringr::str_replace_all(nombres, pattern = "[^[:alpha:][:space:]]", replacement = ""))

#Homologar mayus y minus
personasfinal <- mutate(personasfinal, nombres=stringr::str_to_lower(nombres))
personasfinal <- mutate(personasfinal, nombres=stringr::str_to_title(nombres))

#Lo mismo para apellidos
personasfinal <- mutate(personasfinal, apellidos=stringr::str_replace_all(apellidos, pattern = "[^[:alnum:][:space:]]", replacement = ""))
personasfinal <- mutate(personasfinal, apellidos=stringr::str_replace_all(apellidos, pattern = "[^[:alpha:][:space:]]", replacement = ""))
personasfinal <- mutate(personasfinal, apellidos=stringr::str_to_lower(apellidos))
personasfinal <- mutate(personasfinal, apellidos=stringr::str_to_title(apellidos))

personasfinal <-  personasfinal%>%
   mutate(activo=  NA_character_,
   usercreated=NA_character_,
   usermodifed=NA_character_,
   perfil_educativo=NA_character_) %>%
   rename(municipio_id_nacimiento=municipio_nacimiento_id)


personasfinal <- left_join(personasfinal,grado, by=c("grado_estudios"="nombre"))


personasfinal <- personasfinal %>%
                select(-grado_estudios) %>%
  write_excel_csv("finales_hector/tablas nuevas/personas.csv")







# secretario----------

secretario <- tabla_res[[63]] %>%
              mutate(id=row_number(),
                     activo=NA_character_,
                     usercreated=NA_character_,
                     usermodifed=NA_character_,
                     created_at=NA_character_,
                     updated_at=NA_character_) %>%
              rename("persona_id"="persona_ptr_id")

personas <- read_csv("finales_hector/tablas nuevas/personas.csv") %>%
            select(id)

secretario <- inner_join(secretario, personas, by=c("persona_id"="id")) %>%
              select(id,persona_id, activo:updated_at) %>%
              write_excel_csv("finales_hector/cambios en campos/secretarios.csv")






# control_politico_citados----------


control_citados <- tabla_res[[125]] %>%
                   mutate(persona_id=citado_id+14657,
                   asistencia_id=if_else(asiste==T, 1,
                                 if_else(asiste==F & delega_asistencia==T & excuso==F, 2,
                                 if_else(asiste==F & delega_asistencia==F & excuso==T, 3,
                                 if_else(asiste==F & delega_asistencia==T & excuso==T, 4, 5))))) %>%
                   select(-asiste, -delega_asistencia, -excuso, -citado_id) %>%
                   rename(control_politico_id=citacion_id) %>%
                   mutate(
                   tipo_citado=1,
                   activo=NA_character_,
                   usercreated=NA_character_,
                   usermodifed=NA_character_,
                   created_at=NA_character_,
                   updated_at=NA_character_)






#Pruebas
personas <- read_csv("FinalesDeborah/tablas nuevas/personas.csv") %>%


siestan <- inner_join(control_citados, personas, by=c("persona_id"="id"))
noestan <- anti_join(control_citados, personas, by=c("persona_id"="id"))
set.seed(2021)
muestra <- control_citados %>% sample_n(5)
citacion_prueba <- tabla_res[[123]] %>% filter(itemdeordendeldia_ptr_id %in% muestra$citacion_id)


#
# prueba <- tabla_res[[124]] %>% filter(id %in% muestra)

orden_diaitem <- tabla_res[[128]] %>%
  select(id,  proposito, realizado, orden_del_dia_id)

citacion_prueba <- left_join(citacion_prueba, orden_diaitem, by=c("itemdeordendeldia_ptr_id"="id"))

orden_dia_cuatrienio <- tabla_res[[131]] %>%
  select(id, fecha_programada, fecha_realizada, comentarios, cuatrienio_id)


citacion_prueba <- left_join(citacion_prueba, orden_dia_cuatrienio,by=c("orden_del_dia_id"="id"))
#Fin de Pruebas


# catalogo asistencias------------------

asistencias <- read_csv("finales_hector/cambios en campos/control_politico_citado.csv") %>%
               select(asistencia_id) %>%
               filter(!is.na(asistencia_id)) %>%
               unique() %>%
               rename(id=asistencia_id) %>%
               mutate(nombre=if_else(id==1, "Asiste",
                              if_else(id==2, "Delega asistencia",
                              if_else(id==3, "Excuso",
                              if_else(id==4, "Delega asistencia y excuso", "No asiste, no delega y no excuso")))))




# Invitado asistente citacion----------------

invi <- tabla_res[[127]] %>%
        mutate(persona_id=citado_id+14657,
         asistencia_id=if_else(asiste==T, 1,
                       if_else(asiste==F & delega_asistencia==T & excuso==F, 2,
                       if_else(asiste==F & delega_asistencia==F & excuso==T, 3,
                       if_else(asiste==F & delega_asistencia==T & excuso==T, 4, 5))))) %>%
  select(-asiste, -delega_asistencia, -excuso, -citado_id) %>%
  rename(control_politico_id=citacion_id) %>%
  mutate(tipo_citado=0,
         activo=NA_character_,
         usercreated=NA_character_,
         usermodifed=NA_character_,
         created_at=NA_character_,
         updated_at=NA_character_)

final <- bind_rows(invi, control_citados) %>%
  write_excel_csv("finales_hector/cambios en campos/control_politico_citados.csv")




# agenda legislativas------

agenda_camaras <- tabla_res[[128]] %>%
                  select()
agenda_comisiones <- tabla_res[[129]] %>%
                     rename(comision_id=id,
                            agenda_legislativa=ordendeldia_id,
                            corporacion_id=camara_id) %>%
                     arrange(desc(agenda_legislativa)) %>%
                     mutate(id=row_number(),
                            activo=NA_character_,
                            usercreated=NA_character_,
                            usermodifed=NA_character_,
                            created_at=NA_character_,
                            updated_at=NA_character_
                            ) %>%
                     select(id, agenda_legislativa,corporacion_id, comision_id, id:updated_at) %>%
                     write_excel_csv("finales_hector/tablas nuevas/agenda_legislativa_comisions.csv")


agenda_legislativas <- read_csv("Bases de datos nuevas/agenda_legislativas.csv") %>%
                       mutate(
                         activo=NA_character_,
                         usercreated=NA_character_,
                         usermodifed=NA_character_,
                         created_at=NA_character_,
                         updated_at=NA_character_
                          ) %>%
                         write_excel_csv("finales_hector/idénticas/agenda_legislativas.csv")





# tipo citacions---------

tipocita <- read_csv("Bases de datos nuevas/tipo_citacions.csv") %>%
  mutate(activo=NA_character_,
         usercreated=NA_character_,
         usermodifed=NA_character_,
         created_at=NA_character_,
         updated_at=NA_character_) %>%
  write_excel_csv("finales_hector/idénticas/tipo_citacions.csv")

# proyecto autors---------

proyecto_autor <- read_csv("Bases de datos nuevas/proyecto_ley_autors.csv")


proyecto_autor_congresista <- read_csv("Bases de datos nuevas/proyecto_ley_autors.csv")

congresista <- read_csv("finales/cambio en campos/congresistas.csv") %>%
              select(id, persona_id)

congresita_excel <- tabla_res[[39]]

personas <- tabla_res[[54]] %>%
            select(id, nombres, apellidos)


partido <- tabla_res[[51]]


tabla_gerardo <- tabla_res[[143]]

proyecto_autor_congresista<- left_join(proyecto_autor_congresista, personas, by=c("congresista_id"="id"))

son_na <- proyecto_autor_congresista %>%
          filter(is.na(nombres))


proyecto_autor_congresista <- left_join(proyecto_autor_congresista, congresista, by=c("congresista_id"="id"))


proyecto_autor %>% filter(tipo_autor_id==24) %>% View()
proyecto_ley_comisions <- read_csv("Bases de datos nuevas/proyecto_ley_comisions.csv")%>%
  mutate(activo=NA_character_,
         usercreated=NA_character_,
         usermodifed=NA_character_,
         created_at=NA_character_,
         updated_at=NA_character_) %>%
  write_excel_csv("finales_hector/idénticas/proyecto_ley_comisions.csv")


# transformación ----------------------------------------------------------

crear_bases(campos %>% filter(num==143), tabla_res)

proyectos <- tabla_res[[144]]
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
