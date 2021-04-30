library(tidyverse)
library(highcharter)
library(readr)
library(lubridate)

#Lectura de bases de datos
congresistas <- read_csv("Para Jesús/congresistas.csv")
corporacion <- read_csv("Para Jesús/corporacions.csv")
legislatura <- read_csv("Para Jesús/legislaturas.csv")
partidos <- read_csv("Para Jesús/partidos.csv")
cuatrienios <- read_csv("Para Jesús/cuatrienios.csv")
personas <- read_csv("Para Jesús/personas.csv")
reemplazos <- read_csv("Para Jesús/congresista_reemplazos.csv")
sexo <- read_csv("Para Jesús/generos.csv")
inv <- read_csv("Para Jesús/investigacions.csv")
tipo_inv<-read_csv("Para Jesús/tipo_investigacions.csv")
pl <- read_csv("Para Jesús/proyecto_leys.csv")
pl_autor <- read_csv("Para Jesús/proyecto_ley_autors.csv")
pl_tipo <- read_csv("Para Jesús/tipo_proyectos.csv")
pl_ponentes <- read_csv("Para Jesús/proyecto_ley_ponentes.csv")
circ <- read_csv("Para Jesús/circunscripcions.csv")
depa <- read_csv("Para Jesús/departamentos.csv")
temas <- read_csv("Para Jesús/temas.csv")
control <- read_csv("Para Jesús/control_politicos.csv")
#control_pol1 <- read_csv("Para Jesús/control_politico_citados.csv")
control_pol <- read_csv("Para Jesús/control_politico_citantes.csv")

# Tema
source("Gráficas/tema.R")



# Temas más recurrentes  --------------------

aux0 <- pl %>%
        dplyr::select(proyecto_ley_id = id, tema_id_principal) %>%
        dplyr::left_join(pl_autor,
          by = "proyecto_ley_id") %>%
        dplyr::left_join(congresistas %>%
          select(congresista_id = id, persona_id),
          by = "congresista_id") %>%
        dplyr::left_join(personas %>%
          select(persona_id = id, nombres, apellidos),
          by = "persona_id") %>%
        dplyr::mutate(nombre_congresista = paste(nombres, apellidos)) %>%
        dplyr::left_join(temas %>%
          select(tema_id_principal = id, nombre_temas = nombre),
          by = "tema_id_principal")%>%
        filter(congresista_id == 3) %>%
        group_by(nombre_temas) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        mutate(nombre_temas_2 = case_when(count < (0.7)*mean(count) ~ "", T~nombre_temas) ) %>%
        dplyr::arrange(-count)

    aux0 %>%
    hchart(hcaes(x = nombre_temas, value = count), type = "treemap")%>%
    hc_plotOptions(treemap = list(colorByPoint = T,
      dataLabels = list(enabled = T, format = '{point.nombre_temas_2}'))) %>%
    hc_title(text = "Temas más recurrentes") %>%
    hc_add_theme(thm)


#  Número de Proyectos de Ley como autor & Número de Proyectos de Ley como ponente  --------------------

    aux <- pl_autor %>%
      dplyr::left_join(congresistas %>%
                         select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
                       by = "congresista_id") %>%
      dplyr::left_join(personas %>%
                         select(persona_id = id, nombres, apellidos, genero_id),
                       by = "persona_id") %>%
      dplyr::mutate(nombre_congresista = paste(nombres, apellidos)) %>%
      dplyr::filter(congresista_id == 3) %>%
      dplyr::summarise(n = n(), categoria = "Número de Proyectos de Ley como autor") %>%
      dplyr::bind_rows(
        pl_ponentes %>%
          dplyr::left_join(congresistas %>%
                             select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
                           by = "congresista_id") %>%
          dplyr::left_join(personas %>%
                             select(persona_id = id, nombres, apellidos, genero_id),
                           by = "persona_id") %>%
          dplyr::mutate(nombre_congresista = paste(nombres, apellidos)) %>%
          dplyr::filter(congresista_id == 3) %>%
          dplyr::summarise(n = n(), categoria = "Número de Proyectos de Ley como ponente"))

    aux %>%
      hchart( "pie", hcaes(x = categoria, y = n)) %>%
      hc_add_theme(thm) %>%
      hc_tooltip(enabled = T) %>%
      hc_tooltip(pointFormat = '<b>{point.n}</b>') %>%
      hc_title(text = "Perfil del congresista")


# Número de citaciones  --------------------

aux <- congresistas %>% select(congresista_id = id, persona_id) %>%
       dplyr::left_join(personas %>%
       dplyr::select(persona_id = id, nombres, apellidos),
              by = "persona_id") %>%
      dplyr::mutate(nombre_congresista = paste(nombres, apellidos)) %>%
      dplyr::left_join(control_pol %>%
              select(congresista_id, citados_id= id),
              by = "congresista_id")

aux %>%
  dplyr::filter(congresista_id == 1) %>%
  dplyr::summarise(n = n()) %>%
  hchart("bubble", hcaes(x = 1, y = 1, size = n)) %>%
  hc_xAxis(lineWidth =0, labels = list(enabled= F), title= list(enabled= F)) %>%
  hc_yAxis(lineWidth =0, labels = list(enabled= F), title= list(enabled= F), gridLineWidth =0) %>%
  hc_plotOptions(bubble = list(colorByPoint = F,
                dataLabels = list(enabled = T, style = list(fontSize = "50px")),
                maxSize = "50%",
                minSize = "20%",
                marker = list(fillOpacity = .91,lineWidth=0))) %>%
  hc_title(text = "Número de citaciones") %>%
  hc_tooltip(enabled = F) %>%
  hc_add_theme(thm)
