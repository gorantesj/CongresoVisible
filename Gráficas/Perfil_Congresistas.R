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
thm <- hc_theme(
  colors = c("#66CCFF", "#17789E", "#6AEEB0", "#379E80", "#F78031", "#374B9E"),
  chart = list( style = list(fontFamily = "Bell MT", fontSize = "20px")),
  title = list( style = list(  color = "#17789E", fontSize = "22px", fontWeight= 'bold')),
  subtitle = list(  style = list(    color = "#666666"  ) ),
  legend = list( itemStyle = list(  color = "black"   ),
                 itemHoverStyle = list( color = "gray")  ),
  tooltip = list(borderWidth =0, shadow = F, style = list( fontSize = "16px")),
  yAxis = list(lineWidth = 3,title = list(style= list(fontSize = "16px")),
               tickAmount = 5,
               labels = list(style= list(fontSize = "15px"))),
  xAxis = list(lineWidth = 0,title = list(style= list(fontSize = "16px")),
               labels = list(style= list(fontSize = "18px"))),
  plotOptions = list(bar = list(borderRadius =5), treemap = list(borderRadius =5))
)


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
        filter(congresista_id == 1) %>%
        group_by(nombre_temas) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        dplyr::arrange(-count)

  aux2 <- aux %>%
          mutate(n = 1:nrow(aux)) %>%
          filter(n <= 10) %>% select(-n)

  otros <- aux %>%
          mutate(n = 1:nrow(aux)) %>%
          filter(n > 10) %>%
          dplyr::summarise(count = n(), nombre_temas = "Otros")


  aux2 %>% bind_rows(otros) %>%
  hchart("pie", hcaes(x = nombre_temas, y = count)) %>%
  hc_title(text = "Temas más recurrentes") %>%
  hc_add_theme(thm)

  # billboarder() %>% bb_donutchart(aux2 %>% bind_rows(otros)) %>% bb_legend(position = 'right')


# Número de Proyectos de Ley como autor --------------------

aux <- pl_autor %>%
  dplyr::left_join(congresistas %>%
    select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
    by = "congresista_id") %>%
  dplyr::left_join(personas %>%
    select(persona_id = id, nombres, apellidos, genero_id),
    by = "persona_id") %>%
  dplyr::mutate(nombre_congresista = paste(nombres, apellidos))

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
  hc_title(text = "Número de Proyectos de Ley como autor") %>%
  hc_add_theme(thm)


# Número de Proyectos de Ley como ponente --------------------

aux <- pl_ponentes %>%
        dplyr::left_join(congresistas %>%
          select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
            by = "congresista_id") %>%
        dplyr::left_join(personas %>%
          select(persona_id = id, nombres, apellidos, genero_id),
            by = "persona_id") %>%
        dplyr::mutate(nombre_congresista = paste(nombres, apellidos))

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
  hc_title(text = "Número de Proyectos de Ley como ponente") %>%
  hc_add_theme(thm)


# Número de citaciones --------------------

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
  hc_add_theme(thm)


# Juntas -----------------

# aux <- pl_autor %>%
#       dplyr::left_join(congresistas %>%
#         select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
#         by = "congresista_id") %>%
#       dplyr::left_join(personas %>%
#         select(persona_id = id, nombres, apellidos, genero_id),
#         by = "persona_id") %>%
#       dplyr::mutate(nombre_congresista = paste(nombres, apellidos)) %>%
#       dplyr::filter(congresista_id == 1) %>%
#       dplyr::summarise(n = n(), categoria = "Número de Proyectos de Ley como autor") %>%
#       dplyr::bind_rows(
#         pl_ponentes %>%
#         dplyr::left_join(congresistas %>%
#           select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
#           by = "congresista_id") %>%
#         dplyr::left_join(personas %>%
#           select(persona_id = id, nombres, apellidos, genero_id),
#           by = "persona_id") %>%
#         dplyr::mutate(nombre_congresista = paste(nombres, apellidos)) %>%
#         dplyr::filter(congresista_id == 1) %>%
#         dplyr::summarise(n = n(), categoria = "Número de Proyectos de Ley como ponente")) %>%
#         dplyr::bind_rows(
#         congresistas %>% select(congresista_id = id, persona_id) %>%
#         dplyr::left_join(personas %>%
#           select(persona_id = id, nombres, apellidos),
#           by = "persona_id") %>%
#         dplyr::mutate(nombre_congresista = paste(nombres, apellidos)) %>%
#         dplyr::left_join(control_pol %>%
#           select(congresista_id, citados_id= id),
#           by = "congresista_id") %>%
#         dplyr::filter(congresista_id == 1) %>%
#         dplyr::summarise(n = n(), categoria = "Número de citaciones"))
#
#
#   aux %>%
#   hchart(hcaes(x = 3:1, size = n, y = 1, group = categoria), type = "bubble") %>%
#   hc_xAxis(lineWidth =0, labels = list(enabled= F), title= list(enabled= F)) %>%
#   hc_yAxis(lineWidth =0, labels = list(enabled= F), title= list(enabled= F), gridLineWidth =0) %>%
#   hc_plotOptions(bubble = list(colorByPoint = F,
#     dataLabels = list(enabled = T, style = list(fontSize = "30px")),
#     maxSize = "50%",
#     minSize = "20%",
#     marker = list(fillOpacity = .91,lineWidth=0))) %>%
#   hc_title(text = "Perfil del congresista") %>%
#   hc_add_theme(thm)
