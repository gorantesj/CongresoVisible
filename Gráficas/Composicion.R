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
circ <- read_csv("Para Jesús/circunscripcions.csv")
depa <- read_csv("Para Jesús/departamentos.csv")


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




# Partidos - Distribución de asientos - Senadores --------------------

aux <- congresistas %>%
  left_join(legislatura %>%  select(legislatura = id, cuatrienio_id),
            by = "cuatrienio_id") %>%
  left_join(corporacion %>%  select(id, corporacion =nombre),
            by = c("corporacion_id" = "id")) %>%
  left_join(partidos %>%  select(id,partido  = nombre, color),
            by = c("partido_id" = "id")) %>%
  left_join(personas %>%  select(persona_id = id, nombres, apellidos),
            by = "persona_id") %>%
  mutate(nombre_congresista = paste(nombres, apellidos)) %>%
  group_by(corporacion, partido, cuatrienio_id, legislatura, color) %>%
  summarise(n = n(), miembros = paste(nombre_congresista, collapse=", "))

aux %>% filter(corporacion == "Senado", legislatura ==26) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(label = partido, y = n, name = partido), type = "item") %>%
  hc_title(text = "Asientos por partido") %>%
  hc_subtitle(text = "Senadores") %>%
  hc_tooltip(pointFormat = '<b>{point.n}</b><br> miembros: {point.miembros}') %>%
  hc_add_theme( thm)


# Partidos - Distribución de asientos - Cámara de Representantes --------------------


aux %>% filter(corporacion == "Cámara de Representantes",  legislatura ==26) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(label = partido, y = n, name = partido), type = "item") %>%
  hc_title(text = "Asientos por partido") %>%
  hc_subtitle(text = "Cámara de Representantes") %>%
  hc_tooltip(pointFormat = '<b>{point.n}</b><br> miembros: {point.miembros}') %>%
  hc_add_theme( thm)

# Partidos - Edad y sexo --------------------------------------------------

aux <-congresistas %>%
  left_join(legislatura %>%  select(legislatura = id, cuatrienio_id),
            by = "cuatrienio_id") %>%
  left_join(corporacion %>%  select(id, corporacion =nombre),
            by = c("corporacion_id" = "id")) %>%
  left_join(partidos %>%  select(id,partido  = nombre, color),
            by = c("partido_id" = "id")) %>%
  left_join(personas %>%  select(persona_id = id, nombres, apellidos,
                                 genero_id, fechaNacimiento),
            by = "persona_id") %>%
  left_join(sexo %>% select(genero_id = id, genero = nombre), by = "genero_id") %>%
  mutate(nombre_congresista = paste(nombres, apellidos),
         fechaNacimiento = dmy(fechaNacimiento),
         edad =as.integer(as.numeric( today()-fechaNacimiento)/365 ),
         rango = cut(edad,c(17,29,39,49,59,69,79, 89,100),
                     labels = c("18 - 29","30 - 49","40 - 49",
                                "50 - 59","60 - 69", "70 - 79",
                                "80 - 89", "90 o más"))) %>%
  filter( !is.na(edad)) %>%
  group_by(partido, genero) %>%
  summarise(media = round(mean(edad)), mediana = round(median(edad)),
            minimo = round(min(edad)), maxi = round(max(edad)) ) %>%
  arrange(desc(media))
aux %>%
  hchart(hcaes(x = partido,  y= mediana, group = genero), type = "scatter") %>%
  hc_add_series(aux, hcaes(x = partido, high = maxi, low = minimo, group = genero ),
                type = "errorbar") %>%
  hc_plotOptions(errorbar = list(showInLegend = F)) %>%
  hc_title(text = "Edad mediana de los congresistas por partido") %>%
  hc_xAxis(title = list(text = "Partido")) %>%
  hc_yAxis(title = list(text = "Edad mínima, mediana y máxima")) %>%
  hc_chart(inverted= T) %>%
  hc_add_theme( thm)

# Congresistas - Pirámide Poblacional de Edad y sexo ----------------------

aux <-congresistas %>%
  left_join(legislatura %>%  select(legislatura = id, cuatrienio_id),
            by = "cuatrienio_id") %>%
  left_join(corporacion %>%  select(id, corporacion =nombre),
            by = c("corporacion_id" = "id")) %>%
  left_join(personas %>%  select(persona_id = id, nombres, apellidos,
                                 genero_id, fechaNacimiento),
            by = "persona_id") %>%
  left_join(sexo %>% select(genero_id = id, genero = nombre), by = "genero_id") %>%
  mutate(nombre_congresista = paste(nombres, apellidos),
         fechaNacimiento = dmy(fechaNacimiento),
         edad =as.integer(as.numeric( today()-fechaNacimiento)/365 ),
         rango = cut(edad,c(17,29,39,49,59,69,79, 89,100),
                     labels = c("18 - 29","30 - 39","40 - 49",
                                "50 - 59","60 - 69", "70 - 79",
                                "80 - 89", "90 o más"))) %>%
  count(corporacion, legislatura, genero, rango)

aux%>%  filter(!is.na(rango), legislatura == 28,
               corporacion == "Cámara de Representantes") %>%
  mutate(n2 = case_when(genero == "Masculino"~ n*-1, T~ n %>%  as.double())) %>%
  arrange(desc(rango) )%>%
  hchart(hcaes(y = n2, x = rango, group = genero), type = "bar") %>%
  hc_plotOptions(bar = list(stacking = T))%>%
  hc_title(text = "Pirámide poblacional") %>%
  hc_subtitle(text = "Cámara de Representantes") %>%
  hc_xAxis(title = list(text = "Rango de edad")) %>%
  hc_yAxis(title = list(text = "Total de congresistas")) %>%
  hc_add_theme( thm)

aux%>%  filter(!is.na(rango), legislatura == 28,
               corporacion == "Senado") %>%
  mutate(n2 = case_when(genero == "Masculino"~ n*-1, T~ n %>%  as.double())) %>%
  arrange(desc(rango) )%>%
  hchart(hcaes(y = n2, x = rango, group = genero), type = "bar") %>%
  hc_plotOptions(bar = list(stacking = T))%>%
  hc_title(text = "Pirámide poblacional") %>%
  hc_subtitle(text = "Senado") %>%
  hc_xAxis(title = list(text = "Rango de edad")) %>%
  hc_yAxis(title = list(text = "Total de congresistas")) %>%
  hc_add_theme( thm)


# congresistas - Cuatrienios en el Congreso -------------------------------


aux <- congresistas %>%
  left_join(corporacion %>%  select(id, corporacion =nombre),
            by = c("corporacion_id" = "id")) %>%
  count(persona_id, corporacion)

aux2 <-  data_to_boxplot(data = aux  , variable = n, group_var = corporacion)

highchart() %>%
  hc_xAxis(type = "category") %>%
  hc_add_series_list(aux2)  %>%
  hc_title(text = "Cuatrienios en el Congreso") %>%
  hc_xAxis(title = list(text = "Corporación")) %>%
  hc_yAxis(title = list(text = "Cuatrienios en el congreso"), tickAmount = 6) %>%
  hc_plotOptions(series = list(showInLegend = F)) %>%
  hc_chart(inverted = T)%>%
  hc_add_theme( thm)


# congresistas - investigaciones ------------------------------------------

inv %>%
  left_join(tipo_inv %>%  select(tipo_investigacion_id = id, tipo = nombre),
            by = "tipo_investigacion_id") %>%
  left_join(congresistas %>%
              select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
            by = "congresista_id") %>%
  left_join(corporacion %>%  select(id, corporacion =nombre),
            by = c("corporacion_id" = "id")) %>%
  # left_join(legislatura %>%  select(legislatura = id, cuatrienio_id),
  #           by = "cuatrienio_id") %>%
  left_join(personas %>%  select(persona_id = id, nombres, apellidos),
            by = "persona_id") %>%
  mutate(nombre_congresista = paste(nombres, apellidos),
         inves = if_else(str_detect(tipo,
                                    pattern = "Atención: Pérdida de investidura"),
                         "Pérdida de investidura", "Otro"),
         inves = if_else(str_detect(tipo,
                                    pattern = "Renunció a la curul"),
                         "Renunció a la curul", inves),
         inves = if_else(str_detect(tipo,
                                    pattern = "Silla Vacía"),
                         "Silla Vacía", inves)) %>%
  group_by(inves, corporacion ) %>%
  summarise(n = n(), miembros = paste(nombre_congresista, collapse=", ")) %>%
  mutate(corporacion = case_when(is.na(corporacion)~"No congresista", T~corporacion)) %>%
  filter(!inves == "Otro") %>%
  arrange(desc(n)) %>%
  hchart(hcaes(x = inves, y = n, group = corporacion), type = "bar") %>%
  hc_tooltip(pointFormat = '<b>{point.n}</b><br> miembros: {point.miembros}')%>%
  hc_title(text = "Investigaciones") %>%
  hc_xAxis(title = list(text = "Tipo")) %>%
  hc_yAxis(title = list(text = "Total")) %>%
  hc_add_theme( thm)


# congresistas - Cantidad de PL presentados por sexo ----------------------

pl_autor %>%
  left_join(congresistas %>%
              select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
            by = "congresista_id") %>%
  left_join(personas %>%  select(persona_id = id, nombres, apellidos, genero_id),
            by = "persona_id") %>%
  mutate(nombre_congresista = paste(nombres, apellidos)) %>%
  left_join(sexo %>% select(genero_id = id, genero = nombre), by = "genero_id") %>%
  filter(cuatrienio_id== 8) %>%
  select(congresista_id, proyecto_ley_id, genero) %>%
  group_by(genero, proyecto_ley_id) %>%  summarise() %>%  count(genero) %>%
  hchart(hcaes(x = genero, size = n, y= 1, group = genero), type = "bubble") %>%
  hc_xAxis(lineWidth =0, labels = list(enabled= F), title= list(enabled= F)) %>%
  hc_yAxis(lineWidth =0, labels = list(enabled= F), title= list(enabled= F),
           gridLineWidth =0) %>%
  hc_plotOptions(bubble = list(colorByPoint = F,
                               dataLabels = list(enabled = T,
                                                 # inside = F,
                                                 style = list(fontSize = "20px")),
                               maxSize = "50%",
                               minSize = "20%",
                               marker = list(fillOpacity = .91,lineWidth=0))) %>%
  hc_title(text = "Total de Proyectos de Ley presentados por género") %>%
  hc_add_theme( thm)


# congresistas - partidos -------------------------------------------------

congresistas %>%
  left_join(legislatura %>%  select(legislatura = id, cuatrienio_id),
            by = "cuatrienio_id") %>%
  left_join(corporacion %>%  select(id, corporacion =nombre),
            by = c("corporacion_id" = "id")) %>%
  left_join(partidos %>%  select(id,partido  = nombre, color),
            by = c("partido_id" = "id")) %>%
  left_join(personas %>%  select(persona_id = id, nombres, apellidos),
            by = "persona_id") %>%
  mutate(nombre_congresista = paste(nombres, apellidos)) %>%
  group_by(corporacion, legislatura, partido) %>%  summarise(n = n()) %>%
  filter(corporacion == "Cámara de Representantes", legislatura == 28) %>%
  hchart(hcaes(x = partido, value= n), type = "treemap")%>%
  hc_plotOptions(treemap = list(colorByPoint = T, dataLabels = list(enabled = F))) %>%
  hc_title(text = "Congresistas por partido") %>%
  hc_add_theme( thm)
