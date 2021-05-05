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
source("Gráficas/tema.R")

# Partidos - Distribución de asientos - Senadores --------------------

aux <- congresistas %>%
  left_join(legislatura %>%  select(legislatura = id, cuatrienio_id),
            by = "cuatrienio_id") %>%
  left_join(corporacion %>%  select(corporacion_id= id, corporacion =nombre),
            by = "corporacion_id" ) %>%
  left_join(partidos %>%  select(partido_id =id,partido  = nombre, color),
            by = "partido_id" ) %>%
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
         rango  = case_when(edad < 18 ~ "Menores de 18",
                            edad>=18 & edad<30~ "18 - 29",
                            edad>=30 & edad<40~ "30 - 39",
                            edad>=40 & edad<50~ "40 - 49",
                            edad>=50 & edad<60~ "50 - 59",
                            edad>=60 & edad<70~ "60 - 69",
                            edad>=70 & edad<80~ "70 - 79",
                            edad>=80~ "80 o más")) %>%
  # filter( !is.na(edad)) %>%
  select(legislatura, corporacion, partido, edad, rango, genero) %>%
   filter(legislatura== 28)

aux %>%  group_by(partido) %>%  summarise(mediana = median(edad)) %>%
  arrange(desc(mediana)) %>%
  hchart(hcaes(x = partido, y = mediana), type = "lollipop") %>%
  hc_title(text = "Edad mediana por partido") %>%
  hc_xAxis(title = list(text = "Partido")) %>%
  hc_yAxis(title = list(text = "Mediana")) %>%
  hc_add_theme(thm) %>%
  hc_chart(inverted= T)

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
         rango  = case_when(edad < 18 ~ "Menores de 18",
                            edad>=18 & edad<30~ "18 - 29",
                            edad>=30 & edad<40~ "30 - 39",
                            edad>=40 & edad<50~ "40 - 49",
                            edad>=50 & edad<60~ "50 - 59",
                            edad>=60 & edad<70~ "60 - 69",
                            edad>=70 & edad<80~ "70 - 79",
                            edad>=80~ "80 o más")) %>%
  count(corporacion, legislatura,cuatrienio_id, genero, rango)

aux%>%  filter( legislatura == 28,
               corporacion == "Cámara de Representantes") %>%
  mutate(n2 = case_when(genero == "Masculino"~ n*-1, T~ n %>%  as.double()),
         rango = factor(rango, c("Menores de 18", "18 - 29","30 - 39","40 - 49",
                                 "50 - 59","60 - 69", "70 - 79", "80 o más"))) %>%
  arrange(desc(rango) )%>%
  hchart(hcaes(y = n2, x = rango, group = genero), type = "bar") %>%
  hc_plotOptions(bar = list(stacking = T))%>%
  hc_title(text = "Pirámide poblacional") %>%
  hc_subtitle(text = "Cámara de Representantes") %>%
  hc_xAxis(title = list(text = "Rango de edad")) %>%
  hc_yAxis(title = list(text = "Total de congresistas"),
           labels  = list( formatter = JS("function(){ return Math.abs(this.value); }")) )%>%
  hc_tooltip(shared = T,
             pointFormat = '{series.name}: <b>{point.n}</b><br/>',
             headerFormat= '<span style="font-size: 20px"><b>Rango: {point.key} años</span><br/></b>') %>%
  hc_add_theme( thm)

aux%>%  filter( legislatura == 28,
               corporacion == "Senado") %>% ungroup() %>%
  mutate(n2 = case_when(genero == "Masculino"~ n*-1, T~ n %>%  as.double()),
         rango = factor(rango, c("Menores de 18", "18 - 29","30 - 39","40 - 49",
                                 "50 - 59","60 - 69", "70 - 79", "80 o más"))) %>%
  arrange(desc(rango) )%>%
  hchart(hcaes(y = n2, x = rango, group = genero), type = "bar") %>%
  hc_plotOptions(bar = list(stacking = T))%>%
  hc_title(text = "Pirámide poblacional") %>%
  hc_subtitle(text = "Senado") %>%
  hc_xAxis(title = list(text = "Rango de edad")) %>%
  hc_yAxis(title = list(text = "Total de congresistas"),
           labels  = list( formatter = JS("function(){ return Math.abs(this.value); }")) )%>%
  hc_tooltip(shared = T,
             pointFormat = '{series.name}: <b>{point.n}</b><br/>',
             headerFormat= '<span style="font-size: 20px"><b>Rango: {point.key} años</span><br/></b>') %>%
  hc_add_theme( thm)


# congresistas - Cuatrienios en el Congreso -------------------------------


aux <- congresistas %>%
  # left_join(legislatura %>%  select(legislatura = id, cuatrienio_id),
  #           by = "cuatrienio_id") %>%
  left_join(corporacion %>%  select(id, corporacion =nombre),
            by = c("corporacion_id" = "id")) %>%
  count(persona_id, corporacion)

hchart(aux %>%  filter(corporacion == "Senado") %>%  pull(n))
aux %>% filter(corporacion== "Senado") %>%
  hchart( hcaes(group = corporacion, x = n), type = "histogram")

hchart( density(aux %>%  filter(corporacion == "Senado") %>%  pull(n)), type = "area",
  color = "steelblue") %>%
  hc_add_theme(thm)

hchart( density(aux %>%  filter(corporacion == "Cámara de Representantes") %>%
                  pull(n)), type = "area",
        color = "steelblue", name = "") %>%
  hc_plotOptions(density = list(legend= list(enabled = F))) %>%
  hc_add_theme(thm)


 aux %>%
  group_by(corporacion) %>%
  summarise(media = mean(n), minimo = min(n), maxi = max(n), media2=round( mean(n), 1)) %>%
  hchart(hcaes(x = corporacion, high = maxi, low = minimo , group= corporacion),
         type = "errorbar") %>%
  hc_add_series(aux %>%
                  group_by(corporacion) %>%
                  summarise(media = mean(n), minimo = min(n), maxi = max(n), media2=round( mean(n), 1)),
                hcaes(x = corporacion,  y= media, group= corporacion), type = "scatter") %>%
  hc_plotOptions(errorbar = list(showInLegend = F,tooltip = list(enabled = F))) %>%
  hc_title(text = "Cuatrienios ") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Número de cuatrienios"), min=0) %>%
  hc_tooltip(pointFormat = 'Media: {point.media2}<br>Mínimo: {point.minimo}<br>Máximo: {point.maxi}') %>%
  hc_chart(inverted= T) %>%
  hc_add_theme( thm)




# congresistas - investigaciones ------------------------------------------

inv %>%
  left_join(tipo_inv %>%  select(tipo_investigacion_id = id, tipo = nombre),
            by = "tipo_investigacion_id") %>%
  left_join(congresistas %>%
              select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
            by = "persona_id") %>%
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
  mutate(miembros = case_when(corporacion== "No congresista"~"", T~miembros),
         corporacion = factor(corporacion, c("Cámara de Representantes", "Senado", "No congresista"))) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(x = inves, y = n, group = corporacion), type = "bar") %>%
  hc_tooltip(pointFormat = '<b>{series.name}: {point.n}</b><br> Miembros: {point.miembros}')%>%
  hc_title(text = "Investigaciones") %>%
  hc_xAxis(title = list(text = "Tipo")) %>%
  hc_yAxis(title = list(text = "Total")) %>%
  hc_add_theme( thm)



# congresistas - Cantidad de PL presentados por sexo ----------------------

pl_autor %>%
  left_join(congresistas %>%
              select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
            by = "congresista_id") %>%
   # left_join(legislatura %>%  select(legislatura = id, cuatrienio_id),
   #           by = "cuatrienio_id") %>%
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
  hc_add_theme( thm) %>%
  hc_tooltip(enabled= F)


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
   filter(legislatura== 28) %>%
  mutate(nombre_congresista = paste(nombres, apellidos)) %>%
  group_by(corporacion, legislatura, partido) %>%  summarise(n = n()) %>%
  mutate(partido2 = case_when(n< mean(n)~"", T~partido) ) %>%
  filter(corporacion == "Cámara de Representantes", legislatura == 28) %>%
  hchart(hcaes(x = partido, value= n), type = "treemap")%>%
  hc_plotOptions(treemap = list(colorByPoint = T,
                                dataLabels = list(enabled = T,
                                                  format = '{point.partido2}'))) %>%
  hc_title(text = "Congresistas por partido") %>%
  hc_add_theme( thm)



# sandbox -----------------------------------------------------------------

 inv %>%
   left_join(tipo_inv %>%  select(tipo_investigacion_id = id, tipo = nombre),
             by = "tipo_investigacion_id") %>%
   # left_join(congresistas %>%
   #             select(congresista_id = id,persona_id, corporacion_id, cuatrienio_id),
   #           by = "persona_id") %>%
   # left_join(corporacion %>%  select(id, corporacion =nombre),
   #           by = c("corporacion_id" = "id")) %>%
   # left_join(legislatura %>%  select(legislatura = id, cuatrienio_id),
   #           by = "cuatrienio_id") %>%
   # left_join(personas %>%  select(persona_id = id, nombres, apellidos),
   #           by = "persona_id") %>%
   # mutate(nombre_congresista = paste(nombres, apellidos)) %>%
   mutate( inves = if_else(str_detect(tipo,
                                      pattern = "Atención: Pérdida de investidura"),
                           "Pérdida de investidura", "Otro"),
           inves = if_else(str_detect(tipo,
                                      pattern = "Renunció a la curul"),
                           "Renunció a la curul", inves),
           inves = if_else(str_detect(tipo,
                                      pattern = "Silla Vacía"),
                           "Silla Vacía", inves)) %>%
   # filter(cuatrienio_id == 8) %>%
   # group_by(inves) %>%
   # summarise(n = n(), miembros = paste(nombre_congresista, collapse=", ")) %>%
   count(inves) %>%
   # mutate(corporacion = case_when(is.na(corporacion)~"No congresista", T~corporacion)) %>%
   filter(!inves == "Otro") %>%
   # mutate(miembros = case_when(corporacion== "No congresista"~"", T~miembros),
   #        corporacion = factor(corporacion, c("Cámara de Representantes", "Senado", "No congresista"))) %>%
   arrange(desc(n)) %>%
   hchart(hcaes(x = inves, y = n), type = "bar") %>%
   hc_tooltip(pointFormat = '<b>{series.name}: {point.n}</b><br> {point.miembros}')%>%
   hc_title(text = "Investigaciones") %>%
   hc_xAxis(title = list(text = "Tipo")) %>%
   hc_yAxis(title = list(text = "Total")) %>%
   hc_add_theme( thm)
