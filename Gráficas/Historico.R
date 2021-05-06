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
cuatri <- read_csv(here("Para Jesús", "cuatrienios.csv")) %>%
          select(cuatrienio_id = id, fecha_inicio_cuatri=fecha_inicio) %>%
          arrange(fecha_inicio_cuatri) %>% mutate(cuatrienio=row_number(),
          cuatrienio=factor(cuatrienio, labels = c("Cuatrienio 1994", "Cuatrienio 1998",
          "Cuatrienio 2002", "Cuatrienio 2006", "Cuatrienio 2010", "Cuatrienio 2004",
          "Cuatrienio 2008")))

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

control_pol_0 <- read_csv("Para Jesús/control_politico_citados.csv")
control_pol_1 <- read_csv("Para Jesús/control_politico_citantes.csv")
control_pol_2 <- read_csv("Para Jesús/control_politicos.csv")

inicia <- read_csv("Para Jesús/iniciativas.csv")
estado_proy <- read_csv("Para Jesús/estado_proyecto_leys.csv")
proy_estado <- read_csv("Para Jesús/proyecto_ley_estados.csv")

# Tema
source("Gráficas/tema.R")

# Composición partidista - proporción de mujeres --------------------

# Histórico
aux <- congresistas %>% select(congresista_id = id, persona_id, cuatrienio_id, partido_id) %>%
  left_join(legislatura %>%
    select(legislatura_id = id, cuatrienio_id, legislatura = nombre, fechaInicio, fechaFin),
    by = "cuatrienio_id") %>%
  left_join(partidos %>%
    select(partido_id =id, partido  = nombre, color),
    by = "partido_id" ) %>%
  left_join(personas %>%
    select(persona_id = id, genero_id),
    by = "persona_id") %>%
  left_join(sexo %>%
    select(genero_id = id, genero = nombre),
    by = "genero_id") %>%
  group_by(fechaInicio, genero) %>%
  summarise(n = n())%>%
  mutate(pct = round(100*n / sum(n), 1))

  aux %>%
    hchart(hcaes(x = 'fechaInicio', group = 'genero', y = 'pct'), type = "area") %>%
    hc_plotOptions(area = list(stacking = T)) %>%
    hc_title(text = "Proporción de mujeres") %>%
    hc_subtitle(text = "Historico") %>%
    hc_tooltip(pointFormat = '{point.pct} %') %>%
    hc_xAxis(title = list(text = "Fecha cuatrienios")) %>%
    hc_yAxis(title = list(text = "Porcentaje"), min=0) %>%
    hc_add_theme(thm)

  # Por cuatrienio
  id <- 1
  aux <- congresistas %>% select(congresista_id = id, persona_id, cuatrienio_id, partido_id) %>%
    left_join(partidos %>%
      select(partido_id =id, partido  = nombre, color),
      by = "partido_id" ) %>%
    left_join(personas %>%
      select(persona_id = id, genero_id),
      by = "persona_id") %>%
    left_join(sexo %>%
      select(genero_id = id, genero = nombre),
      by = "genero_id") %>%
    left_join(cuatri, by = "cuatrienio_id") %>%
    filter(!is.na(genero), cuatrienio_id == id)


  base1 <- aux %>%
    select(partido) %>%
    unique()

  base1 <- base1 %>%
    mutate(id = str_to_id(partido)) %>%
    bind_cols(tibble(color = hcl.colors(nrow(base1), palette = "viridis"))) %>%
    rename("name" = "partido")

  base2 <- aux %>%
    count(genero, partido) %>%
    mutate(color = if_else(genero == "Masculino", "#308446",
           if_else(genero == "Femenino","#f5973d","#808080")),
           parent=str_to_id(partido),
           id = as.character(row_number())) %>%
    rename("name"= "genero", "value"="n")

  dde <- list(base1, base2) %>%
    purrr::map(mutate_if, is.factor, as.character) %>%
    bind_rows() %>%
    list_parse() %>%
    purrr::map(function(x) x[!is.na(x)])


  highchart() %>%
    hc_chart(type = "treemap") %>%
    hc_title(text = "Calificación de menciones") %>%
    hc_add_series(
      data = dde, allowDrillToNode = TRUE,
      levelIsConstant = TRUE, textOverflow = "clip",
      dataLabels = list(color = "white"),
      levels = list(
        list(level = 1, borderWidth = 1,
              dataLabels = list(enabled = TRUE, verticalAlign = "top",
                align = "left", style = list(fontSize = "12px", textOutline = FALSE)) ),
        list(level = 2, borderWidth = 0, dataLabels = list(enabled = FALSE))
    )) %>%
    hc_colors("trasnparent") %>%
    hc_title(text = "Proporción de mujeres") %>%
    hc_add_theme(thm)

# Total de citaciones     --------------------------------

  aux <- congresistas %>% select(congresista_id = id, persona_id, cuatrienio_id, partido_id) %>%
    left_join(legislatura %>%
      select(legislatura_id = id, cuatrienio_id, legislatura = nombre, fechaInicio, fechaFin),
      by = "cuatrienio_id") %>%
    left_join(partidos %>%
      select(partido_id =id, partido  = nombre, color),
      by = "partido_id" ) %>%
    left_join(control_pol_1 %>%
      select(congresista_id, control_politico_id),
      by = "congresista_id") %>%
    left_join(control_pol_2 %>%
      select(control_politico_id = id, tipo_control_politico_id) %>%
      filter(tipo_control_politico_id == 1),
      by = "control_politico_id") %>%
    filter(cuatrienio_id == id) %>%
    group_by(partido) %>%
    summarise(n = n())

  aux %>%
    hchart('bar', hcaes(x = 'partido', y = n, group = 'partido')) %>%
    hc_tooltip(pointFormat = 'Número de citaciones: {point.n}')%>%
    hc_title(text = "Total de Citaciones") %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Número de citaciones")) %>%
    hc_add_theme(thm)

# Edad -------------------------------------

  aux <- congresistas %>% select(congresista_id = id, persona_id, cuatrienio_id, partido_id) %>%
    left_join(legislatura %>%
      select(legislatura_id = id, cuatrienio_id, legislatura = nombre, fechaInicio, fechaFin),
      by = "cuatrienio_id") %>%
    left_join(partidos %>%
      select(partido_id =id, partido  = nombre, color),
      by = "partido_id" ) %>%
    left_join(personas %>%
      select(persona_id = id, fechaNacimiento),
      by = "persona_id") %>%
    filter(cuatrienio_id == id, !is.na(fechaNacimiento))%>%
    mutate(edad = as.numeric(today() - (lubridate::dmy(fechaNacimiento)-years(100)))/365 ) %>%
    filter(edad < 100, cuatrienio_id == id) %>%
    group_by(partido) %>%
    summarise(media = mean(edad) %>% round(1),
              minimo = min(edad) %>% round(1),
              maxi = max(edad) %>% round(1),
              media2=round(mean(edad), 1))

    aux %>%
    hchart(hcaes(x = partido, high = maxi, low = minimo , group = partido), type = "errorbar") %>%
    hc_add_series(aux, hcaes(x = partido,  y= media, group= partido), type = "scatter") %>%
    hc_plotOptions(errorbar = list(showInLegend = F,tooltip = list(enabled = F))) %>%
    hc_title(text = "Edades por partido") %>%
    hc_xAxis(title = list(text = "")) %>%
    hc_yAxis(title = list(text = "Edad"), min=0) %>%
    hc_tooltip(pointFormat = 'Media: {point.media2}<br>Mínimo: {point.minimo}<br>Máximo: {point.maxi}') %>%
    hc_chart(inverted= T) %>%
    hc_add_theme(thm)

# Partidos con mayor representación -----------------------------------------------------

    aux <- congresistas %>% select(congresista_id = id, persona_id, cuatrienio_id, partido_id) %>%
      left_join(legislatura %>%
        select(legislatura_id = id, cuatrienio_id, legislatura = nombre, fechaInicio),
        by = "cuatrienio_id") %>%
      left_join(partidos %>%
        select(partido_id =id, partido = nombre),
        by = "partido_id" ) %>%
      left_join(personas %>%
        select(persona_id = id),
        by = "persona_id")

    aux %>%
      group_by(fechaInicio, partido) %>%
      summarise(n = n())%>%
      mutate(pct = round(100*n / sum(n), 1)) %>%
      hchart(hcaes(x = 'fechaInicio', group = 'partido', y = 'pct'), type = "area") %>%
      hc_plotOptions(area = list(stacking = T)) %>%
      hc_title(text = "Proporción de partidos con mayor representación") %>%
      hc_subtitle(text = "Historico") %>%
      hc_tooltip(pointFormat = 'Porcentaje de representación: {point.pct} %') %>%
      hc_xAxis(title = list(text = "Fecha cuatrienios")) %>%
      hc_yAxis(title = list(text = "Porcentaje"), min=0) %>%
      hc_add_theme(thm)

    aux %>% filter(cuatrienio_id == id) %>%
      group_by(fechaInicio, partido) %>%
      summarise(n = n())%>%
      mutate(pct = round(100*n / sum(n), 1)) %>%
      hchart('bar', hcaes(x = 'partido', y = pct, group = 'partido')) %>%
      hc_tooltip(pointFormat = 'Porcentaje de representación: {point.pct} %')%>%
      hc_title(text = "Proporción de partidos con mayor representación") %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_add_theme(thm)

    # Hay que pensar en otra sankey, división por cuatrienio, cada punto un cuatrienio

# Número de iniciativas gubernamentales por cuatrienio  ---------------------------------------

 aux <- pl %>%
    left_join(
      inicia %>% select(iniciativa_id = id, origen_iniciativa = nombre),
      by = "iniciativa_id") %>%
    left_join(legislatura %>%
      select(legislatura_id = id, cuatrienio_id, legislatura = nombre, fechaInicio),
      by = c("cuatrienio_id", "legislatura_id")) %>%
    filter(origen_iniciativa == "Gubernamental") %>%
    group_by(legislatura) %>%
    summarise(n = n()) %>%
    na.omit()

    aux %>% # Ordenar por factores la y
      hchart('bar', hcaes(x = 'legislatura', y = n, group = 'legislatura')) %>%
      hc_tooltip(pointFormat = 'Núm. iniciativas gubernamentales : {point.n}')%>%
      hc_title(text = "Número de iniciativas gubernamentales") %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_add_theme(thm)


# Iniciativas gubernamentales sancionadas como ley por cuatrienio  -----------------------------

aux <- pl %>%
        select(proyecto_ley_id = id, iniciativa_id, legislatura_id, cuatrienio_id) %>%
      left_join(
        inicia %>% select(iniciativa_id = id, origen_iniciativa = nombre),
        by = "iniciativa_id") %>%
      left_join(legislatura %>%
                  select(legislatura_id = id, cuatrienio_id, legislatura = nombre, fechaInicio),
                by = c("cuatrienio_id", "legislatura_id")) %>%
      filter(origen_iniciativa == "Gubernamental") %>%
      left_join(proy_estado,
        by = "proyecto_ley_id") %>%
      left_join(estado_proy %>%
        select(estado_proyecto_ley_id = id, estado_proyecto = nombre),
        by = "estado_proyecto_ley_id") %>%
      filter(estado_proyecto == "Sancionado como Ley") %>%
      group_by(fechaInicio) %>%
        summarise(n = n()) %>%
        na.omit()

    aux %>% # Una serie de tiempo, como la línea y el punto, evolucion
      hchart('bar', hcaes(x = 'fechaInicio', y = n, group = 'fechaInicio')) %>%
      hc_tooltip(pointFormat = 'Núm. iniciativas gubernamentales : {point.n}')%>%
      hc_title(text = "Número de iniciativas gubernamentales sancionadas como ley") %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_add_theme(thm)

    # Highcharts, area de gradado
    # Gubernamental vs no gubernamental, treemap, lineas de votos a favor, separadas por tipo de iniativa,
    # Sankey, dos divisiones a favor o encontra, corte a favor en contra
    # Heatmap particosvs cuatrienio, porcentaje favorable(z)

# Debates de control político citados por cuatrienio -------------------------------------------

    aux <- control_pol_2 %>%
            select(control_politico_id = id, cuatrienio_id, tipo_control_politico_id, titulo) %>%
           left_join(legislatura %>%
            select(legislatura_id = id, cuatrienio_id, legislatura = nombre, fechaInicio) ,
            by = "cuatrienio_id") %>%
          filter(tipo_control_politico_id == 1, !grepl("Debat", titulo)) %>%
          group_by(fechaInicio) %>%
          summarise(n = n())

    aux %>% # Una serie de tiempo, como la línea y el punto, evolucion
      hchart('bar', hcaes(x = 'fechaInicio', y = n, group = 'fechaInicio')) %>%
      hc_tooltip(pointFormat = 'Núm. iniciativas gubernamentales : {point.n}')%>%
      hc_title(text = "Número de iniciativas gubernamentales sancionadas como ley") %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "")) %>%
      hc_add_theme(thm)

# Principales partidos de oposición ------------------------------------------------------------

# Principales partidos de apoyo al gobierno ----------------------------------------------------
