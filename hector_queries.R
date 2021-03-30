require(pacman)
p_load(tidyverse,highcharter, RPostgreSQL, DBI, RMySQL,  RMariaDB, treemapify)


con <- dbConnect(
  drv = RMariaDB::MariaDB(),
  dbname = "prueba33_congresovisible2",
  host = "162.241.62.202",
  username = "prueba33_cv_user",
  password = "CongresoVisible2020",
  port = 3306
)


tbl(con, "proyecto_leys")

#Congreso hoy -> proyectos de ley -----------
colores <- c("#1d67ac",  "#ff4f4f", "#93c73d", "#6d4fa0",
             "#6b4c2a", "#f9a819", "#019188", "#d34787",
             "#164a80", "#F05647", "#0e4c42", "#3b005a",
             "#AD8156", "#fdc10e", "#00b140", "#e995bf" )
# Queries estatus del proyecto por fecha
# Ya no es necesario - aún
# tbl(con, "proyecto_leys") %>%
#   count(fecha_radicacion, estado_proyecto_ley_id,camara_id, cuatrienio_id) %>%
#   left_join(
#     tbl(con, "corporacions") %>%
#       #filter(activo == 1) %>%
#       select(id, camara = nombre),
#     by = c("camara_id" = "id")
#   )  %>%
#   filter( # filtro, puede cambiar
#     camara == "Cámara de Representantes",
#     cuatrienio_id == 1
#   ) %>%
#   arrange(estado_proyecto_ley_id, fecha_radicacion) %>%
#   count(fecha_radicacion, estado_proyecto_ley_id) %>%
#   show_query() %>%
#   collect() %>%
#   hchart(hcaes(x = fecha_radicacion, y = n, group = estado_proyecto_ley_id), type = "line") %>%
#   hc_plotOptions(line= list(lineWidth = 4,
#                             marker = list(radius =0),
#                             stickyTracking=F)) %>%
#   hc_xAxis(crosshair = T, title = list(text = "Fecha",  style = list(color = "#FFF")), type = "datetime",
#            lineWidth = 0, tickWidth  = 0, gridLineWidth =0,
#            showLastLabel= F,
#            labels = list(step = 3, style = list(fontSize = "16px", color = "#FFF") )) %>%
#   hc_yAxis(crosshair = F, title = list(text = "Total de proyectos", style = list(color = "#FFF")), tickAmount = 3, max = 6, min =0,
#            dashStyle = "dot",
#            gridLineWidth =.5, showFirstLabel = F, gridLineColor = "",
#            labels = list( style = list(fontSize = "12px") )) %>%
#   #title
#   hc_title(text = "<b>Proyectos de ley",  style = list(color = "#FFF")) %>%
#   #tooltip
#   hc_tooltip(
#     borderWidth= 0,
#     outside = T,
#     textOutline= "3px contrast",
#     shadow=F,
#     shared = T,
#     split = F,
#     headerFormat= '<span style="font-size: 10px">{point.key}</span><br/>'
#     # pointFormat = '<span style="color:{point.color}">●</span> <b> {point.candidato}<b><br> p. clave: {point.palabra}<br> {point.n} tuits <br> {point.rt} retuits<br> {point.favs} favoritos<br>'
#   ) %>%
#   hc_colors(colors = c("#2C6170", "#FF6B6B", "#FFE66D")) %>%
#   hc_chart(style = list(fontFamily = "Avenir next"
#   ))

# Queries de proyectos de ley presentados por ministros

tbl(con,"proyecto_ley_autors") %>%
  left_join(
    tbl(con, "congresistas") %>%
      select(id, corporacion_id, partido_id) %>%
      left_join(
        tbl(con, "partidos") %>%
          filter(activo == 1) %>%
          select(id),
        by = c("partido_id" = "id")
      ) %>%
      left_join(
        tbl(con, "corporacions") %>%
          filter(activo == 1) %>%
          select(id, camara = nombre),
        by = c("corporacion_id" = "id")
      ) %>%
      select(id,  camara) ,
    by = c("congresista_id"="id")
  ) %>%
  left_join(tbl(con, "proyecto_leys") %>%
              select(id, cuatrienio_id),
            by = c("proyecto_ley_id" = "id")
  ) %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    cuatrienio_id == 1
  )%>%
  count(congresista_id) %>%
  show_query() %>%
  collect() %>%
  ggplot(aes(x = fct_reorder(congresista_id, n),y =n, label = n, fill = congresista_id)) +
  geom_chicklet(width = 0.55 )+
  scale_fill_manual(values = colores)+
  labs(x = "", y = "", title = "Proyectos presentados \npor ministros")+
  geom_text(nudge_y = -1, size =6, color = "white")+
  theme_minimal() +coord_flip()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(),
        text=element_text(size=18, family="Avenir Next"))

# Queries de tipo de proyecto
pal <- colorRampPalette(c("#2B6170", "#93ACAF"))

tbl(con,"proyecto_leys") %>%
  count(tipo_proyecto_id, camara_id, cuatrienio_id) %>%
  left_join(
    tbl(con, "corporacions") %>%
      #filter(activo == 1) %>%
      select(id, camara = nombre),
    by = c("camara_id" = "id")
  )  %>%
  left_join(
    tbl(con, "tipo_proyectos") %>%
      #filter(activo == 1) %>%
      select(id, tipo_proyecto = nombre),
    by = c("tipo_proyecto_id" = "id")
  )  %>%
  filter( # filtro, puede cambiar
    camara == "Cámara de Representantes",
    cuatrienio_id == 1
  ) %>%
  count(tipo_proyecto) %>%
  show_query() %>%
  collect() %>%
  hchart(hcaes(x = tipo_proyecto,  value = n), type = "treemap") %>%
  hc_plotOptions(treemap = list(borderRadius = 7, colorByPoint = T,
    dataLabels = list(style = list(fontFamily = "Avenir next",
                                   fontSize = "16px")))) %>%
  hc_colors(colors = pal(11)) %>%
  hc_chart(style = list(fontFamily = "Avenir next"
  ))



# Queries de proyectos radicados según sexo
# Gráfica nueva
# Pendiente por discutir con Colombia
#La pongo en ggplot

tbl(con,"proyecto_ley_autors") %>%
  count(proyecto_ley_id, congresista_id) %>%
  left_join(
    tbl(con, "proyecto_leys") %>%
      #filter(activo == 1) %>%
      select(id, cuatrienio_id, tema_proyecto_ley_id),
    by = c("proyecto_ley_id" = "id")
  )  %>%
  left_join(
    tbl(con, "congresistas") %>%
      select(id, partido_politico_id, corporacion_id),
    by = c("congresista_id" = "id")
  ) %>%
    left_join(
      tbl(con, "corporacions") %>%
        select(id, camara = nombre),
      by = c("corporacion_id" = "id")
    ) %>%
  left_join(
    tbl(con, "partidos") %>%
      select(id, partido = nombre),
    by = c("partido_politico_id" = "id")
  ) %>%
  left_join(
    tbl(con, "tema_proyecto_leys") %>%
      select(id, tema = nombre),
    by = c("tema_proyecto_ley_id" = "id")
  ) %>%
  filter(corporacion_id == 1) %>%
  count(partido, tema) %>%
  show_query()


ggplot(aes(area = tema_proyecto_ley_id, fill = n, label = tema_proyecto_ley_id,
                subgroup = nombre)) +
  geom_treemap() +
  geom_treemap_subgroup_border()

## Query historico -> composición -> proporción de mujeres---------------
tbl(con, "congresistas") %>%
  select(cuatrienio_id , genero_id, es_representante_camara, es_senador) %>%
  left_join(tbl(con, "cuatrienios") %>%
            select(id, fecha_inicio),
            by=c("cuatrienio_id"="id")) %>%
  left_join(tbl(con, "generos") %>%
            select(id, nombre),
            by=c("genero_id"="id")) %>%
  filter(es_representante_camara==1) %>%
  group_by(cuatrienio_id,fecha_inicio, nombre) %>%
  summarise(total=n()) %>%
  mutate(totales=sum(total, na.rm=T),
         porc=round((total/totales)*100, 2)) %>%
  show_query() %>%
  collect() %>%
  hchart(hcaes(x = fecha_inicio, group= nombre, y =porc), type = "area") %>%
  hc_plotOptions(area = list(stacking = T))

#Query mediana de edad

  tbl(con, "congresistas") %>%
  select(cuatrienio_id , fechaNacimiento, es_representante_camara, es_senador) %>%
  left_join(tbl(con, "cuatrienios") %>%
            select(id, fecha_inicio),
            by=c("cuatrienio_id"="id")) %>%
  mutate(nac= round(DATEDIFF(CURDATE(), fechaNacimiento)/365),
         cuatri=round(DATEDIFF(CURDATE(), fecha_inicio)/365),
         edad=cuatri-nac) %>%
  filter(es_representante_camara==1) %>%
  group_by(fecha_inicio) %>%
  summarise(median_edad=median(edad, na.rm=T))%>%
    show_query() %>%
    collect() %>%
    ggplot +
    geom_bar( aes(x=fct_reorder(fecha_inicio,median_edad) , y=median_edad),
              stat="identity",
              fill="#720026", alpha=0.8, width=0.5) +
    geom_errorbar( aes(x=fecha_inicio, ymin=q1, ymax=q3), width=0.2,
                   colour="black", alpha=0.9, size=.5) +
    #titulos
    labs( y ="Mediana", x = "", title = "Edad mediana por año")+
    # temas
    theme_minimal()+
    theme(legend.position="none",
          panel.grid.minor = element_blank())+
    coord_flip()

  ## Query partidos con más representación---------------

  tbl(con, "congresistas") %>%
    select(cuatrienio_id, partido_id, es_representante_camara, es_senador) %>%
    left_join(tbl(con, "partidos") %>%
              select(id, nombre),
              by=c("partido_id"="id")) %>%
    left_join(tbl(con, "cuatrienios") %>%
                select(id, fecha_inicio),
              by=c("cuatrienio_id"="id")) %>%
    filter(es_representante_camara==1) %>%
    group_by(cuatrienio_id, fecha_inicio, nombre) %>%
    summarise(total=n())%>%
    show_query()

  hchart(hcaes(x = fecha_inicio, group = nombre, y =total), type = "column") %>%
    hc_plotOptions(column = list(stacking = T))

  ## Query total de citaciones---------------


  tbl(con, "citacions") %>%
    select(fecha_proposicion) %>%
    count(fecha_proposicion) %>%
    show_query()









