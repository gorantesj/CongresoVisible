library(magrittr)
library(tidyverse)
library(highcharter)

# Congreso hoy ------------------------------------------------------------
# Congresistas ------------------------------------------------------------
#Con más autorías
tibble(congresista = c("Yenica Acosta", "Laureano Augusto", "Iván Agudelo",
                       "Richard Aguilar", "Modesto Aguilera", "Luis Albán",
                       "José Amar", "Miguel Amín", "Fabio Leduc", "Esperanza Martínez",
                       "Juan Pérez", "Verónica Castro", "Luis Miguel", "Jorge Zepeda", "Germán Hoyos",
                       "Mariana Hijar", "Anatolio Hernández", "Irma Herrera", "Norma Hurtado", "Germán Rojas"),
       n = c(sample(13:5, size = 20, replace = T)),
       grupo = rep(c("Senado", "Representantes"), 10)) %>%
  # group_by(grupo) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(x = congresista, y = n, group = grupo), type = "bar") %>%
  hc_title(text = "Congresistas con más proyectos de ley como autor")


#Con más citaciones
tibble(congresista = c("Yenica Acosta", "Laureano Augusto", "Iván Agudelo",
                       "Richard Aguilar", "Modesto Aguilera", "Luis Albán",
                       "José Amar", "Miguel Amín", "Fabio Leduc", "Esperanza Martínez",
                       "Juan Pérez", "Verónica Castro", "Luis Miguel", "Jorge Zepeda", "Germán Hoyos",
                       "Mariana Hijar", "Anatolio Hernández", "Irma Herrera", "Norma Hurtado", "Germán Rojas"),
       n = c(sample(15:2, size = 20, replace = T)),
       grupo = rep(c("Senado", "Representantes"), 10)) %>%
  # group_by(grupo) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(x = congresista, y = n, group = grupo), type = "bar") %>%
  hc_title(text = "Congresistas con más citaciones")



# Partidos ----------------------------------------------------------------

#Citaciones
tibble(partido = c("CD", "CR", "AV","PDA", "PLC",
                   "CD", "CR", "PSUN", "PCC", "Decentes"),
       grupo = c("Gobierno", "Gobierno", "Oposición", "Oposición", "Independente",
                 "Gobierno",  "Gobierno",  "Gobierno",  "Gobierno", "Oposición"),
       n = c(sample(15:2, size = 10, replace = T)),
       camara = c(rep(c("Senado"), 5), rep(c("Representantes"), 5)) )%>%
  # group_by(grupo) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(x = partido, y = n, group = camara), type = "bar") %>%
  hc_title(text = "Partidos con más citaciones")

#Autorías
tibble(partido = c("CD", "CR", "AV","PDA", "PLC",
                   "CD", "CR", "PSUN", "PCC", "Decentes"),
       grupo = c("Gobierno", "Gobierno", "Oposición", "Oposición", "Independente",
                 "Gobierno",  "Gobierno",  "Gobierno",  "Gobierno", "Oposición"),
       n = c(sample(21:0, size = 10, replace = T)),
       camara = c(rep(c("Senado"), 5), rep(c("Representantes"), 5)) )%>%
  # group_by(grupo) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(x = partido, y = n, group = camara), type = "bar") %>%
  hc_title(text = "Partidos con más autorías")


#Temas
highchart() %>%
  hc_chart(type = 'sankey') %>%
  hc_add_series(
    data = list(
      list(from = 'CD', to = 'Finanzas', weight = 10),
      list(from = 'CD', to = 'Salud', weight = 3),
      list(from = 'CR', to = 'Ecología', weight = 5),
      list(from = 'CR', to = 'Salud', weight = 3),
      list(from = 'CR', to = 'Seguridad', weight = 2),
      list(from = 'AV', to = 'Seguridad', weight = 5),
      list(from = 'AV', to = 'Salud', weight = 1))
  ) %>%
  hc_plotOptions(sankey = list(borderRadius = 7, colorByPoint = T,
                               dataLabels = list(style = list(fontFamily = "Avenir next", fontSize = "16px")))) %>%
  hc_title(text = "Temas frecuentes por partido") %>%
  hc_chart(style = list(fontFamily = "Avenir next"
  )) %>%  hc_add_theme(hc_theme_538())


# Composición -------------------------------------------------------------
# Congresistas ------------------------------------------------------------
# Sexo y edad - Pirámide
tibble(edad = sample(30:78, size = 172, replace = T),
       sexo = sample(c("Hombre", "Mujer"), size = 172, replace = T, prob = c(.7, .3))) %>%
  mutate(rango = cut(edad,c(17,29,39,49,59,69,79, 89,100),
                     labels = c("18 - 29","30 - 39","40 - 49", "50 - 59",
                                "60 - 69", "70 - 79","80 - 89", "90 o más"))) %>%
  count(rango,sexo) %>%
  mutate(n = as.double(n),
         n2= case_when(sexo == "Hombre"~ n*-1,
                       sexo == "Mujer"~ n)) %>%
  hchart(hcaes(y = n2, group = sexo, x = rango), type = "bar") %>%
  hc_plotOptions(bar = list(stacking = T, borderRadius = 5,
                            dataLabels= list(enabled =F,
                                             align= "center",
                                             inside= F,
                                             # rotation = 5,
                                             verticalAlign= 'top',
                                             # x = -5,
                                             crop= F,
                                             overflow= "none",
                                             style=list(color="BLACK", fontSize = "25px", textOutline= "3px contrast"),
                                             format = paste0("{point.n:,.0f} ")

                            )
  ))%>%
  hc_yAxis(labels= list(formatter = JS("function(){ return Math.abs(this.value); }")))

#Asientos por partido
tibble(partido =c("AICO", "AICO",
                  "Alianza Verde", "Alianza Verde",
                  "Bancada Afrocolombiana", "Bancada Afrocolombiana",
                  "BLOQUE PACÍFICO","BLOQUE PACÍFICO",
                  "Cambio Radical", "Cambio Radical",
                  "Centro Democrático", "Centro Democrático",
                  "Coalición Alternativa Santandereana", "Coalición Alternativa Santandereana",
                  "Coalición Lista de la Decencia", "Coalición Lista de la Decencia",
                  "Colombia Humana", "Colombia Humana",
                  "Colombia Justa Libres", "Colombia Justa Libres",
                  "CCACNPR", "CCACNPR",
                  # "Consejo Comunitario Ancestral de Comunidades Negras Playa Renaciente", "Consejo Comunitario Ancestral de Comunidades Negras Playa Renaciente",
                  "Consejo Comunitario La Mamuncia", "Consejo Comunitario La Mamuncia",
                  "Conservador Colombiano", "Conservador Colombiano",
                  "FARC", "FARC",
                  "Liberal Colombiano", "Liberal Colombiano",
                  "MAIS", "MAIS",
                  "MIRA", "MIRA",
                  "Opción Ciudadana", "Opción Ciudadana",
                  "Partido de la U", "Partido de la U",
                  "PDA", "PDA"
),
n = c(1,0, 9, 9, 3,18,23, 28,17, 30, 19, 32,0,1, 3, 2,
      1,1, 3,1, 0,1, 0,1, 13,21, 5,4, 15,35, 1,2, 3,1,
      1,4, 14,25, 6,2 ),
camara = c(rep(c("Senadores", "Representantes"),20 ))) %>%
  filter(camara == "Senadores") %>%
  # group_by(grupo) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(label = partido, y = n, name = partido), type = "item") %>%
  hc_title(text = "Asientos por partido") %>%
  hc_subtitle(text = "Senadores")

tibble(partido =c("AICO", "AICO",
                  "Alianza Verde", "Alianza Verde",
                  "Bancada Afrocolombiana", "Bancada Afrocolombiana",
                  "BLOQUE PACÍFICO","BLOQUE PACÍFICO",
                  "Cambio Radical", "Cambio Radical",
                  "Centro Democrático", "Centro Democrático",
                  "Coalición Alternativa Santandereana", "Coalición Alternativa Santandereana",
                  "Coalición Lista de la Decencia", "Coalición Lista de la Decencia",
                  "Colombia Humana", "Colombia Humana",
                  "Colombia Justa Libres", "Colombia Justa Libres",
                  "CCACNPR", "CCACNPR",
                  # "Consejo Comunitario Ancestral de Comunidades Negras Playa Renaciente", "Consejo Comunitario Ancestral de Comunidades Negras Playa Renaciente",
                  "Consejo Comunitario La Mamuncia", "Consejo Comunitario La Mamuncia",
                  "Conservador Colombiano", "Conservador Colombiano",
                  "FARC", "FARC",
                  "Liberal Colombiano", "Liberal Colombiano",
                  "MAIS", "MAIS",
                  "MIRA", "MIRA",
                  "Opción Ciudadana", "Opción Ciudadana",
                  "Partido de la U", "Partido de la U",
                  "PDA", "PDA"
),
n = c(1,0, 9, 9, 3,18,23, 28,17, 30, 19, 32,0,1, 3, 2,
      1,1, 3,1, 0,1, 0,1, 13,21, 5,4, 15,35, 1,2, 3,1,
      1,4, 14,25, 6,2 ),
camara = c(rep(c("Senadores", "Representantes"),20 ))) %>%
  filter(camara == "Representantes") %>%
  # group_by(grupo) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(label = partido, y = n, name = partido), type = "item") %>%
  hc_title(text = "Asientos por partido") %>%
  hc_subtitle(text = "Representantes")



#Profesión
tibble(partido =c("Derecho", "Economía", "Comunicación", "Ingeniería", "Otros"),
n = c(104,22, 21,16 , 9)) %>%
  arrange(desc(n)) %>%
  hchart(hcaes(label = partido, y = n, name = partido), type = "item") %>%
  hc_title(text = "Profesión")


# Histórico ---------------------------------------------------------------

# Proyectos de ley --------------------------------------------------------

#Proyectos de ley y sancionados como ley

# cambiar a acumulado
proyectos <- tibble( proyecto  = letters[1:50],
                     grupo = sample(c("Total", "Ley"),prob = c(.7, .2), size = 50, replace = T),
                     fecha = sample(seq(today()-20, today(), length.out = 11), size = 50, replace = T )) %>%
  arrange(fecha)

proyectos %>% count(fecha, grupo) %>%
  group_by(grupo) %>%  mutate(n = cumsum(n)) %>%
  filter(grupo == "Total") %>%
  hchart(hcaes(x = fecha, y  = n),name = "Total", type = "area", color = "#93ACAF") %>%
  hc_add_series(proyectos %>% count(fecha, grupo) %>%
                  group_by(grupo) %>%  mutate(n = cumsum(n))%>% filter(grupo != "Total"),
                hcaes(x = fecha, y  = n), type = "line", color = "#2B6170", name = "Ley") %>%
  hc_yAxis(min = 0, title = list(text = "Proyectos" , style = list( fontSize = "16px", color = "#41657A")),
           gridLineWidth =0, tickAmount = 2,
           labels = list(style = list(fontSize = "18px", color = "#41657A"))) %>%
  hc_xAxis( title = list(text = "Fecha", style = list()), type = "datetime",
            labels = list(step = 2,style = list(fontSize = "18px",color = "#13384D")),
            crosshair = list(ebabled= T, color= "#93ACAF", dashStyle="shortdash",
                             width= 2, snap = F, zIndex= 5),
            lineWidth =0, tickWidth =0) %>%
  hc_plotOptions( line = list(marker = list(radius = 0)),
                  area = list(fillOpacity= .3,
                              fillColor = list(
                                linearGradient = list(x1 = 0, x2 = 0, y1 = 0, y2 = 1),
                                stops = list(
                                  c(0, '#93ACAF'),
                                  c(1, '#FFF')   ) ),
                              crisp=F, lineWidth = 0, marker = list(radius =0))) %>%
  hc_title(text = "Proyectos de ley y proyectos sancionados como ley", align = "left", style = list(fontSize = "22px", color = "#13384D")) %>%
  hc_tooltip(borderWidth =0,shadow = F,
             shared = T,
             headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>',
             useHTML = TRUE,
             pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.y}</b><br/>',
             style = list(fontSize = "16px", color = "#41657A")) %>%
  hc_chart(style = list(fontColor = "#1C313D",
                        # backgroundColor = "#FFFBF4",
                        fontFamily= "Avenir Next"),zoomType = "x")

tot<-  tibble(fecha=seq(from = today()-23*365, to =today(), length.out = 8396 ),
              n = sample(c(0,1, 0,2, 0), size = 8396, replace = T))

tot %>% mutate( year= floor_date(fecha, unit = "year")) %>%
  group_by(year) %>%  summarise(n = sum(n)) %>%
  mutate(year = datetime_to_timestamp(year),
         ley = round(n*.4)) %>%
  gather(grupo, n, n:ley) %>%
  hchart(hcaes(x = year, y = n, group = grupo), type = "line") %>%
  hc_plotOptions(line= list(lineWidth = 4,
                            marker = list(radius =0),
                            stickyTracking=F)) %>%
  hc_xAxis(crosshair = T, title = list(text = ""), type = "datetime",
           lineWidth = 0, tickWidth  = 0, gridLineWidth =0,
           showLastLabel= F,
           labels = list(step = 2, style = list(fontSize = "16px", color = "#001c44") )) %>%
  hc_yAxis(crosshair = F, title = list(text = ""), tickAmount = 3,
           dashStyle = "dot",
           gridLineWidth =.5, showFirstLabel = F,
           labels = list( style = list(fontSize = "12px") )) %>%
  #title
  hc_title(text = "Proyectos de ley por año") %>%
  #tooltip
  hc_tooltip(
    borderWidth= 0,
    outside = T,
    textOutline= "3px contrast",
    shadow=F,
    shared = T,
    split = F,
    headerFormat= '<span style="font-size: 20px">{point.key}</span><br/>',
    pointFormat = '{point.n}'  ) %>%
  hc_colors(colors = c("#1A535C", "#FFE66D")) %>%
  hc_chart(style = list(fontFamily = "Avenir next"), polar = T)

# Temas recurrentes
tibble(fecha=seq(from = today()-23*365, to =today(), length.out = 8396 ),
       tema= sample(c("Econimía", "Salud", "Educación",
                      "Medio Ambiente", "Turismo"), size = 8396, replace = T)) %>%
  mutate( year= floor_date(fecha, unit = "year")) %>%
  count(year, tema) %>%
  mutate(year = datetime_to_timestamp(year)) %>%
  hchart(hcaes(x = year, y = n, group = tema), type = "line") %>%
  hc_xAxis(type = "datetime") %>%
  hc_yAxis(title = list(text = "Proyectos de ley")) %>%
hc_tooltip(shared = T)


# Tiempo de aprobación en días promedio por año
aux <- tibble(year = c(1998:2021),
       dias =sample(20:42, size = 24, replace = T),
       min = dias-9,
       max = dias +11) %>%
  arrange(dias)
aux %>%
  hchart(hcaes(x = year, low = min, high = max), type = "columnrange") %>%
  hc_add_series(aux, hcaes(x = year, y = dias), type = "scatter") %>%
  hc_plotOptions(columnrange = list(borderRadius = 8,pointWidth = 10)) %>%
  hc_chart(inverted = T) %>%
  hc_title(text = "Días promedio en aprobar proyectos de ley por año")




# Congresistas ------------------------------------------------------------
# Proporción de mujeres
tibble(hombres = sample(50:100, size = 24, replace = T),
       year = c(1998:2021),
       mujeres = 100-hombres) %>%  gather(sexo, n, c(hombres,mujeres )) %>%
  hchart(hcaes(x = year, group= sexo, y =n), type = "area")

#Edad promedio por año
tibble( year = c(1998:2021),
        media = sample(40:87, size = 24, replace = T),
        q1 =round(quantile(media, probs = .25),2),
        q3 =round(quantile(media, probs = .75),2)) %>%
  mutate(year = as.character(year)) %>%
  ggplot +
  geom_bar( aes(x=fct_reorder(year,media) , y=media), stat="identity", fill="#720026", alpha=0.8, width=0.5) +
  geom_errorbar( aes(x=year, ymin=q1, ymax=q3), width=0.2, colour="black", alpha=0.9, size=.5) +
  #titulos
  labs( y ="Promedio", x = "")+
  # temas
  theme_minimal()+
  theme(legend.position="none",
        panel.grid.minor = element_blank())+
  coord_flip()

#Top partidos
tibble(year = c(1998:2021),
       partido1 =sample(x = c(.1,.3, .4, .2), size = 24, replace = T),
       partido2 =sample(x = c(.1,.3, .2), size = 24, replace = T),
       partido3 =sample(x = c(.1,.3, .2), size = 24, replace = T)) %>%
  gather(grupo, n, partido1:partido3) %>%
  hchart(hcaes(x = year, group = grupo, y =n), type = "column") %>%
  hc_plotOptions(column = list(stacking = T))

# Periodos de mujeres en el cargo

tibble(min = c(1998, 1998, 2006, 2002, 2006, 2006, 2010, 2010, 2014, 2014),
       max = c(2002, 2006, 2010, 2006, 2018, 2010, 2014, 2014, 2018, 2021),
       congresista = c( "Ana H.", "Fabiola V.", "Lucía M.", "Sara H.", "Nerea J.",
                       "Lusa D.", "Andrea E.", "Julia Z.", "Mariana R.", "Fenanda T.")) %>%
  arrange(min) %>%
  hchart(hcaes(x = congresista, low = min, high = max), type = "columnrange") %>%
  hc_plotOptions(columnrange = list(borderRadius = 8,pointWidth = 10)) %>%
  hc_chart(inverted = T)

# % Iniciativas por ministro y aprobadas

tibble(year =c(1998:2021),
       min = sample(x = c(5:57), size = 24, replace = T)) %>%
  hchart(hcaes(x = year,  y = min), type = "area")


# % Iniciativas aprobadas de ministros

tibble(year =c(1998:2021),
       min = sample(x = c(80:92), size = 24, replace = T)) %>%
  hchart(hcaes(x = year,  y = min), type = "area")


# Votación ----------------------------------------------------------------

# Días que se tardan en aprobar dependiendo el partido que propuso el proyecto
aux <- tibble(partido =sample(c("CD", "CR", "AV","PDA", "PLC",
                "CD", "CR", "PSUN", "PCC", "Decentes"), size = 100, replace = T),
       dias = sample(10:87, size = 100, replace = T)) %>%
  group_by(partido) %>% summarise(min= min(dias), max= max(dias),
                                  mean = mean(dias)) %>%
  arrange(mean)
aux %>%
hchart(hcaes(x = partido, low = min, high = max), type = "columnrange") %>%
  hc_add_series(aux, hcaes(x = partido, y = mean), type = "scatter", color = "#FFF") %>%
  hc_plotOptions(columnrange = list(borderRadius = 8,pointWidth = 10)) %>%
  hc_chart(inverted = T)

# votación por tema

tibble(votos = sample(30:78, size = 172, replace = T),
       voto = sample(c("En contra", "A favor"), size = 172, replace = T, prob = c(.5, .5))) %>%
  mutate(tema = cut(votos,c(17,29,39,49,59,69,79, 89,100),
                     labels = c("Economía","Salud","Educación", "Infraestructura",
                                "Seguridad", "Turismo","Medio ambiente", "Hacienda"))) %>%
  count(tema, voto) %>%
  mutate(n = as.double(n),
         n2= case_when(voto == "En contra"~ n*-1,
                       voto == "A favor"~ n)) %>%
  hchart(hcaes(y = n2, group = voto, x = tema), type = "bar") %>%
  hc_plotOptions(bar = list(stacking = T, borderRadius = 5,
                            dataLabels= list(enabled =F,
                                             align= "center",
                                             inside= F,
                                             # rotation = 5,
                                             verticalAlign= 'top',
                                             # x = -5,
                                             crop= F,
                                             overflow= "none",
                                             style=list(color="BLACK", fontSize = "25px", textOutline= "3px contrast"),
                                             format = paste0("{point.n:,.0f} ")

                            )
  ))%>%
  hc_yAxis(labels= list(formatter = JS("function(){ return Math.abs(this.value); }")))

# votación por partido

tibble(votos = sample(30:78, size = 172, replace = T),
       voto = sample(c("En contra", "A favor"), size = 172, replace = T, prob = c(.5, .5))) %>%
  mutate(tema = cut(votos,c(17,29,39,49,59,69,79, 89,100),
                    labels = c("CD", "CR", "AV","PDA", "PLC", "PSUN", "PCC", "Decentes"))) %>%
  count(tema, voto) %>%
  mutate(n = as.double(n),
         n2= case_when(voto == "En contra"~ n*-1,
                       voto == "A favor"~ n)) %>%
  hchart(hcaes(y = n2, group = voto, x = tema), type = "bar") %>%
  hc_plotOptions(bar = list(stacking = T, borderRadius = 5,
                            dataLabels= list(enabled =F,
                                             align= "center",
                                             inside= F,
                                             # rotation = 5,
                                             verticalAlign= 'top',
                                             # x = -5,
                                             crop= F,
                                             overflow= "none",
                                             style=list(color="BLACK", fontSize = "25px", textOutline= "3px contrast"),
                                             format = paste0("{point.n:,.0f} ")

                            )
  ))%>%
  hc_yAxis(labels= list(formatter = JS("function(){ return Math.abs(this.value); }")))
