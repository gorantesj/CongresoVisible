library(tidyverse)
library(highcharter)
library(readr)
library(lubridate)


thm <- hc_theme(
  colors =c("#04192E","#062647", "#0B3B5D","#0D4568", "#0F4F73", "#136489", "#17789E",
            "#3B90B5", "#5EA8CC", "#5EA8CC", "#82C0E3","#56A2C7", "#94CCEF", "#A5D8FA",
            "#C1E3FA", "#DCEEFA", "#E3F2FE"),
  chart = list( style = list(fontFamily = "Bell MT", fontSize = "20px")),
  title = list( style = list(  color = "#17789E", fontSize = "22px", fontWeight= 'bold')),
  subtitle = list(  style = list(    color = "#666666"  ) ),
  legend = list( itemStyle = list(  color = "black"   ),
                 itemHoverStyle = list( color = "gray")  ),
  tooltip = list(borderWidth =0, shadow = F,
                 headerFormat= '<span style="font-size: 20px"><b>{point.key}</span><br/></b>',
                 shape = "square",
                 style = list( fontSize = "16px")),
  yAxis = list(lineWidth = 3,title = list(style= list(fontSize = "16px")),
               tickAmount = 5,
               labels = list(style= list(fontSize = "15px"))),
  xAxis = list(lineWidth = 0,title = list(style= list(fontSize = "16px")),
               labels = list(style= list(fontSize = "18px"))),
  plotOptions = list(bar = list(borderRadius =5),
                     scatter = list(marker = list(radius = 8)),
                     treemap = list(borderRadius =5, colorByPoint = T,
                                    dataLabels = list(style = list(fontFamily = "Bell MT", fontSize = "16px"))),
                     errorbar = list(maxPointWidth = 20))

)
