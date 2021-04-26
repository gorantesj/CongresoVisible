library(tidyverse)
library(highcharter)
library(readr)
library(lubridate)


thm <- hc_theme(
  colors = c("#66CCFF", "#17789E", "#6AEEB0", "#379E80", "#F78031", "#374B9E"),
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
                     treemap = list(borderRadius =5,
                                    dataLabels = list(style = list(fontFamily = "Bell MT", fontSize = "16px"))),
                     errorbar = list(maxPointWidth = 20))

)
