setwd('C:/Users/Lenovo/Desktop/automation')

library(leaflet)
library(dplyr)
library(ggplot2)
library(fmsb)

source('utils/convertions.R')


# creating dataframe
df <- data.frame(
  x = c(1, 2, 3, 4),
  y = c(3, 7, 5, 1),
  z = c(2, 5, 4, 2),
  h = c(1, 2, 3, 1),
  lat = c(-21.15712, -21.19058, -21.21523, -21.17922),
  lon = c(-47.79177, -47.83451, -47.79657, -47.71812)
)

# creating charts
chart1 = df %>% ggplot(aes(x = x, y = y)) +
  geom_col(fill = '#6666ff')
chart1

chart2 = df %>% ggplot(aes(x = x, y = y)) +
  geom_line(color = '#ff6666', size = 2)
chart2

chart3 = df %>% ggplot(aes(x = x, y = y)) +
  geom_col(aes(x = x, y = y), fill = '#66ff66') +
  geom_point(color = '#ff6666', size = 8)
chart3

# creating map
icons = icons(
  iconUrl = ifelse(df$x == 1, 'markers/red_marker.png',
            ifelse(df$x == 2, 'markers/orange_marker.png',
            ifelse(df$x == 3, 'markers/yellow_marker.png', 'markers/green_marker.png'))),
  iconWidth = 38, iconHeight = 38,
  iconAnchorX = 19, iconAnchorY = 38,
)

map = leaflet(df) %>% addTiles() %>%
  addMarkers(~lon, ~lat, icon = icons)
map

# radar chart
#df_radar = df[c(1:4)] %>% slice(1)
df_radar = df[c(1:4)]
df_radar = rbind(rep(7, 4), rep(0, 4), df_radar)

colors_border=c( rgb(0.2,0.5,0.5,0.6), rgb(0.8,0.2,0.5,0.6) , rgb(0.7,0.5,0.1,0.6), rgb(0.3,0.7,0.1,0.6) )
colors_in=c( rgb(0.2,0.5,0.5,0.2), rgb(0.8,0.2,0.5,0.2) , rgb(0.7,0.5,0.1,0.2), rgb(0.3,0.7,0.1,0.2) )

radar = function() {
  radarchart(df_radar, axistype=1, 
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0, 7, 1.75), cglwd=0.8,
           #custom labels
           vlcex=2 )
}
radar()

# saving charts
width = 900
height = 700
saveGgplotAsImage(chart1, 'charts/chart1.jpeg', widthPx = width, heightPx = height)
saveGgplotAsImage(chart2, 'charts/chart2.jpeg', widthPx = width, heightPx = height)
saveGgplotAsImage(chart3, 'charts/chart3.jpeg', widthPx = width, heightPx = height)

# saving maps
saveLeafletAsImage(map, 'charts/map.jpeg', outputHtmlPath = 'maps/map.html',
                   widthPx = 800, heightPx = 500)

# saving radar
jpeg('charts/radar.jpeg', width = 600, height = 600)
radar()
dev.off()

# saving pages
width = 1280
height = 720
cliprect = c(0, 0, width, height)

convertHtmlToPdf('layout/page1.html', 'layout/pdf/page1.pdf', width, height, 
                 outputImagePath = 'layout/img/page1.jpeg', clipRectangle = cliprect)
convertHtmlToPdf('layout/page2.html', 'layout/pdf/page2.pdf', width, height,
                 outputImagePath = 'layout/img/page2.jpeg', clipRectangle = cliprect)
convertHtmlToPdf('layout/page3.html', 'layout/pdf/page3.pdf', width, height,
                 outputImagePath = 'layout/img/page3.jpeg', clipRectangle = cliprect)

# merging pdfs and generating report
mergePdfs(
  inputFiles = c('layout/pdf/page1.pdf', 'layout/pdf/page2.pdf', 'layout/pdf/page3.pdf'),
  outputFile = 'report.pdf'
)
