setwd('C:/Users/Lenovo/Desktop/automation')

library(leaflet)
library(dplyr)
library(ggplot2)
source('utils/convertions.R')


# creating dataframe
df <- data.frame(
  x = c(1, 2, 3, 4),
  y = c(3, 7, 5, 1),
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

# saving charts
width = 900
height = 700
saveGgplotAsImage(chart1, 'charts/chart1.jpeg', widthPx = width, heightPx = height)
saveGgplotAsImage(chart2, 'charts/chart2.jpeg', widthPx = width, heightPx = height)
saveGgplotAsImage(chart3, 'charts/chart3.jpeg', widthPx = width, heightPx = height)

# saving maps
saveLeafletAsImage(map, 'charts/map.jpeg', outputHtmlPath = 'maps/map.html',
                   widthPx = 800, heightPx = 500)

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
