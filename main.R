library(leaflet)
library(dplyr)
library(ggplot2)
library(webshot)
library(htmlwidgets)

setwd('C:/Users/Lenovo/Desktop/automation')

# creating dataframe
df <- data.frame(
  x = c(1, 2, 3, 4),
  y = c(3, 7, 5, 1),
  lat = c(-21.82025, -21.20772, -22.97754, -23.63144),
  lon = c(-46.55864, -50.43809, -49.86874, -46.76599)
)

# creating charts
chart1 = df %>% ggplot(aes(x = x, y = y)) +
  geom_col(fill = '#6666ff')

chart2 = df %>% ggplot(aes(x = x, y = y)) +
  geom_line(color = '#ff6666', size = 2)

chart3 = df %>% ggplot(aes(x = x, y = y)) +
  geom_col(aes(x = x, y = y), fill = '#66ff66') +
  geom_point(color = '#ff6666', size = 8)

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
  
# saving charts
ggsave('charts/chart1.jpg', chart1, units = c('px'), width = 1400, height = 1200)
ggsave('charts/chart2.jpg', chart2, units = c('px'), width = 1400, height = 1200)
ggsave('charts/chart3.jpg', chart3, units = c('px'), width = 1400, height = 1200)

# saving maps
saveWidget(map, 'maps/map.html')
webshot('maps/map.html', 'charts/map.jpg')

# saving pages
webshot('layout/page1.html', 'layout/img/page1.jpg', cliprect = c(0, 0, 1000, 600))
webshot('layout/page2.html', 'layout/img/page2.jpg', cliprect = c(0, 0, 1000, 600))
webshot('layout/page3.html', 'layout/img/page3.jpg', cliprect = c(0, 0, 1000, 600))