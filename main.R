setwd('C:/Users/Lenovo/Desktop/automation')

#install.packages("remotes")
#remotes::install_github("ricardo-bion/ggradar")

library(leaflet)
library(dplyr)
library(ggplot2)
library(fmsb)
# library(ggradar)
library(ggiraphExtra)

source('utils/convertions.R')

# creating dataframe
df <- data.frame(
  names = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'),
  x = c(1, 2, 3, 4, 5, 6, 7, 8, 3),
  y = c(3, 7, 5, 1, 3, 3, 2, 9, 4),
  z = c(2, 5, 4, 2, 9, 9, 2, 3, 5),
  h = c(1, 2, 3, 1, 2, 1, 9, 5, 6),
  lat = c(-21.15712, -21.19058, -21.15674, -21.17922, -21.16182, -21.16770, -21.18363, -21.19701, -21.15414),
  lon = c(-47.79177, -47.83451, -47.79678, -47.71812, -47.79230, -47.80270, -47.71732, -47.81697, -47.78691)
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
# max longitude - min longitude

# df -> dataframe(lat, lon)
getSpan = function(d) {
  return(abs(max(d[,2]) - min(d[,2])))
}

# getting positions of the marker labels (center, left, right)
# df -> dataframe(lat, lon)
getPositions = function(d) 
{
  positions = c()
  
  for(i in 1:nrow(d))
  {
    lat_i = d[i, "lat"]
    lon_i = d[i, "lon"]
    
    distances_i = data.frame()
    
    for(j in 1:nrow(d)) 
    {
      if(i == j) next
      lat_j = d[j, "lat"]
      lon_j = d[j, "lon"]
      
      if(abs(lat_i - lat_j) < 0.03 && abs(lon_i - lon_j) < 0.01)
        distances_i = rbind(distances_i, c(lat_i - lat_j, lon_i - lon_j))
    }
    
    print(distances_i)
    
    
    if(nrow(distances_i) > 0 && !is.null(distances_i))
    {
      colnames(distances_i) = c('lat_dist', 'lon_dist')
      pos_lons = nrow(distances_i %>% filter(lon_dist > 0))
      neg_lons = nrow(distances_i %>% filter(lon_dist < 0))
      pos_lats = nrow(distances_i %>% filter(lat_dist > 0))
      neg_lats = nrow(distances_i %>% filter(lat_dist < 0))
      
      if(pos_lons > neg_lons && neg_lats < pos_lats) {
        positions = c(positions, "right")
      } else if(neg_lons > pos_lons && neg_lats < pos_lats) {
        positions = c(positions, "left")
      }
      else {
        positions = c(positions, "center")
      }
    }
    else {
      positions = c(positions, "center")
    }
  }
    
  return(positions)
}

df$positions = getPositions(df[, c("lat", "lon")])

map = leaflet(df) %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron)

# adding markers to the map
addMarkersToMap = function(map, df, direction = 'auto')
{
  if(nrow(df) == 0) return(map)
  
  offset_y = 0
  if(direction == "center") {
    offset_y = -8
  } else {
    offset_y = 8
  }
  map = map %>% addMarkers(map, lng = df$lon, lat = df$lat,
                           label = df$lat,
                           labelOptions = labelOptions(
                             textOnly = TRUE,
                             noHide = TRUE,
                             style = list("font-weight" = "700"),
                             sticky = "bottom",
                             offset = c(0, offset_y),
                             direction = direction,
                           ),
                           icon = icons(
                             iconUrl = ifelse(df$x == 1, 'markers/red_marker.png',
                                              ifelse(df$x == 2, 'markers/orange_marker.png',
                                                     ifelse(df$x == 3, 'markers/yellow_marker.png', 'markers/green_marker.png'))),
                             iconWidth = 38, iconHeight = 38,
                             iconAnchorX = 19, iconAnchorY = 38,
                           )
  )
  return(map)
}

map = map %>% addMarkersToMap(df %>% filter(positions == 'left'), 'left')
map = map %>% addMarkersToMap(df %>% filter(positions == 'right'), 'right')
map = map %>% addMarkersToMap(df %>% filter(positions == 'center'), 'center')

legend <- paste0(
  "<span>",1," Escola 1","</span><br>",
  "<span>",2," Escola 2","</span><br>",
  "<span>",2," Escola 2","</span><br>",
  "<span>",2," Escola 2","</span><br>",
  "<span>",2," Escola 2","</span><br>",
  "<span>",2," Escola 2","</span><br>",
  "<span>",2," Escola 2","</span><br>",
  "<span>",2," Escola 2","</span><br>",
  "<span>",2," Escola 2","</span><br>"
)

map <- map %>% addControl(
  legend,
  position = "bottomleft"
)
map

# radar chart
#df_radar = df[c(1:4)] %>% slice(1)
df_radar = df[c(2:5)]
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
