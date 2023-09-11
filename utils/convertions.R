if(!require(webshot)) install.packages('webshot')
if(!require(htmlwidgets)) install.packages('htmlwidgets')
if(!require(raster)) install.packages('raster')
if(!require(qpdf)) install.packages('qpdf')
if(!require(ggplot2)) install.packages('ggplot2')

library(webshot)
library(htmlwidgets)
library(raster)
library(qpdf)
library(ggplot2)

# saves a ggplot object as an image
## plot [list of ggplot object]: ggplot object
## savePath [string]: output image file path
## widthPx [int]: width in pixels
## heightPx [int]: height in pixels
saveGgplotAsImage = function(
    plot, 
    savePath,
    widthPx = 1000,
    heightPx = 1000) {
  
  ggplot2::ggsave(savePath, plot, width = widthPx, height = heightPx, units = c('px'))
}

# saves a leaflet object as HTML
## leafletObject [list of leaflet object]: leaflet object
## savePath [string]: output image file path
saveLeafletAsHtml = function(leafletObject, savePath) {
  htmlwidgets::saveWidget(leafletObject, savePath)
}

# saves a leaflet object as an image (first saves the leaflet object
# as html and then converts it to image)
## leafletObject [list of leaflet object]: leaflet object
## outputImagePath [string]: output image path
## outputHtmlPath [string]: output html path (same folder as outputImagePath by default)
saveLeafletAsImage = function (leafletObject, outputImagePath, outputHtmlPath = NULL) {
  if(is.null(outputHtmlPath)){
    outputHtmlPath = paste0(strsplit(outputImagePath, '\\.')[[1]][1], '.html')
  }
  
  saveLeafletAsHtml(leafletObject, outputHtmlPath)
  convertHtmlToImage(outputHtmlPath, outputImagePath)
}

# converts html file to image
## inputHtmlPath [string]: input html file path
## outputImagePath [string]: output image path
## clipRectangle [vector: c(num, num, num, num)]: clipping rectangle, values in pixels
convertHtmlToImage = function(inputHtmlPath, outputImagePath, clipRectangle = NULL) {
  webshot::webshot(inputHtmlPath, outputImagePath, cliprect = clipRectangle)
}

# converts jpeg/jpg files to pdf
## inputJpegPath [string]: input jpeg/jpg file path
## outputPdfPath [string]: output pdf path
## widthPx [int]: width in pixels
## heightPx [int]: height in pixels
convertJpegToPdf = function(
    inputJpegPath, 
    outputPdfPath, 
    widthPx = 1500, 
    heightPx = 1300) {
  
  pdf(outputPdfPath, width = widthPx/300, height = heightPx/300)
  img = raster::brick(inputJpegPath)
  raster::plotRGB(img)
  dev.off()
}

# merges pdf files
#inputFiles [vector: c(string, ...)]: vector of pdf files to be merged
#outputFile [string]: output pdf file path
mergePdfs = function(inputFiles, outputFile) {
  qpdf::pdf_combine(inputFiles, outputFile)
}