if(!require(webshot2)) remotes::install_github("rstudio/webshot2")
if(!require(htmlwidgets)) install.packages('htmlwidgets')
if(!require(raster)) install.packages('raster')
if(!require(qpdf)) install.packages('qpdf')
if(!require(ggplot2)) install.packages('ggplot2')
if(!require(magick)) install.packages('magick')

library(webshot2)
library(htmlwidgets)
library(raster)
library(qpdf)
library(ggplot2)
library(magick)

# saves a ggplot object as an image
## plot [list of ggplot object]: ggplot object
## savePath [string]: output image file path
## widthPx [int]: width in pixels
## heightPx [int]: height in pixels
saveGgplotAsImage = function(
    plot, 
    savePath,
    widthPx = 1000,
    heightPx = 1000,
    dpi = 300) {
  
  ggplot2::ggsave(savePath, plot, width = widthPx, height = heightPx, units = c('px'),
                  dpi = dpi)
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
## widthPx [int]: width in pixels
## heightPx [int]: height in pixels
saveLeafletAsImage = function (
    leafletObject, 
    outputImagePath, 
    outputHtmlPath = NULL,
    widthPx = NULL,
    heightPx = NULL) {
  if(is.null(outputHtmlPath)){
    outputHtmlPath = paste0(strsplit(outputImagePath, '\\.')[[1]][1], '.html')
  }
  
  saveLeafletAsHtml(leafletObject, outputHtmlPath)
  convertHtmlToImage(outputHtmlPath, outputImagePath, widthPx = widthPx, heightPx = heightPx)
}

# converts html file to image
## inputHtmlPath [string]: input html file path
## outputImagePath [string]: output image path
## clipRectangle [vector: c(num, num, num, num)]: clipping rectangle, values in pixels
## widthPx [int]: width in pixels
## heightPx [int]: height in pixels
convertHtmlToImage = function(
    inputHtmlPath, 
    outputImagePath, 
    clipRectangle = NULL,
    widthPx = NULL,
    heightPx = NULL,
    zoom = 1) {
  
  if(is.null(widthPx) || is.null(heightPx)) {
    webshot::webshot(inputHtmlPath, outputImagePath, cliprect = clipRectangle, delay = 0.3,
                     zoom = zoom)
  }
  else {
    webshot::webshot(inputHtmlPath, outputImagePath, cliprect = clipRectangle,
                     vwidth = widthPx,
                     vheight = heightPx,
                     delay = 0.3,
                     zoom = zoom)
  }
}

# converts html file to pdf (first converts html to image and then image to pdf)
## inputHtmlPath [string]: input html file path
## outputPdfPath [string]: output pdf path
## outputImagePath [string]: output image path
## widthPx [int]: width in pixels
## heightPx [int]: height in pixels
convertHtmlToPdf = function(
    inputHtmlPath, 
    outputPdfPath,
    widthPx,
    heightPx,
    clipRectangle = NULL,
    outputImagePath = NULL) {
  
  if(is.null(outputImagePath)) {
    outputImagePath = outputHtmlPath = paste0(strsplit(outputPdfPath, '\\.')[[1]][1], '.jpg')
  }
  
  if(is.null(clipRectangle)) {
    convertHtmlToImage(inputHtmlPath, outputImagePath, c(0, 0, widthPx, heightPx), 
                       widthPx = widthPx, heightPx =  heightPx)
  }
  else {
    convertHtmlToImage(inputHtmlPath, outputImagePath, clipRectangle, 
                       widthPx = widthPx, heightPx =  heightPx)
  }
  convertJpegToPdf(outputImagePath, outputPdfPath)
}

# converts jpeg/jpg files to pdf
## inputJpegPath [string]: input jpeg/jpg file path
## outputPdfPath [string]: output pdf path
## widthPx [int]: width in pixels
## heightPx [int]: height in pixels
convertJpegToPdf = function(
    inputJpegPath, 
    outputPdfPath) {
  
  image = magick::image_read(inputJpegPath)
  magick::image_write(image, outputPdfPath, format = 'pdf')
}

# merges pdf files
#inputFiles [vector: c(string, ...)]: vector of pdf files to be merged
#outputFile [string]: output pdf file path
mergePdfs = function(inputFiles, outputFile) {
  qpdf::pdf_combine(inputFiles, outputFile)
}