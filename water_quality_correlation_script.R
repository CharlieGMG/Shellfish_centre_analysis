#https://r-coder.com/correlation-plot-r/ 

library(ggplot2)
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(corrplot)

WATER_LOG10_CLEAN <- read_excel("C:/Users/44734/Desktop/Shellfish_Centre/MEWQ/RCode/Corr.Matrix/WATER_LOG10_CLEAN.xlsx")
View(WATER_LOG10_CLEAN)

#function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1.5) # Resize the text by level of correlation = + cex.cor * Cor
}

#function to add hist
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  his <- hist(x, plot = FALSE)
  breaks <- his$breaks
  nB <- length(breaks)
  y <- his$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = rgb(0, 1, 1, alpha = 0.5), ...)
  # lines(density(x), col = 2, lwd = 2) # Uncomment to add density lines
}

#upper pannel 
upper.panel<-function(x, y){
  points(x,y, pch = 19, col="red")
}


#plot 
Corr.matrix <- pairs(WATER_LOG10_CLEAN, 
                     lower.panel = panel.cor,
                     upper.panel = upper.panel)

#save - cairo 
ggsave(plot = last_plot(), "W_Corr.matrix1.png", type = "cairo")


