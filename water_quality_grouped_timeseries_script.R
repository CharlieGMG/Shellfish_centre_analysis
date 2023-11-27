library(ggplot2)
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)


##import dataset
W_Grouped <- read_excel("C:/Users/44734/Desktop/Shellfish_Centre/MEWQ/RCode/Viral Graphics/Grouped/W_Grouped.xlsx")
View(W_Grouped)

## theme 
theme.clean <- function(){ 
  
  theme_bw()+ 
    
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          
          axis.text.y = element_text(size = 12), 
          
          axis.title.x = element_text(size = 14, face = "plain"),              
          
          axis.title.y = element_text(size = 14, face = "plain"),              
          
          panel.grid.major.x = element_blank(),                                           
          
          panel.grid.minor.x = element_blank(), 
          
          panel.grid.minor.y = element_blank(), 
          
          panel.grid.major.y = element_blank(),   
          
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"), 
          
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5), 
          
          legend.text = element_text(size = 12, face = "italic"),           
          
          legend.position = "right") 
  
} 


#time series CrAss 
W_Grouped$Date = as.Date(W_Grouped$Date, format = "%m/%d/%Y")


#relabel to bypass alphabetical order 
W_Grouped_Label <- c("Adda" = "Adda", "Cegin" = "Cegin", "Ogwen" = "Ogwen", "zCegin Channel" = "Cegin Channel")

##ggplot
(W1 <- ggplot(data = W_Grouped, aes(x = Date, y = Value, colour=Genotype)) + 
    
    geom_point() + 
    
    facet_wrap(~ ID, ncol=1, labeller = as_labeller(W_Grouped_Label)) +
    
    scale_y_log10() + 
    
    scale_x_date(date_breaks = "1 month" , date_labels = "%b") +
    
    scale_y_log10(labels = scales::comma) +#set date labels  
    
    labs(y = "Viral genome copies/100ml of surface water") +
   
     geom_hline(yintercept = 44.8, colour = "black", linetype = "dashed") +
    
    
    
    theme.clean()) 



#save - cairo 
ggsave("W_Grouped_timeseries.png", type = "cairo") 
