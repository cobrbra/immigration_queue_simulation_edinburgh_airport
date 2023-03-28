library(tidyverse)
library(here)
library(showtext)

font_add_google("Lato")
showtext_auto()

data.frame(x=1:3, y = 2:4) %>% 
  ggplot(aes(x = x, y = y, fill = factor(x))) + geom_col() + 
  labs(x = "x axis",
       y = "y axis",
       title = "The title goes here and should have nice typeface") + 
  theme_edi_airport() +
  scale_fill_manual(values = edi_airport_colours)

edi_airport_colours <- c("#7D0C6E", "#A19384", "#F2D6EA", "#2D0255", "#","#")

theme_edi_airport <- function() {
  font <- "Lato"
  theme_minimal() %+replace%
    theme(
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        # face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 1),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 15),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
    ) 
}

get_figures <- function(results) {
  # results <- targets::tar_read(example_results) # Use for debugging, COMMENT WHEN RUNNING TARGETS
  
  figures <- list()
  
  figures$mt_cars_summary <- results$mt_table %>% 
    ggplot(aes(x = cyl, y = mpg)) + 
      geom_point() + 
      geom_smooth(method = "lm") +
      theme_minimal()
  
  figures$mpg_model_actual_vs_predicted <- results$mpg_model_predictions %>% 
    ggplot(aes(x = actual, y = predicted)) + 
      geom_point() +
      geom_smooth(method = "lm") + 
      theme_minimal()
  
  # figures$figure_1 <- ...
  # insert more code that generates figures here
  
  
  for (figure_index in seq_len(length(figures))) {
    ggsave(paste0(here("figures/"), names(figures)[figure_index], ".png"), 
           figures[[figure_index]])
  }
  return(figures)
}