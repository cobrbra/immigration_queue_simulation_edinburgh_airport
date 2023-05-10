library(here)
library(profvis)

setwd(here("shiny"))
source("ui.R")
source("server.R")
profvis(runApp(appDir = ".")); setwd(here())


