library(shiny)
library(profvis)

source("credentials/credentials.R")
source("ui.R")
source("server.R")

profvis(runApp(appDir = "."))


