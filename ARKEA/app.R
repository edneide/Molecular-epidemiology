##%######################################################%##
#                                                          #
####                     Packages                       ####
#                                                          #
##%######################################################%##


library(scales)
library(formatR)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(dashboardthemes)
library(readr)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(ggdark)
library(bslib)


library(sparkline)
library(kableExtra)
library(formattable)







#   __________________ #< 206cc2ad885ede0b5041c3afdc0e1ac5 ># __________________
#   Function                                                                ####

server <- function(input, output) {
    
     }

#   __________________ #< 1800f744d8f9aeaf319e084fbcb9b6bd ># __________________
#   Header                                                                  ####
    
header <- dashboardHeader()


#   __________________ #< 614893309be85aaf97989a89fbc667f9 ># __________________
#   Sidebar                                                                 ####

sidebar <- dashboardSidebar()


#   __________________ #< dfecf5d7f80c1a4e3e88607e968224af ># __________________
#   Body                                                                    ####


body <- dashboardBody()


##  .................. #< 933543121e490d5078311bfd9a747cae ># ..................
##  ui and shinyApp                                                         ####




ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)



shinyApp(ui, server)

