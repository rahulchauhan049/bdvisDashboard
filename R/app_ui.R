#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import dplyr
#' @import dashboardthemes
#' @import ggpubr
#' @import plotly
#' @import circlepackeR
#' @import data.tree
#' @import flexdashboard
#' @import bdvis
#' @import ggplot2
#' @import tidyr
#' @import treemap
#' @import sunburstR
#' 

app_ui <- function() {
  
  dashboardPage(skin="green",
                dashboardHeader(title="bdvis dashboard"),
                
                #----------------------SideBar Start-------------------------------------
                dashboardSidebar(
                  sidebarMenu(id="sideBar",
                              menuItem("Data Input", tabName = "dataInputTab", icon = icon("database")),
                              menuItem("Data Summary", tabName = "dataSummary", icon = icon("database")),
                              menuItem("Spatial Visualization", tabName = "spatialTab", icon = icon("map-marked")),
                              menuItem("Taxonomic Visualization", tabName = "taxonomicTab", icon = icon("connectdevelop")),
                              menuItem("Temporal Visualization", tabName = "temporalTab", icon = icon("clock"))
                  )#Sidebar menu ends here
                ),#sidebar Dashboard ends here
                #----------------------SideBar End-------------------------------------
                
                
                
                #----------------------Body Start-------------------------------------
                dashboardBody(
                  shinyDashboardThemes(
                    theme = "grey_dark"
                  ),
                  golem_add_external_resources(),
                  # Boxes need to be put in a row (or column)
                  tabItems(
                    
                    tabItem(tabName = "dataInputTab",
                            # -------------------------------
                            mod_dataInput_ui("dataInput_ui_1")
                            # -------------------------------
                    ),
                    
                    
                    
                    tabItem(tabName = "dataSummary",
                            # -------------------------------
                            mod_dataSummary_ui("dataSummary_ui_1")
                            # -------------------------------
                    ), 
                    
                    
                    
                    tabItem(tabName = "spatialTab",
                            # -------------------------------
                            
                            mod_spatial_ui("spatial_ui_1")
                            
                            # -------------------------------
                    ),
                    
                    
                    
                    tabItem(tabName = "taxonomicTab",
                            # -------------------------------
                            
                            mod_taxonomic_ui("taxonomic_ui_1")
                            
                            # -------------------------------
                    ),
                    
                    
                    tabItem(tabName = "temporalTab",
                            # -------------------------------
                            
                            mod_temporal_ui("temporal_ui_1")
                            
                            # -------------------------------
                    )
                    
                  )  
                )#Dashboard Body ends here
                
                #-------------------------Body Ends Here-------------------------------
                
  )#End of dashboard page
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'bdvisShiny')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
