#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(shinyWidgets)
library(geojsonio)
library(DBI)
library(RMySQL)
library(sp)
library(dplyr)
library(tidyr)
library(ggplot2)

# Building connection to Google Cloud SQL
getSqlConnection <- function(){
  con <-
    dbConnect(
      RMySQL::MySQL(),
      username = 'shaunkhoo',
      password = 'password1',
      host = '35.193.228.98', #127.0.0.1
      dbname = 'backend'
    ) # TODO: use a configuration group `group = "my-db")`
  return(con)
}
conn <- getSqlConnection()
full <- DBI::dbGetQuery(conn, 'SELECT * FROM tutees;')
counts <- DBI::dbGetQuery(conn, 'SELECT ct, COUNT(*) AS count FROM tutees GROUP BY ct;')
DBI::dbDisconnect(conn)

# Transforming to form map
ct_geojson <- geojson_read('ct2010_geojson.geojson', method = 'local', what = 'sp')
final_geojson <- sp::merge(ct_geojson, counts, by.x = 'ct2010', by.y = 'ct')
final_geojson@data$count <- ifelse(is.na(final_geojson@data$pop), 0, final_geojson@data$count)
final_geojson@data$count <- final_geojson@data$count %>%
  replace_na(0)
count_palette <- colorNumeric(palette = 'Reds', domain = final_geojson@data$count)

# Define UI for application that draws a histogram
ui <- navbarPage("Teach and Reach", id='cb',
                 tabPanel("Demand",
                          div(class='outer',
                              tags$head(includeCSS("app1styles.css")),
                              
                              leafletOutput("map", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = 50, right = "auto", bottom = "auto",
                                            width = 400, height = "auto",
                                            
                                            h2("Demand for tutors"),
                                            h5("This map displays the areas where students are in need of pro bono tutoring. Click on the shaded regions to see more details."),
                                            
                                            h5(paste0("The total number of tutors needed is: ", sum(final_geojson@data$count, na.rm=TRUE))),
                                            h5("The subjects in demand are:"),
                                            plotOutput("plot", width = 360, height = 360)
                                            
                                            
                                            
                              )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$plot <- renderPlot({
    plotting <- full %>%
      group_by(subject) %>%
      summarise(n()) %>%
      top_n(6) %>%
      rename(count = `n()`) %>%
      arrange(count)
    ggplot(plotting, aes(x = reorder(subject,-count), y = count, fill = subject)) +
      geom_bar(stat='identity') +
      labs(x = 'Subjects',
           y = 'Count') +
      expand_limits(y = 0) +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(linetype = 'dotted', colour="grey"),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            plot.title = element_text(face='bold', hjust=0.5, size=16), 
            plot.subtitle = element_text(face='italic', hjust=0.5, size=11),
            legend.position="none")
  })
  
  output$map <- renderLeaflet({
    ny_lng = -73.994177
    ny_lat = 40.731030
    leaflet() %>%
      addProviderTiles(provider = 'CartoDB.DarkMatter') %>%
      setView(lng = ny_lng, lat = ny_lat, zoom = 12) %>%
      setMaxBounds(lng1 = ny_lng+0.4, lat1 = ny_lat-0.4,
                   lng2 = ny_lng-0.4, lat2 = ny_lat+0.4) %>%
      addPolygons(data = final_geojson, weight = 0, color = ~count_palette(final_geojson@data$count),
                  group = 'Census Tract', opacity = 0.5, fillOpacity = 0.3,
                  popup = ~paste0('No. of students who need tutors: ', final_geojson@data$count)) %>%
      addLegend("bottomright", pal = count_palette, values = final_geojson@data$count,
                title = "Distribution", opacity = 0.7)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

