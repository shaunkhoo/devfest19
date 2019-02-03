#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme('paper'),
  
  # Application title
  titlePanel("Application Form"),
  textInput("fn", "First Name", "", width = 500),
  textInput("ln", "Last Name", "", width = 500),
  textInput("email", "Email Address", "", width = 500),
  textInput("phone", "Phone Number", "", width = 500),
  radioButtons(inputId = 'grade', "Which grade are you willing to teach?",
               choices = list('Elementary', 'Middle', 'High'),
               selected = NULL,
               inline = FALSE,
               width = 500),
  conditionalPanel(width=12,
                   condition = "input.grade.includes('Elementary')",
                   pickerInput(inputId = 'Esubject', label = 'Select subjects', width = 500, 
                               choices = list("mathematics", "science", "social studies", "reading", "writing"),
                               options = pickerOptions(size = 10,
                                                       maxOptions = 3, maxOptionsText = 'Select only up to 3 subjects',
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No subjects selected'),
                               multiple = TRUE)), 
  conditionalPanel(width=12,
                   condition = "input.grade.includes('Middle')",
                   pickerInput(inputId = 'Msubject', label = 'Select subjects', width = 500,
                               choices = list("mathematics", "english", "science", "social studies","spanish", "french", "business"),
                               options = pickerOptions(size = 10,
                                                       maxOptions = 3, maxOptionsText = 'Select only up to 3 subjects',
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No subjects selected'),
                               multiple = TRUE)), 
  conditionalPanel(width=12,
                   condition = "input.grade.includes('High')",
                   pickerInput(inputId = 'Hsubject', label = 'Select subjects', width = 500,
                               choices = list("algebra", "calculus", "statistics", "chemistry", "physics", 
                                              "biology", "earth science", "anatomy", "literature", "writing", 
                                              "history", "spanish", "german", "french", "accounting"),
                               options = pickerOptions(size = 10,
                                                       maxOptions = 3, maxOptionsText = 'Select only up to 3 subjects',
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No subjects selected'),
                               multiple = TRUE)),
  radioButtons(inputId = 'availslots', "How many available timeslots do you have?",
               choices = list('1', '2', '3'),
               selected = NULL,
               inline = FALSE,
               width = 500),
  conditionalPanel(width=12,
                   condition = "input.availslots.includes('1')",
                   pickerInput(inputId = 'timingd1', label = 'Select your first available day', width = 500,
                               choices = list(
                                 "Monday" = "1",
                                 "Tuesday" = "2",
                                 "Wednesday" = "3",
                                 "Thursday" = "4",
                                 "Friday" = "5",
                                 "Saturday" = "6",
                                 "Sunday" = "7"
                               ),
                               options = pickerOptions(actionsBox = FALSE, size = 10,
                                                       maxOptions = 1, maxOptionsText = 'Select only up to 1 day',
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No days selected'),
                               multiple = TRUE), 
                   pickerInput(inputId = 'timingt1', label = 'Select your first available timing', width = 500,
                               choices = list(
                                 "3pm - 4pm" = "A",
                                 "4pm - 5pm" = "B",
                                 "5pm - 6pm" = "C",
                                 "6pm - 7pm" = "D",
                                 "7pm - 8pm" = "E",
                                 "9pm - 10pm" = "F"
                               ),
                               options = pickerOptions(actionsBox = TRUE, size = 10,
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No timings selected'),
                               multiple = TRUE)),
  conditionalPanel(width=12,
                   condition = "input.availslots.includes('2')",
                   pickerInput(inputId = 'timingd1', label = 'Select your first available day', width = 500,
                               choices = list(
                                 "Monday" = "1",
                                 "Tuesday" = "2",
                                 "Wednesday" = "3",
                                 "Thursday" = "4",
                                 "Friday" = "5",
                                 "Saturday" = "6",
                                 "Sunday" = "7"
                               ),
                               options = pickerOptions(actionsBox = FALSE, size = 10,
                                                       maxOptions = 1, maxOptionsText = 'Select only up to 1 day',
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No days selected'),
                               multiple = TRUE), 
                   pickerInput(inputId = 'timingt1', label = 'Select your first available timing', width = 500,
                               choices = list(
                                 "3pm - 4pm" = "A",
                                 "4pm - 5pm" = "B",
                                 "5pm - 6pm" = "C",
                                 "6pm - 7pm" = "D",
                                 "7pm - 8pm" = "E",
                                 "9pm - 10pm" = "F"
                               ),
                               options = pickerOptions(actionsBox = TRUE, size = 10,
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No timings selected'),
                               multiple = TRUE),
                   pickerInput(inputId = 'timingd2', label = 'Select your second available day', width = 500,
                               choices = list(
                                 "Monday" = "1",
                                 "Tuesday" = "2",
                                 "Wednesday" = "3",
                                 "Thursday" = "4",
                                 "Friday" = "5",
                                 "Saturday" = "6",
                                 "Sunday" = "7"
                               ),
                               options = pickerOptions(actionsBox = FALSE, size = 10,
                                                       maxOptions = 1, maxOptionsText = 'Select only up to 1 day',
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No days selected'),
                               multiple = TRUE), 
                   pickerInput(inputId = 'timingt2', label = 'Select your second available timing', width = 500,
                               choices = list(
                                 "3pm - 4pm" = "A",
                                 "4pm - 5pm" = "B",
                                 "5pm - 6pm" = "C",
                                 "6pm - 7pm" = "D",
                                 "7pm - 8pm" = "E",
                                 "9pm - 10pm" = "F"
                               ),
                               options = pickerOptions(actionsBox = TRUE, size = 10,
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No timings selected'),
                               multiple = TRUE)),
  conditionalPanel(width=12,
                   condition = "input.availslots.includes('3')",
                   pickerInput(inputId = 'timingd1', label = 'Select your first available day', width = 500,
                               choices = list(
                                 "Monday" = "1",
                                 "Tuesday" = "2",
                                 "Wednesday" = "3",
                                 "Thursday" = "4",
                                 "Friday" = "5",
                                 "Saturday" = "6",
                                 "Sunday" = "7"
                               ),
                               options = pickerOptions(actionsBox = FALSE, size = 10,
                                                       maxOptions = 1, maxOptionsText = 'Select only up to 1 day',
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No days selected'),
                               multiple = TRUE), 
                   pickerInput(inputId = 'timingt1', label = 'Select your first available timing', width = 500,
                               choices = list(
                                 "3pm - 4pm" = "A",
                                 "4pm - 5pm" = "B",
                                 "5pm - 6pm" = "C",
                                 "6pm - 7pm" = "D",
                                 "7pm - 8pm" = "E",
                                 "9pm - 10pm" = "F"
                               ),
                               options = pickerOptions(actionsBox = TRUE, size = 10,
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No timings selected'),
                               multiple = TRUE),
                   pickerInput(inputId = 'timingd2', label = 'Select your second available day', width = 500,
                               choices = list(
                                 "Monday" = "1",
                                 "Tuesday" = "2",
                                 "Wednesday" = "3",
                                 "Thursday" = "4",
                                 "Friday" = "5",
                                 "Saturday" = "6",
                                 "Sunday" = "7"
                               ),
                               options = pickerOptions(actionsBox = FALSE, size = 10,
                                                       maxOptions = 1, maxOptionsText = 'Select only up to 1 day',
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No days selected'),
                               multiple = TRUE), 
                   pickerInput(inputId = 'timingt2', label = 'Select your second available timing', width = 500,
                               choices = list(
                                 "3pm - 4pm" = "A",
                                 "4pm - 5pm" = "B",
                                 "5pm - 6pm" = "C",
                                 "6pm - 7pm" = "D",
                                 "7pm - 8pm" = "E",
                                 "9pm - 10pm" = "F"
                               ),
                               options = pickerOptions(actionsBox = TRUE, size = 10,
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No timings selected'),
                               multiple = TRUE),
                   pickerInput(inputId = 'timingd3', label = 'Select your third available day', width = 500,
                               choices = list(
                                 "Monday" = "1",
                                 "Tuesday" = "2",
                                 "Wednesday" = "3",
                                 "Thursday" = "4",
                                 "Friday" = "5",
                                 "Saturday" = "6",
                                 "Sunday" = "7"
                               ),
                               options = pickerOptions(actionsBox = FALSE, size = 10,
                                                       maxOptions = 1, maxOptionsText = 'Select only up to 1 day',
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No days selected'),
                               multiple = TRUE), 
                   pickerInput(inputId = 'timingt3', label = 'Select your third available timing', width = 500,
                               choices = list(
                                 "3pm - 4pm" = "A",
                                 "4pm - 5pm" = "B",
                                 "5pm - 6pm" = "C",
                                 "6pm - 7pm" = "D",
                                 "7pm - 8pm" = "E",
                                 "9pm - 10pm" = "F"
                               ),
                               options = pickerOptions(actionsBox = TRUE, size = 10,
                                                       selectedTextFormat = "values",
                                                       noneSelectedText = 'No timings selected'),
                               multiple = TRUE)),
  pickerInput(inputId = 'language', label = 'Which languages can you speak and write?', width = 500,
              choices = list("None", "Spanish", "Arabic", "Portuguese", "French", "Bengali", "Japanese", "German", "Tamil", "Bengali", "Italian", "Russian", "Chinese", "Korean", 'Hindi'),
              options = pickerOptions(actionsBox = FALSE, size = 10,
                                      maxOptions = 3, maxOptionsText = 'Select only up to 3 languages',
                                      selectedTextFormat = "values",
                                      noneSelectedText = 'No languages selected'),
              multiple = TRUE),
  br(),
  h6("Please select on the map below where you would like to tutor at:"),
  leafletOutput("map", width = 500),
  htmlOutput('selected_coords'),
  br(),
  actionButton(inputId = 'submit', label = 'Submit'),
  conditionalPanel(width=12,
                   condition = "input.submit%2==1",
                   h5("Thank you for submitting your application! We will be in touch within 2-3 working days.")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    ny_lng = -73.994177
    ny_lat = 40.731030
    leaflet() %>%
      addProviderTiles(provider = 'CartoDB.DarkMatter') %>%
      setView(lng = ny_lng, lat = ny_lat, zoom = 12) %>%
      setMaxBounds(lng1 = ny_lng+0.4, lat1 = ny_lat-0.4,
                   lng2 = ny_lng-0.4, lat2 = ny_lat+0.4)
  })
  
  observeEvent(input$map_click,
               {
                 leafletProxy('map') %>%
                   clearGroup('highlighted coord') %>%
                   addCircles(lat = input$map_click$lat, lng = input$map_click$lng,
                              radius = 500, stroke = TRUE, color = '#f46161', weight = 2,
                              fillColor = '#ed5b01', fillOpacity = 0.5,
                              group = 'highlighted coord') %>%
                   addCircles(lat = input$map_click$lat, lng = input$map_click$lng,
                              radius = 75, stroke = TRUE, color = '#f46161', weight = 2,
                              fillColor = '#c91717', fillOpacity = 1,
                              group = 'highlighted coord')
                 }
  )
  
  output$selected_coords <- renderText({
    if (is.null(input$map_click)) {
      paste("<h6 style='color:black;'>Selected coordinates: NA</h5>", sep="")
    } else paste("<h6 style='color:black;'>Selected coordinates: ", round(input$map_click$lat, 3), ", ", round(input$map_click$lng, 3), "</h5>", sep = "")
  })
  
}  
# Run the application 
shinyApp(ui = ui, server = server)