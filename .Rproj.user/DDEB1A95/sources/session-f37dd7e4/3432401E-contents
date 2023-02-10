
#Minimal example to create a blank page
library(shiny) #loading the r package shiny
ui <- fluidPage() # creating the user interface (HTML page)
server <- function(input, output){} #for the backend 
shinyApp(ui = ui, server = server) # running the app

"""
- When creating a shiny app in RStudio, you can choose between a single file and a multi file app.
Difference is just about structure of code.
- All apps are started using the function runApp(). If the files are hosted on Github or any other webspace (i.e. runGitHub(), runGist() and runURL())

Deploy your Shiny app on a (Web/Intranet) server
1. Running on shinyapps.io
This site is managed by RStudio and is free for small apps with limited visits and scales up in paid versions.
2. Running your own Shiny Server Open Source
There is a free, open source version of the Shiny server that you can run on, for example, Amazon Web Services or your own server.
3. Running Shiny Server Pro
RStudio also sells a yearly subscription to Shiny Server Pro that provides security, admin and other enhancements when compared to the open source version.
"""

# Deploy a shiny app234
rsconnect::setAccountInfo(name='nehasingh2', token='9C0B13792DD28D0D639CBAAB23ABE918', secret='AwDv8hoG0Ihgq2H5al96Jj83XcD/3n71TZ9oX3Zo')
library(rsconnect)
rsconnect::deployApp('~/day2_summary')


"""
RUN RSTUDIO AS ADMINISTRATOR
Incase of Mac, go to Applications, then right click on RStudio and
  Select 'Show Package Contents'
  Go to Contents/MacOS
  Now open terminal(in bash mode). Type sudo and drag the RStudio.exec into terminal and press on ENTER
  Now RStudio will have admin access!
"""

"""
FLUID WEBSITE LAYOUT- Also referred as liquid layout where the majority of the components inside have percentage widths and thus adjust to the user's screen resolution
to create a fluid based layout, fluidPage() function is used. 
fluidRow() function is used to create rows(horizontal split) within the grid
column() function is used inside fluidRow() to create columns inside these rows
"""
ui <- fluidPage(
  fluidRow(
    column(2, "sidebar_1"),
    column(2, "sidebar_2"),
    column(8, "main")
  )
)
#other layout options - sidebarLayout(), splitLayout(), flowLayout(), verticalLayout()

#titlePanel
ui <- fluidPage(
  titlePanel("title panel"),
  sidebarLayout(position = "right",
                sidebarPanel("sidebar panel"),
                mainPanel("main panel")
                )
)

"""
To Include an image 
1. Place the file in the www subdirectory
2. Link to it with img(src="<file name>")

To Include a CSS file
- use includeCSS(), or
1. Place the file in the www subdirectory
2. Link to it with
tags$head(tags$link(rel="stylesheet", type="text/css", href="<file_name>"))

To Include Javscript
- use includeScript(), or
1. Place the file in the www subdirectory
2. Link to it with
tags$head(tags$script(src="<file_name>"))
"""

#shiny widget gallery- https://shiny.rstudio.com/gallery/widget-gallery.html


#TWO MOST COMMON WAYS TO IMPLEMENT REACTIVE PROGRAMMING IN SHINY
#render* - An output will automatically update whenever an input in its render* function changes
output$z <- renderText({
  input$a
})

#Reactive expression - use reactive to create objects that will be used in multiple outputs
x <- reactive({
  input$a
})
output$y <- renderText({
  x()
})
output$z <- renderText({
  x()
})

"""
Where you place R code in server.R will determine how many times the commands are run (or rerun)
- Beginning of the file, outside of the server function - shiny will run this code once.
Source scripts, load libraries and read data sets
- inside server function, but outside of any render*() function calls -
run once each time a user visits the app. User specific objects
- inside of a render*() function- Shiny will rerun this code each time a user changes a widget mentioned in the chunk.
Code that Shiny must rerun to build an object
"""







