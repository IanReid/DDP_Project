library(shiny)

shinyUI(fluidPage(
    titlePanel("Explore mtcars"),
    sidebarLayout(
        sidebarPanel(
            h5("Check (click) the box below to show the car model for each point."),
            h5("Uncheck it for a less cluttered plot."),
            checkboxInput("show_labels","Identify car models"),
            br(),
            p("Car weight has a big effect on fuel economy."),
            p("Click one of the buttons below to see how other factors affect
    the relationship between weight and miles per gallon."),
            radioButtons("selection",
                        "Choose a factor:",
                        choices = c("None" = "",
                            "Number of cylinders" = "Cylinders",
                            "V or straight engine" = "Shape",
                            "Automatic or manual transmission" = "Transmission",
                            "Number of gears" = "Gears")
                        ),
        ),

        # Show a plot of the generated model
        mainPanel(
            plotOutput("modelPlot"),
            textOutput("R2"),
            textOutput("p_value")
        )
    )
))
