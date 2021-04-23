library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
data("mtcars")
MTcars <- select(mtcars, MilesPerGallon=mpg, Cylinders=cyl,Weight=wt,Shape=vs,
                 Transmission=am, Gears=gear) %>%
     mutate(Cylinders=factor(Cylinders), Gears=factor(Gears),
            Shape=factor(Shape,labels=c("V","Straight")), 
            Transmission=factor(Transmission,labels=c("Automatic","Manual")))

# Define server logic required to plot a regression
shinyServer(function(input, output) {
    mtc.t <- reactive({
        condition = input$selection
        if(condition == "") {
            select(MTcars,MilesPerGallon, Weight)
        } 
        else {
            select(MTcars,MilesPerGallon, Weight, all_of(condition))
        }
    })
    
    fit <- reactive({
        lm(MilesPerGallon ~ ., data = mtc.t())
        })
    
    mtc <- reactive({
        mutate(mtc.t(), Prediction=fit()$fitted.values)
    })
    
    sumfit <- reactive({summary(fit())})
    
    make_title <- reactive({
        if (input$selection == "") {
            "Regression: MilesPerGallon ~ Weight"
        } else {
            c("Regression: MilesPerGallon ~ Weight +", input$selection)
        }
    })
    output$title <- renderText(make_title())
    
    make_p_value <- reactive({
        if (input$selection == "") {
            ""
        } else {
            coefs <- sumfit()$coefficients
            p_vals <- coefs[3:length(coefs[,4]),4]
            c("Probability that", input$selection, 
                       "has no effect:", 
                       round(min(p_vals),4))
        }
    })
    output$p_value <- renderText(make_p_value())

    output$modelPlot <- renderPlot({
        condition <- input$selection
        car_names <- rownames(mtc())
        if (condition == ""){
            p <- ggplot(mtc(), aes(x=Weight, y=MilesPerGallon)) +
                geom_point()  +
                geom_line(aes(x=Weight,y=Prediction)) + 
                ggtitle("Regression: MilesPerGallon ~ Weight")
            if (input$show_labels) {
                p <- p + geom_label_repel(aes(label=car_names), 
                                          fill = "white", size=3.5,
                                          max.overlaps = 12) }
        } 
        else {
            p <- ggplot(mtc(), aes(x=Weight, y=MilesPerGallon)) +
                geom_point(aes(color=mtc()[,3])) +
                geom_line(aes(x=Weight,y=Prediction, color=mtc()[,3]), size=2) + 
                scale_colour_discrete(condition) + 
                ggtitle(paste("Regression: MilesPerGallon ~ Weight +", condition))
            if (input$show_labels) {
                p <- p + geom_label_repel(aes(label=car_names, fill = mtc()[,3]),
                                          colour="white", size=3.5, 
                                          show.legend = FALSE,
                                          max.overlaps = 12)             
            }
        }
        p })

    output$R2 <- renderText(c("R-squared:", round(sumfit()$r.squared,4)))
})
