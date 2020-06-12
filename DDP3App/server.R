library(shiny)
library(miniUI)

shinyServer(function(input, output) {
    mtcars$dispsp <- ifelse(mtcars$disp - 200 > 0, mtcars$disp - 200, 0)
    model1 <- lm(mpg~disp, data=mtcars)
    model2 <- lm(mpg~dispsp + disp, data=mtcars)
    
    model1pred <- reactive({
        dispInput <- input$sliderDISP
        predict(model1, newdata=data.frame(disp=dispInput))
    })
    
    model2pred <- reactive({
        dispInput <- input$sliderDISP
        predict(model2, newdata=data.frame(disp=dispInput,
                                           dispsp= ifelse(dispInput - 200 > 0,
                                                          dispInput - 200, 0)))
    })
    output$plot1 <- renderPlot({
        dispInput <- input$sliderDISP
        
        plot(mtcars$disp, mtcars$mpg, xlab="Volume of Engine Displacement", ylab="Miles per Gallon",
             pch=24, xlim=c(0,500), ylim =c(0, 40))
        if(input$showModel1){
            abline(model1,col="green",lwd=2)
        }
        if(input$showModel2){
            model2lines <- predict(model2, newdata=data.frame(disp=0:500, 
                                                              dispsp=ifelse(0:500 - 200 > 0, 
                                                                            0:500 - 200, 0)
            ))
            lines(0:500, model2lines, col="blue", lwd=2)
        }
        legend(300,40,c("Model 1 Prediction", "Model 2 Prediction"), pch=16, col=c("green","blue"),
               bty="n", cex=1.5)
        points(dispInput, model1pred(), col="green", pch=10, cex=3)
        points(dispInput, model2pred(), col="blue", pch=10, cex=3)
    })
    output$pred1 <- renderText({
        model1pred()
    })
    output$pred2 <- renderText({
        model2pred()
    })
})