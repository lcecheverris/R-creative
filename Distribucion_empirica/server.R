#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    z=seq(-10,10,length.out = 1000)
    aleatorio1=rnorm(z,0,1)  #coge mil numeros, no significa que este entre -12 y 12
    aleatorio2=rnorm(z, mean= 4, sd=0.5)
    
    suma=aleatorio1+aleatorio2
    #hist(suma) saca frecuencia, no probabilidad
   
    
    
    yteorica=dnorm(z, 4, sqrt(1^2+0.5^2)) #si sumo dos normales, la normal resultante es una normal. media=suma, desv=suma
    #no sumo desviaciones estandar en la nomral, sino varianzas
   
    
    
    x    <- suma 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(suma, breaks = bins, probability=TRUE) #histograma de funcion de probabilidad
    
    lines(z,yteorica, lwd=4, col="red")
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    
    #x    <- faithful[, 2] 
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})


