
library(shiny)
library(png)
library(ggplot2)
library(e1071)

#read data
data1 = read.csv("c:/Users/goeyd/r projects/mt simulation/dataset.csv", sep = " ")

data1$flight.risk = as.factor(data1$flight.risk)


dataflight = data1[data1$flight.risk==1,]
datanonflight = data1[data1$flight.risk==0,]


train = rbind(dataflight[1:500,], datanonflight[1:500,])
test = rbind(dataflight[501:1190,], datanonflight[501:6490,])

# functions for inside shiny ------------------------------------------------------------
#function that atumaticly turns variables selected into an accuracy rating.
svmmodel  = function(x,y){
  formula = as.formula(paste(y, "~", x ))  
  model = svm(formula, data=train)
  prediction = predict(model, newdata=test)
  accuracy = sum(prediction == test)/dim(test)[1]
  return(accuracy)
}



# server ------------------------------------------------------------------
# Define server statements shiny
shinyServer(function(input, output, session) {
  #tab panel intorudction to the dashboard
  output$text = renderText("this is the introduction to the dashboard")
  ######tab panel data view
  output$dataview = renderTable(head(data1))
  ######tab panel gender
  output$genplot = renderPlot({
    if(input$gender == "male"){
      h = ggplot(data = data1[data1$gender ==1, ], aes_string(x = input$variable))+geom_histogram()
      print(h)
    }else if(input$gender =="female"){
      h = ggplot(data = data1[data1$gender ==0, ], aes_string(x = input$variable))+geom_histogram()
      print(h)
    } else{
      h = ggplot(data = data1, aes_string(x = input$variable))+geom_histogram()
      print(h)
    }
  })
  #####tab panel regression
  regstat = reactive({lm(as.formula(paste(input$variable_y, "~", paste(input$variable_x,collapse="+"))), data=data1)})
  output$regTab <- renderTable({
    input$Updatebutton1
    isolate({
      if((!is.null(input$variable_x))){
        summary(regstat())$coefficients
      } else {
        print(data.frame(Warning="Please select Model Parameters."))
      }})
  })
  ######tab panel summary
  output$summary = renderTable(summary(data1))
  #######tab panel Plot
  output$dotplot =  renderPlot({
    g = ggplot(data = data1, aes_string(x = input$variablex, y  = input$variabley))+geom_point()
    print(g)
  })
  ######tab panel svm
  #svmstat = reactive({svm(as.formula(paste(input$svmy, "~", paste(input$svmx,collapse="+"))), data=train, kernel = "linear", cost = 0.5011, cross = 10)})  
  
  svm1 =  reactive({svmmodel(input$variablex2,input$variable2y)})
  output$svm <- renderTable({
    input$Updatebutton2
    isolate({
      if(!is.null(input$variablex2)){
        as.data.frame(svm1())
      } else {
        print(data.frame(Warning = "Please select Model Parameters")) 
      }})
  })
  
})

