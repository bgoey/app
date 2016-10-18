
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
libraryload = function(){
  library(shiny)
  library(png)
  library(ggplot2)
  library(e1071)
}
libraryload()

#read data
data1 = read.csv("c:/Users/goeyd/r projects/mt simulation/dataset.csv", sep = " ")

data1$flight.risk = as.factor(data1$flight.risk)
data1$gender = as.factor(data1$gender)

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




# ui ----------------------------------------------------------------------

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$style(HTML("
                  .image{
                  border-radius:20px;
                  width:80px;
                  height:60px;
                  }
                  .td{
                  padding:10px;
                  }
                  #svm{
                  color:black;
                  width:50%;
                  height:50%;
                  }
                  .Title{
                  background-color:orange;
                  color: white;
                  border-radius:15px;
                  }
                  .col-sm-8 nav ul li a{
                  background-color: BLUE;
                  }
                  .navbar .nav li[class=active] a{
                  background-color:blue;
                  color:white;
                  }
                  .navbar{
                  background-color:white
                  border-radius:10px;
                  }
                  .crazy{
                  outline: thick solid black;
                  }
                  
                  "
                  
  )),
  
  mainPanel(h2(class = "Title"), "HR Dashboard Offsite"),
            
            navbarPage(
              # introduction to the dashboard 
              tabPanel("intro"),
              tabPanel("Introduction to the dashboard",class = "crazy",
                       p("This document contains the description of the data file (ING Exercise). This document is ordered as follows; Presented first, is a list of the variables within the given data set. Second, Are the definition of the various variables. Third and last, the ranges and scales are given for the variables with required a survey. "),
                       h3("list:"),
                       p("In the table below you will find the variables contained inside the data side, the defintions, the measurement and example questions."),
                       tags$table(class = "table1",
                         tags$tr(
                           tags$td(class ="td" , strong("Variable name")),
                           tags$td(class ="td", strong(" Definitions  ")),
                           tags$td(class ="td", strong(" Measurement ")),
                           tags$td(class ="td", strong(" Example questions "))
                         ),
                         tags$tr(
                           tags$td(class ="td", "Names"),
                           tags$td(class ="td", "word or a combination of words by which a person, place, or thing, a body or class, or any object of thought is designated, called, or known"),
                           tags$td(class ="td", "As open ended question")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Gender"),
                           tags$td(class ="td", "either male or female"),
                           tags$td(class ="td", "Multiple choice(Female or Male")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Age"),
                           tags$td(class ="td", "A period of human life measure by years"),
                           tags$td(class ="td", "Open ended question")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Length of service"),
                           tags$td(class ="td", "The length of holding a job"),
                           tags$td(class ="td", "Open ended question")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Countries"),
                           tags$td(class ="td", "A nation"),
                           tags$td(class ="td", "Open ended question")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Business Line"),
                           tags$td(class ="td", "A particular line of products /  services"),
                           tags$td(class ="td", "Open ended question")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Agreeableness"),
                           tags$td(class ="td", "Refers to the quality of the interaction along a continuum from compassion and cooperation to antagonism and suspicion(costa, McCrea & Dye, 1991)."),
                           tags$td(class ="td", "Multiple choice (10 point scale. Ranging from 1 strongly disagree to 10 strongly agree"),
                           tags$td(class = "td", "I see myself as sympathetic, warm")
                         ),
                         tags$tr(
                           tags$td(class ="td", "conscientiousness"),
                           tags$td(class ="td", "Refers to having both a proactive and inhibitive aspects. The proactive side is conceptualized as the need for achievement and commitment to work. The inhibitive side consists of dimensions such as order, dutifulness and self-discipline (costa, McCrea & Dye, 1991).  "),
                           tags$td(class ="td", "Multiple choice (10 point scale. Ranging from 1 strongly disagree to 10 strongly agree"),
                           tags$td(class = "td", "I see myself as disorganized, careless")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Openness to experiences"),
                           tags$td(class ="td", "Refers to an individual's willingness to explore, tolerate and consider unfamiliar ideas and experiences. Characterized by  (McCrea & Costa, 1987)."),
                           tags$td(class ="td", "Multiple choice (10 point scale. Ranging from 1 strongly disagree to 10 strongly agree"),
                           tags$td(class = "td", "I like to experience new things")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Extraverted"),
                           tags$td(class ="td", "Refers to the extend individuals are sociable, gregarious, assertive, talkative and active.(Barrick & Mount, 1991)."),
                           tags$td(class ="td", "Multiple choice (10 point scale. Ranging from 1 strongly disagree to 10 strongly agree"),
                           tags$td(class = "td", "I see myself as extraverted, enthusiastic")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Stable"),
                           tags$td(class ="td", "Refers to the extend individuals are anxious, depressed, angry, embarrassed, emotional, worried and insecure (Barrick & Mount, 1991)."),
                           tags$td(class ="td", "Multiple choice (10 point scale. Ranging from 1 strongly disagree to 10 strongly agree"),
                           tags$td(class = "td", "I see myself as calm, emotionally stable")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Leadership"),
                           tags$td(class ="td", "(grant) Transformational leadership is typically conceptualized as a collection of four	dimensions of leader behaviour; inspirational motivation, idealized influence, intellectual stimulation and individual consideration. Inspirational motivation	involves articulating a compelling vision of the future. Idealized influence involves		engaging in charismatic actions that earn respect and cultivate pride. Intellectual	stimulation involves challenging followers to question their assumptions and think	differently (2012). "),
                           tags$td(class ="td", "Multiple choice (5 point likert scale; 1 strongly disagree, 5 strongly agree"),
                           tags$td(class = "td", "My direct supervisor has my respect")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Service Climate"),
                           tags$td(class ="td", "refers to employee perceptions of the practices, procedures, and behaviours that get rewarded, supported, and expected with regard to customer service and	customer service quality (Schneider et al., 1998)."),
                           tags$td(class ="td", "Multiple choice (5point likert scale; 1 strongly disagree, 5 strongly agree)"),
                           tags$td(class = "td", "How would you rate the job knowledge and skills of employees in your	business to deliver superior quality work and service")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Long term incentive(LTI)"),
                           tags$td(class ="td", "Long term incentives; a reward system designed to improve employees' long term performance by providing rewards that may not be tied to the company's share price. "),
                           tags$td(class ="td", "Factual data from compensation and benefits")
                           
                         ),
                         tags$tr(
                           tags$td(class ="td", "Salary"),
                           tags$td(class ="td", " A fixed compensation periodically paid to a person for work or services."),
                           tags$td(class ="td", "Factual data from compensation and benefits")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Burnout"),
                           tags$td(class ="td", "Burnout can be defined as a psychological syndrome in response to chronic interpersonal stressors on the job. It is characterized by emotional exhaustion, depersonalization (also called cynicism) and low personal accomplishment (also called low self-efficacy) (Maslach et al. 2001). Emotional exhaustion can be understood as the depletion of an individual's emotional resources and a feeling of overextension. Depersonalization refers to a negative or apathetic response towards all aspects of work. Low personal accomplishment is defined as a feeling of incompetence or a lack of self-efficacy at work."),
                           tags$td(class ="td", "Multiple choice(5 point likert scale; 1 strongly disagree, 5 strongly agree)"),
                           tags$td(class = "td", "I can tolerate the pressure of my work well.")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Sustainable Engagement"),
                           tags$td(class ="td", "Sustainable engagement is the willingness, ability and having the energy to go above and beyond the requirements of the job. "),
                           tags$td(class ="td", "Multiple choice (5 point likert scale; 1 strongly disagree, 5 strongly agree)"),
                           tags$td(class = "td", "I am willing to work beyond what is required to help my company succeed")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Flight Risk"),
                           tags$td(class ="td", "The intention or possibility of an individual to leave the organization"),
                           tags$td(class ="td", "Multiple choice(yes, no)"),
                           tags$td(class = "td", "Do you intend to leave ING in the near future")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Innovative behaviour"),
                           tags$td(class ="td", "An employee's intentional introduction or application of new ideas, products, processes, and procedures to his or her work role, work unit, or organization.	Furthermore, innovative behaviour extends to generation of new ideas and the realization or implementation of new ideas. "),
                           tags$td(class ="td", "Multiple choice (5 point likert scale; 1 strongly disagree, 5 strongly agree)"),
                           tags$td(class = "td", "Searches out new technologies, processes, techniques, and/or product ideas")
                         ),
                         tags$tr(
                           tags$td(class ="td", "Performance score"),
                           tags$td(class ="td", "The accumulative score of an employees' performance over the year."),
                           tags$td(class ="td", "Multiple choice (5 point scale; 1 weak performance, 5 strong performance)")
                         )
                       )
              ),
              # dataview 
              tabPanel("Dataview",
                       tableOutput("dataview")
              ),
              # summary 
              tabPanel("Summary",
                       tableOutput("summary")),
              # plot  
              tabPanel("Plot",
                       fluidRow(
                         column(12,sidebarPanel(
                         selectInput(inputId = "variablex", label = "Variable", choices = names(data1)[-1], selected = names(data1)[4]),
                         selectInput(inputId = "variabley", label = "Variable", choices = names(data1)[-1], selected = names(data1)[3]) 
                       )),column(6,
                       plotOutput("dotplot")))),
              # gender plot 
              tabPanel("Gender", fluidRow(
                
                sidebarPanel(
                  selectInput(inputId = "gender",
                              label = "male or female", c("male", "female", "all"), selected  = unique(data1$gender)[1]),
                  selectInput(inputId = "variable", label = "Variable", choices = names(data1)[-c(1,2)]))
                , 
                plotOutput("genplot"))),
              # introduction to the dashboard
              tabPanel("Regression",
                       sidebarPanel(
                         selectInput("variable_y", "Dependent Variable:", names(data1)[-1], selected = "none"),
                         
                         checkboxGroupInput("variable_x", "Independent Variables:",names(data1)[-1]),
                         actionButton("Updatebutton1", "Update")
                       ),
                       mainPanel(tableOutput("regTab")
                       )),
              tabPanel("SVM",
                       sidebarPanel(
                         selectInput("variabley2", "Dependent Variable:", names(data1)[c(2,5,17)]),
                         
                         checkboxGroupInput("variablex2", "Independent Variables:",names(data1)[-1]),
                         actionButton("Updatebutton2", "Update")
                       ),
                       mainPanel(tableOutput("svm")
                       )
              )
            )
  )
) 
 







