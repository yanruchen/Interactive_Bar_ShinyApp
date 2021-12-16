library(shiny)
library(ggplot2)
library(mathjaxr)

# REFERENCES:
# shiny reference https://shiny.rstudio.com/articles/tabsets.html
# text style reference https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
# reactive example https://community.rstudio.com/t/how-do-i-create-an-editable-table-to-allow-user-input-draw-scatterplot-and-fit-a-curve-through-those-points/83802
# reactive user select example https://community.rstudio.com/t/changing-datasets-based-on-select-input-in-r-shiny/67891/2
# renderUI example https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
# reactive change label name https://community.rstudio.com/t/reactive-axis-labels-in-shiny-with-ggplot-display-user-selected-label-not-variable-name/17560
# CSS overflow (fit table) https://www.w3schools.com/cssref/pr_pos_overflow.asp
# Add Latex using withMathJax https://shiny.rstudio.com/gallery/mathjax.html
# Example for withMathJax https://stackoverflow.com/questions/30169101/latex-formula-in-shiny-panel
# Include URL in shinyAPP example https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny
# Shiny HTML Tags https://shiny.rstudio.com/articles/tag-glossary.html




# Prepare dataset
example1 <- data.frame(Supplies=c("Desktop", "Laptop", "Notebook","Tablet"),
                       Frequency=c(10, 15, 25,12))
# Add a column for relative frequency
example1["Relative_Frequency"]<-example1$Frequency/sum(example1$Frequency)  
example2 <- data.frame(Supplies=c("Desktop", "Laptop", "Notebook","Tablet"),
                       Frequency=c(20, 10, 5, 15))
# Add a column for relative frequency
example2["Relative_Frequency"]<-example2$Frequency/sum(example2$Frequency)


ui <- fluidPage(

  titlePanel("Bar Graphs"),
  
  sidebarLayout(
    
    sidebarPanel(width = 4,
      htmlOutput("text_side"),  # Introduction paragraphs on the side panel
      
      HTML("<div style ='overflow:scroll;  ' >"),  # use CSS overflow property to fit table
      tableOutput("table1"),   # display datatable
      HTML("</div>"),
      
      selectInput(inputId = "select",label = "Try another dataset", 
                  choices = c("example1","example2"), selected = example1),

      checkboxInput(inputId = "check", label = "choose relative frequency", value = FALSE)
    ),
    
    mainPanel(
  
      tabsetPanel(type = "tabs",
                  tabPanel("Bar Graphs", plotOutput("plot1")),
                  tabPanel("Pareto Graph",htmlOutput("pareto"), plotOutput("plot2")),
                  tabPanel("Frequency vs Realtive Frequency", htmlOutput("text_ref"),
                           tableOutput("table2"), uiOutput("formula1"))
                  )   # use htmlOutput since there are multiple lines
      
    )
      
    )
)
  


server <- function(input, output) {
     # STEP1: DEFINE REACTIVE FUNCTION
     # Display dataset depends on user choice (example1 or example2)
     update_table <- reactive({
     if (input$select == "example1"){
      dataset <- example1
     }
     else if (input$select == "example2"){
      dataset <- example2
     }
     return(dataset)
    })
    
    color_example <- reactive({
      if (input$select == "example1"){
        color <- "blue"
      }
      else if (input$select == "example2"){
        color <- "orange"
      }
      return(color)
    })
     
    # Display frequency or relative frequency graph depends on user choice (check or not)
    chose_freq <- reactive({   
      if (input$check == TRUE){
        yaxis <- update_table()$Relative_Frequency
      }
      else if(input$check == FALSE){
        yaxis <- update_table()$Frequency
      }
      return(yaxis)
    })
    
    # Change y label name depends on user choice on relative frequency
    y_label <- reactive({
      #req(input$check)    # wait until user choose, otherwise not display
      if(input$check == TRUE){
        y_label <- "Relative Frequency"
      } else if(input$check == FALSE){
        y_label <- "Frequency"
      }})
    
    
   
    # STEP2: WRITE RENDER FUNCTIONS
    # Below are for SideBar
    output$text_side <- renderUI({
      str1 <- c("Bar Graph ")
      str2 <- c("(also called Bar Chart) is a graphical display of data using bars of different heights.")
      str3 <- c("The height of the bars are usually the frequency or relative frequency (see the last tab)")
      str4 <- c("Let's use the table below as an example:")
      HTML(paste('<b>',str1,'</b>',str2, '<br/>', str3,'<br/>','<br/>', str4))  # use HTML paste since there are multiple lines
    })
    
    output$table1 <- renderTable({update_table()   # remember ()!
    })
    
  
    
    # Below are for first tab
    output$plot1 <- renderPlot({
      ggplot(update_table(),aes(x=Supplies,y=chose_freq()))+   # update_table() is a function, needs "()"
        geom_bar(fill=color_example(),stat="identity")+   # reactive color, make it easier for user to distinguish dataset
        ylab(y_label())+   # rename y axis, depends on user choice (reactive)
        theme(axis.title.x=element_blank())  # hide name of x axis (use "text" to hide name of bars)
    })
    
    # Below are for second tab
    output$plot2 <- renderPlot({
      ggplot(update_table(),aes(x=reorder(Supplies,-chose_freq()),y=chose_freq()))+  # sort descending using "-"
        geom_bar(fill=color_example(),stat="identity")+  # reactive color, make it easier for user to distinguish dataset
        ylab(y_label())+   # rename y axis
        theme(axis.title.x=element_blank())  # hide name of x axis (use "text" to hide name of bars)
    })
    
    output$pareto <- renderUI({
      str1 <- c("Sometimes it's helpful to sort the bars according to frequency or relative frequency.")
      str2 <- c("If the graph is in descending order, it is called a")
      str3 <- c("Pareto graph.")
      str4 <-tags$a(href= "https://www.jmp.com/en_ch/statistics-knowledge-portal/exploratory-data-analysis/pareto-chart.html",
                    "More about Pareto Graph")  # link not working???
      HTML(paste('<br/>',str1, '<br/>', str2, '<b>',str3,'</b>', str4)) # use <b> </b> to bold face
    })
    
    # Below are for third tab
    output$text_ref <-renderUI({
      str01 <- c("Frequency")
      str1 <- c("is the count of each category.")
      str02 <- c("Relative Frequency")
      str2 <- c("is the count divide by the sum of all the counts.")
      str3 <- c("Let's use example1 to demostrate how to calculate relative frequency:")
      HTML(paste('<br/>','<b>', str01, '</b>',str1, '<b>', str02, '</b>',str2,'<br/>','<br/>', str3))   # use HTML paste since there are multiple lines
    })
    
    output$table2 <- renderTable({example1
    })
    
    output$formula1 <- renderUI({
      withMathJax(
        helpText("Relative frequency for Desktop: $$\\frac {10}{10+15+25+12} = 0.16$$ "),
        helpText("Relative frequency for Laptop: $$\\frac {15}{10+15+25+12} = 0.24$$"),
        helpText("Verify relative frequency for notebook and tablet by yourself!"))
    })
 

}


shinyApp(ui, server)