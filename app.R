library(babynames)
library(shiny)
library(tidyverse)

possible_names <- c("John", "Lisa", "Chantelle", "Kobi", "Jace",
                    "Jessica", "Jared", "James", "Calvin", "Richard",
                    "Julie", "Carter", "Madelyn", "Kyle", "Karley",
                    "Doug", "Tina", "Zach", "Ethan", "Kelli", "Jack",
                    "Hannah", "Amelia", "Craig", "Ali", "Eric", "Whitney")

ui <- fluidPage(
  paste("Comparing Baby Names: Only compare the same gender"),
  radioButtons(inputId = "gender",
               label = "Gender",
               choices = c("F", "M"),
               selected = "F"),
  selectInput(inputId = "name",
              label = "Name",
              selected = "Julie",
              choices = possible_names,
              multiple = TRUE),
  
  plotOutput(outputId = "line")
)


server <- function(input, output) {
  output$line <- renderPlot(
    babynames %>%
      filter(sex == input$gender,
             name == str_to_title(input$name)) %>%
      ggplot(aes(x = year, y = n, color = name)) +
      geom_line(size = 1) +
      labs(title = paste0("You Selected: ", input$name),
           y = "Number of People with that Name",
           x = "Year") +
      scale_x_continuous(breaks = c(1880, 1900, 1920, 1940, 1960, 1980, 2000),
                         labels = c(1880, 1900, 1920, 1940, 1960, 1980, 2000)) +
      theme_light()
  )
}




shinyApp(ui = ui, server = server)
