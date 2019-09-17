library(shiny)
library(babynames)
library(tidyverse)
library(researchr)


accesslex_colors <- c(`main blue` = "#002b49",
                      `main light blue` = "#006072",
                      `second yellow` = "#d18a00",
                      `second orange` = "#d35e13",
                      `second red` = "#8f1336",
                      `second maroon` = "#632d4f",
                      `third green` = "#556221",
                      `third blue gray` = "#4b4f54",
                      `third light blue` = "#7e9bc0")


al_colors <- function(...) {
   colors <- c(...)
   
   if(is.null(colors))
      return(accesslex_colors)
   
   accesslex_colors[colors]
}


theme_researcher <- function(base_size = 10, font = NA) {
   txt <- element_text(size = base_size + 2)
   
   theme_classic(base_size = base_size, base_family = font) +
      theme(
         text = txt,
         plot.title = element_text(color = al_colors("main blue"), face = "bold"),
         plot.subtitle = element_text(face = "italic", color = al_colors("main light blue")),
         axis.title = txt,
         axis.text = txt,
         legend.title = txt,
         legend.text = txt,
         
         axis.line.y = element_blank(),
         axis.line.x = element_blank(),
         panel.grid.major.y = element_line(color = "gray70", size = 0.1),
         panel.grid.minor.y = element_line(color = "gray95", size = 0.1),
         axis.ticks.y = element_blank(),
         axis.ticks.x = element_blank(),
         
         legend.position = "bottom",
         
         plot.caption = element_text(hjust = 0)
      )
}


office_names <- c("Tanya", "Andrew", "Fletcher", "Rachel", "Sara",
           "Nancy", "Richard", "Aaron", "Tiffane", "Apryl",
           "Ana", "Rob", "Ray", "Aisha", "Anna", "Lauren",
           "Matthew", "Isaac")

data(babynames)

babynames2 <- babynames %>%
   mutate(name = name) %>%
   filter(name %in% office_names) %>%
   mutate(sex = ifelse(sex == "F", "Female", "Male"))



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Baby Names"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       selectInput(inputId = "name",
                 label = "Select Name:",
                 choices = office_names,
                 selected = "Richard",
                 selectize = TRUE,
                 multiple = TRUE),
       helpText("It is recommended to choose individuals of the same gender (You will have trouble finding very many",
                "women named Richard)."),
       radioButtons(inputId = "sex",
                  label = "Select Gender",
                  choices = c("Male", "Female"),
                  selected = "Male"),
       sliderInput(inputId = "cutoff",
                   label = "Would you like to filter Birth Year?",
                   min = 1880,
                   max = 2017,
                   value = c(1880, 2017),
                   sep = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput(outputId = "linePlot")
      )
   )
)



server <- function(input, output) ({
   
   output$linePlot <- renderPlot({
      
      
      req(input$name)
      
      babynames2 %>%
         filter(name == input$name,
                sex == input$sex,
                year > input$cutoff[1],
                year < input$cutoff[2]) %>%
         ggplot(aes(x = year, y = n, color = name, group = name)) +
         geom_line(lwd = 1.5) +
         theme_researcher() +
         researchr::scale_color_al() +
         labs(x = "Birth Year", y = "Number of Social Security Cards Issued",
              color = "Name:")
      
      
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

