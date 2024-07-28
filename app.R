library(dplyr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(stringr, quietly = TRUE)
library(ggplot2)

np2014 <- read.csv("np2014_d1.csv")
np2017 <- read.csv("np2017_d1.csv")
np2023 <- read.csv("np2023_d1_mid.csv")
np2023_hi <- read.csv("np2023_d1_hi.csv")
np2023_low <- read.csv("np2023_d1_low.csv")
np2023_zero <- read.csv("np2023_d1_zero.csv")

#' ggplot theme for with a CBO style
#' 
#' ggplot theme for with a CBO style that sets title flush left, ticks on inside, 
#' etc.
#' 
#' @param family the font family to usel "sans" by default.
#' @param base_font_size the base font size -- other are set relative to 
#'   the base size; 11 by default
#' @examples
#' ggplot(economics, aes(x = date, y = unemploy)) + geom_line() + labs(title = "Unemployment") + theme_cbo()
#' 

theme_cbo <- function(family = "sans", base_font_size = 11, ...) {
  
  pt2mm <- .35
  
  theme_classic(base_family = family, base_size = base_font_size) +
    
    theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      panel.grid.major.y = element_line(color = "gray90"),
      # text = element_text(family = family, size = base_size),
      title = element_text(size = rel(1.3)),
      
      plot.title = element_text(family = family,
                                margin = margin(t = 0, r = 0, b = 12, l = 0, unit = "pt"),
                                size = rel(1.2), face = "plain", hjust = 0),
      plot.subtitle = element_text(# family = family,
        margin = margin(t = 0, r = 0, b = 12, l = 0, unit = "pt"),
        size = rel(1.1), 
        face = "plain", hjust = 0),
      plot.caption = element_text(family = family, size = 8, hjust = 0),
      axis.title.y = element_blank(),
      # axis.title.x = element_text(family = family, size = 9),
      axis.text.y = element_text(# family = family,
        size = rel(1.0), 
        face = "plain", margin = margin(t = 0 , r = 10, b = 0, l = 0, unit = "pt"),
        color = "#4d4d4d"),
      axis.text.x = element_text(# family = family,
        size = rel(1.0), 
        face = "plain", margin = margin(t = 10 , r = 0, b = 0, l = 0, unit = "pt"),
        # gray color
        color = "#4d4d4d"),
      # .5 pt axes & ticks
      axis.line = element_line(linewidth = .5*pt2mm, color = "black"),
      axis.ticks = element_line(linewidth = .5*pt2mm, color = "black"),
      # draw ticks on the inside (negaive unit)
      axis.ticks.length = unit(-3, "pt"),
      # push x tick labels down & y tick labels left off of the inside of tick
      ...
    )
}



# The key for RACE is as follows:
# 0 = All races (codes 1 through 6)
# 1 = White alone
# 2 = Black alone
# 3 = AIAN alone
# 4 = Asian alone
# 5 = NHPI alone
# 6 = Two or More Races
# 7 = White alone or in combination
# 8 = Black alone or in combination
# 9 = AIAN alone or in combination
# 10 = Asian alone or in combination
# 11 = NHPI alone or in combination


make_long <- function(data, proj) {
  data |>
  rename_with(tolower) |> 
  filter(race == 0 | race == 1 | race == 8) |> # all race, white only, black in combo
  filter(origin != 0) |> # not Hispanic, Hispanic
  filter(sex != 0) |>  # male, female
  select(-total_pop) |> 
  pivot_longer(cols = c(pop_0:pop_100), names_to = "age", values_to = "pop", names_prefix = "pop_") |>
  mutate(age = as.numeric(age)) |>
  mutate(raceeth = 10 * origin + race) |> # 10, 11, 18 are not Hispanic; 20, 21, 28 are Hispanic
  select(-origin, -race) |>
  pivot_wider(id_cols = c(year, sex, age), names_from = raceeth, values_from = pop) |>
  rename(Hispanic = `20`) |> 
  rename(White = `11`) |>
  rename(Black = `18`) |>
  mutate(`Asian and Other` = `10` - White - Black) |>
  select(-`10`, -`21`, -`28`) |>
  pivot_longer(cols = c("White", "Black", "Hispanic", "Asian and Other"), names_to = "race", values_to = "pop") |> 
  mutate(proj = proj) 
}

pop2023 <- make_long(np2023, "2023")
pop2023_hi <- make_long(np2023, "2023-hi")
pop2023_low <- make_long(np2023, "2023-low")
pop2023_zero <- make_long(np2023, "2023-zero")
pop2017 <- make_long(np2017, "2017")
pop2014 <- make_long(np2014, "2014")
pop <- bind_rows(pop2023, pop2023_hi, pop2023_low, pop2023_zero, pop2017, pop2014)

make_shares <- function(data) {
  data |> 
    group_by(proj, year, race) |> 
    summarize(pop_sr = sum(pop), .groups = "drop") |> # collapse age
    group_by(proj, year) |> 
    mutate(share = pop_sr / sum(pop_sr)) 
}

if(FALSE) {
  input <- list(proj = list("2023"),
                age = list(0, 100),
                years = list(2022, 2100))
  shares <- pop |> 
      filter(proj %in% input$proj,
             between(age, input$age[[1]], input$age[[2]]),
             between(year, input$years[[1]], input$years[[2]])) |> 
      make_shares()
  
  summary(shares)
  summary(pop2023)
  ggplot(shares, aes(x = year, y = share, color = race)) + geom_line()
  View(np2023)
}

ui <- fluidPage(
    
    titlePanel("Race-Origin Shares in the Census Population Projections"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("age",
                    "Choose age range to display:",
                    min = 0, max = 100, value = c(0, 100)
        ),
        sliderInput("years",
                    "Choose year range to display:",
                    min = 2016, max = 2060, value = c(2021, 2052), sep = ""
        ),
        checkboxGroupInput("proj",
                      "Choose projections to compare:",
                      choices = c("2023", "2023-hi", "2023-low", "2023-zero", "2017", "2014"),
                      selected = c("2023")
        )
      ),
      
      mainPanel(
        plotOutput("plot")
        
      )
    )
  )
  
server <- function(input, output) {
  
  shares <- reactive({
    pop |> 
    filter(proj %in% input$proj,
           between(age, input$age[[1]], input$age[[2]]),
           between(year, input$years[[1]], input$years[[2]])) |> 
    make_shares()
  })
  
  output$plot <- renderPlot({
    
    ggplot(data = shares(), aes(x = year, y = share, color = race, linetype = proj)) +
      geom_line() +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "Year", y = NULL,
           color = "Race-Origin", 
           linetype = "Projection year",
           title = "Share",
           caption = "Source: https://www.census.gov/programs-surveys/popproj/data/datasets.html") +
      theme_cbo(base_font_size = 16) +
      theme(plot.caption = element_text(size = 16))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)





  