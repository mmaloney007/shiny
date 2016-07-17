library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Exposure Count" = "exp_count",
  "Total Insured Value" = "TIV",
  "Average Annual Loss" = "AAL",
  "CTE 1%" = "CTE_1",
  "1 in 100 PML" = "PML_100",
  "# of Households" = "households"
)


shinyUI(navbarPage("Exposure Explorer", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("ZIP control"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "households"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "EMT threshold (top n percentile)", 5)
        ),

        plotOutput("hist", height = 200),
        plotOutput("scatterTIVexp_count", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Global Reinsurance Strategy Group: Exposure Management Team'), ' by V. Michael Maloney (2016).'
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

# tabPanel(img(src="lm_img.jpg", height = 50, width = 50))

  conditionalPanel("false", icon("crosshair"))
))
