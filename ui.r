options(warn = -1)

shinyUI(fluidPage(
  titlePanel(title = h1(
    "Housing Loan Comparison Dashboard", align = "center"
  )),
  sidebarLayout(
    sidebarPanel(
      h4("Selection Panel", align = "center"),
      selectInput(
        "Year",
        "Years",
        unique(as.character(combined$As_of_Year)),    # setting range of values
        selected = unique(as.numeric(as.character(    # setting default values
          combined$As_of_Year
        ))),
        multiple = TRUE                               # setting multiple selection as true
      ),
      selectInput(
        "State",
        "State(s)",
        unique(combined$State),
        selected = unique(combined$State),
        multiple = TRUE
      ),
      selectInput(
        "respondent",
        "Respondent Name",
        unique(combined$Respondent_Name_TS),
        selected = "THE NATIONAL CAPITAL BANK",
        multiple = FALSE
      ),
      h5(
        "Note: If there is no Respondent for your combination of years and States, no graphs are returned",
        align = "center"
      )
      
    ),
    mainPanel(
      plotlyOutput(                                   # plotlyoutput for interactive graphs
        "barplot",                                    # We can also use ggplots which are faster comparitively
        width = "100%",
        height = 400,
        inline = T
      ),
      plotlyOutput(
        "bar2",
        width = "20%",
        height = 400,
        inline = T
      ),
      plotlyOutput(
        "bar3",
        width = "20%",
        height = 400,
        inline = T
      ),
      plotlyOutput(
        "bar4",
        width = "20%",
        height = 400,
        inline = T
      ),
      plotlyOutput(
        "bar5",
        width = "20%",
        height = 400,
        inline = T
      )
    )
  )
))
