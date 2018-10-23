options(warn = -1)
require(plotly)
require(data.table)
require(dplyr)

##### Function for loading & transforming data (Please change the file paths to load your data) #####
hmda_init <-
  function(instifile = "2012_to_2014_institutions_data.csv",
           loanfile = "2012_to_2014_loans_data.csv") {
    
    
    institutions <- fread(instifile, stringsAsFactors = FALSE) 
    loans <- fread(loanfile, stringsAsFactors = FALSE)# Using fread() for reading the data fastly
    
    # Merging loans data with Respondent_Name based on As_of_year + Agency_Code + Respondent_ID
    combined <-
      merge(loans,
            institutions[, c("As_of_Year", "Agency_Code", "Respondent_ID",                           "Respondent_Name_TS")],
            by = c("As_of_Year", "Agency_Code", "Respondent_ID")) %>%
      mutate(Loan_Bucket = cut(Loan_Amount_000,       #appending a Loan_Bucket column to the dataframe
                               breaks = c(0, 50, 350, 2000, 5000, 15000, 40000, 100000),
                               labels = 1:7),
             right = FALSE
      ) %>% as.tbl                                    #tbl prevents printing the whole dataframe by mistake
    
    # transforming the columns, correcting the classes of some columns
    suppressWarnings(
      combined <<-                                    #combined dataframe here is saved in the global environment
        combined %>% mutate(
          Applicant_Income_000 = as.numeric(Applicant_Income_000),
          As_of_Year = as.factor(As_of_Year),
          FFIEC_Median_Family_Income = as.numeric(FFIEC_Median_Family_Income) / 1000,
          Loan_Amount_000 = as.numeric(Loan_Amount_000),
          Number_of_Owner_Occupied_Units = as.numeric(Number_of_Owner_Occupied_Units),
          Tract_to_MSA_MD_Income_Pct = as.numeric(Tract_to_MSA_MD_Income_Pct),
          Conventional_Conforming_Flag = factor(Conventional_Conforming_Flag),
          Loan_Purpose_Description = factor(Loan_Purpose_Description),
          Agency_Code_Description = factor(Agency_Code_Description),
          Lien_Status_Description = factor(Lien_Status_Description),
          Loan_Type_Description = factor(Loan_Type_Description),
          Conventional_Status = factor(Conventional_Status),
          Conforming_Status = factor(Conforming_Status)
        )
    )
  }
hmda_init()
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
