options(warn = -1)
packages = c("scales", "readr", "plotly", "editrules", "stringr", "jsonlite",
             "devtools", "tidyr", "data.table", "rjson", "scales", "dplyr", "gridExtra", "shiny", "ggplot2")
x = sapply(packages, function(x) if (!require(x, character.only = T)) install.packages(x))
rm(x, packages)




shinyServer(function(input, output) {

  
  output$barplot <- renderPlotly({              # First barplot with different possible input levels
    a1 <-                                       # i.e. State, Year, respondent name etc
      combined %>% filter(
        As_of_Year %in% input$Year,
        State %in% input$State,
        Respondent_Name_TS %in% input$respondent
      )
    df1 <-
      aggregate(                                # Aggregating Loan Amount by year, state, respondent name 
        Loan_Amount_000 ~ As_of_Year + State + Respondent_Name_TS,
        data = a1,
        FUN = sum
      )
    gg1 <-                                      # Plotting the ggplot graph with year on x, Loan Amount on Y and states as facets
      ggplot(df1, aes(As_of_Year, Loan_Amount_000)) + geom_bar(stat = "identity") + xlab("Year") + ylab("Total Loan Amount") +
      facet_grid(. ~ State) + scale_y_continuous(labels = comma)
    ggplotly(gg1)                               # Converting ggplot into interactive plotly graph using ggplotly()
  })
  
  output$bar2 <- renderPlotly({
    a2 <-
      combined %>% filter(
        As_of_Year %in% input$Year,
        State %in% input$State,
        Respondent_Name_TS %in% input$respondent
      )
    df2 <-
      aggregate(Loan_Amount_000 ~ Loan_Purpose_Description,
                data = a2,
                FUN = sum)
    gg2 <-
      ggplot(df2, aes(Loan_Purpose_Description, Loan_Amount_000)) + geom_bar(stat = "identity") + xlab("Loan Purpose") +
      ylab("") +  scale_y_continuous(labels = comma)+theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
    ggplotly(gg2)
  })
  
  output$bar3 <- renderPlotly({
    a3 <-
      combined %>% filter(
        As_of_Year %in% input$Year,
        State %in% input$State,
        Respondent_Name_TS %in% input$respondent
      )
    df3 <-
      aggregate(Loan_Amount_000 ~ Lien_Status_Description,
                data = a3,
                FUN = sum)
    gg3 <-
      ggplot(df3, aes(Lien_Status_Description, Loan_Amount_000)) + geom_bar(stat = "identity") + xlab("Lien Status") +
      ylab("") + scale_y_continuous(labels = comma)+ theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
    ggplotly(gg3)
  })
  
  output$bar4 <- renderPlotly({
    a4 <-
      combined %>% filter(
        As_of_Year %in% input$Year,
        State %in% input$State,
        Respondent_Name_TS %in% input$respondent
      )
    df4 <-
      aggregate(Loan_Amount_000 ~ Conventional_Status,
                data = a4,
                FUN = sum)
    gg4 <-
      ggplot(df4, aes(Conventional_Status, Loan_Amount_000)) + geom_bar(stat = "identity") + xlab("Conventional Status") +
      ylab("")  + scale_y_continuous(labels = comma)+theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
    ggplotly(gg4)
  })
  
  output$bar5 <- renderPlotly({
    a5 <-
      combined %>% filter(
        As_of_Year %in% input$Year,
        State %in% input$State,
        Respondent_Name_TS %in% input$respondent
      )
    df5 <-
      aggregate(Loan_Amount_000 ~ Conforming_Status,
                data = a5,
                FUN = sum)
    gg5 <-
      ggplot(df5, aes(Conforming_Status, Loan_Amount_000)) + geom_bar(stat = "identity") + xlab("Conforming Status") +
      ylab("")  + scale_y_continuous(labels = comma)+ theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
    ggplotly(gg5)
  })
  
})
