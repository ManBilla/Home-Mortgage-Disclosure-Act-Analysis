options(warn = -1)

##### Loading all the required packages #####
packages = c("readr", "plotly", "editrules", "stringr", "jsonlite",
             "devtools", "tidyr", "data.table", "rjson", "scales", "dplyr", "gridExtra", "shiny")
x = sapply(packages, function(x) if (!require(x, character.only = T)) install.packages(x))
rm(x, packages)



##### Function for loading & transforming data (Please change the file paths to load your data) #####
hmda_init <-
  function(instifile = "C:/Users/ManojKumarBilla/Documents/GitHub/Home-Mortgage-Disclosure-Act-Analysis/2012_to_2014_institutions_data.csv",
           loanfile = "C:/Users/ManojKumarBilla/Documents/GitHub/Home-Mortgage-Disclosure-Act-Analysis/2012_to_2014_loans_data.csv") {
    
                                                      
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


hmda_init()                                           # Call the function to load and transform data


str(combined)                                         # structure of combined dataset


##### Density plot of data using ggplot2 showing the Loan Buckets #####
ggplot(combined, aes(x = Loan_Amount_000)) +
  stat_density(aes(y = ..count..), color = "black", fill = 'blue', alpha = 0.3) +
  geom_vline(xintercept = c(0, 50, 350, 2000, 5000, 15000, 40000, 100000),color = "Magenta") +
  scale_x_continuous(breaks = c(0, 50, 350, 2000, 5000, 15000, 40000, 100000), trans = "log1p", labels = comma) +
  xlab("Loan Amount 000 (In Log Scale)") +
  ylab("Number of Loans")


##### Table showing the details about my Loan Buckets #####
LoanBucketRange <-
  select(combined, Loan_Amount_000, Loan_Bucket) %>%   # Selecting the Loan_Bucket & Loan Amount columns
  arrange(Loan_Bucket) %>% group_by(Loan_Bucket) %>%   # Arranging by Loan_Bucket and grouping by Loan_Bucket
  summarise(                                           # dplyr Function to append Sum, Mean, Count columns
    Loan_Amount_From = min(Loan_Amount_000),
    Loan_Amount_to = max(Loan_Amount_000),
    Mean_Loan_Amount = mean(Loan_Amount_000),
    Number_of_Loans = n(),
    Total_Loan_Amount = sum(Loan_Amount_000)
    ) %>%                                              # using paste0() function to append pecent of loan amount & loan count
  mutate(No.of_Loans_Percent = paste0(round(100 * Number_of_Loans / sum(Number_of_Loans), 2), " %")) %>%
  mutate(Total_Loan_Percent = paste0(round(100 * Total_Loan_Amount / sum(Total_Loan_Amount), 2), " %"))

View(LoanBucketRange)


##### Creating a data export function to export the filtered dataframe to json #####

states = unique(combined$State)
Lien_Status_Description = "First Lien"

hmda_to_json <-
  function(df = combined,                              # combined is used as default. Change if you want for other dataframe
           states = NULL,
           conventional_conforming = NULL) {
    if (is.null(states) | is.null(conventional_conforming)) {
      if (is.null(states) & is.null(conventional_conforming)) {
        states = unique(df$State)
        conventional_conforming = unique(df$Conventional_Conforming_Flag)
      } else if (is.null(states)) {
        states = unique(df$State)
        conventional_conforming = conventional_conforming
      } else if (is.null(conventional_conforming)) {
        states = states
        conventional_conforming = unique(df$Conventional_Conforming_Flag)
      }
      
    } else {
      states = states
      conventional_conforming = conventional_conforming
    }
    
    
    filtered <-                                        # Using dplyr's pipe operator to filter the data
      df %>% filter(State %in% states &
                      Conventional_Conforming_Flag %in% conventional_conforming)
    
    
    JSONexport <- toJSON(filtered)                     # Using toJSON function to convert dataframe to json
    write(JSONexport, "HMDAnew.json")                  # Writing the json file to disk using write function
  }


hmda_to_json(states = c("DC", "MD"), conventional_conforming = "Y")  # Calling the function 


##### Data Quality Assessment #####

str(combined$Loan_Amount_000)                          # Checking the type & number of values
summary(combined$Loan_Amount_000)                      # Five point summary of the Loan Amount column
sd(combined$Loan_Amount_000)                           # Standard Deviation
sum(is.na(combined$Loan_Amount_000))                   # Checking for missing values


ggplot(combined, aes(x = Loan_Amount_000)) +           # density plot without any Log transformation
  stat_density(aes(y = ..count..), color = "black", fill = 'blue',alpha = 0.3) +
  scale_x_continuous(labels = comma) + xlab("Loan Amount 000") + ylab("Number of Loans")

                                                       # dev.off() run this in console if you face issues with ggplot


                                                       # Outliers by Agency COde and by State             
ggplot(combined, aes(as.factor(Agency_Code_Description), log(Loan_Amount_000))) +
  geom_boxplot(outlier.color = "red") + xlab("Agency") + ylab("Loan Amount (Log)")
ggplot(combined, aes(as.factor(State), log(Loan_Amount_000))) + 
  geom_boxplot(outlier.color = "red") + xlab("State") + ylab("Loan Amount (Log)")
                                          

                                                       # Using outlierKD script from datascienceplus.com to find the statistics without and with outliers:
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var), eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  boxplot(var_name, main = "With outliers")
  hist(var_name,
       main = "With outliers",
       xlab = NA,
       ylab = NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main = "Without outliers")
  hist(var_name,
       main = "Without outliers",
       xlab = NA,
       ylab = NA)
  title("Outlier Check", outer = TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot * 100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <-
    readline(prompt = "Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if (response == "y" | response == "yes") {
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

outlierKD(combined, Loan_Amount_000)




p <-
  ggplot(combined, aes(x = combined$Loan_Amount_000, y = combined$Loan_Amount_000)) +
  geom_boxplot()

boxplot(combined$Loan_Amount_000)

p <- ggplot(combined, aes(Loan_Bucket, "Loan"))
p + geom_point()
pp <- ggplot(combined, aes(Loan_Amount_000, Loan_Bucket))
pp + geom_jitter()

institutions<-read.csv("C:/Users/Billa/Google Drive/MSIS/Case Competitions/Capital One/data-challenge-data-master/2012_to_2014_institutions_data.csv")
# For respondent Name it is better to do the data quality assessment before merging the institutions dataset
library(stringr)
institutions$Respondent_Name_TSCopy <- institutions$Respondent_Name_TS  # Creating a copy of original column
institutions$Respondent_Name_TS <-                     # Removing spaces before and after the text
  str_trim(institutions$Respondent_Name_TS)
institutions$Respondent_Name_TS <-                     # Converting all characters to upper case
  toupper(institutions$Respondent_Name_TS)
sum(is.na(institutions$Respondent_Name_TS))            # Checking for missing values
n_occur <- data.frame(table(institutions$Respondent_Name_TS)) # Counting the duplicate values


##### Creating data quality rules #####
                                                       # creating rule that the Loan amount 
                                                       # should always be more than zero
library(editrules)
Rule <- editset("Loan_Amount_000 > 0")
z <- violatedEdits(Rule, combined)
as.character(length(z[z == TRUE]))                     # This gives the number of times 
                                                       # the above rule is violated

E <- editfile("RuleSet.txt")                           # Rules can also be loaded from a .txt file 
ve <- violatedEdits(E, combined)                       # and use summary() to find what % of 
summary(ve)                                            # errors are numeric, character, mixed etc


##### Visual Narrative #####

library(gridExtra)

# Three plots showing total loan, average loan and count of loans by Year

by_year <- group_by(combined, As_of_Year)
a <-                                                   # Appending the Sum, Average and Count to grouped column
  summarise(
    by_year,
    count = n(),
    Total_Loan = sum(Loan_Amount_000),
    Average_Loan = mean(Loan_Amount_000)
  )

p1 <-                                                  # Creating three plots Showing yearly variation of Total Loan Amount
  ggplot(a, aes(x = As_of_Year, y = Total_Loan)) + geom_bar(stat = "identity") +
  ggtitle("Total loan amount") + xlab("Year") + ylab("Total Loan Amount")
p2 <-
  ggplot(a, aes(x = As_of_Year, y = Average_Loan)) + geom_bar(stat = "identity") +
  ggtitle("Average loan amount") + xlab("Year") + ylab("Average Loan Amount")
p3 <-
  ggplot(a, aes(x = As_of_Year, y = count)) + geom_bar(stat = "identity") +
  ggtitle("Number of loans") + xlab("Year") + ylab("Number of Loans")
grid.arrange(p1, p2, p3, ncol = 3, top = "Loan Amount Yearwise comparision")


# Three Bar graphs showing Total Loan Amount by state & area of state
by_state <- group_by(combined, State)
a <-
  summarize(
    by_state,
    count = n(),
    Total_Loan = sum(Loan_Amount_000),
    Average_Loan = mean(Loan_Amount_000)
  )
p1 <-
  ggplot(a, aes(x = State, y = Total_Loan)) + geom_bar(stat = "identity") +
  ggtitle("Total loan amount") + xlab("State") + ylab("Total Loan Amount") + scale_y_continuous(labels = comma)
p2 <-
  ggplot(a, aes(x = State, y = Average_Loan)) + geom_bar(stat = "identity") +
  ggtitle("Average loan amount") + xlab("State") + ylab("Average Loan Amount") + scale_y_continuous(labels = comma)
p3 <-
  ggplot(a, aes(x = State, y = count)) + geom_bar(stat = "identity") + 
  ggtitle("Number of loans") + xlab("State") + ylab("Number of Loans") + scale_y_continuous(labels = comma)


State = c("DC", "DE", "MD", "VA", "WV")
areas = c(68.34, 2491, 12407, 42775, 24038)             # Appending external Area's data 
area = data.frame(State, areas)
p4 <-
  ggplot(area, aes(x = states, y = areas)) + geom_bar(stat = "identity") +
  ggtitle("Land Area") + xlab("State") + ylab("Total Land area (mi^2)")
grid.arrange(p1, p2, p3, p4, ncol = 4, top = "Loan Amount Statewise Comparision")

# from the above dataframe, let us do further analysis by dividing the total loan amount and number of loans by each state's area
ax<-cbind(a,area$areas)
ax1<-ax%>%mutate(countbyarea = count/`area$areas`, totalbyarea = Total_Loan/`area$areas`)

p1 <-
  ggplot(ax1, aes(x = State, y = totalbyarea)) + geom_bar(stat = "identity") +
  ggtitle("Total loan amount/Area") + xlab("State") + ylab("Loan Amount per unit area") + scale_y_continuous(labels = comma)
p2 <-
  ggplot(ax1, aes(x = State, y = Average_Loan)) + geom_bar(stat = "identity") +
  ggtitle("Average loan amount") + xlab("State") + ylab("Average Loan Amount") + scale_y_continuous(labels = comma)
p3 <-
  ggplot(ax1, aes(x = State, y = countbyarea)) + geom_bar(stat = "identity") + 
  ggtitle("Number of loans/Area") + xlab("State") + ylab("Loans per Unit Area") + scale_y_continuous(labels = comma)

grid.arrange(p1, p2, p3, ncol = 3, top = "Loan Amount Statewise Comparision per unit area")



# Statewise and yearwise plot of loan amount
by_stateandyear <- group_by(combined, As_of_Year, State)
a <-
  summarize(
    by_stateandyear,
    count = n(),
    Total_Loan = sum(Loan_Amount_000),
    Average_Loan = mean(Loan_Amount_000)
  )
p1 <-
  ggplot(a, aes(As_of_Year, Total_Loan)) + geom_bar(stat = "identity") + xlab("Year") + ylab("Total Loan Amount") +
  facet_grid(. ~ State) + scale_y_continuous(labels = comma)
p2 <-
  ggplot(a, aes(As_of_Year, Average_Loan)) + geom_bar(stat = "identity") + xlab("Year") + ylab("Average Loan Amount") +
  facet_grid(. ~ State) + scale_y_continuous(labels = comma)
p3 <-
  ggplot(a, aes(As_of_Year, count)) + geom_bar(stat = "identity") + xlab("Year") + ylab("Number of Loans") +
  facet_grid(. ~ State) + scale_y_continuous(labels = comma)
grid.arrange(p1, p2, p3, nrow = 3, top = "State and yearwise Comparision")




#further drilling down into Loan Type Description from Non-Conventional loans

qplot(
  log(Loan_Amount_000),
  data = combined,
  fill = Lien_Status_Description,
  xlab = "Loan Amount (Log Scale)",
  ylab = "Count"
)
by_conventional <-
  group_by(combined,
           As_of_Year,
           Loan_Type_Description,
           Lien_Status_Description)
a <-
  summarize(
    by_conventional,
    count = n(),
    Total_Loan = sum(Loan_Amount_000),
    Average_Loan = mean(Loan_Amount_000)
  )
g <- ggplot(a, aes(As_of_Year, Total_Loan)) + geom_bar(stat = "identity") + 
  facet_grid(. ~ Loan_Type_Description) + xlab("Year") + ylab("Loan Amount") + 
  scale_y_continuous(labels = comma)
g



# Stacked Barplot, stacked by the Loan_Bucket for DC only
by_statesyearsbuckets <-
  group_by(combined, As_of_Year, State, Loan_Bucket) %>% filter(State %in% c("DC"))
a <-
  summarize(
    by_statesyearsbuckets,
    count = n(),
    Total_Loan = sum(Loan_Amount_000),
    Average_Loan = mean(Loan_Amount_000)
  )
p1 <-
  ggplot(a, aes(As_of_Year, Total_Loan, fill = Loan_Bucket)) + geom_bar(stat ="identity") +
  xlab("Year") + ylab("Total Loan Amount") + facet_grid(. ~State) + 
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette ="Set1") + 
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
p2 <-
  ggplot(a, aes(As_of_Year, Average_Loan, fill = Loan_Bucket)) + geom_bar(stat ="identity") +
  xlab("Year") + ylab("Average Loan Amount") + facet_grid(. ~State) + 
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette ="Set1") +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
p3 <-
  ggplot(a, aes(As_of_Year, count, fill = Loan_Bucket)) + geom_bar(stat ="identity") +
  xlab("Year") + ylab("Number of Loans") + facet_grid(. ~ State) +
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ))
grid.arrange(p1, p2, p3, ncol = 3, top = "State and yearwise Comparision with stacked Loan_Buckets")



# Stacked Barplot, stacked by LoanBuckets and for 3 states
by_statesbuckets <- group_by(combined, State, Loan_Bucket)
a <-
  summarize(
    by_statesbuckets,
    count = n(),
    Total_Loan = sum(Loan_Amount_000),
    Average_Loan = mean(Loan_Amount_000)
  )
p1 <-
  ggplot(a, aes(State, Total_Loan, fill = Loan_Bucket)) + geom_bar(stat ="identity") +
  xlab("State") + ylab("Total Loan Amount") + scale_y_continuous(labels = comma) + 
  scale_fill_brewer(palette = "Set1")
p2 <-
  ggplot(a, aes(State, Average_Loan, fill = Loan_Bucket)) + geom_bar(stat ="identity") +
  xlab("State") + ylab("Average Loan Amount") + scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set1")
p3 <-
  ggplot(a, aes(State, count, fill = Loan_Bucket)) + geom_bar(stat = "identity") +
  xlab("State") + ylab("Number of Loans") + scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set1")
grid.arrange(p1, p2, p3, ncol = 3, top = "State and yearwise Comparision")

# Stacked Barplot for DC - 2, stacked by the Loan_Bucket for DC only
by_DC1 <-
  group_by(combined, As_of_Year, State, Loan_Purpose_Description) %>% filter(State %in% c("DC"))%>%summarise(count = n(), Total_Loan = sum(Loan_Amount_000))
by_DC2 <-
  group_by(combined, As_of_Year, State, Lien_Status_Description) %>% filter(State %in% c("DC"))%>%summarise(count = n(), Total_Loan = sum(Loan_Amount_000))
by_DC3 <-
  group_by(combined, As_of_Year, State, Loan_Type_Description) %>% filter(State %in% c("DC"))%>%summarise(count = n(), Total_Loan = sum(Loan_Amount_000))
by_DC4 <-
  group_by(combined, As_of_Year, State, Conventional_Status) %>% filter(State %in% c("DC"))%>%summarise(count = n(), Total_Loan = sum(Loan_Amount_000))
by_DC5 <-
  group_by(combined, As_of_Year, State, Conforming_Status) %>% filter(State %in% c("DC"))%>%summarise(count = n(), Total_Loan = sum(Loan_Amount_000))
p1 <-
  ggplot(by_DC1, aes(As_of_Year, Total_Loan, fill = Loan_Purpose_Description)) + geom_bar(stat ="identity") +
  xlab("Year") + ylab("Total Loan Amount") + facet_grid(. ~State) + 
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette ="Set1")
p2 <-
  ggplot(by_DC2, aes(As_of_Year, Total_Loan, fill = Lien_Status_Description)) + geom_bar(stat ="identity") +
  xlab("Year") + ylab("Total Loan Amount") + facet_grid(. ~State) + 
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette ="Set1")
p3 <-
  ggplot(by_DC3, aes(As_of_Year, Total_Loan, fill = Loan_Type_Description)) + geom_bar(stat ="identity") +
  xlab("Year") + ylab("Total Loan Amount") + facet_grid(. ~State) + 
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette ="Set1")
p4 <-
  ggplot(by_DC4, aes(As_of_Year, Total_Loan, fill = Conventional_Status)) + geom_bar(stat ="identity") +
  xlab("Year") + ylab("Total Loan Amount") + facet_grid(. ~State) + 
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette ="Set1")
p5 <-
  ggplot(by_DC5, aes(As_of_Year, Total_Loan, fill = Conforming_Status)) + geom_bar(stat ="identity") +
  xlab("Year") + ylab("Total Loan Amount") + facet_grid(. ~State) + 
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette ="Set1")
grid.arrange(p1, p2, p3, ncol = 3,nrow = 2, top = "State and yearwise Comparision with stacked Loan_Buckets")

p1
p2
p3
p4
p5
# Scatterplot for correlation between 
# Loan Amount and Applicant Income (Takes some time to plot)
ggplot(combined, aes(log(Loan_Amount_000), Applicant_Income_000)) +
  geom_jitter() + geom_smooth()