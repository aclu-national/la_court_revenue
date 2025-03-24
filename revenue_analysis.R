# Loading Libraries
library(tidyverse)
library(janitor)
library(rvest)
library(pdftools)
library(healthyR)

# Defining Judicial District Courts
judicial_district_courts <- c(
  "1st Judicial District Court",
  "2nd Judicial District Court",
  "3rd Judicial District Court",
  "4th Judicial District Court",
  "5th Judicial District Court",
  "6th Judicial District Court",
  "7th Judicial District Court",
  "8th Judicial District Court",
  "9th Judicial District Court",
  "10th Judicial District Court",
  "11th Judicial District Court",
  "12th Judicial District Court",
  "13th Judicial District Court",
  "14th Judicial District Court",
  "15th Judicial District Court",
  "16th Judicial District Court",
  "17th Judicial District Court",
  "18th Judicial District Court",
  "19th Judicial District Court",
  "20th Judicial District Court",
  "21st Judicial District Court",
  "22nd Judicial District Court",
  "23rd Judicial District Court",
  "24th Judicial District Court",
  "25th Judicial District Court",
  "26th Judicial District Court",
  "27th Judicial District Court",
  "28th Judicial District Court",
  "29th Judicial District Court",
  "30th Judicial District Court",
  "31st Judicial District Court",
  "32nd Judicial District Court",
  "33rd Judicial District Court",
  "34th Judicial District Court",
  "35th Judicial District Court",
  "36th Judicial District Court",
  "37th Judicial District Court",
  "38th Judicial District Court",
  "39th Judicial District Court",
  "40th Judicial District Court",
  "42nd Judicial District Court",
  "Caddo Parish Juvenile Court",
  "East Baton Rouge Parish Family Court",
  "East Baton Rouge Parish Juvenile Court",
  "Jefferson Parish Juvenile Court",
  "Orleans Parish Civil District Court and 1st and 2nd City Court",
  "Orleans Parish Criminal District Court",
  "Orleans Parish Juvenile Court"
)

# Defining City Courts
city_courts <- c(
  "Abbeville City Court",
  "Alexandria City Court",
  "Ascension Parish Court",
  "Baker City Court",
  "Bastrop City Court",
  "Baton Rouge City Court",
  "Bogalusa City Court",
  "Bossier City Court",
  "Breaux Bridge City Court",
  "Bunkie City Court",
  "Crowley City Court",
  "Denham Springs City Court",
  "East St. Tammany City Court",
  "Eunice City Court",
  "Franklin City Court",
  "Hammond City Court",
  "Houma City Court",
  "Jeanerette City Court",
  "Jefferson Parish 1st Parish Court",
  "Jefferson Parish 2nd Parish Court",
  "Jennings City Court",
  "Kaplan City Court",
  "Lafayette City Court",
  "Lake Charles City Court",
  "Leesville City Court",
  "Marksville City Court",
  "Minden City Court",
  "Monroe City Court",
  "Morgan City Court",
  "Natchitoches City Court",
  "New Iberia City Court",
  "New Orleans 1st and 2nd City Courts - combined with Orleans Parish Civil District Court",
  "New Orleans Municipal and Traffic Court",
  "Oakdale City Court",
  "Opelousas City Court",
  "Pineville City Court",
  "Plaquemine City Court",
  "Port Allen City Court",
  "Rayne City Court",
  "Ruston City Court",
  "Shreveport City Court",
  "Springhill City Court",
  "Sulphur City Court",
  "Thibodaux City Court",
  "Vidalia City Court",
  "Ville Platte City Court",
  "West Monroe City Court",
  "Winnfield City Court",
  "Winnsboro City Court",
  "Zachary City Court"
)

# Creating a function to extract relevant data from the PDF's
extract_criminal_revenue <- function(url, court_name) { 

  url <- URLencode(url)
  
  tryCatch({

    # Reading the PDF
    pdf_text <- pdf_text(url) %>% 
      read_lines() %>% 
      str_squish()
    
    # Creating a function to extract the last number from the pdf line
    safe_extract <- function(text, pattern) {
      result <- str_extract_all(text[str_detect(text, fixed(pattern, ignore_case = TRUE))], "\\$[0-9,]+")[[1]]
      if (length(result) > 0) {
        return(tail(result, 1))
      } else {
        return("Not Found")
      }
    }
    
    # Extracting the following totals
    criminal_court_fund <- safe_extract(pdf_text, "Criminal Court Fund")
    total_revenue <- safe_extract(pdf_text, "Total revenue")
    Total_expenses <- safe_extract(pdf_text, "Total expenses")
    criminal_costs_fees_bond_bail <- safe_extract(pdf_text, "Court cost fees, bond forfeitures, bail fees")
    criminal_processing <- safe_extract(pdf_text, "Criminal service, processing, and administrative fees")
    criminal_supervision <- safe_extract(pdf_text, "Supervision and special program fees")
    criminal_special <- safe_extract(pdf_text, "Special revenue fees")
    criminal_contempt <- safe_extract(pdf_text, "Criminal contempt, other fines")
    criminal_total_revenue <- safe_extract(pdf_text, "Total Criminal Defendant")
    
    # Creating a dataframe of the extracted totals
    df <- data.frame(
      Court = court_name,
      Court_Fund = criminal_court_fund,
      Revenue = total_revenue,
      Expenses = Total_expenses,
      Fees_Bond_Bail = criminal_costs_fees_bond_bail,
      Processing = criminal_processing,
      Supervision = criminal_supervision,
      Special = criminal_special,
      Contempt = criminal_contempt,
      Total = criminal_total_revenue
    )
    
    return(df)
    
  # Defining the empty dataframe if the PDF does not exist
  }, error = function(e) {
    message("Error: ", e$message)
    return(data.frame(
      Court = court_name,
      Court_Fund = NA,
      Revenue = NA,
      Expenses = NA,
      Fees_Bond_Bail = NA,
      Processing = NA,
      Supervision = NA,
      Special = NA,
      Contempt = NA,
      Total = NA
    ))
  })
}

# Defining the urls
district_urls <- paste0("https://www.lasc.org/Act116/Year_2024/District,%20Family,%20and%20Juvenile%20Courts/", encoded_judicial_district_courts, ".pdf")
city_urls <- paste0("https://www.lasc.org/Act116/Year_2024/City,%20Parish,%20and%20Municipal%20Courts/", encoded_city_courts, ".pdf")

# Creating a list of all urls
all_urls <- c(district_urls, city_urls)

# Creating a list of all court names
all_court_names <- c(judicial_district_courts, city_courts)

# Creating a list of extracted data from the dataframes
extracted_data_list <- mapply(extract_criminal_revenue,
                              url = all_urls, 
                              court_name = all_court_names, 
                              SIMPLIFY = FALSE)

# Turning the list of dataframes into a single dataframe
final_df <- bind_rows(extracted_data_list) %>%
  mutate(
    court_type = case_when(
      Court %in% judicial_district_courts ~ "District, Family, and Juvenile Courts",
      TRUE ~ "City, Parish, and Municipal Courts"
    ),
      Court_Fund = as.numeric(gsub("[$,]", "", Court_Fund)),
      Revenue = as.numeric(gsub("[$,]", "", Revenue)),
      Expenses = as.numeric(gsub("[$,]", "", Expenses)),
      Fees_Bond_Bail = as.numeric(gsub("[$,]", "", Fees_Bond_Bail)),
      Processing = as.numeric(gsub("[$,]", "", Processing)),
      Supervision = as.numeric(gsub("[$,]", "", Supervision)),
      Special = as.numeric(gsub("[$,]", "", Special)),
      Contempt = as.numeric(gsub("[$,]", "", Contempt)),
      Total = as.numeric(gsub("[$,]", "", Total)),
      empty = is.na(Fees_Bond_Bail) & is.na(Processing) & is.na(Supervision) & is.na(Special) & is.na(Contempt) & is.na(Total),
      True_total = Court_Fund + Fees_Bond_Bail + Processing + Supervision + Special + Contempt
    )


# Analyzing the output
final_df %>%
  group_by(court_type) %>%
  summarize(total = sum(True_total, na.rm = TRUE),
            courts_reporting = sum(!empty),
            prop_courts_reporting = courts_reporting/n(),
            total_if_all_reporting = total/prop_courts_reporting)


