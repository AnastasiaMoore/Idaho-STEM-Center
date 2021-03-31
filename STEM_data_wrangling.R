############################################################################################
#                     This file deals with Data Mining and Wrangling                       #
############################################################################################

#load the required libraries
library(openxlsx)
library(readxl)
library(rvest)
library(tidyverse)
library(naniar)


###########  Below the data comes from IDAHO DEPT OF LABOR:  ############ 


#Load the data



#Idaho 
idaho_url <- "https://lmi.idaho.gov/Portals/0/2020/Projections/Statewide-Occupational-Projections.xlsx"
idaho_data_orig <- read.xlsx(idaho_url, sheet = "Table 1.02", startRow = 2, sep.names = "_",
                             na.strings = "-")

#Eastern Idaho
eastern_idaho_url <- "https://lmi.idaho.gov/Portals/0/2020/Projections/Eastern-Occupational-Projections.xlsx"
eastern_idaho_data_orig <- read.xlsx(eastern_idaho_url, sheet = "Table 1.62", startRow = 2, sep.names = "_",
                                     na.strings = "-")

#Southeastern Idaho
southeastern_idaho_url <- "https://lmi.idaho.gov/Portals/0/2020/Projections/Southeast-Occupational-Projections.xlsx"
southeastern_idaho_data_orig <- read.xlsx(southeastern_idaho_url, sheet = "Table 1.52", startRow = 2, sep.names = "_",
                                          na.strings = "-")



#Tidy the data



#create a function to tidy the datasets
tidy_data <- function(data_orig, region){
  tidy_data <- data_orig

  #fix column names (unmerge)
  names(tidy_data)[names(tidy_data) %in% c("Occupational_Code_and_Title", "X2")]  <- 
    c("Occupation_Code", "Occupation_Title")
  
  #exctract the employment years from the data
  employment_year1 <- tidy_data[2,][which(names(tidy_data) == "Employment_Projection")]
  employment_year2 <- tidy_data[2,][which(names(tidy_data) == "Employment_Projection")+1]
    
  names(tidy_data)[names(tidy_data) %in% c("Employment_Projection", "X5","X6","X7")]  <- 
    c(paste0("Employment_Projection_Number_",employment_year1), paste0("Employment_Projection_Number_",employment_year2),
      paste0("Employment_Projection_Percentage_",employment_year1), paste0("Employment_Projection_Percentage_",employment_year2))
  
  #exctract the change years from the data
  change_year_col_name <- names(tidy_data[which(grepl("Change,_",names(tidy_data)))])
  change_year <- paste0(str_sub(change_year_col_name, 9,12), "_20", str_sub(change_year_col_name, 14,15))
  
  names(tidy_data)[names(tidy_data) %in% c(paste0(change_year_col_name), "X9","X10")]  <- 
    c(paste0("Employment_Change_",change_year), paste0("Employment_Percent_Change_", change_year),
      paste0("Employment_Annual_Change_", change_year))
  
  
  
  #remove all commas in column names
  colnames(tidy_data) <- str_remove_all(colnames(tidy_data), c(",")) 
  
  
  #remove unneeded rows
  tidy_data <- tidy_data[-c(1,2),]
  
  
  
  #reset the row index
  row.names(tidy_data) <- NULL
  #add region and data source to indicate where this data is coming from
  tidy_data <- tidy_data %>% 
    mutate(Region = region,
           Data_Sourse = "Idaho Dept of Labor") 
  
  
  names(tidy_data)[names(tidy_data) %in% c("Projected_Annual_Openings",
                                           paste0("Median_Wage_",employment_year1) )]  <-
    c(paste0("Projected_Annual_Openings_", change_year), paste0("Median_Hourly_Wage_",employment_year1))
  
  #remove last 2 rows because there is no data
  tidy_data <- tidy_data[-c(nrow(tidy_data)-1,nrow(tidy_data)),]
  
  #after tifying data automatically change the new data type of the columns:
  tidy_data <- as.data.frame(tidy_data, stringsAsFactors = FALSE)
  tidy_data <- readr::type_convert(tidy_data)
  
    
  
  
  return(tidy_data)
  
}




#Idaho
idaho_data <- tidy_data(idaho_data_orig, "Idaho")

#Eastern Idaho
eastern_idaho_data <- tidy_data(eastern_idaho_data_orig, "Eastern Idaho")

#Southeastern Idaho
southeastern_idaho_data <- tidy_data(southeastern_idaho_data_orig, "Southeastern Idaho")




#Merge the three data sets
idaho_dept_of_labor_data <- bind_rows(idaho_data, eastern_idaho_data, southeastern_idaho_data)


#after tifying data automatically change the new data type of the columns:
idaho_dept_of_labor_data <- as.data.frame(idaho_dept_of_labor_data, stringsAsFactors = FALSE)
idaho_dept_of_labor_data <- readr::type_convert(idaho_dept_of_labor_data)





###########  Below the data comes from BUREAU OF LABOR STATISTICS:  ############



#Web-scrape the data
#refer to  https://m-clark.github.io/webR/web_scraping.html  to learn how to do it




#Define the function to be used for data scraping (for less repetition in code)
scrape_data <- function(website_url){
  base_page <- read_html(website_url) %>% 
    html_node('body') %>% 
    html_node('table')
  
  #extract the table
  scraped_data <- base_page %>% 
    html_table(fill = TRUE) 
  return(scraped_data)
}

#National
national_data1_orig <- scrape_data("https://data.bls.gov/projections/occupationProj")

national_data2_orig <- scrape_data("https://www.bls.gov/oes/current/oes_nat.htm")


#Idaho
idaho2_data_orig <- scrape_data("https://www.bls.gov/oes/current/oes_id.htm#00-0000")
 

#Eastern Idaho
#Idaho Falls
idaho_falls_data_orig <- scrape_data("https://www.bls.gov/oes/2019/may/oes_26820.htm")

#Pocatello
pocatello_data_orig <- scrape_data("https://www.bls.gov/oes/2019/may/oes_38540.htm")




#Tidying the data



#National
national_data1 <- national_data1_orig


#substitute all colnames with the names in the first row because they are almost duplicates
colnames(national_data1) <- national_data1[1,] 

#remove not needed rows
national_data1 <- national_data1[-1,]  
row.names(national_data1) <- NULL  #reset row index

national_data1 <- national_data1[-1,]  
row.names(national_data1) <- NULL   #reset row index

#fix column names
colnames(national_data1) <- gsub(" ", "_", colnames(national_data1)) #substitute any " " in a column name by "_"
colnames(national_data1) <- str_remove_all(colnames(national_data1), c(",")) #remove all commas in column names
colnames(national_data1) <- gsub("-", "_", colnames(national_data1)) #substitute any "-" in a column name by "_"



#fix values in the rows
#get rid of commas in numerica values
for(i in 3:ncol(national_data1)) {       # for-loop over columns
  for(row in 1:nrow(national_data1)){    #for loop over rows
    national_data1[row,][i]<- str_remove_all( national_data1[row,][i], c(",")) 
    
    # Median_Annual_Wage_ column has ">=208000" value which messes up the type of that column, to make sure the type is still numeric, 
    # I decided to substitute those values with "208100"
    national_data1[row,][i] <- str_replace_all(national_data1[row,][i], ">=208000", "208100") 
  }
}


na_strings <- c("N/A", "$39810", "-")
national_data1 <- national_data1 %>% replace_with_na_all(condition = ~.x %in% na_strings)

#I am using this complex logic to not use years in the names in case the data updates to a different year
#extract the year:
occup_year_col_name <- names(national_data1)[which(  grepl(  "Occupational_Openings_", names(national_data1)  )  )]
occup_year <- str_sub(occup_year_col_name, 23,31)
  
names(national_data1)[names(national_data1) == occup_year_col_name] <-
  paste0("Projected_Annual_Openings_", occup_year)



#add region and data source to indicate where this data is coming from
national_data1 <- national_data1 %>% 
  mutate(Region = "National",
         Data_Sourse = "Bureau of Labor Statistics")


#after tifying data automatically change the new data type of the columns
national_data1 <- as.data.frame(national_data1, stringsAsFactors = FALSE)
national_data1 <- readr::type_convert(national_data1)


#create a function to tidy the rest of the datasets (because the have the same exact structure)



#######################################################
##        A function that tidies scraped data        ##
#######################################################
tidy_scraped_data <- function(data,region){

  data <- national_data2_orig
  region <- "National"

  
  tidy_dataset <- data
  

  #get the index of columns that need to be erazed later
for(i in 1:30) {       # for-loop over columns
    if(is.na(tidy_dataset[2,][i])){   #search for empty values in the 2nd row
      start_col <- i
      break
    }
  }
 

  #get the year the data is gathered in
  data_discription <- tidy_dataset$X2[1]
  data_year <- str_sub(data_discription, 5,8)
  
  
  #substitute all colnames with the names in the 2nd row because they are most accurate
  colnames(tidy_dataset) <- tidy_dataset[2,] 
  
  #remove not needed rows
  tidy_dataset <- tidy_dataset[-(1:3),]  
  row.names(tidy_dataset) <- NULL  #reset row index
  
  
  #remove empty columns at the end of the dataframe
  tidy_dataset[start_col:ncol(tidy_dataset)] <- NULL
  
  
  #fix column names
  colnames(tidy_dataset) <- gsub(" ", "_", colnames(tidy_dataset)) #substitute any " " in a column name by "_"
  colnames(tidy_dataset) <- str_remove_all(colnames(tidy_dataset), c(",")) #remove all commas in column names
   

  names(tidy_dataset)[names(tidy_dataset) %in% c("Occupation_code","Occupation_title_(click_on_the_occupation_title_to_view_its_profile)",
                                           "Employment", "Employment_RSE", "Median_hourly_wage",
                                           "Mean_hourly_wage", "Annual_mean_wage", 
                                           "Mean_wage_RSE")]  <-
    c("Occupation_Code","Occupation_Title", paste0("Employment_",data_year),"Employment_RSE_Percentage",
      paste0("Median_Hourly_Wage_",data_year), paste0("Mean_Hourly_Wage_",data_year),
      paste0("Mean_Annual_Wage_",data_year), "Mean_wage_RSE_Percentage")

  
  #fix values in the rows
  
  #From Bureau of Labor Statistics:
  # (1) Estimates for detailed occupations do not sum to the totals because the totals include occupations not shown separately. Estimates do not include self-employed workers.
  # 
  # (2) Annual wages have been calculated by multiplying the hourly mean wage by a "year-round, full-time" hours figure of 2,080 hours; for those occupations where there is not an hourly wage published, the annual wage has been directly calculated from the reported survey data.
  # 
  # (3) The relative standard error (RSE) is a measure of the reliability of a survey statistic. The smaller the relative standard error, the more precise the estimate.
  # 
  # (4) Wages for some occupations that do not generally work year-round, full time, are reported either as hourly wages or annual salaries depending on how they are typically paid.
  # 
  # (5) This wage is equal to or greater than $100.00 per hour or $208,000 per year.
  # 
  # (8) Estimates not released.
  # 
  # (9) The location quotient is the ratio of the area concentration of occupational employment to the national average concentration. A location quotient greater than one indicates the occupation has a higher share of employment than average, and a location quotient less than one indicates the occupation is less prevalent in the area than average.
  
  #make appropriate substitutions to preserve the type of each column

   # na_strings <- c("(1)","(2)", "(3)", "(4)", "(8)", "(9)")
   # tidy_dataset <- tidy_dataset %>% replace_with_na_all(condition = ~.x %in% na_strings)
   # 
  # tidy_dataset <- lapply(tidy_dataset, gsub, pattern="(1)|(2)|(3)|(4)|(8)|(9)", replacement=NA) %>% unlist()
  
  #try <- tidy_dataset
  for(i in 4:ncol(tidy_dataset)) {       # for-loop over columns
    for(row in 1:nrow(tidy_dataset)){    #for loop over rows
    tidy_dataset[row,][i]<- str_remove_all( tidy_dataset[row,][i], c(",|%|\\$")) #remove all commas,%, $ in values
    
    #(5) corresponds to wage that is equal to or greater than $100.00 per hour or $208,000 per year
    tidy_dataset[row,][i] <- str_replace_all(tidy_dataset[row,][i], "\\(5\\)", ifelse(grepl("Hourly",names(tidy_dataset[i])),"110.00","208100")) #if (5) substitute with mean that is in the next column
    tidy_dataset[row,][i] <-  na_if(tidy_dataset[row,][i] , "(4)")
    tidy_dataset[row,][i] <-  na_if(tidy_dataset[row,][i] , "(8)")
    }
    }
 
   
 #add region and data source to indicate where this data is coming from
  tidy_dataset <- tidy_dataset %>% 
    mutate(Region = region,
           Data_Sourse = "Bureau of Labor Statistics")
  
  #after tifying data automatically change the new data type of the columns
  tidy_dataset <- as.data.frame(tidy_dataset, stringsAsFactors = FALSE)
  tidy_dataset <- readr::type_convert(tidy_dataset)
  
  
  return(tidy_dataset)
}


national_data2 <- tidy_scraped_data(national_data2_orig, "National")


#Idaho
idaho2_data <- tidy_scraped_data(idaho2_data_orig, "Idaho")


#Eastern Idaho
#Idaho Falls
idaho_falls_data <- tidy_scraped_data(idaho_falls_data_orig, "Eastern Idaho")

#Pocatello
pocatello_data <- tidy_scraped_data(pocatello_data_orig, "Eastern Idaho")



#Merge the 4 above data sets
scraped_data <- national_data2 %>%
  full_join(idaho2_data) %>%
  full_join(idaho_falls_data) %>%
  full_join(pocatello_data)



#combine all of the data sets together
occupations_data <- idaho_dept_of_labor_data %>%
  full_join(national_data1)%>%
  full_join(scraped_data)

#add column that would tell if the occupation is STEM or not
occupations_data <- occupations_data %>%
  mutate(STEM = ifelse(grepl("15-|17-|19-",occupations_data$Occupation_Code),"STEM","Non-STEM"))



#reset row index
row.names(occupations_data) <- NULL

#clean-up occupation title names
occupations_data$Occupation_Title <- str_extract(occupations_data$Occupation_Title,ifelse(grepl("\r\n\t\t\t\t Show/hide Example Job", occupations_data$Occupation_Title),".+?(?=\r\n\t\t\t\t Show/hide Example Job)",occupations_data$Occupation_Title ))
#make every word upper case 
occupations_data$Occupation_Title <- str_to_title(occupations_data$Occupation_Title)

#remove not needed columns
occupations_data <- occupations_data[,-c((which(names(occupations_data) == "Level")),
                                         (which(names(occupations_data) == "Workex_Code")),
                                         (which(names(occupations_data) == "trCode")),
                                         (which(names(occupations_data) == "Education_Code")))]


################ pivot longer the years ##################

#get the column names without refering to the years of those columns
Employment_Change_columns <- names(occupations_data)[which(grepl("Employment_Change_2", names(occupations_data)))]
#get the years from those columns
Employment_Change_years_list <- str_extract_all(Employment_Change_columns, "([0-9]+)_([0-9]+)") %>% unlist()
Employment_Change_years <- str_c(Employment_Change_years_list, collapse = "_")



occupations_data <- occupations_data %>% 
  pivot_longer(cols = all_of(Employment_Change_columns),
               names_to = "Employment_Change_year",
               values_to = paste0("Employment_Change_", Employment_Change_years))


#get the column names without refering to the years of those columns
Employment_Percent_Change_columns <- names(occupations_data)[which(grepl("Employment_Percent_Change_2", names(occupations_data)))]
#get the years from those columns
Employment_Percent_Change_years_list <- str_extract_all(Employment_Percent_Change_columns, "([0-9]+)_([0-9]+)") %>% unlist()
Employment_Percent_Change_years <- str_c(Employment_Percent_Change_years_list, collapse = "_")


occupations_data <- occupations_data %>% 
  pivot_longer(cols = all_of(Employment_Percent_Change_columns),
               names_to = "Employment_Percent_Change_year",
               values_to = paste0("Employment_Percent_Change_",Employment_Percent_Change_years))


#get the column names without refering to the years of those columns
Median_Hourly_Wage_columns <- names(occupations_data)[which(grepl("Median_Hourly_Wage_2", names(occupations_data)))]
#get the years from those columns
Median_Hourly_Wage_years_list <- str_extract_all(Median_Hourly_Wage_columns, "([0-9]+)") %>% unlist()
Median_Hourly_Wage_years <- str_c(Median_Hourly_Wage_years_list, collapse = "_")



occupations_data <- occupations_data %>% 
  pivot_longer(cols = all_of(Median_Hourly_Wage_columns),
               names_to = "Median_Hourly_Wage_year",
               values_to = paste0("Median_Hourly_Wage_", Median_Hourly_Wage_years))


#get the column names without refering to the years of those columns
Projected_Annual_Openings_columns <- names(occupations_data)[which(grepl("Projected_Annual_Openings_2", names(occupations_data)))]
#get the years from those columns
Projected_Annual_Openings_years_list <- str_extract_all(Projected_Annual_Openings_columns, "([0-9]+)_([0-9]+)") %>% unlist()
Projected_Annual_Openings_years <- str_c(Projected_Annual_Openings_years_list, collapse = "_")


occupations_data <- occupations_data %>% 
  pivot_longer(cols = all_of(Projected_Annual_Openings_columns),
               names_to = "Projected_Annual_Openings_year",
               values_to = paste0("Projected_Annual_Openings_",Projected_Annual_Openings_years))

#fix the year columns
occupations_data <- occupations_data %>% 
  mutate(Employment_Percent_Change_year = str_extract_all(Employment_Percent_Change_year, "([0-9]+_[0-9]+)"),
         Median_Hourly_Wage_year = str_extract_all(Median_Hourly_Wage_year, "([0-9]+)"),
         Projected_Annual_Openings_year = str_extract_all(Projected_Annual_Openings_year, "([0-9]+_[0-9]+)"))

#fix the dashes in the year columns
occupations_data$Employment_Percent_Change_year <- gsub("_", "-", occupations_data$Employment_Percent_Change_year ) #substitute any "_" in a column name by "-"
occupations_data$Projected_Annual_Openings_year <- gsub("_", "-", occupations_data$Projected_Annual_Openings_year ) #substitute any "_" in a column name by "-"


#get the column name without refering to the years of that column
Median_Hourly_Wage_column <- names(occupations_data)[which(grepl("Median_Hourly_Wage_2", names(occupations_data)))]

#create a character column to display the text in tableau for median wages column (take care of >=$100 of wage)
occupations_data <- occupations_data %>% 
  mutate(Text_Median_Hourly_Wage_column = ifelse(eval(parse(text=Median_Hourly_Wage_column)) == 110, ">=$100.0", paste0("$", round(eval(parse(text=Median_Hourly_Wage_column)),1))))


#write the combined data to excel
write.xlsx(x=occupations_data, file="occupations_data.xlsx", col.names=TRUE, row.names=FALSE, append=TRUE)

