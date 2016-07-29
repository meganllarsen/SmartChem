# Here I am taking the SmartChem.Processor.R code and making it universal to any data output from the SmartChem software.
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)

# Start with files in a folder named "originals": dir.create("originals")
# Add original excel workbooks to that folder.
# If not already done, name files with correct run dates in "yyyymmdd" format as the first 8 characters of the name. This allows rundate to be extracted for each run.

# Create a list of workbook names in the "originals" folder

wbnames.orig <-
  list.files(file.path(".","originals"), pattern = "*\\.XLS", ignore.case = T, full.names = F)

RunDates.df <- data.frame(sc.run = wbnames.orig, sc.date = 

# name of function to process excel workbooks: sc.xlwb.processor

function(workbook) {
  # This code extracts method test and sheet names for a standard SmartChem output excel workbook
  xlss.names <- data.frame(x = excel_sheets(path = file.path("original", workbook)))
  xlss.names <- separate(df, x, into = c("test", "sheet.name"), sep = "_")
  # extract the SmartChem test method
  sc.method <- unique(xlss.names$test)
  # extract run date
  rundate <- paste(substr(workbook, 1, 4), substr(workbook, 5, 6), substr(workbook, 7, 8), sep = "-")
  # Read in worksheets
  result <- read_excel(path = file.path("originals", workbook), sheet = paste0(sc.method, "_Result"))
  controls <- read_excel(path = file.path("originals", workbook), sheet = paste0(sc.method, "_Controls"))
  calibrants <- read_excel(path = file.path("originals", workbook), sheet = paste0(sc.method, "_Calibrants")) 
  rbl <- read_excel(path = file.path("originals", workbook), sheet = paste0(sc.method, "_RBL"))                     
} 

paste(substr("20160726001 - Columbia NH3.XLS", 1, 4), 
        substr("20160726001 - Columbia NH3.XLS", 5, 6), 
        substr("20160726001 - Columbia NH3.XLS", 7, 8), sep = "-")
