# SmartChem data processing
# Print calibration curve
# Summarize method controls - 
  # Dups table with difference and percent difference 
  # check standards (mid-range, blank, ccb)
# Summarize RBL
# Extract table of sample results (exclude controls)

library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

path <- file.path("week of 2016-07-11/")
# Read in data
NO2.result <- read_excel(path = paste(path, "20160713001 - NO2.XLS", sep = ""), sheet = "BNO2_Result")

# Define run date. SmartChem does not record this in the output
RunDate.chr <- "2016-07-13"

# Add RunDate to RunTime and define RunTime variable as POSIX type
NO2.result <- NO2.result %>% mutate(RunDate = RunDate.chr, RunTime = ymd_hms(paste(RunDate, RunTime)))

# Extract table of sample results (exclude controls and dups)
NO2.result.samples <- NO2.result %>% filter(SampleType == 0, !grepl("Dup", SampleID))

# Extract table of dups
NO2.result.dups <- NO2.result %>% filter(SampleType == 0, Position == lead(Position)) %>% 
  bind_rows(NO2.result %>% filter(SampleType == 0, Position == lag(Position))) %>% arrange(Position)
  # Summarize dups
    # Percent difference and absolute difference
  NO2.result.dups <- NO2.result.dups %>% group_by(Position) %>% 
      mutate(pct.diff.conc = 100*Concentration/lag(Concentration), diff.conc = Concentration - lag(Concentration)) %>% 
      filter(grepl("Dup", SampleID)) # filter to include only the dups#
  NO2.result.dups %>% ggplot(aes(RunTime, pct.diff.conc)) + geom_point() + geom_line() + ggtitle("Percent difference of Dups")
  NO2.result.dups %>% ggplot(aes(RunTime, diff.conc)) + geom_point() + geom_line() + ggtitle("Difference of Dups")    
  
    


