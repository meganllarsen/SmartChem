# SmartChem data processing
# Print calibration curve
# Summarize method controls - 
  # Dups table with difference and percent difference 
  # check standards (mid-range, blank, ccb)
# Summarize RBL
# Extract table of sample results (exclude controls)
# For nitrate, calculate NOx and Coil efficiency

library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)

path <- file.path("week of 2016-07-11/")

# Nitrite
# Read in data. Note that "path" does not appear to work consistently (possible Windows/Mac issue?)
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
  
# Nitrate
  # Results data
    # Read in data
    NOx.result <- read_excel(path = "week of 2016-07-11/20160713003 - NO3.XLS", sheet = "BNO3_Result")   
    # Define run date. SmartChem does not record this in the output
    RunDate.chr.NOx <- "2016-07-13"
    # Add RunDate to RunTime and define RunTime variable as POSIX type
    NOx.result <- NOx.result %>% mutate(RunDate = RunDate.chr.NOx, RunTime = ymd_hms(paste(RunDate, RunTime)))
    # Extract table of sample results (exclude controls and dups)
    NOx.result.samples <- NOx.result %>% filter(SampleType == 0, !grepl("Dup", SampleID))
    # Extract table of dups
    NOx.result.dups <- NOx.result %>% filter(SampleType == 0, Position == lead(Position)) %>% 
      bind_rows(NOx.result %>% filter(SampleType == 0, Position == lag(Position))) %>% arrange(Position)
    # Summarize dups
    # Percent difference and absolute difference
    NOx.result.dups <- NOx.result.dups %>% group_by(Position) %>% 
      mutate(pct.diff.conc = 100*Concentration/lag(Concentration), diff.conc = Concentration - lag(Concentration)) %>% 
      filter(grepl("Dup", SampleID)) # filter to include only the dups#
    NOx.result.dups %>% ggplot(aes(RunTime, pct.diff.conc)) + geom_point() + geom_line() + ggtitle("Percent difference of Dups")
    NOx.result.dups %>% ggplot(aes(RunTime, diff.conc)) + geom_point() + geom_line() + ggtitle("Difference of Dups") 
  
  # Quality controls
    # Read in data
    NOx.controls <- read_excel(path = "week of 2016-07-11/20160713003 - NO3.XLS", sheet = "BNO3_Controls") 
    # Add RunDate to RunTime and define RunTime variable as POSIX type
    NOx.controls <- NOx.controls %>% mutate(RunDate = RunDate.chr.NOx, RunTime = ymd_hms(paste(RunDate, RunTime)))
    # Plot controls over duration of run
    NOx.controls %>% ggplot(aes(RunTime, Concentration, color = SampleID, fill = SampleID)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "lm") +
      ggtitle(label = paste("BNO3 Controls"," ",RunDate.chr.NOx, sep = "")) + 
      xlab("Runtime") +
      ylab("Concentration") +
      geom_hline(yintercept = unique(NOx.controls$Nominal, linetype = 2)) 
      # Perhaps add tolerances (upper limit and lower limit) based on long-term data for each method from SC
    # Calculate coil efficiency over duration of run
    NOx.controls %>% filter(grepl("CCN", SampleID)) %>% 
      mutate(cd.coil.effic = (Abs/Nominal)/(lag(Abs)/lag(Nominal))) %>% 
      filter(SampleID == "CCN3") %>% 
      ggplot(aes(x = RunTime, y = cd.coil.effic)) + geom_point() + geom_line() +
      scale_y_continuous(limits = c(.75, 1)) +
      ggtitle(label = paste("Cd Coil Efficiency"," ",RunDate.chr.NOx, sep = ""))
  
  # Calibration curve
    # Read in data
    NOx.calibrants <- read_excel(path = "week of 2016-07-11/20160713003 - NO3.XLS", sheet = "BNO3_Calibrants") 
    # Calculate linear fit stats
    NOx.cal.lm <- lm(NOx.calibrants$Abs ~ NOx.calibrants$Concentration)
    # Plot calibration curve
    NOx.calibrants %>% ggplot(aes(x = Concentration, y = Abs)) +
      geom_point() +
      geom_smooth(method = "lm") + 
      ggtitle(label = paste("Calibrant Report - BNO3", " ", RunDate.chr.NOx))
    
    
 