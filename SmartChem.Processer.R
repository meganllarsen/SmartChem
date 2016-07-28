# SmartChem data processing
# Print calibration curve
# Summarize method controls - 
  # Dups table with difference and percent difference 
  # check standards (mid-range, blank, ccb)
# Summarize RBL
# Extract table of sample results (exclude controls)
# For nitrate, calculate NOx and Coil efficiency

# Write loops to take original data from "original" folder and process into new folders named using the filename of the original file

library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)

# First read in all data and name each table
NO2.result <- read_excel(path = file.path("original", "20160713001 - NO2.XLS"), sheet = "BNO2_Result")
NO2.controls <- read_excel(path = file.path("original", "20160713001 - NO2.XLS"), sheet = "BNO2_Controls")
NO2.calibrants <- read_excel(path = file.path("original", "20160713001 - NO2.XLS"), sheet = "BNO2_Calibrants") 
NO2.rbl <- read_excel(path = file.path("original", "20160713001 - NO2.XLS"), sheet = "BNO2_RBL")

NOx.result <- read_excel(path = file.path("original", "20160713003 - NO3.XLS"), sheet = "BNO3_Result")   


# This code extracts method test and sheet names for a standard SmartChem output file
df <- data.frame(x = excel_sheets(path = file.path("original", "20160713001 - NO2.XLS")))
df <- separate(df, x, into = c("test", "sheet.name"), sep = "_")

# Define run date. SmartChem does not record this in the output
RunDate.chr.NO2 <- "2016-07-13"
RunDate.chr.NOx <- "2016-07-13"
RunDate.chr.NH3a <- "2016-07-12"
RunDate.chr.NH3b <- "2016-07-14"
RunDate.chr.PO4 <- "2016-07-12"


# Nitrite
  # Results and dups
    # Add RunDate to RunTime and define RunTime variable as POSIX type
      NO2.result <- NO2.result %>% mutate(RunDate = RunDate.chr.NO2, RunTime = ymd_hms(paste(RunDate, RunTime)), test = unique(df$test))
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
    # Plot dups summaries 
        NO2.result.dups %>% ggplot(aes(RunTime, pct.diff.conc)) + geom_point() + geom_line() + ggtitle("Percent difference of Dups")
        NO2.result.dups %>% ggplot(aes(RunTime, diff.conc)) + geom_point() + geom_line() + ggtitle("Difference of Dups")    
  
  # Quality controls
    # Read in data
    NO2.controls <- read_excel(path = file.path("original", "20160713001 - NO2.XLS"), sheet = "BNO2_Controls") 
    # Add RunDate to RunTime and define RunTime variable as POSIX type
    NO2.controls <- NO2.controls %>% mutate(RunDate = RunDate.chr.NO2, RunTime = ymd_hms(paste(RunDate, RunTime)))
    # Plot controls over duration of run
    NO2.controls %>% ggplot(aes(RunTime, Concentration, color = SampleID, fill = SampleID)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "lm") +
      ggtitle(label = paste("BNO2 Controls"," ",RunDate.chr.NOx, sep = "")) + 
      xlab("Runtime") +
      ylab("Concentration") +
      geom_hline(yintercept = unique(NO2.controls$Nominal, linetype = 2))   
  
  # Calibration curve
    # Calculate linear fit stats
      NO2.cal.lm <- lm(NO2.calibrants$Abs ~ NO2.calibrants$Concentration)
    # Plot calibration curve
      NO2.calibrants %>% ggplot(aes(x = Concentration, y = Abs)) +
          geom_point() +
          geom_smooth(method = "lm") + 
          ggtitle(label = paste("Calibrant Report - BNO2", " ", RunDate.chr.NO2))    
       
# Nitrate
  # Results and dups
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
    # Plot dups summaries
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
    # Calculate coil efficiency over duration of run (NOx only)
    NOx.controls %>% filter(grepl("CCN", SampleID)) %>% 
      mutate(cd.coil.effic = (Abs/Nominal)/(lag(Abs)/lag(Nominal))) %>% 
      filter(SampleID == "CCN3") %>% 
      ggplot(aes(x = RunTime, y = cd.coil.effic)) + geom_point() + geom_line() +
      scale_y_continuous(limits = c(.75, 1)) +
      ggtitle(label = paste("Cd Coil Efficiency"," ",RunDate.chr.NOx, sep = ""))
  
  # Calibration curve
    # Read in data
    NOx.calibrants <- read_excel(path = file.path("original", "20160713003 - NO3.XLS"), sheet = "BNO3_Calibrants")
    # Calculate linear fit stats
    NOx.cal.lm <- lm(NOx.calibrants$Abs ~ NOx.calibrants$Concentration)
    # Plot calibration curve
    NOx.calibrants %>% ggplot(aes(x = Concentration, y = Abs)) +
      geom_point() +
      geom_smooth(method = "lm") + 
      ggtitle(label = paste("Calibrant Report - BNO3", " ", RunDate.chr.NOx))

#Ammonia    
    # Results and dups
    NH3.result <- read_excel(path = file.path("original", "20160714001 - NH3_2.XLS"), sheet = "WNHA_Result")
    # Add RunDate to RunTime and define RunTime variable as POSIX type
    NH3.result <- NH3.result %>% mutate(RunDate = RunDate.chr.NH3b, RunTime = ymd_hms(paste(RunDate, RunTime)))
    # Extract table of sample results (exclude controls and dups)
    NH3.result.samples <- NH3.result %>% filter(SampleType == 0, !grepl("Dup", SampleID))
    # Extract table of dups
    NH3.result.dups <- NH3.result %>% filter(SampleType == 0, Position == lead(Position)) %>% 
      bind_rows(NH3.result %>% filter(SampleType == 0, Position == lag(Position))) %>% arrange(Position)
    # Summarize dups
    # Percent difference and absolute difference
    NH3.result.dups <- NH3.result.dups %>% group_by(Position) %>% 
      mutate(pct.diff.conc = 100*Concentration/lag(Concentration), diff.conc = Concentration - lag(Concentration)) %>% 
      filter(grepl("Dup", SampleID)) # filter to include only the dups#
    # Plot dups summaries 
    NH3.result.dups %>% ggplot(aes(RunTime, pct.diff.conc)) + geom_point() + geom_line() + ggtitle("Percent difference of Dups")
    NH3.result.dups %>% ggplot(aes(RunTime, diff.conc)) + geom_point() + geom_line() + ggtitle("Difference of Dups")    
    
    # Quality controls
    # Read in data
    NH3.controls <- read_excel(path = file.path("original", "20160714001 - NH3_2.XLS"), sheet = "WNHA_Controls") 
    # Add RunDate to RunTime and define RunTime variable as POSIX type
    NH3.controls <- NH3.controls %>% mutate(RunDate = RunDate.chr.NH3b, RunTime = ymd_hms(paste(RunDate, RunTime)))
    # Plot controls over duration of run
    NH3.controls %>% ggplot(aes(RunTime, Concentration, color = SampleID, fill = SampleID)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "lm") +
      ggtitle(label = paste("WNHA Controls"," ",RunDate.chr.NH3b, sep = "")) + 
      xlab("Runtime") +
      ylab("Concentration") +
      geom_hline(yintercept = unique(NH3.controls$Nominal, linetype = 2))   
    
    # Calibration curve
    NH3.calibrants <- read_excel(path = file.path("original", "20160714001 - NH3_2.XLS"), sheet = "WNHA_Calibrants") 
    # Calculate linear fit stats
    NH3.cal.lm <- lm(NH3.calibrants$Abs ~ NH3.calibrants$Concentration)
    # Plot calibration curve
    NH3.calibrants %>% ggplot(aes(x = Concentration, y = Abs)) +
      geom_point() +
      geom_smooth(method = "lm") + 
      ggtitle(label = paste("Calibrant Report - WNHA", " ", RunDate.chr.NH3b))         
    
#Phosphate    
    # Results and dups
    PO4.result <- read_excel(path = file.path("original", "20160712001 - PO4.XLS"), sheet = "SMPL_Result")
    # Add RunDate to RunTime and define RunTime variable as POSIX type
    PO4.result <- PO4.result %>% mutate(RunDate = RunDate.chr.PO4, RunTime = ymd_hms(paste(RunDate, RunTime)))
    # Extract table of sample results (exclude controls and dups)
    PO4.result.samples <- PO4.result %>% filter(SampleType == 0, !grepl("Dup", SampleID))
    # Extract table of dups
    PO4.result.dups <- PO4.result %>% filter(SampleType == 0, Position == lead(Position)) %>% 
      bind_rows(PO4.result %>% filter(SampleType == 0, Position == lag(Position))) %>% arrange(Position)
    # Summarize dups
    # Percent difference and absolute difference
    PO4.result.dups <- PO4.result.dups %>% group_by(Position) %>% 
      mutate(pct.diff.conc = 100*Concentration/lag(Concentration), diff.conc = Concentration - lag(Concentration)) %>% 
      filter(grepl("Dup", SampleID)) # filter to include only the dups#
    # Plot dups summaries 
    PO4.result.dups %>% ggplot(aes(RunTime, pct.diff.conc)) + geom_point() + geom_line() + ggtitle("Percent difference of Dups")
    PO4.result.dups %>% ggplot(aes(RunTime, diff.conc)) + geom_point() + geom_line() + ggtitle("Difference of Dups")    
    
    # Quality controls
    # Read in data
    PO4.controls <- read_excel(path = file.path("original", "20160712001 - PO4.XLS"), sheet = "SMPL_Controls") 
    # Add RunDate to RunTime and define RunTime variable as POSIX type
    PO4.controls <- PO4.controls %>% mutate(RunDate = RunDate.chr.PO4, RunTime = ymd_hms(paste(RunDate, RunTime)))
    # Plot controls over duration of run
    PO4.controls %>% ggplot(aes(RunTime, Concentration, color = SampleID, fill = SampleID)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "lm") +
      ggtitle(label = paste("SMPL Controls"," ",RunDate.chr.PO4, sep = "")) + 
      xlab("Runtime") +
      ylab("Concentration") +
      geom_hline(yintercept = unique(PO4.controls$Nominal, linetype = 2))   
    
    # Calibration curve
    PO4.calibrants <- read_excel(path = file.path("original", "20160712001 - PO4.XLS"), sheet = "SMPL_Calibrants") 
    # Calculate linear fit stats
    PO4.cal.lm <- lm(PO4.calibrants$Abs ~ PO4.calibrants$Concentration)
    # Plot calibration curve
    PO4.calibrants %>% ggplot(aes(x = Concentration, y = Abs)) +
      geom_point() +
      geom_smooth(method = "lm") + 
      ggtitle(label = paste("Calibrant Report - SMPL", " ", RunDate.chr.PO4))   
    
# Merge all results into one table
NO2.result.samples.merge <- NO2.result.samples %>% select(1:2) %>%  mutate(test = "BNO2")
NOx.result.samples.merge <- NOx.result.samples %>% select(1:2) %>%  mutate(test = "BNO3")
NH3.result.samples.merge <- NH3.result.samples %>% select(1:2) %>%  mutate(test = "WNHA")    
PO4.result.samples.merge <- PO4.result.samples %>% select(1:2) %>%  mutate(test = "SMPL")

combine.list <- list(NO2.result.samples.merge, NOx.result.samples.merge, NH3.result.samples.merge, PO4.result.samples.merge)
merged.data <- bind_rows(combine.list) %>% spread(test, Concentration) %>% mutate(NO3 = BNO3 - BNO2) %>% 
  separate(SampleID, into = c("site", "date.yymmdd", "depth"), sep = "_", remove = F)
merged.data$date.yymmdd <- ymd(as.character(merged.data$date.yymmdd))
merged.data.kate <- merged.data %>% filter(date.yymmdd == "2015-08-18" | date.yymmdd >= "2015-10-15" & date.yymmdd <= "2015-10-20")
write.csv(merged.data.kate, file = "Lacamas.nutrients.AugOct2015.csv")
