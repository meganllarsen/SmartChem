# Here I am taking the SmartChem.Processor.R code and making it universal to any data output from the SmartChem software.
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(lubridate)

# Start with files in a folder named "originals": dir.create("originals")
# Create an output directory to store output: dir.create("output")

# Add original excel workbooks to that folder.
# If not already done, name files with correct run dates in "yyyymmdd" format as the first 8 characters of the name. This allows rundate to be extracted for each run.

# Create a list of filenames for of all the .xls workbooks in the "originals" folder to use for the loop
wbpaths.orig <-
  list.files(file.path(".", "originals"), pattern = "*\\.XLS", ignore.case = T, full.names = F)

# Create a function to process all of the workbooks
sc.xlwb.processor <- function(filename) {
  
  # Extract method test and sheet names from standard SmartChem Excel output
  xlss.names <- data.frame(x = excel_sheets(path = file.path("originals", filename)))
  xlss.names <- separate(xlss.names, x, into = c("test", "sheet.name"), sep = "_")
  # extract the SmartChem test method
  sc.method <- unique(xlss.names$test)
  # extract run date
  rundate <- paste(substr(filename, 1, 4), substr(filename, 5, 6), substr(filename, 7, 8), sep = "-")
  
  # Read in worksheets
  result <- read_excel(path = file.path("originals", filename), sheet = paste0(sc.method, "_Result"))
  controls <- read_excel(path = file.path("originals", filename), sheet = paste0(sc.method, "_Controls"))
  calibrants <- read_excel(path = file.path("originals", filename), sheet = paste0(sc.method, "_Calibrants")) 
  rbl <- read_excel(path = file.path("originals", filename), sheet = paste0(sc.method, "_RBL"))
  
  # Create output path and folder
  output.path <- as.character(file.path("output", paste(sub(".xls","",filename, ignore.case = T), format(Sys.time(), format = "%Y%m%d%H%M"), sep = "-")))
  dir.create(path = output.path)
  
  # Calculate mean RBL
  rbl.mean <- mean(rbl$Abs)
  
  # Calibration curve
    # Calculate linear fit stats
    cal.lm <- lm(calibrants$Abs ~ calibrants$Concentration)
    # Plot calibration curve
    calibrants %>% ggplot(aes(x = Concentration, y = Abs)) +
      geom_point() +
      geom_smooth(method = "lm") + 
      ggtitle(label = paste(sc.method, "Calibrant Report", rundate)) +
      geom_hline(yintercept = rbl.mean, linetype = "longdash")
    ggsave(filename = file.path(output.path, paste0(sc.method, "_", rundate,".cal.curve.pdf")))
    
  # Result processing
    # Add RunDate to RunTime and define RunTime variable as POSIX type
    result <- result %>% mutate(RunDate = rundate, RunTime = ymd_hms(paste(RunDate, RunTime)), test = sc.method)
    # Extract table of sample results (exclude controls and dups)
    result.samples <- result %>% filter(SampleType == 0, !grepl("Dup", SampleID))
    # Extract table of dups
    result.dups <- result %>% filter(SampleType == 0, Position == lead(Position)) %>% 
      bind_rows(result %>% filter(SampleType == 0, Position == lag(Position))) %>% arrange(Position)
    # Summarize dups
    # Calculate Percent difference and difference
    result.dups <- result.dups %>% group_by(Position) %>% 
      mutate(pct.diff.conc = 100*Concentration/lag(Concentration), diff.conc = Concentration - lag(Concentration)) %>% 
      # include only the dups
      filter(grepl("Dup", SampleID)) 
    # Plot dups summaries 
    result.dups %>% ggplot(aes(RunTime, pct.diff.conc)) + geom_point() + geom_line() + ggtitle(paste(sc.method, rundate, "Percent difference of Dups"))
    ggsave(filename = file.path(output.path, paste0(sc.method, "_", rundate,".dups.pctdiff.pdf")))
    result.dups %>% ggplot(aes(RunTime, diff.conc)) + geom_point() + geom_line() + ggtitle(paste(sc.method, rundate, "Difference of Dups"))
    ggsave(filename = file.path(output.path, paste0(sc.method, "_", rundate,".dups.diff.pdf")))
    write.csv(result.samples, file = file.path(output.path, paste0(sc.method, "_", rundate,".result.samples.csv")))
    write.csv(result.dups, file = file.path(output.path, paste0(sc.method, "_", rundate,".result.dups.csv")))
           
  # Quality controls
    # Add RunDate to RunTime and define RunTime variable as POSIX type
    controls <- controls %>% mutate(RunDate = rundate, RunTime = ymd_hms(paste(RunDate, RunTime)))
    # Plot controls over duration of run
    controls %>% ggplot(aes(RunTime, Concentration, color = SampleID, fill = SampleID)) +
      geom_point() +
      geom_line() +
      geom_smooth(method = "lm") +
      ggtitle(label = paste(sc.method, "Controls", rundate)) + 
      xlab("Runtime") +
      ylab("Concentration") +
      geom_hline(yintercept = unique(controls$Nominal, linetype = 2))   
    ggsave(filename = file.path(output.path, paste0(sc.method, "_", rundate,".controls.pdf")))
           
    
  # Calculate coil efficiency over duration of run (NOx only) and plot results
    if(sc.method == "BNO3") {  
      coil.effic <- controls %>% filter(grepl("CCN", SampleID)) %>% 
        mutate(cd.coil.effic = (Abs/Nominal)/(lag(Abs)/lag(Nominal))) %>% 
        filter(SampleID == "CCN3") 
      coil.effic %>%   
      ggplot(aes(x = RunTime, y = cd.coil.effic)) + geom_point() + geom_line() +
        scale_y_continuous(limits = c(.75, 1)) +
        ggtitle(label = paste("Cd Coil Efficiency",rundate))
      ggsave(filename = file.path(output.path, paste0(sc.method, "_", rundate,".coil.effic.pdf")))
    }
} 

# Loop through the workbooks and use sc.xlwb.processor() to process them
for (filename in wbpaths.orig) {
  print(filename)
  sc.xlwb.processor(filename)
}
