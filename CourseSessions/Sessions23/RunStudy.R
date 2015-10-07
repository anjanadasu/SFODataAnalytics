############################################
############################################
# SOME LINES TO MODIFY (if you add/remove lines, these may change!): 24, 28, 29, 30, 34, 38, 42, 46, 49, 52 (typically only 29, 34, 38, 42, 46)
############################################
############################################

# Project Name: "(Big) Data Analytics for SFO Airport"

rm(list = ls( ))

######################################################################

# THESE ARE THE PROJECT PARAMETERS NEEDED TO GENERATE THE REPORT

# When running the case on a local computer, modify this in case you saved the case in a different directory 
# (e.g. local_directory <- "C:/user/MyDocuments" )
# type in the Console below help(getwd) and help(setwd) for more information
local_directory <- paste(getwd(),"CourseSessions/Sessions23", sep="/")
#local_directory <- "~INSEADAnalytics/CourseSessions/Sessions23"

cat("\n *********\n WORKING DIRECTORY IS ", local_directory, "\n PLEASE CHANGE IT IF IT IS NOT CORRECT using setwd(..) - type help(setwd) for more information \n *********")

# Please ENTER the name of the file with the data used. The file should contain a matrix with one row per observation (e.g. person) and one column per attribute. THE NAME OF THIS MATRIX NEEDS TO BE ProjectData (otherwise you will need to replace the name of the ProjectData variable below with whatever your variable name is, which you can see in your Workspace window after you load your file)
datafile_name="SFO" #  do not add .csv at the end! make sure the data are numeric!!!! check your file!

# Please ENTER the filename of the Report and Slides (in the doc directory) to generate 
report_file = "Report_SFO"
slides_file = "Slides_SFO"

# Please ENTER then original raw attributes to use. 
# Please use numbers, not column names! e.g. c(1:5, 7, 8) uses columns 1,2,3,4,5,7,8
factor_attributes_used= c(29:48)

# Please ENTER the selection criterions for the factors to use. 
# Choices: "eigenvalue", "variance", "manual"
factor_selectionciterion = "manual"

# Please ENTER the desired minumum variance explained 
# (ONLY USED in case "variance" is the factor selection criterion used). 
minimum_variance_explained = 65  # between 1 and 100

# Please ENTER the number of factors to use 
# (ONLY USED in case "manual" is the factor selection criterion used).
manual_numb_factors_used = 6

# Please ENTER the rotation eventually used (e.g. "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", and "cluster" - see help(principal)). Defauls is "varimax"
rotation_used="varimax"

# Please enter the minimum number below which you would like not to print - this makes the readability of the tables easier. Default values are either 10e6 (to print everything) or 0.5. Try both to see the difference.
MIN_VALUE=0.5

# Please enter the maximum number of observations to show in the report and slides 
# (DEFAULT is 50. If the number is large the report and slides may not be generated - very slow or will crash!!)
max_data_report = 50 # can also chance in server.R

###########################
# Would you like to also start a web application on YOUR LOCAL COMPUTER once the report and slides are generated?
# Select start_webapp <- 1 ONLY if you run the case on your local computer
# NOTE: Running the web application on your LOCAL computer will open a new browser tab
# Otherwise, when running on a server the application will be automatically available
# through the ShinyApps directory

# 1: start application on LOCAL computer, 0: do not start it
# SELECT 0 if you are running the application on a server 
# (DEFAULT is 0). 
start_local_webapp <- 1
# NOTE: You need to make sure the shiny library is installing (see below)

################################################
# Now run everything

# this loads the selected data: DO NOT EDIT THIS LINE
ProjectData <- read.csv(paste(paste(local_directory, "data", sep="/"), paste(datafile_name,"csv", sep="."), sep = "/"), sep=";", dec=",") # this contains only the matrix ProjectData
ProjectData=data.matrix(ProjectData) 

if (datafile_name == "Boats")
  colnames(ProjectData)<-gsub("\\."," ",colnames(ProjectData))

factor_attributes_used = unique(sapply(factor_attributes_used,function(i) min(ncol(ProjectData), max(i,1))))
ProjectDataFactor=ProjectData[,factor_attributes_used]

### TERMINAL-BASED ANALYSIS
derived_factor_columns = c(4, 7, 30, 36, 39, 43, 44, 48, 42)
derived_factor_columns = unique(sapply(derived_factor_columns,function(i) min(ncol(ProjectData), max(i,1))))
DerivedFactorsData = ProjectData[,derived_factor_columns]
# Convert to data frame
DerivedFactorsData = data.frame(DerivedFactorsData)

# Terminal 1
Terminal1 = subset(DerivedFactorsData, TERM == 1)
Terminal1 = subset(Terminal1, select=c(Q8B.Restaurants, 
                                       Q8H.Information.booths..upper.level.,
                                       Q8K.AirTrain,
                                       Q10A.Cleanliness.of.boarding.areas,
                                       Q10B.Cleanliness.of.domestic.hourly.parking.garage,
                                       Q10F.Cleanliness.of.Restrooms,
                                       Q8N.SFO.Airport.as.a.whole))
Terminal1Average = colMeans(Terminal1)

# Terminal 2
Terminal2 = subset(DerivedFactorsData, TERM == 2)
Terminal2 = subset(Terminal2, select=c(Q8B.Restaurants, 
                                       Q8H.Information.booths..upper.level.,
                                       Q8K.AirTrain,
                                       Q10A.Cleanliness.of.boarding.areas,
                                       Q10B.Cleanliness.of.domestic.hourly.parking.garage,
                                       Q10F.Cleanliness.of.Restrooms,
                                       Q8N.SFO.Airport.as.a.whole))
Terminal2Average = colMeans(Terminal2)

# Terminal 3
Terminal3 = subset(DerivedFactorsData, TERM == 3)
Terminal3 = subset(Terminal3, select=c(Q8B.Restaurants, 
                                       Q8H.Information.booths..upper.level.,
                                       Q8K.AirTrain,
                                       Q10A.Cleanliness.of.boarding.areas,
                                       Q10B.Cleanliness.of.domestic.hourly.parking.garage,
                                       Q10F.Cleanliness.of.Restrooms,
                                       Q8N.SFO.Airport.as.a.whole))
Terminal3Average = colMeans(Terminal3)

# International Terminal
TerminalIntl = subset(DerivedFactorsData, TERM == 4)
TerminalIntl = subset(TerminalIntl, select=c(Q8B.Restaurants, 
                                             Q8H.Information.booths..upper.level.,
                                             Q8K.AirTrain,
                                             Q10A.Cleanliness.of.boarding.areas,
                                             Q10B.Cleanliness.of.domestic.hourly.parking.garage,
                                             Q10F.Cleanliness.of.Restrooms,
                                             Q8N.SFO.Airport.as.a.whole))
TerminalIntlAverage = colMeans(TerminalIntl)

### DELAY TIME-BASED ANALYSIS

# On-time
OnTime = subset(DerivedFactorsData, LATECODE == 1)
OnTime = subset(OnTime, select=c(Q8B.Restaurants,
                                 Q8H.Information.booths..upper.level.,
                                 Q8K.AirTrain,
                                 Q10A.Cleanliness.of.boarding.areas,
                                 Q10B.Cleanliness.of.domestic.hourly.parking.garage,
                                 Q10F.Cleanliness.of.Restrooms,
                                 Q8N.SFO.Airport.as.a.whole))
OnTimeAverage = colMeans(OnTime)

# Late by <= 15 minutes or possibly delayed
Late15 = subset(DerivedFactorsData, LATECODE == 2)
Late15 = subset(Late15, select=c(Q8B.Restaurants, 
                                 Q8H.Information.booths..upper.level.,
                                 Q8K.AirTrain,
                                 Q10A.Cleanliness.of.boarding.areas,
                                 Q10B.Cleanliness.of.domestic.hourly.parking.garage,
                                 Q10F.Cleanliness.of.Restrooms,
                                 Q8N.SFO.Airport.as.a.whole))
Late15Average = colMeans(Late15)

# Late by 16 - 45 minutes or possibly delayed
Late45 = subset(DerivedFactorsData, LATECODE == 3)
Late45 = subset(Late45, select=c(Q8B.Restaurants, 
                                 Q8H.Information.booths..upper.level.,
                                 Q8K.AirTrain,
                                 Q10A.Cleanliness.of.boarding.areas,
                                 Q10B.Cleanliness.of.domestic.hourly.parking.garage,
                                 Q10F.Cleanliness.of.Restrooms,
                                 Q8N.SFO.Airport.as.a.whole))
Late45Average = colMeans(Late45)

# Late by 46 - 90 minutes or possibly delayed
Late90 = subset(DerivedFactorsData, LATECODE == 4)
Late90 = subset(Late90, select=c(Q8B.Restaurants, 
                                 Q8H.Information.booths..upper.level.,
                                 Q8K.AirTrain,
                                 Q10A.Cleanliness.of.boarding.areas,
                                 Q10B.Cleanliness.of.domestic.hourly.parking.garage,
                                 Q10F.Cleanliness.of.Restrooms,
                                 Q8N.SFO.Airport.as.a.whole))
Late90Average = colMeans(Late90)

# Late by > 90 minutes or possibly canceled
SuperLate = subset(DerivedFactorsData, LATECODE == 5)
SuperLate = subset(SuperLate, select=c(Q8B.Restaurants, 
                                 Q8H.Information.booths..upper.level.,
                                 Q8K.AirTrain,
                                 Q10A.Cleanliness.of.boarding.areas,
                                 Q10B.Cleanliness.of.domestic.hourly.parking.garage,
                                 Q10F.Cleanliness.of.Restrooms,
                                 Q8N.SFO.Airport.as.a.whole))
SuperLateAverage = colMeans(SuperLate)

source(paste(local_directory,"R/library.R", sep="/"))

### TO EDIT DEPENDING ON VERSION
if (require(shiny) == FALSE) 
  install.packages("shiny")

source(paste(local_directory,"R/heatmapOutput.R", sep = "/"))
source(paste(local_directory,"R/runcode.R", sep = "/"))

if (start_local_webapp){
  
  # first load the data files in the data directory so that the App see them
  MBAadmin <- read.csv(paste(local_directory, "data/MBAadmin.csv", sep = "/"), sep=";", dec=",") # this contains only the matrix ProjectData
  Boats <- read.csv(paste(local_directory, "data/Boats.csv", sep = "/"), sep=";", dec=",") # this contains only the matrix ProjectData
  Boats=data.matrix(Boats) # this file needs to be converted to "numeric"....
  SFO <- read.csv(paste(local_directory, "data/SFO.csv", sep = "/"), sep=";", dec=",") # this contains only the matrix ProjectData
  
  # now run the app
  runApp(paste(local_directory,"tools", sep="/"))  
}
