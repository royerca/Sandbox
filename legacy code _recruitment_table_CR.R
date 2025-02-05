###############################################
   #### Recruitment rate table 2011-2024 ###
###############################################
#
# This Script was created by Daniel Ottmann in 2016 to create a data frame of recruitment rate fromm the SMURF data
# Updated by Cameron Royer in 2024 to include collection # in final recruitment table
#_______________________________________________________________________________________________________________________________________________________________________
#
#
# Load packages (have to be previously installed in R)
library("plyr")
library("readxl")
library("dplyr")

# Load the Excel file
file_path <- "C:/Users/16616/Desktop/Files/Oregon State University/Graduate School/Rockfish work/OYTB ID/OYTB recruitment/wt.txt"  # Update with the correct path
data <- read.delim("wt.txt", stringsAsFactors=FALSE)

# Change headers
names(data)<-c("site",  "day",  "date",	"collection",	"month",	"year",	"mooring",	"fish_id",	"spp",	"length",	"interval",	"mr")

# Set format of date
# Parse the existing dates first using the correct format
data$date <- as.Date(data$date, format="%m/%d/%y")

# Then reformat the dates to the desired format
data$date <- format(data$date, format="%d-%b-%y")

# Create a lookup vector for month abbreviations
month_lookup <- setNames(1:12, c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                                 "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))

# Convert abbreviated months in props_sfla to numbers
data <- data %>%
  mutate(month = month_lookup[month])

# Remove lost SMURFs 
data<-data[!(data$site==""),]

# Remove CF2 and OR3 (a 2011 mooring set in CF2 that was draged by currents to another location (OR3) and then got lost):
data <- data [!(data$mooring %in% c("CF02","OR03")),]

# Remove YOYs:
data <- subset(data, length < 70) 
head(data[order(-data$length),],10) #making sure YoYs are removed - ordered to see largest fish

# Substitute length = -99999.99 for "blank", and call "no_fish" to the spp when SMURF collections had no fish:
data$length[data$length== -99999.99]<-""
data$spp <- ifelse(data$fish_id == 0, "no_fish", data$spp)

# Call "unID" to unidentified species
data$spp[data$fish_id != 0 & data$spp==""]<-"unID"

# Remove colummn length (if we don't need it):
data<- data[,-10]

#Add a new column to diferenciate if the site is in central oregon or in southern oregon:
data$region <- ifelse(data$site %in% c("CF", "OR"), "Central", "South")

# Check for missing values in key columns
missing_data <- data[is.na(data$day) | is.na(data$mooring) | is.na(data$spp), ]
if (nrow(missing_data) > 0) {
  print("Rows with missing data:")
  print(missing_data)
}

# Remove rows with missing values in key columns
data_clean <- na.omit(data[, c("day", "mooring", "spp")])

# Summarize number of recruits
recruitment <- ddply(data_clean, c("day", "mooring", "spp"), summarise,
                     count = length(spp),
                     .drop = FALSE,
                     .progress = "text")

# Verify the output
head(recruitment)

# Add "all_species" recruitment:
recruitment <- ddply(recruitment, .(day, mooring), function(x){
  total_count <- sum(subset(x, spp != "no_fish")$count)
  newrow <- x[1,] # pulled out a row
  newrow$spp <- "all_spp"
  newrow$count <- total_count
  x <- data.frame(rbind(x, newrow))
  return(x)
}, .progress="text")

summary(data[, c("day", "mooring", "spp")])
data$day <- as.factor(data$day)
data$mooring <- as.factor(data$mooring)
data$spp <- as.factor(data$spp)

# get rid of non-existing moorings (originted by the recursife function applied in line 48):
recruitment <- ddply(recruitment, .(day, mooring), function(x){
  if(sum(x$count)==0) {remove <- "REMOVE"}
  else {remove <- "KEEP"}
  
  # an alternative way to write the same function using 'ifelse'
  # remove <- ifelse(sum(x$count)==0, "REMOVE", "KEEP")
  
  x <- data.frame(x, remove)
  return(x)
}, .progress="text")

# Keep only rows that we want to keep:
recruitment <- recruitment[(recruitment$remove == "KEEP"),]

# get rid of "remove" column:
recruitment <- recruitment[,which(names(recruitment)!= "remove")]

# Adding region, site, mooring, mr, collection, and sampling interval to the new df (for recruitment rates):
temp<-data[,c("region","site","mooring","mr", "collection")]
moorings<-data.frame(mooring=unique(temp$mooring))
moorings<-join(moorings,temp, match="first")
recruitment <- join(recruitment, moorings, by="mooring")

# Adding date and year:
recruitment$date<- as.Date(as.character(recruitment$day), format = "%y%m%d")
recruitment$year<-format(recruitment$date, format = "%Y")

# Add intervals:
temp<-data[,-c(1,3,4,5,6,7,8,9, 11)] #creates a dataframe with only sampling interval, date, region (latter 2 kept to join with recruitment table)
temp <- unique(temp)

#convert temp$day and recruitment$day to character
temp$day <- as.character(temp$day)
recruitment$day <- as.character(recruitment$day)
recruitment <- join(recruitment, temp, by=c("day", "region"))

# Adding recruitment rate:
recruitment$rate <- recruitment$count/recruitment$interval

# Add day of year date:
recruitment$julian <- strptime(recruitment$date,"%Y-%m-%d")$yday+1

# Center the sampling day in the smpling interval:
recruitment$julian <- recruitment$julian - round((recruitment$interval)/2) #makes it so that each recruitment rate "came from" the middle of the sampling interval, assuming fish come in throughout the 2 weeks

# Add mooring distances from the most southern mooring position (south to north in each region): 
recruitment$distance <- ifelse (recruitment$mooring == "CF03", 5599, 
                        ifelse (recruitment$mooring == "CF06", 5094, 
                        ifelse (recruitment$mooring == "CF04", 4671,
                        ifelse (recruitment$mooring == "CF01", 4070, 
                        ifelse (recruitment$mooring == "CF05", 3690,
                        ifelse (recruitment$mooring == "OR06", 2376, 
                        ifelse (recruitment$mooring == "OR02", 1870,
                        ifelse (recruitment$mooring == "OR01", 1219,
                        ifelse (recruitment$mooring == "OR05", 795,
                        ifelse (recruitment$mooring == "OR04", 728, 
                        ifelse (recruitment$mooring == "RR01", 6287, 
                        ifelse (recruitment$mooring == "RR02", 5175, 
                        ifelse (recruitment$mooring == "RR03", 4719,
                        ifelse (recruitment$mooring == "RR04", 4392, 
                        ifelse (recruitment$mooring == "HH01", 1659,
                        ifelse (recruitment$mooring == "HH02", 998, 
                        ifelse (recruitment$mooring == "HH03", 472, 
                        0) # counts for OR07 and HH04 
                        ))))))))))))))))

# Add a column wih the number of moorings in each sampling interval:
temp <- ddply(data,c("day", "region"), summarise, n.moorings = length(unique(mooring))) #to get recruitment rate for each region
recruitment <- join(recruitment, temp, by=c("day", "region"))

# Outfile to a .csv table:
write.table(recruitment, "Recruitment2024.csv", 
            append = FALSE, quote = FALSE, 
            sep = "\t", row.names = FALSE, 
            col.names = TRUE)
