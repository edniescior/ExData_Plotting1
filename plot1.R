##
# Generate Plot 1: Histogram of Global Active Power
#
# Note: The raw data file must be unzipped in the following location relative
#       to the current working directory: data/household_power_consumption.txt
#


##
# Reads in the data file and filters out all but the dates we are interested in.
# Dates are passed in as strings in YYYY-MM-DD format. Converts the time field
# to a complete timestamp to include the date and time.
#
read_file <- function(file, dates) {
    # pass in the dates we are interested in YYYY-MM-DD format
    filterDates <- as.Date(dates, "%Y-%m-%d")
    
    m <- read.csv(file, header=TRUE, sep=";", blank.lines.skip=TRUE, na.strings=c("?"))
    m$Date <- as.Date(m$Date, "%d/%m/%Y")
    
    # filter out all but the dates that we are interested in
    m <- m[m$Date %in% filterDates, ]
    
    # convert the time field to a true timestamp including the date
    m$Time <- strptime(paste(m$Date, m$Time, sep = " "), format="%Y-%m-%d %H:%M:%S")
    
    m
}

##
# Create the plot
#
the_plot <- function(dat) {
    hist(dat$Global_active_power,
         col  = "red",
         xlab = "Global Active Power (kilowatts)",
         ylab = "Frequency",
         main = "Global Active Power")
}

# Read in the data
dat <- read_file(file = "data/household_power_consumption.txt", dates = c("2007-02-01", "2007-02-02"))

# Generate the plot to file
png(file   = "plot1.png",
    width  = 480,
    height = 480,
    units  = "px")
the_plot(dat)
dev.off()