#-----------------------------
#  Exploratory Data Analysis - Course Project 1
#  PLOT 3
#  Author: Arturo Equihua
#  Date : March 8th 2015
#---------------------------------

## First plot from the electric power consumption dataset 
get_tidy <- function(infile) {
  # Obtain the datafile and transform it into the
  # required dataframe for easier manipulation
  
  # Read file with ; as separator, and ? as NA indicator
  rawDF <- read.table(infile, header=TRUE, sep=";", na.strings="?", as.is=TRUE )
  
  # Take data from the dates 2007-02-01 and 2007-02-02. 
  fechas <- paste(rawDF$Date,rawDF$Time)
  fechas <- strptime(fechas,"%d/%m/%Y %H:%M:%S")
  rawDF <- cbind(fechas,rawDF)
  tidyDF <- rawDF[rawDF$Date %in% c("1/2/2007","2/2/2007"),]
  
  tidyDF
}


plot3 <- function(infile, outfile) {
  start_time = Sys.time()  
  writeLines("PLOT 3")
  writeLines("Reading file...")
  
  # Invoke the function that reads the data and generate the plot
  datos <- get_tidy(infile)
  
  # Generate timeseries plot using Base package functions, save it to the outfile PNG file
  png(outfile)
  
  plot(datos$Sub_metering_1 ~ datos$fechas,type="l",xlab="",ylab="Energy sub metering")
  lines(datos$Sub_metering_2 ~ datos$fechas,col="red")
  lines(datos$Sub_metering_3 ~ datos$fechas,col="blue")
  legend('topright',legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col=c("black","red","blue"),lty=c(1,1,1))

  
  dev.off()
  
  elapsed_time <- Sys.time() - start_time  
  writeLines("Done.")
  writeLines(paste("Elapsed time:", elapsed_time, "seconds."))  
}

# plot3("./household_power_consumption.txt","./plot3.PNG")
