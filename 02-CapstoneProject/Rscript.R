###--------------------------------------------------------------------------###
### DOWNLOAD DATASET --------------------------------------------------------###
###--------------------------------------------------------------------------###

dl <- "./data/original.zip"
if(!file.exists(dl))
  download.file("https://github.com/marc-romu/Data-Science-Capstone-HarvardX/raw/main/02-CapstoneProject/data/original.zip", dl, cacheOK=FALSE)

unzip(dl,exdir="./data/")

file.remove(dl)

###--------------------------------------------------------------------------###
### VARIABLES ---------------------------------------------------------------###
###--------------------------------------------------------------------------###

# DEFAULTS
if(!exists("facade")) { facade <- "West" }

if(!exists("img_max_width")) { img_max_width <- 1000 }

if(!exists("img_window1_crop")) { img_window1_crop <- "10x33+15+54" }
if(!exists("img_windowN_crop")) { img_windowN_crop <- "12x34+375+212" }

if(!exists("window_quantity_x")) { window_quantity_x <- 28 }
if(!exists("window_quantity_y")) { window_quantity_y <- 6 }

if(!exists("window_width")) { window_width <- 12 }
if(!exists("window_height")) { window_height <- 33 }


# FOLDERS
folder_data <- "./data"
folder_data_original_images <- paste( folder_data ,
                                      "original" ,
                                      sep="/")
folder_data_makeup_images <- paste( folder_data ,
                                    "makeup" ,
                                    sep="/")
folder_data_windows_images <- paste( folder_data ,
                                     "windows" ,
                                     sep="/")


###--------------------------------------------------------------------------###
### INSTALL AND LOAD REQUIRED LIBRARIES -------------------------------------###
###--------------------------------------------------------------------------###

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library("tidyverse")

if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")
library("foreach")

if(!require(magick)) install.packages("magick", repos = "http://cran.us.r-project.org")
library("magick")

if(!require(magickGUI)) install.packages("magickGUI", repos = "http://cran.us.r-project.org")
library("magickGUI")

if(!require(exifr)) install.packages("exifr", repos = "http://cran.us.r-project.org")
library("exifr")

if(!require(png)) install.packages("png", repos = "http://cran.us.r-project.org")
library("png")

if(!require(broman)) install.packages("broman", repos = "http://cran.us.r-project.org")
library("broman")

if(!require(stats)) install.packages("stats", repos = "http://cran.us.r-project.org")
library("stats")

if(!require(useful)) install.packages("useful", repos = "http://cran.us.r-project.org")
library("useful")

if(!require(filesstrings)) install.packages("filesstrings", repos = "http://cran.us.r-project.org")
library("filesstrings")

if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
library("cluster")

if(!require(fpc)) install.packages("fpc", repos = "http://cran.us.r-project.org")
library("fpc")

if(!require(mclust)) install.packages("mclust", repos = "http://cran.us.r-project.org")
library("mclust")


###--------------------------------------------------------------------------###
### FUNCTIONS ---------------------------------------------------------------###
###--------------------------------------------------------------------------###

# Prompt for a "question" and store the answer in the variable prompt
interactive_prompt <- function(question,default=""){
  
  if(interactive()) {
    
    suppressWarnings( if(exists("prompt_answer")) { rm(prompt_answer) } )
    response <- readline(question)
    if(response == "") { prompt_answer <<- default }
    else { prompt_answer <<- response }
    
  }
  
}

# Create the window polygon by center, width and height
windowPolygon <- function(center,width,height){
  
  center_x <- center[1]
  center_y <- center[2]
  
  left <- center_x-width/2
  bottom <- center_y-height/2
  right <- center_x+width/2
  top <- center_y+height/2
  
  polygon(c(left,right,right,left), c(top,top,bottom,bottom), border = "red", lwd = 2)
  
}



###--------------------------------------------------------------------------###
### 1. IMAGE PROCESSING -----------------------------------------------------###
###--------------------------------------------------------------------------###

# CREATE A MATRIX OF WINDOWS
# --------------------------
# Generate the matrix of windows

# Extract data from the cropped images
window_center <- c( round( window_width/2, digits = 0),
                    round( window_height/2, digits = 0) )

# Get the coordinates for the center of the first window
window1_topleft_x <- img_window1_crop %>% strsplit("\\+") %>% .[[1]] %>% .[2] %>% as.numeric()
window1_topleft_y <- img_window1_crop %>% strsplit("\\+") %>% .[[1]] %>% .[3] %>% as.numeric()

window1_center <- window_center + c( window1_topleft_x , window1_topleft_y )

# Get the coordinates for the center of the last window
windowN_topleft_x <- img_windowN_crop %>% strsplit("\\+") %>% .[[1]] %>% .[2] %>% as.numeric()
windowN_topleft_y <- img_windowN_crop %>% strsplit("\\+") %>% .[[1]] %>% .[3] %>% as.numeric()

windowN_center <- window_center + c( windowN_topleft_x , windowN_topleft_y )

# Calculate the interval between windows
window_interval <- c( ( windowN_center[1] - window1_center[1] ) / ( window_quantity_x - 1 ),
                      ( windowN_center[2] - window1_center[2] ) / ( window_quantity_y - 1 ) )

# Generate the matrix of coordinates for window centers
windows <- expand.grid( round( seq( window1_center[1] , windowN_center[1] , window_interval[1] ) ) ,
                        round( seq( window1_center[2] , windowN_center[2] , window_interval[2] ) )
)

colnames(windows) <- c("center_x","center_y")

# Assign each window a name
names <- expand.grid( 1:window_quantity_x , window_quantity_y:1 )

colnames(names) <- c("X","Y")

names <- names %>% mutate(N = paste(substr(facade,0,1),
                                    Y,
                                    ifelse(X >= 10, X, paste("0",X,sep=""))
                                    ,sep="")
)

windows <- windows %>% mutate(Name = names$N)

# Reorder the columns to put the name first
windows <- windows %>% select(Name, everything())

# Clean up
rm(names)

# Delete non-real windows
windows_to_delete <- c("0")

if( facade == "West" ) {
  windows_to_delete <- c("W101","W102","W103","W104","W105")
}

windows <- windows %>% subset(!(Name %in% windows_to_delete))
row.names(windows) <- 1:nrow(windows)



# EDIT IMAGE LEVELS
# -----------------
# Modify the contrast, brightness and hue to enhance the windows

# Get all the images
f <- list.files( folder_data_original_images , full.names = TRUE)

if ( !file.exists( folder_data_makeup_images ) ){
  dir.create( folder_data_makeup_images )
}

# Make up the images
foreach( i=1:length(f) ) %do% {
  
  # Read the image
  img_temp <- f[i] %>% image_read()
  
  # Get info from the image
  img_datetime <- read_exif(f[i])$DateTimeOriginal
  
  img_datetime <- format(as.POSIXct( img_datetime, format = "%Y:%m:%d %H:%M:%S", tz = "UTC" ),
                         "%Y%m%d-%H%M")
  
  img_lens_exposure <- as.numeric( read_exif(f[i])$ExposureTime )
  
  img_lens_iso <- as.numeric( read_exif(f[i])$ISO )
  
  img_lens_aperture <- as.numeric( read_exif(f[i])$ApertureValue )
  
  # Normalize
  img_temp <- img_temp %>% image_normalize()
  
  # Modulate
  img_temp <- img_temp %>% image_modulate( brightness = 200 ,
                                           saturation = 300 ,
                                           hue = 90 )
  
  # Normalize
  img_temp <- img_temp %>% image_normalize()
  
  # Save the image
  image_write(img_temp, path = paste( folder_data_makeup_images ,
                                      "/" ,
                                      img_datetime ,
                                      "_" ,
                                      facade ,
                                      "_makeup" ,
                                      ".png" ,
                                      sep=""), format = "png")
  
}


###--------------------------------------------------------------------------###
### 2. CROP EVERY WINDOW FROM EVERY IMAGE -----------------------------------###
###--------------------------------------------------------------------------###

# Create a new column to the windows data-set with the crop instructions
windows <- windows %>% mutate(crop = paste( window_width ,
                                            "x" ,
                                            window_height ,
                                            "+" ,
                                            round( center_x - window_width/2 , digits = 0 ) ,
                                            "+" ,
                                            round( center_y - window_height/2 ) ,
                                            sep="")
)

# Check if sub directory exists, otherwise create it
if ( !file.exists( folder_data_windows_images ) ){
  dir.create( folder_data_windows_images )
}

# Get all images from makeup folder
f <- list.files( folder_data_makeup_images , full.names = TRUE)

# Crop all windows for every image and save them
foreach( i=1:length(f) ) %do% {
  
  # Read the image
  img_temp <- f[i] %>% image_read()
  
  # Get info from the file
  img_datetime <- f[i] %>% str_split("/") %>% as_vector() %>% .[4] %>% str_split("_") %>% as_vector() %>% .[1]

  img_path = paste( folder_data_windows_images ,
                    "/" ,
                    img_datetime ,
                    "_" ,
                    sep="")
  
  foreach( j=1:nrow( windows ) ) %do% {
    
    img_temp2 <- img_temp %>% image_crop( geometry = windows$crop[j] )
    
    image_write( img_temp2 , path = paste( img_path ,
                                           windows$Name[j] ,
                                           ".png" ,
                                           sep=""), format = "png")
    
  }
  
}



###--------------------------------------------------------------------------###
### 3. DEFINING THE FEATURES FOR MACHINE LEARNING ---------------------------###
###--------------------------------------------------------------------------###

# GETTING DATA FROM EVERY PIXEL
# -----------------------------
# The pixels of the images are the features for the machine learning process

# Empty previous data if you run the script earlier
if( exists( "windows_features" ) ) { rm(windows_features) }

# Get all the images
f <- list.files( folder_data_windows_images , full.names = TRUE)

foreach( i=1:length(f) ) %do% {
  
  # Get the filename
  filename <- str_split( f[i] , "/" )
  filename <- as.vector(filename[[1]])
  filename <- str_split( filename[length(filename)] , ".png" )
  filename <- filename[[1]][1]
  
  # Read the image
  img_temp <- f[i] %>% image_read()
  
  dat <- img_temp %>% image_data()
  
  # Take only the red channel (nº 1)
  dat <- dat[1,,] %>% as.data.frame()
  
  # Convert hex to dec
  dat <- dat %>% mutate_all(hex2dec)
  
  # Transpose the data
  dat <- dat %>% t()
  
  # Rotate data 90º
  dat <- t( apply(dat, 2, rev) )
  
  # Rename columns
  colnames( dat ) <- 1:window_height
  
  ### DEBUG
  # # Plot the image
  # image( as.matrix( dat ) , col = gray.colors( 256 , start = 0, end = 1) , breaks = 0:256 )
  # # Plot values on image
  # hist(as.matrix( dat ))
  
  # As the position of the roller shutter is the only objective feature, and
  # this is horizontal, let's sum up the rows to reduce the number of features.
  dat <- round( colSums( dat ) / window_width , digits = 0 )
  
  ### DEBUG
  # # Transpose the data
  # dat <- dat %>% as.tibble()
  # dat <- dat %>% t()
  # # Plot the image
  # image( as.matrix( dat ) , col = gray.colors( 256 , start = 0, end = 1) , breaks = 0:256 )
  
  ### DEBUG
  # # Convert matrix to tibble row
  # dat <- as.vector( dat )
  
  ### DEBUG
  # # Convert vector to matrix (way back)
  # dat <- matrix( dat , ncol = window_height )
  
  dat <- as_tibble_row( setNames( dat , paste( "X",1:length(dat),sep="" ) ) )
  
  # Add ID column
  dat <- dat %>% add_column( ID = filename , .before = TRUE)
  
  # Save data to windows_features dataset
  if( !exists( "windows_features" ) ) { windows_features <- dat }
  else { windows_features <- windows_features %>% add_row( dat ) }
  
  # Clean up
  rm(dat)
  
}

# Clean up
rm(f)


###--------------------------------------------------------------------------###
### 4. CLUSTERING USING THE KMEANS ALGORITHM --------------------------------###
###--------------------------------------------------------------------------###

features <- windows_features %>% select(-ID)

# Optimize the number of clusters based on the Hartigan’s rule
clusters_fit <- FitKMeans( features , max.clusters=100 , nstart=25 , seed=45357452 , iter.max = 10 )

# DEBUG
# PlotHartigan(clusters_fit)

clusters_quantity <- clusters_fit$Clusters[ which( clusters_fit$AddCluster == FALSE )[1] ]

if( is.na( clusters_quantity ) ) {
  stop("The optimization of the number of clusters failed.\n")
}

# Cluster images using the kmeans method
km <- kmeans( features , centers = clusters_quantity , nstart = 25 , iter.max = 100000 )

# Add cluster ID to the windows_features table
windows_features <- windows_features %>% add_column( clusterKM = km[["cluster"]] )

# DEBUG
# plot(km, data=features)



###--------------------------------------------------------------------------###
### 5. CLUSTERING USING THE MODEL BASED APPROACH ----------------------------###
###--------------------------------------------------------------------------###

features <- windows_features %>% select(-ID)

# Model Based clustering. Same quantity of clusters to be able to compare it with the previous method.
mb <- Mclust( features , G = clusters_quantity )

# Add cluster ID to the windows_features table
# windows_features <- windows_features %>% select(-clusterMB)
windows_features <- windows_features %>% add_column( clusterMB = mb[["classification"]] )

# plot(mb) # plot results
# summary(mb) # display the best model 


###--------------------------------------------------------------------------###
### 6. COMPARING TWO CLUSTERING SOLUTIONS -----------------------------------###
###--------------------------------------------------------------------------###

# Create a data frame for the analysis data by cluster
analysis <- data.frame( "cluster" = 1:clusters_quantity )

# Get the size of each cluster
## First, create a function to do so
count_clusters <- function( value , vector ){
  return ( sum( vector == value ) )
}

## Second, apply the function to every cluster for the KM method
l <- sapply(1:clusters_quantity, function( value ){
  count_clusters( value , windows_features$clusterKM )
})

## Store the data
analysis <- analysis %>% mutate( "KMsize" = l )

## Third, apply the function to every cluster for the MB method
l <- sapply(1:clusters_quantity, function( value ){
  count_clusters( value , windows_features$clusterMB )
})

## Store the data. It will be used in the report.
analysis <- analysis %>% mutate( "MBsize" = l )

###--------------------------------------------------------------------------###
### END ---------------------------------------------------------------------###
###--------------------------------------------------------------------------###

# Save Rdata file to generate the Report
save.image("./Rdata.RData")
