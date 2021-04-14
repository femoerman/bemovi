#' Function to extract morphological measurements and X- and Y-coordinates for moving particles 
#' 
#' Function calls ImageJ software and its ParticleAnalyzer function to extract for each frame of the video
#' several morphological descriptors and the X- and Y-coordinates of all moving particles. All videos in the raw.video.folder are analyses, separately.
#' @param to.data path to the working directory
#' @param raw.video.folder directory with the raw video files 
#' @param particle.data.folder directory to which the data is saved as a text file
#' @param difference.lag numeric value specifying the offset between two video frames to 
#' compute the difference image. If 0, then no differencing applied.
#' @param min_size minimum size for detection of particles
#' @param max_size maximum size for detection of particles
#' @param thresholds vector containing the min and max threshold values (defaults to c(10,255))
#' @param IJ.path path to ImageJ folder, containing the 'ij.jar' executable
#' @param memory numeric value specifying the amount of memory available to ImageJ (defaults to 512)
#' @param memory.per.identifier numeric value specifying the amount of memory to reserve for every imageJ instance (defaults to 10000)
#' @param max.cores numeric value, specifying the maximum number of cores to use for particle identification (defaults to # of machine cores - 1)
#' @return saves the output of the ParticleAnalyzer function of ImageJ as a text file in the output directory and then assembles the data into a single database 
#' called 'particle.RData'. This data.frame contains information about the following properties: the area (transversal cut), 
#' the mean, minimum and maximum of the grey value, the perimeter, width, length and angle with the dominant-axis of a fitted ellipse, and finally shape parameters such as
#' circularity, aspect ratio, roundness and solidity. For details of the morphological output, please refer to http://rsbweb.nih.gov/ij/docs/guide/146-30.html 
#' @export 

locate_and_measure_particles <- function(to.data, raw.video.folder, particle.data.folder, difference.lag, min_size=0, max_size=10000, 
                                         thresholds = c(10, 255), IJ.path, memory = 512, memory.per.identifier=10000, max.cores=detectCores()-1) {
  
  #ijmacs.folder<-NULL
  
  #Define the video folder
  video.dir <- paste(to.data, raw.video.folder, sep = "")
  
  ##Determine the maximum number of processess that can be run in parallel
  #Filter the df to get a list of the videos that will be analyzed by this processor core
  video.files.filt <- as.character(video.files.df[which(video.files.df$process==process_ID), "video.files"])
  #Ensure that the assigned number of cores is not greater than the number of cores of the machine - 1
  max.cores <- min(ifelse(max.cores==0, detectCores()-1, max.cores), detectCores()-1)
  #Make sure enough memory (10000 Mb) is reserved for every parallel process
  processes <- max(min(max.cores, memory %/% memory_per_identifier), 1)
  #Ensure that every core needs to analyze at least 5 videos (otherwise parallellization does not yield enough time benefit)
  video.files <- list.files(path = video.dir, pattern = paste("\\.", video.format, sep=""))
  min(processes, floor(length(as.character(video.files.df[which(video.files.df$process==process_ID), "video.files"]))/5))
  
  #In Windows, or if only limited computational power, use the unparallellized version of the analysis
  if(.Platform$OS.type == "windows" | processes == 1) {
    ## copy master copy of ImageJ macro there for treatment
    ## if there is differencing (i.e., difference.lag>0)
    if(difference.lag>0)
      text <- readLines(paste0(system.file(package="bemovi"), "/", "ImageJ_macros/Video_to_morphology.ijm"))
    ## if there is no differencing (i.e., difference.lag==0)
    if(difference.lag==0)
      text <- readLines(paste0(system.file(package="bemovi"), "/", "ImageJ_macros/Video_to_morphology_no_differencing.ijm"))
    
    ## use regular expression to insert input & output directory as well as difference lag
    text[grep("video_input = ", text)] <- paste("video_input = ", "'", video.dir, "';", sep = "")
    text[grep("video_output = ", text)] <- paste("video_output = ", "'", to.data, particle.data.folder, "';", sep = "")
    text[grep("lag = ", text)] <- paste("lag = ", difference.lag, ";", sep = "")
    text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep = "")
    text[grep("size=", text)] <- paste('run("Analyze Particles...", "size=',min_size,'-',max_size,' circularity=0.00-1.00 show=Nothing clear stack");',sep = "")
    
    ## re-create ImageJ macro for batch processing of video files with Particle Analyzer
    if (.Platform$OS.type == "windows") {
      dir.create(paste0(to.data, ijmacs.folder), showWarnings = F)
      
      writeLines(text, con = paste(to.data,ijmacs.folder,"Video_to_morphology_tmp.ijm", sep = ""))}
    if (.Platform$OS.type == "unix") {
      dir.create(paste0(to.data, ijmacs.folder), showWarnings = F)
      writeLines(text, con = paste0(to.data, ijmacs.folder, "Video_to_morphology_tmp.ijm"))}
    
    ## create directory to store Particle Analyzer data
    dir.create(paste0(to.data, particle.data.folder), showWarnings = FALSE)
    
    ## run to process video files by calling ImageJ
    if (.Platform$OS.type == "unix") 
      cmd <- paste0("java -Xmx", memory, "m -jar ", IJ.path, "/ij.jar", " -ijpath ", IJ.path, " -macro ","'", 
                    to.data, ijmacs.folder, "Video_to_morphology_tmp.ijm'")
    if (.Platform$OS.type == "windows")
      cmd <- paste0("\"", IJ.path,"\"", " -macro ","\"", paste0(gsub("/", "\\\\", paste0(to.data, ijmacs.folder))), "Video_to_morphology_tmp.ijm", "\"")
    
    error.log <- system(cmd, intern = T)
  } else {
    #Otherwise, perform the parallellized analysis
    
    #Make a dataframe containing the full video files
    video.files <- paste(video.dir, video.files, sep="/")
    video.files.df <- as.data.frame(video.files)
    
    #Divide the videos between the processors that can be used
    video.files.df$process <- ceiling(seq(from=0.001, to=processes, length=nrow(video.files.df)))
    
    #Run parallel processes for particle identification
    error.log <- mclapply(1:processes, parallel_locate_and_measure, to.data, raw.video.folder, particle.data.folder, difference.lag, min_size, max_size, 
                          thresholds, IJ.path, memory,  memory_per_identifier, video.files.df, video.dir, mc.cores = processes)
  }
  
  organise_particle_data(to.data, particle.data.folder)
  
}