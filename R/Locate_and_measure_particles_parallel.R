#' Function to extract morphological measurements and X- and Y-coordinates for moving particles, in a wrapper for parallelized analyses
#' 
#' Function calls ImageJ software and its ParticleAnalyzer function to extract for each frame of the video
#' several morphological descriptors and the X- and Y-coordinates of all moving particles. All videos in the raw.video.folder are analyses, separately.
#' To parallellize this process, we create temporary folders cotaining the subset of videos to be analyzed, as well as containing a temporary
#' folder with an ImageJ/Java copy
#' @param process_ID numeric variable, containing the identifier for the parallel process
#' @param to.data path to the working directory
#' @param raw.avi.folder directory with the compressed avi files 
#' @param particle.data.folder directory to which the data is saved as a text file
#' @param difference.lag numeric value specifying the offset between two video frames to 
#' compute the difference image. If 0, then no differencing applied.
#' @param min_size minimum size for detection of particles
#' @param max_size maximum size for detection of particles
#' @param thresholds vector containing the min and max threshold values (defaults to c(10,255))
#' @param tools.path Path containing the bemovi dependencies
#' @param memory numeric value specifying the amount of memory available to ImageJ
#' @param video.files.df Dataframe containing the video files in the raw data, as well as the process ID for the parallel analysis
#' @return saves the output of the ParticleAnalyzer function of ImageJ as a text file in the output directory and then assembles the data into a single database 
#' called 'particle.RData'. This data.frame contains information about the following properties: the area (transversal cut), 
#' the mean, minimum and maximum of the grey value, the perimeter, width, length and angle with the dominant-axis of a fitted ellipse, and finally shape parameters such as
#' circularity, aspect ratio, roundness and solidity. For details of the morphological output, please refer to http://rsbweb.nih.gov/ij/docs/guide/146-30.html 
#' @import parallel 
#' @export 

parallel_locate_and_measure <- function(process_ID, to.data, raw.avi.folder, particle.data.folder, difference.lag, min_size, max_size, 
                                        thresholds, tools.path, memory, video.files.df){
  
  video.dir <- paste0(to.data, raw.avi.folder)
  
  #Filter the df to get a list of the videos that will be analyzed by this processor core
  video.files.filt <- as.character(video.files.df[which(video.files.df$process==process_ID), 1])
  
  #Make a temporary folder and move the videos there
  temp.dir <- paste0(video.dir, process_ID, sep="/")
  dir.create(temp.dir, showWarnings = F)
  if (.Platform$OS.type == "unix") {
    system(paste("mv -t", temp.dir, paste(unlist(video.files.filt), collapse=' ')))
  }
    
  ## copy master copy of ImageJ macro there for treatment
  ## if there is differencing (i.e., difference.lag>0)
  if(difference.lag>0)
    text <- readLines(paste0(system.file(package="bemovi"), "/", "ImageJ_macros/Video_to_morphology.ijm"))
  ## if there is no differencing (i.e., difference.lag==0)
  if(difference.lag==0)
    text <- readLines(paste0(system.file(package="bemovi"), "/", "ImageJ_macros/Video_to_morphology_no_differencing.ijm"))
  
  ## use regular expression to insert input & output directory as well as difference lag
  text[grep("video_input = ", text)] <- paste("video_input = ", "'", temp.dir, "';", sep = "")
  text[grep("video_output = ", text)] <- paste("video_output = ", "'", to.data, particle.data.folder, "';", sep = "")
  text[grep("lag = ", text)] <- paste("lag = ", difference.lag, ";", sep = "")
  text[grep("setThreshold", text)] <- paste("setThreshold(", thresholds[1], ",", thresholds[2], ");", sep = "")
  text[grep("size=", text)] <- paste('run("Analyze Particles...", "size=',min_size,'-',max_size,' circularity=0.00-1.00 show=Nothing clear stack");',sep = "")
  
  ## re-create ImageJ macro for batch processing of video files with Particle Analyzer
  if (.Platform$OS.type == "unix") {
    ijmacs.file <- paste0(to.data, ijmacs.folder, "Video_to_morphology_tmp", process_ID, ".ijm", sep = "")
    dir.create(paste0(to.data, ijmacs.folder), showWarnings = F)
    writeLines(text, con = ijmacs.file)}
  
  ##Create temporary copy of the imageJ folder
  if (.Platform$OS.type == "unix") {
    IJ.temp.folder <- paste0(to.data, "ImageJ", process_ID, "/")
    system(paste("cp -R ", tools.path, "/Fiji.app ", IJ.temp.folder, sep=""))
    IJ.path <- paste0(IJ.temp.folder, "/ImageJ-linux64")
    fiji.path <- paste0(IJ.temp.folder, "/jars/ij-1.52p.jar")
    
    }
    
  #Call particle analyzer
  if (.Platform$OS.type == "unix") {
    cmd <- paste0("java -Xmx", memory, "m -jar ", fiji.path, " -ijpath ", IJ.path, " -macro ","'", 
                  ijmacs.file, "'")}
  system(cmd)
  
  #Move videos back to original folder and delete temporary folder
  temp.files <- list.files(path = temp.dir, pattern = paste("\\.avi", sep=""))
  temp.files <- paste(temp.dir, temp.files, sep="/")
  if (.Platform$OS.type == "unix") {
    system(paste("mv -t", video.dir, paste(unlist(temp.files), collapse=' ')))
    system(paste("rm -r ", temp.dir))
    system(paste("rm -r ", IJ.temp.folder))
  }
}
    
  
