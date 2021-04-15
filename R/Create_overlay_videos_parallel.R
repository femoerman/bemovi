#' Parallellized version of the function to create a new video with the extracted trajectories (filtered or raw) overlayed onto the original video
#' 
#' A function to parallelize the overlay generation
#' 
#' @param process_ID numeric variable, containing the identifier for the parallel process
#' @param video.files.df Dataframe containing the video files in the raw data, as well as the process ID for the parallel analysis
#' @param traj.data dataframe with the information on morphology and movement for each frame (either "trajectoty.data" or "traj.data.filtered")
#' @param to.data path to the working directory
#' @param merged.data.folder directory where the global database is saved
#' @param raw.video.folder directory with the raw video files 
#' @param temp.overlay.folder temporary directory to save the overlay created with R
#' @param overlay.folder directory where the overlay videos are saved
#' @param width width of the raw video
#' @param height height of the raw video
#' @param difference.lag numeric value specifying the offset between two video frames to compute the difference image
#' @param type string indicating the visualization type (i.e. 'label' or 'traj'): either the overlay
#' is showing the trajectory ID and outlines the detected particle (type='label') or the whole trajectory
#' remains plotted (type='traj').
#' @param predict_spec logical If TRUE, the Master.RData file must have a column called predict_spec, indicating the species to which the trajectory belongs
#' @param contrast.enhancement numeric value to increase the contrast of the original video
#' @param IJ.path path to ImageJ folder, containing the 'ij.jar' executable
#' @param memory numeric value specifying the amount of memory available to ImageJ (defaults to 512)
#' @export

create_overlays_parallel <- function(process_ID, video.files.df, traj.data, to.data, merged.data.folder, raw.video.folder, temp.overlay.folder, overlay.folder, 
                                     width, height, difference.lag, type = "traj",  predict_spec=F, contrast.enhancement = 0, IJ.path, memory = 512) {
  
  ##Define folder with video files
  video.dir <- paste(to.data, raw.video.folder, sep="/")
  
  #Filter the df to get a list of the videos that will be analyzed by this processor core
  video.files.filt <- as.character(video.files.df[which(video.files.df$process==process_ID), "video.files"])
  videos <- unlist(strsplit(video.files.filt, split = "/"))
  videos <- videos[startsWith(videos, "sample_")]
  videos <- substr(videos,1,nchar(videos)-4)
  traj.data.filt <- traj.data[which(traj.data$file %in% videos), ]
  file_names <- unique(traj.data.filt$file)
  
  ## change path for output
  dir.create(paste0(to.data, temp.overlay.folder), showWarnings = F)
  folder.loc <- paste0(to.data, temp.overlay.folder "/")
  for (i in 1:length(file_names)) {
    dir.create(paste0(folder.loc, file_names[i]), showWarnings = F)
    traj.data_tmp <- subset(traj.data, file == file_names[i])
    j <- 1
    
    if (type == "traj") {
      while (j <= max(traj.data$frame)) {
        jpeg(paste(folder.loc, file_names[i], "/", "frame_", j, ".jpg", sep = ""), width = as.numeric(width), height = as.numeric(height), quality = 100)
        par(mar = rep(0, 4), xaxs = c("i"), yaxs = c("i"))
        
        if (predict_spec==F){
          
          print <- subset(traj.data_tmp, traj.data_tmp$frame <= j, select = c("X", "Y", "trajectory"))
          
          ## plot the particle(s) so long as there are some
          if (length(print[, X]) != 0) {
            plot(print$X, print$Y, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 15, cex = 1, asp = 1)
          }
          
          ## otherwise just plot the empty frame
          if (length(print[, X]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, cex = 6, asp = 1)
          }
        }
        
        if (predict_spec==T){
          
          print <- subset(traj.data_tmp,traj.data_tmp$frame <= j, select=c("X","Y","trajectory","predict_spec"))
          
          ## plot the particle(s) so long as there are some
          if (length(print[, X]) != 0) {
            plot(print$X, print$Y, xlim=c(0, as.numeric(width)), ylim=c(as.numeric(height), 0),  col=as.factor(print$predict_spec), pch=15, cex=1, asp=1)
          }
          
          ## otherwise just plot the empty frame
          if (length(print[, X]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, cex = 1, asp = 1)
          }
        }
        
        dev.off()
        j <- j + 1
      }
    }
    
    if (type == "label") {
      while (j <= max(traj.data$frame)) {
        jpeg(paste(folder.loc, file_names[i], "/", "frame_", 
                   j, ".jpg", sep = ""), width = as.numeric(width), height = as.numeric(height), quality = 100)
        par(mar = rep(0, 4), xaxs = c("i"), yaxs = c("i"))
        
        if (predict_spec==F){
          
          print <- subset(traj.data_tmp, traj.data_tmp$frame == j, select = c("X", "Y", "trajectory"))
          
          ## plot the particle(s) so long as there are some
          if (length(print[, X, ]) != 0) {
            plot(print$X, print$Y, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, cex = 6, asp = 1)
            text(print$X, print$Y - 20, print$trajectory, cex = 2, col = "red")
          }
          
          ## otherwise just plot the empty frame
          if (length(print[, X,]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, cex = 6, asp = 1)
          }
        }
        
        if (predict_spec==T){
          
          print <- subset(traj.data_tmp,traj.data_tmp$frame == j, select=c("X","Y","trajectory","predict_spec"))
          
          ## plot the particle(s) so long as there are some
          if (length(print[, X, ]) != 0) {
            plot(print$X, print$Y, xlim=c(0,as.numeric(width)), ylim=c(as.numeric(height), 0), col=as.factor(print$predict_spec), pch=1, cex=6, asp=1)
            text(print$X, print$Y-20,print$trajectory,cex=2,col=as.numeric(print$predict_spec))
          }
          
          ## otherwise just plot the empty frame
          if (length(print[, X, ]) == 0) {
            plot(NA, NA, xlim = c(0, as.numeric(width)), ylim = c(as.numeric(height), 0), col = "blue", pch = 1, 
                 cex = 6, asp = 1)
          }
        }
        
        dev.off()
        j <- j + 1
      }
    }
  }
}
