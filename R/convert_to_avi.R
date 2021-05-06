#' Function to convert the video files to .avi format using lossless conversion
#' 
#' Function uses bftools to convert cxd files and ffmpeg to convert other files and compress to lossles avi
#' @param to.data path to the working directory
#' @param raw.video.folder directory with the raw video files 
#' @param raw.avi.folder directory to store the converted avi files 
#' @param metadata.folder directory to store the metadata from the .cxd files
#' @param tools.path directory where the tools for bemovi (fiji, ffmpeg and bftools) are stored
#' @param fps framerate of the videos
#' @param video.format extension of the video files (e.g. cxd, avi, mp4, mov)
#' @return returns nothing (NULL)
#' @export 

convert_to_avi <- function(to.data, raw.video.folder, raw.avi.folder, metadata.folder, tools.path, fps, video.format){
  
  #Define input.folder
  input.folder <- paste0(to.data, raw.video.folder)
  
  #List all video files 
  video.files <- list.files(input.folder, full.names = T)
  
  #Define and make the folder for the converted video files
  output.folder <- paste0(to.data, raw.avi.folder)
  dir.create(output.folder, showWarnings = T)
  
  #Make the additional folders if one starts from .cxd files
  if(video.format=="cxd"){
    #Create a folder for the temporary files (converted but not yet compressed)
    temp.folder <- paste0(to.data, "temp.avi.folder/")
    dir.create(temp.folder, showWarnings = F)
    
    #Create a folder for the stored metadata folder
    meta.folder <- paste0(to.data, metadata.folder)
    dir.create(meta.folder, showWarnings = F)
  }
  
  #Define the paths to ffmpeg and bftools
  ffmpeg <- paste0(tools.path, "ffmpeg/ffmpeg")
  bfconvert <- paste0(tools.path, "/bftools/bfconvert")
  showinf <- paste0(tools.path, "/bftools/showinf")
  
  #Perform the conversion and compression
  
  for (filename in video.files){
    output.file <- unlist(strsplit(filename, "/")[[1]])
    tempfile <- paste0(temp.folder, "/", tools::file_path_sans_ext(output.file[length(output.file)]), ".avi")
    metadata <- paste0(meta.folder, "/", tools::file_path_sans_ext(output.file[length(output.file)]), ".txt")
    output.file <- paste0(output.folder, "/", tools::file_path_sans_ext(output.file[length(output.file)]), ".avi")
      
    if(video.format=="cxd"){
      #Create a system command to convert the video using bioformats
      arguments <- paste0( " -overwrite -no-upgrade ", filename, " ", tempfile)
      message("Converting ", filename)
      system2(command=bfconvert, args = arguments, stdout = NULL)
    }
      
    #If one use cxd, first convert to avi using bioformats package, then compress using ffmpeg
    if(video.format=="cxd"){
      
      #Create and run a system command to compress the video and enhance contrast
      arguments <- paste0(" -y -r ", fps, " -i ", tempfile, " -vcodec png -compression_level 10 -vtag 'PNG ' ", output.file)
      message("Compressing ", tempfile)
      system2(command=ffmpeg, args = arguments, stdout = NULL)
      
    } else{
      
      #If one instead uses an other format, just compress the videos to .avi format  directly using ffmpreg, and enhance contrast
      arguments <- paste0(" -y -r ", fps, " -i ", filename, " -vcodec png -compression_level 10 -vtag 'PNG ' ", output.file)
      message("Converting and compressing ", filename)
      system2(command=ffmpeg, args = arguments, stdout = NULL)
    }
    
    if(video.format=="cxd"){
      #Create and run a system command to extract and store the metadata
      arguments <- paste0( " -nopix -no-upgrade ",  filename)
      message("Extracting metadata ", filename)
      system2(command=showinf, args = arguments, stdout = metadata)
      
    }
      
    
      
    }
  
  

  #Remove the temporary folder if one starts from .cxd files
  if(video.format=="cxd"){
    unlink(temp.folder, recursive=TRUE)
  }
  return(NULL)
}
