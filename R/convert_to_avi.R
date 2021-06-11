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

convert_to_avi <- function(to.data, raw.video.folder, raw.avi.folder, metadata.folder, tools.path, fps, video.format, compression_level=4){
  
  #Define input.folder
  input.folder <- paste0(to.data, raw.video.folder)
  
  #List all video files 
  video.files <- list.files(input.folder, full.names = T)
  
  #Define and make the folder for the converted video files
  output.folder <- paste0(to.data, raw.avi.folder)
  dir.create(output.folder, showWarnings = T)
  
  #Make the additional folders if one starts from .cxd files
  if(video.format=="cxd"){
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
    metadata <- paste0(meta.folder, "/", tools::file_path_sans_ext(output.file[length(output.file)]), ".txt")
    output.file <- paste0(output.folder, "/", tools::file_path_sans_ext(output.file[length(output.file)]), ".avi")
      
    if(video.format=="cxd"){
      
      #Create temporary folder for .tiff stack
      tiff.folder <- paste0(to.data, "/tiff", match(filename, video.files))
      dir.create(tiff.folder)
      
      #Convert cxd to tiff
      {
        #Prepare arguments
        arguments <- paste0(
          " -overwrite",
          " -no-upgrade ",
          " '", filename, "'",
          " -padded",
          " '", file.path(tiff.folder, "frame%t.tiff"), "'"
        )
        
        #Run command
        message("Converting ", filename, " to .tiff")
        system2(
          command = bfconvert,
          args = arguments,
          stdout = NULL
        )
      }
      
      #Convert tiff stack to avi using ffmpeg
      {
        #Prepare arguments
        arguments <- paste0(
          " -framerate ", fps,
          " -pattern_type glob",
          " -i  '", file.path(tiff.folder, "*.tiff"), "'",
          " -y -vcodec png",
          " -vtag 'PNG '",
          " -compression_level ", compression_level,
          " ", output.file
        )
        
        #Run command
        message("Compressing ", filename)
        system2(
          command = ffmpeg,
          args = arguments,
          stdout = NULL
        )
      }
      
      #Remove temporary folder with tiff stack
      unlink(tiff.folder, recursive=T)
      
      
      #Create and run a system command to extract and store the metadata
      arguments <- paste0( " -nopix -no-upgrade ",  filename)
      message("Extracting metadata ", filename)
      system2(command=showinf, args = arguments, stdout = metadata)
      
    } else {
      arguments <- paste0(" -i '", filename,  "' -y -vcodec png -compression_level  -vtag 'PNG ' ", compression_level, " '", output.file, "'")
      message("Compressing ", filename)
      system2(command=ffmpeg, args = arguments, stdout = NULL)
    }
    }
  return(NULL)
}
