#' Function to check if all dependencies are present, and if not download them
#' 
#' Function calls ImageJ software and its ParticleAnalyzer function to extract for each frame of the video
#' several morphological descriptors and the X- and Y-coordinates of all moving particles. All videos in the raw.video.folder are analyses, separately.
#' @param tools.path path to the folder where the dependencies (fiji, bftools and ffmpeg) should be located
#' @return returns nothing (NULL)
#' @export 

check_tools_folder <- function(tools.path){
  
  if(!file.exists(tools.path)){
    dir.create(tools.path)
  }
  
  #If the particle linker jar is not present, download this jar
  if(!file.exists(paste0(tools.path, "ParticleLinker.jar"))){
    
    #Download .jar file
    utils::download.file(
      url = "https://www.dropbox.com/s/688bjvhqz30yhqu/ParticleLinker.jar?dl=1",
      destfile = file.path(tools.path, "ParticleLinker.jar"),
      mode = "wb"
    )
  }
  
  
  #If ffmpeg is not in the tools folder, download and extract it to a folder called ffmpeg
  if(!file.exists(paste0(tools.path, "/ffmpeg/ffmpeg"))){
    
    #Download .tar file
    utils::download.file(
      url = "https://johnvansickle.com/ffmpeg/releases/ffmpeg-release-amd64-static.tar.xz",
      destfile = file.path(tools.path, "ffmpeg.tar.xz"),
      mode = "wb"
    )
    
    #Create folder to store ffmpeg files
    dir.create(paste0(tools.path, "/ffmpeg"), showWarnings = F)
    
    #Untar file
    system(paste0("tar ", "-xvf ", tools.path, "/ffmpeg.tar.xz -C ", tools.path, "ffmpeg/ --strip-components=1"))
    
    #remove .tar file
    unlink(file.path(tools.path, "/ffmpeg.tar.xz"))
  }

  if(!file.exists(paste0(tools.path, "/bftools/bfconvert"))){
    #Download the .zip file
    utils::download.file(
      url = "http://downloads.openmicroscopy.org/latest/bio-formats5.6/artifacts/bftools.zip",
      destfile = file.path(tools.path, "bftools.zip"),
      mode = "wb"
    )
    
    #Extract to bftools folder
    utils::unzip(
      zipfile = file.path(tools.path, "bftools.zip"),
      exdir = file.path( tools.path )
    )
    
    #Remove the .zip file
    unlink(file.path(tools.path, "bftools.zip"))
  }
  
  if(!file.exists(paste0(tools.path, "/Fiji.app/java/"))){
    
    #Download fiji folder
    link <- "https://downloads.imagej.net/fiji/archive/20191027-2045/fiji-linux64.zip"
    utils::download.file(
      url = link,
      destfile = file.path(tools.path, "fiji.zip"),
      mode = "wb")
    
    #Extract .zip file
    utils::unzip(
      zipfile = file.path(tools.path, "fiji.zip"),
      exdir = file.path( tools.path )
    )
    
    #Remove .zip file
    unlink(file.path(tools.path, "fiji.zip"))
  }
    
  if(file.exists(paste0(tools.path, "/Fiji.app/java/")) & file.exists(paste0(tools.path, "/bftools/bfconvert")) & 
     file.exists(paste0(tools.path, "/ffmpeg/ffmpeg")) & file.exists(paste0(tools.path, "ParticleLinker.jar"))){
    print("All dependencies are downloaded and installed.")
  } else {
    print("Warning: not all dependencies installed succesfully.")
  }
}
