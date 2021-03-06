#' Function to link the particle coordinates through time
#' 
#' The function takes the XY-coordinates provided by the ImageJ ParticleAnalyzer and uses a standalone version of the ImageJ MOSAIC plugin ParticleLinker to create trajectories. 
#' This requires some creation of temporary files, which are subsequently deleted.
#' Multiple instances of the particle linker can be called in unix OS
#' @param to.data path to the working directory 
#' @param particle.data.folder directory where the ParticleAnalyzer output is saved (as text files) (temporary)
#' @param trajectory.data.folder directory where the ParticleLinker is saved (as text files) (temporary???)
#' @param memory numeric value specifying the max amount of memory allocated in general (defaults to 512)
#' @param memory_per_linkerProcess numeric value specifying the max amount of memory allocated to one instance of the ParticleLinker (defaults to 512)
#' @param linkrange numeric value passed to the ParticleLinker specifying the range of adjacent frames which
#' are taken into account when a trajectory is re-constructed 
#' @param disp numeric value that specifies the maximum displacement of a given particle between two frames
#' @param start_vid numeric value to indicate whether the linking should be started with a video other than the first
#' @param raw.avi.folder Folder containing the converted and compressed .avi files
#' @param max.cores Maximum number of cores to be used. Defaults to 1
#' @return Returns a single text file per video containing the X- and Y-coordinates, the frame and a trajectory ID. The files are than automatically merged into a data.table
#' with the movement metrics for each fix appended to the original data (NB: movement metrics often need two (e.g. step length), sometimes even 
#' three (e.g., turning angles) fixes; fixes for which metrics cannot be calculated are padded with NA). The movement parameters are the step length, the step duration, 
#' the step speed (step length/step duration), the gross displacement as the cumulative sum of the step lengths, the net displacement between the first fix of a given trajectory 
#' and the current fix and finally the relative angle (turning angle) and absolute angle (in radians). For details on these metrics, please refer to a dedicated textbook 
#' (e.g. Turch (1998): Quantitative Analysis of Movement: Measuring and Modeling Population Redistribution in Animals and Plants, Sinauer Associates, Sunderland).
#' @import parallel 
#' @export

link_particles <- function(to.data, particle.data.folder, trajectory.data.folder, linkrange = 1, disp = 10, start_vid = 1, memory = 512, memory_per_linkerProcess = 512, raw.avi.folder, max.cores=1) {
  
  if(!exists("to.particlelinker")) stop("Path to ParticleLinker not found. Please specify path in global options.")
  
  if(memory_per_linkerProcess>memory) stop("Machine memory needs to be larger than the memory per linker process.")
  
  PA_output_dir <- paste0(to.data, particle.data.folder)
  traj_out.dir <- paste0(to.data, trajectory.data.folder)
  
  dir.create(traj_out.dir, showWarnings = F)
  all.files <- dir(PA_output_dir, pattern = ".ijout.txt")
  
  # determine how many proceses to run in parallel based on memory usage and available cores
  mem_ratio <- floor(memory/memory_per_linkerProcess)
  no_cores <- detectCores()
  max_linker_processes <- min(c(mem_ratio, no_cores - 1, max.cores))

  # Create folder for logs
  dir.create("linkingLogs/", showWarnings = F)
  
  #Create a function for parallelized linking
  linkingParallel <- function(j, all.files, PA_output_dir){
    PA_data <- read.table(paste0(PA_output_dir, "/", all.files[j]), sep = "\t", header = T)
    
    ## only attempt particle linking if particles were detected in the video note: not sure what would happen if only one
    ## particle was found in one frame
    if (length(PA_data[, 1]) > 0) {
      
      dir <- paste0(to.data, gsub(".avi", "", sub(".ijout.txt", "", all.files[j])))
      dir.create(dir, showWarnings = F)
      
      for (i in 1:max(PA_data$Slice)) {
        frame <- subset(PA_data, Slice == i)[, c(6, 7)]
        frame$Z <- rep(0, length(frame[, 1]))
        sink(paste0(dir, "/frame_", sprintf("%04d", i - 1), ".txt"))
        cat(paste0("frame ", i - 1))
        cat("\n")
        sink()
        write.table(frame, file = paste0(dir, "/frame_", sprintf("%04d", i - 1), ".txt"), append = T, col.names = F, 
                    row.names = F)
      }
      
      ## run ParticleLinker
      if (.Platform$OS.type == "unix") {
        cmd <- paste0("java -Xmx", memory_per_linkerProcess, "m -Dparticle.linkrange=", linkrange, " -Dparticle.displacement=", disp, 
                      " -jar ", " \"", to.particlelinker, "/ParticleLinker.jar","\" ", "'", dir, "'", " \"", traj_out.dir,"/ParticleLinker_", 
                      all.files[j],"\"", " 2>&1 | tee ",to.data, "linkingLogs/log", j, ".txt")
        
      }
      
      system(cmd, timeout=(2600*24*7))
    }
    
    if (length(PA_data[, 1]) == 0) {
      print(paste("***** No particles were detected in video", all.files[j], " -- check the raw video and also threshold values"))
      
    }
      
      
  }
  
  #Perform parallelized analysis
  #start <- Sys.time()
  parallel::mclapply(start_vid:length(all.files), linkingParallel, all.files, PA_output_dir, mc.cores = max_linker_processes)
  
  # merge all files into one database
  data <- organise_link_data(to.data, trajectory.data.folder) 
  
  #calculate movement metrics for each fix and save to disk
  calculate_mvt(data,to.data,trajectory.data.folder,pixel_to_scale,fps)
  
  # delete working directories
  unlink(paste0(to.data, gsub(".avi", "", sub(".ijout.txt", "", all.files))), recursive = T)
  
  #create a counter for the potential errors
  error.count <- 0
  
  #Check if there may have been memory issue
  if (.Platform$OS.type=="unix"){
    #Get a list of all logs
    logs <- list.files(path = "linkingLogs/", full.names = T)
    
    #Count the errors
    for (j in logs){
      error.count <- error.count + length(grep("java.lang.OutOfMemoryError", readLines(j, warn = F), value = TRUE))
    }
    system(paste0("rm -r ", to.data, "linkingLogs"))
    
    if(error.count>0){
      messageError <- paste("Java ran out of memory while linking", error.count, "video(s). Try increasing the assigned memory per linking process")
      stop(messageError)
    } else {
      print("No memory errors occured during particle Linking")
    }
  }
}
