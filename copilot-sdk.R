library(jsonlite)
source("logger.R")
source("RFunction.R")

inputFileName = "Workflow_Instance_001__move2_loc_to_moveStack__2023-04-18_08-31-40.rds" #important to set to NULL for movebank-download
outputFileName = "output.rds"

args <- list()
#################################################################
########################### Arguments ###########################
# The data parameter will be added automatically if input data is available
# The name of the field in the vector must be exaclty the same as in the r function signature
# Example:
# rFunction = function(username, password)
# The paramter must look like:
#    args[["username"]] = "any-username"
#    args[["password"]] = "any-password"

# Add your arguments of your r function here
args[["reso"]] = 8 # 
args[["uni"]] = "hours"
#args[["maptype"]] = 	"terrain" #toner, (roads), (hydda), watercolor, topographic, terrain, streets
args[["maptype"]] = 	"voyager" #"light", dark", "voyager"
args[["mapres"]] = 0.5 # between 0 and 1
args[["frames_per_sec"]] = 200
args[["col_opt"]] = "one" # "one","trackid", "animalid", "other"
args[["other"]] = "sex"
args[["show_legend"]] = TRUE
args[["capt"]] = "ABC et al. Paper about these data. 2022"
args[["file_format"]] = "mp4" # "gif", "mov", "mp4", "flv", "avi", "mpeg", "3gp", "ogg"
args[["ext_adap"]] = 1 # multiplicative to ext

#################################################################
#################################################################
inputData <- NULL
if(!is.null(inputFileName) && inputFileName != "" && file.exists(inputFileName)) {
  cat("Loading file from", inputFileName, "\n")
  inputData <- readRDS(file = inputFileName)
} else {
  cat("Skip loading: no input File", "\n")
}

# Add the data paramter if input data is available
if (!is.null(inputData)) {
  args[["data"]] <- inputData
}

result <- tryCatch({
    do.call(rFunction, args)
  },
  error = function(e) { #if in RFunction.R some error are silenced, they come back here and break the app... (?)
    print(paste("ERROR: ", e))
    stop(e) # re-throw the exception
  }
)

if(!is.null(outputFileName) && outputFileName != "" && !is.null(result)) {
  cat("Storing file to", outputFileName, "\n")
  saveRDS(result, file = outputFileName)
} else {
  cat("Skip store result: no output File or result is missing", "\n")
}