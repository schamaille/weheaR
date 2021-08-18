#' import_labels
#'
#' This function imports label files obtained when using Audacity to annotate audio files.
#' #' @param filenames vector of filenames (label files). Full path or files will be looked for in the working directory
#' @param fullpath_id boolean; Should the full path of the audio file
#' be returned in the file, or only the file name (without extension)?
#'@details If filenames is a vector of files, these files are appended in
#' on single dataframe. The .txt extension is removed at the end of the
#' filenames so that one could easily add an extension and grab the audio
#' files corresponding to the label files.
#' @return
#' @export
#'
#' @examples
import_labels <- function(filenames,fullpath_id=FALSE){

  # function to import one file only
  do_one_file <- function(one_filename,fullpath_id){
    # read file and clean-up
    out <- scan(one_filename,what="character",quiet=T)
    out <- out[which(out!="\\")]
    out <- data.frame(matrix(out,ncol=5,byrow=TRUE))
    out[,c(1:2,4:5)] <- apply(out[,c(1:2,4:5)],2,as.numeric)
    colnames(out) <- c("start_time","end_time","label","freq_low","freq_high")
    out$duration <- with(out,end_time-start_time)
    # add file name or file path, without extension to ease finding audio file later (paste .wav for instance)
    if(fullpath_id){
      file_id <- gsub(".txt","",one_filename)
    } else {file_id <- gsub(".txt","",basename(one_filename))}
    out$file <- rep(file_id,nrow(out))
    # reorganize and output
    out <- out[,c(7,3,1:2,6,4:5)]
    return(out)
  }

  # do
  out_all <- do.call(rbind,lapply(filenames,do_one_file,fullpath_id=fullpath_id))
  return(out_all)

}
