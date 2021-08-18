#' extract_samples
#'
#' This functions extract audio samples from wav files collected by the wehear logger, based on information
#' provided in label table.
#'
#' @param labels data.frame containing the information needed to extract and label samples
#' @param path_audio character string; path where the raw audio files are stored
#' @param path_extracts character string; path where to store the extracts
#' @param ext_audio character string; extension of the raw audio files
#'
#' @return
#' @export
#' @examples
extract_samples <- function(labels,path_audio,path_extracts=getwd(),ext_audio=".wav"){

  #requireNamespace("tuneR")

  # check if labels object has an id field,
  # if not, create one
  if(!"id" %in% colnames(labels)){
    labels$id <- 1:nrow(labels)
  }

  # do the extraction
  audio_names <- unique(labels$file)
  audio_files <- paste0(path_audio,"/",audio_names,ext_audio)

  for(i in 1:length(audio_files)){
    temp_labels <- labels[labels$file==audio_names[i],]
    temp_audio <- tuneR::readWave(audio_files[i]) # freq = 7812.5

    for(ii in 1:nrow(temp_labels)){

      sample_audio <- tuneR::extractWave(temp_audio,
                                         from=temp_labels$start_time[ii],
                                         to=temp_labels$end_time[ii],
                                         xunit="time")
      tuneR::writeWave(sample_audio,
                       filename=paste0(path_extracts,"/",
                                       temp_labels$label[ii],"_labelID_",temp_labels$id[ii],
                                       "_",temp_labels$file[ii],
                                       ".wav"))

    }
  }

  # Done
  cat(paste("Done. Extracted",nrow(labels),"samples"))

}
