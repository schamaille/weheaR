% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_labels.R
\name{import_labels}
\alias{import_labels}
\title{import_labels}
\usage{
import_labels(filenames, fullpath_id = FALSE)
}
\arguments{
\item{filenames}{vector of filenames (label files). Full path or files will be looked for in the working directory}

\item{fullpath_id}{boolean; Should the full path of the audio file
be returned in the file, or only the file name (without extension)?}
}
\value{

}
\description{
This function imports label files obtained when using Audacity to annotate audio files.
}
\details{
If filenames is a vector of files, these files are appended in
on single dataframe. The .txt extension is removed at the end of the
filenames so that one could easily add an extension and grab the audio
files corresponding to the label files.
}
