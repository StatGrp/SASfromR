#' Import SAS data sets from a directory
#'
#' @param out_path Full path to the directory where SAS data sets are stored.
#' @param outdata Character vector of data sets (without .sas7bdat extension) that you want to import. If NULL, all found data sets will be imported.
#'
#' @returns A data.frame or named list of data.frames imported from SAS.
#' @export
#'
#' @examples
#' \dontrun{
#' # should come up with a decent example here without relying on existing data}
import_SAS_data <- function(out_path, outdata=NULL) {

  # check the output directory for existing files and compare with listed outdata
  if (is.null(outdata)) {
    # if no outdata was supplied, load all .sas7bdat files in the output directory
    outdata_files <- list.files(out_path)
    outdata_files <- outdata_files[grepl(".*\\.sas7bdat",outdata_files)]
    outdata_paths <- file.path(out_path,outdata_files)
    names(outdata_paths) <- stringr::str_extract(outdata_files,"(.*)\\.sas7bdat", group=1)
  } else {
    # create paths to the output datasets and check that they exist
    outdata_paths <- file.path(out_path,paste0(outdata,".sas7bdat"))
    names(outdata_paths) <- outdata
    if (any(!file.exists(outdata_paths))) warning("One or more of the listed output datasets were not created. Check that you spelled it correctly and/or check the logs.")
    outdata_paths <- outdata_paths[file.exists(outdata_paths)]
  }

  # import all output datasets
  outdata_list <- list()
  if (length(outdata_paths)>0) {
    for (i in 1:length(outdata_paths)) {
      out_name <- names(outdata_paths)[i]
      out_file <- outdata_paths[i]
      outdata_list[[out_name]] <- haven::read_sas(out_file)
    }
  }

  # return output data
  if (length(outdata_list)==0) {
    return(invisible(NULL))
  } else if (length(outdata_list)==1) {
    return(outdata_list[[1]])
  } else {
    return(outdata_list)
  }
}





