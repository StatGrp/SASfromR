
#' Convert attached formats to proper factor variables
#'
#' @param df data.frame (or object that can be coerced into data.frame) with SAS formats attached (as is the case after import via haven).
#' @param fmts data.frame of formats (should contain variables FMTNAME, START and LABEL)
#'
#' @returns data.frame with factor variables.
#' @export
#'
#' @examples
#' \dontrun{
#' #not even trying
#' }
fmt2fct <- function(df,fmts) {
  # get a vector of all format names
  fmtnames <- unique(fmts$FMTNAME)
  # for each variable, check if it has a format and if it exists in fmtnames
  lapply(df, \(x) {
    x_fmt <- attr(x,"format.sas")
    if (!is.null(x_fmt) && x_fmt %in% fmtnames) {
      fmt <- fmts[which(fmts$FMTNAME==x_fmt),]
      # additional check to see if all elements of variable can be coerced to values in format
      if (all(x %in% c(fmt$START,NA))) return(factor(x,levels=fmt$START,labels=fmt$LABEL))
      else return(x)
    } else return(x)
  }) |> tibble::as_tibble() |> haven::zap_formats()
}

#' Import SAS data sets from a directory
#'
#' @param out_path Full path to the directory where SAS data sets are stored.
#' @param outdata Character vector of data sets (without .sas7bdat extension) that you want to import. If NULL, all found data sets will be imported.
#' @param factor_format_conv Logical, whether to automatically convert SAS-formats to R-factors.
#'
#' @returns A data.frame or named list of data.frames imported from SAS.
#' @export
#'
#' @examples
#' \dontrun{
#' # should come up with a decent example here without relying on existing data}
import_SAS_data <- function(out_path, outdata=NULL, factor_format_conv=TRUE) {

  # load stored formats if they exist
  fmt_file <- file.path(out_path,"fmt","fmt.sas7bdat")
  if (file.exists(fmt_file) & factor_format_conv) {
    fmts <- haven::read_sas(fmt_file)
    fmts <- fmts[c("FMTNAME","START","LABEL")]
    fmts$START <- stringr::str_trim(fmts$START)
  } else {
    fmts <- NULL
  }

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
      if (!is.null(fmts)) {
        outdata_list[[out_name]] <- fmt2fct(outdata_list[[out_name]],fmts)
      }
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
