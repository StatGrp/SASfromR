#' Check if sas.exe is in your PATH
#'
#' @returns Logical, TRUE if sas.exe is in your path, FALSE if not.
#' @noRd
SASinPATH <- function() {
  return(system2("where","sas.exe",stdout=FALSE,stderr=FALSE)==0)
}

#' Check if, and optionally change, names so that they adhere to SAS standards.
#'
#' @param x Character vector of names to be checked.
#' @param type Character, either "columns" or "dataset", referring to which type of SAS-names are targeted. In xpt version 5, column names may only be 8 characters long, otherwise 32.
#' @param repair Logical, whether to repair the names (change them to adhere to SAS standards) or to leave as is.
#' @param warn Logical, whether warning should be displayed when names have been repaired.
#' @param xpt_version Numeric, which version of xpt (SAS transport files) should be used (version 8 is default and recommended).
#'
#' @returns A character vector of names. If repair=FALSE then returns the original names, if repair=TRUE returns the repaired names.
#' @export
#'
#' @examples
#' # check if the names of an R data set are SAS compliant.
#' checkSASnames(iris)
#' # check and repair the names of an R data set.
#' iris2 <- checkSASnames(iris, repair=TRUE)
checkSASnames <- function(x, type="columns", repair=TRUE, warn=TRUE, xpt_version=8) {
  y <- x
  maxlength <- ifelse(type=="columns", ifelse(xpt_version==5,8L,32L), 32L)
  nonalphanum <- grepl("[^0-9a-zA-Z_]",x)
  #### better to use base::abbreviate first, then replace alphanumeric
  if (any(nonalphanum)) {
    if (repair) {
      y <- stringr::str_replace_all(x,"[^0-9a-zA-Z_]","_")
    } else {
      stop(sprintf("The following names have non-alpha-numeric characters which is not allowed in SAS: %s.",
                   paste0(x[nonalphanum],collapse=", ")))
    }
  }
  toolong <- (nchar(y)>maxlength)
  if (any(toolong)) {
    if (repair) {
      y <- abbreviate(y,maxlength)
    } else {
      stop(sprintf("The following names are too long for SAS names: %s",
                   paste0(x[toolong],collapse=", ")))
    }
  }
  chng <- (y!=x)
  if (any(chng)) {
    varchanges <- paste(x[chng],y[chng],sep=" --> ")
    if (warn) warning(sprintf("The following names were changed to comply with SAS standards:\n%s",paste0(varchanges,collapse="\n")))
  }
  return(y)
}



#' Export R data to SAS transport file
#'
#' @param indata Input data, either a single data.frame or a named list of data.frames.
#' @param in_path The full path to the directory where you wish to store it.
#' @param xpt_version Version of xpt transfer file to use. Default (and recommended) is version 8.
#' @param repair_names Logical, whether to repair/change column names in order to comply with SAS standards (a warning will be issued to tell you which names have been changed and what the new names are).
#'
#' @returns A character string that can be used in SAS programs to import the data.
#' @export
#'
#' @examples
#' export_R_data(mtcars,in_path=tempdir())
export_R_data <- function(indata=NULL, in_path, xpt_version=8, repair_names=TRUE) {

  # initialize script_header
  script_header <- character(0)

  # if not null
  if (!is.null(indata)) {
    # check input type
    if (!inherits(indata,"list") & !inherits(indata,"data.frame")) stop("Only dataframes or named lists of dataframes allowed for argument indata")
    if (inherits(indata,"list") & is.null(names(indata))) stop("Only named lists are allowed for argument indata")
    # Handle case when only dataframe supplied
    if (inherits(indata,"data.frame")) {
      indata <- list("indata" = indata)
      message('Because no name was supplied for input datasets it will be named "indata". In SAS-code you can refer to it as "work.indata".')
    }
    # check data set names
    names(indata) <- checkSASnames(names(indata), type="dataset", xpt_version=xpt_version, repair=repair_names)
    # for each entry in indata
    for (in_name in names(indata)) {
      # check names of columns
      names(indata[[in_name]]) <- checkSASnames(names(indata[[in_name]]), type="columns", xpt_version=xpt_version, repair=repair_names)
      # create temporary files in indata temporary directory
      tmp <- file.path(in_path,sprintf("%s.xpt",in_name))
      # export to xpt
      haven::write_xpt(
        indata[[in_name]],
        path = tmp,
        version = xpt_version
      )
      # add import instructions to script header
      script_header <- c(
        script_header,
        sprintf('libname %s xport "%s";',in_name,tmp),
        ifelse(xpt_version==5,
               sprintf("data work.%s; set %s.%s; run;",in_name, in_name, in_name),
               sprintf("%%XPT2LOC(libref=work, filespec=%s);",in_name))
      )
    }
  }
  return(script_header)
}


#' Display a SAS log on the R console.
#'
#' @param sas_log Full path to where the SAS log has been stored.
#'
#' @returns The SAS log.
#' @export
#'
#' @examples
#' \dontrun{
#' logfile <- tempfile()
#' SASfromR("proc contents data=sashelp.cars; run;", log_file = logfile, display_log=FALSE)
#' display_SAS_log(logfile)}
display_SAS_log <- function(sas_log) {
  cat("\n","\033[34mSAS log","\033[33m \n",
      paste0(readLines(sas_log),collapse="\n") |> stringr::str_replace_all("\f","\n"),
      "\033[0m\n")
}

#' Display the output from SAS procedures
#'
#' @param sas_output Full path to where the output has been stored.
#'
#' @returns The SAS output.
#' @export
#'
#' @examples
#' \dontrun{
#' outfile <- tempfile()
#' SASfromR("proc contents data=sashelp.cars; run;", output_file = outfile, display_output=FALSE)
#' display_SAS_output(outfile)}
display_SAS_output <- function(sas_output) {
  cat("\n","\033[34mSAS output","\033[36m \n",
      paste0(readLines(sas_output),collapse="\n") |> stringr::str_replace_all("\f","\n"),
      "\033[0m\n")
}


#' Execute SAS code on your machine (requires installation of SAS)
#'
#' @param sas_script Full path to a stored SAS script to be executed.
#' @param sas_path Full path to sas.exe in your local installation. Only necessary if the directory containing sas.exe is not in your PATH.
#' @param sas_output Full path to where the output from SAS procedures should be stored.
#' @param sas_log Full path to where the SAS-log should be stored.
#' @param display_output Logical, whether to display the output produced by SAS.
#' @param display_log Logical, whether to display the log produced by SAS.
#'
#' @returns A numeric exit-code.
#' @export
#'
#' @examples
#' \dontrun{
#' # create temporary file containing SAS code
#' sas_script <- tempfile()
#' readr::write_lines("proc contents data=sashelp.cars; run;", file=sas_script)
#' # run the stored script using execute_SAS_script()
#' execute_SAS_script(sas_script)}
execute_SAS_script <- function(sas_script,
                               sas_path=NULL,
                               sas_output=tempfile(), sas_log=tempfile(),
                               display_output=TRUE, display_log=TRUE) {

  # check to see if sas.exe is in path or if sas_path has been supplied.
  if (is.null(sas_path) & !SASinPATH()) stop("sas.exe does not appear to be in your PATH. Consider adding it to your PATH or supply the full path to sas.exe to the argument sas_path.")


  # produce command for script
  sas.exe <- ifelse(is.null(sas_path),"sas.exe",sas_path)
  return_code <- system2(sas.exe,
                         c("-nosplash", "-icon", "-sysin", sas_script,"-log", sas_log, "-print",sas_output),
                         stdout = FALSE,
                         stderr = FALSE
                         #stdout=file.path(getwd(),"system.out"),
                         #stderr=file.path(getwd(),"system.err")
  )

  # Handle errors (should be improved)
  if (return_code!="0") {
    if (return_code==2) {
      # if error, display log regardless
      display_SAS_log(sas_log)
      warning(sprintf("The exit-status was %s. Please check the SAS-log for errors.",return_code))
    } else if (return_code==127) {
      warning(sprintf("The exit-status was %s. This probably means that sas.exe is not in your PATH. Add the directory where sas.exe is located to your PATH or set it to working directory (not optimal).",return_code))
    } else {
      warning(sprintf("The exit-status was %s.",return_code))
    }
  }

  # output sas log to console
  if (file.exists(sas_log) & display_log) display_SAS_log(sas_log)
  # output from sas to console
  if (file.exists(sas_output) & display_output) display_SAS_output(sas_output)

  return(return_code)
}




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





