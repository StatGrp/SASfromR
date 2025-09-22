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
  if (is.null(sas_path) & !SASinPATH()) cli::cli_abort(call=NULL,"sas.exe does not appear to be in your PATH. Consider adding it to your PATH or supply the full path to sas.exe to the argument sas_path.")


  # produce command for script
  sas.exe <- ifelse(is.null(sas_path),"sas.exe",sas_path)
  return_code <- system2(sas.exe,
                         c("-nosplash","-RSASUSER", "-batch", "-sysin", sas_script,"-log", sas_log, "-print",sas_output),
                         stdout = FALSE,
                         stderr = FALSE)
  #stdout=file.path(getwd(),"system.out"),
  #stderr=file.path(getwd(),"system.err"))

  # output sas log to console (if requested)
  if (file.exists(sas_log) & display_log) display_SAS_log(sas_log)
  # Handle errors (should be improved)
  sas_conditions <- check_sas_log(sas_log)
  if (length(sas_conditions)>0) {
    cli::cli({
      cli::cli_rule()
      cli::cli_alert("The following errors and/or warnings were reported in the SAS log.")
      for (cond in sas_conditions) {
        if (stringr::str_detect(cond,"^ERROR")) {
          cli::cli_alert_danger(cond)
        } else {
          cli::cli_alert_warning(cond)
        }
      }
      cli::cli_rule()
    })
  }
  if (return_code!="0") {
    # output sas log to console at this stage if requested
    #if (file.exists(sas_log) & display_log) display_SAS_log(sas_log)
    if (return_code==2) {
      # if error, display log regardless
      #display_SAS_log(sas_log)
      cli::cli_abort(call=NULL,"The exit-status was {return_code}. Please check the full SAS-log for errors (display_log=TRUE).")
    } else if (return_code==127) {
      cli::cli_abort(call=NULL,"The exit-status was {return code}. This probably means that sas.exe is not in your PATH. Add the directory where sas.exe is located to your PATH or set it to working directory (not optimal).")
    } else {
      cli::cli_abort(call=NULL,"The exit-status was {return_code}.")
    }
  }

  # output from sas to console
  if (file.exists(sas_output) & display_output) display_SAS_output(sas_output)

  return(return_code)
}




