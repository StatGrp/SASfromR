#' Check if sas.exe is in your PATH
#'
#' @returns Logical, TRUE if sas.exe is in your path, FALSE if not.
#' @noRd
SASinPATH <- function() {
  return(system2("where","sas.exe",stdout=FALSE,stderr=FALSE)==0)
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
  outlines <- readLines(sas_log) |> stringr::str_replace_all("\f\\d.*","\n")
  outlines[stringr::str_detect(outlines,"The SAS System")] <- "\n"
  outlines_decorated <- lapply(outlines, function(x) {
    x <- stringr::str_replace(x,"NOTE","\033[36mNOTE\033[37m")
    x <- stringr::str_replace(x,"WARNING","\033[33mWARNING\033[37m")
    x <- stringr::str_replace(x,"ERROR", "\033[31mERROR\033[37m")
  })
  paste0(outlines_decorated,collapse="\n")
  cli::cli({
    cli::cli_h1("SAS Log")
    cli::cli_div(class="verbatim",
                 theme=list(.verbatim=list(color="gray"),
                            rule = list(color="cyan",
                                        "line-type"=".")))
    cli::cli_verbatim(outlines_decorated)
    cli::cli_rule()
    cli::cli_end()
  })
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
  outlines <- readLines(sas_output) |>
    stringr::str_replace_all("\f","\n") |>
    stringr::str_replace_all("\x83\x83","--") |>
    stringr::str_replace_all("\x83","|")
  outlines[stringr::str_detect(outlines,"The SAS System")] <- "\n"
  outlines <- paste0(outlines,collapse="\n")
  cli::cli({
    cli::cli_h1("SAS Output")
    cli::cli_div(class="verbatim",
                 theme=list(.verbatim=list(color="#fffed5"),
                            rule = list(color="cyan",
                                        "line-type"=".")))
    cli::cli_verbatim(outlines)
    cli::cli_rule()
    cli::cli_end()
  })
}

#' Check the SAS log for errors and warnings.
#'
#' @param sas_log Full path to a stored SAS log.
#'
#' @returns A list of errors and.
#' @export
#'
#' @examples
#' \dontrun{
#' logfile <- tempfile()
#' SASfromR("proc contents data=sashelp.cars; run;", log_file = logfile, display_log=FALSE)
#' display_SAS_log(logfile)}
check_sas_log <- function(sas_log) {
  outlines <- readLines(sas_log)
  errs <- outlines[stringr::str_detect(outlines,"ERROR")]
  warns <- outlines[stringr::str_detect(outlines,"WARNING")]
  return(list(errors = errs, warnings=warns))
}
