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
  outlines <- readLines(sas_log, skipNul=TRUE) |> stringr::str_replace_all("\f\\d.*","\n")
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
  outlines <- readLines(sas_log, skipNul=TRUE)
  conds <- outlines[stringr::str_detect(outlines,"^(ERROR|WARNING).*")]
  return(conds)
}



abbreviateSAS <- function(names_old, minlength=7L, allow_end_num=FALSE, allow_start_underscore=FALSE) {

  if (minlength<=0L) cli::cli_abort("{.variable {minlength}} too small.")
  names_new <- names_old;
  # only alphanumeric and underscore
  names_new <- stringr::str_replace_all(names_old,"[^0-9a-zA-Z_]","_")
  if (!allow_start_underscore) {
    names_new <- stringr::str_replace_all(names_new,"^_","")
  }
  # check length and duplicates
  dups <- duplicated(names_new)
  longs <- (nchar(names_new)>=minlength)

  # no alteration if ok
  if (!any(dups) & !any(longs)) {
    return(names_new)
  }
  # starting point is output from abbreviate
  names_new <- abbreviate(names_new, minlength=minlength, strict=TRUE)
  # then change
  if (!allow_end_num) {
    start <- substr(names_new, 1, nchar(names_new)-1)
    end <- substr(names_new, nchar(names_new), nchar(names_new))
    end <- sapply(end, \(x) if(grepl("\\d$",x)) {letters[as.integer(x)+1]} else {x})
    names_new <- paste0(start,end)
  }
  # check length and duplicates
  dups <- duplicated(names_new)
  if (!any(dups)) {
    return(names_new)
  } else {
    unique_dups <- unique(names_new[dups])
    #idx_list <- lapply(unique_dups, \(x) match(x,names_new))
    for (dup_name in unique_dups) {
      dup_idx <- match(dup_name,names_new)
      dup_names_new <- sapply(1:length(dup_idx),\(i) stringr::str_replace(dup_name,"\\w$",letters[i]))
      names_new[dup_idx] <- dup_names_new
    }
    return(names_new)
  }
}


do_if_exists_SAS <- function(dname,do_this) {
  stringr::str_glue(r"(
  %macro do_if_exists(data);
   %if %sysfunc(exist(&data.)) %then %do;
    {do_this}
   %end;
  %mend do_if_exists;
  %do_if_exists({dname});
  )")
}
do_if_cexists_SAS <- function(dname,do_this) {
  stringr::str_glue(r"(
  %macro do_if_cexists(data);
   %if %sysfunc(cexist(&data.)) %then %do;
    {do_this}
   %end;
  %mend do_if_cexists;
  %do_if_cexists({dname});
  )")
}


