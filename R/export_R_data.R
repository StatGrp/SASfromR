#' Convert names to comply with SAS standards
#'
#' @param x Character vector of names to be converted.
#' @param type Type of SAS name to convert to (variable, dataset or libname).
#' @param warn Logical. Whether to warn the user of changes in names.
#' @param xpt_version Version of xpt to use, 5 or 8 (default and recommended).
#' @param ... Not used.
#'
#' @returns A character vector of modified names.
#' @export
#'
#' @examples
#' toSASnames(c("this.will.change","but_not_this"))
toSASnames <- function(x, type="variable", warn=TRUE, xpt_version=8,...) {
  minlength <- switch(type,
                      variable = ifelse(xpt_version==5,8L,32L),
                      dataset = 32L,
                      libname = 7L,
                      format = 7L)
  # apply stronger rules to format
  if (type=="format") {
    allow_start_underscore <- FALSE
    allow_end_num <- FALSE
  } else if (type=="libname") {
    allow_start_underscore <- FALSE
  } else {
    allow_start_underscore <- TRUE
    allow_end_num <- TRUE
  }
  y <- abbreviateSAS(x, minlength=minlength,
                     allow_start_underscore=allow_start_underscore,
                     allow_end_num = allow_end_num)
  chng <- (y!=x)
  if (any(chng) & warn) {
    namechanges <- paste(x[chng],y[chng],sep=" --> ")
    cli::cli_alert("The following {type} names were changed to comply with SAS standards:\n")
    for (namechange in namechanges) cli::cli_ul(namechange)
  }
  return(y)
}



#' Create a SAS format data set based on factor variables in an R data.frame.
#'
#' @param df R data.frame (or objects that inherits from data.frame, e.g. tibble)
#'
#' @returns A data.frame which can be exported and used in SAS proc format with the cntlin= option to set formats for factor variables.
#' @export
#'
#' @examples
#' fct2fmt(iris)
fct2fmt <- function(df) {
  fct_names <- names(df[sapply(df,is.factor)])
  if (length(fct_names)<=0) {
    return(NULL)
  } else {
    fmt_names <- toSASnames(fct_names, type="format", warn=FALSE)
    fmt_data <- lapply(1:length(fct_names), \(n) {
      x <- fct_names[[n]]
      lbls <- attr(df[[x]],"levels")
      fmts <- fmt_names[[n]]
      data.frame(fmtname = rep(fmts,length(lbls)),
                 start   = 1:length(lbls),
                 label   = lbls,
                 varname = rep(x,length(lbls)))
    })
    do.call(rbind,fmt_data)
  }
}


#' Export a single data.frame to R (with optional format statement)
#'
#' @param df R data.frame to be exported to SAS
#' @param sas_name Character. SAS name of data set
#' @param libname Character. The libname to be used for the transport data set (must be at most 8 characters).
#' @param directory Full file path to where temporary xport data is stored.
#' @param xpt_version Which version of xpt to use, 5 or 8 (recommended).
#' @param format_statement Optional format statement.
#' @param ... Not used.
#'
#' @returns SAS script to import into SAS
#' @export
#'
#' @examples
#' RtoSAS(mtcars,"mtcars")
RtoSAS <- function(df, sas_name, libname=NULL, directory=tempdir(), xpt_version=8, format_statement=NULL,...) {
  if (is.null(libname)) {libname<-sas_name}
  tmp <- file.path(directory,sprintf("%s.xpt",sas_name))
  haven::write_xpt(df, path = tmp, version = xpt_version)
  # add import instructions to script header
  script_header <- c(
    stringr::str_glue('libname {libname} xport "{tmp}";'),
    ifelse(xpt_version==5,
           stringr::str_glue("data work.{sas_name}; set {libname}.{sas_name}; run;"),
           stringr::str_glue("%XPT2LOC(libref=work, filespec={libname});")),
    ifelse(is.null(format_statement),
           stringr::str_glue("data work.{sas_name}; set work.{sas_name}; run;"),
           stringr::str_glue("data work.{sas_name}; set work.{sas_name}; format {format_statement}; run;"))
  )
  return(script_header)
}




#' Export R data to SAS transport file
#'
#' @param indata Input data, either a single data.frame or a named list of data.frames.
#' @param in_path The full path to the directory where you wish to store it.
#' @param xpt_version Version of xpt transfer file to use. Default (and recommended) is version 8.
#' @param warn Logical, whether to warn you of any changes to the variable names that were needed to comply with SAS standards.
#' @param ... Not used.
#'
#' @returns A character string that can be used in SAS programs to import the data.
#' @export
#'
#' @examples
#' export_R_data(mtcars,in_path=tempdir())
export_R_data <- function(indata=NULL, in_path, xpt_version=8, warn=TRUE,...) {

  # initialize script_header
  script_header <- character(0)

  # if not null
  if (!is.null(indata)) {
    # check input type
    if (!inherits(indata,"list") & !inherits(indata,"data.frame")) cli::cli_abort(call=NULL,"Only dataframes or named lists of dataframes allowed for argument indata")
    if (inherits(indata,"list") & is.null(names(indata))) cli::cli_abort(call=NULL,"Only named lists are allowed for argument indata")
    # Handle case when only data.frame supplied
    if (inherits(indata,"data.frame")) {
      indata <- list("indata" = indata)
      cli::cli_alert('Because no name was supplied for input datasets it will be named "indata".')
    }
    # check data set names
    names(indata) <- toSASnames(names(indata),
                                type="dataset",
                                warn=warn,
                                xpt_version=xpt_version,
                                ...)
    # create separate libnames (stronger requirements)
    libnames <- toSASnames(names(indata),
                           type="libname",
                           warn=FALSE,
                           xpt_version=xpt_version,
                           ...)
    # this should almost never happen...
    if (any(nchar(libnames)>7)) cli::cli_abort(call=NULL,"Unable to create unique libnames for the supplied datasets. Please attempt to shorten the names of the datasets.")
    # for each entry in indata
    for (i in 1:length(indata)) {
      df <- indata[[i]]
      df_name <- names(indata)[i]
      libname <- libnames[i]
      # check names of columns
      names(df) <- toSASnames(names(df),
                              type="variable",
                              warn=warn,
                              xpt_version=xpt_version,
                              ...)
      # create format dataset (if there are factor variables)
      fmt_data <- fct2fmt(df)
      # if there are factor variables, export fmt_data and to import instructions
      if (!is.null(fmt_data)) {
        df_namef <- paste0(df_name,"f")
        libnamef <- paste0(libname,"f")
        script_header <- c(script_header,
                           RtoSAS(fmt_data,
                                  df_namef,
                                  libnamef,
                                  in_path,
                                  xpt_version=xpt_version),
                           sprintf("proc format cntlin=%s; run;",df_namef))
        # create format statment
        fmtdata <- fmt_data[c("varname","fmtname")]
        fmtdata <- fmtdata[!duplicated(fmtdata),]
        format_statement <- paste0(unique(sprintf("%s %s.",fmtdata$varname,fmtdata$fmtname)),
                                   collapse=" ")
      } else {
        format_statement<-NULL
      }
      # create temporary files in indata temporary directory
      script_header <- c(script_header,
                         RtoSAS(df,
                                df_name,
                                libname,
                                in_path,
                                xpt_version=xpt_version,
                                format_statement=format_statement))
    }
  }
  return(script_header)
}
