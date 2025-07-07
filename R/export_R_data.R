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
                      libname = 7L)
  y <- stringr::str_replace_all(x,"[^0-9a-zA-Z_]","_")
  y <- abbreviate(y,minlength=minlength)
  chng <- (y!=x)
  if (any(chng) & warn) {
    varchanges <- paste0(paste(x[chng],y[chng],sep=" --> "),collapse="\n")
    cli::cli_alert(
      sprintf("The following %s names were changed to comply with SAS standards:\n%s",
              type,varchanges)
      )
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
  fmt_data <- lapply(fct_names, \(x) {
    lbls <- attr(df[[x]],"levels")
    data.frame(fmtname = rep(abbreviate(x),length(lbls)),
               start = 1:length(lbls),
               label = lbls,
               varname = rep(x,length(lbls)))
  })
  do.call(rbind,fmt_data)
}


#' Export a single data.frame to R (with optional format statment)
#'
#' @param df R data.frame to be exported to SAS
#' @param sas_name Character. SAS name of data set
#' @param directory Full file path to where temporary xport data is stored.
#' @param xpt_version Which version of xpt to use, 5 or 8 (recommended).
#' @param format_statement Optional format statement.
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
    sprintf('libname %s xport "%s";',libname,tmp),
    ifelse(xpt_version==5,
           sprintf("data work.%s; set %s.%s; run;",sas_name, libname, sas_name),
           sprintf("%%XPT2LOC(libref=work, filespec=%s);",libname)),
    sprintf("data work.%s; set work.%s; format %s; run;",sas_name, sas_name, format_statement)
  )
  return(script_header)
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
export_R_data <- function(indata=NULL, in_path, xpt_version=8, warn=TRUE,...) {

  # initialize script_header
  script_header <- character(0)

  # if not null
  if (!is.null(indata)) {
    # check input type
    if (!inherits(indata,"list") & !inherits(indata,"data.frame")) cli::cli_abort(call=NULL,"Only dataframes or named lists of dataframes allowed for argument indata")
    if (inherits(indata,"list") & is.null(names(indata))) cli::cli_abort(call=NULL,"Only named lists are allowed for argument indata")
    # Handle case when only dataframe supplied
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
        format_statement <- paste0(unique(sprintf("%s %s.",fmt_data$varname,fmt_data$fmtname)),
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
