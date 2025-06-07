#### Program to run SAS instructions from R
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

SASfromR <- function(sas_command, indata=NULL, outdata=NULL,
  xpt_version = 8, sas_path = NULL,
  output_file=NULL, log_file=NULL, 
  display_log=FALSE, display_output=TRUE, 
  remove_tempfiles=TRUE, repair_names=TRUE) {
  # A function which allows you to run SAS from within R (through batch-mode), 
  # while using input datasets from R (argument indata). 
  # Returns selected (argument outdata) datasets produced in SAS.
  #
  # SAS code is provided in the `sas_command` argument (preferably as a raw-string).
  # Input datasets from R will be placed in the "work" library/directory with names taken from the 
  # named list supplied in `indata` and can be referenced in the `sas_command` script through work.[in_name].
  # Datasets produced in SAS that you want to output to R should be placed in the library "out" (which is created by the function)
  # and should thus be written as out.[out_name] in `sas_command`. They also have to be specified in the `outdata` argument.
  #
  # Arguments:
  # sas_script:  must be character string (or character vector), ideally raw-string
  # indata:      must be named list (names are used as references in SAS script and therefore need to be valid SAS-names)
  # outdata:     must be character vector of valid names for SAS datasets that are created in the script and should be returned to R as a named list
  # output_file: path to file where SAS output is stored (if NULL, output will be displayed in console)
  # log_file:    path to file where SAS log is stored (if NULL, log will be displayed in console)
  # display_log: wether to display log in console (default: FALSE)
  # display_output: wether to display the output produced by SAS procedures in console (default: TRUE)
  
  # create paths to indata and outdata directories in current temporary directory
  in_path <- file.path(tempdir(),"indata")
  if (!dir.exists(in_path)) dir.create(in_path)
  out_path <- file.path(tempdir(),"outdata")
  if (!dir.exists(out_path)) dir.create(out_path)
  script_header <- sprintf('libname out "%s";',out_path)
  if (!is.null(indata)) {
    # check input type
    if (!inherits(indata,"list") & !inherits(indata,"data.frame")) stop("Only dataframes or named lists of dataframes allowed for argument indata")
    if (inherits(indata,"list") & is.null(names(indata))) stop("Only named lists are allowed for argument indata")
    # Handle case when only dataframe supplied
    if (inherits(indata,"data.frame")) {
      indata <- list("indata" = indata)
      message('Because no name was supplied for input datasets it will be named "indata". In SAS-code you can refer to it as "work.indata".')
    }
    # check dataset names
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

  # add script_header to sas_script and save to temporary file
  #sas_command <- paste(script_header,sas_command, collapse="\n")
  #cat(sas_command)
  sas_script <- tempfile(pattern="sas_script", fileext=".sas", tmpdir=in_path)
  readr::write_lines(c(script_header,sas_command),file=sas_script)

  # create output and log files if not specified
  if (is.null(output_file)) {
    sas_output <- tempfile(tmpdir=in_path)
  } else {
    sas_output <- output_file
  }
  if (is.null(log_file)) {
    sas_log <- tempfile(tmpdir=in_path)
  } else {
    sas_log <- log_file
  }
  # produce command for script
  sas.exe <- ifelse(is.null(sas_path),"sas.exe",sas_path)
  return_code <- system2(sas.exe,
                         c("-nosplash", "-icon", "-sysin", sas_script,"-log", sas_log, "-print",sas_output), 
                         stdout = FALSE,
                         stderr = FALSE
                         #stdout=file.path(getwd(),"system.out"),
                         #stderr=file.path(getwd(),"system.err")
  )
  # run
  #return_code  <- system(cmd)  # Runs sas and saves the return code to
  if (return_code!="0") {
    if (return_code==2) {
      # if error, display log regardless, and 
      cat("\n","\033[34mSAS log","\033[33m \n",
          paste0(readLines(sas_log),collapse="\n") |> stringr::str_replace_all("\f","\n"),
          "\033[0m\n")
      warning(sprintf("The exit-status was %s. Please check the SAS-log for errors.",return_code))
    } else if (return_code==127) {
      warning(sprintf("The exit-status was %s. This probably means that sas.exe is not in your PATH. Add the directory where sas.exe is located to your PATH or set it to working directory (not optimal).",return_code))
    } else {
      warning(sprintf("The exit-status was %s.",return_code))
    }
  }
 
  # output sas log to console
  if (file.exists(sas_log) & display_log) {
    cat("\n","\033[34mSAS log","\033[33m \n",
        paste0(readLines(sas_log),collapse="\n") |> stringr::str_replace_all("\f","\n"),
        "\033[0m\n")
  }
  # output from sas to console
  if (file.exists(sas_output) & display_output) {
    cat("\n","\033[34mSAS output","\033[36m \n",
        paste0(readLines(sas_output),collapse="\n") |> stringr::str_replace_all("\f","\n"),
        "\033[0m\n")
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
    }
  }

  # remove temporary files and directories
  if (remove_tempfiles) {
    unlink(in_path, recursive=TRUE)
    unlink(out_path, recursive=TRUE)
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

runSAS <- function(x=NULL, sas_code, ...) {
  SASfromR(sas_command=sas_code, indata=x, ...)
}

