?export_R_data
export_R_data(mtcars, tempdir())

SASfromR("proc means data=indata; var mpg; ods output Summary=out.means; run;", indata=mtcars)

outfile <- tempfile()
logfile <- tempfile()
SASfromR("proc contents data=sashelp.cars; run;",
         output_file=outfile, log_file=logfile,
         display_output = FALSE, display_log=FALSE)
display_SAS_output(out)
display_SAS_log(out)

sas_script <- tempfile()
readr::write_lines(r"(
                   proc contents data=sashelp.cars out=out.test;
                   run;)",
                   file=sas_script)
execute_SAS_script(sas_script)
import_SAS_data(r"(C:\Users\jensm\AppData\Local\Temp\RtmpYlVfhs\outdata_12320669a578d)","test")


SASfromR("proc contents data=sashelp.cars out=out.test; run;", remove_tempfiles = FALSE)
import_SAS_data(r"(C:\Users\jensm\AppData\Local\Temp\RtmpYlVfhs\outdata_12320669a578d)","test")
?import_SAS_data
list.files(tempdir(),pattern=".xpt")
