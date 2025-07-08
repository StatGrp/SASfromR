#### Outline of how to implement automatic import of SAS formats
out <- mtcars |>
  dplyr::mutate(grp = sample(c(1,2), size=nrow(mtcars), replace=TRUE)) |>
  dplyr::mutate(grp = factor(grp, levels=c(1,2), labels=c("Group 1","Group 2"))) |>
  withSAS(r"(
          proc glimmix data=indata;
            class grp(ref="Group 1");
            model mpg = grp disp / solution;
            ods output ParameterEstimates = out.ests;
          run;
          /* this part should go into script_footer*/
          proc format cntlout=out.fmt;
           *select grp; /* might as well take all formats instead of selecting*/
          run;
  )")
# note that out$ests$grp has format "GRP":
attributes(out$ests$grp)
lvls <-  out$fmt |> dplyr::filter(FMTNAME=="GRP") |> dplyr::pull(START)
lbls <- out$fmt |> dplyr::filter(FMTNAME=="GRP") |> dplyr::pull(LABEL)
out$ests |> dplyr::mutate(grp = factor(grp,levels=stringr::str_trim(lvls), labels=lbls))
# need to somehow go through the fmt dataset and identify which ones are reasonable
# to use when converting to factors
#... although there are not so many
out$fmt$FMTNAME |> unique()
# and only column grp has a  "GRP" matches the attributes
sapply(out$ests,\(x) attr(x,"format.sas")) |> unlist()


# Maybe create a separate (sub-)folder and libname for formats.
# Have a function look through that folder and read in the SAS format dataset (before loading other).
# Then look through the data sets in "out" and add factors/formats as if they match.
# maybe need a more thorough test for match, e.g. wether unique values can be coerced into the values in the format.

out$fmt$START <- stringr::str_trim(out$fmt$START)

fmt2factor <- function(df,fmts) {
  fmtnames <- unique(fmts$FMTNAME)
  lapply(df, \(x) {
    x_fmt <- attr(x,"format.sas")
    if (!is.null(x_fmt) && x_fmt %in% fmtnames) {
      fmt <- fmts[which(fmts$FMTNAME==x_fmt),]
      if (all(x %in% c(fmt$START,NA))) return(factor(x,levels=fmt$START,labels=fmt$LABEL))
      else return(x)
    } else return(x)
  }) |> data.frame()
}
fmt2factor(out$ests,out$fmt)
