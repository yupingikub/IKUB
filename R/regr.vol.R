#' @title Calculates normalized volumes
#' @description Calculates normalized volumes of all label ids for all
#' individuals in the input data frame.
#' @details Input two data frames which the first data frame contains regression
#' coefficients and the second contains ICV of all individuals,
#' the third contains all label_id volumes of all individuals.
#' @param regrdf A data frame including regression coefficients for label_id.
#' It is recommended to use regr.coeff() to generate input data.
#' @param icvdf label_id volume data frame must contain ten columns of which
#' first column has column name pid with character format, the second column
#' has column name p.class with character format, the third column has column
#' name time_points with integer format, the fourth column has column name
#' label_id with integer format, the fifth column has column name rnum with
#' integer format, the sixth column has column name class.id with
#' integer format, the seventh column has column label_name with character
#' format, eighth column has column name vol_mm3 with numeric format and the
#' ninth column has column name icv_mm3 with numeric format.  It is
#' recommended to use icv.label.count() to generate input data.
#' @return Returns a data frame including normalized volumes and none normalized
#' volumes for all individuals.
#' @export
regr.vol <- function(regrdf, icvdf) {
  df <- icvdf%>%
    dplyr::full_join(regrdf, by='label_id')%>%
    dplyr::mutate(icvdev=icv_mm3-meanicv)%>%
    dplyr::mutate(vol_mm3=vol_mm3-k*icvdev, norm='regr_ICV')%>%
    dplyr::select(pid, time_points, rnum, label_id, label_name,
                  class.name, vol_mm3, norm)%>%
    base::rbind(., icvdf %>%
                  dplyr::select(pid, time_points, rnum, label_id, label_name,
                         class.name, vol_mm3)%>%
                  dplyr::mutate(norm='none'))%>%
    dplyr::select(pid, time_points, rnum, label_id, class.name,
           R575.uniq.name=label_name, norm, vol_mm3)%>%
    tidyr::pivot_wider(names_from = c("pid", "time_points"), values_from = "vol_mm3")
  return(df)
}
