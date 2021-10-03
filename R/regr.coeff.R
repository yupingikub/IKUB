#' @title Calculates regression coefficients
#' @description Calculates regression coefficients of all label_ids for a
#' control group.
#' @details Input output from icv.label.count() which is the data frame
#' contains ICVs of all participants in the control group to calculate
#' regression coefficients of all label_ids.
#' @param icvdf A data frame contains ICV, meanicv and volumes for all label_ids
#' of all participants in the control group. It is recommended to use
#' icv.label.count() to generate input data.
#' @return Regression coefficients for all label_ids and mean ICV for the
#' control group.
#' @export

regr.coeff <- function(icvdf) {
  meanicv <- icvdf%>%
    dplyr::group_by('pid')%>%
    dplyr::summarize(icvmean=base::mean(icv_mm3))%>%
    dplyr::pull(icvmean)

  vols <- icvdf%>%
    dplyr::group_by(label_id) %>%
    dplyr::select(pid, p.class, time_points, rnum,
           label_id, icv_mm3, meanicv, vol_mm3)%>%
    dplyr::do(broom::tidy(lm(vol_mm3 ~ icv_mm3, data = .)))%>%
    dplyr::select(label_id, term, estimate)%>%
    tidyr::pivot_wider(.,names_from = term, values_from = estimate)%>%
    left_join(icvdf, by='label_id')%>%
    dplyr::select(label_id, k=icv_mm3.x)%>%
    dplyr::mutate(meanicv=meanicv)%>%
    stats::na.omit()
  return(vols)
}

