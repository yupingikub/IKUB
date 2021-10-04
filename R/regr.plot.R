#' @title Displays all regression lines
#' @description Displays all regression lines of all label_ids for a
#' control group.
#' @details Input output from icv.label.count() which is the data frame
#' contains ICVs of all participants in the control group to calculate
#' regression coefficients of all label_ids.
#' @param icvdf A data frame contains ICV, meanicv and volumes for all label_ids
#' of all participants in the control group. It is recommended to use
#' icv.label.count() to generate input data.
#' @return Regression lines for all label_ids and mean ICV for the
#' control group.
#' @export
regr.plot <- function(icvdf) {
  
df_model <- function(df){
    lm(vol_mm3 ~ icv_mm3, data = df)
}  

regr_plot <- function(data, model) {
  plot_ly(data = data, x = ~icv_mm3, y = ~vol_mm3,
          type = "scatter", mode = "markers", hoverinfo = 'text',
          text = ~paste('</br> pid: ', pid,
                        '</br> icv: ', round(icv_mm3,2),
                        '</br> vol: ', round(vol_mm3,2))) %>%
    layout(legend = list(orientation = 'h'))%>%
    add_trace(data = data, x = ~icv_mm3, y = ~predict(model),
              mode = "lines", name = "lm")
}

vols <- icvdf%>%
  mutate(label_name=substr(label_name, 1, 
                           nchar(label_name)-nchar(class.name)))%>%
  dplyr::select(pid, class.name, label_id, 
                label_name, icv_mm3, vol_mm3)%>%
  mutate(label_name=paste(label_id, label_name, sep=' '))%>%
  select(pid, class.name, label_name, icv_mm3, vol_mm3)

df <- nest(vols, data = !one_of(c('class.name', "label_name")))

df <- df %>%
  mutate(model = map(data, df_model))%>%
  mutate(data_plot = map2_plot(data, model, regr_plot))

for(i in 1:nrow(df)){
  df$regr_line[i] <- paste('y=', round(df[[4]][[i]][["coefficients"]][["icv_mm3"]],4), '*x+', round(df[[4]][[i]][["coefficients"]][["(Intercept)"]],2), sep='')
}
     
df %>%
  trelliscope(name = "ICV and regression line", nrow = 2, ncol = 3)

}