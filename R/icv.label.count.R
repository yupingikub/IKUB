#' @title Generates a data frame for segmentations with tissue classes  
#' @description Generates a data frame for volume calculation for segmentations 
#' with tissue classes. Function can be used to generate ICV data for the 
#' control group, other participant or patients.  
#' NIfTI-files.
#' @details Inputs three data frames as input and returns total volumes of 
#' all label_ids for the supplied NIfTI-files of MR segmentaion and NIfTI-files
#' of tissue classes. 
#' @param filedf A data frame contains participants'/patients' information. 
#' It must contain five columns of which first column has column name filename1,
#' the second column has column name filename2, the third column has column 
#' name pid with character format, the fourth column has column name p.class
#' with character format and the final column has column name time_points 
#' with integer format. filename1 column contains NIfTI-filenames of 
#' segmentation and filename2 column NIfTI-filenames of tissue segmentation. 
#' @param labelnametibble A label name tibble contains two columns of which 
#' first column has column name label_id and the second column has column name
#' label_name. 
#' @param tcdf Tissue class data frame contains three columns. The first column
#' has column name class.id with integer format, the second column has column
#' name class.name with character format and third column has column name 
#' comb.tc with list format. User can combine the segmentations by giving 0 or 1
#' to the vectors in the lists in order to calculate volumes for interest. For 
#' example using list(0,0,0) to define the common segmentations, list(1,1,1) 
#' background plus MR segmentations and list(1,0,1) class 1 and class 3 in the 
#' tissue segmentations plus MR segmentations. 
#' @return Total volumes for all segmentations for the supplied NifTI-files.
#' @export
icv.lable.count <- function(filedf, labelnametibble, tcdf) {
  count <- function(filename1, filename2) {
    seg <- neurobase::readnii(filename1)
    tc3<- neurobase::readnii(filename2)
    volpervoxel <- prod(seg@pixdim[2:4])
    seg.output <- tibble::as_tibble(table(seg))%>%
      dplyr::mutate(label_id=as.integer(seg), vol_mm3=n*volpervoxel)%>%
      dplyr::select(label_id, vol_mm3)
    tc3.seg <- tc3*nrow(seg.output)+seg
    tc3.seg.output <- tibble::as_tibble(table(tc3.seg))%>%
      dplyr::mutate(label_id=as.integer(tc3.seg)+nrow(seg.output), 
                    vol_mm3=n*volpervoxel)%>%
      dplyr::select(label_id, vol_mm3)
    output <- rbind(seg.output, tc3.seg.output)%>%
      dplyr::mutate(filename=filename1, tc5.id=floor(label_id/nrow(seg.output)), 
             rnum=label_id%%nrow(seg.output))
    return(output)
  }
  
  segdf <- purrr::map2_dfr(filedf[,1], filedf[,2], count)
  
  icvdf<- segdf%>%
    dplyr::filter(tc5.id==0)%>%
    dplyr::left_join(filedf, by=c('filename'='filename1'))%>%
    dplyr::group_by(pid)%>%
    dplyr::summarize(icv_mm3=sum(vol_mm3))
    
  segdf <- segdf%>%
    dplyr::select(filename, tc5.id, rnum, vol_mm3)%>%
    tidyr::pivot_wider(names_from = 'tc5.id', values_from = vol_mm3)
  
  seg575<- data.frame(filename=character(), rnum=numeric())
  
  for(i in 1:nrow(tcdf)){
    item1 <- unlist(tcdf[,3][[i]][1])
    item2 <- unlist(tcdf[,3][[i]][2])
    item3 <- unlist(tcdf[,3][[i]][3])
    if(item1==0&item2==0&item3==0){
         tc_vol <- data.frame(filename=segdf[,1], rnum=segdf[,2])%>%
          dplyr::mutate(vol_mm3=segdf[,3], class.id=tcdf[i,1], 
                        class.name=tcdf[i,2])
        seg575 <- rbind(seg575, tc_vol)
    }else if(item1==1&item2==1&item3==1){
        tc_vol <- data.frame(filename=segdf[,1], rnum=segdf[,2])%>%
          dplyr::mutate(vol_mm3=segdf[,4], class.id=tcdf[i,1], 
                        class.name=tcdf[i,2])
        seg575 <- rbind(seg575, tc_vol)
    }else{
        tc_vol <- data.frame(filename=segdf[,1], rnum=segdf[,2])%>%
          dplyr::mutate(vol_mm3=segdf[,5]*item1+segdf[,6]*item2+segdf[,7]*item3, 
                 class.id=tcdf[i,1], class.name=tcdf[i,2])
        seg575 <- rbind(seg575, tc_vol)
    }
  }
seg575$vol_mm3 <- purrr::as_vector(seg575$vol_mm3)  
seg575 <- seg575%>%
  dplyr::left_join(labelnametibble, by=c('rnum'='label_id'))%>%
  dplyr::left_join(filedf, by=c('filename'='filename1'))%>%
  dplyr::mutate(label_id=rnum+nrow(labelnametibble)*class.id)%>%
  stats::na.omit()%>%
  dplyr::mutate(label_name=paste(label_name, class.name))%>%
  dplyr::left_join(icvdf, by='pid')%>%
  dplyr::select(pid, p.class, time_points, label_id, rnum, class.name, 
         label_name, vol_mm3, icv_mm3)
  return(seg575)
}
