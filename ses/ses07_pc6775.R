



# Merging with mastercontrol17Apr06 and ses_all_famtmp (derived from pcall)
mastercontrol17Apr06 <- read_dta(paste0(path_incap_ses_box,"/BDGT_oriente/MasterID/data/stata_clean/mastercontrol17Apr06.dta"))

ses_all_famtmp <- pcall %>%
  dplyr::distinct(census,comuni,familia,.keep_all=TRUE) %>%
  dplyr::select(-id_uni)

pc6775 <- tmp1967m %>% 
  dplyr::filter(id_uni!=0) %>% 
  bind_rows(tmp1975m) %>% 
  inner_join(ses_all_famtmp,
             by=c("census","comuni","familia")) %>% 
  mutate(census = case_when(census == 1967 ~ 67,
                            census == 1975 ~ 75,
                            TRUE ~ NA_real_)) %>% 
  tidyr::pivot_wider(id_cols=id_uni,names_from=census,
                     names_sep = "_",
                     
                     values_from = c("srcses","comuni","familia",
                                     "pcall","pcall_2","pcall_3")) %>% 
  right_join(mastercontrol17Apr06 %>% 
               dplyr::filter(master==1),
             
             by = "id_uni") %>% 
  dplyr::select(id_uni,starts_with("comun"),
                starts_with("srcses"),
                starts_with("familia"),
                starts_with("pc"),
                fechan,
                master) %>% 
  
  # Skipping the labeling for now
  rename_at(
    vars(starts_with("pc")), 
    function(x=.) {
      x_split = str_split(x,pattern = "_");
      
      lapply(x_split,function(y){
        if(length(y)==2){
          y2 = paste0(y[1],
                      y[2]);
          # print(y2);
        } ;
        if(length(y)>2){
          y2 = paste0(y[1],
                      y[3],
                      "_",
                      y[2]);
          # print(y2);
        };
        
        return(y2)
      })
      
    }) %>% 
  group_by(comun) %>% 
  mutate(pcall67_mean = case_when(is.na(pcall67) ~ mean(pcall67,na.rm=TRUE),
                                  TRUE ~ pcall67),
         pcall67_2mean = case_when(is.na(pcall67_2) ~ mean(pcall67_2,na.rm=TRUE),
                                   TRUE ~ pcall67_2),
         pcall67_3mean = case_when(is.na(pcall67_3) ~ mean(pcall67_3,na.rm=TRUE),
                                   TRUE ~ pcall67_3),
         pcall75_mean = case_when(is.na(pcall75) ~ mean(pcall75,na.rm=TRUE),
                                  TRUE ~ pcall75),
         pcall75_2mean = case_when(is.na(pcall75_2) ~ mean(pcall75_2,na.rm=TRUE),
                                   TRUE ~ pcall75_2),
         pcall75_3mean = case_when(is.na(pcall75_3) ~ mean(pcall75_3,na.rm=TRUE),
                                   TRUE ~ pcall75_3)
  ) %>% 
  ungroup() %>% 
  mutate(srcses_67 = case_when(is.na(pcall67) ~ 0,
                               TRUE ~ srcses_67),
         srcses_75 = case_when(is.na(pcall75) ~ 0,
                               TRUE ~ srcses_75)) %>% 
  
  mutate(pcall67 = case_when(is.na(pcall67) ~ pcall67_mean,
                             TRUE ~ pcall67
  ),
  pcall67_2mean = case_when(is.na(pcall67_2) ~ pcall67_2mean,
                            TRUE ~ pcall67_2
  ),
  pcall67_3mean = case_when(is.na(pcall67_3) ~ pcall67_3mean,
                            TRUE ~ pcall67_3
  ),
  pcall75 = case_when(is.na(pcall75) ~ pcall75_mean,
                      TRUE ~ pcall75
  ),
  pcall75_2mean = case_when(is.na(pcall75_2) ~ pcall75_2mean,
                            TRUE ~ pcall75_2
  ),
  pcall75_3mean = case_when(is.na(pcall75_3) ~ pcall75_3mean,
                            TRUE ~ pcall75_3
  )
  ) %>% 
  mutate(
         pcall6775 = case_when(fechan < "1971-01-01" & !is.na(pcall67) ~ pcall67,
                               fechan >= "1971-01-01" & !is.na(pcall75) ~ pcall75,
                               fechan < "1971-01-01" & srcses_67 == 0 & srcses_75 !=0 ~ pcall75,
                               fechan >= "1971-01-01" & srcses_67 !=0 & srcses_75 == 0 ~ pcall67,
                               TRUE ~ NA_real_ ),
         pcall6775_2 = case_when(fechan < "1971-01-01" & !is.na(pcall67) ~ pcall67_2mean,
                               fechan >= "1971-01-01" & !is.na(pcall75) ~ pcall75_2mean,
                               fechan < "1971-01-01" & srcses_67 == 0 & srcses_75 !=0 ~ pcall75_2mean,
                               fechan >= "1971-01-01" & srcses_67 !=0 & srcses_75 == 0 ~ pcall67_2mean,
                               TRUE ~ NA_real_ ),
         pcall6775_3 = case_when(fechan < "1971-01-01" & !is.na(pcall67) ~ pcall67_3mean,
                               fechan >= "1971-01-01" & !is.na(pcall75) ~ pcall75_3mean,
                               fechan < "1971-01-01" & srcses_67 == 0 & srcses_75 !=0 ~ pcall75_3mean,
                               fechan >= "1971-01-01" & srcses_67 !=0 & srcses_75 == 0 ~ pcall67_3mean,
                               TRUE ~ NA_real_ ),
 
  
  src6775 = case_when(fechan < "1971-01-01" ~ paste0("1",srcses_67),
                      fechan >= "1971-01-01" & !is.na(pcall75) ~paste0("2", srcses_75),
                      fechan < "1971-01-01" & srcses_67 == 0 & srcses_75 !=0 ~ paste0("2", srcses_75),
                      fechan >= "1971-01-01" & srcses_67 !=0 & srcses_75 == 0 ~ paste0("1",srcses_67),
                      TRUE ~ NA_character_
  )
  )


rm(parentalapr06,perf1967,perf1975,popdemoind67,ses_all_famtmp,ses_all_indtmp)
rm(tmp1967m,tmp1975m)

compareGroups(~.-id_uni,data=pc6775) %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)