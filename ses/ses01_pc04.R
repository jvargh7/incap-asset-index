

vivm2004 <- read_dta(paste0(path_census,"/transversales/data/stata_clean/vivm2004.dta"))

wi_assets_val <- function(var) {
  case_when(var == 1 ~ 1, 
            var == 0 ~ 0, 
            TRUE ~ NA_real_)
}



pc04_dict <- readxl::read_excel(paste0(path_incap_ses_dfa,"/INCAP SES Variable List.xlsx"),
                                sheet="pc04")

pc04 <- vivm2004 %>%
  dplyr::select(comuni,familia,na.omit(unique(pc04_dict$orig_var))) %>% 
  mutate_at(vars(paste0("v",c(22,24,26:30,32:36))), function(x) factor(x,
                                                              levels=attr(x,"labels"),
                                                              labels=attr(x,"labels") %>% attr(.,"names")))

for (i in 1:nrow(pc04_dict)){
  
  
  if(!is.na(pc04_dict$harmonized_categories[i]) & pc04_dict$harmonized_categories[i]=="yes"){
    new_var = pc04_dict$variable[i]
    var_04 = pc04_dict$orig_var[i]
    category = pc04_dict$var_desc_translated[i]
    pc04[,new_var] <- ifelse(pc04[,var_04] == category,1,0)
    
  }
  
}


pc04 <- pc04 %>% 
  # dplyr::select(-v37) %>% 
  mutate_at(vars(paste0("v",c(23,31,38:53,55,57:58))), ~wi_assets_val(.)) %>% 
  rename(sitio90 = v23,
         tenen = v22,
         vivien = v24,
         cuarto = v25,
         piso = v26,
         techo = v27,
         pared = v28,
         cocin = v29,
         poele = v30,
         luz = v31,
         letrin = v32,
         desag = v33,
         abasag = v34,
         garbage = v35,
         telefono = v36,
         radio = v38,
         cassette = v39,
         equipo = v40,
         tv = v41,
         video = v42,
         cable = v43,
         bike = v44,
         moto = v45,
         auto = v46,
         coser = v47,
         frig = v48,
         micro = v49,
         molino = v50,
         licua = v51,
         plancha = v52,
         aves = v53,
         qaves = v54,
         cerdos = v55,
         qcerdos = v56,
         maqescr = v57,
         compu = v58) %>% 
  
  mutate(pcapcuar = cuarto/v07)
         
compareGroups(~.-comuni-familia,data=pc04) %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)  

pc04 <- pc04 %>% 
  mutate_at(vars(paste0("poele_",c(1:10))), function(x) case_when(is.na(x) ~ 0,
                                                                  TRUE ~ x)) %>% 
  mutate_at(vars(paste0("techo_",c(1:4))), function(x) case_when(is.na(x) ~ 0,
                                                                  TRUE ~ x)) %>% 
  mutate_at(vars(paste0("desag_",c(1:5))), function(x) case_when(is.na(x) ~ 0,
                                                                 TRUE ~ x)) %>% 
  mutate_at(vars(paste0("telef_",c(1:6))), function(x) case_when(is.na(x) ~ 0,
                                                                 TRUE ~ x)) %>% 
  mutate_at(vars(comuni,familia),~as.numeric(.))
