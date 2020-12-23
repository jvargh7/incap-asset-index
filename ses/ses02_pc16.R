mastercontrol17apr06s <- read_dta(paste0(path_incap_ses_box,"/BDGT_oriente/MasterID/data/stata_clean/mastercontrol17apr06s.dta"))

                
wi_assets_val <- function(var) {
  case_when(var == 1 ~ 1, 
            var == 0 ~ 0, 
            TRUE ~ NA_real_)
}

pc16_dict <- readxl::read_excel(paste0(path_incap_ses_dfa,"/INCAP SES Variable List.xlsx"),
                                sheet="pc16")

pc16 <- readRDS(paste0(path_local_working,"/Processed Data/R datasets/gtml_meta_wi.RDS")) %>%
  dplyr::filter(site=="GTML",!is.na(wealth_index_gt16)) %>%
  dplyr::rename(id_uni = pin) %>% 
  # na.omit(unique(pc16_dict$meta_wi)) : select valid item IDs
  dplyr::select(id_uni,na.omit(unique(pc16_dict$meta_wi)),starts_with("r_"))


meta_marital_status <- read_dta(paste0(path_gtml_earlier_data, "/META data as of 20 Jun 2017_stata12.dta")) %>% 
  dplyr::select(iduni,f1a1,f1a3,f1a4) %>% 
  dplyr::mutate(married = case_when(f1a1 %in% c(1,2) ~ 1,
                                      f1a1 %in% c(3,4,5) ~ 0,
                                      TRUE ~ NA_real_)) %>% 
  rename(id_uni = iduni) %>% 
  # dplyr::filter(married == 1)
  dplyr::mutate(id_par = case_when(is.na(f1a4) ~ NA_real_,
                                   f1a4 == 99999 ~ NA_real_,
                                   TRUE ~ f1a4)) %>% 
  mutate(par_in_meta = case_when((id_par %in% id_uni) ~ 1,
                                 TRUE ~ 0))

# meta_partners - v2.0 using f1a4 ---------
meta_partners <- meta_marital_status %>% 
  dplyr::filter(par_in_meta == 1) %>% 
  mutate(id_selected = case_when(id_uni > id_par ~ id_par,
                                 id_uni < id_par ~ id_uni,
                                 TRUE ~ NA_real_)) %>% 
  group_by(id_selected) %>% 
  mutate(nobs = n()) %>% 
  ungroup() %>% 
  dplyr::filter(nobs == 2)

meta_no_partners <- meta_marital_status %>%
  dplyr::select(id_uni) %>%
  anti_join(meta_partners,
            by = "id_uni") %>%
  mutate(id_selected = id_uni)



meta_master <- bind_rows(meta_no_partners,
                         meta_partners %>% 
                           dplyr::select(id_selected,id_uni)
                         )

selected_id_uni = c(unique(meta_partners$id_selected),meta_no_partners$id_selected)

pc16 <- pc16 %>% 
  dplyr::filter(id_uni %in% selected_id_uni)

for (i in 1:nrow(pc16_dict)){
  
  
  if(!is.na(pc16_dict$harmonized_categories[i]) & pc16_dict$harmonized_categories[i]=="yes"){
    new_var = pc16_dict$variable[i]
    var_16 = pc16_dict$meta_wi[i]
    category = pc16_dict$var_desc_translated[i]
    pc16[,new_var] <- ifelse(pc16[,var_16] == category,1,0)
    
  }
  
}

pc16 <- pc16 %>%
  
  # Basic Assets
  mutate(
    v06 = v06 + 1, # since m1a1 starts from 0
    sitio90 = owncasa, # not sure if we should create sitio90 or sitio20
    
    # garbage = m1a17, #no need to actually include since it's not there in previous waves
    
    # telefono = m1b1, #m1b1 = telephone service
    
    
    # casette = ?
    # tocad = ? # Turn-table
    # m1b5 = ipod is not included
    
    #m1b10 = inter, m1b11 =directtv = direct-tv
    #m1b17 = washingmachine = washing machine
    # molino = ? - handgrinder
    # plancha = ? = iron
    # qaves = ? - number of birds
    # qcerdos = ? - number of pigs
    # maqescr = ? - typewriter
    pcapcuar = cuarto/v06,
    
    # vivien_1 : makeshift home/shack?
    # vivien_2 : ranch?
    # vivien_3 : semi-formal house
    # vivien_4 : formal house
    
    
    poele_78 = case_when(poele == "7, Wood stove" ~ 1,
                         TRUE ~ 0),
    letrin_3 = case_when(letrin %in% c("4, Septic Tank", "2, Toilet") ~ 1,
                         TRUE ~ 0),
    letrin_5 = case_when(letrin %in% c("6, Other", "7, Pit latrine") ~ 1,
                         TRUE ~ 0),
    telef_1 = 0,
    # telef_2 = 0,
    telef_6 = 0,
    telef_5 = case_when(telefono ==1 & cellphone == 1 ~ 1,
                        TRUE ~ 0),
    telef_3 = case_when(telefono==0 & cellphone == 1 ~ 1,
                        TRUE ~ 0),
    telef_4 = case_when(telefono==1 & cellphone == 0 ~ 1,
                        TRUE ~ 0),
    cuartv07 = cuarto/v07,
    # owncasa = sitio90, #m1a6
    piso_high = case_when(piso_1 == 1 ~ 0,
                          piso_2 == 1 | piso_3 == 1 | piso_4 == 1 | piso_5 == 1 ~ 1,
                          TRUE ~ NA_real_),
    techo_high = case_when(techo_1 == 1 ~ 0,
                           techo_2 == 1 | techo_3 == 1 | techo_4 == 1  ~ 1,
                           TRUE ~ NA_real_),
    pared_high = case_when(pared_1 == 1 | pared_2 == 1 | pared_3 == 1 ~ 0,
                           pared_4 == 1 | pared_5 == 1 | pared_6 == 1 | pared_7 == 1 | pared_8 == 1 ~ 1,
                           TRUE ~ NA_real_),
    poele_high = case_when(poele_1 == 1 | poele_2 == 1 | poele_3 == 1 | poele_4 == 1 ~ 0,
                           poele_5 == 1 | poele_6 == 1 | poele_78 == 1 | poele_9 == 1 | poele_10 == 1 ~ 1,
                           TRUE ~ NA_real_),
    highqcasa = case_when(piso_high + techo_high + pared_high == 3 ~ 1,
                          TRUE ~ 0)
  ) %>% 
  dplyr::select(id_uni,
                v06, v07,
                sitio90, 
                # cuarto, 
                luz,
                # garbage, # not there in other waves
                # telefono - we are using telef_X
                radio, equipo,
                tv, video, cable,
                bike, moto, auto,
                coser, frig,
                micro, licua,
                aves, cerdos,
                compu,
                pcapcuar,
                
                cellphone,
                ipod,
                # 
                # inter,
                # directtv,
                
                tenen_1 : tenen_7,
                piso_1 : piso_5,
                techo_1 : techo_5,
                pared_1 : pared_8,
                cocin_1 : cocin_4,
                poele_1 : poele_6, poele_78, poele_9,poele_10,
                letrin_1: letrin_5,
                desag_1 : desag_5,
                abasag_1 : abasag_4,
                starts_with("telef_"), #telef_2 : card
                garbage_1 :garbage_6,
                cuartv07,
                owncasa,
                piso_high,
                techo_high,
                pared_high,
                poele_high,
                highqcasa,
                
                washingmachine,
                micro
  )


compareGroups(~.-id_uni,data=pc16) %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)
