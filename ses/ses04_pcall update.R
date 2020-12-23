
vivf2002 <- read_dta(paste0(path_census,"/transversales/data/stata_clean/vivf2002.dta")) %>% 
  mutate_at(vars(comuni,familia,census,v23),~as.numeric(.))


pcall <- haven::read_sas(paste0(path_incap_ses_raw_hc,"/data_Apr06/SAS/ses/pcall.sas7bdat")) 
## Filtering out only cohort households ----------


pca_sample = "cohort"


if(pca_sample == "cohort"){
  source(paste0(path_incap_repo,"/ses/ses_member households.R"))
  
  pcall <- pcall %>% 
    right_join(member_hh,
               by=c("census","comuni","familia"))
}

pcall <- pcall %>% 
  left_join(vivf2002 %>% 
              dplyr::select(comuni,familia,census,v23),
            by=c("comuni","familia","census")) %>% 
  dplyr::mutate(v23 = case_when(v23 %in% c(0,1) ~ v23,
                                TRUE ~ NA_real_)) %>% 
  dplyr::mutate(sitio90 = case_when(census == 2002 ~ v23,
                                    TRUE ~ sitio90)) %>% 
  dplyr::select(familia,comuni,census,
                v06, v07,
                sitio90, 
                # cuarto, 
                luz,
                
                # piso, techo, cocin, pared, poele, letrin, desag, abasag,
                
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
                
                
                
                # Assets not available in 2016, 2018:
                cassette, tocad, molino,
                plancha,maqescr,
                
                tenen_1 : tenen_7,
                piso_1 : piso_5,
                techo_1 : techo_4,
                pared_1 : pared_8,
                cocin_1 : cocin_4,
                poele_1 : poele_10,
                letrin_1, letrin_2,letrin_4,
                desag_1 : desag_3,
                abasag_1 : abasag_4,
                vivien_1:vivien_4,
                telef_1,telef_3 : telef_5,telef_6, #telef_2 : card
                cuartv07,
                owncasa, #owncasa == 1 --> tenen_6, tenen_7
                highqcasa,
                piso_high, # piso : 1 vs 2:5
                techo_high, # techo : 1 vs 2:4
                pared_high, # pared : 1:3 vs 4:8
                poele_high # poele : 0:3 vs 4:9
  ) %>% 
  mutate(techo_5 = 0,
         letrin_5 = 0,
         desag_4 = 0,
         desag_5 = 0,
         poele_78 = case_when(poele_7 == 1 | poele_8 == 1 ~ 1,
                              TRUE ~ 0)) %>% 
  
  # dplyr::filter(census!=2002) %>% 
  bind_rows(pc04 %>% 
              dplyr::select(-piso, -techo, -pared, -cocin, -poele, -letrin, -desag,-abasag,-garbage) %>% 
              mutate(census = 2004,
                     techo_5 = 0,
                     letrin_5 = 0,
                     desag_4 = 0,
                     desag_5 = 0,
                     poele_78 = case_when(poele_7 == 1 | poele_8 == 1 ~ 1,
                                          TRUE ~ 0))) %>%
  # Assets not available in 2016, 2018 are imputed with zero
  bind_rows(pc16  %>% 
              mutate(census = 2016,
                     cassette = NA,
                     tocad = NA,
                     molino = NA,
                     plancha = NA,
                     maqescr = NA
              )) %>% 
  bind_rows(pc18 %>%   
              mutate(census = 2018,
                     cassette = NA,
                     tocad = NA,
                     molino = NA,
                     plancha = NA,
                     maqescr = NA))  %>%
  mutate(telef = case_when(telef_3 == 1 | telef_4 == 1 | telef_5 == 1 | telef_6 == 1 ~ 1,
                           telef_3 == 0 & telef_4 == 0 & telef_5 == 0 & telef_6 == 0 ~ 0,
                           TRUE ~ NA_real_)) %>%
  dplyr::mutate(cocin_34 = case_when(cocin_3 ==1 | cocin_4 == 1 ~ 1,
                                     cocin_3 ==0 & cocin_4 == 0 ~ 0,
                                     TRUE ~ NA_real_),
                letrin_yes = case_when(letrin_1 == 1 ~ 0,
                                       TRUE ~ 1),
                
                abasag_high = case_when(abasag_3 ==1 | abasag_4 == 1 ~ 1,
                                        abasag_3 == 0 & abasag_4 == 0 ~ 0,
                                        TRUE ~ NA_real_),
                
                desag_high = case_when(desag_3 ==1 | desag_4 == 1 ~ 1,
                                       desag_3 == 0 & desag_4 == 0 ~ 0,
                                       TRUE ~ NA_real_)
                )


# Maluccio et al 2005 -----
pca05_df <- pcall %>% 
  dplyr::select(id_uni,census,
                comuni,familia,
                
                # Own radio
                radio,
                # Own record-player : Not sure if it's cassette,
                tocad,
                # Own bicycle
                bike,
                # Own sewing machine
                coser,
                # Own refrigerator
                frig,
                # Own television
                tv,
                # Own motorcycle
                moto,
                # Own automobile
                auto,
                # Own plot of land for house
                sitio90,
                # Own their house
                tenen_7,
                # Number of rooms in house/
                #   number of individuals
                cuartv07,
                # Live in a formal house
                
                # High-quality flooring
                piso_high,
                # High-quality roofing
                techo_high,
                # High-quality walls
                pared_high,
                # Kitchen separate room in house
                cocin_34,
                # Formal cooking medium
                poele_high,
                # Has electricity
                luz,
                # Latrine/toilet
                letrin_yes,
                # Drinking water from well
                abasag_high
 
  ) %>%
  dplyr::mutate_all(~as.numeric(.))


