


# Based on "ses_data_guide.dir":
# C:\Cloud\Box Sync\INCAP SES Harmonization\RSPH Research\Raw and old data\human capital\data_Apr06\SAS\ses
# pcall contains variables from popdemogstack from all census years as well as dummy variables
# familia : defined by unique combination of census, comuni, familia


popdemogind <- read_dta(paste0(path_incap_ses_box,
                               "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                               "popdemogind.dta")) %>% 
  mutate_at(vars(comuni,familia,census,id_uni),~as.numeric(.))


parentalapr06 <- read_sas(paste0(path_incap_ses_raw_hc,"/data_Apr06/SAS/parent/parentalapr06.sas7bdat")) 
perf1967 <- read_sas(paste0(path_incap_ses_raw_hc,"/data_Apr06/SAS/census/perf1967.sas7bdat")) 
perf1975 <- read_sas(paste0(path_incap_ses_raw_hc,"/data_Apr06/SAS/census/perf1975.sas7bdat"))

# Matching to self -----

tmp1967self <- parentalapr06 %>% 
  dplyr::select(id_uni,master) %>% 
  dplyr::filter(master==1) %>% 
  full_join(perf1967 %>% 
              dplyr::select(id_uni,presen,comuni,fam) %>% 
              rename(presen_1 = presen,
                     comuni_1 = comuni,
                     famil_1 = fam),
            by=c("id_uni"))
tmp1975self <- parentalapr06 %>% 
  dplyr::select(id_uni,master) %>% 
  dplyr::filter(master==1) %>% 
  full_join(perf1975 %>% 
              dplyr::select(id_uni,presen,comuni,familia) %>% 
              rename(presen_1 = presen,
                     comuni_1 = comuni,
                     famil_1 = familia),
            by=c("id_uni"))

# Matching to id_unim -----
tmp1967selfmom <- parentalapr06 %>% 
  dplyr::select(id_uni,id_unim,master) %>% 
  dplyr::filter(master==1,!is.na(id_unim)) %>% 
  full_join(perf1967 %>% 
              # no variable "fam" in perf1967
              dplyr::select(id_uni,presen,comuni,fam),
            by=c("id_unim"="id_uni"))  %>% 
  rename(presen_2 = presen,
         comuni_2 = comuni,
         famil_2 = fam) %>% 
  dplyr::select(-id_unim,-master) %>%
  # assert _m=~1 : observation in tmp1967self only? https://www.stata.com/manuals13/dmerge.pdf >> Pg 3
  right_join(tmp1967self,
             by=c("id_uni"))

tmp1975selfmom <- parentalapr06 %>% 
  dplyr::select(id_uni,id_unim,master) %>% 
  dplyr::filter(master==1,!is.na(id_unim)) %>% 
  full_join(perf1975 %>% 
              dplyr::select(id_uni,presen,comuni,familia),
            by=c("id_unim"="id_uni"))  %>% 
  rename(presen_2 = presen,
         comuni_2 = comuni,
         famil_2 = familia) %>% 
  dplyr::select(-id_unim,-master) %>% 
  # assert _m=~1 : observation in tmp1975self only? https://www.stata.com/manuals13/dmerge.pdf >> Pg 3
  
  right_join(tmp1975self,
             by=c("id_uni"))

# Matching to id_unip -----
tmp1967selfmomdad <- parentalapr06 %>% 
  dplyr::select(id_uni,id_unip,master) %>% 
  dplyr::filter(master==1,!is.na(id_unip)) %>% 
  full_join(perf1967 %>% 
              # no variable "fam" in perf1967
              dplyr::select(id_uni,presen,comuni,fam),
            by=c("id_unip"="id_uni"))  %>% 
  rename(presen_3 = presen,
         comuni_3 = comuni,
         famil_3 = fam) %>% 
  dplyr::select(-id_unip,-master) %>% 
  right_join(tmp1967selfmom,
             by=c("id_uni"))

tmp1975selfmomdad <- parentalapr06 %>% 
  dplyr::select(id_uni,id_unip,master) %>% 
  dplyr::filter(master==1,!is.na(id_unip)) %>% 
  full_join(perf1975 %>% 
              dplyr::select(id_uni,presen,comuni,familia),
            by=c("id_unip"="id_uni"))  %>% 
  rename(presen_3 = presen,
         comuni_3 = comuni,
         famil_3 = familia) %>% 
  dplyr::select(-id_unip,-master) %>% 
  right_join(tmp1975selfmom,
             by=c("id_uni"))

# Finding best measure of srcses -----
tmp1967m <- tmp1967selfmomdad %>% 
  dplyr::mutate(srcses = case_when(
    presen_1 == 1 & !is.na(famil_1) & !is.na(comuni_1) ~ 1,
    presen_2 == 1 & !is.na(famil_2) & !is.na(comuni_2) ~ 2,
    presen_3 == 1 & !is.na(famil_3) & !is.na(comuni_3) ~ 3,
    TRUE ~ 0
  ),
  
  comuni = case_when(srcses == 1 ~ comuni_1,
                     srcses == 2 ~ comuni_2,
                     srcses == 3 ~ comuni_3,
                     TRUE ~ NA_real_),
  
  familia = case_when(srcses == 1 ~ famil_1,
                      srcses == 2 ~ famil_2,
                      srcses == 3 ~ famil_3,
                      TRUE ~ NA_real_)
  
  ) %>% 
  dplyr::select(-starts_with("famil_"),-starts_with("comuni_"),-starts_with("presen_")) %>% 
  mutate(census = 1967)
rm(tmp1967self,tmp1967selfmom,tmp1967selfmomdad)


tmp1975m <- tmp1975selfmomdad %>% 
  dplyr::mutate(srcses = case_when(
    presen_1 == 1 & !is.na(famil_1) & !is.na(comuni_1) ~ 1,
    presen_2 == 1 & !is.na(famil_2) & !is.na(comuni_2) ~ 2,
    presen_3 == 1 & !is.na(famil_3) & !is.na(comuni_3) ~ 3,
    TRUE ~ 0
  ),
  
  comuni = case_when(srcses == 1 ~ comuni_1,
                     srcses == 2 ~ comuni_2,
                     srcses == 3 ~ comuni_3,
                     TRUE ~ NA_real_),
  
  familia = case_when(srcses == 1 ~ famil_1,
                      srcses == 2 ~ famil_2,
                      srcses == 3 ~ famil_3,
                      TRUE ~ NA_real_)
  
  ) %>% 
  dplyr::select(-starts_with("famil_"),-starts_with("comuni_"),-starts_with("presen_")) %>% 
  mutate(census = 1975)
rm(tmp1975self,tmp1975selfmom,tmp1975selfmomdad)
