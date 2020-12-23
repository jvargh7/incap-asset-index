path_census <- paste0(path_incap_ses_box,"/BDGT_oriente/Census")



popdemogind <- read_dta(paste0(path_incap_ses_box,
                               "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                               "popdemogind.dta"))  %>% 
  mutate_at(vars(comuni,familia,census,id_uni),~as.numeric(.))

perf <- bind_rows(
  read_dta(paste0(path_census,"/transversales/data/stata_clean/perf1967.dta")) %>%
    mutate(census = 1967) %>% 
    mutate_if(is.labelled,~as.numeric(.)),
  read_dta(paste0(path_census,"/transversales/data/stata_clean/perf1975.dta")) %>%
    mutate(census = 1975) %>% 
    mutate_if(is.labelled,~as.numeric(.)),
  read_dta(paste0(path_census,"/transversales/data/stata_clean/perf1987.dta")) %>%
    mutate(census = 1987) %>% 
    mutate_if(is.labelled,~as.numeric(.)),
  read_dta(paste0(path_census,"/transversales/data/stata_clean/perf1996.dta")) %>%
    mutate(census = 1996) %>% 
    mutate_if(is.labelled,~as.numeric(.)),
  read_dta(paste0(path_census,"/transversales/data/stata_clean/perf2002.dta")) %>%
    mutate(census = 2002) %>% 
    mutate_if(is.labelled,~as.numeric(.)),
  read_dta(paste0(path_census,"/transversales/data/stata_clean/perm2004.dta")) %>%
    mutate(census = 2004) %>% 
    mutate_if(is.labelled,~as.numeric(.))
)


years = c(1967,1975,1987,2002,2016,2018)

source(paste0(path_incap_repo,"/ses/ses01_pc04.R"))
source(paste0(path_incap_repo,"/ses/ses02_pc16.R"))
source(paste0(path_incap_repo,"/ses/ses03_pc18.R"))
source(paste0(path_incap_repo,"/ses/ses04_pcall update.R"))
source(paste0(path_incap_repo,"/ses/ses05_pcadf.R"))

pcall <- pca05_df

source(paste0(path_incap_repo,"/ses/ses06_matching for 1967-1975.R"))
source(paste0(path_incap_repo,"/ses/ses07_pc6775.R"))
source(paste0(path_incap_repo,"/ses/ses08_pc87.R"))
source(paste0(path_incap_repo,"/ses/ses10_pc02.R"))


ses_masters <- pc6775 %>% 
  dplyr::rename(pcall6775_1 = pcall6775,
                pcall1967_1 = pcall67,
                pcall1967_2 = pcall67_2,
                pcall1967_3 = pcall67_3,
                
                pcall1975_1 = pcall75,
                pcall1975_2 = pcall75_2,
                pcall1975_3 = pcall75_3
  ) %>% 
  left_join(pc87 %>% 
              dplyr::select(id_uni,pcall:pcall_3) %>% 
              dplyr::rename(pcall1987_1 = pcall,
                            pcall1987_2 = pcall_2,
                            pcall1987_3 = pcall_3),
            by = "id_uni") %>% 
  

  
  left_join(pc02 %>%
              dplyr::select(id_uni,pcall:pcall_3) %>%
              dplyr::rename(pcall2002_1 = pcall,
                            pcall2002_2 = pcall_2,
                            pcall2002_3 = pcall_3),
            by = "id_uni") %>%
  
  left_join(meta_master %>% 
              left_join(pcall %>% 
                          dplyr::filter(census == 2016),
                        by = c("id_selected"="id_uni")) %>%
              dplyr::select(id_uni,pcall:pcall_3) %>%
              dplyr::rename(pcall2016_1 = pcall,
                            pcall2016_2 = pcall_2,
                            pcall2016_3 = pcall_3),
            by = "id_uni") %>% 
  
  left_join(gates_master %>% 
              left_join(pcall %>% 
                          dplyr::filter(census == 2018),
                        by = c("id_selected"="id_uni")) %>% 
              dplyr::select(id_uni,pcall:pcall_3) %>%
              dplyr::rename(pcall2018_1 = pcall,
                            pcall2018_2 = pcall_2,
                            pcall2018_3 = pcall_3),
            by = "id_uni")

# Check
table(!is.na(ses_masters$pcall2016_1))
table(!is.na(ses_masters$pcall2018_1))


ses_masters %>% 
  dplyr::select(-comuni_67,-comuni_75,
                -familia_67,-familia_75,
                -srcses_67,-srcses_75,
                -fechan,-master) %>% 
  
  compareGroups(comun~.-id_uni,data=.) %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)  

# save -----
saveRDS(ses_masters, paste0(path_incap_ses_dfa,"/ses_masters.RDS"))
write_dta(ses_masters, paste0(path_incap_ses_dfa,"/ses_masters.dta"),version=12)
write_csv(ses_masters, paste0(path_incap_ses_dfa,"/ses_masters.csv"))



