path_census <- paste0(path_incap_ses_box,"/BDGT_oriente/Census")



popdemogind <- read_dta(paste0(path_incap_ses_box,
                               "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                               "popdemogind.dta"))
popdemoind67 <- read_dta(paste0(path_incap_ses_box,
                                "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                                "popdemogind67.dta"))

years = c(0,1967,1975,1987,2002,2016,2018)

source(paste0(path_incap_repo,"/ses/ses01_pc04.R"))
source(paste0(path_incap_repo,"/ses/ses02_pc16.R"))
source(paste0(path_incap_repo,"/ses/ses03_pc18.R"))
source(paste0(path_incap_repo,"/ses/ses04_pcall update.R"))
source(paste0(path_incap_repo,"/ses/ses05_pcadf.R"))


# Modified Maluccio 2005 -----
pca20_df <- bind_cols(pca05_df,
                      pcall %>%
                        dplyr::filter(census %in% years) %>%
                        dplyr::select(
                          video,
                          equipo,
                          compu,
                          telef,
                          molino,
                          washingmachine,
                          plancha, 
                          garbage_5,
                          desag_high))



# pca_df exploratory -----
pca20_df[is.na(pca20_df)] <- 0

pca20_df %>% 
  dplyr::filter(census %in% c(1965:1999)) %>% 
  compareGroups(census~.-id_uni -familia -comuni,data=.)  %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)

pca20_df %>% 
  dplyr::filter(census %in% c(2002:2018)) %>% 
  compareGroups(census~.-id_uni -familia -comuni,data=.)  %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)

## Running PCA -----


pca20_obj = pca20_df   %>%
  dplyr::select(-id_uni,-census,-comuni,-familia,
                # -tocad,
                -molino,-plancha,
                -garbage_5,
                -contains("pcall"))  %>% 
  prcomp(.)


pca20_obj$rotation %>% 
  data.frame() %>% 
  mutate(row = rownames(.)) %>% 
  mutate_at(vars(starts_with("PC")),~round(.,2)) %>% 
  write.csv(.,paste0(path_incap_repo,"/ses/pca20_df_rotation.csv"))


pca20_df[,c("pcall20", paste0("pcall20_",c(2,3)))] <- pca20_obj$x[,1:3]*-1

# Creating ses20_masters.RDS
pcall <- pca20_df %>% 
  dplyr::select(-pcall,-pcall_2,-pcall_3) %>% 
  rename(pcall = pcall20,
         pcall_2 = pcall20_2,
         pcall_3 = pcall20_3)

source(paste0(path_incap_repo,"/ses/ses06_matching for 1967-1975.R"))
source(paste0(path_incap_repo,"/ses/ses07_pc6775.R"))
source(paste0(path_incap_repo,"/ses/ses08_pc87.R"))
source(paste0(path_incap_repo,"/ses/ses10_pc02.R"))




ses20_masters <- pc6775 %>% 
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
  
  left_join(pcall %>% 
              dplyr::filter(census == 2016) %>% 
              dplyr::select(id_uni,pcall:pcall_3) %>%
              dplyr::rename(pcall2016_1 = pcall,
                            pcall2016_2 = pcall_2,
                            pcall2016_3 = pcall_3),
            by = "id_uni") %>% 
  
  left_join(pcall %>% 
              dplyr::filter(census == 2018) %>% 
              dplyr::select(id_uni,pcall:pcall_3) %>%
              dplyr::rename(pcall2018_1 = pcall,
                            pcall2018_2 = pcall_2,
                            pcall2018_3 = pcall_3),
            by = "id_uni")


ses20_masters %>% 
  dplyr::select(-comuni_67,-comuni_75,
                -familia_67,-familia_75,
                -srcses_67,-srcses_75,
                -fechan,-master) %>% 
  
  compareGroups(comun~.-id_uni,data=.) %>% 
  createTable(show.n=TRUE) %>% 
  export2md(.)  

# save -----
saveRDS(ses20_masters, paste0(path_incap_ses_dfa,"/ses20_masters.RDS"))
write_dta(ses20_masters, paste0(path_incap_ses_dfa,"/ses20_masters.dta"),version=12)
write_csv(ses20_masters, paste0(path_incap_ses_dfa,"/ses20_masters.csv"))


ses20_masters <- readRDS(paste0(path_incap_ses_dfa,"/ses20_masters.RDS"))

# CORRELATION ----------
ggplot(data=pca20_df,aes(x=pcall,y=pcall20)) +
  facet_wrap(~census) +
  geom_point(col="grey") +
  geom_smooth(method="lm")+
  stat_cor()+
  xlab("Original Index (z-scores)") +
  ylab("Modified Index (z-scores)") +
  theme_bw() +
  ggtitle("Correlation at Household Level")