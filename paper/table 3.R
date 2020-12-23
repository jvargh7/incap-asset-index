
# Cross-sectional Indices -----------

path_census <- paste0(path_incap_ses_box,"/BDGT_oriente/Census")



popdemogind <- read_dta(paste0(path_incap_ses_box,
                               "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                               "popdemogind.dta"))
popdemoind67 <- read_dta(paste0(path_incap_ses_box,
                                "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                                "popdemogind67.dta"))

years = c(1967,1975,1987,2002,2016,2018)



source(paste0(path_incap_repo,"/ses/ses01_pc04.R"))
source(paste0(path_incap_repo,"/ses/ses02_pc16.R"))
source(paste0(path_incap_repo,"/ses/ses03_pc18.R"))
source(paste0(path_incap_repo,"/ses/ses04_pcall update.R"))
source(paste0(path_incap_repo,"/ses/ses05_pcadf.R"))

items <- c("all",
           "radio",
           "tocad",
           "coser",
           "frig",
           "tv",
           "bike",
           "moto",
           "auto",
           "sitio90",
           "tenen_7",
           # vivien_4,
           "cuartv07",
           "piso_high",
           "techo_high",
           "pared_high",
           "cocin_34",
           "poele_high",
           "letrin_yes",
           "luz",
           "abasag_high")

ses_pca_obj = data.frame(
  year = numeric(),
  iteration = character(),
  item = character(),
  PC1 = numeric()
) %>% 
  bind_rows(pca05_obj$rotation %>% 
              data.frame() %>% 
              mutate(row = rownames(.)) %>% 
              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
              mutate(year = 0,
                     iteration = "all") %>% 
              dplyr::select(year, iteration, row,PC1) %>% 
              rename(item = row))

ses_pca_imp = data.frame(
  year = numeric(),
  iteration = character(),
  importance = numeric()
) %>% 
  bind_rows(summary(pca05_obj)$importance[2,] %>% 
              data.frame(.) %>% 
              rename_all(~paste0("importance",.)) %>% 
              rename(importance = importance.) %>% 
              mutate(row = rownames(.)) %>% 
              mutate(year = 0,
                     iteration = "all") %>% 
              dplyr::select(year, iteration, row, importance))

ses_spearman_cs <- expand.grid(years,items) %>% 
  data.frame() %>% 
  mutate(r = NA,
         Var2 = as.character(Var2))

for (row in 1:nrow(ses_spearman_cs)){
  y = ses_spearman_cs[row,]$Var1
  i = ses_spearman_cs[row,]$Var2
  
  temp_pca_df = pca05_df %>% 
    dplyr::filter(census == y) %>% 
    dplyr::select(-one_of(i))
  
  temp_pca_obj = temp_pca_df %>% 
    dplyr::select(-id_uni,-census,-comuni,-familia,
                  -pcall,-pcall_2,-pcall_3) %>% 
    temp_pca(.,scale_term)
  
  temp_pca_df[,c("temp_pcall", paste0("temp_pcall_",c(2,3)))] <- temp_pca_obj$x[,1:3]
  
  r = paste0(
    # sprintf(abs(cor.test(temp_pca_df$temp_pcall,temp_pca_df$pcall)$estimate),fmt = "%0.2f")
    sprintf(abs(cor(temp_pca_df$temp_pcall,temp_pca_df$pcall,method = "spearman")),fmt = "%0.2f")
    
    # ,
    # " \r (p=",
    # sprintf(cor.test(temp_pca_df$temp_pcall,temp_pca_df$pcall)$p.value,fmt = "%0.3f"),
    # ")"
  )
  ses_spearman_cs[row,]$r = r
  
  if(i == "all"){
    print(y)
    summary(temp_pca_obj) %>% print()
  }
  
  ses_pca_obj = bind_rows(ses_pca_obj,
                          temp_pca_obj$rotation %>% 
                            data.frame() %>% 
                            mutate(row = rownames(.)) %>% 
                            mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
                            mutate(year = y,
                                   iteration = i) %>% 
                            dplyr::select(year, iteration, row,PC1) %>% 
                            rename(item = row))
  
  ses_pca_imp = bind_rows(ses_pca_imp,
                          summary(temp_pca_obj)$importance[2,] %>% 
                            data.frame(.) %>% 
                            rename_all(~paste0("importance",.)) %>% 
                            rename(importance = importance.) %>% 
                            mutate(row = rownames(.)) %>% 
                            mutate(year = y, iteration = i) %>% 
                            dplyr::select(year, iteration, row, importance)
  )
  
}

# Urban/Rural indices --------------
path_census <- paste0(path_incap_ses_box,"/BDGT_oriente/Census")



popdemogind <- read_dta(paste0(path_incap_ses_box,
                               "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                               "popdemogind.dta"))
popdemoind67 <- read_dta(paste0(path_incap_ses_box,
                                "/RSPH Research/Organized data from old waves/Programs/Human capital programs/FNB/Dev_change/",
                                "popdemogind67.dta"))

source(paste0(path_incap_repo,"/structural/classify_urban_rural.R"))

years = c(0,1967,1975,1987,2002,2016,2018)

source(paste0(path_incap_repo,"/ses/ses01_pc04.R"))
source(paste0(path_incap_repo,"/ses/ses02_pc16.R"))
source(paste0(path_incap_repo,"/ses/ses03_pc18.R"))
source(paste0(path_incap_repo,"/ses/ses04_pcall update.R"))
source(paste0(path_incap_repo,"/ses/ses05_pcadf.R"))


pca05_ur_df <- pca05_df %>% 
  left_join(ur %>% 
              dplyr::select(iduni,urbano_rural2015,urbano_rural2018,urbano_rural),
            by = c("id_uni" = "iduni"))

pc18_ur_df <- pc18 %>% 
  full_join(ur %>% 
              dplyr::select(iduni,urbano_rural2015,urbano_rural2018,urbano_rural),
            by = c("id_uni" = "iduni"))


# Year + Item ------------
items <- c(
  "radio",
  "tocad",
  "coser",
  "frig",
  "tv",
  "bike",
  "moto",
  "auto",
  "sitio90",
  "tenen_7",
  # vivien_4,
  "cuartv07",
  "piso_high",
  "techo_high",
  "pared_high",
  "cocin_34",
  "poele_high",
  "letrin_yes",
  "luz",
  "abasag_high")


# Wealth Index for Urban 2015--------

pca_df_urban2015 = pca05_ur_df %>% 
  dplyr::filter(census == 2016,urbano_rural2015 == 0) 

pca05_u2015_obj = pca_df_urban2015 %>% 
  dplyr::select(one_of(items)) %>% 
  temp_pca(.,scale_term)

pca05_u2015_obj$rotation %>% 
  data.frame() %>% 
  mutate(row = rownames(.)) %>% 
  mutate_at(vars(starts_with("PC")),~round(.,2)) %>% 
  write.csv(.,paste0(path_incap_repo,"/ses/pca05_U2015_rotation.csv"))

pca_df_urban2015[,c("pcu15", paste0("pcu15_",c(2,3)))] <- pca05_u2015_obj$x[,1:3]*-1
cor.test(pca_df_urban2015$pcall,pca_df_urban2015$pcu15)

# Wealth Index for Rural 2015--------
pca_df_rural2015 = pca05_ur_df %>% 
  dplyr::filter(census == 2016,urbano_rural2015 == 1) 

pca05_r2015_obj = pca_df_rural2015 %>% 
  dplyr::select(one_of(items)) %>% 
  temp_pca(.,scale_term)

pca05_r2015_obj$rotation %>% 
  data.frame() %>% 
  mutate(row = rownames(.)) %>% 
  mutate_at(vars(starts_with("PC")),~round(.,2)) %>% 
  write.csv(.,paste0(path_incap_repo,"/ses/pca05_R2015_rotation.csv"))

pca_df_rural2015[,c("pcr15", paste0("pcr15_",c(2,3)))] <- pca05_r2015_obj$x[,1:3]
cor.test(pca_df_rural2015$pcall,pca_df_rural2015$pcr15)


# Wealth Index for Urban 2018--------

pca_df_urban2018 = pca05_ur_df %>% 
  dplyr::filter(census == 2018,urbano_rural2018 == 0) 

pca05_u2018_obj = pca_df_urban2018 %>% 
  dplyr::select(one_of(items)) %>% 
  temp_pca(.,scale_term)

pca05_u2018_obj$rotation %>% 
  data.frame() %>% 
  mutate(row = rownames(.)) %>% 
  mutate_at(vars(starts_with("PC")),~round(.,2)) %>% 
  write.csv(.,paste0(path_incap_repo,"/ses/pca05_U2018_rotation.csv"))

pca_df_urban2018[,c("pcu18", paste0("pcu18_",c(2,3)))] <- pca05_u2018_obj$x[,1:3]
cor.test(pca_df_urban2018$pcall,pca_df_urban2018$pcu18)

# Wealth Index for Rural 2018--------
pca_df_rural2018 = pca05_ur_df %>% 
  dplyr::filter(census == 2018,urbano_rural2018 == 1) 

pca05_r2018_obj = pca_df_rural2018 %>% 
  dplyr::select(one_of(items)) %>% 
  temp_pca(.,scale_term)

pca05_r2018_obj$rotation %>% 
  data.frame() %>% 
  mutate(row = rownames(.)) %>% 
  mutate_at(vars(starts_with("PC")),~round(.,2)) %>% 
  write.csv(.,paste0(path_incap_repo,"/ses/pca05_R2018_rotation.csv"))

pca_df_rural2018[,c("pcr18", paste0("pcr18_",c(2,3)))] <- pca05_r2018_obj$x[,1:3]
cor.test(pca_df_rural2018$pcall,pca_df_rural2018$pcr18)

# TABLE GENERATION ---------
index_labels = c("Harmonized","1967","1975","1987","2002","2016","2018")

ses_pca_obj %>% 
  dplyr::filter(iteration == "all") %>%
  mutate(strata = case_when(year == 0 ~ 1,
                            TRUE ~ 2)) %>% 
  mutate(index = paste0(year)) %>% 
  dplyr::select(index,strata,iteration,item,PC1) %>% 
  
  bind_rows(.,
            
            summary(pca05_r2015_obj)$rotation %>% 
              data.frame() %>% 
              mutate(row = rownames(.)) %>% 
              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
              mutate(index = "2016",
                     strata = 3,
                     iteration = "all") %>% 
              dplyr::select(index, strata, iteration, row,PC1) %>% 
              rename(item = row),
            
            summary(pca05_u2015_obj)$rotation %>% 
              data.frame() %>% 
              mutate(row = rownames(.)) %>% 
              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
              mutate(index = "2016",
                     strata = 4,
                     iteration = "all") %>% 
              dplyr::select(index, strata, iteration, row,PC1) %>% 
              rename(item = row),
            
            summary(pca05_r2018_obj)$rotation %>% 
              data.frame() %>% 
              mutate(row = rownames(.)) %>% 
              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
              mutate(index = "2018",
                     strata = 3,
                     iteration = "all") %>% 
              dplyr::select(index, strata, iteration, row,PC1) %>% 
              rename(item = row),
            
            summary(pca05_u2018_obj)$rotation %>% 
              data.frame() %>% 
              mutate(row = rownames(.)) %>% 
              mutate_at(vars(starts_with("PC")),~round(.,2)) %>%
              mutate(index = "2018",
                     strata = 4,
                     iteration = "all") %>% 
              dplyr::select(index, strata, iteration, row,PC1) %>% 
              rename(item = row)
            
  ) %>% 
  
  
  mutate(
    index = factor(index,levels=index_levels,labels=index_labels),
    item = factor(item,var2_levels,var2_labels),
    strata = factor(strata,levels=c(1:4),labels=c("Harmonized","Cross-sectional","Rural","Urban"))) %>% 
  mutate(PC1 = case_when(index == "Harmonized"  ~ PC1*-1,
                         index %in% c("1975","1987","2002","2016","2018") & strata == "Cross-sectional" ~ PC1*-1,
                         strata == "Rural" ~ PC1*-1,
                         index == "2016" & strata == "Urban" ~ PC1*-1,
                         TRUE ~ PC1),
         index_strata = case_when(index == "Harmonized" ~ as.character(index),
                                  TRUE ~ paste0(as.character(index)," ",as.character(strata))),
         item = as.character(item)
  ) %>%
  
  dplyr::select(-iteration,-index,-strata) %>% 
  pivot_wider(names_from="index_strata",values_from="PC1") %>% 
  write.csv(.,paste0(path_dissertation_box,"/aim 1/working/loadings of principal components.csv"))
