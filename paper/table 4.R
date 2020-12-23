
# COMPARISON OF RESIDENTS ----------
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

pcall_orig <- pcall
pcall <- pca05_df

# source(paste0(path_incap_repo,"/ses/ses06_matching for 1967-1975.R"))
source(paste0(path_incap_repo,"/ses/ses07_pc6775.R"))

# 1967 --------------
master67 = pca05_df %>% 
  inner_join(pc6775 %>% 
               dplyr::select(comuni_67,familia_67) %>% 
               distinct(comuni_67,familia_67) %>% 
               dplyr::mutate(census = 1967),
             by = c("census","comuni"="comuni_67","familia"="familia_67")
  )
others67 = pca05_df %>% 
  anti_join(pc6775 %>% 
              dplyr::select(comuni_67,familia_67) %>% 
              distinct(comuni_67,familia_67) %>% 
              dplyr::mutate(census = 1967),
            by = c("census","comuni"="comuni_67","familia"="familia_67")
  ) %>% 
  dplyr::filter(census==1967)

nrow(master67) + nrow(others67) == nrow(pca05_df[pca05_df$census==1967,])

# 1975 --------------
master75 = pca05_df %>% 
  inner_join(pc6775 %>% 
               dplyr::select(comuni_75,familia_75) %>% 
               distinct(comuni_75,familia_75) %>% 
               dplyr::mutate(census = 1975),
             by = c("census","comuni"="comuni_75","familia"="familia_75")
  )

others75 = pca05_df %>% 
  anti_join(pc6775 %>% 
              dplyr::select(comuni_75,familia_75) %>% 
              distinct(comuni_75,familia_75) %>% 
              dplyr::mutate(census = 1975),
            by = c("census","comuni"="comuni_75","familia"="familia_75")
  ) %>% 
  dplyr::filter(census==1975)

nrow(master75) + nrow(others75) == nrow(pca05_df[pca05_df$census==1975,])

# 1987 --------------
source(paste0(path_incap_repo,"/ses/ses08_pc87.R"))
master87 = pca05_df %>% 
  inner_join(pc87 %>% 
               dplyr::select(comuni,familia) %>% 
               distinct(comuni,familia) %>% 
               dplyr::mutate(census = 1987),
             by = c("census","comuni","familia")
  )

others87 = pca05_df %>% 
  anti_join(pc87 %>% 
              dplyr::select(comuni,familia) %>% 
              distinct(comuni,familia) %>% 
              dplyr::mutate(census = 1987),
            by = c("census","comuni","familia")
  ) %>% 
  dplyr::filter(census==1987)

nrow(master87) + nrow(others87) == nrow(pca05_df[pca05_df$census==1987,])

# 2002 --------------
source(paste0(path_incap_repo,"/ses/ses10_pc02.R"))
master02 = pca05_df %>% 
  inner_join(pc02 %>% 
               dplyr::select(comuni,familia) %>% 
               distinct(comuni,familia) %>% 
               dplyr::mutate(census = 2002),
             by = c("census","comuni","familia")
  )

others02 = pca05_df %>% 
  anti_join(pc02 %>% 
              dplyr::select(comuni,familia) %>% 
              distinct(comuni,familia) %>% 
              dplyr::mutate(census = 2002),
            by = c("census","comuni","familia")
  ) %>% 
  dplyr::filter(census==2002)

nrow(master02) + nrow(others02) == nrow(pca05_df[pca05_df$census==2002,])

# Consolidate ----------------

non_pca_vars <- names(pcall_orig)[!names(pcall_orig) %in% names(pca05_df)]
pca_vars <- names(pcall_orig)[names(pcall_orig) %in% names(pca05_df)] %>% .[!. %in% c("census","familia","comuni","id_uni")]


compare_df <- bind_rows(master67 %>% 
                          mutate(master = 1),
                        others67 %>% 
                          mutate(master = 0),
                        
                        master75 %>% 
                          mutate(master = 1),
                        others75 %>% 
                          mutate(master = 0),
                        
                        master87 %>% 
                          mutate(master = 1),
                        others87 %>% 
                          mutate(master = 0),
                        
                        master02 %>% 
                          mutate(master = 1),
                        others02 %>% 
                          mutate(master = 0),
) %>% 
  
  mutate(master = factor(master,levels=c(0,1),labels=c("Other","Master"))) %>% 
  left_join(pcall_orig %>% 
              dplyr::select(census,comuni,familia,
                            
                            one_of(non_pca_vars)
                            
              ),
            by=c("census","comuni","familia"))

formula_compare = paste0("census ~ ",paste0(pca_vars,collapse=" + ")," + pcall")



location_continuous = match(pca_vars,"cuartv07")
location_continuous[is.na(location_continuous)] <- 2
location_continuous <- c(location_continuous,1)

compare_df <- compare_df %>% 
  mutate_at(vars(one_of(pca_vars)), function(x) case_when(is.na(x) ~ 0,
                                                          TRUE ~ x))




# Pooled dataset -----------
pooled_df <- bind_rows(compare_df,
                       pca05_df %>%
                         dplyr::filter(census %in% c(2016,2018)) %>% 
                         dplyr::mutate(master = "Master")) %>% 
  mutate(master = case_when(master == "Master" ~ "Cohort member's household",
                            master == "Other" ~ "Other household"))


pooled_df %>% 
  group_by(census) %>% 
  summarize(n = n(),
            mean_sd = paste0(mean(pcall) %>% round(.,2)," \u00B1 ",sd(pcall) %>% round(.,2)),
            median_iqr = paste0(median(pcall) %>% round(.,2)," (", 
                                quantile(pcall,0.25) %>% round(.,2)," , ",
                                quantile(pcall,0.75) %>% round(.,2),")"),
            range = paste0("[", min(pcall) %>% round(.,2)," , ",max(pcall) %>% round(.,2),"]"),
            iqr = (quantile(pcall,0.25) - quantile(pcall,0.75)) %>% round(.,2)
  ) %>% 
  knitr::kable(.)