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

# Year + Year ------------

ses_pearson3 <- expand.grid(years,years) %>% 
  data.frame() %>% 
  mutate(r = NA)

for (row in 1:nrow(ses_pearson3)){
  y1 = ses_pearson3[row,]$Var1
  y2 = ses_pearson3[row,]$Var2
  
  temp_pca_df = pca05_df %>% 
    dplyr::filter(!census %in% c(y1,y2))
  
  temp_pca_obj = temp_pca_df %>% 
    dplyr::select(-id_uni,-census,-comuni,-familia,
                  -pcall,-pcall_2,-pcall_3) %>% 
    prcomp(.)
  
  temp_pca_df[,c("temp_pcall", paste0("temp_pcall_",c(2,3)))] <- temp_pca_obj$x[,1:3]
  
  r = paste0(
    sprintf(abs(cor(temp_pca_df$temp_pcall,temp_pca_df$pcall,method = "spearman")),fmt = "%0.2f")
   )
  
  ses_pearson3[row,]$r = r
  
}

ses_pearson3 %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_dissertation_box,"aim 1/working/pearson correlation with reduced_yearyear.csv"))
