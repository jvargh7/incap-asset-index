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

# Year + Item ------------
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

ses_pearson <- expand.grid(years,items) %>% 
  data.frame() %>% 
  mutate(r = NA,
         Var2 = as.character(Var2))

for (row in 1:nrow(ses_pearson)){
  y = ses_pearson[row,]$Var1
  i = ses_pearson[row,]$Var2
  
  temp_pca_df = pca05_df %>% 
    dplyr::filter(census != y) %>% 
    dplyr::select(-one_of(i))
  
  temp_pca_obj = temp_pca_df %>% 
    dplyr::select(-id_uni,-census,-comuni,-familia,
                  -pcall,-pcall_2,-pcall_3) %>% 
    prcomp(.)
  
  temp_pca_df[,c("temp_pcall", paste0("temp_pcall_",c(2,3)))] <- temp_pca_obj$x[,1:3]
  
  r = paste0(
    # sprintf(abs(cor.test(temp_pca_df$temp_pcall,temp_pca_df$pcall)$estimate),fmt = "%0.2f")
    sprintf(abs(cor(temp_pca_df$temp_pcall,temp_pca_df$pcall,method = "spearman")),fmt = "%0.2f")
    # ,
    # " \r (p=",
    # sprintf(cor.test(temp_pca_df$temp_pcall,temp_pca_df$pcall)$p.value,fmt = "%0.3f"),
    # ")"
  )
  
  ses_pearson[row,]$r = r
  
}

ses_pearson %>%
  pivot_wider(names_from="Var1",values_from=r) %>%
  write.csv(.,paste0(path_dissertation_box,"aim 1/working/pearson correlation with reduced.csv"))


