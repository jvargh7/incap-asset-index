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


# pca_df exploratory -----
indicator_df <- pcall  %>%
  dplyr::filter(census %in% years) %>% 
  mutate(piso_ord = case_when(piso_1 == 1 ~ 0,
                              piso_2 == 1 | piso_3 == 1 ~ 1,
                              piso_4 == 1 | piso_5 == 1 ~ 2,
                              TRUE ~ 0),
         techo_ord = case_when(techo_1 == 1 ~ 0,
                               techo_2 == 1 | techo_3 == 1  ~ 1,
                               techo_4 == 1 | techo_5 == 1 ~ 2,
                               TRUE ~ 0),
         pared_ord = case_when(pared_1 == 1 | pared_2 == 1 | pared_3 == 1  ~ 0,
                               pared_8 == 1 ~ 1, # Paul suggested 0 but earlier census waves code it high. Hence consensus at Medium
                               pared_4 == 1 | pared_5 == 1 | pared_6 == 1 ~ 1,
                               pared_7 == 1 ~ 2,
                               TRUE ~ 0),
         cocin_ord = case_when(cocin_1 ==1 | cocin_2 == 1 ~ 0,
                               cocin_3 == 1 ~ 1,
                               cocin_4 == 1 ~ 2,
                               TRUE ~ 0),
         poele_ord = case_when(poele_1 == 1 | poele_2 == 1 ~ 0,
                               poele_3 == 1 | poele_4 | poele_5 == 1 | poele_6 == 1 ~ 1,
                               poele_78 == 1 | poele_9 == 1 | poele_10 == 1 ~ 2,
                               TRUE ~ 0),
         letrin_ord = case_when(letrin_1 == 1 ~ 0,
                                letrin_2 == 1 | letrin_5 == 1 ~ 1,
                                letrin_3 == 1 | letrin_4 == 1 ~ 2,
                                TRUE ~ 0),
         abasag_ord = case_when(abasag_4 == 1 ~ 2,
                                (abasag_2 == 1 | abasag_3 == 1) & abasag_4 == 0 ~ 1,
                                abasag_1 == 1 ~ 0,
                                TRUE ~ 0),
         desag_ord = case_when(desag_3 == 1 | desag_4 == 1 ~ 2,
                               desag_2  == 1 & (desag_3 == 0 & desag_4 == 0) ~ 1,
                               desag_1  == 1 & (desag_2 ==0 & desag_3 == 0 & desag_4 == 0)   ~ 0,
                               TRUE ~ 0),
         garbage_ord = case_when(garbage_5 == 1 ~ 2,
                                 garbage_2 == 1 | garbage_3 == 1 | garbage_4 == 1 ~ 1,
                                 garbage_1 == 1 ~ 0,
                                 TRUE ~ NA_real_)
  ) %>% 
  mutate_at(vars(ends_with("_ord")), function(x) factor(x,levels=c(0,1,2), labels=c("low","med","high")))



# Polychoric ------

c_items <- "cuartv07"
d_items <- c(
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
  "luz"
)

p_items <- c(# vivien_4,
  "piso_ord",
  "techo_ord",
  "pared_ord",
  "cocin_ord",
  "poele_ord",
  "letrin_ord",
  "abasag_ord")

ordinal_cor_output <- (indicator_df %>% 
                         dplyr::select(c_items,d_items,p_items) %>% 
                         dplyr::mutate_all(~as.numeric(.)) %>%
                         # dplyr::select(-id_uni,-census,-comuni,-familia,
                         #               -pcall,-pcall_2,-pcall_3) %>%
                         psych::mixedCor(data=.,
                                         c= c_items,
                                         d = d_items,
                                         p = p_items,
                                         use="pairwise",
                                         method="pearson",
                                         correct = 0))
ordinal_cor_mat <- ordinal_cor_output$rho

x = indicator_df %>% 
  dplyr::select(c_items,d_items,p_items) %>% 
  dplyr::mutate_all(~as.numeric(.)) %>% as.matrix()
x[is.na(x)] <- 0
# PCA
ordinal_pca <- prcomp(ordinal_cor_mat)
l = ordinal_pca$rotation[,]
f = x %*% l
indicator_df[,c("ord_pca", paste0("ord_pca_",c(2,3)))] <- f[,1:3]

psych_pca <- psych::principal(r = ordinal_cor_mat, nfactors = 3, rotate = "none") # works if you actually give it the matrix
psych_pca$scores <- psych::factor.scores(x,psych_pca)   
# psych::biplot.psych(psych_pca)

# Factor Analysis
rotation_method = "varimax"
fm_method = "minres"
nfactors = 1
ordinal_efa <- psych::fa(ordinal_cor_mat,
                         n.obs = nrow(indicator_df),
                         nfactors = nfactors,fm=fm_method,rotate=rotation_method)
l = ordinal_efa$loadings[,]
f = x %*% l 

indicator_df[,c("ord_fa")] <- f

ordinal_efa$scores <- psych::factor.scores(x,ordinal_efa)   
# psych::biplot.psych(psych_pca)


r7 = cor(method = "spearman",indicator_df$ord_fa,pca05_df$pcall,use = "pairwise.complete.obs")
r8 = cor(method = "spearman",indicator_df$ord_pca,pca05_df$pcall,use = "pairwise.complete.obs")
r7a = cor(ordinal_efa$scores$scores[,1],pca05_df$pcall,method = "spearman",use="pairwise.complete.obs")
r8a = cor(psych_pca$scores$scores[,1],pca05_df$pcall,method = "spearman",use = "pairwise.complete.obs")