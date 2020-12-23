library(ggpubr)
library(grid)

# DATASET --------
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

# FUNCTIONS ----------
hist_plot <- function(df,title="Title",nbins = 700){
  
  df %>% 
    ggplot(data=.,aes(x=pcall)) +
    # geom_histogram(bins = nbins) + 
    geom_histogram(aes(y = (..count..)/sum(..count..)),binwidth = 0.05) + 
    theme_bw() +
    ggtitle(title) +
    xlab("") +
    ylab("") +
    scale_x_continuous(limits=c(-5,4)) +
    scale_y_continuous(limits = c(0,0.15))
  
}

title_function = function(df,start){
  
  paste0(start," (n = ",nrow(df),")")
  
  
}

# PLOT ---------

pooled <- pca05_df %>% 
  hist_plot(.,title=title_function(.,"Pooled"),nbins = 800)


pool_1967 <- pca05_df %>%
  dplyr::filter(census == 1967) %>% 
  hist_plot(.,title=title_function(.,"Census 1967"),nbins = 400)
pool_1975 <- pca05_df %>%
  dplyr::filter(census == 1975) %>% 
  hist_plot(.,title=title_function(.,"Census 1975"),nbins = 400)
pool_1987 <- pca05_df %>%
  dplyr::filter(census == 1987) %>% 
  hist_plot(.,title=title_function(.,"Census 1987"),nbins = 400)
pool_2002 <- pca05_df %>%
  dplyr::filter(census == 2002) %>% 
  hist_plot(.,title=title_function(.,"Census 2002"),nbins = 400)
pool_2016 <- pca05_df %>%
  dplyr::filter(census == 2016) %>% 
  hist_plot(.,title=title_function(.,"Survey 2015-16"),nbins = 400)
pool_2018 <- pca05_df %>%
  dplyr::filter(census == 2018) %>% 
  hist_plot(.,title=title_function(.,"Survey 2017-18"),nbins = 400)


ggarrange(
  pool_1967,
  pool_1975,
  pool_1987,
  pool_2002,
  pool_2016,
  pool_2018,
  nrow=2,ncol = 3)