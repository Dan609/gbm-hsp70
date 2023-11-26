# The analysis of  Laser scaning confocal images of ex vivo brain tumors
# Dan E. Bobkov, 2023
# Image analysis: get the RGB pixel intensities from folders with tif images
# Red - Mitochondria staining with TMRM
# Green - Hsp70 staining with Abs
################
# options(scipen=.1)
# par(mar = c(4,4,1,.5))
# Import libraries
#install.packages("BiocManager") 
#BiocManager::install("EBImage")
library(EBImage)
#library(xlsx)
library(raster)
library(ggpubr)
library(readxl)
#library(rJava)
#library(xlsxjars)
library(DescTools)
library(ggplot2)
library(dplyr)
library(ggsignif)
library(PMCMRplus)
library(tidyverse)
library(EnvStats)
library(PerformanceAnalytics)
library(tidyverse)
#setwd("~/Yandex.Disk.localized/....")
getwd()

# Choose dir function for mac <<<<<<<<<<<<<<<<<<
choose.dir <- function() {
  system("osascript -e 'tell app \"R\" to POSIX path of (choose folder with prompt \"Choose Folder:\")' > /tmp/R_folder",
         intern = FALSE, ignore.stderr = TRUE)
  p <- system("cat /tmp/R_folder && rm -f /tmp/R_folder", intern = TRUE)
  return(ifelse(length(p), p, NA))
}
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##### DATA FRAME with pixel intensities

df <- data.frame(name = character(),
                 zone = character(),
                 red_mean = integer(),
                 green_mean = integer(),
                 blue_mean = integer(),
                 red_median = integer(),
                 green_median = integer(),
                 blue_median = integer())

#######################################

########################################
######## SELECT FOLDERS WITH IMAGES ####
########################################
file_list_NegativeControl <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_NegativeControl


file_list_AUS <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_AUS





#GBM
file_list_BSV <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_BSV

file_list_YOI <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_YOI

file_list_SAA <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_SAA

file_list_SAA2 <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_SAA2

file_list_AAA <- list.files(path = , choose.dir(),
                             pattern = "tif",
                             all.files = FALSE,
                             full.names = TRUE, recursive = TRUE,
                             ignore.case = FALSE, include.dirs = FALSE,
                             no.. = FALSE)
file_list_AAA

file_list_SSF <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_SSF

file_list_KAS <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_KAS



file_list_GAA <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_GAA


file_list_HTA <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_HTA

file_list_SVE <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_SVE


file_list_GNG <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_GNG

file_list_MVI <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_MVI

file_list_GEN <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_GEN

file_list_LIM <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_LIM

file_list_TMS <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_TMS

file_list_BIV <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_BIV


file_list_MRH <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_MRH


###### START SCAN
start.time.global <- Sys.time()

# Start scan
start.time <- Sys.time()
for (file_name in file_list_NegativeControl) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'NegativeControl', # average pixel values in
                    zone = 'NegativeControl', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Epilepsy
# Start scan
start.time <- Sys.time()
for (file_name in file_list_AUS) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'AUS', # average pixel values in
                    zone = 'Epilepsy', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan


ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw()







# GBM
# Start scan
start.time <- Sys.time()
for (file_name in file_list_BSV) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'BSV', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw()

# Start scan
start.time <- Sys.time()
for (file_name in file_list_YOI) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'YOI', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw()

# Start scan
start.time <- Sys.time()
for (file_name in file_list_SAA) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'SAA', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw() + ylim(0, 50)

# Start scan
start.time <- Sys.time()
for (file_name in file_list_SAA2) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'SAA2', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_AAA) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'AAA', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_SSF) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'SSF', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_KAS) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'KAS', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_LAI) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'LAI', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw() + ylim(0, 50)

# Start scan
start.time <- Sys.time()
for (file_name in file_list_GAA) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'GAA', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_KYN) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'KYN', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
#start.time <- Sys.time()
#for (file_name in file_list_BTM) { 
#  my_image <- readImage(file_name)
#  tmp <- data.frame(name = 'BTM', # average pixel values in
#                    zone = 'GBM', #
#                    red_mean =   apply(my_image, 3, mean)[1], # Red
#                    green_mean = apply(my_image, 3, mean)[2], # Green
#                    blue_mean =  apply(my_image, 3, mean)[3],
#                    red_median =   apply(my_image, 3, median)[1], # Red
#                    green_median = apply(my_image, 3, median)[2], # Green
#                    blue_median =  apply(my_image, 3, median)[3]) # Blue
#  df <- rbind(df, tmp)}
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_HTA) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'HTA', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_SVE) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'SVE', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_GNG) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'GNG', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_MVI) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'MVI', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan


# Start scan
start.time <- Sys.time()
for (file_name in file_list_GEN) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'GEN', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_LIM) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'LIM', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw() + ylim(0, 50)

# Start scan
start.time <- Sys.time()
for (file_name in file_list_TMS) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'TMS', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan


# Start scan
start.time <- Sys.time()
for (file_name in file_list_BIV) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'BIV', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

# Start scan
start.time <- Sys.time()
for (file_name in file_list_MRH) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'MRH', # average pixel values in
                    zone = 'GBM', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

end.time.global <- Sys.time()
time.taken.global <- end.time.global - start.time.global
paste('Glolbal time difference:', time.taken.global, 'sec')
# END SCAN



######### RESULTS
write.csv(df,'df.csv')
###########################





ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw() + xlim(-.1, .1) + ylim(0, 100)

ggplot(data = df, aes(x = green_mean, group = zone, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw() + xlim(-.1, .1)

df1 <- df %>% 
       filter(name != 'NegativeControl')

ggplot(data = df1, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw()

######################################################










shapiro.test(df$red_mean)
ks.test(df$red_mean, 'pnorm')

x <- df[df$zone == 'Before', ]$red_median
y <- df[df$zone == 'After', ]$red_median

x <- df[df$zone == 'Before', ]$red_mean
y <- df[df$zone == 'After', ]$red_mean

wilcox.test(x, y, alternative = "two.sided")

df$zone <- factor(df$zone, ordered = TRUE, 
                     levels = c("Before", "After"))

head(df)


# Plot weight by group and color by group

ggboxplot(df, x = "zone", y = "red_mean", 
          color = "zone", palette = c("#00AFBB", "#E7B800"),
          ylab = "Fluorescense intensity, a.u.", xlab = "Groups")

# Basic violin plot
p <- ggplot(df, aes(x=zone, y=red_mean)) + 
  geom_violin(trim=FALSE)
p + geom_dotplot(binaxis='y', stackdir='center',
                position=position_dodge(1))

# Data generation
# Plot correlation matrix



data <- df[, c(3:8)]

chart.Correlation(data, histogram = TRUE, method = "pearson")





# Use semi-transparent fill
p <- ggplot(df, aes(x=red_mean, fill=zone)) +
  geom_density(alpha=0.4) + scale_fill_brewer(palette="Accent")+
  labs(title="red_mean",x="TMRM level", y = "Density")
p
# Calculate the mean of each group
mu <- ddply(df, "zone", summarise, grp.mean=mean(red_mean))
head(mu)
# Add mean lines
p + geom_vline(data=mu, aes(xintercept=grp.mean, color=zone),
    linetype="dashed") + scale_color_brewer(palette="Accent") + theme_classic()


# Use semi-transparent fill
p <- ggplot(df, aes(x=red_median, fill=zone)) +
  geom_density(alpha=0.4) + scale_fill_brewer(palette="Accent")+
  labs(title="red_median",x="TMRM level", y = "Density")
p
# Calculate the mean of each group
mu <- ddply(df, "zone", summarise, grp.mean=mean(red_median))
head(mu)
# Add mean lines
p + geom_vline(data=mu, aes(xintercept=grp.mean, color=zone),
    linetype="dashed") + scale_color_brewer(palette="Accent") + theme_classic()

































# Start scan
start.time <- Sys.time()
for (file_name in file_list_RVV) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'RVV', # average pixel values in
                    zone = 'Epilepsy', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw()

# Start scan
start.time <- Sys.time()
for (file_name in file_list_SDA) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'SDA', # average pixel values in
                    zone = 'Epilepsy', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw()

# Start scan
start.time <- Sys.time()
for (file_name in file_list_VKS) { 
  my_image <- readImage(file_name)
  tmp <- data.frame(name = 'VKS', # average pixel values in
                    zone = 'Epilepsy', #
                    red_mean =   apply(my_image, 3, mean)[1], # Red
                    green_mean = apply(my_image, 3, mean)[2], # Green
                    blue_mean =  apply(my_image, 3, mean)[3],
                    red_median =   apply(my_image, 3, median)[1], # Red
                    green_median = apply(my_image, 3, median)[2], # Green
                    blue_median =  apply(my_image, 3, median)[3]) # Blue
  df <- rbind(df, tmp)}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# End scan

ggplot(data = df, aes(x = green_mean, group = name, fill = name)) + 
  geom_density(alpha = 0.5) + theme_bw()

# Start scan
#start.time <- Sys.time()
#for (file_name in file_list_SRA) { 
#  my_image <- readImage(file_name)
#  tmp <- data.frame(name = 'SRA', # average pixel values in
#                    zone = 'Epilepsy', #
#                    red_mean =   apply(my_image, 3, mean)[1], # Red
#                    green_mean = apply(my_image, 3, mean)[2], # Green
#                   blue_mean =  apply(my_image, 3, mean)[3],
#                    red_median =   apply(my_image, 3, median)[1], # Red
#                    green_median = apply(my_image, 3, median)[2], # Green
#                    blue_median =  apply(my_image, 3, median)[3]) # Blue
#  df <- rbind(df, tmp)}
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken
# End scan
file_list_RVV <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_RVV

file_list_SDA <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_SDA

#file_list_SRA <- list.files(path = , choose.dir(),
#                            pattern = "tif",
#                            all.files = FALSE,
#                            full.names = TRUE, recursive = TRUE,
#                            ignore.case = FALSE, include.dirs = FALSE,
#                            no.. = FALSE)
#file_list_SRA

file_list_VKS <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_VKS


file_list_LAI <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_LAI

file_list_KYN <- list.files(path = , choose.dir(),
                            pattern = "tif",
                            all.files = FALSE,
                            full.names = TRUE, recursive = TRUE,
                            ignore.case = FALSE, include.dirs = FALSE,
                            no.. = FALSE)
file_list_KYN

#file_list_BTM <- list.files(path = , choose.dir(),
#                            pattern = "tif",
#                            all.files = FALSE,
#                            full.names = TRUE, recursive = TRUE,
#                            ignore.case = FALSE, include.dirs = FALSE,
#                            no.. = FALSE)
#file_list_BTM
