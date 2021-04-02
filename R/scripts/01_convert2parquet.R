

# script to convert csv's to parquet for smaller storage ####################
# download csv's from google drive here:
# https://drive.google.com/drive/u/1/folders/1kiITPSkY_QhI1M0IIRzbwZTDpM4ZrpyU

# make sure v4 is in the name, as these are the yolov4 outs.

# folder holding all of the csv's
source <- "/Users/erikhedlin/Downloads/drive-download-20201109T204142Z-001/"
f <- list.files(source, recursive = TRUE, full.names = TRUE, pattern = "*.csv")

year <- c()
site <- c()
names <- c()

# extract site and year so we can label the parquets
for (i in 1:length(f)){
  site[i] <- stringr::str_sub(f[i], max(str_locate_all(f[i], "/")[[1]][,2]+1))
  site[i] <- stringr::str_replace(site[i], "out.csv", "")
  year[i] <- stringr::str_sub(f[i], str_locate(f[i], "Results_")[,2]+1, str_locate(f[i], "Results_")[,2]+4)
  names[i] <- paste0(site[i], "_", year[i])
}


# read all site csv's into a list
list_df <- lapply(f, read_csv)


# destination to write parquets
destination <- "/Volumes/GoogleDrive/My Drive/NuWCRU/Analysis/NuWCRU/krmp_nest-cam/R/data/site_parquets/"

# write to parquet
for (i in 1:length(list_df)){
  arrow::write_parquet(list_df[[i]], paste0(destination, names[i], ".parquet"))
}
