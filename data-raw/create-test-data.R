
library(readxl)

drc_raw <- readxl::read_xlsx("data-raw/geo_raw_example.xlsx")
drc_ref <- readxl::read_xlsx("data-raw/geo_ref_example.xlsx")
drc_man <- readxl::read_xlsx("data-raw/geo_man_example.xlsx", col_types = "text")

drc_raw <- as.data.frame(drc_raw)
drc_ref <- as.data.frame(drc_ref)[,1:5]
# drc_ref$hcode <- hcodes_int(drc_ref, "adm", prefix = "P")
drc_man <- as.data.frame(drc_man)

usethis::use_data(drc_raw,
                  drc_ref,
                  drc_man,
                  overwrite = TRUE)


### build package and unzip into drc-ebola-linelist-cleaning repo
devtools::build(binary = TRUE, args = c('--preclean'))
system("tar zxvf ~/hmatch_0.1.0.tgz -C ~/drc-ebola-linelist-cleaning/renv/library/R-3.6/x86_64-apple-darwin15.6.0/")

