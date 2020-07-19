
ne_raw <- read.csv("data-raw/geo_raw_example.csv", na.strings = "")
ne_ref <- read.csv("data-raw/geo_ref_example.csv", na.strings = "")

# ne_raw <- readxl::read_xlsx("data-raw/geo_raw_example.xlsx")
# ne_ref <- readxl::read_xlsx("data-raw/geo_ref_example.xlsx")
#
# ne_raw <- as.data.frame(ne_raw)
# ne_ref <- as.data.frame(ne_ref)

ne_ref$hcode <- hmatch::hcodes_int(ne_ref, "^adm")

usethis::use_data(ne_raw,
                  ne_ref,
                  overwrite = TRUE)


# ### build package and unzip into drc-ebola-linelist-cleaning repo
# devtools::build(binary = TRUE, args = c('--preclean'))
# system("tar zxvf ~/hmatch_0.1.0.tgz -C ~/drc-ebola-linelist-cleaning/renv/library/R-4.0/x86_64-apple-darwin17.0/")

