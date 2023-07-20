library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)

# Set target-specific options such as packages.
tar_option_set(
  packages = c("tidyverse", "janitor", "sf", "terra", "purrr"),
  error = "null")



source("R/functions.R")

# End this file with a list of target objects.
list(
  tar_target(spei_data,
             command = {
               read_csv("data/spei_extracted.csv")}),

  tar_target(sites_selected, {
   sites <- c("BSD", "BPD")
   return(sites)
  }),

  tar_target(visualiza_spei,
             command = plot_spei(spei_data = spei_data,
                                 spei_indices = c("spei03", "spei06", "spei09", "spei12"),
                                 sites = sites_selected,
                                 title = "Baza"),
             packages = c("tidyverse")),

  # points
  tar_target(geo_sites,
             command = prepare_shp(shp = "data/geoinfo/geo_pinos_lasa.shp"),
             packages = c("sf", "tidyverse", "terra")),

  tar_target(parcelas_metadata,
             command = get_metadata_parcelas(xlsx_path = "data/geoinfo/geo_pinos_lasa_metadatos.xlsx"),
             packages = c("readxl")),


  #tmin
  tar_target(tmin,
             command = get_clima(geo_sites = geo_sites,
                                 path_files = "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MINIMA_MENSUAL/InfGeografica/InfRaster/COG",
                                 var_name = "tm2_",
                                 suffix = "_COG"),
             packages = c("sf", "tidyverse", "terra", "purrr")),
  #tmax
  tar_target(tmax,
             command = get_clima(geo_sites = geo_sites,
                                 path_files = "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MAXIMA_MENSUAL/InfGeografica/InfRaster/COG",
                                 var_name = "tm3_",
                                 suffix = "_COG"),
             packages = c("sf", "tidyverse", "terra", "purrr")),
  #tmed
  tar_target(tmed,
             command = get_clima(geo_sites = geo_sites,
                                 path_files = "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MEDIA_MENSUAL/InfGeografica/InfRaster/COG",
                                 var_name = "tmed_",
                                 suffix = "_COG"),
             packages = c("sf", "tidyverse", "terra", "purrr")),

  #prec
  tar_target(prec,
             command = get_clima(geo_sites = geo_sites,
                                 path_files = "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/PRECIP_MENSUAL/InfGeografica/InfRaster/COG",
                                 var_name = "p_",
                                 suffix = "_COG"),
             packages = c("sf", "tidyverse", "terra", "purrr")),


  tar_target(save_climate,
             {
               write_csv(tmin, "data/rediam_tmin.csv")
               write_csv(tmax, "data/rediam_tmax.csv")
               write_csv(tmed, "data/rediam_tmed.csv")
               write_csv(prec, "data/rediam_prec.csv")
             }),

  ## Coplas
  tar_target(coplas,
             command = genera_coplas_ts(coplas_path = "data/coplas2019.csv",
                                        parcelas_metadata = parcelas_metadata))


)
