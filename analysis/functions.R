# SPEI -------------------------------------------------------------------------
plot_spei <- function(spei_data, spei_indices, title, sites) {
  spei_selected <- spei_data |>
    filter(spei %in% spei_indices) |>
    filter(site %in% sites) |> filter(!is.na(value)) |>
    mutate(
      sign = ifelse(value > 0, "pos", "neg"),
      variable = recode(spei,
                        "spei01" = "1-month",
                        "spei03" = "3-month",
                        "spei06" = "6-month",
                        "spei09" = "9-month",
                        "spei12" = "12-month",
                        "spei24" = "24-month",
                        "spei48" = "48-month"
      )
    )

  p <- ggplot(spei_selected, aes(x = date, y = value, fill = sign)) +
    geom_bar(stat = "identity") +
    facet_grid(variable~site) +
    scale_fill_manual(values = c("pos" = "darkblue", "neg" = "red")) +
    theme_bw() +
    labs(
      title = title,
      x = "",
      y = "SPEI"
    ) +
    theme(
      strip.background = element_rect(fill = "white"),
      legend.position = "none"
    )

  return(p)
}

# GEOINFO: Points of interest --------------------------------------------------
## Prepare shapefile
prepare_shp <- function(shp) {

  pts <- sf::st_read(shp, quiet = TRUE)
  pts <- st_transform(pts, 25830)
  names(pts)[1] <- "site"
  v <- terra::vect(pts)

  return(v)
}
## Metadata
get_metadata_parcelas <- function(xlsx_path, sheet=NULL) {
  x <- readxl::read_xlsx(xlsx_path, sheet=sheet)
  return(x)
}

# CLIMATE ----------------------------------------------------------------------
# Get climate
get_clima <- function(geo_sites, path_files, var_name, suffix){

  files <- list.files(path = path_files, pattern = ".tif$", full.names = TRUE)

  g0 <- terra::extract(rast(files[1]), geo_sites, xy= TRUE)

  g_list <- files[-1] |>
    map(~ rast(.x) |> extract(geo_sites, xy = TRUE))

  g0 <- reduce(g_list, inner_join, .init = g0)

  g0 <- cbind(site = geo_sites$site, g0)

  df <- g0 |>
    relocate(ID,x,y,site) |>
    pivot_longer(-c(ID, site,  x, y)) |>
    mutate(name = str_remove(name, var_name)) |>
    mutate(name = str_remove(name, suffix)) |>
    separate(name, into=c("year", "mes"))

  out <- df |>
    mutate(year = as.numeric(year))

  return(out)
}

# COPLAS -----------------------------------------------------------------------
## Get and prepare coplas
genera_coplas_ts <- function(coplas_path, parcelas_metadata) {

  coplas <- read_csv(coplas_path, show_col_types = FALSE) |>
    dplyr::filter(code %in% parcelas_metadata$coplas) |>
    right_join(parcelas_metadata, by=c("code"="coplas"), multiple = "all")

  return(coplas)

  }

## Plot coplas

# sites <- c("NSH","NSD","BAH","BSD","NAH1","NAH2","ASH1","ASH2","NAD")
#
# d <- coplas |>
  # rename(code_coplas = code) |>
  # filter(code_site %in% sites) |>
  # filter(!is.na(code_coplas)) |>
  # dplyr::select(-c(prov, sp, elev_min:elevF, area_ha, sp, perim, lat, long,
  #                  samplingDateSummer, samplingDateSpring, inside)) |>
  # relocate(sp_abrev:site, .after = elev_mean) |>
  # pivot_longer(cols = `1993`:`2019`, names_to = 'year')
#
# d |> ggplot(aes(x=year, y=value, group=code_site, colour=code_site)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~name_site, ncol = 1) +
#   theme_bw()


df <- read_csv("data/rediam_prec.csv")
sites <- c("NSH","NSD","BAH","BSD","NAH1","NAH2","ASH1","ASH2","NAD", "ASD")


df |> filter(site %in% sites) |>
  mutate(date = as.Date(paste(year,mes,'01', sep="-"), format = "%Y-%m-%d")) |>
  mutate(site_name = case_when(
    str_detect(site, '^[A]') ~ 'Almijara',
    str_detect(site, 'B') ~ 'Baza',
    str_detect(site, 'N') ~ 'Nevada',
  )) |>
  group_by(year, site, site_name) |>
  summarize(
    v = sum(value)
  ) |>
    ggplot(aes(x=year, y = v,  group=site, colour=site)) +
      geom_point() +
      geom_line() +
      theme_bw() +
  facet_wrap(~site_name, ncol = 1)



