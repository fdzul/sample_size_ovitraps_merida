# Step 1. load the ovitraps dataset ####
load("~/Library/CloudStorage/OneDrive-Personal/automatic_read_ovitraps/8.RData/ovitraps_coordinates/ovitraps_2012_2019_centroid_coordinates_ine_2010.RData")

x <- xy |>
    dplyr::filter(edo == "31") |>
    dplyr::filter(mpo == "050") |>
    dplyr::filter(loc == "0001") |>
    dplyr::filter(!is.na(longitude)) |>
    dplyr::filter(!is.na(eggs)) |>
    dplyr::mutate(month = lubridate::month(date, 
                                           label = TRUE, 
                                           abbr = FALSE),
                  eggs_binary = ifelse(eggs >= 1, 1, 0)) |>
    dplyr::select(-longitude, -latitude)

# Step 1.2 rename the object ####
table(is.na(x$latitude))

# Step 2. Aggregate the ovitrap dataset by #### 373204
library(data.table)
system.time(y <- x[ ,.(n = .N,
               n_pos = sum(eggs >= 1, na.rm = TRUE),
               n_neg = sum(eggs < 1, na.rm = TRUE),
               n_nas = sum(is.na(eggs)),
               eggs_mean = mean(eggs, na.rm = TRUE),
               eggs_var = var(eggs, na.rm = TRUE),
               egg_sd = sd(eggs, na.rm = TRUE),
               eggs_sum = sum(eggs, na.rm = TRUE),
               eggs_median = median(eggs, na.rm = TRUE)),
            by = .(edo, mpo, loc, sector, manzana, month, year, week)][n == 4, ][n_nas == 0,][eggs_sum > 0]
)


y <- x |>
    dplyr::group_by(edo, mpo, loc, sector, manzana, year, month, week) |>
    dplyr::summarise(n = dplyr::n(),
                     n_pos = sum(eggs >= 1, na.rm = TRUE),
                     n_neg = sum(eggs < 1, na.rm = TRUE),
                     n_nas = sum(is.na(eggs)),
                     eggs_mean = mean(eggs, na.rm = TRUE),
                     eggs_var = var(eggs, na.rm = TRUE),
                     egg_sd = sd(eggs, na.rm = TRUE),
                     eggs_sum = sum(eggs, na.rm = TRUE),
                     eggs_median = median(eggs, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(n == 4) |> # blocks with 4 ovitraps
    dplyr::filter(n_nas == 0) |> # blocks with o nas
    dplyr::filter(eggs_sum > 0) # blocks without 4 ovitraps negative


# Step 3. create the id for index matching
y[, id:= paste(edo, mpo, loc, sector, manzana, year, month,
               stringr::str_pad(as.character(week), 
                                width = 2, 
                                side = "left", 
                                pad = "0"), sep = "")]


# Step 4.1 matching #####
x <- as.data.table(x)
x[, id_dat := paste(edo, mpo, loc, sector, manzana, year, month,
                    stringr::str_pad(as.character(week), width = 2, 
                                     side = "left", pad = "0"), 
                    sep = "")]
data.table::setkey(x, id_dat)

key(x)
head(x)

# 4.2 matching #### 
vec_id <- y$id
#x <- x[.(c(vec_id))]
x <- x[list(vec_id)]

# Step 5. save the results ###
save(x, 
     file = "8.RData/ovitraps_for_sample_size_31_merida.RData")


