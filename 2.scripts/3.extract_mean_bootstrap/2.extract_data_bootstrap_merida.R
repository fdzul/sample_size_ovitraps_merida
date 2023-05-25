# Step 1. load the bootstrap dataset  ####
load("D:/OneDrive/proyects/colaboraciones/internacionales/sample_size_ovitraps/8.RData/ovitraps_bootstrap/ovitraps_bootstrap_merida_2012_2019.RData")

# Step 2. create the function #### 
extract_data <- function(x){
    x$bootstrap_data
}
extract_mean <- function(x){
    x$bootstrap_mean
}


# Step 3. extract
y1 <- y |>
    dplyr::mutate(data_bootstrap_1 =  purrr::map(.x = bootstrap_1,
                                                 .f = extract_data)) |>
    dplyr::select(-data, -bootstrap_1, -bootstrap_2, -bootstrap_3, -bootstrap_4) |>
    tidyr::unnest(cols = c(data_bootstrap_1)) |>
    dplyr::mutate(size = 1)

y2 <- y |>
    dplyr::mutate(data_bootstrap_2 =  purrr::map(.x = bootstrap_2,
                                                 .f = extract_data)) |>
    dplyr::select(-data, -bootstrap_1, -bootstrap_2, -bootstrap_3, -bootstrap_4) |>
    tidyr::unnest(cols = c(data_bootstrap_2)) |>
    dplyr::mutate(size = 2)

y3 <- y |>
    dplyr::mutate(data_bootstrap_3 =  purrr::map(.x = bootstrap_3,
                                                 .f = extract_data)) |>
    dplyr::select(-data, -bootstrap_1, -bootstrap_2, -bootstrap_3, -bootstrap_4) |>
    tidyr::unnest(cols = c(data_bootstrap_3)) |>
    dplyr::mutate(size = 3)

y4 <- y |>
    dplyr::mutate(data_bootstrap_4 =  purrr::map(.x = bootstrap_4,
                                                 .f = extract_data)) |>
    dplyr::select(-data, -bootstrap_1, -bootstrap_2, -bootstrap_3, -bootstrap_4) |>
    tidyr::unnest(cols = c(data_bootstrap_4)) |>
    dplyr::mutate(size = 4)

# Step 4. row binding datasets ####
data_ovitraps_bootstrap_merida <- dplyr::bind_rows(y1, y2, y3, y4)


# Step 5. nested dataset & bootstrap ####
save(data_ovitraps_bootstrap_merida, 
     file = "8.RData/ovitraps_bootstrap/bootstrap/data_ovitraps_bootstrap_merida_2012_2019.RData")
