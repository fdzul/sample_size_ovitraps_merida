# Step 1. load the datasets ###
load("D:/OneDrive/proyects/colaboraciones/internacionales/sample_size_ovitraps/8.RData/test_data_bootstrap.RData")
load("~/Library/CloudStorage/OneDrive-Personal/proyects/colaboraciones/internacionales/sample_size_ovitraps/8.RData/test_data_bootstrap.RData")


# Step 2. create the  bootstrap function ####
sample_ovitraps_bootstrap <- function(dataframe, size, nboots){
    
    # Step 1. create the container ####
    bootstrap <- vector(length = nboots, mode = "list")
    bootstrap_mean <- vector(length = nboots, mode = "list")
    
    # Step 2. make the loop ####
    for(i in seq_along(bootstrap)) {
        bootstrap[[i]] <-  dplyr::sample_n(tbl = dataframe,
                                           size = size,
                                           replace = TRUE)
    }
    
    
    # Step 3. bootstrap ####
    
    # Step 3.1 make the function for calculate the mean and
    calculate_mean <- function(x){
        tibble::tibble(mean = mean(x$eggs),
                       var = var(x$eggs))
    }
    
    multi_return <- function() {
        my_list <- list("bootstrap" = bootstrap,
                        "bootstrap_mean" = purrr::map_dfr(.x = bootstrap,
                                                          .f = calculate_mean,
                                                          .id = "id_boot") |>
                            dplyr::mutate(size = size))
        return(my_list)
    }
    multi_return()
    
}

# Step 3. calculate the bootstrap #####
dat <- tibble::tibble(mean = y$eggs,
                      var = y$eggs,
                      size = "original")

x20 <- dplyr::bind_rows(sample_ovitraps_bootstrap(dataframe = y, 
                                                  size = 4, 
                                                  nboots = 20)$bootstrap_mean,
                        sample_ovitraps_bootstrap(dataframe = y, 
                                                  size = 3, 
                                                  nboots = 20)$bootstrap_mean,
                        sample_ovitraps_bootstrap(dataframe = y, 
                                                  size = 2, 
                                                  nboots = 20)$bootstrap_mean,
                        sample_ovitraps_bootstrap(dataframe = y, 
                                                  size = 1, 
                                                  nboots = 20)$bootstrap_mean) |>
    dplyr::mutate(size = factor(size)) |>
    dplyr::mutate(n_boots = 20)

x100 <- dplyr::bind_rows( sample_ovitraps_bootstrap(dataframe = y, 
                                                    size = 4, 
                                                    nboots = 100)$bootstrap_mean,
                          sample_ovitraps_bootstrap(dataframe = y, 
                                                    size = 3, 
                                                    nboots = 100)$bootstrap_mean,
                          sample_ovitraps_bootstrap(dataframe = y, 
                                                    size = 2, 
                                                    nboots = 100)$bootstrap_mean,
                          sample_ovitraps_bootstrap(dataframe = y, 
                                                    size = 1, 
                                                    nboots = 100)$bootstrap_mean) |>
    dplyr::mutate(size = factor(size)) |>
    dplyr::mutate(n_boots = 100)

x500 <- dplyr::bind_rows( sample_ovitraps_bootstrap(dataframe = y, 
                                                    size = 4, 
                                                    nboots = 500)$bootstrap_mean,
                          sample_ovitraps_bootstrap(dataframe = y, 
                                                    size = 3, 
                                                    nboots = 500)$bootstrap_mean,
                          sample_ovitraps_bootstrap(dataframe = y, 
                                                    size = 2, 
                                                    nboots = 500)$bootstrap_mean,
                          sample_ovitraps_bootstrap(dataframe = y, 
                                                    size = 1, 
                                                    nboots = 500)$bootstrap_mean) |>
    dplyr::mutate(size = factor(size)) |>
    dplyr::mutate(n_boots = 500)


x1000 <- dplyr::bind_rows( sample_ovitraps_bootstrap(dataframe = y, 
                                                     size = 4, 
                                                     nboots = 1000)$bootstrap_mean,
                           sample_ovitraps_bootstrap(dataframe = y, 
                                                     size = 3, 
                                                     nboots = 1000)$bootstrap_mean,
                           sample_ovitraps_bootstrap(dataframe = y, 
                                                     size = 2, 
                                                     nboots = 1000)$bootstrap_mean,
                           sample_ovitraps_bootstrap(dataframe = y, 
                                                     size = 1, 
                                                     nboots = 1000)$bootstrap_mean) |>
    dplyr::mutate(size = factor(size)) |>
    dplyr::mutate(n_boots = 1000)



x <- dplyr::bind_rows(x20,
                      x100,
                      x500,
                      x1000)



ggplot2::ggplot(data = x,
                ggplot2::aes(y = mean,
                             x = size,
                             color = size)) +
    ggplot2::stat_summary() +
    ggplot2::geom_point(data = dat,
                        mapping = ggplot2::aes(y = mean,
                                               x = size,
                                               color = size))

ggplot2::ggplot(data = x,
                ggplot2::aes(y = mean,
                             x = size,
                             color = size)) +
    ggplot2::stat_summary() +
    ggplot2::facet_wrap(facets = "n_boots",
                        scales = "free_y") +
    ggplot2::stat_summary(data = dat,
                          mapping = ggplot2::aes(y = mean,
                                                 x = size,
                                                 color = size),
                          geom = "point")

