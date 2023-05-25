
# Step 1. load the dataset for bootstrap ####
load("~/Library/CloudStorage/Dropbox/projects/sample_size_ovitraps_merida/8.RData/ovitraps_for_sample_size_31_merida.RData")

# Step 2. create the  bootstrap function ####
sample_ovitraps_bootstrap <- function(x, size, nboots){
    
    # Step 1. create the container ####
    bootstrap <- vector(length = nboots, mode = "list")
    #bootstrap_mean <- vector(length = nboots, mode = "list")
    
    # Step 2. make the loop ####
    for(i in seq_along(bootstrap)) {
        bootstrap[[i]] <-  dplyr::sample_n(tbl = x,
                                           size = size,
                                           replace = TRUE)
    }
    
    
    # Step 3.0 bootstrap ####
    
    # Step 3.1 make the function for calculate the mean and
    calculate_mean <- function(x){
        tibble::tibble(mean = mean(x$eggs),
                       var = var(x$eggs))
    }
    
    # Step 3.2 make the function for calculate the mean and
    extract_bootstrap <- function(x){
        tibble::tibble(x)
    }
    
    multi_return <- function() {
        my_list <- list("bootstrap" = bootstrap,
                        "bootstrap_mean" = purrr::map_dfr(.x = bootstrap,
                                                          .f = calculate_mean,
                                                          .id = "id_boot") |>
                            dplyr::mutate(size = size),
                        "bootstrap_data" = purrr::map_dfr(.x = bootstrap,
                                                          .f = extract_bootstrap,
                                                          
                                                          
                                                          
                                                          
                                                          
                                                          .id = "id_boot"))
        return(my_list)
    }
    multi_return()
    
}

# Step 3. nested dataset & bootstrap #### 
y <- x |>
    dplyr::group_by(edo, mpo, loc, sector, manzana, 
                    year, month, week, id_block) |>
    tidyr::nest() |>
    dplyr::mutate(bootstrap_1 =  purrr::map(.x = data,
                                            .f = sample_ovitraps_bootstrap,
                                            size = 1, 
                                            nboots = 20),
                  bootstrap_2 =  purrr::map(.x = data,
                                            .f = sample_ovitraps_bootstrap,
                                            size = 2, 
                                            nboots = 20),
                  bootstrap_3 =  purrr::map(.x = data,
                                            .f = sample_ovitraps_bootstrap,
                                            size = 3, 
                                            nboots = 20),
                  bootstrap_4 =  purrr::map(.x = data,
                                            .f = sample_ovitraps_bootstrap,
                                            size = 4, 
                                            nboots = 20))



# Step 4. nested dataset & bootstrap ####
save(y, 
     file = "8.RData/ovitraps_bootstrap/ovitraps_bootstrap_merida_2012_2019.RData")
