# Step 1. load the datasets ###
load("C:/Users/felip/Dropbox/projects/sample_size_ovitraps/8.RData/ovitraps_bootstrap/bootstrap/data_ovitraps_bootstrap_merida_2012_2019.RData")

#load("~/Library/CloudStorage/Dropbox/projects/sample_size_ovitraps/8.RData/results/year_week/inla_mod_year_week.RData")

# Step 2. create the formula ####
inla_eggs <- function(x, fam){
    INLA::inla(formula = eggs ~ arm + f(id_boot, model = "iid"),
               data = x,
               family = fam,
               verbose = TRUE,
               control.compute = list(dic = TRUE))
}

extract_IRR <- function(x){
    x$summary.fixed[2:4,c(1, 3, 5)] |>
        exp() |>
        tibble::rownames_to_column(var = "arm") |>
        dplyr::rename(IRR = mean,
                      Lower = `0.025quant`,
                      Upper = `0.975quant`)
}

# Step 3. run the model zinb1 ####
x  <- data_ovitraps_bootstrap_merida |>
    dplyr::mutate(arm = dplyr::case_when(size == 4 ~ "4_ovitraps",
                                         size == 3 ~ "3_ovitraps",
                                         size == 2 ~ "2_ovitraps",
                                         size == 1 ~ "1_ovitraps")) |>
    dplyr::mutate(arm = factor(arm), 
                  arm = relevel(arm, ref = "4_ovitraps")) |>
    dplyr::group_by(year, week) |>
    tidyr::nest() |>
    dplyr::mutate(inla_model = purrr::map(.x = data,
                                          .f = inla_eggs,
                                          fam = "zeroinflatednbinomial1")) |>
    dplyr::mutate(betas = purrr::map(.x = inla_model,
                                     .f = extract_IRR)) |>
    dplyr::select(-inla_model, -data) |>
    tidyr::unnest(cols = c(betas))



# Step 4. save ther results ####
save(x,
     file = "betas_inla_year_week.RData")














####################       

x_nb1  <- data_ovitraps_bootstrap_merida |>
    dplyr::mutate(arm = dplyr::case_when(size == 4 ~ "4_ovitraps",
                                         size == 3 ~ "3_ovitraps",
                                         size == 2 ~ "2_ovitraps",
                                         size == 1 ~ "1_ovitraps")) |>
    dplyr::mutate(arm = factor(arm), 
                  arm = relevel(arm, ref = "4_ovitraps")) |>
    dplyr::group_by(year, week) |>
    tidyr::nest() |>
    dplyr::mutate(inla_model = purrr::map(.x = data,
                                          .f = inla_eggs,
                                          fam = "nbinomial1"))
