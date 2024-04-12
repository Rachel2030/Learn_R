# Solution for tutorial 4.5 in R

rm(list=ls())
library(reticulate)
library(ggplot2)

ss <- import("starsim")

# EXERCISE 5 of 5: find parameters that match the data
beta <- 0.35 # NB: best-fitting beta is not the same as in simple_abm!
dur_inf <- 10 # However, dur_inf is
seed <- 1

pars <- list(
    n_agents = 200,
    start = 0,
    end = 20,
    dt = 0.2,
    rand_seed = as.integer(seed),
    diseases = dict(
        type = 'sir',
        beta = beta,
        dur_inf = ss$expon(scale=dur_inf),
        init_prev = 5/200,
        p_death = 0
    ),
    networks = dict(
        type = 'static',
        n_contacts = 5
    )
)

# Define the data
data <- c(
 6,  11,  14,  15,  17,  20,  25,  31,  40,  47,  52,  57,  66,
 75,  85,  93,  99, 100, 107, 111, 112, 118, 123, 117, 120, 122,
122, 123, 123, 125, 122, 120, 119, 110, 112, 117, 115, 118, 117,
113, 115, 112, 111, 107, 105, 103, 101,  99, 100,  99,  98,  97,
 94,  93,  92,  91,  89,  88,  88,  87,  86,  83,  81,  80,  78,
 76,  76,  73,  72,  71,  71,  71,  69,  68,  67,  65,  61,  58,
 57,  56,  56,  54,  52,  51,  51,  49,  49,  47,  46,  46,  46,
 46,  45,  45,  45,  43,  43,  40,  37,  36
)
time <- seq(0, length(data)-1) * pars$dt


# Define the plotting function
plot_results <- function(sim) {
    # Pull out results
    res = sim$diseases$sir$results
    time = sim$yearvec[1:length(data)]
    model = res$n_infected[1:length(data)]
    mismatch <- mean(abs(data - model))
    df <- data.frame(
        time=time,
        Data=data,
        Model=model
    )

    res_plot <- ggplot(df, aes(x = time)) +
    geom_point(aes(y = Data, color = 'Data')) +
    geom_line(aes(y = Model, color = 'Model')) +
    labs(title = 'Number of people infected', x = 'Time', y = 'Number of Infected') +
    scale_color_manual(values = c('Data' = 'black', 'Model' = 'red'))

    print(res_plot)
}

# Create and run the simulation
sim = ss$Sim(pars)
sim$run()
plot_results(sim)