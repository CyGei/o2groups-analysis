calculate_mode <- function(x) {
    uniq_x <- unique(x)
    freq_x <- tabulate(match(x, uniq_x))
    uniq_x[which.max(freq_x)]
}
calculate_mode_expected <- function(x) {
    tab <- table(x) %>%
        as.data.frame() %>%
        mutate(
            expected = 1 / n(),
            observed = Freq / sum(Freq)
        )

    if (nrow(tab) < 1) {
        return(NA)
    } else if (any(tab$observed > 1.5 * tab$expected)) {
        return(tab$x[which.max(tab$observed)])
    } else {
        return(NA)
    }
}


plot_hexbin <- function(outcomes, predictors) {
    outcomes_df <-
        readRDS(here("analysis/simulation/data/model", "outcomes_df.rds"))

    outcomes_long <- outcomes_df %>%
        group_by(scenario, peak_coeff) %>%
        mutate(n_pairs = ifelse(row_number() == 1, sum(trials, na.rm = TRUE), NA)) %>%
        ungroup() %>%
        pivot_longer(
            cols = all_of(predictors),
            names_to = "predictor_name",
            values_to = "predictor_value"
        ) %>%
        pivot_longer(
            cols = all_of(outcomes),
            names_to = "outcome_name",
            values_to = "outcome_value"
        )

    hline_df <- data.frame(
        outcome_name = c("bias", "coverage", "sensitivity", "specificity"),
        target = c(0, 0.95, 1, 1)
    )



    cell_df <- do.call(
        "rbind",
        split(
            outcomes_long,
            interaction(outcomes_long$outcome_name, outcomes_long$predictor_name)
        ) |>
            lapply(function(d) {
                d <- d |> drop_na(predictor_value, outcome_value)

                # Check if the conditions are met for xbnds
                if (unique(d$outcome_name) == "specificity" &&
                    unique(d$predictor_name) == "delta") {
                    xbnds <- c(-1, 1)
                } else {
                    xbnds <- range(d$predictor_value)
                }

                hb <- hexbin::hexbin(
                    d$predictor_value,
                    d$outcome_value,
                    xbins = 70,
                    IDs = TRUE,
                    xbnds = xbnds
                )

                # Calculate the mode (most frequent) value for peak_coeff
                mode_peak_coeff <-
                    aggregate(d$peak_coeff, by = list(hb@cID), FUN = calculate_mode)

                cbind(
                    mode_peak_coeff,
                    frequency = hb@count / sum(hb@count),
                    X = hexbin::hcell2xy(hb)$x,
                    Y = hexbin::hcell2xy(hb)$y,
                    outcome_name = d$outcome_name[1],
                    predictor_name = d$predictor_name[1]
                )
            })
    )

    f_ref <-
        cell_df %>%
        filter(outcome_name == "bias" &
            predictor_name == "beta") %>%
        .$frequency %>%
        max()

    cell_df_edit <- cell_df %>%
        rename(peak_coeff = x) %>%
        mutate(
            freq = ifelse(frequency >= f_ref, f_ref, frequency),
            log_freq = log(freq) - min(log(freq)),
            scaled_alpha = (log_freq - min(log_freq)) / (max(log_freq) - min(log_freq))
        )
    percentile_max <- quantile(cell_df_edit$log_freq, 0.87)



    p_custom_hexbin <-
        cell_df_edit %>%
        ggplot() +
        facet_grid(outcome_name ~ predictor_name, scales = "free") +
        geom_hex(
            aes(
                x = X,
                y = Y,
                fill = as.double(peak_coeff),
                alpha = log_freq
            ),
            stat = "identity"
        ) +
        geom_hline(
            data = hline_df,
            aes(yintercept = target),
            col = "#4C4E52",
            linetype = "solid"
        ) +
        geom_smooth(
            data = outcomes_long,
            aes(
                x = predictor_value,
                y = outcome_value,
                color = as.double(peak_coeff),
                group = as.double(peak_coeff)
            ),
            se = FALSE,
            method = "gam",
            formula = y ~ s(x, bs = "cs", k = 7),
            linewidth = 0.5,
            alpha = 1
        ) +
        theme_classic() +
        scale_alpha_continuous(guide = "none") +
        scale_color_gradientn(
            colours = c("#3679c0", "black", "#ff007f"),
            values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
            guide = "none"
        ) +
        scale_fill_gradientn(
            colours = c("#3679c0", "black", "#ff007f"),
            values = scales::rescale(c(-0.5, -0.05, 0, 0.05, 0.5)),
            breaks = seq(0.7, 1.3, 0.1)
        ) +
        theme(
            panel.background = element_rect(fill = "white"),
            strip.text.x = element_text(size = 16, family = "serif"),
            strip.text.y = element_text(size = 16, family = "serif")
        ) +
        labs(
            fill = "Peak \n Coefficient",
            x = "Predictor value",
            y = "Metric value",
            caption = ""
        )

    p_custom_hexbin <- p_custom_hexbin +
        ggh4x::facetted_pos_scales(y = list(
            outcome_name == "bias" ~ scale_y_continuous(limits = c(-1, 2), breaks = seq(-2, 2, 0.5)),
            outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
            outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
            outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
        ))



    p_custom_hexbin <- p_custom_hexbin + scale_alpha_continuous(range = c(0, 1), limits = c(0, percentile_max), guide = "none")


    ggsave(
        here("analysis/simulation/plots", "hexbin.png"),
        plot = p_custom_hexbin,
        width = 16,
        height = 8,
        units = "in",
        dpi = 300
    )
}
