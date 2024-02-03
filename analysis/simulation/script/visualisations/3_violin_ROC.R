# Palette -----------------------------------------------------------------
# https://www.hexcolortool.com/
#alpha_pal <- c("#570c4b", "#8e1380", "#c61cbd", "#ff2bff")
#alpha_pal <- colorRampPalette(c("#000080", "#0aa1ff"))(4)
alpha_pal <- c("#000080", "#0a74ff", "#0aadff", "#00d9ff")
# alpha_pal <- colorRampPalette(c("#006400", "#32CD32"))(4)
# alpha_pal <- rev(
#   monochromeR::generate_palette(
#     "#000080",
#     modification = "go_both_ways",
#     n_colours = 4,
#     view_palette = TRUE
#   )
#)

peak_coeff_pal_low <-
  colorRampPalette(c("orange", "white"))(4)
peak_coeff_pal_low <- c("#f05e16", "#fa9c1b", "#f7be6d", "#ffffff")
peak_coeff_pal_high <-
  colorRampPalette(c("white","#fa0079"))(length(peak_coeff_breaks) - 4 + 1) #"#ff00ff"

# peak_coeff_pal_high <-
#   c("#ff94c8","#f273b0", "#ff47a0", "#eb0071", "#b4005a", "#a4005a", "#94005a", "#84005a")
#remove first color of high palette
peak_coeff_pal_high <- peak_coeff_pal_high[-1]
peak_coeff_pal <- c(peak_coeff_pal_low, peak_coeff_pal_high)

# peak_coeff_pal <-
#   c(
#     "#ffa500",
#     "#bb5308",
#     "#902300",
#     "#000000",
#     "#570c4b",
#     "#720c65",
#     "#8e0a80",
#     "#aa079e",
#     "#c603bd",
#     "#e300dd",
#     "#ff00ff"
#   )

# peak_coeff_pal <-
#   c(
#     "#ff0016",
#     "#b00115",
#     "#66050d",
#     "#000000",
#     "#032f12",
#     "#004718",
#     "#00601c",
#     "#007a1d",
#     "#00951c",
#     "#00b115",
#     "#00cd00"
#   )


# Bias --------------------------------------------------------------------
p_bias <- summary_df %>%
  filter(alpha == 0.05) %>%
  select(peak_coeff, bias) %>%
  ggplot(aes(x = peak_coeff, y = bias)) +
  geom_violin(col = NA, fill = "gray50") +
  stat_summary(fun = mean, geom = "path", group = 1) +
  stat_summary(
    fun = mean,
    geom = "point",
    col = "black",
    size = 2.2
  ) +
  geom_hline(yintercept = 0,
             col = "black",
             lty = 2) +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  theme_publication() +
  labs(x = "Peak coefficient", y = "Bias")


# Coverage ----------------------------------------------------------------
hline_coverage <- hline_df %>%
  filter(outcome_name == "coverage") %>%
  mutate(alpha = list(alpha_breaks)) %>%
  unnest(alpha) %>%
  mutate(
    target = ifelse(outcome_name == "coverage", 1 - alpha, target),
    alpha = as.factor(alpha)
  )

p_coverage <- summary_df %>%
  select(alpha, peak_coeff, coverage) %>%
  group_by(alpha, peak_coeff) %>%
  summarise(coverage = mean(coverage, na.rm = TRUE)) %>%
  ggplot(aes(
    x = peak_coeff,
    y = coverage,
    group = alpha,
    colour = alpha
  )) +
  geom_path(key_glyph = draw_key_rect,
            show.legend = TRUE) +
  geom_point(size = 2.2,
             show.legend = FALSE) +
  geom_hline(
    data = hline_coverage,
    aes(yintercept = target,
        colour = alpha),
    lty = 2,
    show.legend = FALSE
  ) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0, 1, 0.1)) +
  scale_color_manual(name = "Significance level",
                     values = alpha_pal) +
  guide_colour_bar() +
  theme_publication() +
  labs(x = "Peak coefficient", y = "Coverage",) +
  theme(legend.key = element_rect(color = "black", size = 1),
        legend.title = ggtext::element_markdown(size = 12))


# ROC ---------------------------------------------------------------------
p_roc <- summary_df %>%
  mutate(FPR = 1 - specificity,
         TPR = sensitivity) %>%
  select(alpha, peak_coeff, FPR, TPR) %>%
  group_by(alpha, peak_coeff) %>%
  summarise(across(c(FPR, TPR), function(x)
    mean(x, na.rm = TRUE))) %>%
  mutate(is_peak = ifelse(peak_coeff == 1, TRUE, FALSE)) %>%
  ggplot(aes(x = FPR,
             y = TPR)) +
  geom_path(
    aes(group = alpha,
        color = alpha),
    size = 1,
    linejoin = "round",
    key_glyph = draw_key_rect
  ) +
  geom_point(
    aes(fill = peak_coeff,
        size = is_peak),
    shape = 21,
    colour = "black",
    stroke = 0.6,
    key_glyph = draw_key_rect
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "black",
    linetype = "dashed"
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1),
    expand = c(0, 0)
  )  +
  coord_fixed(clip = "off") +
  scale_colour_manual(name = "Alpha",
                      values = alpha_pal) +
  scale_fill_manual(name = "Peak coefficient",
                    values = peak_coeff_pal) +
  scale_size_discrete(name = "Peak coefficient",
                      range = c(3, 5),
                      guide = "none") +
  guide_colour_bar(key_color = "white") +
  guide_fill_bar(key_color = "black") +
  theme_publication() +
  labs(x = "1 - Specificity",
       y = "Sensitivity")+
  theme(legend.key = element_rect(color = "black", size = 1))

# Patchwork ---------------------------------------------------------------
p_bias <- p_bias + theme(axis.title.x = element_blank(),
                         plot.margin = unit(c(0, 0.2, 0, 1), 'lines'))
p_roc <- p_roc +
  guides(colour = "none")

p_coverage <-
  p_coverage + theme(plot.margin = unit(c(0, 0.2, 0.1, 1), 'lines'))

patch1 <- patchwork::wrap_plots((p_bias / p_coverage) | p_roc,
                                guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(10, "points"),
    plot.margin = unit(c(0, 0.35, 0.1, 0.1), 'lines')
  ) &
  patchwork::plot_annotation(tag_levels = 'A')



# Save --------------------------------------------------------------------
ggsave(
  here("analysis/simulation/plots", "patch1.png"),
  patch1,
  width = 14,
  height = 7,
  units = "in",
  dpi = 300
)



# Performance Analysis ----------------------------------------------------

perf <- summary_df %>%
  group_by(alpha, peak_coeff) %>%
  summarise(across(all_of(metrics), list(mean = ~mean(.x, na.rm = TRUE),
                                         sd = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(cols = -c(alpha, peak_coeff),
               names_to = "metric",
               values_to = "value") %>%
  separate(metric, c("outcome", "metric"), sep = "_")


cat("patch1.png \n")
rm(list = setdiff(ls(), ls_snapshot))
gc()
