

# Violin Plot 4 Metrics ---------------------------------------------------
summary <- summary_long %>%
  group_by(peak_coeff, alpha, outcome_name) %>%
  summarise(
    mean = mean(outcome_value, na.rm = TRUE),
    sd = sd(outcome_value, na.rm = TRUE),
    median = median(outcome_value, na.rm = TRUE),
    upper_quantile = quantile(outcome_value, 0.975, na.rm = TRUE),
    lower_quantile = quantile(outcome_value, 0.025, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(lowerCI = mean - 1.96 * (sd / sqrt(n)),
         upperCI = mean + 1.96 * (sd / sqrt(n))) %>%
  ungroup()


hline_violin <- hline_df %>%
  group_by(outcome_name) %>%
  mutate(alpha = list(alpha_breaks)) %>%
  unnest(alpha) %>%
  mutate(
    target = ifelse(outcome_name == "coverage", 1 - alpha, target),
    alpha = as.factor(alpha)
  )

p_violin <-
  ggplot(data = summary_long) +
  facet_wrap( ~ outcome_name,
              scales = "free") +
  geom_violin(
    aes(x = peak_coeff,
        y = outcome_value,
        fill = alpha),
    trim = TRUE,
    scale = "count",
    bw = 0.01,
    color = "gray",
    position = position_dodge(width = 0.5)
  ) +
  geom_path(
    data = summary,
    aes(group = alpha,
        x = peak_coeff,
        y = mean),
    colour = "black",
    position = position_dodge(width = 0.5),
    linewidth = 0.5,
    lineend = "round"
  ) +
  geom_point(
    data = summary,
    aes(
      group = alpha,
      fill = alpha,
      x = peak_coeff,
      y = mean
    ),
    size = 2,
    shape = 21,
    color = "black",
    position = position_dodge(width = 0.5)
  ) +
  geom_hline(data = hline_violin,
             aes(
               yintercept = target,
               linetype = "target value",
               colour = alpha
             ),
  ) +
  scale_linetype_manual("", values = c("target value" = "dashed",
                                       "average trend" = "solid")) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  labs(x = "Peak Coefficient", y = "Metric Value") +
  theme_publication()

p_violin <- p_violin +
  ggh4x::facetted_pos_scales(
    y = list(
      outcome_name == "bias" ~ scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)),
      outcome_name == "coverage" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "sensitivity" ~ scale_y_continuous(limits = c(0, 1)),
      outcome_name == "specificity" ~ scale_y_continuous(limits = c(0, 1))
    )
  )

ggsave(
  plot = p_violin,
  here("analysis/simulation/plots", "violin.png"),
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)




# Violin (bias & coverage) + ROC --------------------------------------------
summary_long2 <- summary_long %>%
  filter(outcome_name %in% c("bias", "coverage") &
           !(outcome_name == "bias" & alpha != 0.05)) %>%
  mutate(my_colour = case_when(
    outcome_name == "bias" ~ "#404040",
    TRUE ~ color_pal("#04179b", "#00ACE6", sort(alpha_breaks))[match(alpha, sort(alpha_breaks))]
  ))


summary2 <- summary %>%
  filter(outcome_name %in% c("bias", "coverage") &
           !(outcome_name == "bias" & alpha != 0.05)) %>%
  mutate(my_colour = case_when(
    outcome_name == "bias" ~ "#404040",
    TRUE ~ color_pal("#04179b", "#00ACE6", sort(alpha_breaks))[match(alpha, sort(alpha_breaks))]
  ))

hline_violin2 <-
  hline_violin %>% filter(outcome_name %in% c("bias", "coverage"))


p_violin2 <- ggplot(data = summary_long2,
                    aes(
                      x = peak_coeff,
                      y = outcome_value,
                      fill = my_colour,
                      group = interaction(alpha, peak_coeff)
                    )) +
  facet_wrap( ~ outcome_name, scales = "free") +
  geom_violin(
    trim = TRUE,
    scale = "count",
    bw = 0.01,
    linewidth = 0,
    position = position_dodge(width = 0.5)
  ) +
  geom_path(
    data = summary2,
    aes(group = alpha, x = peak_coeff, y = mean),
    position = position_dodge(width = 0.5),
    linewidth = 0.5,
    lineend = "round",
    show.legend = FALSE
  ) +
  geom_point(
    data = summary2,
    aes(
      x = peak_coeff,
      y = mean,
      fill = my_colour,
      color = ifelse(outcome_name == "bias", "white", "black"),
      group = interaction(alpha, peak_coeff)
    ),
    size = 2,
    shape = 21,
    position = position_dodge(width = 0.5),
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  ggnewscale::new_scale_colour() +
  ggnewscale::new_scale_fill() +
  geom_hline(
    data = hline_violin2 %>% filter(outcome_name == "bias"),
    aes(yintercept = target),
    colour = "black",
    show.legend = FALSE,
    linetype = "dashed",
    key_glyph = draw_key_rect
  ) +
  geom_hline(
    data = hline_violin2 %>% filter(outcome_name != "bias"),
    aes(yintercept = target, colour = alpha),
    show.legend = TRUE,
    linetype = "dashed",
    key_glyph = draw_key_rect
  ) +
  scale_color_manual(name = "alpha",
                     values = color_pal("#04179b", "#00ACE6", sort(alpha_breaks))) +
  ggh4x::facetted_pos_scales(y = list(
    outcome_name == "coverage" ~ scale_y_continuous(
      limits = c(0.4, 1),
      breaks = seq(0, 1, 0.1),
      expand = c(0, 0)
    )
  )) +
  labs(x = "Peak Coefficient", y = "Metric Value") +
  theme_publication() +
  guides(
    colour = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      label.theme = element_text(color = "white", margin = margin(t = -17.5)),
      keywidth = 2,
      nrow = 1,
      byrow = TRUE
    )
  )


p_roc <- summary_df %>%
  mutate(FPR = 1 - specificity,
         TPR = sensitivity) %>%
  select(alpha, peak_coeff, FPR, TPR) %>%
  group_by(alpha, peak_coeff) %>%
  summarise(across(c(FPR, TPR), function(x)
    mean(x, na.rm = TRUE)),
    header = "Receiver-Operating Characteristic") %>%
  ggplot(aes(x = FPR,
             y = TPR)) +
  facet_wrap( ~ header) +
  geom_path(
    aes(group = alpha, color = alpha),
    linejoin = "round",
    linewidth = 0.7,
    key_glyph = draw_key_rect
  ) +
  geom_point(
    aes(fill = peak_coeff),
    size = 3,
    shape = 21,
    color = "black",
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
  ) +
  coord_cartesian() +
  scale_fill_manual(
    name = "peak coefficient",
    values = color_pal(
      "#3F00FF",
      "#ff007f",
      peak_coeff_breaks,
      focus_value = 1,
      focus_color = "orange"
    )
  ) +
  scale_colour_manual(values = color_pal("#04179b", "#00ACE6", alpha_breaks),
                      guide = "none") +
  # scale_shape_manual(name = "alpha",
  #                    values = c(15, 18, 16, 17)) +
  labs(x = "False Positive Rate (1- Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_publication() +
  guides(
    fill =  guide_legend(
      keywidth = 2,
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      label.theme = element_text(color = "white", margin = margin(t = -17.5)),
      nrow = 1,
      byrow = TRUE
    )
  )

p_violin_roc <-
  patchwork::wrap_plots(p_violin2, p_roc, ncol = 1, guides = "collect") &
  theme(legend.position = "bottom",
        legend.spacing.x = unit(100, "points")) &
  patchwork::plot_annotation(tag_levels = 'A')


ggsave(
  here("analysis/simulation/plots", "violin_roc.png"),
  plot = p_violin_roc,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)



# Analysis ----------------------------------------------------------------

#################################
# Coverage Error
#################################

p_coverage_error <- summary %>%
  filter(outcome_name == "coverage") %>%
  ungroup() %>%
  select(outcome_name, alpha, peak_coeff, mean) %>%
  mutate(target = 1 - as.numeric(levels(alpha))[alpha],
         error = target - mean) %>%
  ggplot(aes(x = peak_coeff, y = error)) +
  geom_line(aes(group = alpha, color = alpha),
            linewidth = 0.55,
            key_glyph = draw_key_rect) +
  geom_point(
    aes(fill = alpha),
    key_glyph = draw_key_rect,
    size = 3,
    shape = 21
  ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  scale_colour_manual(values = color_pal("#04179b", "#00ACE6", alpha_breaks)) +
  scale_fill_manual(values = color_pal("#04179b", "#00ACE6", alpha_breaks)) +
  scale_y_continuous(
    breaks = seq(-0.25, 0.25, 0.05),
    limits = c(-0.25, 0.25),
    expand = c(0, 0)
  ) +
  theme_publication() +
  labs(x = "Peak Coefficient", y = "Error") +
  guides(
    colour =  guide_legend(
      keywidth = 2,
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      label.theme = element_text(color = "white", margin = margin(t = -17.5)),
      nrow = 1,
      byrow = TRUE
    )
  )

ggsave(
  here("analysis/simulation/plots", "coverage_error.png"),
  plot = p_coverage_error,
  width = 16,
  height = 8,
  units = "in",
  dpi = 300
)



#################################
# ROC AUC & F1 Score
#################################
# roc_df <- summary_df %>%
#   mutate(FPR = 1 - specificity,
#          TPR = sensitivity) %>%
#   select(alpha, peak_coeff, FPR, TPR) %>%
#   group_by(alpha, peak_coeff) %>%
#   summarise(across(c(FPR, TPR), function(x)
#     mean(x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   mutate(peak_coeff = as.numeric(levels(peak_coeff))[peak_coeff]) %>%
#   filter(peak_coeff > 0.5)
#
#
#
# compute_auc <- function(fpr, tpr) {
#   # Sort the points by ascending FPR
#   order_idx <- order(fpr)
#   fpr_sorted <- fpr[order_idx]
#   tpr_sorted <- tpr[order_idx]
#
#   # Compute the AUC using the trapezoidal rule
#   auc_value <-
#     sum(diff(fpr_sorted) * (tpr_sorted[-1] + tpr_sorted[-length(tpr_sorted)]) / 2)
#
#   return(auc_value)
# }
#
# # Compute the AUC for each alpha
# summary_df
# pROC::roc()
#
# roc_df %>%
#   group_by(alpha) %>%
#   summarise(
#     proc = list(pROC::roc(TPR, FPR)),
#     manual_AUC = compute_auc(FPR, TPR),
#     pacck_auc = 1 - pracma::trapz(FPR, TPR),
#     x = sum(TPR * FPR) / 2
#   )
#
#
# # Compute the F1 score for each alpha
# #precision == ppv
# #recall == sensitivity
# #F1 = 2 * (precision * recall / (precision + recall))
# #==> F1 = 2 * (ppv * sensitivity / (ppv + sensitivity))
# summary_df %>%
#   mutate() %>%
#   select(alpha, peak_coeff, ppv, sensitivity) %>%
#   group_by(alpha, peak_coeff) %>%
#   summarise(across(c(ppv, sensitivity), function(x)
#     mean(x, na.rm = TRUE))) %>%
#   ungroup() %>%
#   mutate(peak_coeff = as.numeric(levels(peak_coeff))[peak_coeff]) %>%
#   filter(peak_coeff > 1.1) %>%
#   mutate(F1 = 2 * (ppv * sensitivity / (ppv + sensitivity))) %>%
#   group_by(alpha) %>%
#   summarise(mean_F1 = mean(F1, na.rm = TRUE))
#


# clean env but keep base_objects too
rm(list = setdiff(ls(), ls_snapshot))
gc()
