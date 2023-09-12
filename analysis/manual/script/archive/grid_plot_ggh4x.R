library(ggh4x)
wrap_label <- function(x) {
  gsub("_", " ", x)
}
m1 %>%
  # filter(param != "n_groups") %>%
  # mutate(param = wrap_label(param),
  #        scenario = wrap_label(scenario)) %>%
  ggplot(aes(x = group, y = mean_est, col = peak_coeff)) +
  ggh4x::facet_nested_wrap(
    vars(param, scenario),
    scales = "free_x",
    strip = strip_nested(bleed = TRUE,
                         text_x = elem_list_text(colour = c("white", "black"),
                                                 face = c("bold","italic")),
                         background_x = elem_list_rect(fill = c("#4C4E52", "white")),
                         by_layer_x = TRUE),
    labeller = as_labeller(wrap_label),
    drop = TRUE
  ) +

  # estimate
  geom_point(position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = mean_lower_ci, ymax = mean_upper_ci),
    width = 0.2,
    position = position_dodge(0.9)
  ) +

  # true delta
  geom_segment(
    aes(
      y = mean_delta,
      xend = as.integer(as.factor(group)) + 0.5,
      yend = mean_delta
    ),
    color = "black",
    position = position_dodge(0.9),
    alpha = 0.7,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(
      y = mean_delta,
      xend = as.integer(as.factor(group)) - 0.5,
      yend = mean_delta
    ),
    color = "black",
    position = position_dodge(0.9),
    alpha = 0.7,
    show.legend = FALSE
  ) +

  # hline at 0
  geom_hline(aes(yintercept = 0), lty = "dotted") +
  labs(
    x = "Scenario",
    y = "Estimate",
    title = "Mean estimate, mean 95% lower and mean 95% upper CI",
    subtitle = "black lines refer to the true delta estimate"
  ) +
  theme_bw() +
  theme(
    # legend style
    legend.position = "bottom",
    legend.background = element_rect(
      fill = "#c0c0c0",
      linewidth = 0.5,
      linetype = "solid",
      colour = "black"
    ),

    #x ticks/labels
    axis.text.x =  element_blank(),
    axis.ticks.x = element_blank(),

    #facet line
    ggh4x.facet.nestline = element_line(colour = "black")
  )



theme(
  # legend style
  legend.position = "bottom",
  legend.background = element_rect(
    fill = "lightblue",
    linewidth = 0.5,
    linetype = "solid",
    colour = "darkblue"
  ),
  #removing space between panels
  axis.text.x =  element_blank(),
  axis.ticks.x = element_blank(),
  plot.margin = rep(unit(0, "null"), 4),
  panel.spacing = unit(0.5, "lines"),
  axis.ticks.length = unit(0, "null"),
  axis.ticks.margin = unit(0, "null"),

  #editing strips
  strip.background.y =  element_rect(fill = "#4C4E52", color = "#4C4E52"),
  #element_blank(),
  strip.background.x = element_rect(fill = NA, color = "#4C4E52"),
  strip.text.y = element_text(
    angle = 0,
    color = "white",
    face = "bold",
    size = 10
  ),
  strip.text.x = element_text(
    angle = 0,
    color = "black",
    face = "italic",
    size = 10
  ),
  strip.placement = 'outside',
  strip.clip = "off"
) +
  labs(x = "", y = "")
