#H10_H11.R

source("R_Scripts/1_data_import.R")

library(cowplot)
#Variables are numeric

ces <- ces %>%
  mutate(
    party_average = as.numeric(party_average),
    leader_average = as.numeric(leader_average),
    truth_numeric = as.numeric(truth_numeric),
    ordinary_numeric = as.numeric(ordinary_numeric)
  )

#H10
# Party dislike (party_average) -> support for lispop_2 (truth_numeric)

cor_party_truth <- cor.test(
  ces$party_average,
  ces$truth_numeric,
  use = "pairwise.complete.obs"
)

cor_party_truth

# Scatterplot
# ggplot(ces, aes(x = party_average, y = truth_numeric)) +
#   geom_point(na.rm = TRUE) +
#   geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
#   labs(
#     title = paste0("H10: Party Average vs Truth | r = ", round(cor_party_truth, 3)),
#     x = "Party Thermometer Average",
#     y = "Truth (lispop_2 numeric)"
#   )

#H10 2
# Leader dislike (leader_average) -> support for lispop_2 (truth_numeric)

cor_leader_truth <- cor.test(
  ces$leader_average,
  ces$truth_numeric,
  use = "pairwise.complete.obs"
)
r_var2 <- cor_leader_truth$estimate
p_var2 <- cor_leader_truth$p.value
r_var3 <- cor_party_truth$estimate
p_var3 <- cor_party_truth$p.value


# Optional helper to format p-values nicely
fmt_p <- function(p) {
  if (is.na(p)) return("p = NA")
  if (p < .001) "p < .001" else paste0("p = ", sprintf("%.3f", p))
}

# A small helper that builds a scatter with a linear fit and an annotation
scatter_with_annot <- function(data, xvar, yvar, title, r, p,
                               point_col = "grey25", line_col = "steelblue") {
  ggplot(data, aes(x = {{ xvar }}, y = {{ yvar }})) +
    geom_point(color = point_col, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = line_col, linewidth = 1) +
    labs(title = title, x = rlang::as_name(rlang::ensym(xvar)), y = rlang::as_name(rlang::ensym(yvar))) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold")) +
    # Annotation in the top-left corner
    annotate(
      "label",
      x = 25, y = 6,
      #hjust = 25, vjust = 1.1,
      size = 3.6, label.size = 0,
      label = paste0("r = ", sprintf("%.2f", r), ", ", fmt_p(p))
    )
}

# ── Build the two panels ───────────────────────────────────────────────────────
p1 <- scatter_with_annot(
  data = ces, xvar = leader_average, yvar = truth_numeric,
  title = str_wrap("Feelings Toward Leaders v. Politicians should just tell it like it is...",20),
  r = r_var2, p = p_var2,
  line_col = "firebrick"
)+labs(x="Feelings", y="Truth")+geom_jitter(size=0.5)
p1
p2 <- scatter_with_annot(
  data = ces, xvar = party_average, yvar = truth_numeric,
  title = str_wrap("Feelings Toward Parties vs Politicians should just tell it like it is...",20),
  r = r_var3, p = p_var3,
  line_col = "royalblue"
)+labs(x="Feelings", y="Ordinary")+geom_jitter(size=0.5)
# ── Arrange side-by-side with panel tags ───────────────────────────────────────
side_by_side <- plot_grid(p1, p2, nrow = 1)

# Print to viewer
side_by_side

# Optional: save
ggsave(here("Plots/H10.png"), side_by_side, width = 10, height = 4.5, dpi = 300)

#### H11
# Leader and party feelings relationship with lispop_3

cor_party_ordinary <- cor.test(
  ces$party_average,
  ces$ordinary_numeric,
  use = "pairwise.complete.obs"
)

cor_leader_ordinary <- cor.test(
  ces$leader_average,
  ces$ordinary_numeric,
  use = "pairwise.complete.obs"
)
r_var2a <- cor_leader_ordinary$estimate
p_var2a <- cor_leader_ordinary$p.value
r_var3a <- cor_party_ordinary$estimate
p_var3a <- cor_party_ordinary$p.value

# ── Build the two panels ───────────────────────────────────────────────────────
p1a <- scatter_with_annot(
  data = ces, xvar = leader_average, yvar = ordinary_numeric,
  title = str_wrap("Feelings Toward Leaders v. voters are more likely to vote for politicians who are ordinary",20),
  r = r_var2a, p = p_var2a,
  line_col = "firebrick"
)+labs(x="Feelings", y="Ordinary")+geom_jitter(size=0.5)
p1a
p2a <- scatter_with_annot(
  data = ces, xvar = party_average, yvar = ordinary_numeric,
  title = str_wrap("Feelings Toward Parties vs voters are more likely to vote for politicians who are ordinary",30),
  r = r_var3, p = p_var3,
  line_col = "royalblue"
)+labs(x="Feelings", y="Ordinary")+geom_jitter(size=0.5)
p2a
# ── Arrange side-by-side with panel tags ───────────────────────────────────────
side_by_side_ordinary <- plot_grid(p1a, p2a, nrow = 1)

# Print to viewer
side_by_side_ordinary

# Optional: save

ggsave(here("Plots/H10_2.png"), side_by_side_ordinary, width = 10, height = 4.5, dpi = 300)
