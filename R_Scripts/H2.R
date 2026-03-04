# H2.R

source("R_Scripts/1_data_import.R")

library(dplyr)
library(ggplot2)

  ces <- ces %>%
    mutate(
      q10_1 = na_if(as.numeric(kiss_module_Q10_1), 6),
      q10_2 = na_if(as.numeric(kiss_module_Q10_2), 6),
      q10_3 = na_if(as.numeric(kiss_module_Q10_3), 6),
      
      q10_composite = rowMeans(
        across(c(q10_1, q10_2, q10_3)),
        na.rm = TRUE
      ),
      
      # If respondent misses all three items its converted to NA
      q10_composite = ifelse(is.nan(q10_composite), NA, q10_composite)
    )

#Correlations
cor_truth <- cor(ces$q10_composite, ces$truth_numeric, use = "pairwise.complete.obs")
cor_ordinary <- cor(ces$q10_composite, ces$ordinary_numeric, use = "pairwise.complete.obs")

cor_truth
cor_ordinary

#Scatterplots
# Composite vs truth_numeric
ggplot(ces, aes(x = q10_composite, y = truth_numeric)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = paste0("H2: Q10 Composite vs Truth (numeric) | r = ", round(cor_truth, 3)),
    x = "Q10 Composite (mean of Q10_1–Q10_3)",
    y = "Truth (truth_numeric)"
  )

# Composite vs ordinary_numeric
ggplot(ces, aes(x = q10_composite, y = ordinary_numeric)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
  labs(
    title = paste0("H2: Q10 Composite vs Ordinary (numeric) | r = ", round(cor_ordinary, 3)),
    x = "Q10 Composite (mean of Q10_1–Q10_3)",
    y = "Ordinary (ordinary_numeric)"
  )



#Political Efficacy Correlations
ces <- ces %>%
  mutate(
    political_efficacy = as.numeric(political_efficacy),
    truth_numeric = as.numeric(truth_numeric),
    ordinary_numeric = as.numeric(ordinary_numeric)
  )

#Correlations
cor_eff_truth <- cor.test(
  ces$political_efficacy,
  ces$truth_numeric,
  use = "pairwise.complete.obs"
)

cor_eff_ordinary <- cor.test(
  ces$political_efficacy,
  ces$ordinary_numeric,
  use = "pairwise.complete.obs"
)

cor_eff_truth$estimate
cor_eff_ordinary$estimate
cor_eff_ordinary$p.value

#Scatterplot 1

library(ggplot2)
library(cowplot)

# ── Replace these with your existing correlation objects ───────────────────────
# e.g., r_var2 <- 0.42; p_var2 <- 0.003
#       r_var3 <- 0.18; p_var3 <- 0.09
r_var2 <- cor_eff_truth$estimate
p_var2 <- cor_eff_ordinary$p.value
r_var3 <- cor_eff_ordinary$estimate
p_var3 <- cor_eff_ordinary$p.value


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
      x = 0.5, y = 6,
      #hjust = 2, vjust = 1.1,
      size = 3.6, label.size = 0,
      label = paste0("r = ", sprintf("%.2f", r), ", ", fmt_p(p))
    )
}

# ── Build the two panels ───────────────────────────────────────────────────────
p1 <- scatter_with_annot(
  data = ces, xvar = political_efficacy, yvar = truth_numeric,
  title = str_wrap("Political Efficacy v. Politicians should just tell it like it is...",20),
  r = r_var2, p = p_var2,
  line_col = "firebrick"
)+labs(x="Political Efficacy", y="Truth")+geom_jitter(size=0.5)
p1
p2 <- scatter_with_annot(
  data = ces, xvar = political_efficacy, yvar = ordinary_numeric,
  title = str_wrap("Political efficacy vs voters are more likely to vote for politicians who are ordinary",30),
  r = r_var3, p = p_var3,
  line_col = "royalblue"
)+labs(x="Political Efficacy", y="Ordinary")+geom_jitter(size=0.5)
# ── Arrange side-by-side with panel tags ───────────────────────────────────────
side_by_side <- plot_grid(p1, p2, nrow = 1)

# Print to viewer
side_by_side

# Optional: save
ggsave(here("Plots/H2.png"), side_by_side, width = 10, height = 4.5, dpi = 300)
