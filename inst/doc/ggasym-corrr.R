## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.align = "center"
)

## ----load_libs, warning=FALSE, message=FALSE----------------------------------
library(ggasym)
library(corrr)
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
head(mtcars)

## -----------------------------------------------------------------------------
# Pearson correlation
pear <- corrr::correlate(mtcars, method = "pearson", quiet = TRUE) %>%
  corrr::shave(upper = TRUE) %>%
  corrr::stretch() %>%
  stats::na.omit() %>%
  dplyr::rename(pearson_r = "r")
# Spearman correlation
spear <- corrr::correlate(mtcars, method = "spearman", quiet = TRUE) %>%
  corrr::shave(upper = TRUE) %>%
  corrr::stretch() %>%
  stats::na.omit() %>%
  dplyr::rename(spearman_r = "r")

## -----------------------------------------------------------------------------
df <- dplyr::full_join(pear, spear, by = c("x", "y")) %>%
  ggasym::asymmetrise(x, y)

## -----------------------------------------------------------------------------
ggplot(df) +
  geom_asymmat(aes(
    x = x, y = y,
    fill_tl = pearson_r, fill_br = spearman_r
  )) +
  scale_fill_tl_distiller(type = "div", palette = "RdYlBu") +
  scale_fill_br_distiller(type = "div", palette = "RdYlBu")

## ----plotting-----------------------------------------------------------------
ggplot(df) +
  geom_asymmat(aes(
    x = x, y = y,
    fill_tl = pearson_r, fill_br = spearman_r
  )) +
  scale_fill_tl_distiller(
    type = "div", palette = "RdYlBu",
    na.value = "grey90",
    guide = guide_colourbar(
      direction = "horizontal",
      order = 1,
      title.position = "top"
    )
  ) +
  scale_fill_br_distiller(
    type = "div", palette = "RdYlBu",
    na.value = "grey90",
    guide = guide_colourbar(
      direction = "horizontal",
      order = 2,
      title.position = "top"
    )
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "grey25"),
    panel.grid = element_blank()
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_equal() +
  labs(
    x = "spearman correlations", y = "pearson correlations",
    title = "Comparing Pearson and Spearman correlations of 'mtcars'",
    subtitle = "An example of using 'corrr' and 'ggasym' together",
    fill_tl = "Pearson r", fill_br = "Spearman r"
  )

