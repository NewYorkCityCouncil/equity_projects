## Steps ---------------------------------------------------------

# 1. Load packages.
# 2. Set shared R options.
# 3. Define helpers used by the ECE scripts and one-pager.

## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor", "ranger", "data.table", "ggplot2", "leaflet", 
                      "readxl", "tidycensus", "matrixStats", "scales", "htmlwidgets", 
                      "httr", "broom", "httr2", "leaflet", "sf", "nngeo", "stringdist", 
                      "scales", "tidycensus", "car", "pscl", "MASS", "corrplot", "reshape2", 
                      "ggrepel", "leaflet", "mgcv", "stringr", "htmlwidgets", "DHARMa", "knitr", 
                      "kableExtra", "stringi"

)



# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#councildown.check <- "councildown" %in% installed.packages()[,"Package"]
councilverse.check <- "councilverse" %in% installed.packages()[,"Package"]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)
#if(councildown.check == FALSE) remotes::install_github("newyorkcitycouncil/councildown")
if(councilverse.check == FALSE) remotes::install_github("newyorkcitycouncil/councilverse")
  
# packages are loaded
invisible(
  lapply(c(list.of.packages,"councilverse"), require, character.only = TRUE)
)

# remove created variables for packages
rm(list.of.packages,new.packages,councilverse.check)

setDTthreads(1L)
options(tigris_use_cache = TRUE)

## Functions -----------------------------------------------
na_fun <- function(x) {sum(is.na(x))}



# Fits the same simple bivariate model for each PUMA predictor. This is an
nycc_cols <- pal_nycc("single")
main_col <- nycc_cols[1]
accent_col <- pal_nycc("double")[2]

plot_puma_scatter <- function(
    data,
    x,
    y,
    title,
    subtitle = NULL,
    x_lab = NULL,
    y_lab = NULL,
    x_labels = waiver(),
    y_labels = waiver(),
    point_size = 8,
    width = 600,
    height = 400
) {
  
  plot_dt <- copy(
    data[!is.na(get(x)) & !is.na(get(y))]
  )
  
  plot_dt[, hover_text := paste0(
    "<b>", neighb, "</b>",
    "<br>PUMA: ", puma,
    "<br>", x_lab, ": ", round(get(x), 3),
    "<br>", y_lab, ": ", round(get(y), 3)
  )]
  
  m <- lm(
    as.formula(
      paste(y, "~", x)
    ),
    data = plot_dt
  )
  
  line_dt <- data.table(
    x_value = seq(
      min(plot_dt[[x]], na.rm = TRUE),
      max(plot_dt[[x]], na.rm = TRUE),
      length.out = 100
    )
  )
  
  setnames(line_dt, "x_value", x)
  
  line_dt[, y_hat := predict(
    m,
    newdata = line_dt
  )]
  
  plotly::plot_ly() %>% 
    plotly::add_markers(
      data = plot_dt,
      x = as.formula(paste0("~", x)),
      y = as.formula(paste0("~", y)),
      text = ~hover_text,
      hoverinfo = "text",
      marker = list(
        color = main_col,
        size = point_size,
        opacity = 0.75
      ),
      showlegend = FALSE
    ) %>% 
    plotly::add_lines(
      data = line_dt,
      x = as.formula(paste0("~", x)),
      y = ~y_hat,
      line = list(
        color = accent_col,
        width = 3
      ),
      hoverinfo = "skip",
      showlegend = FALSE
    ) %>% 
    plotly::layout(
      title = list(
        text = paste0(
          title,
          if (!is.null(subtitle)) {
            paste0("<br><sup>", subtitle, "</sup>")
          } else {
            ""
          }
        )
      ),
      xaxis = list(
        title = x_lab
      ),
      yaxis = list(
        title = y_lab
      ),
      width = width,
      height = height,
      hovermode = "closest",
      margin = list(
        l = 70,
        r = 30,
        b = 70,
        t = 70
      ),
      annotations = list(
        list(
          text = "Source: 2023 ACS PUMS via IPUMS; ECE site seats and vacancies aggregated to PUMA.",
          xref = "paper",
          yref = "paper",
          x = 0,
          y = -0.18,
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top",
          font = list(size = 11)
        )
      )
    )
}
relationship_summary <- function(data, x, y) {
  d <- data[!is.na(get(x)) & !is.na(get(y))]
  
  data.table(
    x = x,
    y = y,
    n_pumas = nrow(d),
    correlation = cor(d[[x]], d[[y]], use = "complete.obs"),
    lm_slope = coef(lm(d[[y]] ~ d[[x]]))[2],
    lm_r_squared = summary(lm(d[[y]] ~ d[[x]]))$r.squared
  )
}
run_dharma_checks <- function(model, model_name, data = NULL, lon = NULL, lat = NULL) {
  
  message("Running DHARMa checks for: ", model_name)
  
  sim <- simulateResiduals(
    fittedModel = model,
    n = 1000
  )
  
  
  # Core tests
  out <- data.table(
    model = model_name,
    test = c(
      "uniformity",
      "dispersion",
      "zero_inflation",
      "outliers"
    ),
    p_value = c(
      testUniformity(sim)$p.value,
      testDispersion(sim)$p.value,
      testZeroInflation(sim)$p.value,
      testOutliers(sim)$p.value
    )
  )
  
  
  spatial_test <- testSpatialAutocorrelation(
    sim,
    x = lon,
    y = lat
  )
  
  out <- rbind(
    out,
    data.table(
      model = model_name,
      test = "spatial_autocorrelation",
      p_value = spatial_test$p.value
    ),
    fill = TRUE
  )
  
  out[]
}

# ------------------------------------------------------------
# Weighted median helper
# ------------------------------------------------------------

weighted_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  
  if (length(x) == 0) return(NA_real_)
  
  o <- order(x)
  x <- x[o]
  w <- w[o]
  
  x[which(cumsum(w) / sum(w) >= 0.5)[1]]
}
# ------------------------------------------------------------
# Helper: format p-values
# ------------------------------------------------------------

fmt_p <- function(p) {
  fcase(
    is.na(p), NA_character_,
    p < 0.001, "<0.001",
    default = sprintf("%.3f", p)
  )
}

# ------------------------------------------------------------
# Helper: format percent change with CI
# ------------------------------------------------------------

fmt_est <- function(est, low, high) {
  sprintf(
    "%.1f%% (%.1f, %.1f)",
    est,
    low,
    high
  )
}
