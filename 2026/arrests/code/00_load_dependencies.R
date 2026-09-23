## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor")

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#councildown.check <- "councildown" %in% installed.packages()[,"Package"]
councilverse.check <- "councilverse" %in% installed.packages()[,"Package"]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)
#if(councildown.check == FALSE) remotes::install_github("newyorkcitycouncil/councildown")
if(councilverse.check == FALSE) remotes::install_github("newyorkcitycouncil/councilverse")
  
# packages are loaded
lapply(c(list.of.packages,"councilverse"), require, character.only = TRUE)

# remove created variables for packages
rm(list.of.packages,new.packages,councilverse.check)

## Functions -----------------------------------------------

diag_plots <- function(model){
  df_diag <- data.frame(
    fitted    = fitted(model),
    resid     = residuals(model)
  )

  # hist of residuals 
  resid_hist <- df_diag |> 
    ggplot(aes(
      x = resid
    )) + 
    geom_histogram() + 
    labs(
      title = 'Distribution of Residuals',
      x = 'Residuals',
      y = 'Frequency'
    ) + 
    theme_nycc() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 9.5),
      axis.title.y = element_text(size = 9.5)
    )
  
  resid_fitted <- df_diag |> 
    ggplot(aes(
      x = fitted,
      y = resid
    )) + 
    geom_point(alpha = 0.3) + 
    geom_hline(
      yintercept = 0, color = 'red', linetype = 'dashed'
    ) + 
    geom_smooth(
      method = 'loess', se = FALSE, color = 'darkblue'
    ) + 
    labs(
      title = 'Residuals v. Fitted',
      x = 'Fitted Values',
      y = 'Residuals'
    ) + 
    theme_nycc() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 9.5),
      axis.title.y = element_text(size = 9.5)
    )
  
  # QQ Plot 

  qq_plot <- df_diag |> 
    ggplot(aes(
      sample = resid
    )) + 
    stat_qq() + 
    stat_qq_line(color = 'red') + 
    labs(
      title = 'QQ Plot',
      x = 'Theoretical Quantiles',
      y = 'Sample Quantiles'
    ) + 
    theme_nycc() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 9.5),
      axis.title.y = element_text(size = 9.5)
    )
  
  # Scale location plot
  scale_location <- df_diag |> 
    ggplot(aes(
      x = fitted, y = sqrt(abs(resid))
    )) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(method = 'loess', se = FALSE, color = 'darkblue') + 
    labs(
      title = 'Scale-location Plot',
      x = 'Fitted Values',
      y = expression(sqrt("|Residuals|"))
    ) + 
    theme_nycc() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 9.5),
      axis.title.y = element_text(size = 9.5)
    )

  # return(list(resid_fitted, qq_plot, scale_location))
  (resid_hist | resid_fitted) / (qq_plot | scale_location)
}
