
library(stringr)

# Your title function
title_fun <- function(ID) {
  sprintf("CH₄ Flux — ID %s", paste(unique(ID), collapse = ", "))
}

# Output subfolder (relative to working dir)
out_dir <- file.path(getwd(), "plots", "events_flux_by_id")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Loop over all IDs
ids <- sort(unique(tree_dat$ID))

for (id in ids) {
  # Filter per ID
  df <- tree_dat %>%
    filter(CH4_rob_flux.estimate > 0,
           Timepoint != "(none)",
           ID == id)
  
  if (nrow(df) == 0) {
    message("Skipping ID ", id, " — no rows after filter.")
    next
  }
  
  # Use only event vlines for years present in df
  ev <- events %>%
    semi_join(df %>% distinct(Year), by = "Year")
  
  # Build plot
  p <- ggplot(df, aes(x = yday(Date),
                      y = CH4_rob_flux.estimate,
                      color = as.factor(Year))) +
    geom_point(size = 2) +
    geom_line(alpha = 0.7) +
    geom_vline(data = ev,
               aes(xintercept = JD, color = as.factor(Year)),
               linetype = "dashed", alpha = 0.6) +
    labs(title = title_fun(df$ID), color = "Year") +
    theme_minimal(base_size = 12)
  
  # Descriptive filename: sanitize ID + include years
  id_clean   <- str_replace_all(id, "[^A-Za-z0-9_-]", "_")
  years_str  <- paste(sort(unique(df$Year)), collapse = "-")
  file_png   <- file.path(out_dir, sprintf("CH4_flux_ID_%s_years_%s.png", id_clean, years_str))
  
  # Save
  ggsave(filename = file_png, plot = p, width = 8, height = 5, dpi = 300)
}
