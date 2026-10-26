##########################################
### Project: SCRI design for           ###
### vaccine effectiveness              ###
##########################################

###################################
### Script    Functions for     ###
### making plots                ###
###################################

source(here("Codes", "nestedloop.R"))

# Function to extract data for exploratory analysis --------------
data_exploratory <- function(method_scen_ = method_scen(scenario_table = scenarios),
                             results_dir = here("Results")){
  scenarios <- unique(method_scen_$scen_name)
  methods <- unique(method_scen_$methods)
  
  all_data <- list()
  
  for (i in seq_len(nrow(method_scen_))) {
    method_i <- method_scen_$methods[i]
    scen_file <- paste0(method_scen_$scen_name[i], ".csv")
    
    file_path <- file.path(results_dir, method_i, scen_file)
    
    if (!file.exists(file_path)) {
      warning(paste("File not found:", file_path))
      next
    }
    
    df <- tryCatch(read_csv(file_path, show_col_types = FALSE),
                   error = function(e) NULL)
    
    if (is.null(df) || !"est_E" %in% names(df)) {
      warning("gamma_1_hat missing in: ", file_path)
      next
    }
    
    all_data[[length(all_data) + 1]] <- data.frame(
      scen_name = method_scen_$scen_name[i],
      method = method_i,
      gamma_1_hat = df$est_E,
      se_gamma_1 = df$se_E,
      n_event = df$n_event
    )
  }
  
  data_all <- bind_rows(all_data)
  return(data_all)
}

# [Exploratory] Histogram of gamma_1, n_event and se_gamma_1 ---------------
plot_exploratory <- function(
    data,
    output_dir = file.path(here(),"Plots"),
    width = 7,
    height = 6,
    dpi = 300
){
  scenarios <- unique(data$scen_name)
  
  output_dir1 = file.path(output_dir,"Hist_gamma_1_hat")
  output_dir2 = file.path(output_dir,"Hist_se_gamma_1")
  output_dir3 = file.path(output_dir,"Hist_n_event")
  create_directory(output_dir1)
  create_directory(output_dir2)
  create_directory(output_dir3)
  
  
  for (sc in scenarios) {
    df_sc <- data %>% filter(scen_name == sc)
    
    if (nrow(df_sc) == 0) {
      warning("No data for scenario: ", sc)
      next
    }
    
    p <- ggplot(df_sc, aes(x = method, y = gamma_1_hat, fill = method)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.9) +
      labs(
        title = paste("Scenario:", sc),
        x = "Method",
        y = "gamma_1_hat"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none"
      )
    
    ggsave(
      filename = file.path(output_dir1, paste0("plot_", sc, ".png")),
      plot = p,
      width = width,
      height = height,
      dpi = dpi
    )
    
    p2 <- ggplot(df_sc, aes(x = method, y = se_gamma_1, fill = method)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.9) +
      labs(
        title = paste("Scenario:", sc),
        x = "Method",
        y = "se_gamma_1"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none"
      )
    
    ggsave(
      filename = file.path(output_dir2, paste0("plot_", sc, ".png")),
      plot = p2,
      width = width,
      height = height,
      dpi = dpi
    )
    
    p3 <- ggplot(df_sc, aes(x = method, y = n_event, fill = method)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      geom_boxplot(width = 0.15, outlier.size = 0.5, alpha = 0.9) +
      labs(
        title = paste("Scenario:", sc),
        x = "Method",
        y = "n_event"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none"
      )
    
    ggsave(
      filename = file.path(output_dir3, paste0("plot_", sc, ".png")),
      plot = p3,
      width = width,
      height = height,
      dpi = dpi
    )
  }
  
  message("All plots saved to: ", output_dir)
}

# Lollipop plot for bias/ estimated IRR -----------------------------

lollipop_plot <- function(data,
                          aes_x, aes_x_low_ci, aes_x_up_ci,
                          mode = c("gamma", "irr"),   # "gamma" = linear scale, "irr" = log scale
                          refline = 0,               # 0 for bias, 1 or 2 for IRR
                          irr_breaks = NULL,         # vector of IRR ticks, e.g. c(0.5,1,2,4)
                          irr_limits = NULL,         # two-element vector, IRR scale
                          x_break = NULL,            # for gamma mode
                          x_limits = NULL,           # for gamma mode
                          xlabel = "",
                          plot_name) {
  
  mode <- match.arg(mode)
  
  png(here("Plots", paste0(plot_name, ".png")), 
      width = 18, height = 10, units = "cm", res = 300)
  
  p <- ggplot(
    data,
    aes(x = .data[[aes_x]], y = y_dodged, color = methods)
  ) +
    geom_segment(aes(
      x = if (mode == "gamma") refline else log(refline),
      xend = .data[[aes_x]],
      y = y_dodged,
      yend = y_dodged
    ),
    linewidth = 0.6) +
    
    geom_point(size = 1.5) +
    
    geom_errorbarh(
      aes(xmin = .data[[aes_x_low_ci]], xmax = .data[[aes_x_up_ci]]),
      height = 0.1, alpha = 0.6
    ) +
    
    scale_y_continuous(
      breaks = unique(data$p_C_num),
      labels = levels(data$p_C)
    ) +
    
    scale_color_manual(
      name = "Approach",
      labels = c("Ignore C", "Exclude C", "Censor C", "Adjust for C"),
      values = c("#808080","#bf3729","#2f357c", "#e69b00"),
    ) +
    
    facet_wrap(~ risk_win_C_labs) +
    theme_bw() +
    theme(
      axis.title   = element_text(size = 12),
      axis.text    = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 12),
      legend.position = "bottom",
      strip.text   = element_text(size = 12)
    ) +
    labs(y = "Daily probability of C", x = xlabel) +
    coord_flip()
  
  # ---- SCALE LOGIC ----------------------------------------------------------
  
  if (mode == "gamma") {
    # linear scale
    p <- p +
      scale_x_continuous(
        breaks = x_break,
        limits = x_limits
      ) +
      geom_vline(xintercept = refline, linewidth = 1.2)
    
  } else if (mode == "irr") {
    # log scale, but axis shows IRR values
    p <- p +
      scale_x_continuous(
        breaks = log(irr_breaks),
        labels = irr_breaks,
        limits = log(irr_limits)
      ) +
      geom_vline(xintercept = log(refline), linewidth = 1.2)
  }
  
  print(p)
  dev.off()
}

lollipop_plot_noerror <- function(data,
                                  aes_x,
                                  mode = c("gamma", "irr"),   # "gamma" = linear scale, "irr" = log scale
                                  refline = 0,               # 0 for bias, 1 or 2 for IRR
                                  irr_breaks = NULL,         # vector of IRR ticks, e.g. c(0.5,1,2,4)
                                  irr_limits = NULL,         # two-element vector, IRR scale
                                  x_break = NULL,            # for gamma mode
                                  x_limits = NULL,           # for gamma mode
                                  xlabel = "",
                                  plot_name) {
  
  mode <- match.arg(mode)
  
  png(here("Plots", paste0(plot_name, ".png")), 
      width = 20, height = 9, units = "cm", res = 300)
  
  p <- ggplot(
    data,
    aes(x = .data[[aes_x]], y = y_dodged, color = methods)
  ) +
    geom_segment(aes(
      x = if (mode == "gamma") refline else log(refline),
      xend = .data[[aes_x]],
      y = y_dodged,
      yend = y_dodged
    ),
    linewidth = 0.6) +
    
    geom_point(size = 1.5) +
    
    scale_y_continuous(
      breaks = unique(data$p_C_num),
      labels = levels(data$p_C)
    ) +
    
    scale_color_manual(
      name = "Approach",
      labels = c("Ignore C", "Exclude C", "Censor C", "Adjust for C"),
      values = c("#808080","#bf3729","#2f357c", "#e69b00")
    ) +
    
    facet_wrap(~ risk_win_C_labs) +
    theme_bw() +
    theme(
      axis.title   = element_text(size = 12),
      axis.text    = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 12),
      strip.text   = element_text(size = 12)
    ) +
    labs(y = "Daily probability of C", x = xlabel) +
    coord_flip()
  
  # ---- SCALE LOGIC ----------------------------------------------------------
  
  if (mode == "gamma") {
    # linear scale
    p <- p +
      scale_x_continuous(
        breaks = x_break,
        limits = x_limits
      ) +
      geom_vline(xintercept = refline, linewidth = 1.2)
    
  } else if (mode == "irr") {
    # log scale, but axis shows IRR values
    p <- p +
      scale_x_continuous(
        breaks = log(irr_breaks),
        labels = irr_breaks,
        limits = log(irr_limits)
      ) +
      geom_vline(xintercept = log(refline), linewidth = 1.2)
  }
  
  print(p)
  dev.off()
}

# Lollipop plot variance  -----------------------------

lollipop_plot_var <- function(data, aes_x, aes_x_low_ci, aes_x_up_ci, plot_name){
  
  png(here("Plots", paste0(plot_name, ".png")), width = 18, height = 10, units = "cm", res = 300)
  
  p <- ggplot(data,
              aes(x = .data[[aes_x]],
                  y = y_dodged,
                  color = methods)) +
    
    geom_point(size = 1.5) +
    
    geom_errorbarh(aes(xmin = .data[[aes_x_low_ci]],
                       xmax = .data[[aes_x_up_ci]]),
                   width = 0.1,
                   alpha = 0.4) +
    
    scale_y_continuous(
      breaks = unique(data$p_C_num),
      labels = levels(data$p_C)
    ) +
    
    scale_x_continuous(
      limits = c(0, 0.6)) +
    
    scale_color_manual(
      name = "Approach",
      labels = c("Ignore C", "Exclude C", "Censor C", "Adjust for C"),
      values = c("#808080","#bf3729","#2f357c", "#e69b00"),
    ) +
    
    facet_wrap(~ risk_win_C_labs) +
    theme_bw() +
    theme(
      axis.title   = element_text(size = 12),
      axis.text    = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 12),
      legend.position = "bottom",
      strip.text   = element_text(size = 12)
    ) +
    labs(y = "Daily probability of C", x = expression("Empirical standard error of " * hat(gamma)[1])) +
    coord_flip()
  
  print(p)
  dev.off()
}

# Lollipop plot nr event  -----------------------------

nr_event_plot <- function(data, aes_x, plot_name){
  
  png(here("Plots", paste0(plot_name, ".png")), width = 18, height = 10, units = "cm", res = 300)
  
  p <- ggplot(data,
              aes(y = .data[[aes_x]],
                  x = y_dodged,
                  fill = methods)) +
    
    geom_bar(stat = "identity") +
    
    scale_x_continuous(
      breaks = unique(data$p_C_num),
      labels = levels(data$p_C)
    ) +
    
    scale_y_continuous(breaks = seq(from = 0, to = 2000, by = 200)) +
    
    scale_fill_manual(
      name = "Approach",
      labels = c("Ignore C", "Exclude C", "Censor C", "Adjust for C"),
      values = c("#808080","#bf3729","#2f357c", "#e69b00")
    ) +
    
    facet_wrap(~ risk_win_C_labs) +
    theme_bw() +
    theme(
      axis.title   = element_text(size = 12),
      axis.text    = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.text  = element_text(size = 12),
      legend.position = "bottom",
      strip.text   = element_text(size = 12)
    ) +
    labs(x = "Daily probability of C", y = "Mean number of events") #+
  #    coord_flip()
  
  print(p)
  dev.off()
}

# Nested loop plot
nested_loop_plot <- function(data,
                             yvars,
                             ylab = expression("Bias"),
                             loop_label = c("Effect U\u2192Y \n(OR = 1, 0.25, 2, 15)", "Effect C\u2192E \n(OR = 1, 0.25, 2, 5)",
                                            "Daily prob. C \n(0.0005, 0.001, 0.005)", "Duration C\u2192Y \n(10, 30, 60 days)"),
                             outfile = NULL,
                             refline = 0,
                             ylim,
                             ymax_ref,
                             y_log = FALSE,
                             ybreaks,
                             y_tick_label,
                             xlab = NULL,
                             legend_labels = c("Ignore C", "Exclude C", "Censor C", "Adjust for C"),
                             colours = c("#808080","#bf3729","#2f357c", "#e69b00"),
                             track_cex = 0.7,          # NEW: smaller track-label font
                             track_col = "#6c5d9e",    # NEW: track-label colour
                             track_offset = 0.004,     # NEW: track labels closer to axis
                             ylab_line = 3,            # horizontal distance of y title from the plot (3 text line)
                             ylab_at = NULL,           # NEW: vertical centre of y title
                             ylab_cex = 1,
                             xlab_line   = 1,          # distance of x title from the plot
                             legend_inset = -0.15,     # distance of legend from the plot
                             plot_width = 7,
                             plot_height = 7) {
  
  cols <- colours
  if (length(cols) < length(yvars)) cols <- rep(cols, length.out = length(yvars))
  
  if (is.null(ylab_at)) ylab_at <- mean(range(ybreaks))   # centre on the tick region
  
  if (!is.null(outfile))
    png(outfile, width = plot_width, height = plot_height, units = "in", res = 300)
  
  par(pty = "m", bg = "white", mar = c(5.5, 7, 2, 2), # margin: bottom, left, top, right
      mgp = c(3, 1, 0))
  
  if (y_log == FALSE) {
    plot(data$p_C, type = "n", ylim = ylim, bty = "n",
         xlab = "", ylab = "", las = 1, xaxt = "n", yaxt = "n")
  } else {
    plot(data$IRR_E, log = "y", type = "n", ylim = ylim, bty = "n",
         xlab = "", ylab = "", las = 1, xaxt = "n", yaxt = "n")
  }
  
  # all tick marks, no labels
  axis(side = 2, at = round(ybreaks, 1), labels = FALSE, las = 1)
  # labels only at the chosen positions, no extra ticks
  axis(side = 2, at = y_tick_label, labels = y_tick_label, tick = FALSE, las = 1)
  
  ## y-axis title, placed and centred by hand
  mtext(ylab, side = 2, line = ylab_line, at = ylab_at, las = 0, cex = ylab_cex)
  
  ## x-axis title
  mtext(xlab, side = 1, line = xlab_line, cex = 1)
  
  
  ## Background nested-loop grid
  lines.nestedloop(data, which = "v",
                   col = c("#454545", "#BBBBBB", "#DDDDDD", "#EEEEEE"))
  
  ## Step tracks with smaller, coloured names close to the axis
  lines.nestedloop2(data, which = "r",
                    varlabels = loop_label,
                    ymin.refline = ylim[1],
                    ymax.refline = ymax_ref,
                    log = y_log,
                    label.mode = "margin",
                    annotate.values = FALSE,
                    label.cex = track_cex,
                    label.col = track_col,
                    label.offset = track_offset)
  
  abline(h = refline, col = "black", lwd = 2)
  
  for (i in seq_along(yvars))
    lines(data[[yvars[i]]], col = cols[i], lwd = 1.5, type = "s")
  
  legend("bottom", horiz = TRUE, inset = c(0, legend_inset), # distance from the plot margin
         xpd = NA,
         lwd = 4, 
         col = cols[seq_along(yvars)], cex = 0.8,
         bty = "n", legend = legend_labels)
  
  if (!is.null(outfile)) dev.off()
}
