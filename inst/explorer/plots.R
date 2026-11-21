# Cohort explorer: table, plot and layout helpers shared by the modules.

# Headline number: a plain value box; `tone` colours the value of a status box.
stat_box <- function(title, value, icon = NULL, tone = c("neutral", "success", "warning", "danger"), sub = NULL) {
  tone <- match.arg(tone)
  value_box(title, if (tone == "neutral") value else tags$span(class = paste0("text-", tone), value),
            if (!is.null(sub)) tags$p(class = "rh-stat-sub", sub),
            showcase = if (!is.null(icon)) bs_icon(icon), showcase_layout = showcase_top_right(width = "2.5rem", max_height = "2.5rem"),
            theme = value_box_theme(bg = "#ffffff", fg = "#1f2328"),   # not "default", which paints icons with a gradient
            class = paste0("rh-stat rh-", tone), fill = FALSE)
}
# "k of n passed" over the checks that could be made (N/A excluded), and its tone.
checks_summary <- function(ck) {
  run <- ck$status != "N/A"
  list(text = sprintf("%d of %d", sum(ck$status[run] == "PASS"), sum(run)),
       tone = if (all(ck$status[run] == "PASS")) "success" else "danger")
}
# A row of buttons at their natural width (cards would otherwise stretch each to full width).
button_row <- function(...) tags$div(class = "rh-button-row", ...)
# What a page shows before it has anything to show.
empty_state <- function(icon, title, ...) {
  tags$div(class = "rh-empty", bs_icon(icon, size = "2rem"), tags$h5(title), tags$p(class = "text-muted mb-0", ...))
}

# Table controls in fixed places: search left and Copy/CSV right above the
# table; page size, row count and pages below it.
.dt_dom_data <- "<'rh-dt-top'fB>rt<'rh-dt-bottom'lip>"
.dt_dom_summary <- "<'rh-dt-top rh-dt-end'B>t"
.dt_dom_pages <- "t<'rh-dt-bottom'ip>"            # a short list with pages only
.dt_dom_search <- "<'rh-dt-top'f>t<'rh-dt-bottom'ip>"
dt_table <- function(df, page = 15, dom = .dt_dom_data, filter = "top", ...) {
  if (identical(dom, .dt_dom_data) && nrow(df) <= page) dom <- "<'rh-dt-top'fB>rt"   # it all fits: no pager
  n <- sort(unique(c(10, 15, 25, 50, 100, page)))
  DT::datatable(df, rownames = FALSE, filter = filter, extensions = "Buttons", fillContainer = FALSE,
                options = list(pageLength = page, scrollX = TRUE, dom = dom, buttons = c("copy", "csv"),
                               lengthMenu = list(c(n, -1), c(as.character(n), "All")),
                               autoWidth = FALSE, ...),
                class = "compact stripe hover")
}
# A results table: every row at once, no paging, search or column filters;
# copy and CSV export only.
summary_dt <- function(df, ...) {
  DT::datatable(df, rownames = FALSE, extensions = "Buttons", fillContainer = FALSE,
                options = list(dom = .dt_dom_summary, paging = FALSE, ordering = FALSE, scrollX = TRUE, buttons = c("copy", "csv"),
                               autoWidth = FALSE, ...),
                class = "compact stripe hover nowrap")
}
status_style <- function(dt, col = "status") {
  formatStyle(dt, col, color = styleEqual(c("PASS", "FAIL", "CHECK", "TRUE", "FALSE", "N/A"),
                                          c("#2f7d4f", "#a53f3f", "#a53f3f", "#2f7d4f", "#a53f3f", "#5c6670")),
              fontWeight = "bold")
}

# Empty base for plots built trace by trace (traces are added with inherit = FALSE).
# Plot toolbar without the plotly logo; the camera button saves an SVG, which scales for papers.
plot_config <- function(p) config(p, displaylogo = FALSE, toImageButtonOptions = list(format = "svg", filename = "rendehr-plot"))
blank_plot <- function() plot_ly()
# Placeholder plot with a message (plotly_empty() makes plotly warn "no trace type").
empty_plot <- function(txt = "No data") {
  plot_ly(x = 0, y = 0, type = "scatter", mode = "markers", marker = list(opacity = 0), hoverinfo = "skip",
          showlegend = FALSE) %>%
    layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
           annotations = list(list(text = txt, showarrow = FALSE, xref = "paper", yref = "paper", x = 0.5, y = 0.5,
                                   font = list(size = 14, color = "grey")))) %>%
    plot_config()
}
PAL <- c("#37474f", "#78909c", "#b0a58c", "#7f9a8f", "#9b8aa6", "#b08968", "#6f6f6f", "#c3c9cf",
         "#4f6a72", "#d0b49a")
group_colors <- function(var, levels) {
  p <- PALETTES[[var]]
  if (var == "source_label") p <- SRC_COLORS
  if (!is.null(p) && all(levels %in% names(p))) return(p[levels])
  stats::setNames(rep_len(PAL, length(levels)), levels)
}

# ---- patient time series ------------------------------------------------------------------------------------
# Units: NA and the string "NA" print as nothing.
clean_unit <- function(u) { u <- as.character(u); ifelse(is.na(u) | u == "NA", "", u) }
# Minutes since hospital admission -> x; the ICU clock starts at `icu0` (start of the chosen stay, minutes
# since hospital admission).
x_min <- function(m, axis, admit = NULL, icu0 = NULL) {
  switch(axis, hadm = m / 60, icu = (m - (icu0 %||% 0)) / 60,
         date = if (!is.null(admit) && !is.na(admit)) admit + m * 60 else m / 60)
}
x_of <- function(d, axis, admit = NULL, icu0 = NULL) {
  if (axis == "icu" && is.null(icu0)) return(d$t_icu / 60)
  x_min(d$t_hadm, axis, admit, icu0)
}
axis_title <- function(axis) {
  c(hadm = "Hours since hospital admission", icu = "Hours since ICU admission",
    date = "Date/time (de-identified, shifted)")[[axis]]
}
icu_shapes <- function(icu, axis, admit = NULL, icu0 = NULL) {
  if (!nrow(icu) || (axis == "icu" && is.null(icu0))) return(list())
  lapply(seq_len(nrow(icu)), function(i)
    list(type = "rect", xref = "x", yref = "paper", y0 = 0, y1 = 1, line = list(width = 0), layer = "below",
         x0 = x_min(icu$icu_in_min[i], axis, admit, icu0), x1 = x_min(icu$icu_out_min[i], axis, admit, icu0),
         fillcolor = "rgba(55,71,79,0.13)"))
}
# plotly drops layout(shapes = ) set on a subplot; set them on the built figure.
with_shapes <- function(p, shapes) {
  p <- plotly_build(p)
  p$x$layout$shapes <- shapes
  p
}

# Stacked panels sharing the x axis: panels = named list(panel title -> concepts); an optional medication lane at
# the bottom.
ts_panels_plot <- function(ts, panels, icu = NULL, dur = NULL, axis = "hadm", admit = NULL,
                           map_line = TRUE, height_per = 190, icu0 = NULL) {
  ts <- ts %>% filter(!is.na(value))
  panels <- Filter(function(cs) any(cs %in% ts$concept), panels)
  plots <- list()
  for (pn in names(panels)) {
    d <- ts %>% filter(concept %in% panels[[pn]])
    p <- plot_ly(type = "scattergl", mode = "lines+markers")
    for (j in seq_along(unique(d$concept))) {
      cn <- unique(d$concept)[j]; dd <- d %>% filter(concept == cn) %>% arrange(t_hadm)
      col <- PAL[(j - 1) %% 10 + 1]
      p <- p %>% add_trace(x = x_of(dd, axis, admit, icu0), y = dd$value, name = cn, legendgroup = cn, type = "scattergl",
                           mode = "lines+markers", marker = list(size = 4, color = col), line = list(width = 1, color = col),
                           text = sprintf("%s: %s %s<br>%.1f h since admission", cn, dd$value_chr, clean_unit(dd$unit),
                                          dd$t_hadm / 60), hoverinfo = "text")
    }
    if (map_line && any(d$concept == "map"))
      p <- p %>% add_lines(x = range(x_of(d, axis, admit, icu0), na.rm = TRUE), y = c(65, 65), name = "MAP 65",
                           line = list(dash = "dot", color = "grey50", width = 1), hoverinfo = "skip", showlegend = FALSE)
    plots[[pn]] <- p %>% layout(yaxis = list(title = list(text = pn, font = list(size = 10))))
  }
  lv_d <- rev(sort(unique(dur$drug)))
  if (!is.null(dur) && length(lv_d)) {
    mp <- blank_plot()
    dd <- dur %>% mutate(y = drug, e = pmax(end_min, start_min + 30))  # short exposures drawn 30 min wide
    neg <- rep_len(coalesce(as.logical(dd$negative_duration %||% FALSE), FALSE), nrow(dd))
    mp <- mp %>%
      add_segments(inherit = FALSE, x = x_min(dd$start_min, axis, admit, icu0), xend = x_min(dd$e, axis, admit, icu0),
                   y = dd$y, yend = dd$y, color = I("#b08968"), line = list(width = 6), showlegend = FALSE, hoverinfo = "text",
                   text = sprintf("%s<br>%.1f to %.1f h since admission (%.0f min)%s", dd$drug, dd$start_min / 60,
                                  dd$end_min / 60, dd$duration_min, if_else(neg, "<br>negative duration in the source", "")))
    plots[["Medications"]] <- mp %>%
      layout(yaxis = list(title = list(text = "Medications", font = list(size = 10)), tickfont = list(size = 8),
                          type = "category", categoryorder = "array", categoryarray = lv_d))
  }
  if (!length(plots)) return(list(p = empty_plot("No values for the selected concepts"), h = 200))
  hts <- sapply(names(plots), function(n) if (n == "Medications") max(1, min(3, length(lv_d) / 6)) else 1)
  shapes <- icu_shapes(icu %||% tibble(), axis, admit, icu0)
  h <- round(height_per * sum(hts) + 90)
  p <- subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE, heights = unname(hts / sum(hts)),
               margin = 0.012) %>%
    layout(xaxis = list(title = axis_title(axis)),
           legend = list(orientation = "h", y = -90 / h), hovermode = "closest") %>% plot_config()
  list(p = with_shapes(p, shapes), h = h)
}

# Small multiples: every selected concept of an admission, on one shared time range.
ts_grid_plot <- function(ts, concepts, axis = "hadm", admit = NULL, ncol = 3) {
  ts <- ts %>% filter(!is.na(value), concept %in% concepts)
  cs <- intersect(concepts, unique(ts$concept))
  if (!length(cs)) return(list(p = empty_plot(), h = 200))
  pl <- lapply(seq_along(cs), function(i) {
    d <- ts %>% filter(concept == cs[i]) %>% arrange(t_hadm); col <- PAL[(i - 1) %% 10 + 1]
    plot_ly(x = x_of(d, axis, admit), y = d$value, type = "scattergl", mode = "lines+markers", name = cs[i],
            showlegend = FALSE, marker = list(size = 3, color = col), line = list(width = 1, color = col),
            text = sprintf("%s: %s %s<br>%.1f h", cs[i], d$value_chr, clean_unit(d$unit), d$t_hadm / 60),
            hoverinfo = "text") %>%
      layout(annotations = list(list(text = sprintf("<b>%s</b> (%s)", cs[i], clean_unit(d$unit[1])), x = 0.5, y = 1.02,
                                     xref = "paper", yref = "paper", showarrow = FALSE, xanchor = "center",
                                     yanchor = "bottom", font = list(size = 10))))
  })
  nr <- ceiling(length(cs) / ncol)
  xr <- range(x_of(ts, axis, admit), na.rm = TRUE); xr <- xr + c(-0.02, 0.02) * max(diff(as.numeric(xr)), 1)
  p <- plotly_build(subplot(pl, nrows = nr, margin = c(0.02, 0.02, 0.9 / (nr * 3), 0.02), shareX = FALSE) %>%
                      plot_config())
  for (ax in grep("^xaxis", names(p$x$layout), value = TRUE)) p$x$layout[[ax]][c("range", "autorange")] <- list(xr, FALSE)
  list(p = p, h = 210 * nr + 40)
}

# Admissions of one patient (Gantt): in calendar time when dates exist,
# otherwise in sequence with fixed gaps. Unknown lengths are drawn as one day.
patient_timeline_plot <- function(adms, icus, mode = c("calendar", "sequence")) {
  mode <- match.arg(mode)
  if (!nrow(adms)) return(list(p = empty_plot(), h = 200))
  adms <- adms %>% mutate(los_draw = coalesce(hosp_los_min, 1440))
  if (mode == "calendar" && all(!is.na(adms$admittime))) {
    a <- adms %>% arrange(admittime) %>%
      mutate(x0 = admittime, x1 = admittime + los_draw * 60, xlab = "Date (de-identified, shifted)")
  } else {
    gap <- if (all(is.na(adms$admittime))) 30 else 3
    a <- adms %>% arrange(coalesce(as.numeric(admittime), as.numeric(year)), seq_hos, adm_id) %>%
      mutate(dur_d = los_draw / 1440, x0 = cumsum(lag(dur_d + gap, default = 0)), x1 = x0 + dur_d,
             xlab = if (gap == 30) "Days (no dates in this source; admissions in sequence, 30-day gaps)"
                    else "Days, admissions in sequence (3-day gaps; not to calendar scale)")
  }
  a <- a %>% mutate(row = sprintf("%s adm %s", if_else(in_cohort, "★", " "), adm_id), col = tl_group)
  rows_order <- rev(unique(a$row))
  cols <- TL_COLORS
  miss <- setdiff(unique(a$col), names(cols))
  if (length(miss)) cols <- c(cols, stats::setNames(rep_len(PAL, length(miss)), miss))
  p <- blank_plot()
  for (cc in unique(a$col)) {
    d <- a %>% filter(col == cc)
    p <- p %>% add_segments(inherit = FALSE, x = d$x0, xend = d$x1, y = d$row, yend = d$row,
                            line = list(width = 14, color = cols[[cc]]), name = cc, hoverinfo = "text",
                            text = sprintf("Admission %s<br>%s<br>%s | died in hospital: %s", d$adm_id, cc,
                                           if_else(is.na(d$hosp_los_min), "LOS unknown (drawn as 1 d)",
                                                   sprintf("LOS %.1f d", d$hosp_los_min / 1440)),
                                           if_else(d$hospital_mortality %in% 1, "yes", "no")))
  }
  if (nrow(icus)) {
    ii <- icus %>% inner_join(a %>% select(adm_id, x0, row), by = "adm_id") %>%
      mutate(s = if (inherits(a$x0, "POSIXct")) x0 + icu_in_min * 60 else x0 + icu_in_min / 1440,
             e = if (inherits(a$x0, "POSIXct")) x0 + icu_out_min * 60 else x0 + icu_out_min / 1440)
    if (nrow(ii))
      p <- p %>% add_segments(inherit = FALSE, x = ii$s, xend = ii$e, y = ii$row, yend = ii$row,
                              line = list(width = 6, color = "#e69f00"), name = "ICU stay", hoverinfo = "text",
                              text = sprintf("ICU stay %s (%s)<br>%.1f d", ii$icu_id, coalesce(ii$icu_unit, ""),
                                             (ii$icu_out_min - ii$icu_in_min) / 1440))
  }
  h <- 170 + 34 * nrow(a)
  list(p = p %>% layout(xaxis = list(title = a$xlab[1]),
                        yaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = rows_order),
                        legend = list(orientation = "h", y = -110 / h)) %>% plot_config(), h = h)
}
