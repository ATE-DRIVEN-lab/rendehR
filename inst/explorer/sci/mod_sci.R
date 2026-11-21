# ---- SCI-specific analyses ------------------------------------------------------------------------------------------------
CORD <- CAT_LEVELS[1:3]                                                                 # the SCI categories
CORD_ONLY <- c("level_group", "completeness", "completeness3", "coded_plegia")      # groupings defined for the cord categories
sci_ui <- function(id) {
  ns <- NS(id)
  all_box <- list(`actions-box` = TRUE)
  layout_sidebar(border = FALSE, border_radius = FALSE, fillable = TRUE, 
    sidebar = sidebar(width = 280, title = "Scope",
      checkboxGroupInput(ns("src"), "Source", stats::setNames(SRC, SRC_LABEL[SRC]), selected = SRC),
      checkboxInput(ns("mask"), "Mask counts < 10", TRUE), sliderInput(ns("minn"), "Minimum group size shown", 1, 100, 20),
      tags$p(class = "small text-muted", "Level group and completeness are defined for the spinal cord categories (traumatic, non-traumatic, unspecified SCI); grouping by them keeps those categories only.")),
    navset_card_underline(full_screen = TRUE, nav_panel("Neurological classification",
        layout_columns(col_widths = c(4, 4, 4), selectInput(ns("nc_desc"), "Descriptor", c("Neurological level group" = "level_group", "Lesion completeness / cord syndrome" = "completeness",
                                                                                             "Completeness (3 groups)" = "completeness3", "Coded plegia" = "coded_plegia")),
                       selectInput(ns("nc_by"), "Split by", c("SCI-D category" = "second_level", "Source" = "source_label", "Age group" = "age_group", "Coding period" = "icd_period")),
                       pickerInput(ns("nc_cat"), "Categories", CORD, selected = CORD, multiple = TRUE, options = all_box)),
        withSpinner(plotlyOutput(ns("nc_plot"), height = "460px")), DTOutput(ns("nc_tab"))),
      nav_panel("Complications",
        layout_columns(col_widths = c(3, 3, 3, 3), selectInput(ns("cx_by"), "Group by", c("SCI-D category" = "second_level", "Neurological level group" = "level_group",
                                                                                   "Completeness (3 groups)" = "completeness3", "Source" = "source_label", "SCI vs Not SCI" = "sci")),
                       pickerInput(ns("cx_sel"), "Complications", CX_LAB, selected = CX_LAB, multiple = TRUE, options = all_box),
                       pickerInput(ns("cx_cat"), "Categories", CAT_LEVELS, selected = CAT_LEVELS, multiple = TRUE, options = all_box),
                       radioButtons(ns("cx_view"), "View", c("Heat map" = "heat", "Forest (95% CI)" = "forest"), inline = TRUE)),
        withSpinner(plotlyOutput(ns("cx_plot"), height = "560px")), uiOutput(ns("cx_note")), DTOutput(ns("cx_tab"))),
      nav_panel("Interventions",
        layout_columns(col_widths = c(3, 3, 3, 3), selectInput(ns("iv_by"), "Group by", c("SCI-D category" = "second_level", "Neurological level group" = "level_group",
                                                                                   "Completeness (3 groups)" = "completeness3", "Source" = "source_label", "SCI vs Not SCI" = "sci")),
                       pickerInput(ns("iv_sel"), "Interventions", IV_LAB, selected = IV_LAB, multiple = TRUE, options = all_box),
                       pickerInput(ns("iv_cat"), "Categories", CAT_LEVELS, selected = CAT_LEVELS, multiple = TRUE, options = all_box),
                       tags$div(class = "pt-4", checkboxInput(ns("iv_icu"), "ICU admissions only", FALSE))),
        withSpinner(plotlyOutput(ns("iv_plot"), height = "600px")), uiOutput(ns("iv_note")), DTOutput(ns("iv_tab"))),
      nav_panel("MAP (first 72 h)",
        layout_columns(col_widths = c(4, 4, 4), sliderInput(ns("map_target"), "MAP target (mmHg)", 65, 100, 85, step = 1),
                       sliderInput(ns("map_share"), "'Mostly at target' = share of values ≥ target", 0.25, 0.9, 0.5, step = 0.05),
                       pickerInput(ns("map_cat"), "Categories", c("Traumatic SCI", "Spinal trauma control"), selected = c("Traumatic SCI", "Spinal trauma control"), multiple = TRUE, options = all_box)),
        tags$p(class = "small text-muted", "Every MAP value (20-200 mmHg) in the first 72 h of ICU for traumatic SCI and spinal trauma control; ICU stays with ≥ 6 values."),
        layout_columns(col_widths = c(6, 6), withSpinner(plotlyOutput(ns("map_box"), height = "430px")), withSpinner(plotlyOutput(ns("map_curve"), height = "430px"))),
        DTOutput(ns("map_tab"))),
      nav_panel("Surgery timing (MIMIC-IV)",
        layout_columns(col_widths = c(6, 6), pickerInput(ns("sg_cat"), "Categories", CAT_LEVELS, selected = c("Traumatic SCI", "Non-traumatic SCI", "Spinal trauma control"), multiple = TRUE, options = all_box),
                       sliderInput(ns("sg_max"), "Show days up to", 3, 30, 10)),
        withSpinner(plotlyOutput(ns("sg_plot"), height = "430px")),
        tags$p(class = "small text-muted", "First spine procedure of MIMIC-IV admissions; procedures dated before the admission date are shown as 'Before admission' and counted in 'By day 1' / 'By day 3'."),
        DTOutput(ns("sg_tab"))),
      nav_panel("Outcomes by impairment",
        layout_columns(col_widths = c(4, 4, 4), selectInput(ns("oi_by"), "Impairment", c("Neurological level group" = "level_group", "Completeness (3 groups)" = "completeness3",
                                                                                     "Lesion completeness / cord syndrome" = "completeness", "Coded plegia" = "coded_plegia")),
                       pickerInput(ns("oi_cat"), "Categories", CORD, selected = "Traumatic SCI", multiple = TRUE, options = all_box),
                       pickerInput(ns("oi_out"), "Outcomes", c("In-hospital mortality" = "hospital_mortality", "1-year mortality (MIMIC)" = "death_1y", "ICU admission" = "any_icu",
                                                             "Mechanical ventilation" = "mechanical_ventilation", "Tracheostomy" = "tracheostomy", "Discharged to rehabilitation" = "discharged_rehab"),
                                   selected = c("hospital_mortality", "death_1y", "mechanical_ventilation", "tracheostomy", "discharged_rehab"), multiple = TRUE, options = all_box)),
        withSpinner(plotlyOutput(ns("oi_plot"), height = "500px")), uiOutput(ns("oi_note")), DTOutput(ns("oi_tab"))),
      nav_panel("Epidemiology and trends",
        layout_columns(col_widths = c(6, 6),
          tags$div(class = "mb-4", tags$h6("Traumatic SCI: cause of injury by age (MIMIC; % of admissions with a cause code)"), withSpinner(plotlyOutput(ns("ep_cause"), height = "380px")), uiOutput(ns("ep_cause_note"))),
          tags$div(class = "mb-4", tags$h6("Traumatic SCI: male share by age"), withSpinner(plotlyOutput(ns("ep_sex"), height = "380px")))),
        layout_columns(col_widths = c(6, 6),
          tags$div(class = "mb-4", tags$h6("MIMIC-IV: category mix over time (approximate calendar period)"), selectInput(ns("ep_unit"), NULL, c("Share" = "pct", "Count" = "n")),
               withSpinner(plotlyOutput(ns("ep_trend"), height = "380px"))),
          tags$div(class = "mb-4", tags$h6("Injury level recorded in the codes"), selectInput(ns("ep_lvlcat"), NULL, CAT_LEVELS, "Traumatic SCI"), withSpinner(plotlyOutput(ns("ep_lvl"), height = "330px")))))
    ))
}
sci_server <- function(id) moduleServer(id, function(input, output, session) {
  A <- reactive({ validate(need(length(input$src) > 0, "Select at least one source.")); ADM %>% filter(source %in% input$src) })
  masked <- function(k) isTRUE(input$mask) & k >= 1 & k < 10
  pick <- function(x, what = "category") need(length(x) > 0, sprintf("Select at least one %s.", what))
  ci_txt <- function(k, lo, hi) if_else(masked(k), "–", sprintf("%.1f-%.1f%%", 100 * lo, 100 * hi))
  note <- function(...) { x <- c(...); if (length(x)) tags$p(class = "small text-muted mb-1", paste(x, collapse = " ")) }
  # Forest plot: groups dodged around each row, colours fixed per group; masked cells (1-9 events) are not drawn.
  forest <- function(d, xlab, var, rows) {
    lv <- grp_levels(d$grp); m <- masked(d$k); rows <- intersect(rows, d$lab); ng <- length(lv); w <- min(0.8 / ng, 0.25)
    d <- d[!m, ] %>% mutate(txt = sprintf("%s | %s<br>%s, 95%% CI %.1f-%.1f%%, n = %s", grp, lab, kpct(k, n, FALSE), 100 * lo, 100 * hi, fmt_n(n)),
                            y = length(rows) + 1 - match(lab, rows) + ((ng + 1) / 2 - match(grp, lv)) * w, grp = factor(grp, intersect(lv, grp)))
    validate(need(nrow(d) > 0, "Every cell has 1-9 events (masked)."))
    plot_ly(d, y = ~y, x = ~p, color = ~grp, colors = group_colors(var, grp_levels(ADM[[var]]))[levels(d$grp)], type = "scatter", mode = "markers",
            error_x = list(type = "data", symmetric = FALSE, array = ~hi - p, arrayminus = ~p - lo, thickness = 1.2, width = 2), text = ~txt, hoverinfo = "text") %>%
      layout(xaxis = list(title = xlab, tickformat = ".0%", rangemode = "tozero"),
             yaxis = list(title = "", tickvals = rev(seq_along(rows)), ticktext = rows, zeroline = FALSE, showgrid = FALSE, range = c(0.4, length(rows) + 0.6)),
             shapes = lapply(seq_along(rows)[-1] - 0.5, function(b) list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = b, y1 = b, line = list(color = "#e3e6e8", width = 1))),
             legend = list(orientation = "h", y = -0.15, traceorder = "normal")) %>% plot_config()
  }
  heat <- function(d, rows) {
    lv <- grp_levels(d$by); m <- masked(d$k); rows <- intersect(rows, d$lab)
    d <- d %>% mutate(z = if_else(m, NA_real_, p), txt = if_else(m, sprintf("%s | %s<br><10 events (masked)", by, lab),
                                                                  sprintf("%s | %s<br>%s, n = %s", by, lab, kpct(k, n, FALSE), fmt_n(n)))) %>%
      arrange(match(by, lv), match(lab, rows))
    if (all(is.na(d$z))) return(empty_plot("Every cell is masked by the small-cell rule."))
    plot_ly(d, x = ~by, y = ~lab, z = ~z, type = "heatmap", colorscale = "Reds", zmin = 0, text = ~txt, hoverinfo = "text",
            colorbar = list(title = "Share", tickformat = ".0%")) %>%
      layout(xaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = lv),
             yaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = rev(rows)), margin = list(l = 10)) %>%
      plot_config()
  }
  # Footnotes of a rate page: variables a source does not capture, groups under the minimum size, masked cells.
  rate_note <- function(all, vars, labs) {
    nc <- NOT_CAPTURED %>% filter(source %in% input$src, var %in% vars) %>% mutate(src = SRC_LABEL[source], l = names(labs)[match(var, labs)]) %>%
      group_by(src) %>% summarise(t = paste(l, collapse = ", "), .groups = "drop")
    hid <- all %>% group_by(by) %>% summarise(n = max(n), .groups = "drop") %>% filter(n < input$minn)
    k <- sum(all$n >= input$minn & masked(all$k))
    note(if (nrow(nc)) paste0("Not captured (no event in the whole source; left out instead of shown as 0%): ", paste(sprintf("%s: %s", nc$src, nc$t), collapse = "; "), "."),
         if (nrow(hid)) sprintf("Groups below the minimum size (%d): %s.", input$minn, paste(grp_levels(hid$by), collapse = ", ")),
         if (k) sprintf("%d cell%s with 1-9 events not drawn (Mask counts < 10).", k, if (k > 1) "s" else ""))
  }

  ncd <- reactive({ validate(pick(input$nc_cat)); neuro_counts(A(), input$nc_cat, input$nc_desc, input$nc_by) %>% filter(N >= input$minn) })
  output$nc_plot <- renderPlotly({ d <- ncd(); validate(need(nrow(d) > 0, "No groups."))
    vl <- grp_levels(d$val); bl <- grp_levels(d$by)
    d <- d %>% mutate(lab = sprintf("%s | %s: %s", by, val, mask_kp(n, pct, input$mask)), val = factor(val, vl))
    plot_ly(d, y = ~by, x = ~pct, color = ~val, colors = seq_colors(vl), type = "bar", orientation = "h", text = ~lab, hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "stack", xaxis = list(tickformat = ".0%", title = ""), yaxis = list(title = "", type = "category", categoryorder = "array", categoryarray = rev(bl)),
             legend = list(orientation = "h", y = -0.2, traceorder = "normal")) %>% plot_config() })
  output$nc_tab <- renderDT(dt_table(ncd() %>% arrange(match(by, grp_levels(by)), match(val, grp_levels(val))) %>%
                                       transmute(Group = by, Value = val, `n (%)` = kpct(n, N, input$mask), N = fmt_n(N)), page = 20))

  cx_all <- reactive({ validate(pick(input$cx_sel, "complication"), pick(input$cx_cat))
    event_rates(sci_scope(A() %>% filter(second_level %in% input$cx_cat), input$cx_by), input$cx_by, input$cx_sel) })
  cxd <- reactive(cx_all() %>% filter(n >= input$minn) %>% bind_cols(wilson(.$k, .$n)) %>% mutate(lab = names(CX_LAB)[match(var, CX_LAB)]))
  output$cx_plot <- renderPlotly({ d <- cxd(); validate(need(nrow(d) > 0, "No groups."))
    if (input$cx_view == "heat") heat(d, names(CX_LAB))
    else forest(d %>% rename(grp = by), "Flagged admissions with the complication", input$cx_by, names(CX_LAB)) })
  output$cx_note <- renderUI(rate_note(cx_all(), input$cx_sel, CX_LAB))
  output$cx_tab <- renderDT(dt_table(cxd() %>% arrange(match(var, CX_LAB), match(by, grp_levels(by))) %>% transmute(Complication = lab, Group = by, `n (%)` = kpct(k, n, input$mask), `95% CI` = ci_txt(k, lo, hi)), page = 15))

  iv_all <- reactive({ validate(pick(input$iv_sel, "intervention"), pick(input$iv_cat))
    d <- A() %>% filter(second_level %in% input$iv_cat); if (isTRUE(input$iv_icu)) d <- d %>% filter(any_icu %in% 1)
    event_rates(sci_scope(d, input$iv_by), input$iv_by, input$iv_sel) })
  ivd <- reactive(iv_all() %>% filter(n >= input$minn) %>% bind_cols(wilson(.$k, .$n)) %>% mutate(lab = names(IV_LAB)[match(var, IV_LAB)]))
  output$iv_plot <- renderPlotly({ d <- ivd(); validate(need(nrow(d) > 0, "No groups."))
    forest(d %>% rename(grp = by), sprintf("%s receiving the intervention", if (isTRUE(input$iv_icu)) "ICU admissions" else "Flagged admissions"), input$iv_by, names(IV_LAB)) })
  output$iv_note <- renderUI(rate_note(iv_all(), input$iv_sel, IV_LAB))
  output$iv_tab <- renderDT(dt_table(ivd() %>% arrange(match(var, IV_LAB), match(by, grp_levels(by))) %>% transmute(Intervention = lab, Group = by, `n (%)` = kpct(k, n, input$mask), `95% CI` = ci_txt(k, lo, hi)), page = 15))

  mapd <- reactive({ validate(pick(input$map_cat))
    MAP_VALUES %>% filter(source %in% input$src) %>% group_by(source, icu_id) %>%
      summarise(n = n(), map_median = median(v), share = mean(v >= input$map_target), lt65 = mean(v < 65), .groups = "drop") %>% filter(n >= 6) %>%
      inner_join(MAP_STAY %>% select(source, icu_id, second_level), by = c("source", "icu_id")) %>% filter(second_level %in% input$map_cat) %>%
      mutate(source_label = SRC_LABEL[source]) })
  output$map_box <- renderPlotly({ d <- mapd(); validate(need(nrow(d) > 0, "No ICU stays."))
    lv <- intersect(CAT_LEVELS, d$second_level)
    d <- d %>% mutate(second_level = factor(second_level, lv), source_label = factor(source_label, intersect(SRC_LABEL[SRC], source_label)))
    plot_ly(d, x = ~source_label, y = ~share, color = ~second_level, colors = CAT_COLORS[lv], type = "box", boxpoints = FALSE) %>%
      layout(boxmode = "group", yaxis = list(title = sprintf("Share of MAP values ≥ %d mmHg per stay", input$map_target), tickformat = ".0%"), xaxis = list(title = ""),
             legend = list(orientation = "h", y = -0.15)) %>% plot_config() })
  output$map_curve <- renderPlotly({ validate(pick(input$map_cat))
    d <- map_target_curve(input$src, input$map_cat, input$map_share)
    validate(need(nrow(d) > 0, "No ICU stays."))
    dash <- stats::setNames(c("solid", "dash", "dot", "dashdot")[seq_along(SRC)], SRC); p <- blank_plot()
    for (cc in intersect(CAT_LEVELS, d$second_level)) for (s in intersect(SRC, d$source)) { x <- d %>% filter(second_level == cc, source == s)
      p <- p %>% add_lines(inherit = FALSE, x = x$target, y = x$p, name = paste(cc, SRC_LABEL[[s]], sep = " | "), line = list(color = CAT_COLORS[[cc]], dash = dash[[s]]),
                           text = sprintf("%s | %s<br>target %d: %.1f%% of %s stays", cc, SRC_LABEL[[s]], x$target, 100 * x$p, fmt_n(x$stays)), hoverinfo = "text") }
    p %>% layout(xaxis = list(title = "MAP target (mmHg)"), yaxis = list(title = sprintf("ICU stays with ≥ %.0f%% of values at target", 100 * input$map_share), tickformat = ".0%", range = c(0, 1)),
             shapes = list(list(type = "line", x0 = input$map_target, x1 = input$map_target, y0 = 0, y1 = 1, yref = "paper", line = list(dash = "dot", color = "grey"))),
             legend = list(orientation = "h", y = -0.25)) %>% plot_config() })
  output$map_tab <- renderDT(dt_table(mapd() %>% group_by(Category = second_level, Source = source_label) %>%
    summarise(`ICU stays` = mask_n(n(), input$mask), `MAP median [IQR]` = med_iqr(map_median), `Share ≥ target, median [IQR]` = med_iqr(100 * share, 0),
              `Stays mostly at target` = sprintf("%.1f%%", 100 * mean(share >= input$map_share)), `Share < 65, median [IQR]` = med_iqr(100 * lt65, 0), .groups = "drop") %>%
    arrange(match(Category, CAT_LEVELS), match(Source, SRC_LABEL[SRC])), page = 10))

  sgd <- reactive({ validate(need("mimic_iv" %in% input$src, "Surgery timing is available for MIMIC-IV only: select MIMIC-IV under Source."), pick(input$sg_cat))
    SURG %>% filter(second_level %in% input$sg_cat) })
  output$sg_plot <- renderPlotly({ d <- sgd(); validate(need(nrow(d) > 0, "No operated admissions."))
    mx <- input$sg_max; xl <- c("Before admission", as.character(seq_len(mx) - 1), paste0(mx, "+")); lv <- intersect(CAT_LEVELS, d$second_level)
    x <- d %>% mutate(dd = case_when(days < 0 ~ "Before admission", days >= mx ~ paste0(mx, "+"), TRUE ~ as.character(days))) %>%
      count(second_level, dd) %>% group_by(second_level) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
      mutate(second_level = factor(second_level, lv), dd = factor(dd, xl))
    plot_ly(x, x = ~dd, y = ~p, color = ~second_level, colors = group_colors("second_level", lv), type = "bar",
            text = ~sprintf("%s, %s: %s", second_level, if_else(dd == "Before admission", "before admission", paste("day", dd)), mask_kp(n, p, input$mask)),
            hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "group", xaxis = list(title = "Days from admission to first spine procedure", type = "category", categoryorder = "array", categoryarray = xl),
             yaxis = list(title = "Operated admissions", tickformat = ".0%"), legend = list(orientation = "h", y = -0.2)) %>% plot_config() })
  output$sg_tab <- renderDT(dt_table(surgery_summary(sgd()) %>% arrange(match(second_level, CAT_LEVELS)) %>%
    transmute(Category = second_level, `Operated admissions` = mask_n(n, input$mask), `Before admission` = mask_n(before, input$mask), `Days, median [IQR]` = med,
              `By day 1` = sprintf("%.1f%%", 100 * by1), `By day 3` = sprintf("%.1f%%", 100 * by3)), page = 10))

  oid <- reactive({ validate(pick(input$oi_cat), pick(input$oi_out, "outcome"))
    A() %>% filter(second_level %in% input$oi_cat, !is.na(.data[[input$oi_by]])) %>% select(source, by = all_of(input$oi_by), all_of(input$oi_out)) %>%
      mutate(by = as.character(by)) %>% pivot_longer(-c(source, by), names_to = "o", values_to = "y") %>%
      filter(!(o == "any_icu" & source == "eicu")) %>%                                   # every eICU-CRD admission is an ICU stay
      group_by(by, o) %>% summarise(k = sum(y, na.rm = TRUE), n = sum(!is.na(y)), .groups = "drop") %>%
      filter(n >= input$minn) %>% bind_cols(wilson(.$k, .$n)) %>% mutate(lab = var_label(o)) })
  output$oi_plot <- renderPlotly({ d <- oid(); validate(need(nrow(d) > 0, "No groups with enough admissions."))
    forest(d %>% transmute(grp = by, lab, k, n, p, lo, hi), "Admissions", input$oi_by, var_label(input$oi_out)) })
  output$oi_note <- renderUI(note(
    if ("discharged_rehab" %in% input$oi_out) "Discharged to rehabilitation: among admissions with a known discharge location.",
    if ("any_icu" %in% input$oi_out && "eicu" %in% input$src) "ICU admission excludes eICU-CRD, where every admission is an ICU stay by design.",
    if ("death_1y" %in% input$oi_out) "1-year mortality: MIMIC only."))
  output$oi_tab <- renderDT(dt_table(oid() %>% arrange(match(by, grp_levels(by)), match(o, input$oi_out)) %>%
                                       transmute(Group = by, Outcome = lab, `n (%)` = kpct(k, n, input$mask), `95% CI` = ci_txt(k, lo, hi)), page = 20))

  tz <- reactive(A() %>% filter(second_level == "Traumatic SCI"))
  cause <- reactive(cause_by_age(tz()))
  output$ep_cause <- renderPlotly({
    x <- cause() %>% filter(den >= input$minn); validate(need(nrow(x) > 0, "No MIMIC admissions with a cause code in the selection."))
    x <- x %>% mutate(txt = if_else(masked(n), sprintf("%s, %s: <10", age_group, c), sprintf("%s, %s: %.1f%% (%s of %s)", age_group, c, 100 * p, fmt_n(n), fmt_n(den))))
    plot_ly(x, x = ~age_group, y = ~p, color = ~c, type = "bar", text = ~txt, hoverinfo = "text", textposition = "none") %>%
      layout(barmode = "group", yaxis = list(tickformat = ".0%", title = ""), xaxis = list(title = ""), legend = list(orientation = "h", y = -0.12)) %>% plot_config() })
  output$ep_cause_note <- renderUI({ h <- cause() %>% distinct(age_group, den) %>% filter(den < input$minn)
    note(if (nrow(h)) sprintf("Age groups below the minimum size (%d) not shown: %s.", input$minn, paste(as.character(sort(h$age_group)), collapse = ", "))) })
  output$ep_sex <- renderPlotly({
    x <- tz() %>% filter(!is.na(sex)) %>% group_by(age_group) %>% summarise(k = sum(sex == "male"), N = n(), .groups = "drop") %>% filter(N >= input$minn)
    validate(need(nrow(x) > 0, "No groups."))
    x <- bind_cols(x, wilson(x$k, x$N))
    plot_ly(x, x = ~age_group, y = ~p, type = "scatter", mode = "lines+markers", error_y = list(type = "data", symmetric = FALSE, array = ~hi - p, arrayminus = ~p - lo),
            text = ~sprintf("%s: %.1f%% male (n = %s)", age_group, 100 * p, fmt_n(N)), hoverinfo = "text") %>%
      layout(yaxis = list(tickformat = ".0%", title = "Male"), xaxis = list(title = "Age group")) %>% plot_config() })
  output$ep_trend <- renderPlotly({
    validate(need(nrow(MIV_ANCHOR) > 0, "Needs the live MIMIC-IV patients table (anchor years)."))
    x <- mimic_iv_trends() %>% mutate(y = if (input$ep_unit == "pct") pct else n, second_level = factor(second_level, intersect(CAT_LEVELS, second_level)),
                                      txt = sprintf("%s, %s: %s", period, second_level, mask_kp(n, pct, input$mask)))
    plot_ly(x, x = ~period, y = ~y, color = ~second_level, colors = CAT_COLORS, type = "scatter", mode = "lines+markers", text = ~txt, hoverinfo = "text") %>%
      layout(yaxis = list(title = "", tickformat = if (input$ep_unit == "pct") ".0%" else ","), xaxis = list(title = "Approximate period (ICD-10 from Oct 2015)"), legend = list(orientation = "h", y = -0.3)) %>%
      plot_config() })
  output$ep_lvl <- renderPlotly({
    x <- A() %>% filter(second_level == input$ep_lvlcat) %>% count(injury_level) %>% mutate(p = n / sum(n))
    plot_ly(x, x = ~injury_level, y = ~p, type = "bar", marker = list(color = CAT_COLORS[[input$ep_lvlcat]]), text = ~sprintf("%s: %s", injury_level, mask_kp(n, p, input$mask)), hoverinfo = "text", textposition = "none") %>%
      layout(yaxis = list(tickformat = ".0%", title = ""), xaxis = list(title = "")) %>% plot_config() })
})
# MIMIC-IV approximate calendar year of an admission (anchor_year_group midpoint + years since anchor_year), as in analysis/R/11
MIV_ANCHOR <- tryCatch(rendehR::fetch_table("mimic_iv", "patients") %>% transmute(patient_id = as.character(subject_id), anchor_year, anchor_year_group),
                       error = function(e) tibble(patient_id = character(), anchor_year = integer(), anchor_year_group = character()))
adm_year_shift <- function(d) { a <- MIV_ANCHOR[match(d$patient_id, MIV_ANCHOR$patient_id), ]
  as.integer(substr(a$anchor_year_group, 1, 4)) + 1 + (as.integer(format(d$admittime, "%Y")) - a$anchor_year) }

# share of ICU stays with >= `share` of their MAP values at or above each integer target (65-100 mmHg).
# MAP values are binned to whole mmHg: for integer targets floor(v) >= target is identical to v >= target.
MAP_BINS <- MAP_VALUES %>% mutate(b = floor(v)) %>% count(source, icu_id, b) %>%
  inner_join(MAP_STAY %>% select(source, icu_id, second_level), by = c("source", "icu_id"))
map_target_curve <- function(src, cats, share, targets = 65:100) {
  x <- MAP_BINS %>% filter(source %in% src, second_level %in% cats)
  bind_rows(lapply(targets, function(thr) x %>% group_by(second_level, source, icu_id) %>%
                     summarise(s = sum(n[b >= thr]) / sum(n), .groups = "drop") %>%
                     group_by(second_level, source) %>% summarise(p = mean(s >= share), stays = n(), .groups = "drop") %>% mutate(target = thr)))
}

# ---- computations shared by the page and the tests ----------------------------------------------------------
# (variable x source) pairs without a single event in the whole source: the source does not record them.
NOT_CAPTURED <- ADM %>% group_by(source) %>% summarise(across(all_of(c(unname(CX_LAB), unname(IV_LAB))), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-source, names_to = "var", values_to = "k") %>% filter(k == 0) %>% select(source, var)
# Cord-only groupings keep the cord categories.
sci_scope <- function(d, by) { d <- d %>% filter(!is.na(.data[[by]])); if (by %in% CORD_ONLY) d <- d %>% filter(second_level %in% CORD); d }
# Events per group; sources that do not capture a variable are left out of its denominator.
event_rates <- function(d, by, vars) d %>% select(source, by = all_of(by), all_of(vars)) %>% mutate(by = as.character(by)) %>%
  pivot_longer(-c(source, by), names_to = "var", values_to = "y") %>% anti_join(NOT_CAPTURED, by = c("source", "var")) %>%
  group_by(by, var) %>% summarise(k = sum(y, na.rm = TRUE), n = sum(!is.na(y)), .groups = "drop")
neuro_counts <- function(d, cats, desc, by) d %>% filter(second_level %in% cats, !is.na(.data[[desc]])) %>%
  count(by = as.character(.data[[by]]), val = .data[[desc]]) %>% group_by(by) %>% mutate(N = sum(n), pct = n / N) %>% ungroup()
surgery_summary <- function(d) d %>% group_by(second_level) %>%                  # as T31c: procedures before admission included
  summarise(n = n(), before = sum(days < 0), med = med_iqr(days, 0), by1 = mean(days <= 1), by3 = mean(days <= 3), .groups = "drop")
cause_by_age <- function(tz) { d <- tz %>% filter(!is.na(inj_cats))                        # % of admissions with a cause code (multi-label)
  d %>% mutate(c = str_split(inj_cats, fixed(" | "))) %>% tidyr::unnest(c) %>% count(age_group, c) %>%
    left_join(d %>% count(age_group, name = "den"), by = "age_group") %>% mutate(p = n / den) }
mimic_iv_trends <- function() { d <- ADM %>% filter(source == "mimic_iv", !is.na(admittime))
  d %>% mutate(period = cut(adm_year_shift(d), c(2007, 2010, 2013, 2016, 2019, 2023), labels = c("2008-10", "2011-13", "2014-16", "2017-19", "2020-22"))) %>%
    filter(!is.na(period)) %>% count(period, second_level) %>% group_by(period) %>% mutate(pct = n / sum(n)) %>% ungroup() }
