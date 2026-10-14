server <- function(input, output, session) {

  # Active tab — macrofiscal is the landing page
  active_tab <- reactiveVal("macrofiscal")

  observeEvent(input$active_tab, {
    active_tab(input$active_tab)
    updateTabsetPanel(session, "main_tabs", selected = input$active_tab)
  })

  # Core reactive: load all country data on selection change
  cdata <- reactive({
    iso <- req(input$country_select)

    scores <- latest_tiers |> filter(country_code == iso)

    wdi <- dbGetQuery(con, "
      SELECT indicator_id, period, value
      FROM bronze.wb_wdi_raw
      WHERE country_code = ?
      ORDER BY period
    ", params = list(iso))

    weo <- dbGetQuery(con, "
      SELECT indicator_code, year, value
      FROM bronze.imf_weo_annual
      WHERE iso3c = ?
      ORDER BY year
    ", params = list(iso))

    pop <- dbGetQuery(con, "
      SELECT year, population
      FROM bronze.population_annual
      WHERE iso3c = ?
      ORDER BY year DESC
      LIMIT 1
    ", params = list(iso))

    gdp <- dbGetQuery(con, "
      SELECT year, gdp_const_usd, gdp_per_capita_const_usd
      FROM bronze.gdp_annual
      WHERE iso3c = ?
      ORDER BY year DESC
      LIMIT 1
    ", params = list(iso))

    list(scores = scores, wdi = wdi, weo = weo, pop = pop, gdp = gdp)
  })

  # Helper: filter WDI to one indicator
  wdi_s <- function(df, id) df |> filter(indicator_id == id) |> arrange(period)

  # Helper: filter WEO to one indicator
  weo_s <- function(df, code) df |> filter(indicator_code == code) |> arrange(year)

  # Helper: latest non-NA value
  latest_ind <- function(df) {
    v <- df$value[!is.na(df$value)]
    if (length(v) == 0) NA_real_ else tail(v, 1)
  }

  # Helper: attach ResizeObserver to any full-size echarts chart
  ro_render <- function(chart) {
    htmlwidgets::onRender(chart, "function(el) {
      var inst = echarts.getInstanceByDom(el);
      if (!inst) return;
      if (window.ResizeObserver) {
        var ro = new ResizeObserver(function() { inst.resize(); });
        ro.observe(el.parentElement || el);
      } else {
        setTimeout(function() { inst.resize(); }, 300);
      }
    }")
  }


  # ---- Country header -------------------------------------------------------

  output$country_header_ui <- renderUI({
    d <- cdata(); s <- d$scores
    if (nrow(s) == 0) return(NULL)

    iso3 <- s$country_code[1]
    iso2 <- tryCatch(
      tolower(countrycode::countrycode(iso3, "iso3c", "iso2c")),
      error = function(e) tolower(substr(iso3, 1, 2))
    )
    if (is.na(iso2)) iso2 <- tolower(substr(iso3, 1, 2))

    pop <- if (nrow(d$pop) > 0) fmt_big(d$pop$population[1]) else "N/A"

    tags$div(class = "country-header",
      tags$img(src   = paste0("https://flagcdn.com/h40/", iso2, ".png"),
               class = "country-flag", alt = s$country_name[1], height = "36px"),
      tags$div(
        tags$div(class = "country-name", s$country_name[1]),
        tags$div(class = "country-meta",
          paste0(s$region[1], " \u00b7 ", s$income_group[1], " \u00b7 Pop: ", pop))
      )
    )
  })


  # ---- Score cards ----------------------------------------------------------

  output$score_cards_ui <- renderUI({
    s  <- cdata()$scores
    at <- active_tab()

    make_dim_card <- function(tab_id, label, score_col) {
      sc  <- if (nrow(s) > 0) s[[score_col]][1] else NA_real_
      col <- score_color(sc)
      val <- if (is.na(sc)) "N/A" else sprintf("%.3f", sc)
      pct <- if (is.na(sc)) 0 else min(sc * 100, 100)

      tip <- switch(tab_id,
        macrofiscal = paste0(
          "Scores a country's latent capacity to absorb external shocks from its fiscal position. ",
          "Three indicators enter a Bayesian model: current account balance, fiscal balance ",
          "(net govt lending/borrowing), and government gross debt — all as % of GDP. ",
          "Debt is negated so all three indicators share a protective orientation: higher scores mean ",
          "widening deficits and mounting debt that erode the capacity to respond without additional borrowing."
        ),
        importreliance = paste0(
          "Measures structural vulnerability from concentrated import sourcing across food, energy, ",
          "cereals, and fertilizers. Each annual indicator is 1 minus the Herfindahl-Hirschman Index (HHI) ",
          "of import partner concentration — a score near zero means near-total dependence on a single source country. ",
          "A monthly layer adds acute signals: food and energy import volume shortfalls (z-score vs. seasonal baseline) ",
          "and price volatility using the Morgan-Reinhardt basket CV specification."
        ),
        exportfragility = paste0(
          "Measures structural export concentration and revenue dependence. Three annual indicators: ",
          "1 minus partner HHI (buyer diversification), 1 minus commodity HHI (product diversification), ",
          "and export independence (1 minus the Pink Sheet commodity share of total exports). ",
          "Monthly signals track export volume shortfalls and price collapses relative to seasonal norms, ",
          "plus a weighted basket price volatility (CV) measure. CV above 0.30 signals elevated instability; ",
          "above 0.40 is a structural concern."
        ),
        network = paste0(
          "A deterministic (non-Bayesian) measure of risk inherited through trade networks. ",
          "A country's D4 score is the trade-weighted average of its import partners' composite risk ",
          "scores (D1 + D2 + D3), with weights restricted to food, energy, and fertilizer imports. ",
          "A country sourcing critical inputs from structurally fragile partners inherits a share of their ",
          "vulnerability. Credible intervals propagate from the D1–D3 posterior bounds through the same formula."
        ),
        ""
      )

      tags$div(
        class   = paste("score-card", if (tab_id == at) "score-card-active" else ""),
        id      = paste0("card-", tab_id),
        onclick = sprintf("selectCard('%s')", tab_id),
        style   = sprintf("border-left-color:%s", col),
        tags$div(class = "score-card-label",
          label,
          if (nchar(tip) > 0) tags$span(
            class   = "dim-pill",
            onclick = "event.stopPropagation()",
            "more info",
            tags$span(class = "dim-tooltip", tip)
          )
        ),
        tags$div(class = "score-card-value", style = sprintf("color:%s", col), val),
        tags$div(class = "score-bar-wrap",
          tags$div(class = "score-bar-fill",
                   style = sprintf("width:%.1f%%;background:%s", pct, col)))
      )
    }

    mean_sc  <- if (nrow(s) > 0) s$mean_score[1] else NA_real_
    mean_col <- score_color(mean_sc)

    tags$div(class = "score-cards-row",
      tags$div(
        class   = paste("score-card score-card-overall",
                        if ("overview" == at) "score-card-active" else ""),
        id      = "card-overview",
        onclick = "selectCard('overview')",
        style   = sprintf("border-left-color:%s", mean_col),
        tags$div(class = "overall-label", "OVERALL"),
        tags$div(class = "overall-score",
                 style = sprintf("color:%s", mean_col),
                 if (is.na(mean_sc)) "N/A" else sprintf("%.3f", mean_sc)),
        if (nrow(s) > 0) HTML(tier_badge_html(as.character(s$tier[1])))
      ),
      make_dim_card("macrofiscal",     "MACRO-FISCAL STABILITY",    "d1_score"),
      make_dim_card("importreliance",  "STRATEGIC IMPORT RELIANCE", "d2_score"),
      make_dim_card("exportfragility", "EXPORT FRAGILITY",          "d3_score"),
      make_dim_card("network",         "NETWORK CONTAGION RISK",    "d4_score")
    )
  })


  # ---- Numerical KPI card (horizontal, col-4) --------------------------------

  output$kpi_nums <- renderUI({
    d <- cdata(); cur_yr <- as.integer(format(Sys.Date(), "%Y"))

    gdp_val <- if (nrow(d$gdp) > 0) fmt_big(d$gdp$gdp_const_usd[1]) else "N/A"
    gpc_val <- if (nrow(d$gdp) > 0)
      paste0(format(round(d$gdp$gdp_per_capita_const_usd[1]), big.mark = ","), " USD")
      else "N/A"

    debt_v <- weo_s(d$weo, "GGXWDG_NGDP") |>
      dplyr::filter(year <= cur_yr) |> pull(value)
    debt_v   <- debt_v[!is.na(debt_v)]
    debt_val <- if (length(debt_v) > 0) sprintf("%.1f%%", tail(debt_v, 1)) else "N/A"

    tags$div(class = "kpi-nums-horiz panel-card",
      tags$div(class = "kpi-num-item",
        tags$div(class = "kpi-num-value", gdp_val),
        tags$div(class = "kpi-num-label", "GDP (CONST. USD)")
      ),
      tags$div(class = "kpi-num-divider-v"),
      tags$div(class = "kpi-num-item",
        tags$div(class = "kpi-num-value", gpc_val),
        tags$div(class = "kpi-num-label", "GDP PER CAPITA")
      ),
      tags$div(class = "kpi-num-divider-v"),
      tags$div(class = "kpi-num-item",
        tags$div(class = "kpi-num-value", debt_val),
        tags$div(class = "kpi-num-label", "GROSS DEBT % GDP")
      )
    )
  })


  # ---- Chart function -------------------------------------------------------
  # e_kpi_chart: time series + optional comparison lines, cross-linked tooltips.
  # All 9 KPI charts share echarts group "kpi_d1" for synchronized crosshair.

  e_kpi_chart <- function(df, color, type = "line", height = "120px", fmt = "%.1f",
                           region_df = NULL, income_df = NULL,
                           region_lbl = "Region", income_lbl = "Income",
                           series_name = NULL) {
    if (is.null(series_name)) series_name <- cdata()$scores$country_code[1] %||% "Country"

    df <- df[!is.na(df$val) & !is.na(df$yr), ]

    if (nrow(df) < 2) {
      return(
        data.frame(x = 1, y = 1) |>
          e_charts(x, width = "100%", height = height) |>
          e_x_axis(show = FALSE) |> e_y_axis(show = FALSE) |>
          e_legend(show = FALSE) |>
          e_grid(left = 0, right = 0, top = 0, bottom = 0) |>
          e_title(text = "Data not available", left = "center", top = "middle",
                  textStyle = list(fontSize = 11, color = "#b0bbc7",
                                   fontFamily = "Work Sans", fontWeight = "normal"))
      )
    }

    # Last-value scatter label
    df$last_val <- NA_real_
    df$last_val[nrow(df)] <- df$val[nrow(df)]
    lbl <- sprintf(fmt, df$val[nrow(df)])

    has_region <- !is.null(region_df) && nrow(region_df) > 0
    has_income <- !is.null(income_df) && nrow(income_df) > 0

    # Merge comparison columns (avoids e_data dataset switch)
    df_plot <- df |> dplyr::rename(main = val, last_main = last_val)
    if (has_region) df_plot <- dplyr::full_join(
      df_plot, region_df |> dplyr::rename(region_avg = val), by = "yr")
    if (has_income) df_plot <- dplyr::full_join(
      df_plot, income_df |> dplyr::rename(income_avg = val), by = "yr")
    df_plot <- df_plot |> dplyr::arrange(yr)

    # Main series
    base <- if (type == "bar") {
      df_plot |> e_charts(yr, width = "100%", height = height) |>
        e_bar(main, name = series_name, barMaxWidth = 18,
              itemStyle = list(color = htmlwidgets::JS(sprintf(
                'function(p){return p.value[1]>=0?"%s":"#B8795E";}', color
              ))))
    } else {
      df_plot |> e_charts(yr, width = "100%", height = height) |>
        e_line(main, name = series_name, smooth = TRUE, symbol = "none",
               lineStyle = list(color = color, width = 2.5),
               areaStyle = list(color = paste0(color, "22")))
    }

    # Comparison lines
    if (has_region) base <- base |>
      e_line(region_avg, name = region_lbl, smooth = TRUE, symbol = "none",
             lineStyle = list(color = "#859C72", width = 2, type = "dashed"))
    if (has_income) base <- base |>
      e_line(income_avg, name = income_lbl, smooth = TRUE, symbol = "none",
             lineStyle = list(color = "#4A7C8C", width = 2, type = "dotted"))

    # Multi-series tooltip (skips scatter dots)
    tooltip_js <- "function(params){
      if(!params||!params[0])return'';
      var yr=params[0].name;
      var html='<div style=\"font-weight:600;font-size:11px;margin-bottom:3px\">'+yr+'</div>';
      params.forEach(function(p){
        if(p.seriesType==='scatter')return;
        var val=(!p.value||isNaN(+p.value[1]))?'N/A':(+p.value[1]).toFixed(2);
        html+='<div style=\"display:flex;align-items:center;font-size:10px;gap:2px\">'
             +'<span style=\"flex:1;color:#555\">'+p.seriesName+'</span>'
             +'<span style=\"font-weight:600;margin-left:10px\">'+val+'</span></div>';
      });
      return html;
    }"

    base |>
      e_scatter(last_main, name = "_dot", symbol = "circle", symbolSize = 8,
                itemStyle = list(color = color, borderColor = "#ffffff", borderWidth = 2),
                label = list(show = TRUE,
                  formatter = htmlwidgets::JS(sprintf("function(){return '%s';}", lbl)),
                  position = "top", fontSize = 10, fontFamily = "Oswald",
                  color = color, fontWeight = "bold")) |>
      e_x_axis(
        axisLabel = list(
          fontSize = 9, color = "#8a96a3", rotate = 0,
          interval = htmlwidgets::JS(
            "function(i,v){
              var w=document.documentElement.clientWidth;
              if(w<576)return i%4===0;
              if(w<992)return i%2===0;
              return true;
            }")),
        axisLine = list(lineStyle = list(color = "#dde4ec")),
        axisTick = list(show = FALSE), splitLine = list(show = FALSE)
      ) |>
      e_y_axis(
        axisLabel  = list(fontSize = 9, color = "#8a96a3",
                          formatter = htmlwidgets::JS("function(v){return v.toFixed(1);}")),
        splitLine  = list(lineStyle = list(color = "#eff2f6", type = "dashed")),
        axisLine   = list(show = FALSE), axisTick = list(show = FALSE), splitNumber = 3
      ) |>
      e_grid(left = 28, right = 10, top = 18, bottom = 28) |>
      e_legend(show = FALSE) |>
      e_tooltip(trigger = "axis",
                backgroundColor = "rgba(128, 128, 128, 0.10)",
                borderColor     = "rgba(0,0,0,0.10)",
                textStyle       = list(color = "#2E3338", fontSize = 10),
                axisPointer = list(type = "line",
                                   lineStyle = list(color = "#aab", width = 1.5, type = "dashed")),
                formatter = htmlwidgets::JS(tooltip_js)) |>
      htmlwidgets::onRender("function(el){
        var inst=echarts.getInstanceByDom(el);
        if(!inst)return;
        inst.group='kpi_d1';
        setTimeout(function(){ echarts.connect('kpi_d1'); }, 400);
        if(window.ResizeObserver){
          new ResizeObserver(function(){ inst.resize(); }).observe(el.parentElement||el);
        }else{ setTimeout(function(){ inst.resize(); }, 300); }
      }")
  }

  # Prep WEO series -> yr/val df for 2015:cur_yr
  prep_weo <- function(weo_df, code, cur_yr) {
    weo_s(weo_df, code) |>
      dplyr::filter(year >= 2019, year <= cur_yr) |>
      dplyr::transmute(yr = as.character(year), val = value)
  }

  # Prep WDI series -> yr/val df for 2015:cur_yr
  prep_wdi <- function(wdi_df, id, cur_yr) {
    wdi_s(wdi_df, id) |>
      dplyr::mutate(yr = as.character(as.integer(period))) |>
      dplyr::filter(!is.na(value), as.integer(yr) >= 2015, as.integer(yr) <= cur_yr) |>
      dplyr::transmute(yr, val = value)
  }


  # ---- Show Comparisons toggle & peer data ----------------------------------

  show_comparisons <- reactiveVal(FALSE)

  observeEvent(input$toggle_comparisons, {
    show_comparisons(!show_comparisons())
    lbl <- if (show_comparisons()) "Hide Comparisons" else "Show Comparisons"
    updateActionButton(session, "toggle_comparisons", label = lbl)
  })

  # Peer averages: one reactive; queries run only when toggle is on
  comp_data <- reactive({
    req(show_comparisons())
    s <- cdata()$scores
    if (nrow(s) == 0) return(NULL)

    my_iso    <- s$country_code[1]
    my_region <- s$region[1]
    my_income <- s$income_group[1]
    cur_yr    <- as.integer(format(Sys.Date(), "%Y"))

    region_peers <- latest_tiers |>
      dplyr::filter(region == my_region, country_code != my_iso) |>
      dplyr::pull(country_code)
    income_peers <- latest_tiers |>
      dplyr::filter(income_group == my_income, country_code != my_iso) |>
      dplyr::pull(country_code)

    weo_avg <- function(iso_vec) {
      if (length(iso_vec) == 0) return(data.frame())
      ids <- paste(sprintf("'%s'", iso_vec), collapse = ",")
      tryCatch(dbGetQuery(con, sprintf("
        SELECT indicator_code, year, AVG(value) AS avg_val
        FROM bronze.imf_weo_annual
        WHERE iso3c IN (%s) AND year >= 2019 AND year <= %d
        GROUP BY indicator_code, year ORDER BY indicator_code, year
      ", ids, cur_yr)), error = function(e) data.frame())
    }

    wdi_avg <- function(iso_vec) {
      if (length(iso_vec) == 0) return(data.frame())
      ids <- paste(sprintf("'%s'", iso_vec), collapse = ",")
      tryCatch(dbGetQuery(con, sprintf("
        SELECT indicator_id,
               CAST(period AS INTEGER) AS year,
               AVG(value) AS avg_val
        FROM bronze.wb_wdi_raw
        WHERE country_code IN (%s)
          AND TRY_CAST(period AS INTEGER) >= 2015
          AND TRY_CAST(period AS INTEGER) <= %d
        GROUP BY indicator_id, year ORDER BY indicator_id, year
      ", ids, cur_yr)), error = function(e) data.frame())
    }

    wdi_r <- wdi_avg(region_peers)
    wdi_i <- wdi_avg(income_peers)

    # Compute ratio peer series from already-averaged WDI data (avg(num)/avg(denom))
    ratio_s <- function(df, num_id, denom_id, mult = 1) {
      if (nrow(df) == 0) return(data.frame())
      num <- df |> dplyr::filter(indicator_id == num_id) |>
        dplyr::select(year, nv = avg_val)
      den <- df |> dplyr::filter(indicator_id == denom_id) |>
        dplyr::select(year, dv = avg_val)
      dplyr::inner_join(num, den, by = "year") |>
        dplyr::filter(!is.na(dv), dv > 0) |>
        dplyr::transmute(yr = as.character(year), val = nv / dv * mult)
    }

    net_fdi_s <- function(df) {
      if (nrow(df) == 0) return(data.frame())
      fin  <- df |> dplyr::filter(indicator_id == "BX.KLT.DINV.CD.WD") |>
        dplyr::select(year, fin = avg_val)
      fout <- df |> dplyr::filter(indicator_id == "BM.KLT.DINV.CD.WD") |>
        dplyr::select(year, fout = avg_val)
      gdp  <- df |> dplyr::filter(indicator_id == "NY.GDP.MKTP.CD") |>
        dplyr::select(year, gdp = avg_val)
      fin |> dplyr::inner_join(fout, by = "year") |>
        dplyr::inner_join(gdp,  by = "year") |>
        dplyr::filter(gdp > 0) |>
        dplyr::transmute(yr = as.character(year), val = (fin - fout) / gdp * 100)
    }

    list(
      weo_region    = weo_avg(region_peers),
      weo_income    = weo_avg(income_peers),
      wdi_region    = wdi_r,
      wdi_income    = wdi_i,
      debt_svc_r    = ratio_s(wdi_r, "DT.TDS.DECT.CD", "BX.GSR.GNFS.CD", 100),
      debt_svc_i    = ratio_s(wdi_i, "DT.TDS.DECT.CD", "BX.GSR.GNFS.CD", 100),
      ext_debt_r    = ratio_s(wdi_r, "DT.DOD.DSTC.CD", "FI.RES.TOTL.CD"),
      ext_debt_i    = ratio_s(wdi_i, "DT.DOD.DSTC.CD", "FI.RES.TOTL.CD"),
      fdi_r         = net_fdi_s(wdi_r),
      fdi_i         = net_fdi_s(wdi_i),
      region_lbl    = my_region,
      income_lbl    = my_income
    )
  })

  # Helpers: extract yr/val series for one indicator from comp_data
  get_comp_weo <- function(code) {
    if (!show_comparisons()) return(list(NULL, NULL))
    cd <- comp_data(); if (is.null(cd)) return(list(NULL, NULL))
    pull_s <- function(df) {
      if (nrow(df) == 0) return(NULL)
      s <- df |> dplyr::filter(indicator_code == code) |>
           dplyr::transmute(yr = as.character(year), val = avg_val)
      if (nrow(s) > 1) s else NULL
    }
    list(pull_s(cd$weo_region), pull_s(cd$weo_income))
  }

  get_comp_wdi <- function(id) {
    if (!show_comparisons()) return(list(NULL, NULL))
    cd <- comp_data(); if (is.null(cd)) return(list(NULL, NULL))
    pull_s <- function(df) {
      if (nrow(df) == 0) return(NULL)
      s <- df |> dplyr::filter(indicator_id == id) |>
           dplyr::transmute(yr = as.character(year), val = avg_val)
      if (nrow(s) > 1) s else NULL
    }
    list(pull_s(cd$wdi_region), pull_s(cd$wdi_income))
  }

  # Pre-computed ratio series (debt_svc, ext_debt, fdi) — keys match comp_data list names
  get_comp_ratio <- function(r_key, i_key) {
    if (!show_comparisons()) return(list(NULL, NULL))
    cd <- comp_data(); if (is.null(cd)) return(list(NULL, NULL))
    to_s <- function(df) if (!is.null(df) && nrow(df) > 1) df else NULL
    list(to_s(cd[[r_key]]), to_s(cd[[i_key]]))
  }

  # Short display labels for regions and income groups
  shorten_label <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) return(x)
    lookup <- c(
      "Sub-Saharan Africa"                              = "Sub-Saharan Africa",
      "East Asia & Pacific"                             = "E. Asia & Pac.",
      "Europe & Central Asia"                           = "Eur. & C. Asia",
      "Latin America & Caribbean"                       = "Latin America",
      "Middle East & North Africa"                      = "MENA",
      "Middle East, North Africa, Afghanistan & Pakistan" = "MENA-Afg/Pak",
      "North America"                                   = "N. America",
      "South Asia"                                      = "South Asia",
      "Upper middle income"                             = "Upper Mid. Income",
      "Lower middle income"                             = "Lower Mid. Income",
      "High income"                                     = "High Income",
      "Low income"                                      = "Low Income"
    )
    out <- lookup[x]
    if (is.na(out)) x else unname(out)
  }

  comp_lbls <- reactive({
    s <- cdata()$scores
    list(
      region = shorten_label(if (nrow(s) > 0) s$region[1] else "Regional Avg"),
      income = shorten_label(if (nrow(s) > 0) s$income_group[1] else "Income Avg")
    )
  })

  # Shared legend rendered above the 3x3 grid; hidden when comparisons are off
  output$kpi_legend <- renderUI({
    if (!show_comparisons()) return(NULL)
    lbl <- comp_lbls()
    tags$div(class = "kpi-shared-legend",
      tags$span(class = "kl-item",
        tags$span(class = "kl-line kl-country"),
        tags$span(class = "kl-text", "Country")
      ),
      tags$span(class = "kl-sep"),
      tags$span(class = "kl-item",
        tags$span(class = "kl-line kl-region"),
        tags$span(class = "kl-text", lbl$region)
      ),
      tags$span(class = "kl-sep"),
      tags$span(class = "kl-item",
        tags$span(class = "kl-line kl-income"),
        tags$span(class = "kl-text", lbl$income)
      )
    )
  })


  # ---- 9 KPI chart renders --------------------------------------------------

  output$spark_gdp_growth <- renderEcharts4r({
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    comp <- get_comp_weo("NGDP_RPCH"); lbl <- comp_lbls()
    e_kpi_chart(prep_weo(cdata()$weo, "NGDP_RPCH", cur_yr),
                color = "#5D8FA3", type = "bar",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$spark_current_acct <- renderEcharts4r({
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    comp <- get_comp_weo("BCA_NGDPD"); lbl <- comp_lbls()
    e_kpi_chart(prep_weo(cdata()$weo, "BCA_NGDPD", cur_yr),
                color = "#8B9C6B",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$spark_fiscal <- renderEcharts4r({
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    comp <- get_comp_weo("GGXCNL_NGDP"); lbl <- comp_lbls()
    e_kpi_chart(prep_weo(cdata()$weo, "GGXCNL_NGDP", cur_yr),
                color = "#C4A563",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$spark_inflation <- renderEcharts4r({
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    comp <- get_comp_weo("PCPIPCH"); lbl <- comp_lbls()
    e_kpi_chart(prep_weo(cdata()$weo, "PCPIPCH", cur_yr),
                color = "#B8795E",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$d1_chart_gross_debt <- renderEcharts4r({
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    comp <- get_comp_weo("GGXWDG_NGDP"); lbl <- comp_lbls()
    e_kpi_chart(prep_weo(cdata()$weo, "GGXWDG_NGDP", cur_yr),
                color = "#8B2635",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$d1_chart_debt_svc <- renderEcharts4r({
    d <- cdata(); cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    svc <- prep_wdi(d$wdi, "DT.TDS.DECT.CD", cur_yr)
    exp <- prep_wdi(d$wdi, "BX.GSR.GNFS.CD", cur_yr)
    df  <- dplyr::inner_join(svc, exp, by = "yr", suffix = c("_svc","_exp")) |>
      dplyr::mutate(val = ifelse(!is.na(val_exp) & val_exp > 0,
                                  val_svc / val_exp * 100, NA_real_)) |>
      dplyr::select(yr, val)
    e_kpi_chart(df, color = "#C17B5A")  # ratio: no peer comparison
  })

  output$d1_chart_ext_debt <- renderEcharts4r({
    d <- cdata(); cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    debt <- prep_wdi(d$wdi, "DT.DOD.DSTC.CD", cur_yr)
    res  <- prep_wdi(d$wdi, "FI.RES.TOTL.CD",  cur_yr)
    df   <- dplyr::inner_join(debt, res, by = "yr", suffix = c("_d","_r")) |>
      dplyr::mutate(val = ifelse(!is.na(val_r) & val_r > 0,
                                  val_d / val_r, NA_real_)) |>
      dplyr::select(yr, val)
    e_kpi_chart(df, color = "#6A7381", fmt = "%.2f")  # ratio: no peer comparison
  })

  output$d1_chart_trade <- renderEcharts4r({
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    comp <- get_comp_wdi("NE.TRD.GNFS.ZS"); lbl <- comp_lbls()
    e_kpi_chart(prep_wdi(cdata()$wdi, "NE.TRD.GNFS.ZS", cur_yr),
                color = "#5D8FA3",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$d1_chart_fdi <- renderEcharts4r({
    d <- cdata(); cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    fin  <- prep_wdi(d$wdi, "BX.KLT.DINV.CD.WD", cur_yr)
    fout <- prep_wdi(d$wdi, "BM.KLT.DINV.CD.WD", cur_yr)
    gdpn <- prep_wdi(d$wdi, "NY.GDP.MKTP.CD",     cur_yr)
    df   <- dplyr::inner_join(fin, fout, by = "yr", suffix = c("_in","_out")) |>
      dplyr::inner_join(gdpn, by = "yr") |>
      dplyr::mutate(val = ifelse(!is.na(val) & val > 0,
                                  (val_in - val_out) / val * 100, NA_real_)) |>
      dplyr::select(yr, val)
    e_kpi_chart(df, color = "#8B9C6B")  # ratio: no peer comparison
  })


  # ---- D1: Fiscal health multi-line chart -----------------------------------

  output$d1_fiscal_chart <- renderEcharts4r({
    d <- cdata(); cur_yr <- as.integer(format(Sys.Date(), "%Y"))

    ca   <- weo_s(d$weo, "BCA_NGDPD")   |> select(year, ca   = value)
    fb   <- weo_s(d$weo, "GGXCNL_NGDP") |> select(year, fb   = value)
    debt <- weo_s(d$weo, "GGXWDG_NGDP") |> select(year, debt = value)

    df <- ca |>
      full_join(fb,   by = "year") |>
      full_join(debt, by = "year") |>
      dplyr::filter(year >= 2019, year <= cur_yr + 3) |>
      mutate(yr = as.character(year)) |>
      arrange(year)

    if (nrow(df) == 0) return(NULL)

    df |>
      e_charts(yr, width = "100%", height = "260px") |>
      e_line(ca,   name = "Current Account % GDP",
             smooth = TRUE, symbol = "none",
             lineStyle = list(color = "#5D8FA3", width = 3)) |>
      e_line(fb,   name = "Fiscal Balance % GDP",
             smooth = TRUE, symbol = "none",
             lineStyle = list(color = "#C4A563", width = 3)) |>
      e_line(debt, name = "Govt Gross Debt % GDP",
             smooth = TRUE, symbol = "none",
             lineStyle = list(color = "#8B2635", width = 3)) |>
      e_x_axis(
        axisLabel = list(fontSize = 10, color = "#8a96a3"),
        axisLine  = list(lineStyle = list(color = "#dde4ec")),
        axisTick  = list(show = FALSE)
      ) |>
      e_y_axis(
        axisLabel = list(fontSize = 10, color = "#8a96a3",
                         formatter = htmlwidgets::JS(
                           "function(v){return v.toFixed(0)+'%';}")),
        splitLine = list(lineStyle = list(color = "#eff2f6", type = "dashed")),
        axisLine  = list(show = FALSE), axisTick = list(show = FALSE)
      ) |>
      e_legend(bottom = 0, left = "center",
               textStyle = list(fontSize = 10, fontFamily = "Work Sans")) |>
      e_tooltip(trigger = "axis") |>
      e_grid(left = "8%", right = "3%", top = "5%", bottom = "22%") |>
      ro_render()
  })


  # ---- D1: GDP growth / inflation / net FDI combo chart --------------------

  output$d1_gdp_combo_chart <- renderEcharts4r({
    d <- cdata(); cur_yr <- as.integer(format(Sys.Date(), "%Y"))

    gdp_g <- weo_s(d$weo, "NGDP_RPCH") |>
      dplyr::filter(year >= 2019, year <= cur_yr) |>
      select(year, gdp_g = value)

    infl <- weo_s(d$weo, "PCPIPCH") |>
      dplyr::filter(year >= 2019, year <= cur_yr) |>
      select(year, infl = value)

    # Net FDI % GDP from WDI (period is character "YYYY")
    fdi_in  <- wdi_s(d$wdi, "BX.KLT.DINV.CD.WD") |>
      mutate(yr = as.integer(period)) |>
      dplyr::filter(yr >= 2015, yr <= cur_yr) |>
      select(year = yr, fdi_in = value)

    fdi_out <- wdi_s(d$wdi, "BM.KLT.DINV.CD.WD") |>
      mutate(yr = as.integer(period)) |>
      dplyr::filter(yr >= 2015, yr <= cur_yr) |>
      select(year = yr, fdi_out = value)

    gdp_usd <- wdi_s(d$wdi, "NY.GDP.MKTP.CD") |>
      mutate(yr = as.integer(period)) |>
      dplyr::filter(yr >= 2015, yr <= cur_yr) |>
      select(year = yr, gdp_usd = value)

    fdi_pct <- fdi_in |>
      left_join(fdi_out, by = "year") |>
      left_join(gdp_usd, by = "year") |>
      mutate(net_fdi = ifelse(!is.na(gdp_usd) & gdp_usd > 0,
                               (fdi_in - fdi_out) / gdp_usd * 100, NA_real_)) |>
      select(year, net_fdi)

    df <- gdp_g |>
      full_join(infl,    by = "year") |>
      full_join(fdi_pct, by = "year") |>
      dplyr::filter(!is.na(year)) |>
      arrange(year) |>
      mutate(yr = as.character(year))

    if (nrow(df) == 0) return(NULL)

    df |>
      e_charts(yr, width = "100%", height = "220px") |>
      e_bar(gdp_g, name = "GDP Growth %", barMaxWidth = 26,
            itemStyle = list(color = htmlwidgets::JS(
              'function(p){return p.value[1]>=0?"#5D8FA3":"#B8795E";}'))) |>
      e_line(infl,    name = "Inflation %",
             smooth = TRUE, symbol = "none",
             lineStyle = list(color = "#C4A563", width = 2.5)) |>
      e_line(net_fdi, name = "Net FDI % GDP",
             smooth = TRUE, symbol = "none",
             lineStyle = list(color = "#8B9C6B", width = 2.5)) |>
      e_x_axis(
        axisLabel = list(fontSize = 10, color = "#8a96a3"),
        axisLine  = list(lineStyle = list(color = "#dde4ec")),
        axisTick  = list(show = FALSE)
      ) |>
      e_y_axis(
        axisLabel = list(fontSize = 10, color = "#8a96a3",
                         formatter = htmlwidgets::JS(
                           "function(v){return v.toFixed(1)+'%';}")),
        splitLine = list(lineStyle = list(color = "#eff2f6", type = "dashed")),
        axisLine  = list(show = FALSE), axisTick = list(show = FALSE)
      ) |>
      e_legend(bottom = 0, left = "center",
               textStyle = list(fontSize = 10, fontFamily = "Work Sans")) |>
      e_tooltip(trigger = "axis") |>
      e_grid(left = "8%", right = "3%", top = "5%", bottom = "20%") |>
      ro_render()
  })


  # ---- D1: Assessment — from gold.narratives dimension='D1' ----------------

  output$d1_assessment <- renderUI({
    iso <- input$country_select

    narr <- tryCatch(
      dbGetQuery(con, "
        SELECT narrative FROM gold.narratives
        WHERE country_code = ? AND dimension = 'D1'
        ORDER BY generated_at DESC
        LIMIT 1
      ", params = list(iso)),
      error = function(e) data.frame(narrative = character(0))
    )

    if (nrow(narr) == 0 || is.na(narr$narrative[1])) {
      tags$p(class = "narrative-placeholder",
             "Assessment not yet generated for this country.")
    } else {
      paras <- strsplit(narr$narrative[1], "\n\n")[[1]]
      paras <- paras[nchar(trimws(paras)) > 0]
      tags$div(class = "narrative-text",
        lapply(paras, function(p) tags$p(p))
      )
    }
  })


  # ---- Row 4: Fiscal KPI charts -------------------------------------------

  output$d1_chart_gross_debt <- renderEcharts4r({
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    comp <- get_comp_weo("GGXWDG_NGDP"); lbl <- comp_lbls()
    e_kpi_chart(prep_weo(cdata()$weo, "GGXWDG_NGDP", cur_yr), color = "#C4A563",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$d1_chart_debt_svc <- renderEcharts4r({
    d <- cdata(); cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    svc <- prep_wdi(d$wdi, "DT.TDS.DECT.CD", cur_yr)
    exp <- prep_wdi(d$wdi, "BX.GSR.GNFS.CD", cur_yr)
    df  <- dplyr::inner_join(svc, exp, by = "yr", suffix = c("_svc", "_exp")) |>
      dplyr::mutate(val = ifelse(!is.na(val_exp) & val_exp > 0,
                                  val_svc / val_exp * 100, NA_real_)) |>
      dplyr::select(yr, val)
    comp <- get_comp_ratio("debt_svc_r", "debt_svc_i"); lbl <- comp_lbls()
    e_kpi_chart(df, color = "#C17B5A",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$d1_chart_ext_debt <- renderEcharts4r({
    d <- cdata(); cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    debt <- prep_wdi(d$wdi, "DT.DOD.DSTC.CD", cur_yr)
    res  <- prep_wdi(d$wdi, "FI.RES.TOTL.CD",  cur_yr)
    df   <- dplyr::inner_join(debt, res, by = "yr", suffix = c("_d", "_r")) |>
      dplyr::mutate(val = ifelse(!is.na(val_r) & val_r > 0,
                                  val_d / val_r, NA_real_)) |>
      dplyr::select(yr, val)
    comp <- get_comp_ratio("ext_debt_r", "ext_debt_i"); lbl <- comp_lbls()
    e_kpi_chart(df, color = "#6A7381", fmt = "%.2f",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })


  # ---- Row 5: Trade & external charts ------------------------------------

  output$d1_chart_trade <- renderEcharts4r({
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    comp <- get_comp_wdi("NE.TRD.GNFS.ZS"); lbl <- comp_lbls()
    e_kpi_chart(prep_wdi(cdata()$wdi, "NE.TRD.GNFS.ZS", cur_yr), color = "#5D8FA3",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$d1_chart_fdi <- renderEcharts4r({
    d <- cdata(); cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    fin  <- prep_wdi(d$wdi, "BX.KLT.DINV.CD.WD", cur_yr)
    fout <- prep_wdi(d$wdi, "BM.KLT.DINV.CD.WD", cur_yr)
    gdpn <- prep_wdi(d$wdi, "NY.GDP.MKTP.CD",     cur_yr)
    df   <- dplyr::inner_join(fin, fout, by = "yr", suffix = c("_in", "_out")) |>
      dplyr::inner_join(gdpn, by = "yr") |>
      dplyr::mutate(val = ifelse(!is.na(val) & val > 0,
                                  (val_in - val_out) / val * 100, NA_real_)) |>
      dplyr::select(yr, val)
    comp <- get_comp_ratio("fdi_r", "fdi_i"); lbl <- comp_lbls()
    e_kpi_chart(df, color = "#8B9C6B",
                region_df = comp[[1]], income_df = comp[[2]],
                region_lbl = lbl$region, income_lbl = lbl$income)
  })

  output$d1_chart_reer <- renderEcharts4r({
    cur_yr <- as.integer(format(Sys.Date(), "%Y"))
    e_kpi_chart(prep_wdi(cdata()$wdi, "PX.REX.REER", cur_yr), color = "#4A7C8C")
  })


  # ---- Shared indicator detail panel ---------------------------------------

  output$indicator_info <- renderUI({
    key <- input$selected_kpi
    if (is.null(key) || nchar(key) == 0) key <- "gdp_growth"  # default on load
    desc <- kpi_descriptions[[key]]
    if (is.null(desc)) return(NULL)
    tags$div(
      tags$p(class = "indicator-desc-title", desc$title),
      tags$p(class = "indicator-desc",       desc$text)
    )
  })


  # ================================================================
  # D2 - STRATEGIC IMPORT RELIANCE
  # ================================================================

  # Full-size empty state — also used by other dimensions
  empty_chart <- function(msg = "Data not available", height = "220px") {
    data.frame(x = 1, y = 1) |>
      e_charts(x, width = "100%", height = height) |>
      e_scatter(y, symbol_size = 0) |>   # invisible — echarts requires ≥1 series
      e_x_axis(show = FALSE) |> e_y_axis(show = FALSE) |>
      e_legend(show = FALSE) |>
      e_grid(left = 0, right = 0, top = 0, bottom = 0) |>
      e_title(text = msg, left = "center", top = "middle",
              textStyle = list(fontSize = 12, color = "#8a96a3",
                               fontFamily = "Work Sans", fontWeight = "normal")) |>
      ro_render()
  }

  # Debug: show column names, first row, and distinct indicators for gold tables
  output$d2_debug_text <- renderText({
    iso <- cdata()$scores$country_code[1] %||% "USA"
    lines <- character(0)
    for (tbl in c("bronze.comtrade_top5_commodities",
                  "bronze.comtrade_food_net_trade",
                  "gold.d2_annual_indicator_ts",
                  "gold.d2_monthly_indicator_ts")) {
      info <- tryCatch({
        r <- dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 1", tbl))
        inds <- if (grepl("indicator_ts", tbl)) {
          tryCatch({
            iv <- dbGetQuery(con, sprintf(
              "SELECT DISTINCT indicator FROM %s WHERE country_code = '%s'", tbl, iso))
            paste("indicators:", paste(iv$indicator, collapse=", "))
          }, error = function(e) "")
        } else ""
        paste0("cols: ", paste(names(r), collapse=", "), "\n  ", inds)
      }, error = function(e) paste("NOT FOUND:", conditionMessage(e)))
      lines <- c(lines, paste0(tbl, ":\n  ", info))
    }
    paste(lines, collapse="\n\n")
  })

  # D2 chart descriptions — shown in the reactive "About this chart" panel
  d2_chart_desc <- list(
    top5 = list(
      title = "Top 5 Imports",
      text  = paste0(
        "This table shows the five commodities that account for the largest share of import ",
        "value in the most recent year available. Think of it as a quick x-ray of where a ",
        "country's supply chain dependencies are most concentrated. When a country's top imports ",
        "are dominated by commodities sourced from a single supplier country — think one nation ",
        "supplying most of a country's oil, or one region providing most of its staple grain — ",
        "any disruption to that source lands hard and fast on domestic markets. This table is ",
        "best read alongside the supplier diversity chart: high import value in a category ",
        "combined with low diversity is where the real risk sits."
      )
    ),
    diversity = list(
      title = "Supplier Diversity",
      text  = paste0(
        "Each line tracks how spread out a country's import sources are for a commodity group — ",
        "food, energy, cereals, and fertilizers. The scale runs from 0 to 1: near 1 means imports ",
        "are spread across many different supplier countries (resilient), while near 0 means almost ",
        "everything comes from a single source (fragile). The measure is 1 minus the ",
        "Herfindahl-Hirschman Index (HHI), which is the standard measure of market concentration. ",
        "Lines drifting downward mean a country's sourcing is narrowing — each step down amplifies ",
        "the damage if that dominant supplier has a bad harvest, imposes export restrictions, or ",
        "faces political disruption. By default, Food and Energy are shown. Click any label in the ",
        "legend to add or remove categories from the view."
      )
    ),
    food_net = list(
      title = "Food Net Trade Position",
      text  = paste0(
        "This chart shows whether a country is a net earner or net spender on each food ",
        "subcategory — the difference between export value and import value. Green bars point ",
        "right, meaning the country sells more of that food type than it buys from abroad. Red ",
        "bars point left, meaning the opposite: the country depends on global markets to meet ",
        "domestic consumption needs for that food type. The further a red bar extends, the more ",
        "structurally dependent the country is on imported food in that category. This matters ",
        "because structurally food-importing countries are directly exposed to global commodity ",
        "price shocks and supply disruptions — there's no domestic buffer to fall back on."
      )
    ),
    cv_map = list(
      title = "Price Volatility — Global View",
      text  = paste0(
        "This map shows the latest observed food import price volatility — measured by the ",
        "Coefficient of Variation (CV) — for every country in the dataset. The color scale ",
        "runs from green (low CV, stable and predictable import prices) through amber to red ",
        "(high CV, highly erratic prices that make supply-chain planning difficult). ",
        "The global context matters: when neighboring countries or major trading partners are ",
        "also showing high volatility, it usually signals a regional supply disruption rather ",
        "than a country-specific issue — which is broader in scope and harder to hedge individually."
      )
    ),
    monthly = list(
      title = "Price Volatility & Anomaly Signals",
      text  = paste0(
        "This chart combines two lenses on import stress. The lines (left axis) show the ",
        "Basket Coefficient of Variation (CV) — how erratic a commodity group's import price ",
        "basket has been over the past rolling year. Below 0.15 is calm; above 0.30 means ",
        "price swings are large enough to make supply-chain budgeting unreliable. ",
        "The bars (right axis) are anomaly z-scores — they show whether this month's import ",
        "price or volume is unusually high or low versus the same month in past years. ",
        "Above +1.5 is a notable spike; below -1.5 is a significant shortfall. ",
        "Food, energy, and fertilizer are all available for both measures. ",
        "By default, Food Basket CV and Food Import Volume are shown. ",
        "Click any series label in the legend to add or remove other categories — try adding ",
        "Energy Basket CV to compare chronic instability across commodity groups."
      )
    )
  )

  output$d2_chart_info <- renderUI({
    key <- input$selected_d2_chart
    if (is.null(key) || nchar(key) == 0) key <- "diversity"
    desc <- d2_chart_desc[[key]]
    if (is.null(desc)) return(NULL)
    tags$div(
      tags$p(class = "indicator-desc-title", desc$title),
      tags$p(class = "indicator-desc",       desc$text)
    )
  })

  # D2 narrative
  output$d2_assessment <- renderUI({
    iso <- cdata()$scores$country_code[1]
    if (is.null(iso) || is.na(iso)) return(tags$p(class = "narrative-placeholder", "Select a country."))
    narr <- tryCatch(
      dbGetQuery(con, sprintf(
        "SELECT narrative FROM gold.narratives
         WHERE country_code = '%s' AND dimension = 'D2'
         ORDER BY generated_at DESC LIMIT 1", iso))$narrative[1],
      error = function(e) NA_character_)
    if (is.na(narr) || nchar(trimws(narr %||% "")) == 0)
      return(tags$p(class = "narrative-placeholder",
                    "No assessment available for this country."))
    tags$div(class = "narrative-text", HTML(narr))
  })


  # D2 supplier diversity — pivot gold.d2_annual_indicator_ts (long format)
  output$d2_diversity_chart <- renderEcharts4r({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(empty_chart())

      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "SELECT year,
                  AVG(CASE WHEN indicator = 'food'       THEN fit_mean END) AS diversity_food,
                  AVG(CASE WHEN indicator = 'energy'     THEN fit_mean END) AS diversity_energy,
                  AVG(CASE WHEN indicator = 'cereal'     THEN fit_mean END) AS diversity_cereal,
                  AVG(CASE WHEN indicator = 'fertilizer' THEN fit_mean END) AS diversity_fertilizer
           FROM gold.d2_annual_indicator_ts
           WHERE country_code = '%s' AND year >= 2019
           GROUP BY year ORDER BY year", iso)),
        error = function(e) data.frame())

      if (is.null(df) || nrow(df) < 2 ||
          all(is.na(df$diversity_food) & is.na(df$diversity_energy)))
        return(empty_chart("Diversity data not available"))

      df <- df |> dplyr::mutate(yr = as.character(year))

      chart <- df |> e_charts(yr, width = "100%", height = "220px")
      if (any(!is.na(df$diversity_food)))
        chart <- chart |> e_line(diversity_food, name = "Food",
                 smooth=TRUE, symbol="none",
                 lineStyle=list(color="#5D8FA3", width=2.5),
                 areaStyle=list(color="#5D8FA332"))
      if (any(!is.na(df$diversity_energy)))
        chart <- chart |> e_line(diversity_energy, name = "Energy",
                 smooth=TRUE, symbol="none",
                 lineStyle=list(color="#C4A563", width=2.5))
      if (any(!is.na(df$diversity_cereal)))
        chart <- chart |> e_line(diversity_cereal, name = "Cereals",
                 smooth=TRUE, symbol="none",
                 lineStyle=list(color="#859C72", width=2.5))
      if (any(!is.na(df$diversity_fertilizer)))
        chart <- chart |> e_line(diversity_fertilizer, name = "Fertilizer",
                 smooth=TRUE, symbol="none",
                 lineStyle=list(color="#B8795E", width=2.5))

      chart |>
        e_y_axis(splitNumber = 4,
                 name = "Diversity score",
                 nameTextStyle = list(fontSize = 8, color = "#8a96a3"),
                 axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 splitLine = list(lineStyle = list(color = "#eff2f6", type = "dashed")),
                 axisLine  = list(show = FALSE), axisTick = list(show = FALSE)) |>
        e_x_axis(axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 axisLine  = list(lineStyle = list(color = "#dde4ec")),
                 axisTick  = list(show = FALSE), splitLine = list(show = FALSE)) |>
        e_legend(top = 4, left = "center", itemWidth = 12, itemHeight = 6, itemGap = 10,
                 textStyle = list(fontSize = 9, fontFamily = "Work Sans"),
                 selected  = list("Food"=TRUE, "Energy"=TRUE,
                                  "Cereals"=FALSE, "Fertilizer"=FALSE)) |>
        e_grid(left = 44, right = 10, top = 32, bottom = 28) |>
        e_tooltip(trigger = "axis",
                  backgroundColor = "rgba(128,128,128,0.10)",
                  borderColor     = "rgba(0,0,0,0.10)",
                  textStyle       = list(color = "#2E3338", fontSize = 10)) |>
        ro_render()
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(empty_chart())
      empty_chart(paste("Error:", conditionMessage(e)))
    })
  })

  # D2 top-5 imports — exact column names from diagnostic
  output$d2_top5_imports <- renderDT({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso))
        return(datatable(data.frame(Note="Select a country"),
                         options=list(dom='t'), rownames=FALSE))
      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "WITH latest AS (
             SELECT MAX(yr) AS max_yr
             FROM bronze.comtrade_top5_commodities
             WHERE iso3c = '%s' AND flow_desc = 'Import'
           )
           SELECT rank,
                  commodity_name,
                  category,
                  ROUND(trade_value_usd / 1e6, 1) AS value_m
           FROM bronze.comtrade_top5_commodities, latest
           WHERE iso3c = '%s' AND flow_desc = 'Import' AND yr = max_yr
           ORDER BY rank", iso, iso)),
        error = function(e) data.frame())
      if (is.null(df) || nrow(df) == 0)
        return(datatable(data.frame(Note="No top-5 import data found"),
                         options=list(dom="t"), rownames=FALSE))
      names(df) <- c("Rank", "Commodity", "Category", "USD M")
      datatable(df, options=list(dom="t", ordering=FALSE, pageLength=5),
                selection = "none",
                rownames=FALSE, class="compact stripe") |>
        formatStyle("USD M", fontWeight = "bold")
    }, error = function(e)
      datatable(data.frame(Note=paste("Error:", conditionMessage(e))),
                options=list(dom="t"), rownames=FALSE))
  })

  # D2 food net trade — exact column names: food_subcategory, net_trade_usd
  output$d2_food_net_trade <- renderEcharts4r({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(empty_chart())
      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "WITH latest AS (
             SELECT MAX(yr) AS max_yr
             FROM bronze.comtrade_food_net_trade WHERE iso3c = '%s'
           )
           SELECT food_subcategory AS subcategory,
                  ROUND(net_trade_usd / 1e6, 2) AS net_m
           FROM bronze.comtrade_food_net_trade, latest
           WHERE iso3c = '%s' AND yr = max_yr
           ORDER BY net_trade_usd", iso, iso)),
        error = function(e) data.frame())
      if (is.null(df) || nrow(df) == 0)
        return(empty_chart("Food net trade data not found"))

      # Pre-assign surplus/deficit in R — avoids unreliable JS p.value extraction
      df <- df |>
        dplyr::mutate(
          surplus = ifelse(net_m >= 0, net_m, 0),
          deficit = ifelse(net_m < 0,  net_m, 0)
        )

      df |>
        e_charts(subcategory, width = "100%", height = "220px") |>
        e_bar(surplus, name = "Net Exporter", stack = "net", barMaxWidth = 26,
              itemStyle = list(color = "#859C72")) |>
        e_bar(deficit, name = "Net Importer", stack = "net", barMaxWidth = 26,
              itemStyle = list(color = "#B8795E")) |>
        e_flip_coords() |>
        e_x_axis(axisLabel = list(fontSize = 9, color = "#8a96a3",
                                   formatter = htmlwidgets::JS(
                                     "function(v){ return (Math.abs(v)>=1000) ? (v/1000).toFixed(0)+'B' : v+'M'; }")),
                 splitLine = list(lineStyle = list(color = "#eff2f6", type = "dashed")),
                 axisLine  = list(show = FALSE), axisTick = list(show = FALSE)) |>
        e_y_axis(axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 axisLine  = list(show = FALSE), axisTick = list(show = FALSE),
                 splitLine = list(show = FALSE)) |>
        e_legend(show = FALSE) |>
        e_grid(left = 88, right = 16, top = 8, bottom = 28) |>
        e_tooltip(trigger = "axis",
                  backgroundColor = "rgba(128,128,128,0.10)",
                  borderColor     = "rgba(0,0,0,0.10)",
                  textStyle       = list(color = "#2E3338", fontSize = 10),
                  formatter = htmlwidgets::JS("function(params){
                    if (!params || !params[0]) return '';
                    var cat = params[0].name;
                    var val = 0;
                    params.forEach(function(p){ val += (+p.value || 0); });
                    var absV = Math.abs(val).toFixed(1);
                    var dir = val >= 0 ? 'net exporter' : 'net importer';
                    return '<strong>'+cat+'</strong><br/>'+dir+': $'+absV+'M';
                  }")) |>
        ro_render()
    }, error = function(e) empty_chart(paste("Error:", conditionMessage(e))))
  })



  # D2 monthly signals — 2023+, bolds selected metric from d2_map_metric input
  output$d2_monthly_signals <- renderEcharts4r({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(empty_chart())
      sel <- input$d2_map_metric %||% "food_cv"

      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "SELECT year, month,
            AVG(CASE WHEN indicator IN ('food_cv','food_basket_vol','food_cv_basket_wtd')
                THEN fit_mean END) AS food_cv,
            AVG(CASE WHEN indicator IN ('energy_cv','energy_basket_vol','energy_cv_basket_wtd')
                THEN fit_mean END) AS energy_cv,
            AVG(CASE WHEN indicator IN ('fertilizer_cv','fert_cv','food_input_cv',
                                        'food_input_basket_vol','fertilizer_cv_basket_wtd')
                THEN fit_mean END) AS fert_cv,
            AVG(CASE WHEN indicator IN ('food_vol','food_vol_risk')       THEN fit_mean END) AS food_vol,
            AVG(CASE WHEN indicator IN ('food_price','food_price_risk')   THEN fit_mean END) AS food_price,
            AVG(CASE WHEN indicator IN ('energy_vol','energy_vol_risk')   THEN fit_mean END) AS energy_vol,
            AVG(CASE WHEN indicator IN ('energy_price','energy_price_risk') THEN fit_mean END) AS energy_price
           FROM gold.d2_monthly_indicator_ts
           WHERE country_code = '%s' AND year >= 2023
           GROUP BY year, month ORDER BY year, month", iso)),
        error = function(e) data.frame())

      if (is.null(df) || nrow(df) < 2)
        return(empty_chart("Monthly signal data not available"))

      df <- df |> dplyr::mutate(ym = sprintf("%d-%02d", year, month))
      has <- function(col) col %in% names(df) && any(!is.na(df[[col]]))

      # Styling: selected metric is bold+opaque; others are thin+faded
      lw  <- function(k) if (k == sel) 3.5 else 2.0
      lc  <- function(k, hex) if (k == sel) hex else paste0(substr(hex,1,7), "99")
      bc  <- function(k, hex) if (k == sel) paste0(hex, "EE") else paste0(hex, "88")

      # Legend: always show food_cv + food_vol defaults, plus whatever is selected
      defaults <- c("food_cv", "food_vol")
      vis <- function(k) k == sel || k %in% defaults

      base <- df |> e_charts(ym, width = "100%", height = "250px")

      if (has("food_cv"))
        base <- base |> e_line(food_cv,    name = "Food Basket CV",       y_index = 0,
          smooth = TRUE, symbol = "none",
          lineStyle = list(color = lc("food_cv","#5D8FA3"),    width = lw("food_cv")))
      if (has("energy_cv"))
        base <- base |> e_line(energy_cv,  name = "Energy Basket CV",     y_index = 0,
          smooth = TRUE, symbol = "none",
          lineStyle = list(color = lc("energy_cv","#C4A563"),  width = lw("energy_cv")))
      if (has("fert_cv"))
        base <- base |> e_line(fert_cv,    name = "Fertilizer Basket CV", y_index = 0,
          smooth = TRUE, symbol = "none",
          lineStyle = list(color = lc("fert_cv","#859C72"),    width = lw("fert_cv")))
      if (has("food_vol"))
        base <- base |> e_bar(food_vol,    name = "Food Import Volume",   y_index = 1,
          barMaxWidth = 9, itemStyle = list(color = bc("food_vol","#5D8FA3")))
      if (has("food_price"))
        base <- base |> e_bar(food_price,  name = "Food Import Price",    y_index = 1,
          barMaxWidth = 9, itemStyle = list(color = bc("food_price","#B8795E")))
      if (has("energy_vol"))
        base <- base |> e_bar(energy_vol,  name = "Energy Import Volume", y_index = 1,
          barMaxWidth = 9, itemStyle = list(color = bc("energy_vol","#C4A563")))
      if (has("energy_price"))
        base <- base |> e_bar(energy_price, name = "Energy Import Price", y_index = 1,
          barMaxWidth = 9, itemStyle = list(color = bc("energy_price","#956455")))

      base |>
        e_y_axis(name = "CV", splitNumber = 3,
                 nameTextStyle = list(fontSize = 9, color = "#8a96a3"),
                 axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 splitLine = list(lineStyle = list(color = "#eff2f6", type = "dashed")),
                 axisLine  = list(show = FALSE), axisTick = list(show = FALSE)) |>
        e_y_axis(index = 1, name = "Anomaly (Z)", splitNumber = 4,
                 nameTextStyle = list(fontSize = 9, color = "#8a96a3"),
                 axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 splitLine = list(show = FALSE),
                 axisLine  = list(show = FALSE), axisTick = list(show = FALSE)) |>
        e_x_axis(axisLabel = list(fontSize = 8, color = "#8a96a3",
                   interval = htmlwidgets::JS(
                     "function(i){ return i%3===0; }")),
                 axisLine = list(lineStyle = list(color = "#dde4ec")),
                 axisTick = list(show = FALSE), splitLine = list(show = FALSE)) |>
        e_legend(top = 4, left = "center", itemWidth = 12, itemHeight = 6, itemGap = 8,
                 textStyle = list(fontSize = 9, fontFamily = "Work Sans"),
                 selected = setNames(
                   as.list(sapply(c("food_cv","energy_cv","fert_cv",
                                    "food_vol","food_price","energy_vol","energy_price"),
                                  vis)),
                   c("Food Basket CV","Energy Basket CV","Fertilizer Basket CV",
                     "Food Import Volume","Food Import Price",
                     "Energy Import Volume","Energy Import Price"))) |>
        e_grid(left = 40, right = 55, top = 36, bottom = 28) |>
        e_tooltip(trigger = "axis",
                  backgroundColor = "rgba(128,128,128,0.10)",
                  borderColor = "rgba(0,0,0,0.10)",
                  textStyle = list(color = "#2E3338", fontSize = 10)) |>
        ro_render()
    }, error = function(e) empty_chart(paste("Error:", conditionMessage(e))))
  })

  # D2 global choropleth — latest value of selected metric across all countries
  output$d2_cv_map <- renderEcharts4r({
    tryCatch({
      sel <- input$d2_map_metric %||% "food_cv"

      ind_sql <- list(
        food_cv    = "'food_cv','food_basket_vol','food_cv_basket_wtd'",
        energy_cv  = "'energy_cv','energy_basket_vol','energy_cv_basket_wtd'",
        fert_cv    = "'fertilizer_cv','fert_cv','food_input_cv','fertilizer_cv_basket_wtd'",
        food_vol   = "'food_vol','food_vol_risk'",
        food_price = "'food_price','food_price_risk'",
        energy_vol = "'energy_vol','energy_vol_risk'"
      )
      inds <- ind_sql[[sel]]

      # Latest single time point (global max year*100+month for this indicator)
      df_map <- tryCatch(
        dbGetQuery(con, sprintf(
          "WITH mx AS (
             SELECT MAX(year * 100 + month) AS ym
             FROM gold.d2_monthly_indicator_ts
             WHERE indicator IN (%s)
           )
           SELECT m.country_code, AVG(m.fit_mean) AS value
           FROM gold.d2_monthly_indicator_ts m, mx
           WHERE m.year * 100 + m.month = mx.ym
             AND m.indicator IN (%s)
           GROUP BY m.country_code", inds, inds)),
        error = function(e) data.frame())

      if (is.null(df_map) || nrow(df_map) == 0)
        return(empty_chart("Map data not available"))

      df_map <- df_map |>
        dplyr::mutate(echarts_name = countrycode::countrycode(
          country_code, "iso3c", "country.name.en", warn = FALSE)) |>
        dplyr::filter(!is.na(echarts_name), !is.na(value))

      if (nrow(df_map) == 0) return(empty_chart("Country name mapping failed"))

      abs_max <- max(abs(df_map$value), na.rm = TRUE)
      abs_max <- if (is.finite(abs_max) && abs_max > 0) abs_max else 2

      df_map |>
        e_charts(echarts_name, width = "100%", height = "250px") |>
        e_map(value, map = "world",
              roam = FALSE, aspectScale = 0.75,
              itemStyle = list(
                borderColor = "#c5cdd6", borderWidth = 0.3,
                areaColor   = "#e8edf2"),
              emphasis = list(
                itemStyle = list(areaColor = "#f0ad4e"))) |>
        e_visual_map(value,
                     min = -abs_max, max = abs_max,
                     type = "continuous", calculable = FALSE,
                     inRange = list(color = c("#2B6CB0","#f7f9fc","#C05621")),
                     outOfRange = list(color = "#d3dce6"),
                     text = c("High", "Low"),
                     orient = "vertical", right = 2, bottom = 20,
                     itemHeight = 55, itemWidth = 7,
                     textStyle = list(fontSize = 8, color = "#8a96a3")) |>
        e_tooltip(trigger = "item",
                  formatter = htmlwidgets::JS(
                    "function(p){
                       if(p.value===undefined||isNaN(p.value))
                         return '<b>'+p.name+'</b><br/>No data';
                       return '<b>'+p.name+'</b><br/>Score: '+p.value.toFixed(2);
                     }"),
                  backgroundColor = "rgba(128,128,128,0.10)",
                  borderColor = "rgba(0,0,0,0.10)",
                  textStyle = list(color = "#2E3338", fontSize = 10)) |>
        ro_render()
    }, error = function(e) empty_chart(paste("Map error:", conditionMessage(e))))
  })


  # ============================================================

  # ============================================================

  # ============================================================
  # D3  EXPORT FRAGILITY
  # ============================================================

  d3_chart_desc <- list(
    concentration = list(
      title = "Are exports going to many buyers and many products?",
      text  = paste0(
        "The two lines answer two questions: does this country sell to many different buyers, ",
        "and does it export many different products? Both run from 0 (very concentrated) to 1 ",
        "(highly diversified). When either line is low the country depends on a narrow base — ",
        "if that buyer stops buying or that product loses value there is little fallback."
      )
    ),
    basket_cv = list(
      title = "How stable have export prices been recently?",
      text  = paste0(
        "This measures how much export prices have been jumping around over the past year. ",
        "Zero means typical volatility for this country; negative means calmer than usual; ",
        "positive means more erratic. High instability makes government budget planning harder."
      )
    ),
    export_vol = list(
      title = "Are exports running above or below normal right now?",
      text  = paste0(
        "Whether this country is currently shipping more or less than seasonal patterns ",
        "would predict. Zero is normal. A drop below zero means exports have fallen short — ",
        "possibly due to disruption, blocked routes, or weak demand from key buyers."
      )
    ),
    export_price = list(
      title = "Are exporters getting paid more or less than usual?",
      text  = paste0(
        "Whether this country is receiving higher or lower prices for its exports compared ",
        "to the same time in past years. Zero is normal. Below zero means weaker prices ",
        "than historical patterns suggest — for commodity exporters this squeezes revenues."
      )
    ),
    top5 = list(
      title = "What does this country actually export?",
      text  = paste0(
        "The five products earning the most export income in the latest available year. ",
        "When one or two products dominate — especially raw commodities — the whole economy ",
        "is exposed to price swings for those goods."
      )
    ),
    reer = list(
      title = "Is the local currency making exports more expensive for buyers?",
      text  = paste0(
        "The currency value vs trading partners adjusted for inflation. Above 100 means the ",
        "local currency has strengthened — foreign buyers pay more for the same goods, which ",
        "can gradually erode export competitiveness. Source: World Bank."
      )
    ),
    minerals = list(
      title = "Does this country control a critical global resource?",
      text  = paste0(
        "Appears when a country produces more than 5% of global supply of a critical mineral. ",
        "Industries like batteries and electronics have few substitutes for these materials, ",
        "giving the producer real leverage in trade negotiations. Source: USGS MCS."
      )
    )
  )

  output$d3_chart_info <- renderUI({
    key <- input$selected_d3_chart
    if (is.null(key) || nchar(key) == 0) key <- "concentration"
    desc <- d3_chart_desc[[key]]
    if (is.null(desc))
      return(tags$p(class = "narrative-placeholder", "Click a chart above to learn more."))
    tags$div(
      tags$p(class = "indicator-desc-title", desc$title),
      tags$p(class = "indicator-desc",       desc$text)
    )
  })

  output$d3_assessment <- renderUI({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso))
        return(tags$p(class = "narrative-placeholder", "Select a country above."))
      narr <- tryCatch(
        dbGetQuery(con, sprintf(
          "SELECT narrative FROM gold.narratives
           WHERE country_code = '%s' AND dimension = 'D3'
           ORDER BY year DESC LIMIT 1", iso)),
        error = function(e) data.frame(narrative = character(0)))
      if (nrow(narr) == 0 || is.na(narr$narrative[1]))
        return(tags$p(class = "narrative-placeholder",
          "Export fragility narrative not yet available for this country."))
      tags$p(class = "narrative-text", narr$narrative[1])
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(NULL)
      tags$p(class = "narrative-placeholder", paste("Error:", conditionMessage(e)))
    })
  })

  # Exact D2 pattern: direct cdata() call + outer tryCatch with silent.error handler
  output$d3_concentration <- renderEcharts4r({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(empty_chart())

      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "SELECT year,
                  AVG(CASE WHEN indicator = 'partner'      THEN fit_mean END) AS div_partner,
                  AVG(CASE WHEN indicator = 'commodity'    THEN fit_mean END) AS div_commodity,
                  AVG(CASE WHEN indicator = 'independence' THEN fit_mean END) AS div_indep
           FROM gold.d3_annual_indicator_ts
           WHERE country_code = '%s'
           GROUP BY year ORDER BY year", iso)),
        error = function(e) data.frame())

      if (is.null(df) || nrow(df) < 2 || all(is.na(df$div_partner)))
        return(empty_chart("Concentration data not available"))

      df <- df |> dplyr::mutate(yr = as.character(year))

      chart <- df |> e_charts(yr, width = "100%", height = "255px")
      if (any(!is.na(df$div_partner)))
        chart <- chart |> e_line(div_partner, name = "Partner Diversity",
          smooth = TRUE, symbol = "none",
          lineStyle = list(color = "#5D8FA3", width = 2.5),
          areaStyle = list(color = "#5D8FA332"))
      if (any(!is.na(df$div_commodity)))
        chart <- chart |> e_line(div_commodity, name = "Commodity Diversity",
          smooth = TRUE, symbol = "none",
          lineStyle = list(color = "#859C72", width = 2.5))
      if (any(!is.na(df$div_indep)))
        chart <- chart |> e_line(div_indep, name = "Export Independence",
          smooth = TRUE, symbol = "none",
          lineStyle = list(color = "#C4A563", width = 2.5))

      chart |>
        e_y_axis(splitNumber = 4, name = "Diversity score",
                 nameTextStyle = list(fontSize = 8, color = "#8a96a3"),
                 axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 splitLine = list(lineStyle = list(color = "#eff2f6", type = "dashed")),
                 axisLine = list(show = FALSE), axisTick = list(show = FALSE)) |>
        e_x_axis(axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 axisLine = list(lineStyle = list(color = "#dde4ec")),
                 axisTick = list(show = FALSE), splitLine = list(show = FALSE)) |>
        e_legend(top = 4, left = "center", itemWidth = 12, itemHeight = 6, itemGap = 10,
                 textStyle = list(fontSize = 9, fontFamily = "Work Sans")) |>
        e_grid(left = 44, right = 10, top = 32, bottom = 28) |>
        e_tooltip(trigger = "axis", textStyle = list(color = "#2E3338", fontSize = 10)) |>
        ro_render()
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(empty_chart())
      empty_chart(paste("Error:", conditionMessage(e)))
    })
  })

  output$d3_top5_exports <- renderDT({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(datatable(data.frame(Note = "No country")))

      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "SELECT rank, commodity_name, category,
                  ROUND(trade_value_usd / 1e6, 1) AS value_m
           FROM bronze.comtrade_top5_commodities
           WHERE iso3c = '%s' AND flow_desc = 'Export'
             AND yr = (SELECT MAX(yr) FROM bronze.comtrade_top5_commodities
                       WHERE iso3c = '%s' AND flow_desc = 'Export')
           ORDER BY rank", iso, iso)),
        error = function(e) data.frame())

      if (nrow(df) == 0)
        return(datatable(data.frame(Note = "No export data available")))

      names(df) <- c("Rank", "Commodity", "Category", "USD M")
      vals <- df[["USD M"]]
      dt <- datatable(df, options = list(dom = "t", ordering = FALSE, pageLength = 5),
                      selection = "none", rownames = FALSE, class = "compact stripe")
      if (sum(!is.na(vals)) >= 2)
        dt <- dt |> formatStyle("USD M",
          background = styleColorBar(range(vals, na.rm = TRUE), "#859C7233"),
          backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
          backgroundPosition = "left")
      dt
    }, error = function(e) datatable(data.frame(Error = conditionMessage(e))))
  })

  output$d3_reer <- renderEcharts4r({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(empty_chart())

      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "SELECT CAST(period AS INTEGER) AS year, value AS reer
           FROM bronze.wb_wdi_raw
           WHERE country_code = '%s'
             AND indicator_id = 'PX.REX.REER'
             AND value IS NOT NULL
           ORDER BY period", iso)),
        error = function(e) data.frame())

      if (nrow(df) < 2) return(empty_chart("REER data not available"))

      df <- df |> dplyr::filter(!is.na(reer)) |>
                  dplyr::mutate(yr = as.character(year))

      df |>
        e_charts(yr, width = "100%", height = "255px") |>
        e_line(reer, name = "REER", smooth = TRUE, symbol = "none",
               lineStyle = list(color = "#2B4C6F", width = 2.5),
               areaStyle = list(color = "#2B4C6F28")) |>
        e_y_axis(splitNumber = 4, axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 splitLine = list(lineStyle = list(color = "#eff2f6", type = "dashed")),
                 axisLine = list(show = FALSE), axisTick = list(show = FALSE)) |>
        e_x_axis(axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 axisLine = list(lineStyle = list(color = "#dde4ec")),
                 axisTick = list(show = FALSE), splitLine = list(show = FALSE)) |>
        e_legend(show = FALSE) |>
        e_grid(left = 44, right = 10, top = 12, bottom = 28) |>
        e_tooltip(trigger = "axis", textStyle = list(color = "#2E3338", fontSize = 10)) |>
        ro_render()
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(empty_chart())
      empty_chart(paste("Error:", conditionMessage(e)))
    })
  })

  output$d3_map_label <- renderText({
    sel <- input$selected_d3_chart
    if (is.null(sel) || nchar(sel) == 0) return("D3 Composite Score")
    c(basket_cv    = "Export Basket CV",
      export_vol   = "Export Volume Shock",
      export_price = "Export Price Shock",
      top5         = "D3 Composite Score",
      concentration= "D3 Composite Score",
      reer         = "D3 Composite Score",
      minerals     = "D3 Composite Score")[[sel]] %||% "D3 Composite Score"
  })

  output$d3_cv_map <- renderEcharts4r({
    tryCatch({
      sel   <- input$selected_d3_chart %||% ""
      inds  <- list(
        basket_cv    = "basket_price_vol",
        export_vol   = "vol_risk",
        export_price = "price_risk"
      )[[sel]]

      df_map <- if (!is.null(inds)) {
        tryCatch(
          dbGetQuery(con, sprintf(
            "WITH mx AS (
               SELECT MAX(year*100+month) AS ym
               FROM gold.d3_monthly_indicator_ts WHERE indicator = '%s'
             )
             SELECT m.country_code, AVG(m.fit_mean) AS value
             FROM gold.d3_monthly_indicator_ts m, mx
             WHERE m.year*100+m.month = mx.ym AND m.indicator = '%s'
             GROUP BY m.country_code", inds, inds)),
          error = function(e) data.frame())
      } else data.frame()

      if (nrow(df_map) == 0)
        df_map <- latest_tiers |>
          dplyr::transmute(country_code, value = d3_score) |>
          dplyr::filter(!is.na(value))

      if (nrow(df_map) == 0) return(empty_chart("Map data not available"))

      df_map <- df_map |>
        dplyr::mutate(ename = countrycode::countrycode(
          country_code, "iso3c", "country.name.en", warn = FALSE)) |>
        dplyr::filter(!is.na(ename), !is.na(value))

      abs_max <- max(abs(df_map$value), na.rm = TRUE)
      abs_max <- if (is.finite(abs_max) && abs_max > 0) abs_max else 1

      df_map |>
        e_charts(ename, width = "100%", height = "290px") |>
        e_map(value, map = "world", roam = FALSE, aspectScale = 0.75,
              itemStyle = list(borderColor = "#c5cdd6", borderWidth = 0.3,
                               areaColor = "#e8edf2"),
              emphasis = list(itemStyle = list(areaColor = "#f0ad4e"))) |>
        e_visual_map(value, min = -abs_max, max = abs_max,
                     type = "continuous", calculable = FALSE,
                     inRange = list(color = c("#2B6CB0","#f7f9fc","#C05621")),
                     outOfRange = list(color = "#d3dce6"), text = c("High","Low"),
                     orient = "vertical", right = 2, bottom = 20,
                     itemHeight = 55, itemWidth = 7,
                     textStyle = list(fontSize = 8, color = "#8a96a3")) |>
        e_tooltip(trigger = "item",
                  formatter = htmlwidgets::JS(
                    "function(p){ return (p.value===undefined||isNaN(p.value))
                       ? '<b>'+p.name+'</b><br/>No data'
                       : '<b>'+p.name+'</b><br/>Score: '+p.value.toFixed(2); }"),
                  textStyle = list(color = "#2E3338", fontSize = 10)) |>
        ro_render()
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(empty_chart())
      empty_chart(paste("Map error:", conditionMessage(e)))
    })
  })

  # D3 monthly sparklines — exact D2 pattern, confirmed indicator names
  d3_sparkline <- function(iso, ind_name, clr) {
    tryCatch({
      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "WITH latest AS (
             SELECT MAX(year*100+month) AS mx
             FROM gold.d3_monthly_indicator_ts
             WHERE country_code = '%s' AND indicator = '%s'
           )
           SELECT year, month, fit_mean AS val
           FROM gold.d3_monthly_indicator_ts, latest
           WHERE country_code = '%s' AND indicator = '%s'
             AND year*100+month >= mx - 200
           ORDER BY year, month", iso, ind_name, iso, ind_name)),
        error = function(e) data.frame())

      if (nrow(df) < 2 || all(is.na(df$val)))
        return(empty_chart("No data", height = "210px"))

      df <- df |> dplyr::mutate(ym = sprintf("%d-%02d", year, month))

      df |>
        e_charts(ym, width = "100%", height = "210px") |>
        e_line(val, smooth = TRUE, symbol = "none", name = "Value",
               lineStyle = list(color = clr, width = 2.5),
               areaStyle = list(color = paste0(substr(clr,1,7),"32"))) |>
        e_y_axis(splitNumber = 3, axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 splitLine = list(lineStyle = list(color = "#eff2f6", type = "dashed")),
                 axisLine = list(show = FALSE), axisTick = list(show = FALSE)) |>
        e_x_axis(axisLabel = list(fontSize = 9, color = "#8a96a3",
                   formatter = htmlwidgets::JS(
                     "function(v){ return v.slice(-2)==='01'?v.slice(0,4):''; }")),
                 axisLine = list(lineStyle = list(color = "#dde4ec")),
                 axisTick = list(show = FALSE), splitLine = list(show = FALSE)) |>
        e_legend(show = FALSE) |>
        e_grid(left = 40, right = 8, top = 10, bottom = 24) |>
        e_tooltip(trigger = "axis", textStyle = list(color = "#2E3338", fontSize = 10)) |>
        ro_render()
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(empty_chart(height = "210px"))
      empty_chart(paste("Error:", conditionMessage(e)), height = "210px")
    })
  }

  output$d3_cv_line <- renderEcharts4r({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(empty_chart(height = "210px"))
      d3_sparkline(iso, "basket_price_vol", "#5D8FA3")
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(empty_chart(height = "210px"))
      empty_chart(paste("Error:", conditionMessage(e)), height = "210px")
    })
  })

  output$d3_vol_line <- renderEcharts4r({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(empty_chart(height = "210px"))
      d3_sparkline(iso, "vol_risk", "#B8795E")
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(empty_chart(height = "210px"))
      empty_chart(paste("Error:", conditionMessage(e)), height = "210px")
    })
  })

  output$d3_price_line <- renderEcharts4r({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(empty_chart(height = "210px"))
      d3_sparkline(iso, "price_risk", "#859C72")
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(empty_chart(height = "210px"))
      empty_chart(paste("Error:", conditionMessage(e)), height = "210px")
    })
  })

  output$d3_mineral_panel <- renderUI({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(NULL)
      minerals_df <- tryCatch(
        dbGetQuery(con, sprintf(
          "WITH latest AS (SELECT MAX(mcs_edition) AS ed FROM bronze.usgs_production),
           gp AS (SELECT p.mineral, SUM(p.production_qty) AS wp
                  FROM bronze.usgs_production p, latest
                  WHERE p.mcs_edition = latest.ed AND p.production_qty > 0
                  GROUP BY p.mineral),
           cp AS (SELECT p.mineral, SUM(p.production_qty) AS cp
                  FROM bronze.usgs_production p, latest
                  WHERE p.iso3c = '%s' AND p.mcs_edition = latest.ed
                    AND p.production_qty > 0 GROUP BY p.mineral)
           SELECT c.mineral, ROUND(100.0*c.cp/g.wp,1) AS pct
           FROM cp c JOIN gp g ON c.mineral = g.mineral
           WHERE g.wp > 0 AND 100.0*c.cp/g.wp > 5 ORDER BY pct DESC", iso)),
        error = function(e) data.frame())
      if (nrow(minerals_df) == 0) return(NULL)
      chips <- lapply(seq_len(nrow(minerals_df)), function(i) {
        pct <- minerals_df$pct[i]
        bg  <- sprintf("rgba(74,124,140,%.2f)", 0.45 + min(1,(pct-5)/55) * 0.45)
        tags$div(class = "mineral-chip", style = sprintf("background:%s", bg),
          tags$div(class = "mineral-chip-name", toupper(minerals_df$mineral[i])),
          tags$div(class = "mineral-chip-pct",  sprintf("%.0f%%", pct)),
          tags$div(class = "mineral-chip-sub",  "of world supply"))
      })
      intro <- if (nrow(minerals_df) == 1)
        sprintf("This country produces %.0f%% of the world's %s supply.",
                minerals_df$pct[1], minerals_df$mineral[1])
      else
        sprintf("Controls a major share of global supply for %d critical minerals.",
                nrow(minerals_df))
      tags$div(class = "panel-card mineral-panel d3-chart-card",
        onclick = "selectD3Chart('minerals', this)",
        tags$div(class = "panel-title",
          tags$i(class="fas fa-gem", style="margin-right:5px;color:var(--brand-secondary)"),
          "CRITICAL MINERAL LEVERAGE"),
        tags$p(class = "mineral-intro", intro),
        tags$div(class = "mineral-chips-row", chips),
        tags$p(class = "mineral-note",
          "Share of global mine production. Source: USGS Mineral Commodity Summaries."))
    }, error = function(e) NULL)
  })

  output$d3_debug_text <- renderText({
    iso <- tryCatch(cdata()$scores$country_code[1], error = function(e) paste("ERROR:", conditionMessage(e)))
    paste0("iso: ", iso, "\n",
      tryCatch({
        r <- dbGetQuery(con, sprintf(
          "SELECT DISTINCT indicator, MIN(year) AS yr_min, MAX(year) AS yr_max
           FROM gold.d3_annual_indicator_ts
           WHERE country_code = '%s' GROUP BY indicator", iso))
        if (nrow(r) == 0) "d3_annual: NO ROWS"
        else paste0("d3_annual (", nrow(r), " indicators, years ",
                    min(r$yr_min),"-",max(r$yr_max),"): ",
                    paste(r$indicator, collapse=", "))
      }, error = function(e) paste("d3_annual ERROR:", conditionMessage(e))), "\n",
      tryCatch({
        r <- dbGetQuery(con, sprintf(
          "SELECT DISTINCT indicator, MIN(year) AS yr_min, MAX(year) AS yr_max
           FROM gold.d3_monthly_indicator_ts
           WHERE country_code = '%s' GROUP BY indicator", iso))
        if (nrow(r) == 0) "d3_monthly: NO ROWS"
        else paste0("d3_monthly (", nrow(r), " indicators, years ",
                    min(r$yr_min),"-",max(r$yr_max),"): ",
                    paste(r$indicator, collapse=", "))
      }, error = function(e) paste("d3_monthly ERROR:", conditionMessage(e))), "\n",
      tryCatch({
        r <- dbGetQuery(con, sprintf(
          "SELECT COUNT(*) AS n, MAX(yr) AS latest_yr
           FROM bronze.comtrade_top5_commodities
           WHERE iso3c = '%s' AND flow_desc = 'Export'", iso))
        paste0("comtrade exports: ", r$n, " rows, latest yr=", r$latest_yr)
      }, error = function(e) paste("comtrade ERROR:", conditionMessage(e))))
  })


  # ============================================================
  # D4  NETWORK CONTAGION RISK
  # ============================================================

  d4_chart_desc <- list(
    partner_chart = list(
      title = "Why do my trading partners' problems become my problem?",
      text  = paste0(
        "When a country sources critical imports from partners that are themselves ",
        "economically fragile — carrying high debt, facing supply disruptions, or ",
        "dependent on volatile export revenues — any shock to those partners ripples ",
        "upstream. Each bar shows one of this country's key import partners and how ",
        "risky their economic situation currently looks, based on the same four-",
        "dimension model used throughout this platform. The bar color runs from green ",
        "(Low risk) to dark red (Critical). A long dark-red bar from a partner that ",
        "supplies a large share of food or energy imports is the signal most worth watching."
      )
    ),
    exposure_table = list(
      title = "Where exactly does the exposure come from?",
      text  = paste0(
        "This table breaks down each import partner's contribution to total exposure. ",
        "The Trade Weight column shows what share of imports (by value) comes from ",
        "that partner. The D1/D2/D3 columns are the partner's own macro-fiscal, import ",
        "reliance, and export fragility scores — you're inheriting a weighted slice of ",
        "each. The category columns (Food, Energy, Fertilizer) show what fraction of ",
        "this country's food, energy, and fertilizer imports come from each specific ",
        "partner. A partner with a high composite score AND a large share of your ",
        "food imports is a high-priority contagion channel to monitor."
      )
    )
  )

  output$d4_chart_info <- renderUI({
    key <- input$selected_d4_chart
    if (is.null(key) || nchar(key) == 0) key <- "partner_chart"
    desc <- d4_chart_desc[[key]]
    if (is.null(desc))
      return(tags$p(class="narrative-placeholder", "Click a chart above to learn more."))
    tags$div(
      tags$p(class = "indicator-desc-title", desc$title),
      tags$p(class = "indicator-desc",       desc$text)
    )
  })

  output$d4_assessment <- renderUI({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso))
        return(tags$p(class="narrative-placeholder", "Select a country above."))
      narr <- tryCatch(
        dbGetQuery(con, sprintf(
          "SELECT narrative FROM gold.narratives
           WHERE country_code = '%s' AND dimension = 'D4'
           ORDER BY year DESC LIMIT 1", iso)),
        error = function(e) data.frame(narrative = character(0)))
      if (nrow(narr) == 0 || is.na(narr$narrative[1]))
        return(tags$p(class="narrative-placeholder",
          "Network contagion narrative not yet available for this country."))
      tags$p(class = "narrative-text", narr$narrative[1])
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(NULL)
      tags$p(class="narrative-placeholder", paste("Error:", conditionMessage(e)))
    })
  })

  # D4 partner risk bar chart
  output$d4_partner_chart <- renderEcharts4r({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(empty_chart())

      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "WITH latest AS (
             SELECT MAX(year*100+month) AS ym
             FROM gold.d4_partner_detail WHERE country_code = '%s'
           )
           SELECT p.* FROM gold.d4_partner_detail p, latest
           WHERE p.country_code = '%s'
             AND p.year*100+p.month = latest.ym
           ORDER BY weight_norm DESC
           LIMIT 20", iso, iso)),
        error = function(e) data.frame())

      if (nrow(df) == 0) return(empty_chart("Partner risk data not available"))

      # Flexible column detection
      comp_col    <- intersect(names(df), c("composite_risk","composite_score","risk_score","composite","d4_score"))[1]
      partner_col <- intersect(names(df), c("partner_name","partner_country","partner"))[1]
      weight_col  <- intersect(names(df), c("weight_norm","trade_weight","weight","import_weight","norm_weight"))[1]

      if (is.na(comp_col) || is.na(partner_col) || is.na(weight_col))
        return(empty_chart(paste("Col detection failed. Cols:", paste(names(df), collapse=","))))

      df <- df |>
        dplyr::rename(comp = !!comp_col, pname = !!partner_col, wt = !!weight_col) |>
        dplyr::filter(!is.na(comp), !is.na(pname)) |>
        dplyr::mutate(
          label = sprintf("%s  %.1f%%", pname, wt * 100)
        ) |>
        dplyr::arrange(comp)     # ascending so highest risk at top of horizontal bar

      ht <- paste0(max(320, nrow(df) * 24 + 80), "px")

      df |>
        e_charts(label, width = "100%", height = ht) |>
        e_bar(comp, name = "Composite Risk",
              barMaxWidth = 16,
              itemStyle = list(color = htmlwidgets::JS(
                "function(p) {
                   var v = +p.value; if(isNaN(v)) v = +p.data;
                   if(v >= 0.75) return '#8B2635';
                   if(v >= 0.65) return '#C17B5A';
                   if(v >= 0.50) return '#C4A563';
                   if(v >= 0.30) return '#5D8FA3';
                   return '#859C72';
                 }"))) |>
        e_flip_coords() |>
        e_mark_line(data   = list(list(xAxis = 0.75)),
                    symbol = list("none","none"),
                    lineStyle = list(color = "#8B2635", type = "dashed", width = 1),
                    label  = list(formatter = "Critical", fontSize = 8,
                                  color = "#8B2635", position = "insideEndTop")) |>
        e_mark_line(data   = list(list(xAxis = 0.65)),
                    symbol = list("none","none"),
                    lineStyle = list(color = "#C17B5A", type = "dashed", width = 1),
                    label  = list(formatter = "Elevated", fontSize = 8,
                                  color = "#C17B5A", position = "insideEndTop")) |>
        e_x_axis(min = 0, max = 1, splitNumber = 5,
                 axisLabel = list(fontSize = 9, color = "#8a96a3"),
                 axisLine  = list(lineStyle = list(color = "#dde4ec")),
                 splitLine = list(lineStyle = list(color = "#eff2f6", type = "dashed"))) |>
        e_y_axis(axisLabel = list(fontSize = 9, color = "#2E3338"),
                 axisLine  = list(show = FALSE), axisTick = list(show = FALSE)) |>
        e_legend(show = FALSE) |>
        e_grid(left = 200, right = 30, top = 10, bottom = 30) |>
        e_tooltip(trigger = "item",
                  formatter = htmlwidgets::JS(
                    "function(p){ return '<b>'+p.name+'</b><br/>Composite risk: '+
                      (+p.value).toFixed(3); }"),
                  textStyle = list(color = "#2E3338", fontSize = 10)) |>
        ro_render()
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) return(empty_chart())
      empty_chart(paste("Error:", conditionMessage(e)))
    })
  })

  # D4 supply chain exposure table
  output$d4_exposure_table <- renderDT({
    tryCatch({
      iso <- cdata()$scores$country_code[1]
      if (is.null(iso) || is.na(iso)) return(datatable(data.frame(Note = "No country")))

      df <- tryCatch(
        dbGetQuery(con, sprintf(
          "WITH latest AS (
             SELECT MAX(year*100+month) AS ym
             FROM gold.d4_partner_detail WHERE country_code = '%s'
           )
           SELECT p.* FROM gold.d4_partner_detail p, latest
           WHERE p.country_code = '%s'
             AND p.year*100+p.month = latest.ym
           ORDER BY weight_norm DESC", iso, iso)),
        error = function(e) data.frame())

      if (nrow(df) == 0) return(datatable(data.frame(Note = "No partner data available")))

      comp_col    <- intersect(names(df), c("composite_risk","composite_score","risk_score","composite","d4_score"))[1]
      partner_col <- intersect(names(df), c("partner_name","partner_country","partner"))[1]
      weight_col  <- intersect(names(df), c("weight_norm","trade_weight","weight","import_weight","norm_weight"))[1]

      if (is.na(comp_col)) return(datatable(data.frame(Error = paste("Cols:", paste(names(df), collapse=",")))))

      out <- data.frame(
        Partner      = df[[partner_col]],
        `Trade Wt.`  = sprintf("%.1f%%", df[[weight_col]] * 100),
        Composite    = round(df[[comp_col]], 3),
        check.names  = FALSE
      )
      if ("d1_score" %in% names(df)) out$`Macro-Fiscal`   <- round(df$d1_score, 3)
      if ("d2_score" %in% names(df)) out$`Import Reliance` <- round(df$d2_score, 3)
      if ("d3_score" %in% names(df)) out$`Export Fragility`<- round(df$d3_score, 3)
      if ("food_import_share"        %in% names(df)) out$Food       <- sprintf("%.1f%%", df$food_import_share * 100)
      if ("energy_import_share"      %in% names(df)) out$Energy     <- sprintf("%.1f%%", df$energy_import_share * 100)
      if ("fertilizer_import_share"  %in% names(df)) out$Fertilizer <- sprintf("%.1f%%", df$fertilizer_import_share * 100)
      if ("export_share"             %in% names(df)) out$`Export Dest.` <- sprintf("%.1f%%", df$export_share * 100)

      datatable(out,
        options = list(dom = "tp", pageLength = 15, ordering = TRUE,
                       columnDefs = list(list(className = "dt-right",
                                              targets = seq_len(ncol(out)) - 1))),
        selection = "none", rownames = FALSE, class = "compact stripe") |>
        formatStyle("Composite",
          background = styleColorBar(c(0, 1), "#859C7233"),
          backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
          backgroundPosition = "left")
    }, error = function(e) datatable(data.frame(Error = conditionMessage(e))))
  })

  output$d4_debug_text <- renderText({
    iso <- tryCatch(cdata()$scores$country_code[1], error = function(e) paste("ERROR:", conditionMessage(e)))
    paste0("iso: ", iso, "\n",
      tryCatch({
        r <- dbGetQuery(con, sprintf(
          "SELECT * FROM gold.d4_partner_detail WHERE country_code = '%s' LIMIT 1", iso))
        n <- dbGetQuery(con, sprintf(
          "SELECT COUNT(*) AS n FROM gold.d4_partner_detail WHERE country_code = '%s'", iso))$n
        paste0("d4_partner_detail cols: ", paste(names(r), collapse=", "),
               "\nrows for ", iso, ": ", n)
      }, error = function(e) paste("d4_partner_detail ERROR:", conditionMessage(e))))
  })

}