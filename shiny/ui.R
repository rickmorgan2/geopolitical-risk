fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(
      href = paste0("https://fonts.googleapis.com/css2?family=Oswald:wght@400;500;600",
                    "&family=Work+Sans:ital,wght@0,300;0,400;0,500;0,600;1,400&display=swap"),
      rel  = "stylesheet"
    ),
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$script(HTML("
      function selectCard(tabId) {
        document.querySelectorAll('.score-card').forEach(c => c.classList.remove('score-card-active'));
        var card = document.getElementById('card-' + tabId);
        if (card) card.classList.add('score-card-active');
        Shiny.setInputValue('active_tab', tabId, {priority: 'event'});
      }

      function selectKpi(key, el) {
        document.querySelectorAll('.kpi-clickable').forEach(c => c.classList.remove('kpi-selected'));
        if (el) el.classList.add('kpi-selected');
        Shiny.setInputValue('selected_kpi', key, {priority: 'event'});
      }

      function selectD4Chart(key, el) {
        document.querySelectorAll('.d4-chart-card').forEach(c => c.classList.remove('d4-chart-active'));
        if (el) el.classList.add('d4-chart-active');
        Shiny.setInputValue('selected_d4_chart', key, {priority: 'event'});
      }

      function selectD3Chart(key, el) {
        document.querySelectorAll('.d3-chart-card').forEach(c => c.classList.remove('d3-chart-active'));
        if (el) el.classList.add('d3-chart-active');
        Shiny.setInputValue('selected_d3_chart', key, {priority: 'event'});
      }

      function selectD2Chart(key, el) {
        document.querySelectorAll('.d2-chart-card').forEach(c => c.classList.remove('d2-chart-active'));
        if (el) el.classList.add('d2-chart-active');
        Shiny.setInputValue('selected_d2_chart', key, {priority: 'event'});
      }

      $(document).on('shiny:value', function(event) {
        if (event.name === 'score_cards_ui') {
          setTimeout(function() {
            var cur = Shiny.shinyapp.$inputValues['active_tab'] || 'macrofiscal';
            document.querySelectorAll('.score-card').forEach(c => c.classList.remove('score-card-active'));
            var card = document.getElementById('card-' + cur);
            if (card) card.classList.add('score-card-active');
          }, 50);
        }
        if (event.name === 'indicator_info') {
          if (!document.querySelector('.kpi-selected')) {
            setTimeout(function() {
              var first = document.querySelector('.kpi-grid-row .kpi-clickable');
              if (first) first.classList.add('kpi-selected');
            }, 80);
          }
        }
        if (event.name === 'd4_chart_info') {
          if (!document.querySelector('.d4-chart-active')) {
            setTimeout(function() {
              var first = document.querySelector('.d4-chart-card');
              if (first) first.classList.add('d4-chart-active');
            }, 80);
          }
        }
        if (event.name === 'd3_chart_info') {
          if (!document.querySelector('.d3-chart-active')) {
            setTimeout(function() {
              var first = document.querySelector('.d3-chart-card');
              if (first) first.classList.add('d3-chart-active');
            }, 80);
          }
        }
        if (event.name === 'd2_chart_info') {
          if (!document.querySelector('.d2-chart-active')) {
            setTimeout(function() {
              var first = document.querySelector('.d2-chart-card');
              if (first) first.classList.add('d2-chart-active');
            }, 80);
          }
        }
      });
    "))
  ),

  # ---- Sticky header --------------------------------------------------------
  tags$div(class = "report-topbar",
    tags$div(class = "topbar-inner",

      tags$div(class = "topbar-row",
        tags$span(class = "topbar-wordmark", "Pressure Points"),
        tags$div(class = "topbar-selector",
          selectInput("country_select", NULL,
                      choices  = country_choices,
                      selected = "USA",
                      width    = "100%")
        ),
        tags$div(class = "topbar-right",
          tags$a(href = "#", class = "about-link",
                 onclick = "selectCard('about'); return false;",
                 tags$i(class = "fas fa-info-circle"), " About")
        )
      ),

      uiOutput("country_header_ui"),
      uiOutput("score_cards_ui")
    )
  ),

  # ---- Main tab content -----------------------------------------------------
  tags$div(class = "app-main",
    tabsetPanel(
      id   = "main_tabs",
      type = "hidden",




      # ================================================================
      tabPanelBody("macrofiscal",
        tags$div(class = "tab-body",

          # Numeric summary card
          fluidRow(column(12, uiOutput("kpi_nums"))),

          # Assessment panel
          tags$div(class = "panel-card assessment-panel-card",
            tags$div(class = "panel-title-row",
              tags$span(class = "panel-title", "ASSESSMENT"),
              tags$span(class = "ai-badge",
                tags$i(class = "fas fa-robot"), " AI-Generated \u00b7 Experimental"
              )
            ),
            uiOutput("d1_assessment")
          ),

          # Comparison toggle — sits just above the figures
          tags$div(class = "comp-toggle-row",
            actionButton("toggle_comparisons",
              label = "Show Comparisons",
              class = "btn-comparisons",
              icon  = icon("chart-line"))
          ),

          # Shared legend + hint + KPI grid
          uiOutput("kpi_legend"),
          tags$p(class = "kpi-grid-hint", "Click on a figure to learn more"),
          tags$div(class = "row kpi-grid-row",
            tags$div(class = "col-12 col-sm-6 col-md-4",
              tags$div(class = "sparkline-card panel-card kpi-clickable",
                onclick = "selectKpi('gdp_growth', this)",
                tags$div(class = "panel-title", "GDP GROWTH %"),
                echarts4rOutput("spark_gdp_growth", height = "120px")
              )
            ),
            tags$div(class = "col-12 col-sm-6 col-md-4",
              tags$div(class = "sparkline-card panel-card kpi-clickable",
                onclick = "selectKpi('current_acct', this)",
                tags$div(class = "panel-title", "CURRENT ACCOUNT % GDP"),
                echarts4rOutput("spark_current_acct", height = "120px")
              )
            ),
            tags$div(class = "col-12 col-sm-6 col-md-4",
              tags$div(class = "sparkline-card panel-card kpi-clickable",
                onclick = "selectKpi('fiscal_bal', this)",
                tags$div(class = "panel-title", "FISCAL BALANCE % GDP"),
                echarts4rOutput("spark_fiscal", height = "120px")
              )
            ),
            tags$div(class = "col-12 col-sm-6 col-md-4",
              tags$div(class = "sparkline-card panel-card kpi-clickable",
                onclick = "selectKpi('inflation', this)",
                tags$div(class = "panel-title", "INFLATION %"),
                echarts4rOutput("spark_inflation", height = "120px")
              )
            ),
            tags$div(class = "col-12 col-sm-6 col-md-4",
              tags$div(class = "sparkline-card panel-card kpi-clickable",
                onclick = "selectKpi('gross_debt', this)",
                tags$div(class = "panel-title", "GOVT GROSS DEBT % GDP"),
                echarts4rOutput("d1_chart_gross_debt", height = "120px")
              )
            ),
            tags$div(class = "col-12 col-sm-6 col-md-4",
              tags$div(class = "sparkline-card panel-card kpi-clickable",
                onclick = "selectKpi('debt_svc', this)",
                tags$div(class = "panel-title", "DEBT SERVICE % EXPORTS"),
                echarts4rOutput("d1_chart_debt_svc", height = "120px")
              )
            ),
            tags$div(class = "col-12 col-sm-6 col-md-4",
              tags$div(class = "sparkline-card panel-card kpi-clickable",
                onclick = "selectKpi('ext_debt', this)",
                tags$div(class = "panel-title", "EXT. DEBT / RESERVES"),
                echarts4rOutput("d1_chart_ext_debt", height = "120px")
              )
            ),
            tags$div(class = "col-12 col-sm-6 col-md-4",
              tags$div(class = "sparkline-card panel-card kpi-clickable",
                onclick = "selectKpi('trade_gdp', this)",
                tags$div(class = "panel-title", "TRADE % GDP"),
                echarts4rOutput("d1_chart_trade", height = "120px")
              )
            ),
            tags$div(class = "col-12 col-sm-6 col-md-4",
              tags$div(class = "sparkline-card panel-card kpi-clickable",
                onclick = "selectKpi('net_fdi', this)",
                tags$div(class = "panel-title", "NET FDI % GDP"),
                echarts4rOutput("d1_chart_fdi", height = "120px")
              )
            )
          ),
          # Indicator detail panel — below the figures, defaults to GDP growth
          tags$div(class = "panel-card indicator-info-panel",
            tags$div(class = "panel-title-row",
              tags$span(class = "panel-title", "ABOUT THIS INDICATOR"),
              tags$span(class = "ai-badge",
                tags$i(class = "fas fa-robot"), " AI-Generated \u00b7 Experimental"
              )
            ),
            uiOutput("indicator_info")
          )
        )
      ),



      # Stubs for remaining tabs
      # STRATEGIC IMPORT RELIANCE
      tabPanelBody("importreliance",
        tags$div(class = "tab-body",

          # Temporary DB debug panel — remove once column names confirmed
          tags$details(
            tags$summary(style="font-size:0.75em;color:#8a96a3;cursor:pointer;margin-bottom:6px",
                         "DB diagnostics (click to expand)"),
            verbatimTextOutput("d2_debug_text")
          ),

          # Row 1: Assessment — full width
          tags$div(class = "panel-card assessment-panel-card",
            tags$div(class = "panel-title-row",
              tags$span(class = "panel-title", "ASSESSMENT"),
              tags$span(class = "ai-badge",
                tags$i(class = "fas fa-robot"), " AI-Generated \u00b7 Experimental"
              )
            ),
            uiOutput("d2_assessment")
          ),

          # Row 2: Top 5 imports | Supplier diversity | Food net trade
          tags$p(class = "kpi-grid-hint", "Click on a figure to learn more"),
          fluidRow(
            column(4,
              tags$div(class = "panel-card d2-chart-card",
                onclick = "selectD2Chart('top5', this)",
                tags$div(class = "panel-title", "TOP 5 IMPORTS — LATEST YEAR"),
                DTOutput("d2_top5_imports")
              )
            ),
            column(4,
              tags$div(class = "panel-card d2-chart-card",
                onclick = "selectD2Chart('diversity', this)",
                tags$div(class = "panel-title", "SUPPLIER DIVERSITY — ANNUAL"),
                echarts4rOutput("d2_diversity_chart", height = "220px")
              )
            ),
            column(4,
              tags$div(class = "panel-card d2-chart-card",
                onclick = "selectD2Chart('food_net', this)",
                tags$div(class = "panel-title", "FOOD NET TRADE POSITION — LATEST YEAR"),
                echarts4rOutput("d2_food_net_trade", height = "220px")
              )
            )
          ),

          # Metric selector: drives both time-series emphasis and choropleth
          tags$div(class = "d2-metric-row",
            tags$span(class = "d2-metric-label", "Map & highlight:"),
            selectInput("d2_map_metric", NULL,
              choices = c(
                "Food Basket CV"        = "food_cv",
                "Energy Basket CV"      = "energy_cv",
                "Fertilizer Basket CV"  = "fert_cv",
                "Food Import Volume"    = "food_vol",
                "Food Import Price"     = "food_price",
                "Energy Import Volume"  = "energy_vol"
              ),
              selected = "food_cv", width = "195px"
            )
          ),

          # Row 3: Monthly signals (col-8) + global choropleth (col-4)
          fluidRow(
            column(8,
              tags$div(class = "panel-card d2-chart-card",
                onclick = "selectD2Chart('monthly', this)",
                tags$div(class = "panel-title", "PRICE VOLATILITY & ANOMALY SIGNALS — MONTHLY"),
                echarts4rOutput("d2_monthly_signals", height = "250px")
              )
            ),
            column(4,
              tags$div(class = "panel-card",
                tags$div(class = "panel-title", "GLOBAL LATEST"),
                echarts4rOutput("d2_cv_map", height = "250px")
              )
            )
          ),

          # About this chart — reactive panel, updates on card click
          tags$div(class = "panel-card indicator-info-panel",
            tags$div(class = "panel-title-row",
              tags$span(class = "panel-title", "ABOUT THIS CHART"),
              tags$span(class = "ai-badge",
                tags$i(class = "fas fa-robot"), " AI-Generated \u00b7 Experimental"
              )
            ),
            uiOutput("d2_chart_info")
          )
        )
      ),

      tabPanelBody("exportfragility",
        tags$div(class = "tab-body",

          # Debug panel (collapsible)
          tags$details(
            tags$summary(style = "font-size:0.75em;color:#8a96a3;cursor:pointer;padding:4px 0",
              "DB diagnostics (expand to debug)"),
            tags$pre(id = "d3_debug_pre", style = "font-size:0.7em;max-height:200px;overflow:auto",
              textOutput("d3_debug_text"))
          ),

          # Row 1: Assessment full width
          tags$div(class = "panel-card assessment-panel-card",
            tags$div(class = "panel-title-row",
              tags$span(class = "panel-title", "ASSESSMENT — EXPORT FRAGILITY"),
              tags$span(class = "ai-badge",
                tags$i(class = "fas fa-robot"), " AI-Generated \u00b7 Experimental")
            ),
            uiOutput("d3_assessment")
          ),

          # Row 2: Top 5 exports (col-3) | Concentration (col-5) | REER (col-4)
          tags$p(class = "kpi-grid-hint", "Click on a figure to learn more"),
          fluidRow(
            column(3,
              tags$div(class = "panel-card d3-chart-card",
                onclick = "selectD3Chart('top5', this)",
                tags$div(class = "panel-title", "TOP 5 EXPORTS — LATEST YEAR"),
                DTOutput("d3_top5_exports")
              )
            ),
            column(5,
              tags$div(class = "panel-card d3-chart-card",
                onclick = "selectD3Chart('concentration', this)",
                tags$div(class = "panel-title", "EXPORT CONCENTRATION — ANNUAL"),
                echarts4rOutput("d3_concentration", height = "255px")
              )
            ),
            column(4,
              tags$div(class = "panel-card d3-chart-card",
                onclick = "selectD3Chart('reer', this)",
                tags$div(class = "panel-title", "REAL EFFECTIVE EXCHANGE RATE"),
                echarts4rOutput("d3_reer", height = "255px")
              )
            )
          ),

          # Row 3: Map (col-5) + three revenue signal sparklines (col-7)
          fluidRow(
            column(5,
              tags$div(class = "panel-card",
                tags$div(class = "panel-title",
                  "GLOBAL LATEST \u2014 ", textOutput("d3_map_label", inline = TRUE)
                ),
                echarts4rOutput("d3_cv_map", height = "290px")
              )
            ),
            column(7,
              fluidRow(
                column(4,
                  tags$div(class = "panel-card d3-chart-card sparkline-card",
                    onclick = "selectD3Chart('basket_cv', this)",
                    tags$div(class = "panel-title", "EXPORT BASKET CV"),
                    echarts4rOutput("d3_cv_line", height = "210px")
                  )
                ),
                column(4,
                  tags$div(class = "panel-card d3-chart-card sparkline-card",
                    onclick = "selectD3Chart('export_vol', this)",
                    tags$div(class = "panel-title", "EXPORT VOLUME SHOCK"),
                    echarts4rOutput("d3_vol_line", height = "210px")
                  )
                ),
                column(4,
                  tags$div(class = "panel-card d3-chart-card sparkline-card",
                    onclick = "selectD3Chart('export_price', this)",
                    tags$div(class = "panel-title", "EXPORT PRICE SHOCK"),
                    echarts4rOutput("d3_price_line", height = "210px")
                  )
                )
              )
            )
          ),

          # Row 4: Conditional mineral panel
          uiOutput("d3_mineral_panel"),

          # Row 5: About this chart
          tags$div(class = "panel-card indicator-info-panel",
            tags$div(class = "panel-title-row",
              tags$span(class = "panel-title", "ABOUT THIS CHART")
            ),
            uiOutput("d3_chart_info")
          )
        )
      ),

      tabPanelBody("network",
        tags$div(class = "tab-body",

          # Debug panel
          tags$details(
            tags$summary(style = "font-size:0.75em;color:#8a96a3;cursor:pointer;padding:4px 0",
              "DB diagnostics (expand to debug)"),
            tags$pre(style = "font-size:0.7em;max-height:200px;overflow:auto",
              textOutput("d4_debug_text"))
          ),

          # Row 1: Partner risk chart (col-7) + Assessment (col-5)
          fluidRow(
            column(7,
              tags$div(class = "panel-card d4-chart-card",
                onclick = "selectD4Chart('partner_chart', this)",
                tags$div(class = "panel-title", "CRITICAL IMPORT PARTNER RISK"),
                echarts4rOutput("d4_partner_chart", height = "420px")
              )
            ),
            column(5,
              tags$div(class = "panel-card assessment-panel-card",
                tags$div(class = "panel-title-row",
                  tags$span(class = "panel-title", "ASSESSMENT — NETWORK CONTAGION"),
                  tags$span(class = "ai-badge",
                    tags$i(class = "fas fa-robot"), " AI-Generated \u00b7 Experimental")
                ),
                uiOutput("d4_assessment")
              )
            )
          ),

          # Row 2: Supply chain exposure table (full width)
          tags$div(class = "panel-card d4-chart-card",
            onclick = "selectD4Chart('exposure_table', this)",
            tags$div(class = "panel-title", "SUPPLY CHAIN EXPOSURE BY PARTNER"),
            DTOutput("d4_exposure_table")
          ),

          # About this chart
          tags$div(class = "panel-card indicator-info-panel",
            tags$div(class = "panel-title-row",
              tags$span(class = "panel-title", "ABOUT THIS CHART")
            ),
            uiOutput("d4_chart_info")
          )
        )
      ),

      tabPanelBody("overview",
        tags$div(class = "tab-body",
          tags$div(class = "panel-card stub-panel",
            tags$div(class = "panel-title", "OVERALL COMPOSITE SCORE"),
            tags$p(class = "stub-note", "Overview summary coming soon.")
          )
        )
      ),

      tabPanelBody("about",
        tags$div(class = "tab-body about-page",
          tags$div(class = "panel-card",
            tags$div(class = "panel-title", "ABOUT PRESSURE POINTS"),
            tags$p(
              "Pressure Points scores 175 countries across four risk dimensions: macro-fiscal stability,",
              " strategic import reliance, export fragility, and network contagion risk.",
              " Scores are derived from Bayesian models with 95% credible intervals",
              " and updated quarterly."
            )
          )
        )
      )
    )
  )
)
