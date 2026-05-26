library(shiny)
library(tidyverse)
library(lubridate)
library(lunar)
library(MASS)
library(broom)

## Chart themese -------
theme_moth <- function() {
  theme_classic(base_size = 15) +
    theme(
      plot.title =
        element_text(
          face = "bold",
          size = 18,
          hjust = 0.5
        ),
      plot.subtitle =
        element_text(
          size = 12,
          color = "grey30"
        ),
      axis.title =
        element_text(
          face = "bold"
        ),
      
      axis.text =
        element_text(
          color = "black"
        ),
      legend.position = "top",
      legend.title =
        element_text(face = "bold"),
      panel.border =
        element_rect(
          fill = NA,
          color = "black",
          linewidth = 0.8
        ),
      plot.margin =
        margin(15, 15, 15, 15)
    )
}

# Colors -------
moth_colors <- c(
  "2023" = "#0072B2",
  "2024" = "#D55E00",
  "2025" = "#009E73",
  "2026" = "#CC79A7"
)

# UI -------

ui <- fluidPage(
  titlePanel("Luna Moth Phenology Analyzer"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        "img_dir",
        "Image Folder Path",
        value = "D:/moths/Code/AbundanceV1/Images"
      ),
      fileInput(
        "env_file",
        "Environmental CSV",
        accept = ".csv"
      ),
      actionButton(
        "run",
        "Run Analysis"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Seasonal Curves",
          plotOutput("curve_plot")
        ),

        tabPanel(
          "Cumulative Emergence",
          plotOutput("cumulative_plot")
        ),

        tabPanel(
          "Moon Illumination",
          plotOutput("moon_plot")
        ),

        tabPanel(
          "Phenology Metrics",
          tableOutput("metrics_table")
        ),
        
        tabPanel(
          "Environmental Models",
          
          h3("AIC Comparison"),
          tableOutput("aic_table"),
          
          br(),
          
          h3("Full Model Coefficients"),
          tableOutput("coef_table_all"),
          
          br(),
          
          h3("Temperature Model"),
          tableOutput("coef_table_temp"),
          plotOutput("temp_plot"),
          
          br(),
          
          h3("Humidity Model"),
          tableOutput("coef_table_humid"),
          plotOutput("humid_plot"),
          
          br(),
          
          h3("Wind Model"),
          tableOutput("coef_table_wind"),
          plotOutput("wind_plot"),
          
          br(),
          
          h3("Moon Model"),
          tableOutput("coef_table_moon"),
          plotOutput("moon_model_plot")
        )
        
      )
    )
  )
)

# SERVER -------

server <- function(input, output) {
  
  analysis <- eventReactive(input$run, {
  
    # Load image filenames =============
    files <- list.files(
      path = input$img_dir,
      pattern = "\\.(jpg|jpeg)$",
      ignore.case = TRUE,
      full.names = FALSE
    )

    # Parse filenames =============
    dat <- tibble(filename = files) %>%
      
      mutate(
        
        base = str_remove(
          filename,
          regex("\\.(jpg|jpeg)$",
                ignore_case = TRUE)
        )
      ) %>%
      
      separate(
        base,
        into = c(
          "year",
          "location",
          "monthday",
          "time",
          "extra1",
          "extra2"
        ),
        sep = "_",
        remove = FALSE
      ) %>%
      
      mutate(
        
        month = substr(monthday, 1, 2),
        day   = substr(monthday, 3, 4),
        
        date = ymd(
          paste(year, month, day,
                sep = "-")
        ),
        
        doy = yday(date),
        
        year = as.factor(year)
      )
    
    # Nightly abundance =============
    nightly <- dat %>%
      group_by(year, date, doy) %>%
      summarise(
        abundance = n(),
        .groups = "drop"
      )

    # Moon illumination =============
    nightly <- nightly %>%
      mutate(
        moon_illumination =
          lunar.illumination(date)
      )

    # Load environmental data =============
    if (!is.null(input$env_file)) {
      env <- read_csv(
        input$env_file$datapath,
        show_col_types = FALSE
      )
      env$date <- parse_date_time(
        env$date,
        orders = c(
          "ymd",
          "mdy",
          "dmy"
        )
      )
      
      env$date <- as.Date(env$date)
      nightly$date <- as.Date(nightly$date)
      nightly <- nightly %>%
        left_join(
          env,
          by = "date"
        )
    }
    
    # Phenology metrics =============
    phenology <- nightly %>%
      group_by(year) %>%
      summarise(
        first_appearance =
          min(date),
        peak_date =
          date[which.max(abundance)],
        peak_abundance =
          max(abundance),
        last_appearance =
          max(date),
        season_length_days =
          as.numeric(
            max(date) - min(date)
          ) + 1,
        .groups = "drop"
      )
    
    # Environmental model =============
    
    coef_table_all <- NULL
    coef_table_temp <- NULL
    coef_table_humid <- NULL
    coef_table_wind <- NULL
    coef_table_moon <- NULL
    
    aic_table <- NULL
    
    m_temp <- NULL
    m_humid <- NULL
    m_wind <- NULL
    m_moon <- NULL
    m_all <- NULL
    
    if (all(c(
      "temp",
      "humidity",
      "wind"
    ) %in% names(nightly))) {
      
      model_data <- nightly %>%
        
        drop_na(
          abundance,
          temp,
          humidity,
          wind,
          moon_illumination
        )
      
      print(head(model_data))
      print(nrow(model_data))
      
      if (nrow(model_data) > 5) {

        # Individual models
        m_temp <- glm.nb(
          abundance ~ temp,
          data = model_data
        )
        
        m_humid <- glm.nb(
          abundance ~ humidity,
          data = model_data
        )
        
        m_wind <- glm.nb(
          abundance ~ wind,
          data = model_data
        )
        
        m_moon <- glm.nb(
          abundance ~ moon_illumination,
          data = model_data
        )

        # Full model
        m_all <- glm.nb(
          abundance ~
            doy +
            I(doy^2) +
            temp +
            humidity +
            wind +
            moon_illumination,
          
          data = model_data
        )

        # AIC table
        # AIC table
        aic_table <- AIC(
          m_temp,
          m_humid,
          m_wind,
          m_moon
        ) %>%
          as.data.frame() %>%
          rownames_to_column("Model") %>%
          mutate(
            Model = c(
              "Temperature",
              "Humidity",
              "Wind",
              "Moon Illumination"
            )
          )

        # Coefficient tables 
        coef_table_all <- tidy(m_all)
        coef_table_temp <- tidy(m_temp)
        coef_table_humid <- tidy(m_humid)
        coef_table_wind <- tidy(m_wind)
        coef_table_moon <- tidy(m_moon)
        
      }
    }
    

    # Return results =============
    
    list(
      nightly = nightly,
      phenology = phenology,
      
      aic_table = aic_table,
      
      coef_table_all = coef_table_all,
      coef_table_temp = coef_table_temp,
      coef_table_humid = coef_table_humid,
      coef_table_wind = coef_table_wind,
      coef_table_moon = coef_table_moon,
      
      m_temp = m_temp,
      m_humid = m_humid,
      m_wind = m_wind,
      m_moon = m_moon
    )
  })

  # Seasonal curves =============
  
  output$curve_plot <- renderPlot({
    nightly <- analysis()$nightly
    ggplot(
      nightly,
      aes(
        x = doy,
        y = abundance,
        color = year,
        group = year
      )
    ) +
      geom_line(
        linewidth = 1.3,
        alpha = 0.9
      ) +
      geom_point(
        size = 3
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        linewidth = 1,
        linetype = "dashed"
      ) +
      scale_color_manual(
        values = moth_colors
      ) +
      labs(
        title = "Seasonal Abundance Curves",
        subtitle =
          "Nightly Luna moth abundance by year",
        x = "Day of Year",
        y = "Nightly Abundance",
        color = "Year"
      ) +
      theme_moth()
    
  })
  
  # Cumulative emergence =============
  
  output$cumulative_plot <- renderPlot({
    nightly <- analysis()$nightly
    cumulative <- nightly %>%
      arrange(year, doy) %>%
      group_by(year) %>%
      mutate(
        cumulative_abundance =
          cumsum(abundance),
        cumulative_percent =
          cumulative_abundance /
          max(cumulative_abundance)
      )
    
    ggplot(
      cumulative,
      aes(
        x = doy,
        y = cumulative_percent,
        color = year
      )
    ) +
      
      geom_line(
        linewidth = 1.8
      ) +
      scale_color_manual(
        values = moth_colors
      ) +
      scale_y_continuous(
        labels =
          scales::percent_format()
      ) +
      labs(
        title = "Cumulative Emergence Curves",
        subtitle =
          "Relative seasonal emergence timing",
        x = "Day of Year",
        y = "Cumulative Emergence",
        color = "Year"
      ) +
      theme_moth()
    
  })
  
  # Moon illumination plot =============
  
  output$moon_plot <- renderPlot({
    nightly <- analysis()$nightly
    ggplot(
      nightly,
      aes(
        x = moon_illumination,
        y = abundance,
        color = year
      )
    ) +
      geom_point(
        size = 3,
        alpha = 0.75
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        linewidth = 1.2
      ) +
      scale_color_manual(
        values = moth_colors
      ) +
      scale_x_continuous(
        limits = c(0, 1)
      ) +
      labs(
        title = "Moon Illumination Effects",
        subtitle =
          "Relationship between moonlight and abundance",
        x = "Moon Illumination",
        y = "Nightly Abundance",
        color = "Year"
      ) +
      theme_moth()
    
  })
  
  # Phenology metrics table =============
  
  output$metrics_table <- renderTable({
    analysis()$phenology %>%
      mutate(
        first_appearance =
          as.character(first_appearance),
        peak_date =
          as.character(peak_date),
        last_appearance =
          as.character(last_appearance)
      )
  })
  
  output$aic_table <- renderTable({
    req(analysis()$aic_table)
    analysis()$aic_table
  })
  
  output$coef_table_all <- renderTable({
    req(analysis()$coef_table_all)
    analysis()$coef_table_all %>%
      mutate(
        p.value = signif(p.value, 3)
      )
  })
  
  output$coef_table_temp <- renderTable({
    req(analysis()$coef_table_temp)
    analysis()$coef_table_temp %>%
      mutate(
        p.value = signif(p.value, 3)
      )
  })
  
  output$coef_table_humid <- renderTable({
    req(analysis()$coef_table_humid)
    analysis()$coef_table_humid %>%
      mutate(
        p.value = signif(p.value, 3)
      )
  })
  
  output$coef_table_wind <- renderTable({
    req(analysis()$coef_table_wind)
    analysis()$coef_table_wind %>%
      mutate(
        p.value = signif(p.value, 3)
      )
  })
  
  output$coef_table_moon <- renderTable({
    req(analysis()$coef_table_moon)
    analysis()$coef_table_moon %>%
      mutate(
        p.value = signif(p.value, 3)
      )
  })
  
  # Environmental coefficients =============
  output$coef_table <- renderTable({
    req(analysis()$coef_table)
    analysis()$coef_table %>%
      mutate(
        p.value =
          signif(p.value, 3)
      )
  })
  
  # Plots =============
  output$temp_plot <- renderPlot({
    req(analysis()$coef_table_temp)
    pval <- analysis()$coef_table_temp$p.value[2]
    if (pval < 0.05) {
      nightly <- analysis()$nightly
      ggplot(
        nightly,
        aes(
          x = temp,
          y = abundance
        )
      ) +
        geom_point(
          size = 3,
          alpha = 0.7,
          color = "#0072B2"
        ) +
        geom_smooth(
          method = "glm",
          method.args = list(
            family = "poisson"
          ),
          se = TRUE
        ) +
        labs(
          title = "Temperature Effect",
          x = "Temperature",
          y = "Abundance"
        ) +
        theme_moth()
      
    } else {
      plot.new()
      text(
        0.5,
        0.5,
        "Temperature effect not significant"
      )
    }
  })
  
  output$humid_plot <- renderPlot({
    req(analysis()$coef_table_humid)
    pval <- analysis()$coef_table_humid$p.value[2]
    if (pval < 0.05) {
      nightly <- analysis()$nightly
      ggplot(
        nightly,
        aes(
          x = humid,
          y = abundance
        )
      ) +
        geom_point(
          size = 3,
          alpha = 0.7,
          color = "#0072B2"
        ) +
        geom_smooth(
          method = "glm",
          method.args = list(
            family = "poisson"
          ),
          se = TRUE
        ) +
        labs(
          title = "humidity Effect",
          x = "humidity",
          y = "Abundance"
        ) +
        theme_moth()
      
    } else {
      plot.new()
      text(
        0.5,
        0.5,
        "humidity effect not significant"
      )
    }
  })
  
  output$wind_plot <- renderPlot({
    req(analysis()$coef_table_wind)
    pval <- analysis()$coef_table_wind$p.value[2]
    if (pval < 0.05) {
      nightly <- analysis()$nightly
      ggplot(
        nightly,
        aes(
          x = wind,
          y = abundance
        )
      ) +
        geom_point(
          size = 3,
          alpha = 0.7,
          color = "#0072B2"
        ) +
        geom_smooth(
          method = "glm",
          method.args = list(
            family = "poisson"
          ),
          se = TRUE
        ) +
        labs(
          title = "wind Effect",
          x = "wind",
          y = "Abundance"
        ) +
        theme_moth()
      
    } else {
      plot.new()
      text(
        0.5,
        0.5,
        "wind effect not significant"
      )
    }
  })
  
  output$moon_plot <- renderPlot({
    req(analysis()$coef_table_moon)
    pval <- analysis()$coef_table_moon$p.value[2]
    if (pval < 0.05) {
      nightly <- analysis()$nightly
      ggplot(
        nightly,
        aes(
          x = moon_illumination,
          y = abundance
        )
      ) +
        geom_point(
          size = 3,
          alpha = 0.7,
          color = "#0072B2"
        ) +
        geom_smooth(
          method = "glm",
          method.args = list(
            family = "poisson"
          ),
          se = TRUE
        ) +
        labs(
          title = "moon phase Effect",
          x = "moon phase",
          y = "Abundance"
        ) +
        theme_moth()
      
    } else {
      plot.new()
      text(
        0.5,
        0.5,
        "moon phase effect not significant"
      )
    }
  })
}

# Run app -------

shinyApp(ui, server)