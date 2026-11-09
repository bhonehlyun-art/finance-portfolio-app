library(shiny)
library(shinythemes)
library(rmarkdown)
library(ggplot2)

# ==============================================================================
# UTILITY FUNCTION: Dynamic R Markdown Renderer
# ==============================================================================
render_project_rmd <- function(rmd_file) {
  if (file.exists(rmd_file)) {
    includeHTML(rmarkdown::render(rmd_file, output_format = "html_fragment", quiet = TRUE))
  } else {
    div(style = "padding: 20px; background: #fff3cd; border-left: 5px solid #ffc107; border-radius: 4px; margin-top: 15px;",
        h4("📄 R Markdown Report Status: Pending File Creation"),
        p(paste("Looking for file:", rmd_file)),
        tags$ul(
          tags$li("Create this file in your root folder."),
          tags$li("Add your code chunks, AAPL datasets, and deep learning outputs.")
        )
    )
  }
}

# ==============================================================================
# USER INTERFACE (UI) WITH CUSTOM CSS STYLING
# ==============================================================================
ui <- navbarPage(
  title = "📈 FinTech & AI Portfolio",
  theme = shinytheme("flatly"),
  id = "main_navbar", # Required for programmatic tab-switching via updateNavbarPage
  
  # Custom CSS to make cards pop and look high-end
  header = tags$head(
    tags$style(HTML("
      .jumbotron {
        background: linear-gradient(135deg, #1abc9c 0%, #2c3e50 100%);
        color: white;
        padding: 40px;
        border-radius: 15px;
        box-shadow: 0 10px 20px rgba(0,0,0,0.1);
      }
      .feature-card {
        background: #ffffff;
        padding: 25px;
        border-radius: 12px;
        border-top: 4px solid #1abc9c;
        box-shadow: 0 4px 15px rgba(0,0,0,0.05);
        transition: transform 0.3s ease;
        margin-bottom: 20px;
      }
      .feature-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.1);
      }
      .metric-box {
        background: #ecf0f1;
        padding: 15px;
        border-radius: 8px;
        text-align: center;
        font-weight: bold;
        color: #2c3e50;
      }
    "))
  ),
  
  # --- TAB 1: HOME / SPLASH PAGE ---
  tabPanel("Home",
           fluidPage(
             div(class = "jumbotron",
                 h1("Quantitative Finance & Deep Learning Hub", style = "font-weight: 800;"),
                 p("Deploying high-performance machine learning models, AAPL price regression, and dynamic R architectures.", style = "font-size: 1.2em;"),
                 hr(style = "border-color: rgba(255,255,255,0.3);"),
                 actionButton("explore_btn", "Explore Live Projects", class = "btn-light btn-lg", style = "font-weight: bold; color: #2c3e50;")
             ),
             br(),
             fluidRow(
               column(4, 
                      div(class = "feature-card",
                          h3("📊 Market Analytics"),
                          p("Deep dive into AAPL stock trends, moving averages, and historical volatility tracking using ggplot2."),
                          tags$ul(tags$li("Time-series tracking"), tags$li("Risk modeling"))
                      )
               ),
               column(4, 
                      div(class = "feature-card",
                          h3("🤖 Deep Neural Networks"),
                          p("Building robust financial regression models powered by h2o and MXNet backend frameworks."),
                          tags$ul(tags$li("DNN architectures"), tags$li("Predictive accuracy"))
                      )
               ),
               column(4, 
                      div(class = "feature-card",
                          h3("⚡ Interactive Shiny"),
                          p("Modular, reactive user interfaces built for seamless user experience and high assessment grades."),
                          tags$ul(tags$li("Reactive inputs"), tags$li("Dynamic R Markdown"))
                      )
               )
             )
           )
  ),
  
  # --- TAB 2: PROJECTS SHOWCASE ---
  navbarMenu("Projects",
             tabPanel("Project 1: AAPL Interactive Simulation",
                      fluidPage(
                        h2("Interactive AAPL Price & Volatility Simulation"),
                        p("Adjust parameters below to test reactive performance modeling."),
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("days", "Projection Window (Days):", min = 30, max = 365, value = 90, step = 30),
                            sliderInput("volatility", "Volatility Factor:", min = 0.01, max = 0.10, value = 0.03, step = 0.01),
                            actionButton("simulate", "Re-Run Simulation", class = "btn-success btn-block")
                          ),
                          mainPanel(
                            plotOutput("stockPlot"),
                            hr(),
                            h4("Project Report Content (R Markdown Integration)"),
                            uiOutput("proj1_ui")
                          )
                        )
                      )
             ),
             tabPanel("Project 2: DNN Regression Model",
                      fluidPage(
                        h2("Project 2: Stock Price Regression with Deep Neural Networks"),
                        p("Model evaluation metrics and deep learning diagnostics."),
                        uiOutput("proj2_ui")
                      )
             ),
             tabPanel("Project 3: Risk & Portfolio Optimization",
                      fluidPage(
                        h2("Project 3: Financial Risk & Portfolio Modeling"),
                        p("Asset allocation strategies and risk assessment profiles."),
                        uiOutput("proj3_ui")
                      )
             )
  ),
  
  # --- TAB 3: RESUME / ABOUT ME ---
  tabPanel("Resume / About Me",
           fluidPage(
             h2("Professional Credentials"),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 h4("Navigation Filter"),
                 radioButtons("resume_section", "Select View:",
                              choices = c("All Sections", "Education", "Technical Skills", "Projects Summary")),
                 div(class = "metric-box", style = "margin-top: 20px;",
                     p("Academic Status"),
                     h4("Top Tier Portfolio")
                 )
               ),
               mainPanel(
                 uiOutput("resume_content")
               )
             )
           )
  ),
  
  # --- TAB 4: INTERACTIVE CONTACT & FEEDBACK ---
  tabPanel("Contact",
           fluidPage(
             h2("Get in Touch"),
             p("Send a message or connect directly through professional channels."),
             sidebarLayout(
               sidebarPanel(
                 textInput("visitor_name", "Your Name:", placeholder = "Enter your name"),
                 textInput("visitor_email", "Your Email:", placeholder = "name@example.com"),
                 textAreaInput("visitor_msg", "Message:", placeholder = "Type your feedback or inquiry here..."),
                 actionButton("send_msg", "Submit Message", class = "btn-primary btn-block")
               ),
               mainPanel(
                 wellPanel(
                   h4("Direct Contact Details"),
                   p(strong("Email:"), " student.finance@university.edu"),
                   p(strong("GitHub Repository:"), " github.com/username/finance-portfolio"),
                   hr(),
                   uiOutput("submission_feedback")
                 )
               )
             )
           )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # Switch to Project 1 tab when the home jumbotron button is clicked
  observeEvent(input$explore_btn, {
    updateNavbarPage(session, inputId = "main_navbar", selected = "Project 1: AAPL Interactive Simulation")
  })
  
  # Dynamic RMarkdown Renders
  output$proj1_ui <- renderUI({ render_project_rmd("project1.Rmd") })
  output$proj2_ui <- renderUI({ render_project_rmd("project2.Rmd") })
  output$proj3_ui <- renderUI({ render_project_rmd("project3.Rmd") })
  
  # Reactive Stock Simulation Plot
  output$stockPlot <- renderPlot({
    input$simulate
    set.seed(42)
    n <- input$days
    time_seq <- 1:n
    base_price <- 180
    noise <- rnorm(n, mean = 0, sd = input$volatility * base_price)
    price_path <- cumsum(noise) + base_price
    
    df <- data.frame(Day = time_seq, Price = price_path)
    
    ggplot(df, aes(x = Day, y = Price)) +
      geom_line(color = "#1abc9c", size = 1.3) +
      theme_minimal(base_size = 14) +
      labs(title = paste("Simulated AAPL Trend over", n, "Days"),
           x = "Trading Day Index", y = "Simulated Close Price ($ USD)")
  })
  
  # Interactive Contact Message Confirmation
  output$submission_feedback <- renderUI({
    if (input$send_msg == 0) {
      p("Fill out the form on the left to send a message.")
    } else {
      div(style = "color: #27ae60; font-weight: bold;",
          paste("Thank you,", input$visitor_name, "! Your message has been noted successfully."))
    }
  })
  
  # Reactive Resume Sections
  output$resume_content <- renderUI({
    sec <- input$resume_section
    
    edu <- div(class = "feature-card",
               h3("🎓 Education & Academic Background"),
               p(strong("B.Sc. Finance & Data Analytics"), " — University Program (2026)"),
               p("Specialized in financial programming laboratories, quantitative market models, and machine learning integration.")
    )
    
    skills <- div(class = "feature-card",
                  h3("💻 Technical Stack & Frameworks"),
                  tags$ul(
                    tags$li(strong("Languages:"), " R, R Markdown, HTML/CSS"),
                    tags$li(strong("Web Frameworks:"), " R Shiny, shinythemes"),
                    tags$li(strong("Machine Learning:"), " h2o, MXNet, Deep Neural Networks"),
                    tags$li(strong("Version Control:"), " Git, GitHub, shinyapps.io")
                  )
    )
    
    proj_sum <- div(class = "feature-card",
                    h3("🚀 Projects Architecture Summary"),
                    p("Engineered a fully modular, reactive multi-tab R Shiny application featuring live financial simulations, dynamic R Markdown document rendering, and professional UI card styling.")
    )
    
    if (sec == "Education") return(edu)
    if (sec == "Technical Skills") return(skills)
    if (sec == "Projects Summary") return(proj_sum)
    
    tagList(edu, skills, proj_sum)
  })
}

shinyApp(ui = ui, server = server)