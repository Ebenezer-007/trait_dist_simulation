# Load required libraries
library(shiny)
library(shinyBS)
library(bs4Dash)
library(ggplot2)
library(dplyr)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = dashboardBrand(
    title = "Trait Dist. Simulator",
    image = "images/logo.jpg",
    opacity = 0.8
  )),
  dashboardSidebar(
    expandOnHover = TRUE,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Individual Simulation", tabName = "individual", icon = icon("chart-line")),
      menuItem("Combined Simulation", tabName = "combined", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            title = "Welcome to the Trait Distribution Simulator",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = FALSE,
            p("Explore the genetic basis of traits with our interactive simulator!"),
            p("Use the tabs to navigate between different types of simulations.")
          )
        ),
        fluidRow(
          box(
            title = "Individual Simulation",
            width = 4,
            solidHeader = TRUE,
            status = "info",
            collapsible = FALSE,
            icon = icon("chart-line"),
            p("Run simulations on individual genetic architectures to see how different factors influence trait distributions.")
          ),
          box(
            title = "Combined Simulation",
            width = 4,
            solidHeader = TRUE,
            status = "success",
            icon = icon("chart-bar"),
            collapsible = FALSE,
            p("Compare trait distributions under different genetic architectures in one combined view.")
          ),
          box(
            title = "Customizable Parameters",
            width = 4,
            solidHeader = TRUE,
            status = "warning",
            icon = icon("sliders-h"),
            collapsible = FALSE,
            p("Adjust the number of loci, individuals, and other parameters to tailor the simulation to your needs.")
          )
        )
      ),
      tabItem(
        tabName = "individual",
        fluidRow(
          box(
            title = "Input Parameters",
            width = 4,
            solidHeader = TRUE,
            status = "primary",
            
            bs4Dash::popover(numericInput("num_loci", "Number of Loci", value = 1000, min = 10, max = 10000),
                             title = "Number of Loci",
                             content = "The number of genetic loci (locations) considered 
                    in the simulation. More loci can lead to more complex trait 
                    distributions.",
                             placement = "right"),
            
            bs4Dash::popover(numericInput("num_individuals", "Number of Individuals", value = 1000, min = 100, max = 10000),
                             title = "Number of Individuals",
                             content = "The sample size for the simulation. Larger numbers provide 
                    more accurate distributions but may increase computation time.", 
                             placement = "right"),
            
            popover(selectInput("effect_size_dist", "Effect Size Distribution",
                                choices = c("Fisher" = "fisher", "Kimura" = "Kimura")),
                    title = "Effect Size Distribution",
                    content = "The model used to generate effect sizes for each locus. 
                    Fisher's model assumes many small effects, while Kimura's model 
                    allows for larger effect sizes.", 
                    placement = "right"),
            
            popover(selectInput("architecture", "Genetic Architecture",
                                choices = c("Additive", "Dominant", "Epistatic")), 
                    title = "Genetic Architecture",
                    content = "The way genes interact to produce the trait. 
                    Additive: effects sum linearly. Dominant: presence of an allele 
                    determines the effect. Epistatic: genes interact non-linearly.", 
                    placement = "right"),
            
            actionButton("run_simulation", "Run Simulation", class = "btn-primary")
            
          ),
          box(
            title = "Trait Distribution Plot",
            width = 8,
            solidHeader = TRUE,
            status = "info",
            plotOutput("trait_plot")
          )
        ),
        fluidRow(
          box(
            title = "Effect Size Distribution",
            width = 12,
            solidHeader = TRUE,
            status = "success",
            plotOutput("effect_size_plot")
          )
        )
      ),
      tabItem(
        tabName = "combined",
        fluidRow(
          box(
            title = "Input Parameters",
            width = 4,
            solidHeader = TRUE,
            status = "primary",
            
            popover(
              numericInput("combined_num_loci", "Number of Loci", value = 1000, min = 10, max = 10000),
              title = "Number of Loci",
              content = "The number of genetic loci used in all three simulations. This allows for direct comparison between different genetic architectures.",
              placement = "right"
            ),
            
            popover(
              numericInput("combined_num_individuals", "Number of Individuals", value = 1000, min = 100, max = 10000),
              title = "Number of Individuals",
              content = "The sample size used for all three simulations. Consistent sample size ensures fair comparison between architectures.",
              placement = "right"
            ),
            
            popover(
              selectInput("combined_effect_size_dist", "Effect Size Distribution",
                          choices = c("Fisher" = "fisher", "Kimura" = "Kimura")),
              title = "Effect Size Distribution",
              content = "The model used to generate effect sizes. This will be applied consistently across all three genetic architectures.",
              placement = "right"
            ),
            
            actionButton("run_combined_simulation", "Run Combined Simulation", class = "btn-primary")
          ),
          box(
            title = "Combined Trait Distribution Plot",
            width = 8,
            solidHeader = TRUE,
            status = "info",
            plotOutput("combined_trait_plot", height = "600px")
          )
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Function to generate effect sizes
  generate_effect_sizes <- function(n_variants = 50, effect_size_dist = "fisher") {
    if (effect_size_dist == "fisher") {
      effect_sizes <- rexp(n_variants, rate = 1)
      effect_sizes <- effect_sizes / max(effect_sizes)
    } else if(effect_size_dist == "Kimura") {
      effect_sizes <- runif(n_variants, min = 0.1, max = 1)
    }
    return(effect_sizes)
  }
  
  # Function to generate genotype data
  generate_genotype_data <- function(num_loci, num_individuals) {
    genotype_matrix <- matrix(sample(c(-1, 0, 1), num_individuals * num_loci, replace = TRUE), nrow = num_individuals)
    return(genotype_matrix)
  }
  
  # Function to simulate trait distribution
  simulate_trait_distribution <- function(num_loci, num_individuals, effect_sizes, architecture) {
    genotypes <- generate_genotype_data(num_loci, num_individuals)
    
    if (architecture == "Additive") {
      trait_values <- rowSums(genotypes * effect_sizes)
    } else if (architecture == "Dominant") {
      dominant_effect <- ifelse(genotypes != 0, 1, 0)
      trait_values <- rowSums(dominant_effect * effect_sizes)
    } else if (architecture == "Epistatic") {
      interaction_effect <- genotypes[,1] * genotypes[,2]
      trait_values <- rowSums(genotypes * effect_sizes) + interaction_effect * effect_sizes[1]
    }
    
    return(trait_values)
  }
  
  # Reactive values to store simulation results
  sim_results <- reactiveVal(NULL)
  combined_sim_results <- reactiveVal(NULL)
  
  # Run simulation when the button is clicked
  observeEvent(input$run_simulation, {
    effect_sizes <- generate_effect_sizes(input$num_loci, input$effect_size_dist)
    trait_values <- simulate_trait_distribution(input$num_loci, input$num_individuals, effect_sizes, input$architecture)
    
    sim_results(list(
      trait_values = trait_values,
      effect_sizes = effect_sizes
    ))
  })
  
  # Run combined simulation when the button is clicked
  observeEvent(input$run_combined_simulation, {
    effect_sizes <- generate_effect_sizes(input$combined_num_loci, input$combined_effect_size_dist)
    
    additive_traits <- simulate_trait_distribution(input$combined_num_loci, input$combined_num_individuals, effect_sizes, "Additive")
    dominant_traits <- simulate_trait_distribution(input$combined_num_loci, input$combined_num_individuals, effect_sizes, "Dominant")
    epistatic_traits <- simulate_trait_distribution(input$combined_num_loci, input$combined_num_individuals, effect_sizes, "Epistatic")
    
    combined_sim_results(list(
      Additive = additive_traits,
      Dominant = dominant_traits,
      Epistatic = epistatic_traits
    ))
  })
  
  # Render the trait distribution plot
  output$trait_plot <- renderPlot({
    req(sim_results())
    
    ggplot(data.frame(Trait = sim_results()$trait_values), aes(x = Trait)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = paste("Trait Distribution -", isolate({input$architecture}), "Architecture"),
           x = "Trait Value", 
           y = "Frequency") +
      theme(text = element_text(size = 14), axis.title = element_text(face = "bold"))
  })
  
  # Render the effect size distribution plot
  output$effect_size_plot <- renderPlot({
    req(sim_results())
    
    ggplot(data.frame(EffectSize = sim_results()$effect_sizes), aes(x = EffectSize)) +
      geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
      theme_minimal() +
      labs(title = paste("Effect Size Distribution -", isolate({input$effect_size_dist}), "Model"),
           x = "Effect Size", 
           y = "Frequency") +
      theme(text = element_text(size = 14), axis.title = element_text(face = "bold"))
  })
  
  # Render the combined trait distribution plot
  output$combined_trait_plot <- renderPlot({
    req(combined_sim_results())
    
    combined_data <- data.frame(
      Trait = c(combined_sim_results()$Additive, combined_sim_results()$Dominant, combined_sim_results()$Epistatic),
      Architecture = factor(rep(c("Additive", "Dominant", "Epistatic"), each = input$combined_num_individuals))
    )
    
    ggplot(combined_data, aes(x = Trait, fill = Architecture)) +
      geom_histogram(bins = 30, color = "black") +
      facet_wrap(~ Architecture, scales = "free_x", ncol = 1) +
      theme_minimal() +
      labs(title = "Trait Distributions under Different Genetic Architectures",
           x = "Trait Value", 
           y = "Frequency") +
      theme(text = element_text(size = 14), 
            axis.title = element_text(face = "bold"),
            strip.text = element_text(size = 16, face = "bold")) +
      scale_fill_manual(values = c("Additive" = "#FFA07A", "Dominant" = "#90EE90", "Epistatic" = "#87CEFA"))
  })
}

# Run the Shiny app
shinyApp(ui, server)