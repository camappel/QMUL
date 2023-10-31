library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)

colors <- c(
    "1" = "red",
    "2" = "lightseagreen",
    "3" = "purple",
    "4" = "yellow",
    "5" = "darkslategray"
)

nhs_payments <- read.csv("../Analysis/Data/nhs_payments.csv")

ui <- fluidPage(
    titlePanel("NHS Payments Plot"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "icb",
                label = "Select ICB:",
                choices = unique(nhs_payments$ICB.NAME)
            )
        ),
        mainPanel(
            plotOutput(outputId = "plot", width = "100%", height = "600px")
        )
    )
)

server <- function(input, output, session) {
    plot_reactive <- reactive({
        # Select the data for the selected ICB
        agg <- nhs_payments[nhs_payments$ICB.NAME == input$icb, ] %>%
            group_by(Year, IMD_decile) %>%
            summarise(
                Number.of.Registered.Patients..Last.Known.Figure. = sum(Number.of.Registered.Patients..Last.Known.Figure., na.rm = TRUE),
                Number.of.Weighted.Patients..Last.Known.Figure. = sum(Number.of.Weighted.Patients..Last.Known.Figure., na.rm = TRUE),
                Total.NHS.Payments.to.General.Practice = sum(Total.NHS.Payments.to.General.Practice, na.rm = TRUE),
                Deductions.for.Pensions..Levies.and.Prescription.Charge.Income = sum(Deductions.for.Pensions..Levies.and.Prescription.Charge.Income, na.rm = TRUE),
                Total.NHS.Payments.to.General.Practice.Minus.Deductions = sum(Total.NHS.Payments.to.General.Practice.Minus.Deductions, na.rm = TRUE)
            )

        agg <- agg %>% mutate(
            Average_check_registered = Total.NHS.Payments.to.General.Practice.Minus.Deductions / Number.of.Registered.Patients..Last.Known.Figure.,
            Average_check_weighted = Total.NHS.Payments.to.General.Practice.Minus.Deductions / Number.of.Weighted.Patients..Last.Known.Figure.
        )

        # make IMD_decile a factor
        agg$IMD_decile <- factor(agg$IMD_decile, levels = c("5", "4", "3", "2", "1"))

        # Create the line graph
        agg[!is.na(agg$IMD_decile), ] %>%
            ggplot(aes(x = Year, y = Average_check_weighted, group = IMD_decile, colour = IMD_decile)) +
            geom_line() +
            geom_point() +
            labs(x = "", y = "", title = "Net NHS payments per weighted patient by IMD quintile") +
            theme_minimal() +
            theme(legend.position = "top") +
            scale_color_manual(values = colors, labels = c("Q5 (least deprived)", "Q4", "Q3", "Q2", "Q1 (most deprived)"))
    })

    # Render the plot when the ICB selection changes
    output$plot <- renderPlot({
        plot_reactive()
    })
}

shinyApp(ui, server)
