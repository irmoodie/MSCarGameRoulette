# LOAD LIBRARIES ------------------------------------
library(shiny)


# USER INTERFACE ------------------------------------
ui <- fluidPage(
    
    titlePanel("Microsoft Car Game Roulette"),
    
    
    sidebarLayout(
        sidebarPanel(
            textInput("players",
                         "Players (separated by commas):",
                         "PlayerA, PlayerB, PlayerC"),
            
            numericInput("rangeStart",
                         "Min Performance Index",
                         401),
            
            numericInput("rangeEnd",
                         "Max Performance Index",
                         900),
            
            textInput("tyres",
                      "Tyres (separated by commas):",
                      "Off-road, Drift, Stock, Race, Offroad, Drag"),
            
            textInput("races",
                      "Races (separated by commas):",
                      "Offroad, Rally, Road-race"),
            
            textInput("car_type",
                      "Vehicles (separated by commas, default is all):",
                      "Buggies, Classic Muscle, Classic Racers, Classic Rally, Classic Sports Cars, Cult Cars, Drift Cars, Extreme Track Toys, GT Cars, Hot Hatch, Hypercars, Modern Muscle, Modern Rally, Modern Sports Cars, Modern Supercars, Offroad, Pickups & 4x4's, Rally Monsters, Rare Classics, Retro Hot Hatch, Retro Muscle, Retro Rally, Retro Saloons, Retro Sports Cars, Retro Super Cars, Rods and Customs, Sports Utility Heroes, Super GT, Super Hot Hatch, Super Saloons, Track Toys, Trucks, Unlimited Buggies, Unlimited Offroad, UTV's, Vans & Utility, Vintage Racers"),
            
            br(),
            actionButton("do", "Generate setups"),
            actionButton("car", "Generate vehicle category"),
            actionButton("race", "Generate race category"),
        ),
        
        mainPanel(
            tableOutput("randNumbers"),
            tableOutput("randCar"),
            tableOutput("randRace")
        )
    )
)


# SERVER INFORMATION --------------------------------
server <- function(input, output) {
    
    # Use an action button as an event to generate the setups
    random_data <- eventReactive(input$do, {  
        
        numbers <- input$rangeStart:input$rangeEnd
        race_start <- sample(numbers, 1)
        
        if (race_start-round(race_start, digits = -2)>=0) {
            race_range <- ((round(race_start, digits = -2)):(round(race_start, digits = -2)+100))[2:101]
        } else {
            race_range <- ((round(race_start, digits = -2)-100):(round(race_start, digits = -2)))[2:101]
        }
        
        data.frame(
            player = unlist(strsplit(input$players, ",")),
            score = sample(race_range, length(unlist(strsplit(input$players, ","))), replace = TRUE),
            tyres = sample(unlist(strsplit(input$tyres, ",")), length(unlist(strsplit(input$players, ","))))
            )
        
    })
    
    random_race <- eventReactive(input$race, {
        data.frame(
            race_category = sample(unlist(strsplit(input$races, ",")), 1)
        )
    })
    
    random_car <- eventReactive(input$car, {
        data.frame(
            car_category = sample(unlist(strsplit(input$car_type, ",")), 1)
        )
    })
    
    output$randNumbers <- renderTable({
        random_data()
    }, rownames = FALSE, colnames = TRUE)
    
    output$randCar <- renderTable({
        random_car()
    }, rownames = FALSE, colnames = TRUE)
    
    output$randRace <- renderTable({
        random_race()
    }, rownames = FALSE, colnames = TRUE)
    
    
}

# RUN APPLICATION ----------------------------------
shinyApp(ui = ui, server = server)