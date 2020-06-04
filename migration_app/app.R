library(shiny)
library(sf)
library(readr)
library(dplyr)
library(ggmap)

setwd("C:/Users/rache/Documents/GitHub/GISIII_FinalProject/app_data")
counties <- st_read('counties.shp')
counties_merged <- st_read('counties_merged.shp')
states_merged <- st_read('states_merged.shp')
regions_merged <- st_read('regions_merged')

od_cty = read_csv('od_cty.csv')
od_st = read_csv('od_st.csv')
od_reg = read_csv('od_reg.csv')


# Define UI 
ui <- fluidPage(
    
    # Application title
    titlePanel("United States Domestic Migration Patterns by Year"),
    
    # Sidebar with a slider input for years 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year",
                        min = 2014,
                        max = 2018,
                        step=1,
                        sep='',
                        value = 2018)
        ),
        
        # Show maps
        mainPanel(
            leafletOutput('county_map'),
            leafletOutput('state_map'),
            plotOutput('state_flows'),
            leafletOutput('region_map'),
            plotOutput('region_flows')
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    output$county_map <- renderLeaflet({
        
        counties_by_year <- filter(counties_merged,
                                   year == input$year)
        
        pal <- colorQuantile('RdYlBu', counties_by_year$net_migration, n=6)
        
        leaflet(data = counties_by_year) %>%
            addPolygons(fillColor = ~pal(net_migration),
                        weight=1,
                        opacity=1, 
                        color='black',
                        fillOpacity=0.8,
                        highlight = highlightOptions(
                            weight = 2,
                            color = 'white',
                            fillOpacity=0.9,
                            bringToFront=TRUE),
                        label = counties_by_year$net_migration) %>%
            addLegend(pal = pal, values = ~net_migration, opacity = 0.8, 
                      title='County Migrants', position='bottomright')
    })
    
    output$state_map <- renderLeaflet({
        
        states_by_year <- filter(states_merged,
                                 year == input$year)
        
        pal <- colorQuantile('RdYlBu', states_by_year$net_migrants, n=6)
        
        leaflet(data = states_by_year) %>%
            addPolygons(fillColor = ~pal(net_migrants),
                        weight=2,
                        opacity=1, 
                        color='black',
                        fillOpacity=0.8,
                        highlight = highlightOptions(
                            weight = 4,
                            color = 'white',
                            fillOpacity=0.9,
                            bringToFront=TRUE),
                        label = states_by_year$net_migrants) %>%
            addLegend(pal = pal, values = ~net_migrants, opacity = 0.8, 
                      title='State Migrants', position='bottomright')
    })
    
    output$state_flows <- renderPlot({
        state_base = counties %>% group_by(state) %>% summarize()
        
        st_yr = filter(od_st, 
                       year==input$year)
        
        p <- ggplot(data=state_base) +
            geom_sf(color='white', alpha=0.7, size = 0.2, fill='black') +
            geom_segment(data=st_yr,
                         aes(y=orig_lat, x=orig_lon, yend=dest_lat, xend=dest_lon, 
                             alpha=migrants, color=migrants), size=st_yr$migrants/20000) +
            scale_alpha_continuous(range=c(0.05, 1)) +
            scale_colour_gradient(low='#ffffb2', high='#f03b20') +
            theme_minimal() +
            theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  axis.ticks=element_blank(),
                  plot.title = element_text(hjust=0.5)) +
            ggtitle('State Level Outflows')
        print(p)
    })
    
    output$region_map <- renderLeaflet({
        
        regions_by_year <- filter(regions_merged,
                                  year == input$year)
        
        pal <- colorQuantile('RdYlBu', regions_by_year$net_migrants, n=4)
        
        leaflet(data = regions_by_year) %>%
            addPolygons(fillColor = ~pal(net_migrants),
                        weight=2,
                        opacity=1, 
                        color='black',
                        fillOpacity=0.8,
                        highlight = highlightOptions(
                            weight = 4,
                            color = 'white',
                            fillOpacity=0.9,
                            bringToFront=TRUE),
                        label = regions_by_year$net_migrants) %>%
            addLegend(pal = pal, values = ~net_migrants, opacity = 0.8, 
                      title='Region Migrants', position='bottomright')
    })
    
    output$region_flows <- renderPlot({
        reg_base = counties %>% group_by(REGION) %>% summarize()
        
        reg_yr = filter(od_reg, 
                        year==input$year)
        
        q <- ggplot(data=reg_base) +
            geom_sf(color='white', alpha=0.7, fill = 'black') +
            geom_segment(data=reg_yr,
                         aes(y=orig_lat, x=orig_lon, yend=dest_lat, xend=dest_lon, 
                             alpha=migrants, color=migrants),
                         size=reg_yr$migrants/100000) +
            scale_colour_gradient(low='#ffffb2', high='#f03b20') +
            geom_point(data=od_reg, aes(orig_lon, orig_lat), color='#31a354', size=1.5, name='origin') +
            geom_point(data=od_reg, aes(dest_lon, dest_lat), color='#bd0026', size=1.5, name='destination') +
            theme_minimal() +
            theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  axis.ticks=element_blank(),
                  plot.title = element_text(hjust=0.5)) +
            ggtitle('Region Level Outflows')
        print(q)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)