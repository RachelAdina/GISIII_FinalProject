library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(spData)
library(glue)
library(readr)

library(shiny)
library(ggmap)
library(leaflet)

setwd("C:/Users/rache/Documents/GitHub/GISIII_FinalProject/app_data")

#Note: the app doesn't render properly when running directly from the shapefiles. 
#I have recreated them in the code below. 
#Please see the attached .Rmd file for function annotations. 

#counties <- st_read('counties.shp')
#counties_merged <- st_read('counties_merged.shp')
#states_merged <- st_read('states_merged.shp')
#regions_merged <- st_read('regions_merged.shp')

od_cty = read_csv('od_cty.csv')
od_st = read_csv('od_st.csv')
od_reg = read_csv('od_reg.csv')


census_api_key("aeb3a2d13263357ddb5af381a90ce369a3e1a85f", install = TRUE, overwrite=TRUE)
counties <- get_acs(geography = 'county',
                    variables = 'B01001_001',
                    geometry = TRUE)

counties = counties %>%
    select(GEOID, NAME, geometry) %>%
    filter(!grepl('Puerto Rico', NAME)) %>%
    filter(!grepl('Alaska', NAME)) %>%
    filter(!grepl('Hawaii', NAME))

data(us_states)
crosswalk <- us_states %>% 
    st_drop_geometry() %>%
    select(GEOID, NAME, REGION) %>%
    rename(st_fips = GEOID)

counties = counties %>% separate(NAME, into = c('county', 'state'), sep=', ')
counties = left_join(counties, crosswalk, by=c('state'='NAME'))


years = c('1314', '1415', '1516', '1617', '1718')

load_inflows = function(name, yr){
    df = read_csv(name)
    df = subset(df, y2_countyfips!='000' & y1_statefips !='96')
    df['year'] <- yr
    df = df %>% 
        filter(grepl('Total Migration-US', y1_countyname)) %>%
        unite('geoid', y2_statefips:y2_countyfips, sep='', remove=TRUE) %>%
        select(geoid, year, n1) %>%
        rename(inflows = n1)
}

inflow_list <- list()
for(year in years){
    df = load_inflows(glue('inflow{year}.csv'), year)
    inflow_list[[year]] <- df
}

inflows = do.call('rbind', inflow_list)
substring(inflows$year, 1, 2) <- '20'


load_outflows = function(name, yr){
    df = read_csv(name)
    df = subset(df, y1_countyfips!='000' & y2_statefips !='96')
    df['year'] <- yr
    df = df %>% 
        filter(grepl('Total Migration-US', y2_countyname)) %>%
        unite('geoid', y1_statefips:y1_countyfips, sep='', remove=TRUE) %>%
        select(geoid, year, n1) %>%
        rename(outflows = n1)
}

outflow_list <- list()
for(year in years){
    df = load_outflows(glue('outflow{year}.csv'), year)
    outflow_list[[year]] <- df
}

outflows = do.call('rbind', outflow_list)
substring(outflows$year, 1, 2) <- '20'

net_join <- function(inflow, outflow, shape){
    df = inner_join(inflow, outflow, by=c('geoid', 'year'))
    df['net_migration'] = df$inflows - df$outflows
    shape_merge = left_join(shape, df, by=c('GEOID'='geoid'))
}

counties_merged = net_join(inflows, outflows, counties)

aggregate_geom = function(df, col){
    level <- enquo(col)
    new_df = df %>% group_by((!!level), year) %>%
        summarize(net_migrants = sum(net_migration, na.rm=TRUE))
}

states_merged = aggregate_geom(counties_merged, state)
regions_merged = aggregate_geom(counties_merged, REGION)


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