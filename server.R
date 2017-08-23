
library(reshape)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(leaflet)
library(geosphere)
library(ggplot2)
library(plyr)
library(plotly)
library(parsetR)
library(networkD3)
library(circlize)
library(shiny)


server <- function(input, output, session) {
  

# read Migration data 
data_total <- read.csv(file = "UN_Migration_2015.csv", header = TRUE, sep = ",")

# read country coordinate data
countries <- read.csv(file = "country_coordinate.csv", header = TRUE, sep = ",")

wMap <- readOGR(dsn = path.expand("ne_110m_admin_0_countries"), layer = "ne_110m_admin_0_countries")
drops <- c("sovereignt", "iso_a2","pop_est", "gdp_md_est", "economy", "income_grp", "continent", "region_wb")
wMap@data[, ! (colnames(wMap@data) %in% drops)] <- NULL 


# clean country name in countries table
countries$newname <- chartr("'", " ", countries$COUNTRY_UN)
countries$newname <- chartr("(", " ", countries$newname)
countries$newname <- chartr(")", " ", countries$newname)
countries$newname <- chartr("-", " ", countries$newname)
countries$newname <- gsub("\\s","", chartr(",", " ", countries$newname))

# extract country data from Migration table
colnames(data_total)[2] <- 'destination' 
df_countries <- data_total[data_total$Country.code < 900,]
df_countries$destination <- as.character(df_countries$destination)
df_countries <- subset(df_countries, !is.na(X.))

# clean country name of destination in Migration table
df_countries$destination <- chartr( "(", " ", df_countries$destination )
df_countries$destination <- chartr( ")", " ", df_countries$destination )
df_countries$destination <- chartr( ",", " ", df_countries$destination )
df_countries$destination <- chartr( "'", " ", df_countries$destination )
df_countries$destination <- chartr( "-", " ", df_countries$destination )
df_countries$destination <- gsub("\\s","", chartr(",", " ", df_countries$destination))

# clean country name of column in Migration table
colnames(df_countries)[4:236] <- gsub("\\s","", chartr("."," ",colnames(df_countries)[4:236]) )

# search the country name in destination of df_countries but not in countries
dest_notmatch1 <- setdiff( df_countries$destination, countries$newname ) 
# search the country name in countries but not in destination of df_countries
dest_notmatch2 <- setdiff( countries$newname, df_countries$destination )  
# search the country name in column names of df_countries but not in countries
orig_notmatch1 <- setdiff( colnames(df_countries[4:235]), countries$newname )
# search the country name in countries but not in the colnames of df_countries
orig_notmatch2 <- setdiff( countries$newname, colnames(df_countries[4:235])) 

# replace the unmatched country name in destination of df_countries by those in countries
df_countries$destination[ df_countries$destination == dest_notmatch1[1] ] <- dest_notmatch2[1]
df_countries$destination[ df_countries$destination == dest_notmatch1[2] ] <- dest_notmatch2[2]

# replace the unmatched country name in column names of df_countries by those in countries
colnames(df_countries)[ colnames(df_countries) == orig_notmatch1[1] ] <- dest_notmatch2[2]
colnames(df_countries)[ colnames(df_countries) == orig_notmatch1[2] ] <- dest_notmatch2[1]
df_countries <- df_countries[ , -which(names(df_countries) %in% orig_notmatch1[3:4] )] 

# melt df_countries, make column name of origin country become values of a variable in df_countries
df_melted <- melt(df_countries, id.vars = c("X.", "destination", "Country.code", "datayear"), 
                  measure.vars = colnames(df_countries)[4:233],
                  value.name = "STOCK", na.rm=TRUE)

# merge df_countries and country coordinate table to assign latitude and longitude to destination country 
df_merged <- merge(df_melted, countries, by.x = "destination", by.y = "newname")
# rename the column of df_merged
colnames(df_merged)[7:10] <- c("Destination", "ISOCODE.d", "Lat.d","Lon.d")

# merge df_countries and country coordinate table to assign latitude and longitude to origin country 
df_merged <- merge(df_merged, countries, by.x = "variable", by.y = "newname")
colnames(df_merged)[11:14] <- c("Origin", "ISOCODE.o", "Lat.o","Lon.o")
colnames(df_merged)[colnames(df_merged) == "value"] <- "STOCK"

# remove unnecessary columns
df_merged <- within(df_merged, rm( variable ))
# df_merged <- within(df_merged, rm( destination ))
df_merged <- within(df_merged, rm( Country.code ))


# transform the value of Stock
df_merged$cubSTOCK <- (df_merged$STOCK)^(1/3)


# calculte total amount of immigration and emmigration for each country                                                             
emmi_sum <- aggregate(df_countries[,4:233],df_countries["datayear"],sum, na.rm = TRUE)
emmi_sum <- melt(emmi_sum, id = 'datayear')
names(emmi_sum)[c(2,3)] <- c("destination","emmi")
immi_sum <- df_countries[ , c ('destination','datayear') ]
immi_sum$immi <- rowSums(df_countries[ , ! names(df_countries) %in% c('destination','datayear')], na.rm = TRUE)
country_summ <- merge(emmi_sum, immi_sum, by = c('destination','datayear') ) 
df_merged_sum <- merge(df_merged, country_summ, by = c('destination','datayear'), all.x = TRUE)

# str(df_merged_sum)




#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Shiny  Part
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *



#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# Sankey diagram
reg_cou <- wdata[,c("iso_a2","region_wb","economy")]

G7 <-  reg_cou[ which(reg_cou["economy"] == "1. Developed region: G7"), "iso_a2"]
dvlped_noG7 <-  reg_cou[ which(reg_cou["economy"] == "2. Developed region: nonG7"), "iso_a2"]
BRIC <-  reg_cou[ which(reg_cou["economy"] == "3. Emerging region: BRIC"), "iso_a2"]
MIKT <-  reg_cou[ which(reg_cou["economy"] == "4. Emerging region: MIKT"), "iso_a2"]
G20 <- reg_cou[ which(reg_cou["economy"] == "5. Emerging region: G20"), "iso_a2"]
dvlping_latin <- reg_cou[ which(reg_cou["economy"] == "6. Developing region" & 
                             reg_cou["region_wb"] == "Latin America & Caribbean" ), "iso_a2"]
dvlping_africa <- reg_cou[ which(reg_cou["economy"] == "6. Developing region" & 
                                  (reg_cou["region_wb"] == "Sub-Saharan Africa" |
                                  reg_cou["region_wb"] == "Middle East & North Africa")), "iso_a2"]
dvlping_other <- setdiff( reg_cou[ which(reg_cou["economy"] == "6. Developing region"), "iso_a2"], dvlping_latin )
dvlping_other <- setdiff(dvlping_other, dvlping_africa)
  
least_dvlp <- reg_cou[ which(reg_cou["economy"] == "7. Least developed region"), "iso_a2"]
least_dvlp_africa <- reg_cou[ which(reg_cou["economy"] == "7. Least developed region" & 
                                   (reg_cou["region_wb"] == "Sub-Saharan Africa" |
                                      reg_cou["region_wb"] == "Middle East & North Africa")), "iso_a2"]
least_dvlp_other <- setdiff(least_dvlp, least_dvlp_africa)


dest_select  <- reactive({
                  validate(
                    need(!is.null(input$destin_select),
                         "Please select a region"))
              
              switch(input$destin_select,
                     
                     "Developed region: G7" = G7,
                     "Developed region: nonG7" = dvlped_noG7,
                     "Emerging region: BRIC" = BRIC,
                     "Emerging region: MIKT" = MIKT,
                     "Emerging region: G20" = G20,
                     "Developing region: Latin America & Caribbean" = dvlping_latin,
                     "Developing region: Africa" = dvlping_africa,
                     "Developing region: Other" = dvlping_other,
                     "Least developed region: Africa" = least_dvlp_africa,
                     "Least developed region: Other" = least_dvlp_other
                     
                     )
  
              })


ori_select  <- reactive({
          validate(
            need(!is.null(input$origin_select),
                 "Please select a region"))
          switch(input$origin_select,
                 
                 "Developed region: G7" = G7,
                 "Developed region: nonG7" = dvlped_noG7,
                 "Emerging region: BRIC" = BRIC,
                 "Emerging region: MIKT" = MIKT,
                 "Emerging region: G20" = G20,
                 "Developing region: Latin America & Caribbean" = dvlping_latin,
                 "Developing region: Africa" = dvlping_africa,
                 "Developing region: Other" = dvlping_other,
                 "Least developed region: Africa" = least_dvlp_africa,
                 "Least developed region: Other" = least_dvlp_other
      )
        
      })


year_select = 2010

# Load plot when open the app
df2 <- df_merged[((df_merged$datayear == 2015)
                  & (df_merged$ISOCODE.d %in% G7 )
                  & (df_merged$ISOCODE.o %in% G20  )),
                 c("Origin","Destination","STOCK")]
names(df2) <- c("Source", "Target", "STOCK")


output$sankey <- renderParset( {
  
  parset(df2, dimensions = c('Source','Target'), 
         value = htmlwidgets::JS("function(d){return d.STOCK}"), 
         tension = 0.5)
})

observeEvent(input$update, {
  
  df2 <- df_merged[((df_merged$datayear == year_select)
                    & (df_merged$ISOCODE.d %in% dest_select() )
                    & (df_merged$ISOCODE.o %in% ori_select()  )),
                   c("Origin","Destination","STOCK")]
  names(df2) <- c("Source", "Target", "STOCK")
  
  
  output$sankey <- renderParset( {
    
    parset(df2, dimensions = c('Source','Target'), 
           value = htmlwidgets::JS("function(d){return d.STOCK}"), 
           tension = 0.5)
    
  } )
  
})




#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# line chart: the change of immigration and emmigration of one country over years

#coun_code <- reactive( {input$select2} )
coun_data <- aggregate(cbind(emmi, immi) ~ ISOCODE.d+datayear+Destination, df_merged_sum, mean)

observeEvent(input$map_shape_click, { 
  p <- input$map_shape_click
  if(!is.null(p$id)){
    if(is.null(input$country) || input$country!=p$id)
      updateSelectInput(session,"country", selected=p$id)
  } })


observeEvent( input$country,{
      coun_code <- input$country
      output$trendPlot <- renderPlotly({
        coun_data <- coun_data[ coun_data$ISOCODE.d == coun_code, ]
        plot_ly(coun_data, x = ~datayear, y = ~emmi, name = 'Emmigrants', type = 'scatter', mode = 'lines+markers') %>%
          add_trace(y = ~immi, name = 'Immigrants', mode = 'lines+markers') %>%
          layout(title = "Immigrants and Emmigrants Stock during 1990 and 2015",
                 xaxis = list(title = "Year"),
                 yaxis = list (title = "Stock"),
                 showLegend = F )
        
      })
  }
)



#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# bump chart
coun_code = "AU"
choice = "ISOCODE.o"

t1 <- ""
bump_chart <- function (coun_code, choice) {  bump_df1 <- df_merged[ which(df_merged$ISOCODE.d == coun_code  |
                                     df_merged$ISOCODE.o == coun_code),
                             c("Destination","ISOCODE.d", "Origin", "ISOCODE.o","STOCK","datayear")]
                            bump_df2 <- bump_df1[ which(bump_df1[,choice] == coun_code), ]
                            t1 <- ""
                            if (choice == "ISOCODE.d"){
                              names(bump_df2)[names(bump_df2) == "Destination"] <- "thisCoun"
                              names(bump_df2)[names(bump_df2) == "Origin"] <- "otherCoun"
                              t1 <- paste0( "Source Countries of Immigrants of " ,  as.character( bump_df4[1,"thisCoun"] ) )
                            } else {
                              names(bump_df2)[names(bump_df2) == "Destination"] <- "otherCoun"
                              names(bump_df2)[names(bump_df2) == "Origin"] <- "thisCoun"
                              t1 <- paste0( "Destination Countries of Emmigrants from " ,  as.character( bump_df4[1,"thisCoun"] ) )
                            }
      
                            bump_df3 <- bump_df2 %>%
                              arrange(datayear, -STOCK) %>%
                              group_by(datayear) %>%
                              mutate(rank = row_number())
                            bump_df3 <- data.frame(bump_df3)
                            
                            rank_candidate <- unique( bump_df3[ bump_df3$rank < 21, "otherCoun" ])
                            rank_candidate <- factor(rank_candidate)
                            bump_df4 <- bump_df3[ bump_df3$otherCoun %in% rank_candidate ,]
                            
                            bump_df4$STOCK <- bump_df4$STOCK/100000 
                            return (bump_df4)
                             }


output$bumpchart <- renderPlot({
  
   bump_df4  <- bump_chart(input$country, input$radio)
   ggplot(bump_df4, aes(datayear, rank,
                       group = otherCoun, colour = otherCoun, 
                       label = otherCoun, size = bump_df4$STOCK)) +
    geom_line( lineend="round",linejoin="mitre") +
    geom_label(data = subset(bump_df4,datayear == input$year), size=3, 
               aes(x = datayear,  size = 3.5, hjust = 0.8)) + 
    labs(title = t1 )+
    theme_bw() + theme(legend.position = "none", panel.border = element_blank(),
                       axis.ticks = element_blank()) +
    scale_y_continuous(breaks = NULL,trans = "reverse", limits = c(30, 1))  
  
})

#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# chord diagram: migration flow across regions
world <- wMap@data[, c("iso_a2", "region_wb")] 
chord_df <-  merge(x = df_merged[,c( "STOCK", "Destination", "ISOCODE.d", "Origin", "ISOCODE.o","datayear")],
                   y = world, 
                   by.x = "ISOCODE.d", by.y = "iso_a2",
                   all.x=TRUE)
names(chord_df)[names(chord_df) == "region_wb"] <- "region.d"

chord_df <-  merge(x = chord_df,
                   y = world, 
                   by.x = "ISOCODE.o", by.y = "iso_a2",
                   all.x=TRUE)
names(chord_df)[names(chord_df) == "region_wb"] <- "region.o"

chord_df <- chord_df[complete.cases( chord_df[,c("region.d","region.o") ]), ]
chord_df2 <- aggregate(STOCK ~ region.d+region.o+datayear, chord_df, mean)

output$chord <- renderPlot({
  
  
  chordDiagram(chord_df2[chord_df2$datayear == input$year,-3] ,directional = 1,
               direction.type = c("diffHeight", "arrows"), 
               link.arr.type = "big.arrow", diffHeight = -0.04)
})


  
  
  
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# choropleth


df_agg <- aggregate(cbind(emmi, immi) ~ ISOCODE.d+datayear, df_merged_sum, mean)

    year_select = 1990
    data_select = "Migration Amount"
    df_agg = df_agg[df_agg$datayear == year_select,]
    colnames(df_agg)[1] <- "iso_a2"
    wMap@data <- left_join( wMap@data, df_agg )

    bins1 <- quantile(wMap@data$emmi, c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, 1 ), na.rm = TRUE) 
    names(bins1) <- NULL
    bins1 <- round( c(0, bins1) )
    pal1 <- colorBin("Purples", domain = wMap$emmi, bins = bins1)

    bins2 <- quantile(wMap@data$immi, c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, 1 ), na.rm = TRUE) 
    names(bins2) <- NULL
    bins2 <- round( c(0, bins2) )
    pal2 <- colorBin("Reds", domain = wMap$immi, bins = bins2)


    labels1 <- sprintf(
      "<span style='color: Purple;'> <strong>%s</strong> </span>
      <span style='color: Blue;'><br/> %s </span>
      <br/>%g Thousand",
      data_select, wMap$sovereignt, wMap$emmi/1000 
    ) %>% lapply(htmltools::HTML)
    
    
    labels2 <- sprintf(
      "<span style='color: Green;'> <strong>%s</strong> </span>
      <span style='color: Blue;'><br/> %s </span>
      <br/>%g Thousand",
      data_select, wMap$sovereignt, wMap$immi/1000 
    ) %>% lapply(htmltools::HTML)


output$map <- renderLeaflet({
  
  chrpl <- leaflet(
    options = leafletOptions(minZoom = 1, maxZoom = 4 )) %>% 
    addProviderTiles(providers$Esri.NatGeoWorldMap,
                     options = providerTileOptions(opacity = 0.4)) %>%
    addPolygons(data = wMap, 
                #fillColor = ~qpal(emmi), 
                fillOpacity = 0.8, 
                #color = ~qpal(emmi), 
                fillColor = ~pal1(emmi),
                #layerId = ~ iso_a2,
                weight = 1,
                opacity = 1,
                dashArray = "3",
                group = "Emmigrant",
                highlight = highlightOptions(
                  weight = 2,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = FALSE),
                label = labels1,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    #%>%
    #addLegend(pal = pal, values = ~emmi, opacity = 0.7, title = "unit: Thousand",
    #          position = "bottomright")  %>%
    
    addPolygons(data = wMap, 
                #fillColor = ~qpal(emmi), 
                fillOpacity = 0.8, 
                #color = ~qpal(emmi), 
                fillColor = ~pal2(immi),
                layerId = ~ iso_a2,
                weight = 1,
                opacity = 1,
                dashArray = "3",
                group = "Immigrant",
                highlight = highlightOptions(
                  weight = 2,
                  color = "#333",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = FALSE),
                label = labels2,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))
  
  
})



# %>% addLegend(pal = pal, values = ~emmi, opacity = 0.7, title = "unit: Thousand",
#            position = "bottomright") 





#colors= c(”#1DCEC6″,”#094AA5″,”#0181FD”,”white”,”#92B966″ )



#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
# great circle 

year = 2015
country = "AU"

flowss <- reactive({ df_merged.sub <- df_merged[which(df_merged$datayear == input$year &
                                        (df_merged$ISOCODE.d == input$country |
                                           df_merged$ISOCODE.o == input$country)),]
        
        df_merged.sub <- df_merged.sub[df_merged.sub$STOCK > 0,]
        df_merged.sub <- df_merged.sub[complete.cases(df_merged.sub),]
        df_merged.sub$distin <- "0"
        df_merged.sub[df_merged.sub$ISOCODE.d == input$country,]$distin <- "1"
        df_merged.sub$distin <- as.character(df_merged.sub$distin)
        
        a <- df_merged.sub[order(df_merged.sub$STOCK, decreasing = TRUE ),]
        b <- by(a, a['distin'], head, n = 20)
        df_merged.sub <- Reduce(rbind, b)
        
        flowss <- gcIntermediate(df_merged.sub[,c("Lon.o","Lat.o")], 
                                df_merged.sub[,c("Lon.d","Lat.d")],  
                                arc.nombre,
                                breakAtDateLine = TRUE, addStartEnd = TRUE, sp = TRUE)
        flowss$STOCK <- (df_merged.sub$STOCK)
        flowss$qSTOCK <- (df_merged.sub$STOCK)^(1/3) / 6
        flowss$origin <- df_merged.sub$Origin
        flowss$destination <- df_merged.sub$Destination
        flowss$distin <- df_merged.sub$distin
        return(flowss)
        }
      )

#df_flowss <- fortify.SpatialLinesDataFrame(flowss)



hover <- reactive({  paste0(flowss()$origin, " to ", 
                                    flowss()$destination, ': ', 
                                    as.character(flowss()$STOCK)) }
  )

 
pal3 <- reactive( { 
                   pal3 <- colorFactor(brewer.pal(12, 'Set3'), flowss()$distin) 
                   pal3 
                   } )
  #return(pal3)

c <- as.character(seq(1:40))
observe({
  flowss <- flowss()
  hover <- hover()
  pal3 <- pal3()
  if(is.null(flowss))
    return()
  if(is.null(hover))
    return()
  if(is.null(pal3))
    return()
  
  leafletProxy("map") %>%
    addPolylines(data = flowss, weight = ~qSTOCK, label = hover,color = ~pal3(distin), 
                  group = "flow", layerId = c ) %>%
    addLayersControl(
      baseGroups = c("Emmigrant", 
                     "Immigrant"),
      overlayGroups = c("flow"),
      options = layersControlOptions(collapsed = FALSE))
  
  
})
  



  
  #%>%
  # addLayersControl(overlayGroups = unique(flowss$destination), 
  #                 options = layersControlOptions(collapsed = FALSE))


#observeEvent(input$Map_shape_click, { # update the location selectInput on map clicks
#    p <- input$Map_shape_click
    #print(p)
 # })
  


observe ({
    click <- input$map_shape_click
    if(is.null(click))
      return()
    p <- input$map_shape_click
    print(p$id)
    
    #text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng, "ID", click$id)
    #text2<-paste("You've selected point ", click$id)
    #map$clearPopups()
    #map$showPopup( click$lat, click$lng, text)
    #output$Click_text<-renderText({
    #  text2
    #})
})

wdata <- wMap@data
wdata$sovereignt <- as.character(wdata$sovereignt) 
coun_select <- setNames(wdata$iso_a2, wdata$sovereignt)





}



