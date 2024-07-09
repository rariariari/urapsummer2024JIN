library(magrittr)
library(dplyr)
library(data.table)
library(leaflet)
library(haven)
library(ggplot2)
library(cowplot)
library(scales)
library(sf)
library(shiny)
# options(warn=-1)
options(shiny.maxRequestSize=500*1024^2)

cbsa_geoms <- readRDS("usashapefiles/cbsa/cbsa_geoms.rsd") 
states <- readRDS("usashapefiles/states/states.rsd")
cbsa_geomsHighres <- readRDS("usashapefiles/cbsa/cbsa_geomsHighres.rsd")
statesHighres <- readRDS("usashapefiles/states/statesHighres.rsd")

#placeholder values
schools <- c("None")
fields <- c("All")
all_metros <- data.frame(metro_area=("None"))
full_level_dataALL <- NULL
full_level_dataBACH <- NULL

coord_pattern <-"^\\s*(-?\\d+\\.\\d+[NS])\\s*(-?\\d+\\.\\d+[NS])\\s*(-?\\d+\\.\\d+[EW])\\s*(-?\\d+\\.\\d+[EW])$"
popupPaste <- "<style> div.leaflet-popup-content-wrapper {width:100% !important;}</style>"

crosswalk <- fread("schoolnetworkdata/crosswalk.csv")

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Define onRender as to not load the full htmlwidgets package
addHook <- function (x, hookName, jsCode, data = NULL) 
{
  if (length(jsCode) == 0) 
    return(x)
  if (length(jsCode) > 1) 
    jsCode <- paste(jsCode, collapse = "\n")
  x$jsHooks[[hookName]] <- c(x$jsHooks[[hookName]], list(list(code = jsCode, 
                                                              data = data)))
  x
}
onRender <- function (x, jsCode, data = NULL) {addHook(x, "render", jsCode, data)}


# Palette function to color the maps,
pal2 <- function(data,colors){
  colorNumeric(palette = colors,domain = c(-max(abs(data)),0,max(abs(data))))}

# This function is called only in the genfiltered_data function
# It saves old values for inspection in the map's clickable popups
saveOldStats <- function(df,filt_data){
  df %>%
    mutate(old_N_uw1 = N_uw,
           old_N_uw2 = filt_data$N_uw,
           old_N_w1 = N_w,
           old_N_w2 = filt_data$N_w,
           old_logsize_uw1 = logsize_uw,
           old_logsize_uw2 = filt_data$logsize_uw,
           old_logsize_w1 = logsize_w,
           old_logsize_w2 = filt_data$logsize_w)
}

# This is also a function only called in genfiltered_data
# It does most of the data cleaning, and appends the appropriate geometry
# Highres geoms for creating images, lowres for the interactive map
filterFieldSum <- function(df,input,res) {
  df %>%
    {`if`(input$field=='All',.,
          filter(.,field==input$field))} %>%
    {`if`(nrow(.)==0,
          .,
          mutate(.,metro_area =
                   case_when(
                     metro_area=="connecticut nonmetropolitan area" ~ "hartford metropolitan area",
                     # metro_area=="delaware nonmetropolitan area" ~ "philadelphia metropolitan area",
                     # metro_area=="new jersey nonmetropolitan area" ~ "new york city metropolitan area",
                     metro_area=="rhode island nonmetropolitan area" ~ "providence metropolitan area",
                     metro_area=="anaheim metropolitan area" ~ "los angeles metropolitan area",
                     metro_area=="fort lauderdale metropolitan area" ~ "miami metropolitan area",
                     metro_area=="fort worth metropolitan area" ~ "dallas metropolitan area",
                     metro_area=="long beach metropolitan area" ~ "los angeles metropolitan area",
                     metro_area=="riverside metropolitan area" ~ "los angeles metropolitan area",
                     metro_area=="tacoma metropolitan area" ~ "seattle metropolitan area",
                     metro_area=="west palm beach metropolitan area" ~ "miami metropolitan area",
                     metro_area=="" ~ NA,
                     .default=as.character(metro_area)))
    )} %>%
    group_by(metro_area,instnm) %>%
    reframe(N_uw = sum(N_uw),
            N_w = sum(N_w),
            logsize_uw = ifelse(sum(N_uw)==0,0,
                                log(sum(N_uw))),
            logsize_w = ifelse(sum(N_w)==0,0,
                               log(sum(N_w)))) %>%
    full_join(x=all_metros,by='metro_area') %>%
    mutate(CBSAFP=metro_area) %>%
    {`if`(res=="high",
          full_join(x=cbsa_geomsHighres %>% select(CBSAFP,long,lat,NAMELSAD),
                    y=.,by='CBSAFP'),
          full_join(x=cbsa_geoms %>% select(CBSAFP,long,lat,NAMELSAD),
                    y=.,by='CBSAFP'))} %>%
    {`if`(input$excludemetro,
          filter(.,!grepl("nonmetropolitan", CBSAFP)),
          .)} %>%
    mutate(field=input$field,
           N_uw = ifelse(is.na(metro_area) | is.na(N_uw),
                         0,N_uw),
           N_w = ifelse(is.na(metro_area) | is.na(N_w),
                        0,N_w),
           logsize_uw = ifelse(is.na(metro_area) | is.na(logsize_uw),
                               0,logsize_uw),
           logsize_w = ifelse(is.na(metro_area) | is.na(logsize_w),
                              0,logsize_w),
           instnm=input$school) %>%
    st_as_sf()
}

# This function provides the map data for all modules
# It checks which module called it and manipulates accordingly
genfiltered_data <- function(input,res){
  if(input$bach){datums <- full_level_dataBACH}
  else{datums <- full_level_dataALL}
  filt_data <- datums %>%
    filter(instnm==input$school1)  %>%
    filterFieldSum(input=input,res=res)
  if(input$diff=="Simple"){
    cbsa_level_data <- datums %>%
      filter(instnm==input$school2) %>%
      filterFieldSum(input=input,res=res) %>%
      saveOldStats(filt_data=filt_data) %>%
      mutate(N_uw = N_uw-filt_data$N_uw,
             N_w = N_w-filt_data$N_w,
             logsize_uw = logsize_uw-filt_data$logsize_uw,
             logsize_w = logsize_w-filt_data$logsize_w)
  }
  else if(input$diff=="log"){
    cbsa_level_data <- datums %>%
      filter(instnm==input$school2) %>%
      filterFieldSum(input=input,res=res) %>%
      saveOldStats(filt_data=filt_data) %>%
      mutate(N_uw =
               ifelse(N_uw-filt_data$N_uw>=0,
                      log(N_uw-filt_data$N_uw),
                      -log(abs(N_uw-filt_data$N_uw))),
             N_w =
               ifelse(N_w-filt_data$N_w>=0,
                      log(N_w-filt_data$N_w),
                      -log(abs(N_w-filt_data$N_w))),
             logsize_uw =
               ifelse(logsize_uw-filt_data$logsize_uw>=0,
                      log(logsize_uw-filt_data$logsize_uw),
                      -log(abs(logsize_uw-filt_data$logsize_uw))),
             logsize_w =
               ifelse(logsize_w-filt_data$logsize_w>=0,
                      log(logsize_w-filt_data$logsize_w),
                      -log(abs(logsize_w-filt_data$logsize_w)))) %>%
      mutate(N_uw =
               ifelse(N_uw=="-Inf",0,N_uw),
             N_w =
               ifelse(N_w=="-Inf",0,N_w),
             logsize_uw =
               ifelse(logsize_uw=="-Inf",0,logsize_uw),
             logsize_w =
               ifelse(logsize_w=="-Inf",0,logsize_w))
  }
  else{
    cbsa_level_data <- datums %>%
      filter(instnm==input$school2) %>%
      filterFieldSum(input=input,res=res) %>%
      saveOldStats(filt_data=filt_data) %>%
      mutate(N_uw =
               ifelse(N_uw+filt_data$N_uw==0,
                      0,
                      (N_uw-filt_data$N_uw)/(N_uw+filt_data$N_uw)),
             N_w =
               ifelse(N_w+filt_data$N_w==0,
                      0,
                      (N_w-filt_data$N_w)/(N_w+filt_data$N_w)),
             logsize_uw =
               ifelse(logsize_uw+filt_data$logsize_uw==0,
                      0,
                      (logsize_uw-filt_data$logsize_uw)/(logsize_uw+filt_data$logsize_uw)),
             logsize_w =
               ifelse(logsize_w+filt_data$logsize_w==0,
                      0,
                      (logsize_w-filt_data$logsize_w)/(logsize_w+filt_data$logsize_w)))
  }
  return(cbsa_level_data)
}

# This function generates the base leaflet map for all modules
gen_basemap <- function(){
  leaflet(options = leafletOptions(attributionControl=FALSE,worldCopyJump = TRUE,
  )) %>%
    addMapPane(name = "polygons", zIndex = 410) %>% 
    addMapPane(name = "maplabels", zIndex = 420) %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addProviderTiles("CartoDB.PositronOnlyLabels",
                     options = leafletOptions(pane = "maplabels"),
                     group = "map labels") %>%
    addLayersControl(.,overlayGroups = c("map labels")) %>%
    addControl(html = paste0("<input id=\"CBSAOpacitySlide\" type=\"range\" min=\"0\" max=\"2.4\" step=\"0.1\" value=\"1\">"),
               position="bottomleft") %>%
    onRender(
      paste0("function(el, x, data) {
        var map = this;
        var layerName = 'zones';
        var slider = $('#CBSAOpacitySlide');
        var initialOpacity = {};
        var container = $('<div class=\"slider-container\"></div>');
        slider.wrap(container);
        var title = $('<div id=\"OpacitySliderTitle\">Adjust opacity multiplier:</div>');
        slider.before(title);

        var storeInitialOpacity = function() {
            var layer = map.layerManager._byGroup[layerName];
            Object.keys(layer).forEach(function(el) {
                if (layer[el] instanceof L.Polygon) {
                    initialOpacity[layer[el]._leaflet_id] = layer[el].options.fillOpacity;
                }
            });
        };

        var evthandler = function(e) {
            var multiplier = +e.target.value;
            var layer = map.layerManager._byGroup[layerName];
            Shiny.setInputValue('opacityMultiplier',multiplier);
            Object.keys(layer).forEach(function(el) {
                if (layer[el] instanceof L.Polygon) {
                    var initialOp = initialOpacity[layer[el]._leaflet_id];
                    layer[el].setStyle({fillOpacity: initialOp * multiplier});
                    Shiny.setInputValue('opacityMultiplier', multiplier);
                }
            });
        };

        map.on('layeradd', function(e) {
            if (e.layer.options && e.layer.options.group === layerName) {
                storeInitialOpacity();
            }
        });

        slider.mousedown(function() { map.dragging.disable(); });
        slider.mouseup(function() { map.dragging.enable(); });
        slider.on('input', evthandler);
      
    }")
    ) %>%
    onRender("function(el, x) {
    var map = this;
    map.removeControl(map.zoomControl);
  }")
}

# This function capitalizes words for use in legends
capitalize_words <- function(input) {
  text<- gsub("(.*?)(metropolitan|nonmetropolitan).*", "\\1", input$exclude, ignore.case = TRUE)
  words <- unlist(strsplit(text, " "))
  capitalized_words <- paste0(toupper(substring(words, 1, 1)), tolower(substring(words, 2)))
  paste(capitalized_words, collapse = " ")
}

# This function fetch user inputs to draw and return a downloadable map
fetch_usermap<- function(input){
  infos <- genfiltered_data(input,"high")
  m <- ggplot() +
    theme_void()
  name <- capitalize_words(input)
  
  if(input$data!='None' & input$school1!="None" & input$school2!="None"){
    if(input$exclude!="None"){
      m <- m +
        geom_sf(data=infos %>%
                  filter(metro_area==input$exclude),
                fill="grey30",
                aes(alpha=0.4*input$opacityMultiplier)) +
        scale_alpha("",labels=name)
      infos <- infos %>%
        filter(metro_area!=input$exclude)
    }
    infos <- eval(parse(text=paste0("arrange(infos,", input$data,")")))
    infos_col <- eval(parse(text=paste0("infos$", input$data)))
    colorpalRange <- seq(-max(abs(infos_col)),max(abs(infos_col)),length=5000)
    m <- m +
      geom_sf(data=infos,
              aes(fill=infos_col,color=infos_col),
              alpha = 0.4*input$opacityMultiplier,
      ) +
      scale_fill_gradientn(values =rescale(colorpalRange),
                           colors=
                             pal2(colorpalRange,input$color)(colorpalRange),
                           name=input$imgLegend,
                           limits=c(-max(abs(infos_col)),max(abs(infos_col)))) +
      scale_color_gradientn(values =rescale(colorpalRange),
                           colors=
                             pal2(colorpalRange,input$color)(colorpalRange),
                           name=input$imgLegend,
                           limits=c(-max(abs(infos_col)),max(abs(infos_col))))
  }
  m <- m +
    geom_sf(data=statesHighres,color="black",fill=NA)
  alaska <- m +
    coord_sf(crs = 3467, 
             xlim = c(-2400000, 1600000), 
             ylim = c(200000, 2500000)) + guides(fill="none",
                                                 color="none",
                                                 alpha="none")
  hawaii <- m +
    coord_sf(xlim=c(-167,-154),ylim=c(18,24)) + guides(fill="none",
                                                       color="none",
                                                       alpha="none")
  combined <- ggdraw(m + coord_sf(xlim=c(-123,-69),ylim=c(25,49))) +
    theme(panel.background =
            element_rect(fill = input$imgBackCol,
                         colour = input$imgOutlCol,
                         linewidth= input$imgOutlSize))
  if(!input$omitAlaska){
    combined <- combined + draw_plot(alaska,0,0.05,0.25,0.25)
  }
  if(!input$omitHawaii){
    combined <- combined + draw_plot(hawaii,0.2,0.1,0.15,0.15)
  }
  return(combined)}

# This is the basic UI module for the two-school comparison tool
ui <-
  fillPage(
    tags$head(tags$style(
      HTML('
             #input_date_control {background-color: rgba(0,0,255,0.2);;}
             #sel_date {background-color: rgba(0,0,255,1);}')
    )),
    tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  #   tags$style(HTML("
  #   .leaflet-bottom.leaflet-left {
  #       visibility: hidden;
  #   }
  # ")),
    leafletOutput("map", width = "100%", height = "100%"),
    conditionalPanel(condition="1==0",numericInput("opacityMultiplier","",1)),
    conditionalPanel(condition="!output.filesUploaded",
                     fluidPage(
                       tags$style(HTML("
                          .uploadprompter-panel {
                            position: absolute;
                            top: 0;
                            left: 0;
                            width: 100%;
                            height: 100%;
                            display: flex;
                            justify-content: center;
                            align-items: center;
                            background-color: #f9f9f9;
                            z-index: 2147483000;
                          }
                          
                          .panel-content {
                            text-align: center;
                            padding: 20px;
                          }
                        ")),
                       div(class = "uploadprompter-panel",
                           div(class = "panel-content",
                               fluidRow(verbatimTextOutput("startupmessage")),
                               fluidRow(
                                 column(6,
                                        fileInput("file1", "Choose all degree data:",
                                                  accept = c(".csv",".dta"))),
                                 column(6,
                                        fileInput("file2", "Choose bachelor-only data:",
                                                  accept = c(".csv",".dta")))
                               ),
                               fluidRow(verbatimTextOutput("readingmessage"))
                           ))
                     )
    ),
    conditionalPanel(condition="output.filesUploaded",
                     absolutePanel(
                       fluidRow(
                         column(2,
                                selectInput("data", "Select Statistic:",
                                            choices = c("None", "N_uw", "N_w","logsize_uw", "logsize_w"))),
                         column(2,
                                selectizeInput("field","Select field:", choices = fields)),
                         column(2,
                                selectizeInput("school1", "Select School 1: (bottom of legend)",
                                               choices = NULL)
                         ),
                         column(2,
                                selectizeInput("school2", "Select School 2: (top of legend)",
                                               choices = NULL)
                         ),
                         column(2,
                                checkboxInput("excludemetro","Exclude nonmetro",value=TRUE),
                                checkboxInput("showStates","State lines",value=FALSE)
                         )
                       ),
                       top="10px",left="15px",width="88%",height="5%",style = "z-index: 10000000000;"
                     ),
                     absolutePanel(
                       tags$head(
                         tags$style(HTML("
      .dropdown {
        position: relative;
        display: inline-block;
      }
      .dropdown-content {
        display: none;
        position: absolute;
        background-color: #f9f9f9;
        min-width: 200px;
        box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
        z-index: 1;
        padding: 12px;
        right: 0;
      }
      .dropdown-content input, .dropdown-content select {
        margin-bottom: 10px;
        width: 100%;
      }
      .dropdown:hover .dropdown-content {
        display: block;
      }
    "))
                       ),
                       div(class = "dropdown",
                           actionButton("dropbtn", NULL, icon=icon("cog", lib = "glyphicon")),
                           div(class = "dropdown-content",
                               selectInput("color","Select color theme:",
                                           choices = c('RdBu',"PiYG","PuOr","PRGn","viridis","plasma")),
                               selectInput("diff","Select differencing method:",
                                           choices = c('Simple','Normalized (-1 to +1)',"log")),
                               selectizeInput("exclude",
                                              label = "Exclude area:", 
                                              choices =NULL),
                               checkboxInput("bach","Bachelor's Only")
                           )
                       ),
                       top="65px",right="40px",width="10px",height="10px",style = "z-index: 10000;"
                     ),
                     absolutePanel(
                       tags$head(
                         tags$style(HTML("
      .infodropdown {
        position: relative;
        display: inline-block;
      }
      .infodropdown-content {
        pointer-events: none;
        visibility: hidden;
        position: absolute;
        background-color: none;
        min-width: 200px;
        z-index: 1;
        padding: 0px;
        left: 0;
        bottom: 100%;
      }
      .infodropdown-content input, .infodropdown-content select {
        margin-bottom: 10px;
        width: 100%;
      }
      .infodropdown:hover .infodropdown-content {
        visibility: visible;
      }
    "))
                       ),
                       div(class = "infodropdown",
                           actionButton("dropbtninfo", NULL, icon=icon("info-sign", lib = "glyphicon")),
                           span(class = "infodropdown-content",
                                verbatimTextOutput("infomessage")
                           )
                       ),
                       bottom="122px",left="10px",width="10px",height="10px",style = "z-index: 10000;"
                     ),
                     absolutePanel(
                       tags$head(
                         tags$style(HTML("
      .dldropdown {
        position: relative;
        display: inline-block;
      }
      .dldropdown-content {
        display: none;
        position: absolute;
        background-color: #f9f9f9;
        min-width: 200px;
        box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
        z-index: 1;
        padding: 12px;
        left: 0;
        bottom: 100%;
      }
      .dldropdown-content input, .dldropdown-content select {
        margin-bottom: 10px;
        width: 100%;
      }
      .dldropdown:hover .dldropdown-content {
        display: block;
      }
    "))
                       ),
                       div(class = "dldropdown",
                           downloadButton("dl","Save as image"),
                           div(class = "dldropdown-content",
                               numericInput("imgHeight","Height (px):",
                                            value=1500),
                               numericInput("imgWidth","Width (px):",
                                            value=3000),
                               selectInput("imgType","Format:",
                                           choices=c("png","pdf","jpeg","tiff","svg"),
                                           selected="png"),
                               textInput("imgLegend","Legend title:",value="Difference"),
                               textInput("imgBackCol","Background color (hex/any ggplot2 value):",
                                            value="#00000000"),
                               textInput("imgOutlCol","Outline color (hex/any ggplot2 value):",
                                            value="#00000000"),
                               numericInput("imgOutlSize","Outline width (mm):",
                                            value=0),
                               fluidRow(
                                 column(6,
                                        checkboxInput("omitAlaska","Omit Alaska")),
                                 column(6,
                                        checkboxInput("omitHawaii","Omit Hawaii"))
                               )
                           )
                       ),
                       bottom="85px",left="10px",width="10px",height="10px",style = "z-index: 10000;"
                     )
    )
  )

# This is the accompanying basic server module for the two-school comparison tool
server <- function(input, output, session){

  output$map <- renderLeaflet({
    gen_basemap()
  })

  filesUploaded <- reactive({
    !is.null(input$file1) && !is.null(input$file2)
  })
  
  output$filesUploaded <- reactive({
    filesUploaded()
  })
  outputOptions(output, "filesUploaded", suspendWhenHidden = FALSE)
  
  output$startupmessage <- renderText({
    paste0("Please upload the data in either .csv or .dta format.\n",
           "This build only accepts the field x institution x metro level.\n",
           "Note that reading a .csv is ~40x faster than reading a .dta for this app.\n")
  })
  
  output$readingmessage <- renderText({
    paste0("It will take a minute or two to read and to upload.")
  })
  
  observe({
    req(input$file1,input$file2)
    
    ext1 <- tools::file_ext(input$file1$name)
    if (ext1 == "csv") {full_level_dataALL<<-fread(input$file1$datapath)}
    else{full_level_dataALL<<-read_dta(input$file1$datapath)}
    
    ext2 <- tools::file_ext(input$file2$name)
    if (ext2 == "csv") {full_level_dataBACH <<- fread(input$file2$datapath)}
    else{full_level_dataBACH <<- read_dta(input$file2$datapath)}
    
    schools <<- c("None",unique(full_level_dataALL$instnm))
    fields <<- c("All",unique(full_level_dataALL$field))
    all_metros <<- data.frame(metro_area = unique(crosswalk$metro_area))
    all_metros[nrow(all_metros) + 1,] = c(NA)
    updateSelectizeInput("school1",choices=schools,session=session)
    updateSelectizeInput("school2",choices=schools,session=session)
    updateSelectizeInput("field",choices=fields,session=session)
    updateSelectizeInput("exclude",choices=c("None",all_metros$metro_area),
                         session=session,selected="None")
  })
  
  
  output$infomessage <- renderText({
    paste0("Be gentle with me. I'm not programmed for the general use. \n",
           "This tool compares differences in schools on the same map.\n",
           "Simple differencing simply subtracts the statistic from school1 and school2.\n",
           "Normalized differencing divides this result by the total number of students in that area.\n",
           "Log differencing takes the ln of the absolute difference, inducing negative values when necessary.\n",
           "The interactive map is drawn with simplified polygons for performance.\n",
           "Zoom in a lot to take note of this.\n",
           "Also, layers can be clicked to reveal their specific information.\n",
           "Finally, remember color is the only \"true\" mapping here. Opacity/radius is scaled for better visualization.\n",
           "Built by Ari G. for Richard Jin, URAP2024. Powered by Leaflet with basemap tiles provided and copyright by Carto.")
  })
  
  updateSelectizeInput(session, 'school1', choices = schools, server = TRUE)
  updateSelectizeInput(session, 'school2', choices = schools, server = TRUE)
  updateSelectizeInput(session, 'exclude', choices = c("None",all_metros$metro_area), server = TRUE)
  
  observeEvent(input$data, {
    leafletProxy("map") %>% clearGroup(c('zones','circles','state')) %>%
      removeControl("Legend") %>% removeControl("LegendExclude")
    if(input$data!='None'){
      cbsa_level_data <- genfiltered_data(input,"low")
      if(input$exclude!="None"){
        name <- capitalize_words(input)
        leafletProxy("map") %>%
          addPolygons(data=cbsa_level_data %>%
                        filter(metro_area==input$exclude),
                      fill="grey",
                      fillOpacity = 0.4,
                      opacity = 1,
                      weight=1,
                      color= "grey",
                      popup=paste0(popupPaste,input$data,"= NA"),
                      popupOptions = popupOptions(closeOnClick = TRUE),
                      group="zones") %>%
          addLegend(pal = colorFactor(palette = "grey",domain=name),
                    values = name,
                    layerId = "LegendExclude",
                    position="bottomright")
        cbsa_level_data <- cbsa_level_data %>%
          filter(metro_area!=input$exclude)
      }
      cbsa_col <- eval(parse(text=paste0("cbsa_level_data$", input$data)))
      leafletProxy("map") %>%
        addPolygons(data=cbsa_level_data,
                    fill=cbsa_col,
                    fillOpacity = 0.4,
                    opacity = 1,
                    weight=1,
                    color= pal2(cbsa_col,input$color)(cbsa_col),
                    popup=paste0(popupPaste,cbsa_level_data$NAMELSAD," <br> ",
                                 "school1 = ",eval(parse(text=paste0("cbsa_level_data$old_", input$data,"1")))," <br> ",
                                 "school2 = ",eval(parse(text=paste0("cbsa_level_data$old_", input$data,"2")))," <br> ",
                                 input$diff," diff=",cbsa_col),
                    popupOptions = popupOptions(closeOnClick = TRUE),
                    group="zones") %>%
        addLegend(pal = colorNumeric(palette = input$color,
                                     domain = c(-max(abs(cbsa_col)), 0, max(abs(cbsa_col)))),
                  values = seq(-max(abs(cbsa_col)),max(abs(cbsa_col)),length=9),
                  layerId = "Legend",
                  position="bottomright")
      
    }
    
    if(input$showStates){
      leafletProxy("map") %>%
        addPolygons(data=states,color="black",
                    group="state",fill=NA,weight=1,opacity=1)
    }
    
  })
  
  reDraw <- reactive({
    list(input$school1,input$school2,input$color,input$showStates,
         input$diff,input$field,input$bach,input$exclude,input$excludemetro)
  })
  observeEvent(reDraw(), {
    temp <- input$data
    updateSelectInput(session,"data",selected = 'None')
    updateSelectInput(session,"data",selected = temp)
  })
  
  user.created.map <- reactive({
    fetch_usermap(input)
  })
  
  output$dl <- downloadHandler(
    filename = function(){paste0(Sys.Date(),'_customMap.',input$imgType)},
    content = function(file) {
      ggsave(file,plot=user.created.map(),units="px",
             width=input$imgWidth,height=input$imgHeight,
             dpi=max(input$imgWidth,input$imgHeight)/8,
             device=input$imgType)
    })
  
}

shinyApp(ui = ui, server = server)