#Load packages ----
library(shiny)
library(dplyr)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinycssloaders)
library(lubridate)


# Settings ----
#Avoid sci notation
options(scipen=999)

app_name <- "Mass Digitization Dashboard"
app_ver <- "1.5.7"
github_link <- "https://github.com/Smithsonian/DPOMD_dashboard/"


#Internal or external server?
source("settings.R")

#Load data tables
load("data/data.RData")


# UI ----
ui <- fluidPage(
  
  tags$head(
    tags$title(app_name)
  ),
  HTML("<div class=\"alert alert-danger\" role=\"alert\">The Smithsonian is currently closed as part of the efforts to contain the spread of COVID-19. Some of our digitization projects have restarted and some are on hold until the staff and vendors can safely return to the museums.</div>"),
  fluidRow(
    column(width = 9,
           HTML(paste0("<h1><a href=\"http://dpo.si.edu\" target = _blank><img src=\"DPO_logo_76.png\" alt=\"DPO Logo\" title=\"DPO Logo\"></a> | ", app_name, "</h1>"))
    ),
    column(width = 3,
           uiOutput("external_server")
    )
  ),
  
    tabsetPanel(type = "tabs",
                tabPanel("Summary", 
                         br(),
                         fluidRow(
                           column(width = 3,
                                  uiOutput("topsummary3")
                           ),
                           column(width = 3,
                                  uiOutput("topsummary4")
                           ),
                           column(width = 3,
                                  uiOutput("topsummary1")
                           ),
                           column(width = 3,
                                  uiOutput("topsummary2")
                           )
                         ),
                        shinycssloaders::withSpinner(plotlyOutput("mdprogress", width = "100%", height = "480px")),
                        h3("Digitization Projects"),
                        p("Click a project in the table below to see more details:"),
                        
                        DT::dataTableOutput("projects"),
                         
                        HTML("<p class=\"text-info\"><small>* Value was estimated.<br>** Progress is based on the number of specimens/objects expected to be digitized. This value may change during production due to a number of reasons.</small></p>"),
                        
                        tags$em("Some of the data is courtesy of the DAMS team.")
                        ),
                
                tabPanel("Progress in Select Projects",
                         br(),
                         fluidRow(
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">NMNH - Herbarium<br>Images Captured by Month</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_bot")),
                                  HTML("</div></div>")
                           ),
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">NMNH - Paleobiology EPICC Mass Digitization - 2019<br>Images Captured by Day</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_paleo2")),
                                  HTML("</div></div>")
                           )
                          ),
                         br(),
                         fluidRow(
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">NMNH - Entomology Bumblebees and Carpenter Bees<br>Images Captured by Day</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_bees2")),
                                  HTML("</div></div>")
                           ),
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">NMAH - Russian coins and medals, Lilly, and Straub collections<br>Images Captured by Day</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_numis")),
                                  HTML("</div></div>")
                           )
                         ),
                         
                         br(),
                         fluidRow(
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">SG - Garden Club of America and Ken Druse Garden Photograph Collections<br>Images Captured by Day</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_druse")),
                                  HTML("</div></div>")
                           ),
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">NMAfA - Stephen Grant Postcard Collection<br>Images Captured by Day</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_nmafapost")),
                                  HTML("</div></div>")
                           )
                         ),
                         
                         br(),
                         fluidRow(
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">SIB - Castle Chair Collection<br>Images Captured by Day</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_sib")),
                                  HTML("</div></div>")
                           ),
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">NMAH - Princeton and War Posters Collections<br>Images Captured by Day</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_posters")),
                                  HTML("</div></div>")
                           )
                         ),
                         
                         br(),
                         fluidRow(
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">SG - Garden Furnishings, Horticult. Artifacts, and Archives<br>Images Captured by Day</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_gardensarch")),
                                  HTML("</div></div>")
                           ),
                           column(width = 6,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">SG - Orchids Live Collection<br>Images Captured by Day</div>
                                                <div class=\"panel-body\">"),
                                  shinycssloaders::withSpinner(plotlyOutput("progress_orchids")),
                                  HTML("</div></div>")
                           )
                         ),
                         
                         br()
                         
                         #tags$iframe(src="https://public.tableau.com/views/MDStats/Dashboard1?:showVizHome=no&:embed=true&:device=desktop", width="1140", height="1200", seamless = TRUE)
                        ),
                tabPanel("Daily/Monthly Statistics", 
                         br(),
                         fluidRow(
                           column(width = 5,
                                  HTML("<div class=\"panel panel-primary\">
                                                <div class=\"panel-heading\">Select one project to display detailed statistics</div>
                                                <div class=\"panel-body\">"),
                                  uiOutput("projects_stats2"),
                                  HTML("</div></div>")
                           )
                         ),
                         uiOutput("stats_daily_header"),
                         fluidRow(
                           column(width = 6,
                                  h4("Images captured by day:"),
                                  DT::dataTableOutput("stats_daily")
                           ),
                           column(width = 6,
                                  h4("Images captured by month:"),
                                  DT::dataTableOutput("stats_monthly")
                           )
                         ),
                         br(),
                         br(),
                         hr(),
                         tags$em("The data used to calculate statistics is courtesy of the DAMS team.")
                ),
                #help ----
                tabPanel("About/Definitions", 
                         fluidRow(
                           column(width = 4,
                             h4("About this Dashboard"),
                             p("This dashboard provides a partial view of the DPO Mass Digitization projects. 
                             We hope this helps collection staff, other Smithsonian staff, and interested 
                             parties to understand better how each project is progressing. We will 
                               continue to update this dashboard to display more details."),
                             h4("Data Sources"),
                             p("We are integrating data from multiple sources:"),
                             HTML("<ul>
                                      <li>DAMS</li>
                                      <li>Collection databases</li>
                                      <li>DPO QC systems</li>
                                  </ul>"),
                             p("For questions or comments about this dashboard, please contact Luis J. Villanueva or Ken Rahaim at the Digitization Program Office, OCIO."),
                             br(),
                             p("We thank DAMS for providing us with a view into their database and Research Computing for hosting the R/Shiny server.")
                             
                           ),
                           column(width = 1,
                                  HTML("&nbsp;")
                           ),
                           column(width = 4,
                                  
                                  h4("Definitions and notes"),
                                  HTML("<dl>
                                        <dt>Specimens</dt><dd>Number of specimens or objects in a collection that will be or have been digitized.</dd>
                                        <dt>Project Goal</dt><dd>Estimate of specimens or objects to digitized in a project.</dd>
                                        <dt>Number of images captured</dt><dd>The number of images captured is calculated after the vendor has delivered them to DPO and the images have been ingested in DAMS. Some projects may require additional post-processing that result in the images being delivered days or weeks after capture.</dd>
                                        <dt>Objects/Specimens Digitized vs Images Captured</dt><dd>The number of objects or specimens digitized may not match the number of images in some projects. We capture several views for each object in some projects. In other projects, some objects are too big for the field of view of the camera. Other projects have multiple objects in a single picture.</dd>
                                        <dt>Dates</dt><dd>These are the range of dates between the beginning of the digitization project and the final delivery of the images by the vendor. This range does not include the time for planning, preparation of the collection, or ingest of the images into the collection database.</dd>
                                        <dt>Status</dt><dd>Projects are categorized as \"Completed\" when the vendor completes delivery of the images. There might be additional steps needed to import the images to the various systems.</dd>
                                
                                </dl>")
                                  #<dt>Funding (coming soon)</dt><dd>We list the major sources of funding for each project. There may be additional costs covered by the unit or OCIO/DPO (<em>e.g.</em> staff, data processing and storage, and other computing resources) that are not quantified directly.</dd>
                                  ),
                           column(width = 3,
                                  h4("Mass Digi links"),
                                  HTML("<p>"),
                                  a(href = "https://dpo.si.edu", target = "_blank", "DPO Website"),
                                  br(),
                                  a(href = "https://twitter.com/sixdigi", target = "_blank", icon("twitter"), "@SIxDIGI"),
                                  br(),
                                  a(href = "https://www.instagram.com/SIXDIGI/", target = "_blank", icon("instagram"), "@SIxDIGI"),
                                  br(),
                                  a(href = "https://www.facebook.com/SIxDIGI/", target = "_blank", icon("facebook"), "@SIxDIGI")
                                  )
                          )
                )
    ),
  br(),
  br(),
  br(),
  uiOutput("footer")#,
  #tags$head(HTML('<meta http-equiv="refresh" content="600">'))
)



# Server ----
server <- function(input, output, session) {

  #font for plotly----
  plotfont <- list(
    family = "Helvetica")
  
  
  #mdprogress----
  output$mdprogress <- renderPlotly({
    
    running_total_df <- projects_monthly_data %>% 
      dplyr::group_by(date_sort, month) %>% 
      dplyr::summarise(total = sum(images_captured), objects = sum(objects_digitized)) %>% 
      dplyr::filter(total != 0)
    
    running_total_df$images_captured_running <- cumsum(running_total_df$total)
    running_total_df$objects_digitized_running <- cumsum(running_total_df$objects)
    running_total_df$unix_date <- as.numeric(as.POSIXct(running_total_df$date_sort, format="%Y-%m-%d"))*1000
    
    fig <- plot_ly(type = 'scatter', mode = 'lines+markers') 
    fig <- fig %>%
      add_trace(
        name = 'Images Captured',
        x = running_total_df$unix_date, 
        y = running_total_df$images_captured_running,
        text = ~paste0(prettyNum(as.integer(running_total_df$images_captured_running), big.mark = ",", scientific=FALSE), 
                       ' images<br> ', 
                       running_total_df$month),
        hoverinfo = 'text',
        line = list(color = '#002554'),
        marker = list(color = '#002554'),
        showlegend = T
      ) %>% 
      add_trace(
        name = "Objects/Specimens Digitized",
        x = running_total_df$unix_date, 
        y = running_total_df$objects_digitized_running,
        text = ~paste0(prettyNum(as.integer(running_total_df$objects_digitized_running), big.mark = ",", scientific=FALSE), 
                       ' objects/specimens<br> ', 
                       running_total_df$month),
        hoverinfo = 'text',
        line = list(color = '#009CDE'),
        marker = list(color = '#009CDE'),
        showlegend = T
      ) %>% 
      layout(
        xaxis = list(
            title = "Month", 
            fixedrange = TRUE, 
            range = c(
              as.numeric(as.POSIXct(
                min(running_total_df$date_sort), format="%Y-%m-%d"))*1000,
              as.numeric(as.POSIXct(
                max(running_total_df$date_sort) + months(1), format="%Y-%m-%d"))*1000
              ),
            type = 'date',
            tickformat = "%b %Y",
            autotick = F,
            dtick = "M3",
            tickangle = -45,
            ticks = "inside",
            showgrid = FALSE,
            size = 18
            ),
        yaxis = list(title = "Running Total", fixedrange = TRUE),
        font = plotfont,
        legend = list(x = 0.8, y = 0.1)
      )
  })
  
  
  
  
  #progress_bot----
  output$progress_bot <- renderPlotly({
    this_proj_data <- projects_monthly_data[projects_monthly_data$project_id == 100,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$month, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Month", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
      font = plotfont
      )
    })
  
  
  #progress_paleo2----
  output$progress_paleo2 <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 127,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      )
  })
  
  
  
  #progress_bees2----
  output$progress_bees2 <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 126,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      )
  })
  
  #progress_numis----
  output$progress_numis <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 124,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      )
  })
  
  
  
  #progress_sib----
  output$progress_sib <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 101,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      )
  })
  
  
  #progress_sib----
  output$progress_sib <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 101,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      )
  })
  
  
  #progress_posters----
  output$progress_posters <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 119,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      )
  })
  
  
  
  
  #progress_gardensarch----
  output$progress_gardensarch <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 119,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      )
  })
  
  
  
  #progress_orchids----
  output$progress_orchids <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 107,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      ) %>% 
      config(displaylogo = FALSE)
  })
  
  
  
  
  #progress_druse----
  output$progress_druse <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 120,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      ) %>% 
      config(displaylogo = FALSE)
  })
  
  
  
  #progress_nmafapost----
  output$progress_nmafapost <- renderPlotly({
    this_proj_data <- projects_daily_data[projects_daily_data$project_id == 125,]
    
    fig <- plot_ly(type = 'bar') 
    fig <- fig %>%
      add_trace(
        x = this_proj_data$date_sort, 
        y = this_proj_data$images_captured,
        text = ~paste0(' ', prettyNum(as.integer(this_proj_data$images_captured), big.mark = ",", scientific=FALSE), ' images<br> ', this_proj_data$date, ' '),
        hoverinfo = 'text',
        marker = list(color='#009CDE'),
        showlegend = F
      ) %>% layout(
        xaxis = list(title = "Date", fixedrange = TRUE),
        yaxis = list(title = "Images Captured", fixedrange = TRUE),
        font = plotfont
      ) %>% 
      config(displaylogo = FALSE)
  })
  
  
  
  #topsummary1----
  output$topsummary1 <- renderUI({
    tagList(
      HTML("<div class=\"alert alert-success\" role=\"alert\">"),
      h2(dim(proj_data)[1]),
      h4("Digitization Projects"),
      HTML("</div>")
    )
  })
  
  #topsummary2----
  output$topsummary2 <- renderUI({
    tagList(
      HTML("<div class=\"alert alert-success\" role=\"alert\">"),
      h2(dim(proj_data_ongoing)[1]),
      h4("Ongoing Projects"),
      HTML("</div>")
    )
  })
  
  #topsummary3----
  output$topsummary3 <- renderUI({
    tagList(
      HTML("<div class=\"alert alert-success\" role=\"alert\">"),
      h2(prettyNum(as.integer(sum(proj_data$objects_digitized)), big.mark = ",", scientific=FALSE)),
      h4("Specimens/Objects Digitized"),
      HTML("</div>")
    )
  })
  
  #topsummary4----
  output$topsummary4 <- renderUI({
    tagList(
      HTML("<div class=\"alert alert-success\" role=\"alert\">"),
      h2(prettyNum(as.integer(sum(proj_data$images_taken)), big.mark = ",", scientific=FALSE)),
      h4("Images Captured"),
      HTML("</div>")
    )
  })
  
  
  
  #external_server ----
  output$external_server <- renderUI({
    if (is_internal == FALSE){
      req(FALSE)
    }
    tagList(
      br(),
      HTML("<div class=\"alert alert-info pull-right\" role=\"alert\">"),
      HTML("<small><span class=\"glyphicon glyphicon-hand-right\" aria-hidden=\"true\"></span> Dashboard outside the SI network: <a href=\"https://shiny.si.edu/massdigi/\" target=_blank class=\"alert-link\">https://shiny.si.edu/massdigi/</a></small></div>")
    )
  })
  
  
  
  #projects----
  output$projects <- DT::renderDataTable({
    
    projects_list_tbl2 <- projects_list_tbl
    
    #rename(projects_list_tbl2, c("Project Progress" = "Project Progress**"))
    
    for (i in seq(1, dim(projects_list_tbl2)[1])){
      
      projects_list_tbl2$Title[1]
      
      this_proj <- projects[projects$project_title == projects_list_tbl2$Title[i], ]
      
      edan_images <- projects_edan[projects_edan$project_id == this_proj$project_id, ]
      
      if (dim(edan_images)[1] > 2){
        projects_list_tbl2$Title[i] <- paste0(projects_list_tbl2$Title[i], " &nbsp; <span class=\"glyphicon glyphicon-picture\" aria-hidden=\"true\" title = \"Click on the row to see example images from the project\"></span>")
      }
      
      proj_media <- projects_media[projects_media$project_id == this_proj$project_id, ]
      
      if (dim(proj_media[proj_media$media_type == "yt", ])[1] > 0){
        projects_list_tbl2$Title[i] <- paste0(projects_list_tbl2$Title[i], " &nbsp; <span class=\"glyphicon glyphicon-facetime-video\" aria-hidden=\"true\" title = \"Click on the row to see a video of the project\"></span>")
      }
    }
    
    
    
    DT::datatable(
      projects_list_tbl2,
      escape = FALSE,
      options = list(
        searching = FALSE,
        ordering = TRUE, 
        paging = TRUE,
        pageLength = dim(projects_list_tbl)[1],
        dom = 't',
        #Pull these cols to the right
        columnDefs = list(list(
          className = 'dt-right', targets = c(4, 5)))
      ),
      rownames = FALSE,
      colnames = c('Unit', 'Title', 'Status', 'Specimens/Obj Digitized', 'Project Progress <sup>**</sup>', 'Images Captured', 'Dates'),
      selection = 'single'#,
      #caption = htmltools::tags$caption(
      #  style = 'caption-side: bottom; text-align: left;',
      #  '* Value was estimated\n** Project progress is based on the estimated number of specimens/objects expected to be digitized'
      #)
      ) %>% 
        formatStyle("Dates", "white-space" = "nowrap") %>% formatStyle(
          'Status',
          backgroundColor = styleEqual(c('Ongoing', 'Completed'), c('#dff0d8', '#ECECEC'))
        )
  })
  
  
  observeEvent(input$projects_rows_selected, {
    
    selected_project_title <- projects_list_tbl[input$projects_rows_selected,]$'Title'
    
    proj_info <- projects_info[projects_info$project_title == selected_project_title,]
    
    if (!is.na(proj_info$filecheck_link) && is_internal){
      fc_link <- paste0("<dt>Production Dashboard</dt><dd>", a(href = proj_info$filecheck_link, target = "_blank", "Link") ,"</dd>")
    }else{
      fc_link <- ""
    }
    
    if (!is.na(proj_info$project_url)){
      this_url <- proj_info$project_url
      if (nchar(this_url) > 40){
        this_url <- paste0(strtrim(this_url, 40), "...")
      }
      url <- HTML(paste0("<dt>URL</dt><dd><a href=\"", proj_info$project_url, "\" target = \"_blank\">", this_url, "</a></dd>"))
    }else{
      url <- ""
    }
    
    if (!is.na(proj_info$project_end)){
      project_end <- HTML(paste0("<dt>End Date</dt><dd>", proj_info$project_end, "</dd>"))
    }else{
      project_end <- ""
    }
    
    proj_media <- projects_media[projects_media$project_id == proj_info$project_id, ]
    #proj_budget <- projects_budget[projects_budget$project_id == proj_info$project_id, ]
    
    media_links <- ""
    
    if (dim(proj_media)[1] > 0){
      media_links <- "<h4>Videos, articles, and other sites about this project:</h4>"
      for (i in seq(1, dim(proj_media)[1])){
        if (proj_media$media_type[i] == 'yt'){
          media_links <- paste0(media_links, "<p>", proj_media$media_title[i], ":<br><iframe width=\"300px\" height=\"240px\" src=\"https://www.youtube.com/embed/", proj_media$media_link[i], "\" frameborder=\"0\" allow=\"accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen></iframe></p>")
        }else{
          media_links <- paste0(media_links, "<p><a href=\"", proj_media$media_link[i], "\" target = _blank>", proj_media$media_title[i],"</a></p>")
        }
      }
      media_links <- paste0(media_links, "<hr>")
    }
    
    unit <- projects_units[projects_units$unit == proj_info$project_unit, ]
    
    objects_percent <- round((proj_info$objects_digitized / proj_info$collex_to_digitize) * 100, 2)
    
    b_info <- ""
    
    # if (dim(proj_budget)[1] > 0){
    #   
    #   b_info <- paste0("Total: $ ", prettyNum(sum(proj_budget$budget_amount), big.mark = ",", scientific=FALSE), "<br><ul>")
    #   
    #   for (b in seq(1, dim(proj_budget)[1])){
    #     b_info <- paste0(b_info, "<li>", proj_budget$budget_source[b], ": ", proj_budget$budget[b], "</li>")
    #   }
    #   b_info <- paste0(b_info, "</ul>")
    # }else{
    #   b_info = "NA"
    # }
    
    #edan ----
    edan_html <- ""
    edan_images <- projects_edan[projects_edan$project_id == proj_info$project_id,]
    
    if (dim(edan_images)[1] > 0){
      proj_edan_images <- sample_n(edan_images, 3, replace = FALSE)
      
      if (dim(proj_edan_images)[1] == 3){
        
        print(proj_edan_images)
        
        edan1 <- HTML(paste0("<a href=\"", proj_edan_images$link[1], "\" target = _blank><img class = \"loading\" src=\"", proj_edan_images$img_file[1], "\" style=\"max-height: 160px; width:auto;\" alt=\"", proj_edan_images$title[1], "\" title=\"", proj_edan_images$title[1], "\"></a><p>", proj_edan_images$title[1], "<br>", proj_edan_images$credit[1], "|", proj_edan_images$notes[1], "</p>"))
        
        edan2 <- HTML(paste0("<a href=\"", proj_edan_images$link[2], "\" target = _blank><img class = \"loading\" src=\"", proj_edan_images$img_file[2], "\" style=\"max-height: 160px; width:auto;\" alt=\"", proj_edan_images$title[1], "\" title=\"", proj_edan_images$title[1], "\"></a><p>", proj_edan_images$title[2], "<br>", proj_edan_images$credit[2], "|", proj_edan_images$notes[2], "</p>"))
        
        edan3 <- HTML(paste0("<a href=\"", proj_edan_images$link[3], "\" target = _blank><img class = \"loading\" src=\"", proj_edan_images$img_file[3], "\" style=\"max-height: 160px; width:auto;\" alt=\"", proj_edan_images$title[1], "\" title=\"", proj_edan_images$title[1], "\"></a><p>", proj_edan_images$title[3], "<br>", proj_edan_images$credit[3], "|", proj_edan_images$notes[3], "</p>"))
        
        edan_html <- HTML(paste("<h4>Example images from the project:</h4>", tagList(
          fluidRow(
            column(width = 4,
                   edan1
            ),
            column(width = 4,
                   edan2
            ),
            column(width = 4,
                   edan3
            )
          ),
          hr()
        )
        ))
      }
    }
    
    
    
    showModal(modalDialog(
      size = "l",
      title = "Project Info",
      HTML(media_links),
      edan_html,
      HTML(paste0("<h4>Project details:</h4><dl class = \"dl-horizontal\"><dt>Project Title</dt><dd>", proj_info$project_title, "</dd>")),
      HTML(fc_link),
      HTML(paste0("<dt>Unit</dt><dd><a href=\"", unit$unit_link, "\" target = _blank>", unit$unit_fullname, "</a> (", unit$unit, ")</dd>")),
      HTML(paste0("<dt>Project Type</dt><dd>", proj_info$project_type, "</dd>")),
      HTML(paste0("<dt>Status</dt><dd>", proj_info$project_status, "</dd>")),
      HTML(paste0("<dt>Specimens Digitized</dt><dd>", prettyNum(proj_info$objects_digitized, big.mark = ",", scientific=FALSE), " (", objects_percent, " % of the Project Goal of ", prettyNum(proj_info$collex_to_digitize, big.mark = ",", scientific=FALSE), ")</dd>")),
      HTML(paste0("<dt>Images Captured</dt><dd>", prettyNum(proj_info$images_taken, big.mark = ",", scientific=FALSE), "</dd>")),
      #HTML(paste0("<dt>Budget Summary</dt><dd>", b_info, "</dd>")),
      HTML(paste0("<dt>Description</dt><dd>", proj_info$project_description, "</dd>")),
      HTML(paste0("<dt>Methods</dt><dd>", proj_info$project_method, "</dd>")),
      HTML(paste0("<dt>Project Manager</dt><dd>", proj_info$project_manager, "</dd>")),
      url,
      HTML(paste0("<dt>Start Date</dt><dd>", proj_info$project_start, "</dd>")),
      project_end,
      HTML("</dl>"),
      easyClose = TRUE
    ))
  })
  
  
  
  

  
   
  #projects_stats----
  output$projects_stats <- renderUI({
    choices = setNames(projects_daily$process_summary, projects_daily$project_title)
    selectInput("process_summary", "Project:",  choices, width = "100%")
  })
  
  
  #projects_stats2----
  output$projects_stats2 <- renderUI({
    choices = setNames(projects_daily$project_id, projects_daily$project_title)
    selectInput("process_summary2", "Project:",  choices, width = "100%")
  })
  
  
  #stats ----
  output$stats <- renderUI({
    req(input$process_summary)
    
    proj_info <- proj_stats[proj_stats$process_summary == input$process_summary, ]
    
    project_size <- projects_size[projects_size$project_id == proj$project_id, ]
    
    if (!is.na(proj_info$project_end)){
      project_end <- proj_info$project_end
    }else{
      project_end <- ""
    }
    
    tagList(
      HTML(paste0("<h4>", proj$project_unit, " - ", proj$project_title, " Project Statistics:</h4>")),
      HTML("<dl class = \"dl-horizontal\"><dt>Objects Digitized</dt><dd>", prettyNum(as.integer(proj_info$objects_digitized), big.mark = ",", scientific=FALSE), "</dd>"),
      HTML("<dt>Project type</dt><dd>", proj_info$project_type, "</dd>"),
      HTML("<dt>Status</dt><dd>", proj_info$project_status, "</dd>"),
      HTML("<dt>Dates</dt><dd>", proj_info$project_start, " - ",  project_end, "</dd>"),
      HTML(paste0("<dt>Average file size</dt><dd><ul><li>TIF files: ", prettyNum(round(project_size$avg_file_size, 2), big.mark = ",", scientific=FALSE), " MB</li><li>Raw files: ", prettyNum(round(project_size$avg_rawfile_size, 2), big.mark = ",", scientific=FALSE), " MB</li></ul></dd>")),
      HTML(paste0("<dt>Total file size</dt><dd> <ul><li>TIF files: ", prettyNum(round(project_size$total_file_size/1000, 2), big.mark = ",", scientific=FALSE), " GB</li><li>Raw files: ", prettyNum(round(project_size$total_rawfile_size/1000, 2), big.mark = ",", scientific=FALSE), " GB</li></ul></dd><dl>"))
      )
  })
  
  
  
  output$stats_daily_header <- renderUI({
    req(input$process_summary2)
    proj <- projects[projects$project_id == input$process_summary2, ]
    
    h3(paste0("Detail statistics of the project: ", proj$project_title, " (", proj$project_unit, ")"))
  })
  
  
  
  #stats_daily----
  output$stats_daily <- DT::renderDataTable({
    req(input$process_summary2)
    proj <- projects[projects$project_id == input$process_summary2, ]
    
    daily_data <- projects_daily_data[projects_daily_data$project_id == input$process_summary2, ]
    
    if (proj$images_estimated == 1){
      daily_data$`Images Captured*` <- prettyNum(daily_data$images_captured, big.mark = ",", scientific=FALSE)
    }else{
      daily_data$`Images Captured` <- prettyNum(daily_data$images_captured, big.mark = ",", scientific=FALSE)
    }
    
    if (proj$objects_estimated == 1){
      daily_data$`Objects Digitized*` <- prettyNum(daily_data$objects_digitized, big.mark = ",", scientific=FALSE)
    }else{
      daily_data$`Objects Digitized` <- prettyNum(daily_data$objects_digitized, big.mark = ",", scientific=FALSE)
    }
    
    daily_data <- select(daily_data, -date_sort) %>% 
      select(-images_captured) %>% 
      select(-objects_digitized) %>% 
      select(-project_id)
    
      DT::datatable(
        daily_data,
        extensions = 'Buttons',
        escape = FALSE, 
        options = list(
          paging = TRUE,
          searching = FALSE,
          fixedColumns = FALSE,
          autoWidth = FALSE,
          ordering = FALSE,
          dom = 'Bfrtip',
          buttons = c('csv', 'excel'),
          pageLength = 30
        ),
        selection = 'none',
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'caption-side: bottom; text-align: left;',
          '* value was estimated'
        ),
        class = "display"
      )
  }, server = FALSE)
  
  
  
  #stats_monthly----
  output$stats_monthly <- DT::renderDataTable({
    req(input$process_summary2)
    proj <- projects[projects$project_id == input$process_summary2, ]
    
    daily_data <- projects_monthly_data[projects_monthly_data$project_id == input$process_summary2, ]
    
    if (proj$images_estimated == 1){
      daily_data$`Images Captured*` <- prettyNum(daily_data$images_captured, big.mark = ",", scientific=FALSE)
    }else{
      daily_data$`Images Captured` <- prettyNum(daily_data$images_captured, big.mark = ",", scientific=FALSE)
    }
    
    if (proj$objects_estimated == 1){
      daily_data$`Objects Digitized*` <- prettyNum(daily_data$objects_digitized, big.mark = ",", scientific=FALSE)
    }else{
      daily_data$`Objects Digitized` <- prettyNum(daily_data$objects_digitized, big.mark = ",", scientific=FALSE)
    }
    
    daily_data <- select(daily_data, -date_sort) %>% 
      select(-images_captured) %>% 
      select(-objects_digitized) %>% 
      select(-project_id)
    
    DT::datatable(
      daily_data,
      extensions = 'Buttons',
      escape = FALSE, 
      options = list(
        paging = TRUE,
        searching = FALSE,
        fixedColumns = FALSE,
        autoWidth = FALSE,
        ordering = FALSE,
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 12
      ),
      selection = 'none',
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        '* value was estimated'
      ),
      class = "display"
    )
  }, server = FALSE)
  
  
   
  
  
  
  #footer ----
  output$footer <- renderUI({
    HTML(paste0("<div class=\"footer navbar-fixed-bottom\" style=\"background: #C6C6C6; padding-top: 10px;\"><p>&nbsp;&nbsp;", app_name, ", ver. ", app_ver, " | <a href=\"https://dpo.si.edu/\" target = \"_blank\">DPO</a>, <a href=\"https://www.si.edu/ocio\" target = \"_blank\">OCIO</a>, <a href=\"https://www.si.edu/\" target = \"_blank\">Smithsonian Institution</a> | <a href=\"", github_link, "\" target = _blank>Source code</a> | Data updated on: ", last_date, "</p></div>"))
  })
}


# Run app ----
shinyApp(ui = ui, server = server, onStart = function() {
  cat("Loading\n")
  #Load project ----
  onStop(function() {
    cat("Closing\n")
  })
})
