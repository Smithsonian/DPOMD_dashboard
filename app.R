#Load packages ----
library(shiny)
library(dplyr)
library(shinyWidgets)
library(DT)


# Settings ----
#Avoid sci notation
options(scipen=999)

app_name <- "Mass Digitization Dashboard"
app_ver <- "1.4.1"
github_link <- "https://github.com/Smithsonian/DPOMD_dashboard/"


#Load data tables
load("data/data.RData")


# UI ----
ui <- fluidPage(
  
  tags$head(
    tags$title(app_name)#,
    # tags$style(HTML("
    #   .loading {
    #       background: transparent url('ajax-loader.gif') center no-repeat;
    #       min-height: 100px;
    #       min-width: 70px;
    #     }
    # "))
  ),
  
  HTML(paste0("<h1><a href=\"http://dpo.si.edu\" target = _blank><img src=\"dpologo.jpg\"></a> | ", app_name, "</h1>")),
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
                        #HTML("<div class=\"alert alert-warning\" role=\"alert\"><strong>Notice</strong>: The statistics in the dashboard will be updated less frequently during the period the museums are closed.</div>"),
                        h3("Digitization Projects"),
                        p("Click a project in the table below to see more details:"),
                        DT::dataTableOutput("projects"),
                         
                        tags$em("Some of the data is courtesy of the DAMS team.")
                        ),
                # tabPanel("Summary Statistics", 
                #          br(),
                #          fluidRow(
                #            column(width = 6,
                #                   HTML("<div class=\"panel panel-primary\">
                #                                 <div class=\"panel-heading\">Select one project to display detailed statistics</div>
                #                                 <div class=\"panel-body\">"),
                #                   uiOutput("projects_stats"),
                #                   HTML("</div></div>")
                #            )
                #          ),
                #          fluidRow(
                #            column(width = 6,
                #                   uiOutput("stats")
                #            ),
                #            column(width = 6,
                #                   uiOutput("stats_all")
                #            )
                #          ),
                #          fluidRow(
                #            column(width = 6,
                #                   uiOutput("stats1"),
                #                   uiOutput("stats3")
                #            ),
                #            column(width = 6,
                #                   uiOutput("stats2"),
                #                   uiOutput("stats4")
                #            )
                #          ),
                #          tags$em("The data used to calculate statistics is courtesy of the DAMS team.")
                # ),
                tabPanel("Progress in Select Projects",
                         br(),
                         tags$iframe(src="https://public.tableau.com/views/MDStats/Dashboard1?:showVizHome=no&:embed=true&:device=desktop", width="1140", height="1200", seamless = TRUE)
                        ),
                tabPanel("Daily Statistics", 
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
                tabPanel("About/Help", 
                         fluidRow(
                           column(width = 4,
                             h4("About this Dashboard"),
                             p("This dashboard provides a partial view of the data related to the DPO mass digitization projects. We hope this 
                               helps collection staff, other Smithsonian staff, and interested parties to understand better how each project
                               is progressing. We will continue to update this dashboard to display more details."),
                             h4("Data Sources"),
                             p("We are integrating data from multiple sources:"),
                             HTML("<ul>
                                      <li>DAMS</li>
                                      <li>Collection databases</li>
                                      <li>DPO QC systems</li>
                                  </ul>"),
                             p("For questions or comments about this dashboard, please contact Luis J. Villanueva or Ken Rahaim at the Digitization Program Office, OCIO.")
                             
                           ),
                           column(width = 1,
                                  HTML("&nbsp;")
                           ),
                           column(width = 4,
                                  #help ----
                                  h4("Definitions and notes"),
                                  HTML("<dl>
                                        <dt>Specimens</dt><dd>Number of specimens or objects in a collection that will be or have been digitized.</dd>
                                        <dt>Project Goal</dt><dd>Estimate of specimens or objects to digitized in a project.</dd>
                                        <dt>Number of images captured</dt><dd>The number of images captured is calculated after the vendor has delivered them to DPO. Some projects may require additional post-processing that result in the images being delivered days or weeks after capture.</dd>
                                        <dt>Ojects/Specimens Digitized vs Images Captured</dt><dd>The number of objects or specimens digitized may not match the number of images in some projects. For some projects we capture several views for each object and some objects are too big for the field of view of the camera. Other projects may have multiple objects in a single picture.</dd>
                                        <dt>Dates</dt><dd>These are the range of dates between the beginning of the digitization project and the final delivery of the images by the vendor. This range does not include the time for planning, preparation of the collection, or ingest of the images into the collection database. We plan to provide some of this data in the future.</dd>
                                        <dt>Status</dt><dd>Projects are categorized as \"Completed\" when the vendor completes delivery of the images. There might be additional steps needed to import the images to the various systems.</dd>
                                <dt>Funding (coming soon)</dt><dd>We list the major sources of funding for each project. There may be additional costs covered by the unit or OCIO/DPO (<em>e.g.</em> staff, data processing and storage, and other computing resources) that are not quantified directly.</dd>
                                </dl>")
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
  uiOutput("footer"),
  tags$head(HTML('<meta http-equiv="refresh" content="600">'))
)



# Server ----
server <- function(input, output, session) {

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
  
  
  #list of projects----
  output$projects <- DT::renderDataTable({
    
    DT::datatable(
      projects_list_tbl,
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
      selection = 'single',
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        '* value was estimated'
      )) %>% 
        formatStyle("Dates", "white-space" = "nowrap") %>% formatStyle(
          'Status',
          backgroundColor = styleEqual(c('Ongoing', 'Completed'), c('#dff0d8', '#ECECEC'))
        )
  })
  
  
  observeEvent(input$projects_rows_selected, {
    
    selected_project_title <- projects_list_tbl[input$projects_rows_selected,]$'Title'
    
    proj_info <- projects_info[projects_info$project_title == selected_project_title,]
    
    if (!is.na(proj_info$filecheck_link)){
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
    
    #print(projects_units)
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
    
    showModal(modalDialog(
      size = "l",
      title = "Project Info",
      HTML(media_links),
      
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
  
  
  
  #edan ----
  output$edan <- renderUI({
    req(FALSE)
    #Rewrite, too slow
    # req(input$projects_rows_selected)
    # 
    # selected_project_id <- projects_list[input$projects_rows_selected,]$project_id
    # 
    # query_edan <- dbGetQuery(db, paste0("SELECT * from projects_edan WHERE project_id = ", selected_project_id))
    # 
    # if (dim(query_edan)[1] > 0){
    #   if (dim(query_edan)[1] > 2){
    #     edan_examples <<- dplyr::sample_n(query_edan, 2, replace = FALSE)
    #   }else{
    #     edan_examples <<- query_edan
    #   }
    # }else{
    #   req(FALSE)
    # }
    # 
    # this_item_search <- EDANr::searchEDAN(q = edan_examples[1,]$edan_id, AppID = AppID, AppKey = AppKey)
    # if (this_item_search$rowCount == 1){
    #   edan_id <- this_item_search$rows$url
    #   
    #   this_item <- EDANr::getContentEDAN(itemID = edan_id, AppID = AppID, AppKey = AppKey)
    #   
    #   IDSid1 <- this_item$content$descriptiveNonRepeating$online_media$media$idsId[1]
    #   if (!is.null(IDSid1)){
    #     title1 <- this_item$content$descriptiveNonRepeating$title$content[1]
    #     if (title1 == "Untitled Object"){
    #       title1 <- ""
    #     }
    #     
    #     link1 <- this_item$content$descriptiveNonRepeating$record_link[1]
    #     if (is.null(link1)){
    #       link1 <- paste0("http://collections.si.edu/search/detail/", edan_id)
    #     }
    #     
    #     credit1 <- this_item$content$freetext$creditLine$content[1]
    #     if (!is.null(credit1)){
    #       credit1 <- paste0("<br>", credit1)
    #     }else{
    #       credit1 <- ""
    #     }
    #     
    #     notes1 <- paste(this_item$content$freetext$notes$content[1], collapse = ". ")
    #     if (!is.null(notes1) && notes1 != "1"){
    #       notes1 <- paste0("<br>", notes1)
    #     }else{
    #       notes1 <- ""
    #     }
    #     
    #     if (substr(IDSid1, 0, 4) != 'http'){
    #       IDSid1 <- paste0('http://ids.si.edu/ids/deliveryService?id=', IDSid1)
    #     }
    #     
    #     edan1_img <- HTML(paste0(paste0("<h4>Example images from the project:</h4><a href=\"", IDSid1, "\" target = _blank><img class = \"loading\" src=\"", IDSid1, "&max_h=130\" style=\"max-height: 130px; width:auto;\"></a>", 
    #                                     "<dl><dd>", title1, 
    #                                     credit1, 
    #                                     notes1, 
    #                                     "<br><a href=\"", link1, "\" target = _blank>Object record</a></dd></dl>")))
    #   }else{
    #     edan1_img <- ""
    #   }
    # }else{
    #   if (edan_examples[1,]$dams_only == '1'){
    #     edan1_img <- HTML(paste0(paste0("<h4>Example images from the project:</h4><a href=\"http://ids.si.edu/ids/dynamic?id=", 
    #                                     edan_examples[1,]$edan_id, 
    #                                     "\" target = _blank><img class = \"loading\" src=\"http://ids.si.edu/ids/deliveryService?id=", 
    #                                     edan_examples[1,]$edan_id, 
    #                                     "&max_h=130\" style=\"max-height: 130px; width:auto;\"></a><p><em>Details and link not available</em></p><br>")))
    #   }else{
    #     #Try using the id directly
    #     this_item <- EDANr::getContentEDAN(itemID = edan_examples[1,]$edan_id, AppID = AppID, AppKey = AppKey)
    #     
    #     IDSid1 <- this_item$content$descriptiveNonRepeating$online_media$media$idsId[1]
    #     if (!is.null(IDSid1)){
    #       title1 <- this_item$content$descriptiveNonRepeating$title$content[1]
    #       if (title1 == "Untitled Object"){
    #         title1 <- ""
    #       }
    #       
    #       link1 <- this_item$content$descriptiveNonRepeating$record_link[1]
    #       if (is.null(link1)){
    #         link1 <- paste0("http://collections.si.edu/search/detail/", edan_id)
    #       }
    #       
    #       credit1 <- this_item$content$freetext$creditLine$content[1]
    #       if (!is.null(credit1)){
    #         credit1 <- paste0("<br>", credit1)
    #       }else{
    #         credit1 <- ""
    #       }
    #       
    #       notes1 <- paste(this_item$content$freetext$notes$content[1], collapse = ". ")
    #       if (!is.null(notes1) && notes1 != "1"){
    #         notes1 <- paste0("<br>", notes1)
    #       }else{
    #         notes1 <- ""
    #       }
    #       
    #       if (substr(IDSid1, 0, 4) != 'http'){
    #         IDSid1 <- paste0('http://ids.si.edu/ids/deliveryService?id=', IDSid1)
    #       }
    #       
    #       edan1_img <- HTML(paste0(paste0("<h4>Example images from the project:</h4><a href=\"", IDSid1, "\" target = _blank><img class = \"loading\" src=\"", IDSid1, "&max_h=130\" style=\"max-height: 130px; width:auto;\"></a>", 
    #                                       "<dl><dd>", title1, 
    #                                       credit1, 
    #                                       notes1, 
    #                                       "<br><a href=\"", link1, "\" target = _blank>Link</a></dd></dl>")))
    #     }else{
    #       edan1_img <- ""
    #     }
    #   }
    # }
    # 
    # if (dim(edan_examples)[1] == 2){
    #     
    #   this_item_search <- EDANr::searchEDAN(q = edan_examples[2,]$edan_id, AppID = AppID, AppKey = AppKey)
    #   if (this_item_search$rowCount == 1){
    #     edan_id <- this_item_search$rows$url
    #     
    #     this_item <- EDANr::getContentEDAN(itemID = edan_id, AppID = AppID, AppKey = AppKey)
    #     
    #     IDSid2 <- this_item$content$descriptiveNonRepeating$online_media$media$idsId[1]
    #     if (!is.null(IDSid2)){
    #         
    #       title2 <- this_item$content$descriptiveNonRepeating$title$content[1]
    #       if (title2 == "Untitled Object"){
    #         title2 <- ""
    #       }
    #       
    #       link2 <- this_item$content$descriptiveNonRepeating$record_link[1]
    #       if (is.null(link2)){
    #         link2 <- paste0("http://collections.si.edu/search/detail/", edan_id)
    #       }
    #       credit2 <- this_item$content$freetext$creditLine$content[1]
    #       if (!is.null(credit2)){
    #         credit2 <- paste0("<br>", credit2)
    #       }else{
    #         credit2 <- ""
    #       }
    #       
    #       notes2 <- paste(this_item$content$freetext$notes$content[1], collapse = ". ")
    #       if (!is.null(notes2) && notes2 != "1"){
    #         notes2 <- paste0("<br>", notes2)
    #       }else{
    #         notes2 <- ""
    #       }
    #       
    #       if (substr(IDSid2, 0, 4) != 'http'){
    #         IDSid2 <- paste0('http://ids.si.edu/ids/deliveryService?id=', IDSid2)
    #       }
    #       
    #       edan2_img <- HTML(paste0(paste0("<p>&nbsp;</p><a href=\"", IDSid2, "\" target = _blank><img class = \"loading\" src=\"", IDSid2, "&max_h=130\" style=\"max-height: 130px; width:auto;\"></a>", 
    #                                     "<dl><dd>", title2, 
    #                                     credit2, 
    #                                     notes2, 
    #                                     "<br><a href=\"", link2, "\" target = _blank>Object record</a></dd></dl>")))
    #     }else{
    #       edan2_img <- ""
    #       cat(paste0("error", edan_examples[2,]$edan_id))
    #     }
    #     
    #   }else{
    #     if (edan_examples[2,]$dams_only == '1'){
    #       edan2_img <- HTML(paste0(paste0("<p>&nbsp;</p><a href=\"http://ids.si.edu/ids/dynamic?id=", 
    #                                     edan_examples[2,]$edan_id, 
    #                                     "\" target = _blank><img class = \"loading\" src=\"http://ids.si.edu/ids/deliveryService?id=", 
    #                                     edan_examples[2,]$edan_id, 
    #                                     "&max_h=130\" style=\"max-height: 130px; width:auto;\"></a><p><em>Details and link not available</em></p><br>")))
    #     }
    #   }
    #   
    # }else{
    #   edan2_img <- ""
    # }
    # 
    # if (edan1_img != "" && edan2_img != ""){
    #   edan_hr <- "<hr>"
    # }else{
    #   edan_hr <- ""
    # }
    # 
    # tagList(
    #   fluidRow(
    #     column(width = 6,
    #            edan1_img
    #     ),
    #     column(width = 6,
    #            edan2_img
    #     )
    #   ),
    #   HTML(edan_hr)
    # )
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
  
  
  #stats_all ----
  # output$stats_all <- renderUI({
  #   req(input$process_summary)
  #     tagList(
  #       h4("Number of days between steps post capture of the images:"),
  #       tags$img(src = paste0(input$process_summary, ".png"), width = 600, height = "auto"),
  #       br()
  #     )
  # })
  
  #stats1 ----
  # output$stats1 <- renderUI({
  #   req(input$process_summary)
  #   tagList(
  #     h4("Days between capture of the images and VFCU completed by month:"),
  #     tags$img(src = paste0(input$process_summary, "_1.png"), width = 600, height = "auto"),
  #     br()
  #   )
  # })
  
  #stats2 ----
  # output$stats2 <- renderUI({
  #   req(input$process_summary)
  #   tagList(
  #     h4("Days between capture of the images and ingest into DAMS by month:"),
  #     tags$img(src = paste0(input$process_summary, "_2.png"), width = 600, height = "auto"),
  #     br()
  #   )
  # })
  
  #stats3 ----
  # output$stats3 <- renderUI({
  #   req(input$process_summary)
  #   tagList(
  #     h4("Days between capture of the images and data connect to CIS by month:"),
  #     tags$img(src = paste0(input$process_summary, "_3.png"), width = 600, height = "auto"),
  #     br()
  #   )
  # })
  
  #stats4 ----
  # output$stats4 <- renderUI({
  #   req(input$process_summary)
  #   tagList(
  #     h4("Days between capture of the images and metadata sync to EDAN by month:"),
  #     tags$img(src = paste0(input$process_summary, "_4.png"), width = 600, height = "auto"),
  #     br()
  #   )
  # })

  
  
  
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
  
  
   
  
  #stats_daily_fig----
  # output$stats_daily_fig <- DT::renderDataTable({
  #   req(input$process_summary2)
  #   tagList(
  #     h4("Days between capture of the images and VFCU completed by month:"),
  #     tags$img(src = paste0(input$process_summary2, "_day.png"), width = 600, height = "auto"),
  #     br()
  #   )
  # })
  
  
  
  #stats_monthly_fig----
  # output$stats_monthly_fig <- DT::renderDataTable({
  #   req(input$process_summary)
  #   tagList(
  #     h4("Days between capture of the images and VFCU completed by month:"),
  #     tags$img(src = paste0(input$process_summary, "_month.png"), width = 600, height = "auto"),
  #     br()
  #   )
  # })
  
  
  
  #footer ----
  output$footer <- renderUI({
    HTML(paste0("<div class=\"footer navbar-fixed-bottom\" style=\"background: #C6C6C6; padding-top: 10px;\"><p>&nbsp;&nbsp;", app_name, ", ver. ", app_ver, " | <a href=\"", github_link, "\" target = _blank>Source code</a> | Data updated on: ", last_date, "</p></div>"))
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
