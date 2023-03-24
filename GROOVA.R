library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(ggthemes)
library(ggrepel)
library(DT)
library(shiny)
library(shiny.i18n)
library(shinyWidgets)
library(shinyjs)
library(ggpubr)


#app theme functions
themeSelector <- function() {
    div(
        div(  shiny.i18n::usei18n(i18n),
            selectInput("shinytheme-selector", i18n$t("Choose Theme"),
                        c("default", shinythemes:::allThemes()),
                        selectize = FALSE
            )
        ),
        tags$script(
            "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'bootstrap';
        curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
        )
    )
}

#language functions
i18n <- Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language("English") # here you select the default translation to display

# Define UI for application that manipulates data
ui <- 
    
        navbarPage(title = "GROOVA", id = "tabs" , selected = "ud" , inverse = TRUE, 
                 
                 tabPanel(title ="", icon = icon("fa-solid fa-gear"),
                          fluidPage( 
                              #theme = shinytheme("readable"),
                              #shinythemes::themeSelector(),
                              mainPanel(
                                  shiny.i18n::usei18n(i18n),
                                  div(style = "float: left;",
                                      selectInput("selected_language",
                                                  i18n$t("Change Language"),
                                                  choices = i18n$get_languages(),
                                                  selected = i18n$get_key_translation()),
                                      themeSelector(),
                                  br(),
                                  br(),
                                  br(),
                                  tags$p(i18n$t("GROOVA is developed by Golnaz Arastoopour Irgens at Clemson University")),
                                  tags$p(i18n$t("Feedback provided from the wonderful teachers and children at McKissick Elementary in Easley, SC")),
                                  tags$p(i18n$t("This project is supported by the National Science Foundation (DRL-2031175)")),
                                  tags$p(i18n$t("For more information visit")),
                                  tags$a(href="http://www.datapopups.com", "Data Pop-Ups Project")
                                  )
                                  
                              )
                          )
                 ),
                 
                 
                 tabPanel(title = i18n$t("Upload Data"), value = "ud",
                          fluidPage( 
                              sidebarLayout(
                                  sidebarPanel(
                                      fileInput("file", i18n$t("Upload Data Spreadsheet"),
                                                multiple = TRUE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv",
                                                           ".xls",
                                                           ".xlsx")),
                                  ),
                                  mainPanel(
                                      
                                      DT::dataTableOutput("table")
                                  ) 
                              )
                          )),
                 tabPanel(title = i18n$t("Bar Graph 1"),
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput("cat_var_1",i18n$t("Categorical Variable (X)"),choices="Not Selected"),
                                    prettySwitch("legend",
                                                 label = i18n$t("Legend"),
                                                 value = FALSE,
                                                 status = "primary",
                                                 fill = TRUE
                                    ),
                                    br(),
                                    br(),
                                    textInput("title1", i18n$t("Graph Title"), " "),
                                    br(),
                                    br(),
                                    actionBttn("run_button",i18n$t("Graph it!"), style = "simple", color = "primary", size = "sm")
                                ),
                                mainPanel(
                                    br(),
                                    plotOutput("plot_1"),
                                    br(),
                                    downloadBttn('downloadplot1',i18n$t('Download Graph'), style = "simple", color = "primary", size = "sm"),
                                    br(),
                                    br(),
                                    textAreaInput("story1", "Data Story Notes", width = "950px", height = "200px"),
                                    br(),
                                    downloadBttn('downloadstory1', i18n$t('Download Story'), style = "simple", color = "primary", size = "sm"),
                                    br()

                                )
                            )
                        )
                       ),


                    tabPanel(title = i18n$t("Bar Graph 2"),
                             fluidPage(
                                 sidebarLayout(
                                     sidebarPanel(
                                         selectInput("cat_var_2",i18n$t("Categorical Variable (X)"), choices="Not Selected"),
                                         selectInput("num_var_3",i18n$t("Numeric Variable (Y)"), choices="Not Selected"),
                                         prettySwitch("legend2",
                                                      label = i18n$t("Legend"),
                                                      value = FALSE,
                                                      status = "primary",
                                                      fill = TRUE
                                         ),
                                         br(),
                                         br(),
                                         textInput("title3", i18n$t("Graph Title"), " "),
                                         br(),
                                         br(),
                                         actionBttn("run_button_3",i18n$t("Graph it!"), style = "simple", color = "primary", size = "sm")
                                     ),
                                     mainPanel(
                                         br(),
                                         plotOutput("plot_3"),
                                         br(),
                                         downloadBttn('downloadplot3',i18n$t('Download Graph'), style = "simple", color = "primary", size = "sm"),
                                         br(),
                                         br(),
                                         textAreaInput("story3", "Data Story Notes", width = "950px", height = "200px"),
                                         br(),
                                         downloadBttn('downloadstory3', i18n$t('Download Story'), style = "simple", color = "primary", size = "sm"),
                                         br()
                    
                                     )
                                 )
                             )),
                 
                 tabPanel(title = i18n$t("Bar Graph 3"),
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("cat_var_3",i18n$t("Categorical Variable (X)"), choices="Not Selected"),
                                      selectInput("cat_var_4",i18n$t("Grouping Variable"), choices="Not Selected"),
                                      #prettySwitch("legend3",
                                                   #label = i18n$t("Legend"),
                                                   #value = FALSE,
                                                   #status = "primary",
                                                   #fill = TRUE
                                      #),
                                      br(),
                                      br(),
                                      textInput("title4", i18n$t("Graph Title"), " "),
                                      br(),
                                      br(),
                                      actionBttn("run_button_4",i18n$t("Graph it!"), style = "simple", color = "primary", size = "sm")
                                  ),
                                  mainPanel(
                                      br(),
                                      plotOutput("plot_4"),
                                      br(),
                                      downloadBttn('downloadplot4',i18n$t('Download Graph'), style = "simple", color = "primary", size = "sm"),
                                      br(),
                                      br(),
                                      textAreaInput("story4", "Data Story Notes", width = "950px", height = "200px"),
                                      br(),
                                      downloadBttn('downloadstory4', i18n$t('Download Story'), style = "simple", color = "primary", size = "sm"),
                                      br()
                                      
                                  )
                              )
                          )),
                    

                    tabPanel(title = i18n$t("Scatter Plot"),
                         fluidPage(
                             sidebarLayout(
                                 sidebarPanel(
                                     selectInput("num_var_1",i18n$t("Numeric Variable (X)"),choices = "Not Selected"),
                                     selectInput("num_var_2",i18n$t("Numeric Variable (Y)"),choices = "Not Selected"),
                                     selectInput("group_var",i18n$t("Grouping Variable"),choices = "Not Selected"),
                                     selectInput("label_var",i18n$t("Labels"), choices = "Not Selected"),
                                     br(),
                                     br(),
                                     textInput("title2", i18n$t("Graph Title"), " "),
                                     br(),
                                     br(),
                                     actionBttn("run_button_2",i18n$t("Graph it!"),icon=icon("chart-line"), style = "simple", color = "primary", size = "sm")
                                 ),
                    
                                 mainPanel(
                                     br(),
                                     plotOutput("plot_2", hover = hoverOpts(id = "plot_hover")),
                                     br(),
                                     downloadBttn('downloadplot2',i18n$t('Download Graph'), style = "simple", color = "primary", size = "sm"),
                                     br(),
                                     br(),
                                     textAreaInput("story2", "Data Story Notes", width = "950px", height = "200px"),
                                     br(),
                                     downloadBttn('downloadstory2', i18n$t('Download Story'), style = "simple", color = "primary", size = "sm"),
                                     br()
                    
                                 )
                    
                             )
                         )))
                                    
                                
     


# Define server logic 
server <- function(input, output, session) {
    
    #plot theme 
    #theme_set(theme_pander())
    
    observeEvent(input$selected_language, {
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selected_language)
    })
    
    data_input <- reactive({
        req(input$file)
        
        tryCatch(
            {
                
                if (tolower(tools::file_ext(input$file$datapath)) == "xlsx") { 
                    clean_data <- read_excel(input$file$datapath)
                }
                
                else {
                    clean_data <- read.csv(input$file$datapath)
                }
                
                names(clean_data) <- make.names(names(clean_data),unique = TRUE)
                
                clean_data
                
            },
            
            error = function(e) {
                stop(safeError(e))
            }
        )
        
    })
    
    
    output$table <- DT::renderDataTable({
        
        data_input() %>%
            datatable(
                # filter = "bottom",
                #server = TRUE, 
                rownames = FALSE,
                class = "cell-border stripe",
                options = list(
                    paging = FALSE,
                    # searching = FALSE,
                    autoWidth = TRUE)
            )
    })
    
    # observeEvent(data_input(), {
    #     appendTab(inputId = "tabs",  tab = tabPanel(title = i18n$t("Bar Graph 1"),
    #                                           fluidPage(
    #                                               sidebarLayout(
    #                                                   sidebarPanel(
    #                                                       selectInput("cat_var_1",i18n$t("Categorical Variable (X)"),choices="Not Selected"),
    #                                                       prettySwitch("legend",
    #                                                                    label = i18n$t("Legend"),
    #                                                                    value = FALSE,
    #                                                                    status = "primary",
    #                                                                    fill = TRUE
    #                                                       ),
    #                                                       br(),
    #                                                       br(),
    #                                                       textInput("title1", i18n$t("Graph Title"), " "),
    #                                                       br(),
    #                                                       br(),
    #                                                       actionBttn("run_button",i18n$t("Graph it!"), style = "simple", color = "primary", size = "sm")
    #                                                   ),
    #                                                   mainPanel(
    #                                                       br(),
    #                                                       plotOutput("plot_1"),
    #                                                       br(),
    #                                                       downloadBttn('downloadplot1',i18n$t('Download Graph'), style = "simple", color = "primary", size = "sm"),
    #                                                       br(),
    #                                                       br(),
    #                                                       textAreaInput("story1", "Data Story Notes", width = "950px", height = "200px"),
    #                                                       br(),
    #                                                       downloadBttn('downloadstory1', i18n$t('Download Story'), style = "simple", color = "primary", size = "sm"),
    #                                                       br()
    #                                                       
    #                                                   )
    #                                               )
    #                                           )
    #                                          )
    #               )
    #     appendTab(inputId = "tabs", tab =  
    #                   tabPanel(title = i18n$t("Bar Graph 2"), 
    #                            fluidPage(
    #                                sidebarLayout(
    #                                    sidebarPanel(
    #                                        selectInput("cat_var_2",i18n$t("Categorical Variable (X)"), choices="Not Selected"),
    #                                        selectInput("num_var_3",i18n$t("Numeric Variable (Y)"), choices="Not Selected"),
    #                                        prettySwitch("legend2",
    #                                                     label = i18n$t("Legend"),
    #                                                     value = FALSE,
    #                                                     status = "primary",
    #                                                     fill = TRUE
    #                                        ),
    #                                        br(),
    #                                        br(),
    #                                        textInput("title3", i18n$t("Graph Title"), " "),
    #                                        br(),
    #                                        br(),
    #                                        actionBttn("run_button_3",i18n$t("Graph it!"), style = "simple", color = "primary", size = "sm")
    #                                    ),
    #                                    mainPanel(
    #                                        br(),
    #                                        plotOutput("plot_3"),
    #                                        br(),
    #                                        downloadBttn('downloadplot3',i18n$t('Download Graph'), style = "simple", color = "primary", size = "sm"),
    #                                        br(),
    #                                        br(),
    #                                        textAreaInput("story3", "Data Story Notes", width = "950px", height = "200px"),
    #                                        br(),
    #                                        downloadBttn('downloadstory3', i18n$t('Download Story'), style = "simple", color = "primary", size = "sm"),
    #                                        br()
    #                                        
    #                                    )
    #                                )
    #                            ))
    #               )
    #     appendTab(inputId = "tabs",  
    #               tabPanel(title = i18n$t("Scatter Plot"), 
    #                        fluidPage(
    #                            sidebarLayout(
    #                                sidebarPanel(
    #                                    selectInput("num_var_1",i18n$t("Numeric Variable (X)"),choices = "Not Selected"),
    #                                    selectInput("num_var_2",i18n$t("Numeric Variable (Y)"),choices = "Not Selected"),
    #                                    selectInput("group_var",i18n$t("Grouping Variable"),choices = "Not Selected"),
    #                                    selectInput("label_var",i18n$t("Labels"), choices = "Not Selected"),
    #                                    br(),
    #                                    br(),
    #                                    textInput("title2", i18n$t("Graph Title"), " "),
    #                                    br(),
    #                                    br(),
    #                                    actionBttn("run_button_2",i18n$t("Graph it!"),icon=icon("chart-line"), style = "simple", color = "primary", size = "sm")
    #                                ),
    #                                
    #                                mainPanel(
    #                                    br(),
    #                                    plotOutput("plot_2", hover = hoverOpts(id = "plot_hover")),
    #                                    br(),
    #                                    downloadBttn('downloadplot2',i18n$t('Download Graph'), style = "simple", color = "primary", size = "sm"),
    #                                    br(),
    #                                    br(),
    #                                    textAreaInput("story2", "Data Story Notes", width = "950px", height = "200px"),
    #                                    br(),
    #                                    downloadBttn('downloadstory2', i18n$t('Download Story'), style = "simple", color = "primary", size = "sm"),
    #                                    br()
    #                                    
    #                                )
    #                                
    #                            )
    #                        ),
    #                        
    #                        fluidRow(
    #                            column(width = 3,
    #                                   textOutput("coordinate"),
    #                                   verbatimTextOutput("hover_info")
    #                            )
    #                        )
    #               ))
    # })

    observeEvent(data_input(),{
        cat_subset <- select_if(data_input(), is.character)
        choices <- c(names(cat_subset))
        updateSelectInput(inputId = "cat_var_1", choices = choices)
    })
    
    observeEvent(data_input(),{
        cat_subset <- select_if(data_input(), is.character)
        num_subset <- select_if(data_input(), is.numeric)
        choices <- c(names(cat_subset))
        choices2 <- c(names(num_subset))
        updateSelectInput(inputId = "cat_var_2", choices = choices)
        updateSelectInput(inputId = "num_var_3", choices = choices2)
    })
    
    observeEvent(data_input(),{
        num_subset <- select_if(data_input(), is.numeric)
        cat_subset <- select_if(data_input(), is.character)
        choices <- c(names(num_subset))
        choices2 <- c(names(cat_subset))
        updateSelectInput(inputId = "num_var_1", choices = choices)
        updateSelectInput(inputId = "num_var_2", choices = choices)
        updateSelectInput(inputId = "group_var", choices = c("Not Selected", choices2))
        updateSelectInput(inputId = "label_var", choices = c("Not Selected", choices2))
    })
    
    observeEvent(data_input(),{
        cat_subset <- select_if(data_input(), is.character)
        choices <- c(names(cat_subset))
        updateSelectInput(inputId = "cat_var_3", choices = choices)
        updateSelectInput(inputId = "cat_var_4", choices = choices)
    })
    
    
    observeEvent(data_input(), {
        show_alert(
            title = "Upload Successful",
            text = "You are ready to select a graph!",
            type = "success"
        )
        
    })
    
    cat_var_1 <- eventReactive(input$run_button,input$cat_var_1)
    
    cat_var_2 <- eventReactive(input$run_button_3,input$cat_var_2)
    num_var_3 <- eventReactive(input$run_button_3,input$num_var_3)
    
    num_var_1 <- eventReactive(input$run_button_2,input$num_var_1)
    num_var_2 <- eventReactive(input$run_button_2,input$num_var_2)
    group_var <- eventReactive(input$run_button_2,input$group_var)
    label_var <- eventReactive(input$run_button_2,input$label_var)
    
    cat_var_3 <- eventReactive(input$run_button_4,input$cat_var_3)
    cat_var_4 <- eventReactive(input$run_button_4,input$cat_var_4)
    
    
    plot_1 <- eventReactive(input$run_button,{
        draw_plot_1(data_input(), cat_var_1())
    })
    
    plot_2 <- eventReactive(input$run_button_2,{
        draw_plot_2(data_input(), num_var_1(), num_var_2(), group_var(), label_var())
    })
    
    plot_3 <- eventReactive(input$run_button_3,{
        draw_plot_3(data_input(), cat_var_2(), num_var_3())
    })
    
    plot_4 <- eventReactive(input$run_button_4,{
        draw_plot_4(data_input(), cat_var_3(), cat_var_4())
    })
    
    
    #BAR CHART OF ONE CAT VARIABLE WITH COUNTS, BAR CHART
    
    draw_plot_1 <- function(data_input, cat_var_1){
        
        if(cat_var_1 != "Not Selected" & 
           input$legend
        ){
            ggplot(data = data_input, 
                   aes_string(x = cat_var_1, fill = cat_var_1
                   )) +
                geom_bar(alpha = .7) +
                theme_clean(base_size = 20) +
                theme(axis.text.x=element_text(angle=90,hjust=1)) +
                ggtitle("default_label") +
                ggtitle(req(input$title1))
        }
        
        else if(cat_var_1 != "Not Selected"  
        ){
            ggplot(data = data_input, 
                   aes_string(x = cat_var_1, fill = cat_var_1
                   )) +
                geom_bar(alpha = .7) +
                theme_clean(base_size = 20) +
                theme(axis.text.x=element_text(angle=90,hjust=1)) +
                theme(legend.position = "none") +
                ggtitle("default_label") +
                ggtitle(req(input$title1))
        }
    }
    
    
    #BAR CHART OF ONE CAT VARIABLE WITH COUNTS, BAR CHART 2A
    
    draw_plot_3 <- function(data_input, cat_var_2, num_var_3){
        
        if(cat_var_2 != "Not Selected" & 
           input$legend2
        ){
            
            ggplot(data = data_input, 
                   aes_string(x = paste0("fct_reorder(", cat_var_2, "," , num_var_3,")"), y = num_var_3, fill = cat_var_2
                   )) +
                geom_bar(stat = "identity", alpha = .7) +
                theme_clean(base_size = 20) +
                theme(axis.text.x=element_text(angle=90,hjust=1)) +
                xlab(cat_var_2) +
                stat_summary(fun = sum, aes(label = ..y..), geom = "text", vjust = -.3) +
                scale_y_continuous(expand = expansion(add = c(0, 5))) +
                ggtitle("default_label") +
                ggtitle(req(input$title3))
        }
        else if(cat_var_2 != "Not Selected"  
        ){
            ggplot(data = data_input, 
                   aes_string(x = paste0("fct_reorder(", cat_var_2, "," , num_var_3,")"), y = num_var_3, fill = cat_var_2
                   )) +
                geom_bar(stat = "identity", alpha = .7) +
                theme_clean(base_size = 18) +
                theme(axis.text.x=element_text(angle=90,hjust=1)) +
                theme(legend.position = "none") +
                xlab(cat_var_2) +
                stat_summary(fun = sum, aes(label = ..y..), geom = "text", vjust = -.3) +
                scale_y_continuous(expand = expansion(add = c(0, 5))) +
                ggtitle("default_label") +
                ggtitle(req(input$title3))
        }
    }
    
    
    #SCATTER PLOT WITH TWO NUMERIC VARIABLES 
    draw_plot_2 <- function(data_input, num_var_1, num_var_2, group_var, label_var){
        
        ifelse (num_var_1 != "Not Selected" &
                    num_var_2 != "Not Selected" &
                    group_var != "Not Selected" &
                    label_var != "Not Selected",
                
                {
                    the_graph <- ggplot(data = data_input,
                                        aes_string(x = num_var_1 , y = num_var_2, color = group_var
                                        )) +
                        geom_point(size = 4) +
                        geom_text_repel(aes_string(label = label_var)) +
                        theme_clean(base_size = 18) +
                        ggtitle("default_label") +
                        ggtitle(req(input$title2))
                    
                    
                }, 
                
                ifelse (num_var_1 != "Not Selected" &
                            num_var_2 != "Not Selected" &
                            label_var != "Not Selected",
                        
                        {
                            the_graph <- ggplot(data = data_input,
                                                aes_string(x = num_var_1 , y = num_var_2
                                                )) +
                                geom_point(size = 4) +
                                geom_text_repel(aes_string(label = label_var)) +
                                theme_clean(base_size = 18) +
                                ggtitle("default_label") +
                                ggtitle(req(input$title2))
                            
                            
                        }, 
                        
                        ifelse(num_var_1 != "Not Selected" &
                                   num_var_2 != "Not Selected" &
                                   group_var != "Not Selected" , 
                               
                               {
                                   
                                   the_graph <- ggplot(data = data_input,
                                                       aes_string(x = num_var_1 , y = num_var_2, color = group_var
                                                       )) +
                                       geom_point(size = 4) +
                                       theme_clean(base_size = 18) +
                                       ggtitle("default_label") +
                                       ggtitle(req(input$title2))
                                   
                               }, 
                               
                               ifelse(num_var_1 != "Not Selected" &
                                          num_var_2 != "Not Selected", 
                                      
                                      {
                                          
                                          the_graph <- ggplot(data = data_input,
                                                              aes_string(x = num_var_1 , y = num_var_2
                                                              )) +
                                              geom_point(size = 4, color = sample(c("violet" , "blue" , "magenta" , "green", "red"), 1), alpha = .7) +
                                              theme_clean(base_size = 18) +
                                              ggtitle("default_label") +
                                              ggtitle(req(input$title2))
                                          
                                          
                                      },
                                      
                                      the_graph <- "NA" ))))
        
        the_graph
    }
    
    #BAR CHART OF TWO CAT VARIABLES SIDE BY SIDE, BAR CHART 2B
    
    draw_plot_4 <- function(data_input, cat_var_3, cat_var_4){
       
            ggplot(data = data_input, 
                   aes_string(x = cat_var_3, fill = cat_var_4
                   )) +
                geom_bar(position = "dodge", alpha = .7) +
                theme_clean(base_size = 20) +
                theme(axis.text.x=element_text(angle=90,hjust=1)) +
                theme(legend.position = "right") +
                ggtitle("default_label") +
                ggtitle(req(input$title4))
        
    }
    
    
    
    output$plot_1 <- renderPlot(plot_1())
    output$plot_2 <- renderPlot(plot_2())
    output$plot_3 <- renderPlot(plot_3())
    output$plot_4 <- renderPlot(plot_4())
    
    # output$coordinate <- renderText({ 
    #     "Hover over Points to see X and Y Coordinates"
    # })
    # 
    # output$hover_info <- renderPrint({
    #     req(input$plot_hover)
    #     x <- round(input$plot_hover$x, 0)
    #     y <- round(input$plot_hover$y, 0)
    #     cat("[", x, ", ", y, "]", sep = "")
    # })
    
    #ggarrange(plot_1(), )
    
    output$downloadplot1 <- downloadHandler(
        filename = function(){
            paste('bargraph1', '.png', sep = '')
        },
        content = function(file){
            req(plot_1())
            ggsave(file, plot = plot_1(), device = 'png')
        }
    )
    
    output$downloadplot2 <- downloadHandler(
        filename = function(){
            paste('scatterplot', '.png', sep = '')
        },
        content = function(file){
            req(plot_2())
            ggsave(file, plot = plot_2(), device = 'png')
        }
    )
    
    output$downloadplot3 <- downloadHandler(
        filename = function(){
            paste('bargraph2', '.png', sep = '')
        },
        content = function(file){
            req(plot_3())
            ggsave(file, plot = plot_3(), device = 'png')
        }
    )
    
    output$downloadplot4 <- downloadHandler(
        filename = function(){
            paste('bargraph3', '.png', sep = '')
        },
        content = function(file){
            req(plot_4())
            ggsave(file, plot = plot_4(), device = 'png')
        }
    )
    
    output$downloadstory1 <- downloadHandler(
        filename = function(){
            paste("story1", ".txt", sep='')
        },
        content = function(file){
            cat(input$story1, file=file)
            
        }
    )
    
    output$downloadstory2 <- downloadHandler(
        filename = function(){
            paste("story2", ".txt", sep='')
        },
        content = function(file){
            cat(input$story2, file=file)
            
        }
    )
    
    output$downloadstory3 <- downloadHandler(
        filename = function(){
            paste("story3", ".txt", sep='')
        },
        content = function(file){
            cat(input$story3, file=file)
            
        }
    )
    
    output$downloadstory4 <- downloadHandler(
        filename = function(){
            paste("story4", ".txt", sep='')
        },
        content = function(file){
            cat(input$story4, file=file)
            
        }
    )
    
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)


