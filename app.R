library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
#library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)

#setwd("C:/MDS/Block 5/DSCI532/labs/individual/mental_health_in_tech_dashboard_r")

app <- Dash$new(
  external_stylesheets = dbcThemes$BOOTSTRAP
  )

logo <- "https://cdn-icons-png.flaticon.com/512/2017/2017231.png"
data <- read.csv('data/survey.csv')


chart_tpye = c("Bar","Pie")
genderlist = c("Male","Female","Other")
sizelist = c("1-5","6-25","26-100","100-500","500-1000","More than 1000")
agelist = c("18-24","25-34","35-44","45-54","55+")


tabs <- htmlDiv(
  list(
    dbcTabs(
      list(
        dbcTab(label = "Summary Overview", tab_id = "tab-1"),
        dbcTab(label = "Interactive View", tab_id = "tab-2"),
        dbcTab(label = "Map View", tab_id = "tab-3")
      ),
      id = "tabs",
      active_tab = "tab-2"
    ),
    div(id = "content")
  )
)

tab1 <- dbcCard(
  dbcCardBody(
    list(
      p("This is tab 1!", className = "card-text"),
      dbcButton("Click here", color = "success")
    )
  ),
  className = "mt-3"
)


qdict = tibble("Q11"="Does your employer provide resources to learn more about mental health issues and how to seek help?",
             "Q12"="Is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?",
             "Q13"="How easy is it for you to take medical leave for a mental health condition?",
             "Q14"="Do you think that discussing a mental health issue with your employer would have negative consequences?",
             "Q15"="Do you think that discussing a physical health issue with your employer would have negative consequences?",
             "Q16"="Would you be willing to discuss a mental health issue with your coworkers?",
             "Q17"="Would you be willing to discuss a mental health issue with your direct supervisor(s)?",
             "Q18"="Would you bring up a mental health issue with a potential employer in an interview?",
             "Q19"="Would you bring up a physical health issue with a potential employer in an interview?",
             "Q20"="Do you feel that your employer takes mental health as seriously as physical health?",
             "Q21"="Have you heard of or observed negative consequences for coworkers with mental health conditions in your workplace?")

tab2 <- htmlDiv(
  list(
    dbcContainer(
      list(
        htmlBr(),
        dbcRow(
          list(
            dbcCol(
              list(
                dbcToast(
                  list(
                    htmlH1("Mental Health in Tech Dashboard"),
                    htmlBr(),
                    htmlP(
                      list(
                        htmlH4("Plot type"),
                        htmlBr(),
                        dccRadioItems(
                          id='chart-widget',
                          options = c("Pie", "Bar"),
                          value="Pie",
                          labelStyle=list(display="block")
                        ),
                        htmlBr(),
                        
                        htmlH4("Survey questions"),
                        dccDropdown(
                          id='q-widget',
                          value = colnames(qdict)[1],
                          options = qdict %>% colnames %>% purrr::map(function(col) list(label = unlist(qdict[col], use.names = FALSE), value = col)),
                          optionHeight = 100
                        ),
                        htmlBr(),
                        
                        htmlH4("Gender"),
                        dccDropdown(
                          id = 'gender-widget',
                          value = genderlist,
                          options = genderlist %>% purrr::map(function(col) list(label = col, value = col)),
                          multi = TRUE
                        ),
                        htmlBr(),
                        
                        htmlH4("Age"),
                        dccDropdown(
                          id = 'age-widget',
                          value = agelist,
                          options = agelist %>% purrr::map(function(col) list(label = col, value = col)),
                          multi = TRUE
                        ),
                        htmlBr(),
                        
                        htmlH4("Company size"),
                        dccDropdown(
                          id = 'size-widget',
                          value = sizelist,
                          options = sizelist %>% purrr::map(function(col) list(label = col, value = col)),
                          multi = TRUE
                        )
                      )
                    )
                  )
                )
              ),width=5,style=list('margin-right'='0px','margin-left'='20px')
            ),
            dbcCol(
              list(
                dbcToast(
                  list(
                    htmlH3(id='display-question'),
                    htmlBr(),
                    htmlBr(),
                    dccGraph(
                      id='interactive',style=list('margin-left'='100px','width'='100%','height'='700px'),
                    )
                  ),style=list('position'='center', 'width'='100%', 'height'='800px')
                )
              ),class_name = 'chart-box', style=list('margin-right'='0px','margin-left'='-150px')
            )
          )
        )
      )
    )
  )
)

tab3 <- dbcCard(
  dbcCardBody(
    list(
      p("This is tab 3!", className = "card-text"),
      dbcButton("Don't click here", color = "danger")
    )
  ),
  className = "mt-3",
)


navbar <- dbcNavbar(
  dbcContainer(
    list(
      a(
        # Use row and col to control vertical alignment of logo / brand
        dbcRow(
          list(
            dbcCol(img(src = logo, height = "30px")),
            dbcCol(dbcNavbarBrand("Mental Health in Tech Dashboard", className = "ms-2"))
          ),
          align = "left",
          className = "g-0"
        ),
        href = "https://github.com/UBC-MDS/mental_health_in_tech_dashboard_r",
        style = list("textDecoration" = "none")
      )
    )
  ),
  color = "lightskyblue",
  dark = TRUE
)



app %>% set_layout(
  dbcRow(
    list(
      navbar,        
      tabs
    )
  )
)


app$callback(
  output("content", "children"),
  list(input("tabs", "active_tab")),
  function(at) {
    if (at == "tab-1") {
      return(tab1)
    } else if (at == "tab-2") {
      return(tab2)
    } else if (at == "tab-3") {
      return(tab3)
    }
    return(p("This should not ever be displayed"))
  }
)


pie_chart <- function(df, col, title=NULL, colors=NULL){
  col2 <- sym(col)
  df_p <- df %>% 
    group_by({{ col2 }}) %>% 
    summarize(cnt=n()/nrow(df)) %>%
    ungroup()
  
  if (is.null(title)){
    title = col
  }
  
  if (is.null(colors)){
    fig <- plot_ly(df_p, 
                   labels = as.formula(paste0('~', col)), 
                   values = ~cnt,
                   hoverinfo='label+percent',
                   textinfo='label+percent',
                   type = 'pie',
                   width = 650,
                   height = 550
    )
  } else {
    fig <- plot_ly(df_p, 
                   labels = as.formula(paste0('~', col)), 
                   values = ~cnt,
                   hoverinfo='label+percent',
                   textinfo='label+percent',
                   type = 'pie',
                   width = 650,
                   height = 550,
                   marker=list(colors=colors)
    )
  }
  fig <- fig %>% layout(margin=list(l=0, r=0, t=0, b=0))
}


app$callback(
  output("display-question", 'children'),
  list(input('q-widget', "value")),
  function(question){
    htmlH3(qdict[question] %>% pull())
  }
)
  
app$callback(
  output("interactive", 'figure'),
  list(input('q-widget', "value"),
       input('chart-widget', "value"),
       input('gender-widget', "value"),
       input('age-widget', "value"),
       input('size-widget', "value")),
  function(question, chart_type, gender, age, size){
    #print(question)
    df2 <- data %>% filter(Gender %in% unlist(gender,use.names=FALSE)
                            & Age %in% unlist(age,use.names=FALSE)
                            & Q5 %in% unlist(size,use.names=FALSE))
    if (chart_type == "Pie"){
      colors = c('skyblue','navy','lightgray')
      fig<-pie_chart(df2, question, title=NULL, colors)
      fig
      
    }
  }

)

app$run_server()






