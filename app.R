library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(plotly)
library(gapminder)
library(readr)


app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

df <- read_csv("Data/test.csv")

# heatmap
make_heatmap <- function(){
    newdf <- df %>%
        filter(job %in% c('Truck / Tractor Driver', 'Salesman', 'Operative', 'Manager / Owner', 
                        'Laborer', 'Foremen', 'Farmer', 'Farm Laborer',
                        'Clerical Worker', 'Carpenter')) %>%
        mutate(year = factor(year)) %>%
        group_by(job, year) %>%
        summarise(total = sum(count)/1000)

    plot <- newdf %>%
        ggplot(aes(x = year, y = job)) +
        geom_tile(aes(fill = total)) +
        scale_fill_gradient(low = "#fff8fc", high = "#2e0563") +
        ylab("Job") +
        xlab("Year") +
        theme_bw() +
        scale_x_discrete(expand = expand_scale(mult = c(0, 0.01))) +
        scale_y_discrete(expand = expand_scale(mult = c(0, 0.05))) +
        ggtitle("Total employment over the years")+
        labs(fill = "Total Counts (Thousands)")
    
    ggplotly(plot)
}

heatmap_graph <- dccGraph(
    id = 'count-graph',
    figure = make_heatmap()
)



# dropdown
dropdown_data <- c('Clerical Worker','Farm Laborer','Farmer',
                         'Janitor','Laborer','Manager / Owner','Operative','value',
                         'Professional - Misc','Salesman','Truck / Tractor Driver')

jobsdropdown <- dccDropdown(
    id  = "jobs",
    options = lapply(dropdown_data, function(x) {
        list(label = x, value = x)
    }),
      value = "Janitor"
)

# line plot
make_line <- function(selected_jobs = dropdown_data){
    data <- dplyr::filter(df, job == selected_jobs) 

    p <- ggplot(data, aes(x=year, y=count, group=sex)) + 
        xlab("Year")+
        ylab("Count")+ 
        ggtitle("Change of number of jobs over the years")+ 
        geom_line(aes(color = sex))+ geom_point() 

    ggplotly(p)
    
}

line_graph <- dccGraph(
  id = 'job-graph',
  figure = make_line()
)

# bar plot
make_bar <- function(){
    p1 <- df %>% 
        mutate(year = factor(year)) %>%
        group_by(year, sex) %>%
        summarise(total = sum(count),
                percent = sum(perc)*100) %>%
        ggplot(aes(x = year, y = percent, fill = sex)) +
            geom_bar(stat = "identity") +
            ylab("Percentage")+
            xlab("Year") +
            scale_x_discrete(expand = expand_scale(mult = c(0, 0.01))) +
            scale_y_discrete(expand = expand_scale(mult = c(0, 0.001))) +
            ggtitle("Employment gender gap over the years")       
    ggplotly(p1)
}

bar_graph <- dccGraph(
    id = 'ratio-graph',
    figure = make_bar()
    
)



app$layout(htmlDiv(list(
  htmlH1('JOB ANALYZER', style = list('margin-left' =  '40%', 'margin-right' = '5%')),
  htmlIframe(height=10, width=10, style=list(borderWidth = 0)),
  htmlH3('This is an interactive dashboard analyzing the job market and comparing the changes between the two genders; males and females. 
  This app used vega job dataset. From 1850 till 2000 the data was collected for each decade (Except for 1890-1990).', style = list("backgroundColor" = "#e1edfa", 'margin-left' =  '5%', 'margin-right' = '5%')),
  htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
  dccTabs(id="tabs-example", value='tab-1', children=list(
    dccTab(label='Job Count', value='tab-1'),
    dccTab(label='Job Trend', value='tab-2'),
    dccTab(label='Gender Ratio', value='tab-3')
  )),
  htmlDiv(id='tabs-content-example') 
)))

app$callback(
  output = list(id='tabs-content-example', property = 'children'),
  params = list(input(id = 'tabs-example', property = 'value')),
  function(tab){
    if(tab == 'tab-1'){
      return(htmlDiv(list(
        htmlH5('This is a heatmap showing the change of the employment total count over the years. 
        Interact with this map by hovering over a point to get more details. 
        A comparison between men and women is possible by dragging over a region on the heatmap shown in the bar chart. 
        Only 10 jobs out of the 250 jobs that are in the dataset are selected for the purpose of the comparison.', style = list("backgroundColor" = "#e3d2f7", 'margin-left' =  '10%', 'margin-right' = '10%')),
        htmlIframe(height=20, width=50, style=list(borderWidth = 0)),
        htmlDiv(heatmap_graph, style = list('margin-left' =  '10%', 'margin-right' = '10%'))
             
      ))) 
    }  
    else if(tab == 'tab-2'){
      return(htmlDiv(list(
        htmlH5('This is a graph showing the trend of total count for a particular job for both sexes. 
        Interact with this graph by selecting a specific job from the 10 jobs in the dropdown menu.', style = list("backgroundColor" = "#e6f4fa", 'margin-left' =  '10%', 'margin-right' = '10%')),
        htmlIframe(height=20, width=10, style=list(borderWidth = 0)),
        htmlDiv(jobsdropdown, style = list('margin-left' =  '10%', 'margin-right' =  '60%')),
        htmlDiv(line_graph, style = list('margin-left' =  '10%', 'margin-right' = '10%')) 
  
      )))
    }
        else if(tab == 'tab-3'){
      return(htmlDiv(list(
        htmlH5('This is a graph showing the change in the labour force gender gap over time for all the jobs in the jobs dataset. 
        Interact with this graph by hovering over a bar to get the details of the actual percentage of the selected year and gender.', style = list("backgroundColor" = "#fae9e1", 'margin-left' =  '10%', 'margin-right' = '10%')),
        htmlIframe(height=20, width=10, style=list(borderWidth = 0)),
        htmlDiv(bar_graph, style = list('margin-left' =  '10%', 'margin-right' = '10%'))
      )))
    }
  }
)


app$callback(
  output=list(id = 'job-graph', property='figure'),
  params=list(input(id = 'jobs', property='value')),
  function(selected_jobs){
    make_line(selected_jobs) 
  })




app$run_server()

