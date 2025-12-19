library(ggplot2); library(tidyr); library(dplyr); library(magrittr); library(janitor)
library(shiny); library(htmltools); library(DT); library(bslib)

weight_choose_function = numericInput # so we can change what function we want to use for our inputs

datafiles = system("ls | grep .csv | sed -e 's/\\..*$//'", intern = TRUE)
datafiles = datafiles[-which("missingness" == datafiles)]
names(datafiles) = datafiles
idx = which("PCA_means" == names(datafiles))
names(datafiles)[idx] = paste0(names(datafiles[idx]), " (recommended)")
#datafiles = clean_names(datafiles, case="sentence")

using_data = read.csv("PCA_means.csv")
# Temporarily hardcoding in the data to load, I'm assuming that these are pre-processed as much as possible. In particular, each group is the distance from the respective school to Reed.
using_data %<>% 
  select(-X) %>% 
  pivot_wider(names_from = group, values_from = dist_standard)

group_vector = using_data %>%
  .[,3:ncol(.)] %>% 
  colnames() # get the order of weights in the dataset we use

weight_determination = function(input) { # Function to pull weights out of the user input
  weights = c(
    input$weight_1,
    input$weight_2,
    input$weight_3,
    input$weight_4,
    input$weight_5,
    input$weight_6,
    input$weight_7,
    input$weight_8
  )
  for(i in 1:8) {
    if(weights[i] == 0) {
      weights[i] = 0.000001 # it freaks out if any of the weights are zero, so set them to be negligable
    }
  }
  weight_sum = sum(weights)
  if (weight_sum == 0) {
    return(rep(c(1), 8))
  }
  weights = weights/weight_sum
  
  return(weights)
}
sidebar_fun = sidebar(
      h1("Welcome to our shiny app!"),
      p("This is a custom weighting tool to find possible comparators to Reed College. For a better description, see the \"Description\" tab. To adjust weights, change the values in the following text entries and the table will update with a new list."),
      selectInput("specified_data", "What method would you like to use", datafiles),
      actionButton("reset_weights", "Reset Weights"),
      weight_choose_function("weight_1", group_vector[1], min=0, max=256, 1),
      weight_choose_function("weight_2", group_vector[2], min=0, max=256, 1),
      weight_choose_function("weight_3", group_vector[3], min=0, max=256, 1),
      weight_choose_function("weight_4", group_vector[4], min=0, max=256, 1),
      weight_choose_function("weight_5", group_vector[5], min=0, max=256, 1),
      weight_choose_function("weight_6", group_vector[6], min=0, max=256, 1),
      weight_choose_function("weight_7", group_vector[7], min=0, max=256, 1),
      weight_choose_function("weight_8", group_vector[8], min=0, max=256, 1),
      p("Download your list here:"),
      downloadButton("download_data"),
      p("See the breakdown of distances here"),
      downloadButton("specific_weight_download"),
      width = 450
    )

ui = page_navbar(
  title = "Comparator College Specification",
  sidebar = sidebar_fun,
  nav_panel(
    title = "Main", 
    h1("Selecting Normalized Weights"),
    # h2(" Weights"),
    tableOutput("weight_df"),
    h2("Comparator College list"),
    DTOutput("user_college_dataframe")
  ),
  nav_panel(
    title = "Description",
    h1("Description"),
    h3("Purpose"),
    p("This is the tool we made for comparing colleges to Reed College. Details are in the technical report which can be found from the 'Links' tab. To get a list of comparator candidates, specify weights for each group. A variable group is a combination of different attributes which are summarized by a single value. For example, test scores represents various statistics relating to SAT test scores and ACT test scores."),
    h3("Functionality"),
    p("By default, all groups are weighted the same. To weight a group to be twice as important as the others is to make those variables represented twice. By default, the data table shows schools in order of closeness to Reed College. 
    Individual schools can be found using the search bar to the top right of the table. Reverse order of College, Distance, and Rank can be found by toggling a column's text.
    "),
    h3("Disclaimer"),
    p("This is meant to be preliminary. This tool does not take into account more qualitative information like school culture or historical context. Likewise, it excludes more niche attributes such as religious affiliation or location."),
    p("We would also like to note that missingness of data may create bias in our methods. In the table the 'missingness' column is the proportion of data missing for that school. Some schools have a considerable amount of data missing while some schools have all data included. The data in its entirety has the most missingness in the Alumni Donations and Test Scores groups."),
    p("We will not discuss the philosophical differences between Z-scores and Principal Component Analysis here; however, it is suffice to say that both methods accomplish this task in a reasonably similar way, although produce slightly different results. 
    It is worth noting that the 'Distance' values, displayed in the table on the 'Main', are not comparable between methods.    
    We tried to keep them to similar scales, but a school having a distance of 1 from Reed College in Z-scores and 0.1 in PCA does not indicate that PCA ranks this school as closer unless it is closer relative to other schools. Similarly, a schools distance across difference weights is not truly indicative of a difference, we again try to normalize differences to be on similar scales, but there are no guarantees. 
    In summary, distance values are only informative relative to other school's distances. A school which has twice the distance from Reed College as another is a meaningful difference, for example.")
  ),
#  nav_spacer(),
  nav_menu(
    title = "Links",
    nav_item(
      htmltools::tags$a(
	shiny::icon("github"), 
	"Github", 
	href = "https://github.com/pearce790/Math343_ComparatorColleges", 
	target = "_blank"
      )  
    ),
    nav_item(
      htmltools::tags$a(
	shiny::icon("fa-solid fa-file-pdf"),
	"Report",
  
  ## MAKE SURE WE UPDATE WHEN REPORT IS DONE
	href = "https://hi",
	target = "_blank"
      )
    ),
    nav_item(
      htmltools::tags$a(
	shiny::icon("pencil"),
	"Current Comparators",
	href = "https://www.reed.edu/ir/comparatorschools.html",
	target = "_blank"
      )
    )
  )
)
server = function(input, output, session) {
  observeEvent(input$reset_weights, {
    updateNumericInput(session, "weight_1", value = 1)
    updateNumericInput(session, "weight_2", value = 1)
    updateNumericInput(session, "weight_3", value = 1)
    updateNumericInput(session, "weight_4", value = 1)
    updateNumericInput(session, "weight_5", value = 1)
    updateNumericInput(session, "weight_6", value = 1)
    updateNumericInput(session, "weight_7", value = 1)
    updateNumericInput(session, "weight_8", value = 1)
  })
  output$the_first_weight = reactive({input$weight_1})
#  output$weights = weight_determination(input)
  output$test = reactive({input$specified_data})

  output$download_data = downloadHandler(
    filename = function() {
      paste0("comparator_list.csv")
    }, 
    content = function(file) {
      write.csv(user_dataframe_prelim(), file)
    })

  chosen_dataframe = reactive({
    read.csv(paste0(input$specified_data, ".csv")) %>% 
      dplyr::select(c(name, UnitID, group, dist_standard)) %>% 
#      mutate(dist_standard = ifelse(is.na(dist_standard), 0, dist_standard)) %>% 
      pivot_wider(names_from = group, values_from = dist_standard) %>% 
      left_join(read.csv("missingness.csv"))
  })

  output$weight_df = renderTable({
    weights = weight_determination(input)
    rtn_df = data.frame(x = 0)
    for (i in 1:8) {
      rtn_df[1,i] = round(weights[i], 2)
      colnames(rtn_df)[i] = group_vector[i]
    }
    rownames(rtn_df) = "Weights"
    return(rtn_df)
  })

  output$user_college_dataframe = renderDT({
    user_dataframe_prelim() %>% 
    DT::datatable(
      ., options = list(
	lengthMenu = list(c(31, 51,101, -1), c('30', '50', '100', 'All'))
      ),
      rowname = FALSE
    )
  })

  user_dataframe_prelim = reactive({
    weights = weight_determination(input)
    specified_weight_df = chosen_dataframe() %>%
      select(name, missingness)
    specified_weight_df$Distance = 0

#    return(chosen_dataframe())

    ## This is producing different distances than what's saved in our Data folder...
    for (i in 1:8) {
      temp_vec = chosen_dataframe()[[2+i]]
      temp_vec = temp_vec/sd(temp_vec) # first normalize variance
      temp_vec = weights[i] * temp_vec # then apply weights
      temp_vec = temp_vec^2	       # then square for adding
      temp_vec = sqrt(temp_vec)	       # then sqrt for dist calc
      specified_weight_df$Distance = sqrt((specified_weight_df$Distance)^2 + temp_vec^2)
    }

    specified_weight_df %<>% rename(
      College = name
    ) %>% 
    arrange(Distance) %>%
    mutate(Distance = Distance/(.$Distance %>% sort() %>% .[2])) # normalize distance so first real value has distance one

  specified_weight_df %<>%
    mutate(Distance = round(Distance, 3),
	   missingness = round(missingness, 3)) %>% 
    mutate(Rank = 0:(nrow(.)-1),
	   comparator = College %in% c(
	      "Agnes Scott College", "Bard College",
              "Bates College", "Beloit College", 
	      "Bryn Mawr College", "Carleton College",
              "Claremont McKenna College", "Colorado College",
	      "Davidson College", "Denison University",
              "Earlham College", "Grinnell College",
	      "Hamilton College", "Haverford College",
              "Kenyon College", "Lawrence University",
	      "Lewis & Clark College", "Macalester College",
              "Oberlin College", "Occidental College",
	      "Pitzer College", "Pomona College",
              "Sarah Lawrence College", "Swarthmore College",
	      "Trinity University", "University of Puget Sound",
              "Vassar College", "Wesleyan University",
	      "Whitman College", "Willamette University", 
	      "Reed College")) %>% 
    select(College, Rank, comparator, Distance, missingness) # mess with order
    
    return(specified_weight_df)
  })
  output$specific_weight_download = downloadHandler(
    filename = function() {
      paste0("weighted_distances.csv")
    }, 
    content = function(file) {
      specified_data = chosen_dataframe()
      weights = weight_determination(input)
      weight_coords = specified_data %>%
	select(name, missingness)
      for (i in 1:8) {
	temp_vec = specified_data[[2+i]]
        temp_vec = temp_vec/sd(temp_vec) # first normalize variance
        temp_vec = weights[i] * temp_vec # then apply weights
        temp_vec = temp_vec^2	       	 # then square for adding
        temp_vec = sqrt(temp_vec)        # then sqrt for dist calc
	weight_coords[ncol(weight_coords) + 1] = temp_vec
	colnames(weight_coords)[ncol(weight_coords)] = colnames(specified_data)[2+i]
      }
      write.csv(weight_coords, file)
    })
}

shinyApp(ui=ui, server=server)
