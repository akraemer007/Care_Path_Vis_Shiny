library(shiny)
library(dplyr)
library("devtools")
library(Cairo)
install_github("looker/lookr")
library(LookR)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(magrittr)
library(stringr)
library(RPostgreSQL)

# setwd('/Users/andrewkraemer/Google\ Drive/WB\ -\ Analytics/Projects/R/CarePathViz')
source("LookRApiCredentials.R")

looker_setup(   id = IdLookR,
                secret = secretLookR,
                api_path = api_pathLookR
)

# Starting Look || pulls from "CareCardSummaryTidy" Look
CPD <- run_look(602)

CarePathData <- tbl_df(CPD) # so I don't have to rerun the look everytime
CarePathData <- as.data.frame(as.matrix(CarePathData),stringsAsFactors = F)
# # str(CarePathData)
# 
# save(CarePathData, file="CarePathData.Rda")

# load(file="CarePathData.Rda")

# Renaming CarePaths Sanely
names(CarePathData) <- gsub("care_cards\\.|patient_information\\.|organization_tree\\.", "",names(CarePathData))

numericFields <- c('completed_sum', 'completed_and_expired_sum', 'view_count_sum', 'unique_view_count', 'appears_offset') # to systemize this I could just have it look at the last x amount that I could update, since measures are always the last columns
CarePathData[ ,numericFields] <- lapply( numericFields, function(x) as.numeric( CarePathData[ ,x] ) )

# str(CarePathData)

# Cleaning --------------------------------------------------------

# use for dropdown tool
parent_orgs <- CarePathData %>% 
  distinct(organization_parent_name) %>% 
  mutate( organization_parent_name = substr( organization_parent_name, 0, 20))

CarePathData.Tidy <- CarePathData %>%
  filter( !is.na( appears_offset) ) %>%
  mutate( PrePostFlag <- factor( PrePostFlag, levels = c( 'Pre', 'Post' ) ),
          is_form <- factor( is_form, levels = c( 'No', 'Yes' ) ),
          CardForm = ifelse( is_form == 'Yes', 'Form', 'CareCard' ),
          organization_parent_name = substr( organization_parent_name , 0, 20 ),
          appears_offset = ifelse( appears_offset < -50 , -50, 
                                   ifelse( appears_offset > 90, 90, appears_offset ) ),
          guide_item_type = ifelse( is_form == 'Yes', 'Form',
                                    ifelse( required == 'Yes', 'CareCard - Required',
                                            'CareCard - Recommended' ) ),
          concat_title = paste( guide_item_type, title, ', ', title_extension),
          title_full = paste( title, ', ', title_extension),
          title_full = ifelse(
            substr( title_full, nchar( title_full ) -1, nchar( title_full ) ) == ', ', title,
            title_full ) #gets rid of title's with ", " at end
  ) %>%
  group_by(
    # guide_item_id,
    organization_parent_name,
    # guide_item_rank,
    care_path_name,
    title_full,
    CardForm,
    PrePostFlag,
    is_form,
    appears_offset,
    guide_item_type,
    conditional_card,
    concat_title
  )%>%
  summarise( total.completed_sum = sum( completed_sum ),
             total.completed_and_expired_sum = sum( completed_and_expired_sum ),
             total.view_count_sum = sum( view_count_sum )
             # total.pcp = sum( patient_care_path_count )
             # , count.pcp = n() figure out how to get a functional better count of pcp Looker says total PCP is 1855 for these clients
  ) %>%
  arrange( organization_parent_name, care_path_name, appears_offset, guide_item_type, concat_title ) %>%
  group_by( organization_parent_name, care_path_name, appears_offset ) %>%
  mutate(
    rank = min_rank( concat_title )
  )

b <- c(c(-8:7)*7,999)
l <- c(c(-7:8)*7)

CarePathData.Tidy.Req <- CarePathData.Tidy %>%
  ungroup() %>%
  filter(guide_item_type %in% c('CareCard - Required', 'Form')) %>%
  mutate(appears_offset_wk = cut(
    appears_offset,
    breaks = b,
    labels = l,
    include.lowest = TRUE
  )) %>%
  group_by(organization_parent_name, care_path_name, appears_offset_wk) %>%
  mutate(rank = factor(min_rank(concat_title)))

# shiny server ----------------------------------
shinyServer(function(input, output) {
  
  output$CarePath <- renderPlot({
    Client <- input$Client
    CarePathData.Tidy.Req.OneClient <- CarePathData.Tidy.Req %>% filter( organization_parent_name == Client )
    
    # draw the histogram with the specified number of bins
    ggplot( CarePathData.Tidy.Req.OneClient ,aes(x = appears_offset_wk, y = total.completed_sum/total.completed_and_expired_sum, group = rank))+
      # geom_bar( aes(fill = CardForm), position = position_dodge(width = 0.9), stat = 'identity', color="black") +
      geom_point( aes(color = CardForm), size = 3, position = position_dodge(width = 0.9)) +
      # geom_text( aes( label = guide_item_rank_num ), hjust = 0, vjust = 0 ) +
      facet_grid( care_path_name~. ) +
      scale_x_discrete( drop = FALSE ) +
      scale_y_continuous( labels = scales::percent, limits = c(0,1)) +
      geom_vline( aes(xintercept = which(levels(appears_offset_wk) == '0') )  ) + #adds solid line to chart
      # geom_text( x = 0, label = "\nSurgery", y = max(CarePathData.Tidy.Req$rank), family = 'sans', angle=0,  color = 'gray34', size = 3, alpha = 1/5 ) + #puts text next to line for surg date
      labs( title = 'CarePath Utilization', x = 'Days Offset from Surgery', y = 'Completion %') +
      theme_bw()
  })
})

