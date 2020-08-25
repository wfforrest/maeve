## ----load_R_packages, echo = FALSE, eval = TRUE-------------------------------
 suppressPackageStartupMessages( library( magrittr ) )
 suppressPackageStartupMessages( library(  ggplot2 ) )
 suppressPackageStartupMessages( library(    maeve ) )




## ----vismodegib_example_setup, echo = TRUE, eval = TRUE-----------------------

  maeve_reset() # make sure package options start at their defaults.
  maeve_options( ncol_value = 5 ) # with 10 groups, this facets to 2 rows & 5 columns.

  ## The high-dose group was observed post-treatment for several weeks.
  ## For this example, restrict to the three weeks of treatment common
  ## to all ten dose groups.
  ##
  vismo21 <- dplyr::filter( vismodegib, DAY_OF_STUDY <= 21 ) # restrict to days [0, 21].

  ##
  figure_with_raw_TUMOR_VOLUME <- maeve::draw_study( vismo21, y_label = 'raw tumor volumes in mm^3' )
  print( figure_with_raw_TUMOR_VOLUME )
  ##
  ## The 'endpoint_name' default value of 'TUMOR_VOLUME' can be overridden in-place by giving a 
  ## different endpoint_name to draw_study():
  figure_with_raw_BODY_WEIGHT <-
      maeve::draw_study( vismo21, endpoint_name = 'BODY_WEIGHT', y_label = 'body weight in g' )
  
  print( figure_with_raw_BODY_WEIGHT )


## ----tally_study_examples, eval = TRUE, echo = TRUE---------------------------

  tally_study( vismodegib )

  ## Tally the study, but include the count of partial responses (PR),
  ## defined as a mouse with a tumor that falls to 75% of its baseline
  ## at least once in the study.  Include log odds ratio estimate and
  ## standard error relative to the reference group (which is the first
  ## group by default).
  tally_study( vismodegib, response = 'PR', PR_threshold = .75 ) %>% format( digits = 3 )



## ----vismodegib_example_with_model_study, echo = TRUE, eval = TRUE------------

  maeve_reset() ## Reset package options to their defaults.
  maeve_options( metric = 'AUC', # area-under-curve group-summary metric, 'eGaIT' by default.
                 ncol_value = 5  # with 10 groups, this facets to 2 rows & 5 columns.
                )

  model_list    <- maeve::model_study( vismo21 )

  ## This next code fits the *same* model as the one line above.
  ## The options passed are normally accessed by 'model_study()'
  ## from 'maeve_options()'.  They are also needed by lots of
  ## subsequent functions, so managing them by keeping a common
  ## list in 'maeve_options()' simplifies the commands.
  model_verbose <- maeve::model_study( vismo21,
                                       ##
                                       group_name    = 'group_name',
                                       subject_ID    = 'animalID',
                                       x_name        = 'DAY_OF_STUDY',
                                       endpoint_name = 'TUMOR_VOLUME'
                                      )

  ## check that the predicted values match:
  identical( predict(    model_list[['md4_gamm4']]$gam ),
             predict( model_verbose[['md4_gamm4']]$gam )
            )
  


## ----vismodegib_example_with_predict_study, echo = TRUE, eval = TRUE----------

  predicted_data_frame <- maeve::predict_study( model_list )

  ## Among several other columns are the original four used in modeling and
  ## the 'y' column, where here y = log( 1 + TUMOR_VOLUME ):
  ##
  predicted_data_frame %>%
      dplyr::select( group_name, animalID, DAY_OF_STUDY, TUMOR_VOLUME, y ) %>%
      format( digits = 3 ) %>%
      head( 10 )
  


## ----vismodegib_example_with_draw_study, echo = TRUE, eval = TRUE-------------

  ## revise the raw data plot to draw the transformed variable with fitted values.
  ##
  figure_with_fits_by_dose <- maeve::draw_study( predicted_data_frame, endpoint_name = 'y', fit = 'spline' )

  print( figure_with_fits_by_dose )

  ## Add an overlay plot with all the group-level fits in one plot:
  figure_overlay <- draw_overlay( predicted_data_frame, legend_position_char = 'right' )

  print( figure_overlay )



## ----vismodegib_example_compare_groups_Identity_contrast, echo = TRUE, eval = TRUE, results = 'asis'----


 ## Generate a list with multcomp::glht() models, summary data frames, and simple figures:
 cg_Identity <- compare_groups( model_list, extended_output = TRUE ) # include, SEs, p-values, etc.

 ## The output is arranged into three sub-lists containing, respectively:
 ## (1) multcomp::glht() models,
 ## (2) summary data, and
 ## (3) basic figures of some summary results
 names( cg_Identity )
 
 ## Here is a summary table of effect estimates for each group.  The number
 ## shown is the eGaIT summary statistic for each treatment, which will be
 ## similar to the linear mixed effects slope of log tumor volume on day in
 ## this setting, since the log tumor volume profiles are fairly linear.
 ##
 cg_Identity$data$effectDF %>%
     base::format( digits = 3 ) %>%
     ## NB: If running this as an R script, just comment out or remove
     ## the function "print_table_by_output_format()".  It is included
     ## to facilitate table printing in different document formats.
     print_table_by_output_format() # defined in header, based on compilation format ( PDF, HTML, DOCX ).
 
 ## A vanilla one-way layout figure of the contrasted effects with confidence intervals is auto-generated.
 ##
 print( cg_Identity$figures$figCI )



## ----vismodegib_example_compare_groups_Identity_contrast_Three_Metrics, echo = TRUE, eval = TRUE, results = 'asis'----

  maeve_reset() ## Reset package options to their defaults.
  maeve_options( metric = c( 'linear', 'ITGR', 'AUC' ) )
  model_list_3metrics <- maeve::model_study( vismo21 )

  ## Generate a list with multcomp::glht() models, summary data frames, and simple figures:
  cg_Identity_3metrics <- compare_groups( model_list_3metrics )

  ## We repeat just the one-way layout summary figure:
  print(
      cg_Identity_3metrics$figures$figCI +
      ## set 'axis.text.x' to avoid overwriting the now more crowded contrast labels:
      theme( axis.text.x = element_text( angle = 90, size = 8 ) )
  )



## ----vismodegib_example_compare_groups_Dunnett_contrast, echo = TRUE, eval = TRUE, results = 'asis', size = 'footnotesize'----

 ## Request Dunnett test comparisons, using the 10 mg/kg dose group as the common reference.
 ## By default, "reference_Dunnett" will be the first level of the 'group_name' factor, i.e., 
 ## 'dose_0.0' in this case.
 maeve_reset()
 maeve_options( contrast = 'Dunnett', reference_Dunnett = 'dose_10', metric = 'AUC' )

 cg_Dunnett <- compare_groups( model_list, adjustment_method = 'holm', extended_output = TRUE )

 ## Here is a summary table of each active treatment's effect estimate minus the control group estimate.
 ## The 'Estimate' column is the difference in eGaIT summary statistics between active treatment
 ## and control.
 ##
 cg_Dunnett$data$effectDF %>%
     base::format( digits = 3 ) %>%
     ## NB: If running this as an R script, just comment out or remove
     ## the function "print_table_by_output_format()".  It is included
     ## to facilitate table printing in different document formats.
     print_table_by_output_format() # defined in header, based on compilation format ( PDF, HTML, DOCX ).
   
 ## A one-way layout figure of the contrasted effects with confidence intervals:
 ##
 print(
     cg_Dunnett$figures$figCI +
     ## set 'axis.text.x' to avoid overwriting the now-longer contrast labels:
     theme( axis.text.x = element_text( angle = 45, size = 8 ) ) + 
     labs( title = 'eGaIT AUC effect of doses relative to "10 mg/kg" reference, with 95% CIs' )
 ) 


