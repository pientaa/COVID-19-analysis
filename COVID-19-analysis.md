---
title: "COVID-19 Analysis"
author: "Łukasz Pięta"
date: "11/11/2020"
output: 
  html_document: 
    keep_md: yes
    df_print: kable
    code_folding: "hide"
    toc: true
    toc_float: true
---






## Read data

```r
df <- read_excel("wuhan_blood_sample_data_Jan_Feb_2020.xlsx")
```

## Clean dataset

```r
patients_df <- df %>% group_by(`Admission time`, `Discharge time`, gender, age, outcome) %>%
summarise(PATIENT_ID = sum(PATIENT_ID, na.rm = TRUE), `Total blood tests` = n()) 

patients_df <- patients_df %>%
    mutate(`Days in hospital` = ceiling(difftime(`Discharge time`, `Admission time`, units = "days")))


df <- full_join(patients_df %>% ungroup() %>% select(`Admission time`, PATIENT_ID), df %>% select(-PATIENT_ID), by="Admission time")

df$gender<-ifelse(df$gender==1, 'Male', 'Female') 
df <- df %>% mutate(gender = as.factor(gender))

patients_df$gender<-ifelse(patients_df$gender==1, 'Male', 'Female') 
patients_df <- patients_df %>% mutate(gender = as.factor(gender))

df$outcome<-ifelse(df$outcome==1, 'Death', 'Survival')
df <- df %>% mutate(outcome = as.factor(outcome))

patients_df$outcome<-ifelse(patients_df$outcome==1, 'Death', 'Survival')
patients_df <- patients_df %>% mutate(outcome = as.factor(outcome))
```

## Patients summary

```r
patients_summary <- patients_df %>% ungroup() %>% select(-c(age, PATIENT_ID))

tbl_summary(
    patients_summary,
    by = outcome,
    label = gender ~ "Gender",
) %>%
    add_n() %>%
    modify_header(label = "") %>%
    add_overall() %>%
    bold_labels() 
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wdvcmxwdet .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#wdvcmxwdet .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wdvcmxwdet .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wdvcmxwdet .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wdvcmxwdet .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wdvcmxwdet .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wdvcmxwdet .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#wdvcmxwdet .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#wdvcmxwdet .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wdvcmxwdet .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wdvcmxwdet .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#wdvcmxwdet .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#wdvcmxwdet .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#wdvcmxwdet .gt_from_md > :first-child {
  margin-top: 0;
}

#wdvcmxwdet .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wdvcmxwdet .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#wdvcmxwdet .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#wdvcmxwdet .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wdvcmxwdet .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#wdvcmxwdet .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wdvcmxwdet .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wdvcmxwdet .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wdvcmxwdet .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wdvcmxwdet .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wdvcmxwdet .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#wdvcmxwdet .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wdvcmxwdet .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#wdvcmxwdet .gt_left {
  text-align: left;
}

#wdvcmxwdet .gt_center {
  text-align: center;
}

#wdvcmxwdet .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wdvcmxwdet .gt_font_normal {
  font-weight: normal;
}

#wdvcmxwdet .gt_font_bold {
  font-weight: bold;
}

#wdvcmxwdet .gt_font_italic {
  font-style: italic;
}

#wdvcmxwdet .gt_super {
  font-size: 65%;
}

#wdvcmxwdet .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="wdvcmxwdet" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Overall</strong>, N = 375<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Survival</strong>, N = 201<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Death</strong>, N = 174<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Gender</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">375</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
      <td class="gt_row gt_center">224 (60%)</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">98 (49%)</td>
      <td class="gt_row gt_center">126 (72%)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
      <td class="gt_row gt_center">151 (40%)</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">103 (51%)</td>
      <td class="gt_row gt_center">48 (28%)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Total blood tests</td>
      <td class="gt_row gt_center">16 (9, 21)</td>
      <td class="gt_row gt_center">375</td>
      <td class="gt_row gt_center">16 (12, 20)</td>
      <td class="gt_row gt_center">14 (7, 24)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Days in hospital</td>
      <td class="gt_row gt_center">10 (5, 16)</td>
      <td class="gt_row gt_center">375</td>
      <td class="gt_row gt_center">14 (10, 18)</td>
      <td class="gt_row gt_center">6 (3, 10)</td>
    </tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="5">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Statistics presented: n (%); Median (IQR)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table></div><!--/html_preserve-->

```r
 ggplot(patients_df, aes(x=age,fill=gender)) + geom_histogram(binwidth = 1) + facet_grid(. ~ gender) + scale_x_continuous(name="Age", limits=c(min(df$age), max(df$age)), breaks = seq(0, 100, by=10)) + scale_y_continuous(name = "Number of patients", limits = c(0,10), breaks = seq(0,10, by=1)) +
    theme_minimal()
```

![](COVID-19-analysis_files/figure-html/data_transformed-1.png)<!-- -->

### Patients grouped by outcome, age and gender

```r
 ggplot(patients_df, aes(x=age,fill=outcome)) + geom_histogram(binwidth = 1) + facet_grid(outcome ~ gender) + scale_x_continuous(name="Age", limits=c(min(df$age), max(df$age)), breaks = seq(0, 100, by=10)) + scale_y_continuous(name = "Number of patients", limits = c(0,9), breaks = seq(0,9, by=1)) +
    theme_minimal()
```

![](COVID-19-analysis_files/figure-html/gender_distribution-1.png)<!-- -->

### Patients grouped by outcome and hospitalization duration

```r
ggplot(patients_df, aes(x=`Days in hospital`, fill=outcome)) + geom_histogram(binwidth = 1) + facet_grid(. ~ outcome)  + ylab("Number of patients") +
    theme_minimal()
```

![](COVID-19-analysis_files/figure-html/hospitalization_duration-1.png)<!-- -->

### Patients grouped by outcome and total blood tests taken

```r
ggplot(patients_df, aes(x=`Total blood tests`, fill=outcome)) + geom_histogram(binwidth = 1) + facet_grid(. ~ outcome)  + ylab("Number of patients") +
  scale_x_continuous(name="Total blood tests", limits=c(0, 60), breaks = seq(0, 60, by=10)) +
   scale_y_continuous(name = "Number of patients", limits = c(0,20), breaks = seq(0,20, by=2)) +
    theme_minimal()
```

![](COVID-19-analysis_files/figure-html/total_blood_tests-1.png)<!-- -->

## Biomarkers

```r
no_dates_df <- df %>% select(-c('Admission time', 'Discharge time', 'RE_DATE', 'PATIENT_ID', 'age', 'gender' ))
tbl_summary(
    no_dates_df,
    by = outcome,
    missing = "no"
            ) %>%
    add_n() %>%
    modify_header(label = "**Biomarker**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Final patient outcome related to the test**") %>%
    bold_labels() 
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#oynnbhgvqd .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#oynnbhgvqd .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#oynnbhgvqd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#oynnbhgvqd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#oynnbhgvqd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oynnbhgvqd .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#oynnbhgvqd .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#oynnbhgvqd .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#oynnbhgvqd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#oynnbhgvqd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#oynnbhgvqd .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#oynnbhgvqd .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#oynnbhgvqd .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#oynnbhgvqd .gt_from_md > :first-child {
  margin-top: 0;
}

#oynnbhgvqd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#oynnbhgvqd .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#oynnbhgvqd .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#oynnbhgvqd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oynnbhgvqd .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#oynnbhgvqd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oynnbhgvqd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#oynnbhgvqd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#oynnbhgvqd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oynnbhgvqd .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#oynnbhgvqd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#oynnbhgvqd .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#oynnbhgvqd .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#oynnbhgvqd .gt_left {
  text-align: left;
}

#oynnbhgvqd .gt_center {
  text-align: center;
}

#oynnbhgvqd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#oynnbhgvqd .gt_font_normal {
  font-weight: normal;
}

#oynnbhgvqd .gt_font_bold {
  font-weight: bold;
}

#oynnbhgvqd .gt_font_italic {
  font-style: italic;
}

#oynnbhgvqd .gt_super {
  font-size: 65%;
}

#oynnbhgvqd .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="oynnbhgvqd" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1"><strong>Biomarker</strong></th>
      <th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1"><strong>N</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">
        <span class="gt_column_spanner"><strong>Final patient outcome related to the test</strong></span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Death</strong>, N = 2,905<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Survival</strong>, N = 3,215<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Hypersensitive cardiac troponinI</td>
      <td class="gt_row gt_center">507</td>
      <td class="gt_row gt_center">70 (18, 631)</td>
      <td class="gt_row gt_center">3 (2, 7)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">hemoglobin</td>
      <td class="gt_row gt_center">975</td>
      <td class="gt_row gt_center">123 (110, 135)</td>
      <td class="gt_row gt_center">127 (116, 138)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Serum chloride</td>
      <td class="gt_row gt_center">975</td>
      <td class="gt_row gt_center">104 (100, 111)</td>
      <td class="gt_row gt_center">101 (99, 103)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Prothrombin time</td>
      <td class="gt_row gt_center">662</td>
      <td class="gt_row gt_center">16.3 (15.0, 18.2)</td>
      <td class="gt_row gt_center">13.6 (13.1, 14.1)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">procalcitonin</td>
      <td class="gt_row gt_center">459</td>
      <td class="gt_row gt_center">0.38 (0.14, 1.13)</td>
      <td class="gt_row gt_center">0.04 (0.02, 0.06)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">eosinophils(%)</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.00 (0.00, 0.10)</td>
      <td class="gt_row gt_center">0.70 (0.00, 1.80)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 2 receptor</td>
      <td class="gt_row gt_center">268</td>
      <td class="gt_row gt_center">1,180 (807, 1,603)</td>
      <td class="gt_row gt_center">529 (400, 742)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Alkaline phosphatase</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">83 (64, 123)</td>
      <td class="gt_row gt_center">60 (50, 75)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">albumin</td>
      <td class="gt_row gt_center">934</td>
      <td class="gt_row gt_center">28 (24, 31)</td>
      <td class="gt_row gt_center">36 (34, 39)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">basophil(%)</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.10 (0.10, 0.20)</td>
      <td class="gt_row gt_center">0.20 (0.10, 0.40)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 10</td>
      <td class="gt_row gt_center">267</td>
      <td class="gt_row gt_center">11 (6, 17)</td>
      <td class="gt_row gt_center">5 (5, 8)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Total bilirubin</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">14 (10, 25)</td>
      <td class="gt_row gt_center">8 (6, 12)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Platelet count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">112 (55, 174)</td>
      <td class="gt_row gt_center">229 (176, 290)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">monocytes(%)</td>
      <td class="gt_row gt_center">958</td>
      <td class="gt_row gt_center">3.0 (2.0, 4.7)</td>
      <td class="gt_row gt_center">8.2 (6.3, 10.0)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">antithrombin</td>
      <td class="gt_row gt_center">330</td>
      <td class="gt_row gt_center">80 (70, 92)</td>
      <td class="gt_row gt_center">93 (86, 103)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 8</td>
      <td class="gt_row gt_center">268</td>
      <td class="gt_row gt_center">30 (18, 61)</td>
      <td class="gt_row gt_center">11 (7, 19)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">indirect bilirubin</td>
      <td class="gt_row gt_center">906</td>
      <td class="gt_row gt_center">6.2 (4.2, 9.2)</td>
      <td class="gt_row gt_center">4.9 (3.4, 7.1)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Red blood cell distribution width</td>
      <td class="gt_row gt_center">923</td>
      <td class="gt_row gt_center">13.20 (12.40, 14.40)</td>
      <td class="gt_row gt_center">12.20 (11.80, 12.80)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">neutrophils(%)</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">92 (88, 95)</td>
      <td class="gt_row gt_center">66 (56, 76)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">total protein</td>
      <td class="gt_row gt_center">931</td>
      <td class="gt_row gt_center">62 (57, 68)</td>
      <td class="gt_row gt_center">68 (65, 72)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Quantification of Treponema pallidum antibodies</td>
      <td class="gt_row gt_center">279</td>
      <td class="gt_row gt_center">0.06 (0.04, 0.07)</td>
      <td class="gt_row gt_center">0.05 (0.04, 0.07)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Prothrombin activity</td>
      <td class="gt_row gt_center">659</td>
      <td class="gt_row gt_center">66 (56, 78)</td>
      <td class="gt_row gt_center">94 (88, 103)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">HBsAg</td>
      <td class="gt_row gt_center">279</td>
      <td class="gt_row gt_center">0.01 (0.00, 0.02)</td>
      <td class="gt_row gt_center">0.00 (0.00, 0.01)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">mean corpuscular volume</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">91.3 (87.1, 96.4)</td>
      <td class="gt_row gt_center">89.8 (86.8, 91.9)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">hematocrit</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">35.9 (32.5, 39.8)</td>
      <td class="gt_row gt_center">37.1 (34.3, 39.9)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">White blood cell count</td>
      <td class="gt_row gt_center">1,127</td>
      <td class="gt_row gt_center">12 (8, 17)</td>
      <td class="gt_row gt_center">6 (4, 8)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Tumor necrosis factorα</td>
      <td class="gt_row gt_center">268</td>
      <td class="gt_row gt_center">11 (8, 17)</td>
      <td class="gt_row gt_center">8 (6, 10)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">mean corpuscular hemoglobin concentration</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">342 (331, 350)</td>
      <td class="gt_row gt_center">343 (335, 350)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">fibrinogen</td>
      <td class="gt_row gt_center">566</td>
      <td class="gt_row gt_center">3.92 (2.44, 5.63)</td>
      <td class="gt_row gt_center">4.40 (3.56, 5.34)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 1β</td>
      <td class="gt_row gt_center">268</td>
      <td class="gt_row gt_center">5.0 (5.0, 5.0)</td>
      <td class="gt_row gt_center">5.0 (5.0, 5.0)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Urea</td>
      <td class="gt_row gt_center">936</td>
      <td class="gt_row gt_center">11 (7, 17)</td>
      <td class="gt_row gt_center">4 (3, 5)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">lymphocyte count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.46 (0.31, 0.69)</td>
      <td class="gt_row gt_center">1.25 (0.87, 1.62)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">PH value</td>
      <td class="gt_row gt_center">384</td>
      <td class="gt_row gt_center">6.50 (6.00, 7.41)</td>
      <td class="gt_row gt_center">6.50 (6.00, 7.00)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Red blood cell count</td>
      <td class="gt_row gt_center">1,127</td>
      <td class="gt_row gt_center">4.0 (3.6, 4.6)</td>
      <td class="gt_row gt_center">4.2 (3.8, 4.7)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Eosinophil count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.00 (0.00, 0.01)</td>
      <td class="gt_row gt_center">0.03 (0.00, 0.09)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Corrected calcium</td>
      <td class="gt_row gt_center">914</td>
      <td class="gt_row gt_center">2.35 (2.27, 2.44)</td>
      <td class="gt_row gt_center">2.37 (2.27, 2.44)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Serum potassium</td>
      <td class="gt_row gt_center">980</td>
      <td class="gt_row gt_center">4.60 (4.04, 5.27)</td>
      <td class="gt_row gt_center">4.28 (3.92, 4.62)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">glucose</td>
      <td class="gt_row gt_center">775</td>
      <td class="gt_row gt_center">9.1 (6.9, 13.3)</td>
      <td class="gt_row gt_center">5.7 (5.0, 7.6)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">neutrophils count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">10.8 (7.0, 15.2)</td>
      <td class="gt_row gt_center">3.5 (2.4, 5.2)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Direct bilirubin</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">8 (5, 14)</td>
      <td class="gt_row gt_center">4 (2, 5)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Mean platelet volume</td>
      <td class="gt_row gt_center">862</td>
      <td class="gt_row gt_center">11.30 (10.70, 12.20)</td>
      <td class="gt_row gt_center">10.40 (9.90, 11.00)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">ferritin</td>
      <td class="gt_row gt_center">283</td>
      <td class="gt_row gt_center">1,636 (928, 2,517)</td>
      <td class="gt_row gt_center">504 (235, 834)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">RBC distribution width SD</td>
      <td class="gt_row gt_center">923</td>
      <td class="gt_row gt_center">43.7 (39.9, 48.5)</td>
      <td class="gt_row gt_center">39.5 (37.6, 41.4)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Thrombin time</td>
      <td class="gt_row gt_center">566</td>
      <td class="gt_row gt_center">17.30 (15.80, 19.75)</td>
      <td class="gt_row gt_center">16.40 (15.60, 17.30)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">(%)lymphocyte</td>
      <td class="gt_row gt_center">958</td>
      <td class="gt_row gt_center">4 (2, 7)</td>
      <td class="gt_row gt_center">24 (16, 33)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">HCV antibody quantification</td>
      <td class="gt_row gt_center">279</td>
      <td class="gt_row gt_center">0.07 (0.04, 0.11)</td>
      <td class="gt_row gt_center">0.06 (0.04, 0.08)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">D-D dimer</td>
      <td class="gt_row gt_center">630</td>
      <td class="gt_row gt_center">19 (3, 21)</td>
      <td class="gt_row gt_center">1 (0, 1)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Total cholesterol</td>
      <td class="gt_row gt_center">931</td>
      <td class="gt_row gt_center">3.32 (2.72, 3.88)</td>
      <td class="gt_row gt_center">3.93 (3.39, 4.48)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">aspartate aminotransferase</td>
      <td class="gt_row gt_center">935</td>
      <td class="gt_row gt_center">38 (25, 59)</td>
      <td class="gt_row gt_center">21 (17, 29)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Uric acid</td>
      <td class="gt_row gt_center">934</td>
      <td class="gt_row gt_center">245 (166, 374)</td>
      <td class="gt_row gt_center">240 (193, 304)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">HCO3-</td>
      <td class="gt_row gt_center">934</td>
      <td class="gt_row gt_center">21.8 (18.8, 24.7)</td>
      <td class="gt_row gt_center">24.7 (22.8, 26.7)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">calcium</td>
      <td class="gt_row gt_center">979</td>
      <td class="gt_row gt_center">2.00 (1.90, 2.08)</td>
      <td class="gt_row gt_center">2.17 (2.10, 2.25)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Amino-terminal brain natriuretic peptide precursor(NT-proBNP)</td>
      <td class="gt_row gt_center">475</td>
      <td class="gt_row gt_center">1,467 (516, 4,578)</td>
      <td class="gt_row gt_center">64 (23, 166)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Lactate dehydrogenase</td>
      <td class="gt_row gt_center">934</td>
      <td class="gt_row gt_center">593 (431, 840)</td>
      <td class="gt_row gt_center">220 (189, 278)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">platelet large cell ratio</td>
      <td class="gt_row gt_center">862</td>
      <td class="gt_row gt_center">35 (30, 42)</td>
      <td class="gt_row gt_center">28 (23, 33)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 6</td>
      <td class="gt_row gt_center">272</td>
      <td class="gt_row gt_center">66 (30, 142)</td>
      <td class="gt_row gt_center">8 (2, 21)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Fibrin degradation products</td>
      <td class="gt_row gt_center">330</td>
      <td class="gt_row gt_center">114 (18, 150)</td>
      <td class="gt_row gt_center">4 (4, 4)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">monocytes count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.36 (0.20, 0.58)</td>
      <td class="gt_row gt_center">0.43 (0.32, 0.58)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">PLT distribution width</td>
      <td class="gt_row gt_center">862</td>
      <td class="gt_row gt_center">13.60 (12.10, 15.93)</td>
      <td class="gt_row gt_center">11.70 (10.70, 13.00)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">globulin</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">34.1 (30.2, 38.2)</td>
      <td class="gt_row gt_center">31.8 (29.5, 35.2)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">γ-glutamyl transpeptidase</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">42 (27, 79)</td>
      <td class="gt_row gt_center">29 (19, 46)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">International standard ratio</td>
      <td class="gt_row gt_center">659</td>
      <td class="gt_row gt_center">1.31 (1.17, 1.48)</td>
      <td class="gt_row gt_center">1.04 (0.99, 1.09)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">basophil count(#)</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.010 (0.010, 0.030)</td>
      <td class="gt_row gt_center">0.010 (0.010, 0.020)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">2019-nCoV nucleic acid detection</td>
      <td class="gt_row gt_center">501</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-1</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">57 (100%)</td>
      <td class="gt_row gt_center">444 (100%)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">mean corpuscular hemoglobin</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">31.20 (29.90, 32.70)</td>
      <td class="gt_row gt_center">30.70 (29.60, 31.90)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Activation of partial thromboplastin time</td>
      <td class="gt_row gt_center">568</td>
      <td class="gt_row gt_center">40 (36, 45)</td>
      <td class="gt_row gt_center">39 (35, 43)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">High sensitivity C-reactive protein</td>
      <td class="gt_row gt_center">737</td>
      <td class="gt_row gt_center">114 (65, 191)</td>
      <td class="gt_row gt_center">7 (2, 35)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">HIV antibody quantification</td>
      <td class="gt_row gt_center">278</td>
      <td class="gt_row gt_center">0.08 (0.07, 0.11)</td>
      <td class="gt_row gt_center">0.09 (0.08, 0.11)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">serum sodium</td>
      <td class="gt_row gt_center">975</td>
      <td class="gt_row gt_center">142 (138, 148)</td>
      <td class="gt_row gt_center">140 (138, 141)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">thrombocytocrit</td>
      <td class="gt_row gt_center">862</td>
      <td class="gt_row gt_center">0.15 (0.10, 0.21)</td>
      <td class="gt_row gt_center">0.24 (0.19, 0.30)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">ESR</td>
      <td class="gt_row gt_center">383</td>
      <td class="gt_row gt_center">36 (16, 59)</td>
      <td class="gt_row gt_center">26 (13, 40)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">glutamic-pyruvic transaminase</td>
      <td class="gt_row gt_center">931</td>
      <td class="gt_row gt_center">26 (18, 44)</td>
      <td class="gt_row gt_center">21 (15, 36)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">eGFR</td>
      <td class="gt_row gt_center">936</td>
      <td class="gt_row gt_center">72 (43, 91)</td>
      <td class="gt_row gt_center">100 (85, 114)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">creatinine</td>
      <td class="gt_row gt_center">936</td>
      <td class="gt_row gt_center">88 (68, 130)</td>
      <td class="gt_row gt_center">64 (54, 83)</td>
    </tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Statistics presented: Median (IQR); n (%)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table></div><!--/html_preserve-->

### Finding correlation

Finding correlation between outcome and other variables. Ignoring `Admission time`, `PATIENT_ID`, `RE_DATE` and `Discharge time` and dropping `outcome` which correlation with itself is equal to `1.0`.


```r
cor_df <-df

cor_df$outcome<-ifelse(cor_df$outcome=='Death', 1, 0)
cor_df$gender<-ifelse(cor_df$gender=='Male', 1, 0)

cor_df <- cor_df %>% rename(isMale=gender)
cor_df <- cor_df %>% rename(Death=outcome)

cor_df <- cor_df %>% select (-c(`Admission time`, PATIENT_ID, RE_DATE, `Discharge time` ))

corrMatrix <- cor(cor_df[sapply(cor_df, is.numeric)], use='pairwise.complete.obs')

correlation_df <- as.data.frame(corrMatrix)

correlation_df %>% rownames_to_column('variable') %>% filter(variable != 'Death') %>% select(variable, Death) %>% mutate(Death = abs(Death)) %>%
  arrange(desc(Death)) %>%
  rename(`Outcome correlation` = Death) %>%
  head(10) 
```

<div class="kable-table">

|variable                            | Outcome correlation|
|:-----------------------------------|-------------------:|
|neutrophils(%)                      |           0.7258383|
|(%)lymphocyte                       |           0.7230203|
|albumin                             |           0.6892432|
|Prothrombin activity                |           0.6562405|
|High sensitivity C-reactive protein |           0.6524696|
|D-D dimer                           |           0.6368244|
|Lactate dehydrogenase               |           0.6230357|
|neutrophils count                   |           0.6071749|
|Fibrin degradation products         |           0.6044110|
|age                                 |           0.5860651|

</div>

### Use of correlation

In following section, we will use 3 biomarkers that are most correlated to outcome and visualize theirs mean values among each patient at 3D graph. So we take only these patients who had all of these 3 biomarkers tested at least once. If somebody was tested more than once, then mean value of all tests for each biomarker is calculated.


```r
new_df  <- df

new_df <- new_df %>% select(PATIENT_ID, `neutrophils(%)`, `(%)lymphocyte`, albumin, outcome) %>%
    group_by(PATIENT_ID, outcome) %>%
    summarise(Neutrophils_mean = mean(`neutrophils(%)`, na.rm = TRUE), Lymphocyte_mean = mean(`(%)lymphocyte`, na.rm = TRUE), albumin_mean = mean(`albumin`, na.rm = TRUE))

new_df <- new_df %>% filter(!is.nan(Neutrophils_mean) & !is.nan(Lymphocyte_mean) & !is.nan(albumin_mean))


mycolors <- c('royalblue1', 'darkcyan')
new_df$color <- mycolors[ as.numeric(new_df$outcome) ]

par(mar=c(0,0,0,0))
plot3d( 
    x=new_df$Neutrophils_mean, y=new_df$Lymphocyte_mean, z=new_df$albumin_mean, 
    col = new_df$color, 
    type = 's', 
    radius = 1,
    legend=TRUE,
    xlab="Neutrophils(%)", ylab="Lymphocyte(%)", zlab="Albumin")
legend3d("topright", legend = c('Death', 'Survival'), pch = 10, col = mycolors, cex=0.8, inset=c(0.02))

writeWebGL( filename="3d_correlation_mean.html" ,  width=600, height=600)
```

Data after cleaning and transformation:


```r
tbl_summary(
    new_df %>% ungroup() %>%rename(`Neutrophils(%)` = Neutrophils_mean, `Lymphocyte(%)` = Lymphocyte_mean, `Albumin`= albumin_mean) %>% select(-PATIENT_ID, -color),
    by = outcome
) %>%
    add_n() %>%
    modify_header(label = "**Mean value of**") %>%
    add_overall() %>%
    bold_labels() 
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#zdoitrfwll .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#zdoitrfwll .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zdoitrfwll .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#zdoitrfwll .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#zdoitrfwll .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zdoitrfwll .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#zdoitrfwll .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#zdoitrfwll .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#zdoitrfwll .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#zdoitrfwll .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#zdoitrfwll .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#zdoitrfwll .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#zdoitrfwll .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#zdoitrfwll .gt_from_md > :first-child {
  margin-top: 0;
}

#zdoitrfwll .gt_from_md > :last-child {
  margin-bottom: 0;
}

#zdoitrfwll .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#zdoitrfwll .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#zdoitrfwll .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zdoitrfwll .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#zdoitrfwll .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#zdoitrfwll .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#zdoitrfwll .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#zdoitrfwll .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#zdoitrfwll .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zdoitrfwll .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#zdoitrfwll .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#zdoitrfwll .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#zdoitrfwll .gt_left {
  text-align: left;
}

#zdoitrfwll .gt_center {
  text-align: center;
}

#zdoitrfwll .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#zdoitrfwll .gt_font_normal {
  font-weight: normal;
}

#zdoitrfwll .gt_font_bold {
  font-weight: bold;
}

#zdoitrfwll .gt_font_italic {
  font-style: italic;
}

#zdoitrfwll .gt_super {
  font-size: 65%;
}

#zdoitrfwll .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="zdoitrfwll" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Mean value of</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Overall</strong>, N = 354<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Death</strong>, N = 162<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Survival</strong>, N = 192<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Neutrophils(%)</td>
      <td class="gt_row gt_center">77 (63, 91)</td>
      <td class="gt_row gt_center">354</td>
      <td class="gt_row gt_center">91 (87, 94)</td>
      <td class="gt_row gt_center">66 (56, 72)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Lymphocyte(%)</td>
      <td class="gt_row gt_center">15 (5, 26)</td>
      <td class="gt_row gt_center">354</td>
      <td class="gt_row gt_center">5 (3, 8)</td>
      <td class="gt_row gt_center">25 (18, 31)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Albumin</td>
      <td class="gt_row gt_center">33.1 (29.1, 37.0)</td>
      <td class="gt_row gt_center">354</td>
      <td class="gt_row gt_center">28.9 (26.3, 31.2)</td>
      <td class="gt_row gt_center">36.5 (33.9, 39.4)</td>
    </tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="5">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Statistics presented: Median (IQR)
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table></div><!--/html_preserve-->


```r
htmltools::includeHTML("./3d_correlation_mean.html")
```

<!--html_preserve-->    <html><head>
	<TITLE>RGL model</TITLE>
    </head>
    <body onload="rgl.start();"> 
    
    <div align="center">
<script>/*
 * Copyright (C) 2009 Apple Inc. All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 * Copyright (2016) Duncan Murdoch - fixed CanvasMatrix4.ortho,
 * cleaned up.
 */
/*
    CanvasMatrix4 class

    This class implements a 4x4 matrix. It has functions which
    duplicate the functionality of the OpenGL matrix stack and
    glut functions.

    IDL:

    [
        Constructor(in CanvasMatrix4 matrix),           // copy passed matrix into new CanvasMatrix4
        Constructor(in sequence<float> array)           // create new CanvasMatrix4 with 16 floats (row major)
        Constructor()                                   // create new CanvasMatrix4 with identity matrix
    ]
    interface CanvasMatrix4 {
        attribute float m11;
        attribute float m12;
        attribute float m13;
        attribute float m14;
        attribute float m21;
        attribute float m22;
        attribute float m23;
        attribute float m24;
        attribute float m31;
        attribute float m32;
        attribute float m33;
        attribute float m34;
        attribute float m41;
        attribute float m42;
        attribute float m43;
        attribute float m44;

        void load(in CanvasMatrix4 matrix);                 // copy the values from the passed matrix
        void load(in sequence<float> array);                // copy 16 floats into the matrix
        sequence<float> getAsArray();                       // return the matrix as an array of 16 floats
        WebGLFloatArray getAsCanvasFloatArray();           // return the matrix as a WebGLFloatArray with 16 values
        void makeIdentity();                                // replace the matrix with identity
        void transpose();                                   // replace the matrix with its transpose
        void invert();                                      // replace the matrix with its inverse

        void translate(in float x, in float y, in float z); // multiply the matrix by passed translation values on the right
        void scale(in float x, in float y, in float z);     // multiply the matrix by passed scale values on the right
        void rotate(in float angle,                         // multiply the matrix by passed rotation values on the right
                    in float x, in float y, in float z);    // (angle is in degrees)
        void multRight(in CanvasMatrix matrix);             // multiply the matrix by the passed matrix on the right
        void multLeft(in CanvasMatrix matrix);              // multiply the matrix by the passed matrix on the left
        void ortho(in float left, in float right,           // multiply the matrix by the passed ortho values on the right
                   in float bottom, in float top,
                   in float near, in float far);
        void frustum(in float left, in float right,         // multiply the matrix by the passed frustum values on the right
                     in float bottom, in float top,
                     in float near, in float far);
        void perspective(in float fovy, in float aspect,    // multiply the matrix by the passed perspective values on the right
                         in float zNear, in float zFar);
        void lookat(in float eyex, in float eyey, in float eyez,    // multiply the matrix by the passed lookat
                    in float ctrx, in float ctry, in float ctrz,    // values on the right
                    in float upx, in float upy, in float upz);
    }
*/

CanvasMatrix4 = function(m)
{
    if (typeof m == 'object') {
        if ("length" in m && m.length >= 16) {
            this.load(m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7], m[8], m[9], m[10], m[11], m[12], m[13], m[14], m[15]);
            return;
        }
        else if (m instanceof CanvasMatrix4) {
            this.load(m);
            return;
        }
    }
    this.makeIdentity();
};

CanvasMatrix4.prototype.load = function()
{
    if (arguments.length == 1 && typeof arguments[0] == 'object') {
        var matrix = arguments[0];

        if ("length" in matrix && matrix.length == 16) {
            this.m11 = matrix[0];
            this.m12 = matrix[1];
            this.m13 = matrix[2];
            this.m14 = matrix[3];

            this.m21 = matrix[4];
            this.m22 = matrix[5];
            this.m23 = matrix[6];
            this.m24 = matrix[7];

            this.m31 = matrix[8];
            this.m32 = matrix[9];
            this.m33 = matrix[10];
            this.m34 = matrix[11];

            this.m41 = matrix[12];
            this.m42 = matrix[13];
            this.m43 = matrix[14];
            this.m44 = matrix[15];
            return;
        }

        if (arguments[0] instanceof CanvasMatrix4) {

            this.m11 = matrix.m11;
            this.m12 = matrix.m12;
            this.m13 = matrix.m13;
            this.m14 = matrix.m14;

            this.m21 = matrix.m21;
            this.m22 = matrix.m22;
            this.m23 = matrix.m23;
            this.m24 = matrix.m24;

            this.m31 = matrix.m31;
            this.m32 = matrix.m32;
            this.m33 = matrix.m33;
            this.m34 = matrix.m34;

            this.m41 = matrix.m41;
            this.m42 = matrix.m42;
            this.m43 = matrix.m43;
            this.m44 = matrix.m44;
            return;
        }
    }

    this.makeIdentity();
};

CanvasMatrix4.prototype.getAsArray = function()
{
    return [
        this.m11, this.m12, this.m13, this.m14,
        this.m21, this.m22, this.m23, this.m24,
        this.m31, this.m32, this.m33, this.m34,
        this.m41, this.m42, this.m43, this.m44
    ];
};

CanvasMatrix4.prototype.getAsWebGLFloatArray = function()
{
    return new WebGLFloatArray(this.getAsArray());
};

CanvasMatrix4.prototype.makeIdentity = function()
{
    this.m11 = 1;
    this.m12 = 0;
    this.m13 = 0;
    this.m14 = 0;

    this.m21 = 0;
    this.m22 = 1;
    this.m23 = 0;
    this.m24 = 0;

    this.m31 = 0;
    this.m32 = 0;
    this.m33 = 1;
    this.m34 = 0;

    this.m41 = 0;
    this.m42 = 0;
    this.m43 = 0;
    this.m44 = 1;
};

CanvasMatrix4.prototype.transpose = function()
{
    var tmp = this.m12;
    this.m12 = this.m21;
    this.m21 = tmp;

    tmp = this.m13;
    this.m13 = this.m31;
    this.m31 = tmp;

    tmp = this.m14;
    this.m14 = this.m41;
    this.m41 = tmp;

    tmp = this.m23;
    this.m23 = this.m32;
    this.m32 = tmp;

    tmp = this.m24;
    this.m24 = this.m42;
    this.m42 = tmp;

    tmp = this.m34;
    this.m34 = this.m43;
    this.m43 = tmp;
};

CanvasMatrix4.prototype.invert = function()
{
    // Calculate the 4x4 determinant
    // If the determinant is zero,
    // then the inverse matrix is not unique.
    var det = this._determinant4x4();

    if (Math.abs(det) < 1e-8)
        return null;

    this._makeAdjoint();

    // Scale the adjoint matrix to get the inverse
    this.m11 /= det;
    this.m12 /= det;
    this.m13 /= det;
    this.m14 /= det;

    this.m21 /= det;
    this.m22 /= det;
    this.m23 /= det;
    this.m24 /= det;

    this.m31 /= det;
    this.m32 /= det;
    this.m33 /= det;
    this.m34 /= det;

    this.m41 /= det;
    this.m42 /= det;
    this.m43 /= det;
    this.m44 /= det;
};

CanvasMatrix4.prototype.translate = function(x,y,z)
{
    if (x === undefined)
        x = 0;
    if (y === undefined)
        y = 0;
    if (z === undefined)
        z = 0;

    var matrix = new CanvasMatrix4();
    matrix.m41 = x;
    matrix.m42 = y;
    matrix.m43 = z;

    this.multRight(matrix);
};

CanvasMatrix4.prototype.scale = function(x,y,z)
{
    if (x === undefined)
        x = 1;
    if (z === undefined) {
        if (y === undefined) {
            y = x;
            z = x;
        }
        else
            z = 1;
    }
    else if (y === undefined)
        y = x;

    var matrix = new CanvasMatrix4();
    matrix.m11 = x;
    matrix.m22 = y;
    matrix.m33 = z;

    this.multRight(matrix);
};

CanvasMatrix4.prototype.rotate = function(angle,x,y,z)
{
    // angles are in degrees. Switch to radians
    angle = angle / 180 * Math.PI;

    angle /= 2;
    var sinA = Math.sin(angle);
    var cosA = Math.cos(angle);
    var sinA2 = sinA * sinA;

    // normalize
    var length = Math.sqrt(x * x + y * y + z * z);
    if (length === 0) {
        // bad vector, just use something reasonable
        x = 0;
        y = 0;
        z = 1;
    } else if (length != 1) {
        x /= length;
        y /= length;
        z /= length;
    }

    var mat = new CanvasMatrix4();

    // optimize case where axis is along major axis
    if (x == 1 && y === 0 && z === 0) {
        mat.m11 = 1;
        mat.m12 = 0;
        mat.m13 = 0;
        mat.m21 = 0;
        mat.m22 = 1 - 2 * sinA2;
        mat.m23 = 2 * sinA * cosA;
        mat.m31 = 0;
        mat.m32 = -2 * sinA * cosA;
        mat.m33 = 1 - 2 * sinA2;
        mat.m14 = mat.m24 = mat.m34 = 0;
        mat.m41 = mat.m42 = mat.m43 = 0;
        mat.m44 = 1;
    } else if (x === 0 && y == 1 && z === 0) {
        mat.m11 = 1 - 2 * sinA2;
        mat.m12 = 0;
        mat.m13 = -2 * sinA * cosA;
        mat.m21 = 0;
        mat.m22 = 1;
        mat.m23 = 0;
        mat.m31 = 2 * sinA * cosA;
        mat.m32 = 0;
        mat.m33 = 1 - 2 * sinA2;
        mat.m14 = mat.m24 = mat.m34 = 0;
        mat.m41 = mat.m42 = mat.m43 = 0;
        mat.m44 = 1;
    } else if (x === 0 && y === 0 && z == 1) {
        mat.m11 = 1 - 2 * sinA2;
        mat.m12 = 2 * sinA * cosA;
        mat.m13 = 0;
        mat.m21 = -2 * sinA * cosA;
        mat.m22 = 1 - 2 * sinA2;
        mat.m23 = 0;
        mat.m31 = 0;
        mat.m32 = 0;
        mat.m33 = 1;
        mat.m14 = mat.m24 = mat.m34 = 0;
        mat.m41 = mat.m42 = mat.m43 = 0;
        mat.m44 = 1;
    } else {
        var x2 = x*x;
        var y2 = y*y;
        var z2 = z*z;

        mat.m11 = 1 - 2 * (y2 + z2) * sinA2;
        mat.m12 = 2 * (x * y * sinA2 + z * sinA * cosA);
        mat.m13 = 2 * (x * z * sinA2 - y * sinA * cosA);
        mat.m21 = 2 * (y * x * sinA2 - z * sinA * cosA);
        mat.m22 = 1 - 2 * (z2 + x2) * sinA2;
        mat.m23 = 2 * (y * z * sinA2 + x * sinA * cosA);
        mat.m31 = 2 * (z * x * sinA2 + y * sinA * cosA);
        mat.m32 = 2 * (z * y * sinA2 - x * sinA * cosA);
        mat.m33 = 1 - 2 * (x2 + y2) * sinA2;
        mat.m14 = mat.m24 = mat.m34 = 0;
        mat.m41 = mat.m42 = mat.m43 = 0;
        mat.m44 = 1;
    }
    this.multRight(mat);
};

CanvasMatrix4.prototype.multRight = function(mat)
{
    var m11 = (this.m11 * mat.m11 + this.m12 * mat.m21 +
               this.m13 * mat.m31 + this.m14 * mat.m41);
    var m12 = (this.m11 * mat.m12 + this.m12 * mat.m22 +
               this.m13 * mat.m32 + this.m14 * mat.m42);
    var m13 = (this.m11 * mat.m13 + this.m12 * mat.m23 +
               this.m13 * mat.m33 + this.m14 * mat.m43);
    var m14 = (this.m11 * mat.m14 + this.m12 * mat.m24 +
               this.m13 * mat.m34 + this.m14 * mat.m44);

    var m21 = (this.m21 * mat.m11 + this.m22 * mat.m21 +
               this.m23 * mat.m31 + this.m24 * mat.m41);
    var m22 = (this.m21 * mat.m12 + this.m22 * mat.m22 +
               this.m23 * mat.m32 + this.m24 * mat.m42);
    var m23 = (this.m21 * mat.m13 + this.m22 * mat.m23 +
               this.m23 * mat.m33 + this.m24 * mat.m43);
    var m24 = (this.m21 * mat.m14 + this.m22 * mat.m24 +
               this.m23 * mat.m34 + this.m24 * mat.m44);

    var m31 = (this.m31 * mat.m11 + this.m32 * mat.m21 +
               this.m33 * mat.m31 + this.m34 * mat.m41);
    var m32 = (this.m31 * mat.m12 + this.m32 * mat.m22 +
               this.m33 * mat.m32 + this.m34 * mat.m42);
    var m33 = (this.m31 * mat.m13 + this.m32 * mat.m23 +
               this.m33 * mat.m33 + this.m34 * mat.m43);
    var m34 = (this.m31 * mat.m14 + this.m32 * mat.m24 +
               this.m33 * mat.m34 + this.m34 * mat.m44);

    var m41 = (this.m41 * mat.m11 + this.m42 * mat.m21 +
               this.m43 * mat.m31 + this.m44 * mat.m41);
    var m42 = (this.m41 * mat.m12 + this.m42 * mat.m22 +
               this.m43 * mat.m32 + this.m44 * mat.m42);
    var m43 = (this.m41 * mat.m13 + this.m42 * mat.m23 +
               this.m43 * mat.m33 + this.m44 * mat.m43);
    var m44 = (this.m41 * mat.m14 + this.m42 * mat.m24 +
               this.m43 * mat.m34 + this.m44 * mat.m44);

    this.m11 = m11;
    this.m12 = m12;
    this.m13 = m13;
    this.m14 = m14;

    this.m21 = m21;
    this.m22 = m22;
    this.m23 = m23;
    this.m24 = m24;

    this.m31 = m31;
    this.m32 = m32;
    this.m33 = m33;
    this.m34 = m34;

    this.m41 = m41;
    this.m42 = m42;
    this.m43 = m43;
    this.m44 = m44;
};

CanvasMatrix4.prototype.multLeft = function(mat)
{
    var m11 = (mat.m11 * this.m11 + mat.m12 * this.m21 +
               mat.m13 * this.m31 + mat.m14 * this.m41);
    var m12 = (mat.m11 * this.m12 + mat.m12 * this.m22 +
               mat.m13 * this.m32 + mat.m14 * this.m42);
    var m13 = (mat.m11 * this.m13 + mat.m12 * this.m23 +
               mat.m13 * this.m33 + mat.m14 * this.m43);
    var m14 = (mat.m11 * this.m14 + mat.m12 * this.m24 +
               mat.m13 * this.m34 + mat.m14 * this.m44);

    var m21 = (mat.m21 * this.m11 + mat.m22 * this.m21 +
               mat.m23 * this.m31 + mat.m24 * this.m41);
    var m22 = (mat.m21 * this.m12 + mat.m22 * this.m22 +
               mat.m23 * this.m32 + mat.m24 * this.m42);
    var m23 = (mat.m21 * this.m13 + mat.m22 * this.m23 +
               mat.m23 * this.m33 + mat.m24 * this.m43);
    var m24 = (mat.m21 * this.m14 + mat.m22 * this.m24 +
               mat.m23 * this.m34 + mat.m24 * this.m44);

    var m31 = (mat.m31 * this.m11 + mat.m32 * this.m21 +
               mat.m33 * this.m31 + mat.m34 * this.m41);
    var m32 = (mat.m31 * this.m12 + mat.m32 * this.m22 +
               mat.m33 * this.m32 + mat.m34 * this.m42);
    var m33 = (mat.m31 * this.m13 + mat.m32 * this.m23 +
               mat.m33 * this.m33 + mat.m34 * this.m43);
    var m34 = (mat.m31 * this.m14 + mat.m32 * this.m24 +
               mat.m33 * this.m34 + mat.m34 * this.m44);

    var m41 = (mat.m41 * this.m11 + mat.m42 * this.m21 +
               mat.m43 * this.m31 + mat.m44 * this.m41);
    var m42 = (mat.m41 * this.m12 + mat.m42 * this.m22 +
               mat.m43 * this.m32 + mat.m44 * this.m42);
    var m43 = (mat.m41 * this.m13 + mat.m42 * this.m23 +
               mat.m43 * this.m33 + mat.m44 * this.m43);
    var m44 = (mat.m41 * this.m14 + mat.m42 * this.m24 +
               mat.m43 * this.m34 + mat.m44 * this.m44);

    this.m11 = m11;
    this.m12 = m12;
    this.m13 = m13;
    this.m14 = m14;

    this.m21 = m21;
    this.m22 = m22;
    this.m23 = m23;
    this.m24 = m24;

    this.m31 = m31;
    this.m32 = m32;
    this.m33 = m33;
    this.m34 = m34;

    this.m41 = m41;
    this.m42 = m42;
    this.m43 = m43;
    this.m44 = m44;
};

CanvasMatrix4.prototype.ortho = function(left, right, bottom, top, near, far)
{
    var tx = (left + right) / (left - right);
    var ty = (top + bottom) / (bottom - top);
    var tz = (far + near) / (near - far);

    var matrix = new CanvasMatrix4();
    matrix.m11 = 2 / (right - left);
    matrix.m12 = 0;
    matrix.m13 = 0;
    matrix.m14 = 0;
    matrix.m21 = 0;
    matrix.m22 = 2 / (top - bottom);
    matrix.m23 = 0;
    matrix.m24 = 0;
    matrix.m31 = 0;
    matrix.m32 = 0;
    matrix.m33 = -2 / (far - near);
    matrix.m34 = 0;
    matrix.m41 = tx;
    matrix.m42 = ty;
    matrix.m43 = tz;
    matrix.m44 = 1;

    this.multRight(matrix);
};

CanvasMatrix4.prototype.frustum = function(left, right, bottom, top, near, far)
{
    var matrix = new CanvasMatrix4();
    var A = (right + left) / (right - left);
    var B = (top + bottom) / (top - bottom);
    var C = -(far + near) / (far - near);
    var D = -(2 * far * near) / (far - near);

    matrix.m11 = (2 * near) / (right - left);
    matrix.m12 = 0;
    matrix.m13 = 0;
    matrix.m14 = 0;

    matrix.m21 = 0;
    matrix.m22 = 2 * near / (top - bottom);
    matrix.m23 = 0;
    matrix.m24 = 0;

    matrix.m31 = A;
    matrix.m32 = B;
    matrix.m33 = C;
    matrix.m34 = -1;

    matrix.m41 = 0;
    matrix.m42 = 0;
    matrix.m43 = D;
    matrix.m44 = 0;

    this.multRight(matrix);
};

CanvasMatrix4.prototype.perspective = function(fovy, aspect, zNear, zFar)
{
    var top = Math.tan(fovy * Math.PI / 360) * zNear;
    var bottom = -top;
    var left = aspect * bottom;
    var right = aspect * top;
    this.frustum(left, right, bottom, top, zNear, zFar);
};

CanvasMatrix4.prototype.lookat = function(eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz)
{
    var matrix = new CanvasMatrix4();

    // Make rotation matrix

    // Z vector
    var zx = eyex - centerx;
    var zy = eyey - centery;
    var zz = eyez - centerz;
    var mag = Math.sqrt(zx * zx + zy * zy + zz * zz);
    if (mag) {
        zx /= mag;
        zy /= mag;
        zz /= mag;
    }

    // Y vector
    var yx = upx;
    var yy = upy;
    var yz = upz;

    // X vector = Y cross Z
    xx =  yy * zz - yz * zy;
    xy = -yx * zz + yz * zx;
    xz =  yx * zy - yy * zx;

    // Recompute Y = Z cross X
    yx = zy * xz - zz * xy;
    yy = -zx * xz + zz * xx;
    yx = zx * xy - zy * xx;

    // cross product gives area of parallelogram, which is < 1.0 for
    // non-perpendicular unit-length vectors; so normalize x, y here

    mag = Math.sqrt(xx * xx + xy * xy + xz * xz);
    if (mag) {
        xx /= mag;
        xy /= mag;
        xz /= mag;
    }

    mag = Math.sqrt(yx * yx + yy * yy + yz * yz);
    if (mag) {
        yx /= mag;
        yy /= mag;
        yz /= mag;
    }

    matrix.m11 = xx;
    matrix.m12 = xy;
    matrix.m13 = xz;
    matrix.m14 = 0;

    matrix.m21 = yx;
    matrix.m22 = yy;
    matrix.m23 = yz;
    matrix.m24 = 0;

    matrix.m31 = zx;
    matrix.m32 = zy;
    matrix.m33 = zz;
    matrix.m34 = 0;

    matrix.m41 = 0;
    matrix.m42 = 0;
    matrix.m43 = 0;
    matrix.m44 = 1;
    matrix.translate(-eyex, -eyey, -eyez);

    this.multRight(matrix);
};

// Support functions
CanvasMatrix4.prototype._determinant2x2 = function(a, b, c, d)
{
    return a * d - b * c;
};

CanvasMatrix4.prototype._determinant3x3 = function(a1, a2, a3, b1, b2, b3, c1, c2, c3)
{
    return a1 * this._determinant2x2(b2, b3, c2, c3) -
         b1 * this._determinant2x2(a2, a3, c2, c3) +
         c1 * this._determinant2x2(a2, a3, b2, b3);
};

CanvasMatrix4.prototype._determinant4x4 = function()
{
    var a1 = this.m11;
    var b1 = this.m12;
    var c1 = this.m13;
    var d1 = this.m14;

    var a2 = this.m21;
    var b2 = this.m22;
    var c2 = this.m23;
    var d2 = this.m24;

    var a3 = this.m31;
    var b3 = this.m32;
    var c3 = this.m33;
    var d3 = this.m34;

    var a4 = this.m41;
    var b4 = this.m42;
    var c4 = this.m43;
    var d4 = this.m44;

    return a1 * this._determinant3x3(b2, b3, b4, c2, c3, c4, d2, d3, d4) -
         b1 * this._determinant3x3(a2, a3, a4, c2, c3, c4, d2, d3, d4) +
         c1 * this._determinant3x3(a2, a3, a4, b2, b3, b4, d2, d3, d4) -
         d1 * this._determinant3x3(a2, a3, a4, b2, b3, b4, c2, c3, c4);
};

CanvasMatrix4.prototype._makeAdjoint = function()
{
    var a1 = this.m11;
    var b1 = this.m12;
    var c1 = this.m13;
    var d1 = this.m14;

    var a2 = this.m21;
    var b2 = this.m22;
    var c2 = this.m23;
    var d2 = this.m24;

    var a3 = this.m31;
    var b3 = this.m32;
    var c3 = this.m33;
    var d3 = this.m34;

    var a4 = this.m41;
    var b4 = this.m42;
    var c4 = this.m43;
    var d4 = this.m44;

    // Row column labeling reversed since we transpose rows & columns
    this.m11  =   this._determinant3x3(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    this.m21  = - this._determinant3x3(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    this.m31  =   this._determinant3x3(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    this.m41  = - this._determinant3x3(a2, a3, a4, b2, b3, b4, c2, c3, c4);

    this.m12  = - this._determinant3x3(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    this.m22  =   this._determinant3x3(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    this.m32  = - this._determinant3x3(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    this.m42  =   this._determinant3x3(a1, a3, a4, b1, b3, b4, c1, c3, c4);

    this.m13  =   this._determinant3x3(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    this.m23  = - this._determinant3x3(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    this.m33  =   this._determinant3x3(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    this.m43  = - this._determinant3x3(a1, a2, a4, b1, b2, b4, c1, c2, c4);

    this.m14  = - this._determinant3x3(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    this.m24  =   this._determinant3x3(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    this.m34  = - this._determinant3x3(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    this.m44  =   this._determinant3x3(a1, a2, a3, b1, b2, b3, c1, c2, c3);
};</script>
<script>// To generate the help pages for this library, use

// jsdoc --destination ../../../doc/rglwidgetClass --template ~/node_modules/jsdoc-baseline rglClass.src.js

// To validate, use

// setwd(".../inst/htmlwidgets/lib/rglClass")
// hints <- js::jshint(readLines("rglClass.src.js"))
// hints[, c("line", "reason")]

/**
 * The class of an rgl widget
 * @class
*/
rglwidgetClass = function() {
    this.canvas = null;
    this.userMatrix = new CanvasMatrix4();
    this.types = [];
    this.prMatrix = new CanvasMatrix4();
    this.mvMatrix = new CanvasMatrix4();
    this.vp = null;
    this.prmvMatrix = null;
    this.origs = null;
    this.gl = null;
    this.scene = null;
    this.select = {state: "inactive", subscene: null, region: {p1: {x:0, y:0}, p2: {x:0, y:0}}};
    this.drawing = false;
};

    /**
     * Multiply matrix by vector
     * @returns {number[]}
     * @param M {number[][]} Left operand
     * @param v {number[]} Right operand
     */
    rglwidgetClass.prototype.multMV = function(M, v) {
        return [ M.m11 * v[0] + M.m12 * v[1] + M.m13 * v[2] + M.m14 * v[3],
                 M.m21 * v[0] + M.m22 * v[1] + M.m23 * v[2] + M.m24 * v[3],
                 M.m31 * v[0] + M.m32 * v[1] + M.m33 * v[2] + M.m34 * v[3],
                 M.m41 * v[0] + M.m42 * v[1] + M.m43 * v[2] + M.m44 * v[3]
               ];
    };
    
    /**
     * Multiply row vector by Matrix
     * @returns {number[]}
     * @param v {number[]} left operand
     * @param M {number[][]} right operand
     */
    rglwidgetClass.prototype.multVM = function(v, M) {
        return [ M.m11 * v[0] + M.m21 * v[1] + M.m31 * v[2] + M.m41 * v[3],
                 M.m12 * v[0] + M.m22 * v[1] + M.m32 * v[2] + M.m42 * v[3],
                 M.m13 * v[0] + M.m23 * v[1] + M.m33 * v[2] + M.m43 * v[3],
                 M.m14 * v[0] + M.m24 * v[1] + M.m34 * v[2] + M.m44 * v[3]
               ];
    };
    
    /**
     * Euclidean length of a vector
     * @returns {number}
     * @param v {number[]}
     */
    rglwidgetClass.prototype.vlen = function(v) {
      return Math.sqrt(this.dotprod(v, v));
    };

    /**
     * Dot product of two vectors
     * @instance rglwidgetClass
     * @returns {number}
     * @param a {number[]}
     * @param b {number[]}
     */
    rglwidgetClass.prototype.dotprod = function(a, b) {
      return a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
    };

    /**
     * Cross product of two vectors
     * @returns {number[]}
     * @param a {number[]}
     * @param b {number[]}
     */
    rglwidgetClass.prototype.xprod = function(a, b) {
      return [a[1]*b[2] - a[2]*b[1],
          a[2]*b[0] - a[0]*b[2],
          a[0]*b[1] - a[1]*b[0]];
    };

    /**
     * Bind vectors or matrices by columns
     * @returns {number[][]}
     * @param a {number[]|number[][]}
     * @param b {number[]|number[][]}
     */
    rglwidgetClass.prototype.cbind = function(a, b) {
      if (b.length < a.length)
        b = this.repeatToLen(b, a.length);
      else if (a.length < b.length)
        a = this.repeatToLen(a, b.length);
      return a.map(function(currentValue, index, array) {
            return currentValue.concat(b[index]);
      });
    };

    /**
     * Swap elements
     * @returns {any[]}
     * @param a {any[]}
     * @param i {number} Element to swap
     * @param j {number} Other element to swap
     */
    rglwidgetClass.prototype.swap = function(a, i, j) {
      var temp = a[i];
      a[i] = a[j];
      a[j] = temp;
    };

    /**
     * Flatten a matrix into a vector
     * @returns {any[]}
     * @param a {any[][]}
     */
    rglwidgetClass.prototype.flatten = function(arr, result) {
      var value;
      if (typeof result === "undefined") result = [];
      for (var i = 0, length = arr.length; i < length; i++) {
        value = arr[i];
        if (Array.isArray(value)) {
          this.flatten(value, result);
        } else {
          result.push(value);
        }
      }
      return result;
    };

    /**
     * set element of 1d or 2d array as if it was flattened.
     * Column major, zero based!
     * @returns {any[]|any[][]}
     * @param {any[]|any[][]} a - array
     * @param {number} i - element
     * @param {any} value
     */
    rglwidgetClass.prototype.setElement = function(a, i, value) {
      if (Array.isArray(a[0])) {
        var dim = a.length,
            col = Math.floor(i/dim),
            row = i % dim;
        a[row][col] = value;
      } else {
        a[i] = value;
      }
    };

    /**
     * Transpose an array
     * @returns {any[][]}
     * @param {any[][]} a
     */
    rglwidgetClass.prototype.transpose = function(a) {
      var newArray = [],
          n = a.length,
          m = a[0].length,
          i;
      for(i = 0; i < m; i++){
        newArray.push([]);
      }

      for(i = 0; i < n; i++){
        for(var j = 0; j < m; j++){
          newArray[j].push(a[i][j]);
        }
      }
      return newArray;
    };

    /**
     * Calculate sum of squares of a numeric vector
     * @returns {number}
     * @param {number[]} x
     */
    rglwidgetClass.prototype.sumsq = function(x) {
      var result = 0, i;
      for (i=0; i < x.length; i++)
        result += x[i]*x[i];
      return result;
    };

    /**
     * Convert a matrix to a CanvasMatrix4
     * @returns {CanvasMatrix4}
     * @param {number[][]|number[]} mat
     */
    rglwidgetClass.prototype.toCanvasMatrix4 = function(mat) {
      if (mat instanceof CanvasMatrix4)
        return mat;
      var result = new CanvasMatrix4();
      mat = this.flatten(this.transpose(mat));
      result.load(mat);
      return result;
    };

    /**
     * Convert an R-style numeric colour string to an rgb vector
     * @returns {number[]}
     * @param {string} s
     */
    rglwidgetClass.prototype.stringToRgb = function(s) {
      s = s.replace("#", "");
      var bigint = parseInt(s, 16);
      return [((bigint >> 16) & 255)/255,
              ((bigint >> 8) & 255)/255,
               (bigint & 255)/255];
    };

    /**
     * Take a component-by-component product of two 3 vectors
     * @returns {number[]}
     * @param {number[]} x
     * @param {number[]} y
     */
    rglwidgetClass.prototype.componentProduct = function(x, y) {
      if (typeof y === "undefined") {
        this.alertOnce("Bad arg to componentProduct");
      }
      var result = new Float32Array(3), i;
      for (i = 0; i<3; i++)
        result[i] = x[i]*y[i];
      return result;
    };

    /**
     * Get next higher power of two
     * @returns { number }
     * @param { number } value - input value
     */
    rglwidgetClass.prototype.getPowerOfTwo = function(value) {
      var pow = 1;
      while(pow<value) {
        pow *= 2;
      }
      return pow;
    };

    /**
     * Unique entries
     * @returns { any[] }
     * @param { any[] } arr - An array
     */
    rglwidgetClass.prototype.unique = function(arr) {
      arr = [].concat(arr);
      return arr.filter(function(value, index, self) {
        return self.indexOf(value) === index;
      });
    };

    /**
     * Shallow compare of arrays
     * @returns { boolean }
     * @param { any[] } a - An array
     * @param { any[] } b - Another array
     */
    rglwidgetClass.prototype.equalArrays = function(a, b) {
      return a === b || (a && b &&
                      a.length === b.length &&
                      a.every(function(v, i) {return v === b[i];}));
    };
    
    /**
     * Repeat an array to a desired length
     * @returns {any[]}
     * @param {any | any[]} arr The input array
     * @param {number} len The desired output length
     */
    rglwidgetClass.prototype.repeatToLen = function(arr, len) {
      arr = [].concat(arr);
      while (arr.length < len/2)
        arr = arr.concat(arr);
      return arr.concat(arr.slice(0, len - arr.length));
    };

    /**
     * Give a single alert message, not to be repeated.
     * @param {string} msg  The message to give.
     */
    rglwidgetClass.prototype.alertOnce = function(msg) {
      if (typeof this.alerted !== "undefined")
        return;
      this.alerted = true;
      alert(msg);
    };

    rglwidgetClass.prototype.f_is_lit = 1;
    rglwidgetClass.prototype.f_is_smooth = 2;
    rglwidgetClass.prototype.f_has_texture = 4;
    rglwidgetClass.prototype.f_depth_sort = 8;
    rglwidgetClass.prototype.f_fixed_quads = 16;
    rglwidgetClass.prototype.f_is_transparent = 32;
    rglwidgetClass.prototype.f_is_lines = 64;
    rglwidgetClass.prototype.f_sprites_3d = 128;
    rglwidgetClass.prototype.f_sprite_3d = 256;
    rglwidgetClass.prototype.f_is_subscene = 512;
    rglwidgetClass.prototype.f_is_clipplanes = 1024;
    rglwidgetClass.prototype.f_fixed_size = 2048;
    rglwidgetClass.prototype.f_is_points = 4096;
    rglwidgetClass.prototype.f_is_twosided = 8192;
    rglwidgetClass.prototype.f_fat_lines = 16384;
    rglwidgetClass.prototype.f_is_brush = 32768;

    /**
     * Which list does a particular id come from?
     * @returns { string }
     * @param {number} id The id to look up.
     */
    rglwidgetClass.prototype.whichList = function(id) {
      var obj = this.getObj(id),
          flags = obj.flags;
        if (obj.type === "light")
          return "lights";
        if (flags & this.f_is_subscene)
            return "subscenes";
        if (flags & this.f_is_clipplanes)
            return "clipplanes";
        if (flags & this.f_is_transparent)
            return "transparent";
        return "opaque";
    };

    /**
     * Get an object by id number.
     * @returns { Object }
     * @param {number} id
     */
    rglwidgetClass.prototype.getObj = function(id) {
      if (typeof id !== "number") {
        this.alertOnce("getObj id is "+typeof id);
      }
      return this.scene.objects[id];
    };

    /**
     * Get ids of a particular type from a subscene or the whole scene
     * @returns { number[] }
     * @param {string} type What type of object?
     * @param {number} subscene  Which subscene?  If not given, find in the whole scene
     */
    rglwidgetClass.prototype.getIdsByType = function(type, subscene) {
      var
        result = [], i, self = this;
      if (typeof subscene === "undefined") {
        Object.keys(this.scene.objects).forEach(
          function(key) {
            key = parseInt(key, 10);
            if (self.getObj(key).type === type)
              result.push(key);
          });
      } else {
        ids = this.getObj(subscene).objects;
        for (i=0; i < ids.length; i++) {
          if (this.getObj(ids[i]).type === type) {
            result.push(ids[i]);
          }
        }
      }
      return result;
    };

    /**
     * Get a particular material property for an id
     * @returns { any }
     * @param {number} id  Which object?
     * @param {string} property Which material property?
     */
    rglwidgetClass.prototype.getMaterial = function(id, property) {
      var obj = this.getObj(id), mat;
      if (typeof obj.material === "undefined")
        console.error("material undefined");
      mat = obj.material[property];
      if (typeof mat === "undefined")
          mat = this.scene.material[property];
      return mat;
    };

    /**
     * Is a particular id in a subscene?
     * @returns { boolean }
     * @param {number} id Which id?
     * @param {number} subscene Which subscene id?
     */
    rglwidgetClass.prototype.inSubscene = function(id, subscene) {
      return this.getObj(subscene).objects.indexOf(id) > -1;
    };

    /**
     * Add an id to a subscene.
     * @param {number} id Which id?
     * @param {number} subscene Which subscene id?
     */
    rglwidgetClass.prototype.addToSubscene = function(id, subscene) {
      var thelist,
          thesub = this.getObj(subscene),
          ids = [id],
          obj = this.getObj(id), i;
      if (typeof obj != "undefined" && typeof (obj.newIds) !== "undefined") {
        ids = ids.concat(obj.newIds);
      }
      thesub.objects = [].concat(thesub.objects);
      for (i = 0; i < ids.length; i++) {
        id = ids[i];
        if (thesub.objects.indexOf(id) == -1) {
          thelist = this.whichList(id);
          thesub.objects.push(id);
          thesub[thelist].push(id);
        }
      }
    };

    /**
     * Delete an id from a subscene
     * @param { number } id - the id to add
     * @param { number } subscene - the id of the subscene
     */
    rglwidgetClass.prototype.delFromSubscene = function(id, subscene) {
      var thelist,
          thesub = this.getObj(subscene),
          obj = this.getObj(id),
          ids = [id], i;
      if (typeof obj !== "undefined" && typeof (obj.newIds) !== "undefined")
        ids = ids.concat(obj.newIds);
      thesub.objects = [].concat(thesub.objects); // It might be a scalar
      for (j=0; j<ids.length;j++) {
        id = ids[j];
        i = thesub.objects.indexOf(id);
        if (i > -1) {
          thesub.objects.splice(i, 1);
          thelist = this.whichList(id);
          i = thesub[thelist].indexOf(id);
          thesub[thelist].splice(i, 1);
        }
      }
    };

    /**
     * Set the ids in a subscene
     * @param { number[] } ids - the ids to set
     * @param { number } subsceneid - the id of the subscene
     */
    rglwidgetClass.prototype.setSubsceneEntries = function(ids, subsceneid) {
      var sub = this.getObj(subsceneid);
      sub.objects = ids;
      this.initSubscene(subsceneid);
    };

    /**
     * Get the ids in a subscene
     * @returns {number[]}
     * @param { number } subscene - the id of the subscene
     */
    rglwidgetClass.prototype.getSubsceneEntries = function(subscene) {
      return this.getObj(subscene).objects;
    };

    /**
     * Get the ids of the subscenes within a subscene
     * @returns { number[] }
     * @param { number } subscene - the id of the subscene
     */
    rglwidgetClass.prototype.getChildSubscenes = function(subscene) {
      return this.getObj(subscene).subscenes;
    };

    /**
     * Start drawing
     * @returns { boolean } Previous state
     */
    rglwidgetClass.prototype.startDrawing = function() {
    	var value = this.drawing;
    	this.drawing = true;
    	return value;
    };

    /**
     * Stop drawing and check for context loss
     * @param { boolean } saved - Previous state
     */
    rglwidgetClass.prototype.stopDrawing = function(saved) {
      this.drawing = saved;
      if (!saved && this.gl && this.gl.isContextLost())
        this.restartCanvas();
    };

    /**
     * Generate the vertex shader for an object
     * @returns {string}
     * @param { number } id - Id of object
     */
    rglwidgetClass.prototype.getVertexShader = function(id) {
      var obj = this.getObj(id),
          userShader = obj.userVertexShader,
          flags = obj.flags,
          type = obj.type,
          is_lit = flags & this.f_is_lit,
          has_texture = flags & this.f_has_texture,
          fixed_quads = flags & this.f_fixed_quads,
          sprites_3d = flags & this.f_sprites_3d,
          sprite_3d = flags & this.f_sprite_3d,
          nclipplanes = this.countClipplanes(),
          fixed_size = flags & this.f_fixed_size,
          is_points = flags & this.f_is_points,
          is_twosided = flags & this.f_is_twosided,
          fat_lines = flags & this.f_fat_lines,
          is_brush = flags & this.f_is_brush,
          result;

      if (type === "clipplanes" || sprites_3d) return;

      if (typeof userShader !== "undefined") return userShader;

      result = "  /* ****** "+type+" object "+id+" vertex shader ****** */\n"+
      "  attribute vec3 aPos;\n"+
      "  attribute vec4 aCol;\n"+
      " uniform mat4 mvMatrix;\n"+
      " uniform mat4 prMatrix;\n"+
      " varying vec4 vCol;\n"+
      " varying vec4 vPosition;\n";

      if ((is_lit && !fixed_quads && !is_brush) || sprite_3d)
        result = result + "  attribute vec3 aNorm;\n"+
                          " uniform mat4 normMatrix;\n"+
                          " varying vec3 vNormal;\n";

      if (has_texture || type === "text")
        result = result + " attribute vec2 aTexcoord;\n"+
                          " varying vec2 vTexcoord;\n";

      if (fixed_size)
        result = result + "  uniform vec2 textScale;\n";

      if (fixed_quads)
        result = result + "  attribute vec2 aOfs;\n";
      else if (sprite_3d)
        result = result + "  uniform vec3 uOrig;\n"+
                          "  uniform float uSize;\n"+
                          "  uniform mat4 usermat;\n";

      if (is_twosided)
        result = result + "  attribute vec3 aPos1;\n"+
                          "  attribute vec3 aPos2;\n"+
                          "  varying float normz;\n";

      if (fat_lines) {
      	result = result +   "  attribute vec3 aNext;\n"+
                            "  attribute vec2 aPoint;\n"+
                            "  varying vec2 vPoint;\n"+
                            "  varying float vLength;\n"+
                            "  uniform float uAspect;\n"+
                            "  uniform float uLwd;\n";
      }
      
      result = result + "  void main(void) {\n";

      if ((nclipplanes || (!fixed_quads && !sprite_3d)) && !is_brush)
        result = result + "    vPosition = mvMatrix * vec4(aPos, 1.);\n";

      if (!fixed_quads && !sprite_3d && !is_brush)
        result = result + "    gl_Position = prMatrix * vPosition;\n";

      if (is_points) {
        var size = this.getMaterial(id, "size");
        result = result + "    gl_PointSize = "+size.toFixed(1)+";\n";
      }

      result = result + "    vCol = aCol;\n";

      if (is_lit && !fixed_quads && !sprite_3d && !is_brush)
        result = result + "    vNormal = normalize((normMatrix * vec4(aNorm, 1.)).xyz);\n";

      if (has_texture || type == "text")
        result = result + "    vTexcoord = aTexcoord;\n";

      if (fixed_size)
        result = result + "    vec4 pos = prMatrix * mvMatrix * vec4(aPos, 1.);\n"+
                          "   pos = pos/pos.w;\n"+
                          "   gl_Position = pos + vec4(aOfs*textScale, 0.,0.);\n";

      if (type == "sprites" && !fixed_size)
        result = result + "    vec4 pos = mvMatrix * vec4(aPos, 1.);\n"+
                          "   pos = pos/pos.w + vec4(aOfs, 0., 0.);\n"+
                          "   gl_Position = prMatrix*pos;\n";

      if (sprite_3d)
        result = result + "   vNormal = normalize((normMatrix * vec4(aNorm, 1.)).xyz);\n"+
                          "   vec4 pos = mvMatrix * vec4(uOrig, 1.);\n"+
                          "   vPosition = pos/pos.w + vec4(uSize*(vec4(aPos, 1.)*usermat).xyz,0.);\n"+
                          "   gl_Position = prMatrix * vPosition;\n";

      if (is_twosided)
        result = result + "   vec4 pos1 = prMatrix*(mvMatrix*vec4(aPos1, 1.));\n"+
                          "   pos1 = pos1/pos1.w - gl_Position/gl_Position.w;\n"+
                          "   vec4 pos2 = prMatrix*(mvMatrix*vec4(aPos2, 1.));\n"+
                          "   pos2 = pos2/pos2.w - gl_Position/gl_Position.w;\n"+
                          "   normz = pos1.x*pos2.y - pos1.y*pos2.x;\n";
                          
      if (fat_lines) 
        /* This code was inspired by Matt Deslauriers' code in https://mattdesl.svbtle.com/drawing-lines-is-hard */
        result = result + "   vec2 aspectVec = vec2(uAspect, 1.0);\n"+
                          "   mat4 projViewModel = prMatrix * mvMatrix;\n"+
                          "   vec4 currentProjected = projViewModel * vec4(aPos, 1.0);\n"+
                          "   currentProjected = currentProjected/currentProjected.w;\n"+
                          "   vec4 nextProjected = projViewModel * vec4(aNext, 1.0);\n"+
                          "   vec2 currentScreen = currentProjected.xy * aspectVec;\n"+
                          "   vec2 nextScreen = (nextProjected.xy / nextProjected.w) * aspectVec;\n"+
                          "   float len = uLwd;\n"+
                          "   vec2 dir = vec2(1.0, 0.0);\n"+
                          "   vPoint = aPoint;\n"+
                          "   vLength = length(nextScreen - currentScreen)/2.0;\n"+
                          "   vLength = vLength/(vLength + len);\n"+
                          "   if (vLength > 0.0) {\n"+
                          "     dir = normalize(nextScreen - currentScreen);\n"+
                          "   }\n"+
                          "   vec2 normal = vec2(-dir.y, dir.x);\n"+
                          "   dir.x /= uAspect;\n"+
                          "   normal.x /= uAspect;\n"+
                          "   vec4 offset = vec4(len*(normal*aPoint.x*aPoint.y - dir), 0.0, 0.0);\n"+
                          "   gl_Position = currentProjected + offset;\n";

      if (is_brush)
        result = result + "   gl_Position = vec4(aPos, 1.);\n";
        
      result = result + "  }\n";

      // console.log(result);
      return result;
    };

    /**
     * Generate the fragment shader for an object
     * @returns {string}
     * @param { number } id - Id of object
     */
    rglwidgetClass.prototype.getFragmentShader = function(id) {
      var obj = this.getObj(id),
          userShader = obj.userFragmentShader,
          flags = obj.flags,
          type = obj.type,
          is_lit = flags & this.f_is_lit,
          has_texture = flags & this.f_has_texture,
          fixed_quads = flags & this.f_fixed_quads,
          sprites_3d = flags & this.f_sprites_3d,
          is_twosided = (flags & this.f_is_twosided) > 0,
          fat_lines = flags & this.f_fat_lines,
          is_transparent = flags & this.f_is_transparent,
          nclipplanes = this.countClipplanes(), i,
          texture_format, nlights,
          result;

      if (type === "clipplanes" || sprites_3d) return;

      if (typeof userShader !== "undefined") return userShader;

      if (has_texture)
        texture_format = this.getMaterial(id, "textype");

      result = "/* ****** "+type+" object "+id+" fragment shader ****** */\n"+
               "#ifdef GL_ES\n"+
               "#ifdef GL_FRAGMENT_PRECISION_HIGH\n"+
               "  precision highp float;\n"+
               "#else\n"+
               "  precision mediump float;\n"+
               "#endif\n"+
               "#endif\n"+
               "  varying vec4 vCol; // carries alpha\n"+
               "  varying vec4 vPosition;\n";

      if (has_texture || type === "text")
        result = result + "  varying vec2 vTexcoord;\n"+
                          " uniform sampler2D uSampler;\n";

      if (is_lit && !fixed_quads)
        result = result + "  varying vec3 vNormal;\n";

      for (i = 0; i < nclipplanes; i++)
        result = result + "  uniform vec4 vClipplane"+i+";\n";

      if (is_lit) {
        nlights = this.countLights();
        if (nlights)
            result = result + "  uniform mat4 mvMatrix;\n";
        else
            is_lit = false;
      }

      if (is_lit) {
        result = result + "   uniform vec3 emission;\n"+
                          "   uniform float shininess;\n";

        for (i=0; i < nlights; i++) {
          result = result + "   uniform vec3 ambient" + i + ";\n"+
                            "   uniform vec3 specular" + i +"; // light*material\n"+
                            "   uniform vec3 diffuse" + i + ";\n"+
                            "   uniform vec3 lightDir" + i + ";\n"+
                            "   uniform bool viewpoint" + i + ";\n"+
                            "   uniform bool finite" + i + ";\n";
        }
      }

      if (is_twosided)
        result = result + "   uniform bool front;\n"+
                          "   varying float normz;\n";
                          
      if (fat_lines)
        result = result + "   varying vec2 vPoint;\n"+
                          "   varying float vLength;\n";

      result = result + "  void main(void) {\n";
      
      if (fat_lines) {
        result = result + "    vec2 point = vPoint;\n"+
                          "    bool neg = point.y < 0.0;\n"+
                          "    point.y = neg ? "+
                          "      (point.y + vLength)/(1.0 - vLength) :\n"+
                          "     -(point.y - vLength)/(1.0 - vLength);\n";
        if (is_transparent && type == "linestrip")
          result = result+"    if (neg && length(point) <= 1.0) discard;\n";
        result = result + "    point.y = min(point.y, 0.0);\n"+
                          "    if (length(point) > 1.0) discard;\n";
      }

      for (i=0; i < nclipplanes;i++)
        result = result + "    if (dot(vPosition, vClipplane"+i+") < 0.0) discard;\n";

      if (fixed_quads) {
        result = result +   "    vec3 n = vec3(0., 0., 1.);\n";
      } else if (is_lit) {
      	result = result +   "    vec3 n = normalize(vNormal);\n";
      }

      if (is_twosided) {
      	result = result +   "    if ((normz <= 0.) != front) discard;\n";
      }

      if (is_lit) {
        result = result + "    vec3 eye = normalize(-vPosition.xyz);\n"+
                          "   vec3 lightdir;\n"+
                          "   vec4 colDiff;\n"+
                          "   vec3 halfVec;\n"+
                          "   vec4 lighteffect = vec4(emission, 0.);\n"+
                          "   vec3 col;\n"+
                          "   float nDotL;\n";
        if (!fixed_quads) {
          result = result +   "   n = -faceforward(n, n, eye);\n";
        }
        for (i=0; i < nlights; i++) {
          result = result + "   colDiff = vec4(vCol.rgb * diffuse" + i + ", vCol.a);\n"+
                            "   lightdir = lightDir" + i + ";\n"+
                            "   if (!viewpoint" + i +")\n"+
                            "     lightdir = (mvMatrix * vec4(lightdir, 1.)).xyz;\n"+
                            "   if (!finite" + i + ") {\n"+
                            "     halfVec = normalize(lightdir + eye);\n"+
                            "   } else {\n"+
                            "     lightdir = normalize(lightdir - vPosition.xyz);\n"+
                            "     halfVec = normalize(lightdir + eye);\n"+
                            "   }\n"+
                            "    col = ambient" + i + ";\n"+
                            "   nDotL = dot(n, lightdir);\n"+
                            "   col = col + max(nDotL, 0.) * colDiff.rgb;\n"+
                            "   col = col + pow(max(dot(halfVec, n), 0.), shininess) * specular" + i + ";\n"+
                            "   lighteffect = lighteffect + vec4(col, colDiff.a);\n";
        }

      } else {
        result = result +   "   vec4 colDiff = vCol;\n"+
                            "    vec4 lighteffect = colDiff;\n";
      }

      if (type === "text")
        result = result +   "    vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n";

      if (has_texture) {
        result = result + {
            rgb:            "   vec4 textureColor = lighteffect*vec4(texture2D(uSampler, vTexcoord).rgb, 1.);\n",
            rgba:           "   vec4 textureColor = lighteffect*texture2D(uSampler, vTexcoord);\n",
            alpha:          "   vec4 textureColor = texture2D(uSampler, vTexcoord);\n"+
                            "   float luminance = dot(vec3(1.,1.,1.), textureColor.rgb)/3.;\n"+
                            "   textureColor =  vec4(lighteffect.rgb, lighteffect.a*luminance);\n",
            luminance:      "   vec4 textureColor = vec4(lighteffect.rgb*dot(texture2D(uSampler, vTexcoord).rgb, vec3(1.,1.,1.))/3., lighteffect.a);\n",
          "luminance.alpha":"    vec4 textureColor = texture2D(uSampler, vTexcoord);\n"+
                            "   float luminance = dot(vec3(1.,1.,1.),textureColor.rgb)/3.;\n"+
                            "   textureColor = vec4(lighteffect.rgb*luminance, lighteffect.a*textureColor.a);\n"
          }[texture_format]+
                            "   gl_FragColor = textureColor;\n";
      } else if (type === "text") {
        result = result +   "    if (textureColor.a < 0.1)\n"+
                            "     discard;\n"+
                            "   else\n"+
                            "     gl_FragColor = textureColor;\n";
      } else
        result = result +   "   gl_FragColor = lighteffect;\n";

      //if (fat_lines)
      //  result = result +   "   gl_FragColor = vec4(0.0, abs(point.x), abs(point.y), 1.0);"
      result = result + "  }\n";

      // console.log(result);
      return result;
    };

    /**
     * Call gl functions to create and compile shader
     * @returns {Object}
     * @param { number } shaderType - gl code for shader type
     * @param { string } code - code for the shader
     */
    rglwidgetClass.prototype.getShader = function(shaderType, code) {
        var gl = this.gl, shader;
        shader = gl.createShader(shaderType);
        gl.shaderSource(shader, code);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS) && !gl.isContextLost())
            alert(gl.getShaderInfoLog(shader));
        return shader;
    };

    /**
     * Handle a texture after its image has been loaded
     * @param { Object } texture - the gl texture object
     * @param { Object } textureCanvas - the canvas holding the image
     */
    rglwidgetClass.prototype.handleLoadedTexture = function(texture, textureCanvas) {
      var gl = this.gl || this.initGL();
      gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);

      gl.bindTexture(gl.TEXTURE_2D, texture);
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, textureCanvas);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_NEAREST);
      gl.generateMipmap(gl.TEXTURE_2D);

      gl.bindTexture(gl.TEXTURE_2D, null);
    };

    /**
     * Get maximum dimension of texture in current browser.
     * @returns {number}
     */
    rglwidgetClass.prototype.getMaxTexSize = function() {
      var gl = this.gl || this.initGL();	
      return Math.min(4096, gl.getParameter(gl.MAX_TEXTURE_SIZE));
    };
    
    /**
     * Load an image to a texture
     * @param { string } uri - The image location
     * @param { Object } texture - the gl texture object
     */
    rglwidgetClass.prototype.loadImageToTexture = function(uri, texture) {
      var canvas = this.textureCanvas,
          ctx = canvas.getContext("2d"),
          image = new Image(),
          self = this;

       image.onload = function() {
         var w = image.width,
             h = image.height,
             canvasX = self.getPowerOfTwo(w),
             canvasY = self.getPowerOfTwo(h),
             gl = self.gl || self.initGL(),
             maxTexSize = self.getMaxTexSize();
         while (canvasX > 1 && canvasY > 1 && (canvasX > maxTexSize || canvasY > maxTexSize)) {
           canvasX /= 2;
           canvasY /= 2;
         }
         canvas.width = canvasX;
         canvas.height = canvasY;
         ctx.imageSmoothingEnabled = true;
         ctx.drawImage(image, 0, 0, canvasX, canvasY);
         self.handleLoadedTexture(texture, canvas);
         self.drawScene();
       };
       image.src = uri;
     };

    /**
     * Draw text to the texture canvas
     * @returns { Object } object with text measurements
     * @param { string } text - the text
     * @param { number } cex - expansion
     * @param { string } family - font family
     * @param { number } font - font number
     */
    rglwidgetClass.prototype.drawTextToCanvas = function(text, cex, family, font) {
       var canvasX, canvasY,
           textY,
           scaling = 20,
           textColour = "white",

           backgroundColour = "rgba(0,0,0,0)",
           canvas = this.textureCanvas,
           ctx = canvas.getContext("2d"),
           i, textHeight = 0, textHeights = [], width, widths = [], 
           offsetx, offsety = 0, line, lines = [], offsetsx = [],
           offsetsy = [], lineoffsetsy = [], fontStrings = [],
           maxTexSize = this.getMaxTexSize(),
           getFontString = function(i) {
             textHeights[i] = scaling*cex[i];
             var fontString = textHeights[i] + "px",
                 family0 = family[i],
                 font0 = font[i];
             if (family0 === "sans")
               family0 = "sans-serif";
             else if (family0 === "mono")
               family0 = "monospace";
             fontString = fontString + " " + family0;
             if (font0 === 2 || font0 === 4)
               fontString = "bold " + fontString;
             if (font0 === 3 || font0 === 4)
               fontString = "italic " + fontString;
             return fontString;
           };
       cex = this.repeatToLen(cex, text.length);
       family = this.repeatToLen(family, text.length);
       font = this.repeatToLen(font, text.length);

       canvasX = 1;
       line = -1;
       offsetx = maxTexSize;
       for (i = 0; i < text.length; i++)  {
         ctx.font = fontStrings[i] = getFontString(i);
         width = widths[i] = ctx.measureText(text[i]).width;
         if (offsetx + width > maxTexSize) {
           line += 1;
           offsety = lineoffsetsy[line] = offsety + 2*textHeight;
           if (offsety > maxTexSize)
             console.error("Too many strings for texture.");
           textHeight = 0;
           offsetx = 0;
         }
         textHeight = Math.max(textHeight, textHeights[i]);
         offsetsx[i] = offsetx;
         offsetx += width;
         canvasX = Math.max(canvasX, offsetx);
         lines[i] = line;
       }
       offsety = lineoffsetsy[line] = offsety + 2*textHeight;
       for (i = 0; i < text.length; i++) {
       	 offsetsy[i] = lineoffsetsy[lines[i]];
       }
       
       canvasX = this.getPowerOfTwo(canvasX);
       canvasY = this.getPowerOfTwo(offsety);

       canvas.width = canvasX;
       canvas.height = canvasY;

       ctx.fillStyle = backgroundColour;
       ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);

       ctx.textBaseline = "alphabetic";
       for(i = 0; i < text.length; i++) {
         ctx.font = fontStrings[i];
         ctx.fillStyle = textColour;
         ctx.textAlign = "left";
         ctx.fillText(text[i], offsetsx[i],  offsetsy[i]);
       }
       return {canvasX:canvasX, canvasY:canvasY,
               widths:widths, textHeights:textHeights,
               offsetsx:offsetsx, offsetsy:offsetsy};
     };

    /**
     * Set the gl viewport and scissor test
     * @param { number } id - id of subscene
     */
    rglwidgetClass.prototype.setViewport = function(id) {
       var gl = this.gl || this.initGL(),
         vp = this.getObj(id).par3d.viewport,
         x = vp.x*this.canvas.width,
         y = vp.y*this.canvas.height,
         width = vp.width*this.canvas.width,
         height = vp.height*this.canvas.height;
       this.vp = {x:x, y:y, width:width, height:height};
       gl.viewport(x, y, width, height);
       gl.scissor(x, y, width, height);
       gl.enable(gl.SCISSOR_TEST);
     };

    /**
     * Set the projection matrix for a subscene
     * @param { number } id - id of subscene
     */
    rglwidgetClass.prototype.setprMatrix = function(id) {
       var subscene = this.getObj(id),
          embedding = subscene.embeddings.projection;
       if (embedding === "replace")
         this.prMatrix.makeIdentity();
       else
         this.setprMatrix(subscene.parent);
       if (embedding === "inherit")
         return;
       // This is based on the Frustum::enclose code from geom.cpp
       var bbox = subscene.par3d.bbox,
           scale = subscene.par3d.scale,
           ranges = [(bbox[1]-bbox[0])*scale[0]/2,
                     (bbox[3]-bbox[2])*scale[1]/2,
                     (bbox[5]-bbox[4])*scale[2]/2],
           radius = Math.sqrt(this.sumsq(ranges))*1.1; // A bit bigger to handle labels
       if (radius <= 0) radius = 1;
       var observer = subscene.par3d.observer,
           distance = observer[2],
           FOV = subscene.par3d.FOV, ortho = FOV === 0,
           t = ortho ? 1 : Math.tan(FOV*Math.PI/360),
           near = distance - radius,
           far = distance + radius,
           hlen,
           aspect = this.vp.width/this.vp.height,
           z = subscene.par3d.zoom,
           userProjection = subscene.par3d.userProjection;
       if (far < 0.0)
         far = 1.0;
       if (near < far/100.0)
         near = far/100.0;
       hlen = t*near;
       if (ortho) {
         if (aspect > 1)
           this.prMatrix.ortho(-hlen*aspect*z, hlen*aspect*z,
                          -hlen*z, hlen*z, near, far);
         else
           this.prMatrix.ortho(-hlen*z, hlen*z,
                          -hlen*z/aspect, hlen*z/aspect,
                          near, far);
       } else {
         if (aspect > 1)
           this.prMatrix.frustum(-hlen*aspect*z, hlen*aspect*z,
                          -hlen*z, hlen*z, near, far);
         else
           this.prMatrix.frustum(-hlen*z, hlen*z,
                          -hlen*z/aspect, hlen*z/aspect,
                          near, far);
       }
       this.prMatrix.multRight(userProjection);
     };

    /**
     * Set the model-view matrix for a subscene
     * @param { number } id - id of the subscene
     */
    rglwidgetClass.prototype.setmvMatrix = function(id) {
       var observer = this.getObj(id).par3d.observer;
       this.mvMatrix.makeIdentity();
       this.setmodelMatrix(id);
       this.mvMatrix.translate(-observer[0], -observer[1], -observer[2]);

     };

    /**
     * Set the model matrix for a subscene
     * @param { number } id - id of the subscene
     */
    rglwidgetClass.prototype.setmodelMatrix = function(id) {
      var subscene = this.getObj(id),
          embedding = subscene.embeddings.model;
      if (embedding !== "inherit") {
        var scale = subscene.par3d.scale,
            bbox = subscene.par3d.bbox,
            center = [(bbox[0]+bbox[1])/2,
                      (bbox[2]+bbox[3])/2,
                      (bbox[4]+bbox[5])/2];
         this.mvMatrix.translate(-center[0], -center[1], -center[2]);
         this.mvMatrix.scale(scale[0], scale[1], scale[2]);
         this.mvMatrix.multRight( subscene.par3d.userMatrix );
       }
       if (embedding !== "replace")
         this.setmodelMatrix(subscene.parent);
     };

    /**
     * Set the normals matrix for a subscene
     * @param { number } subsceneid - id of the subscene
     */
    rglwidgetClass.prototype.setnormMatrix = function(subsceneid) {
       var self = this,
       recurse = function(id) {
         var sub = self.getObj(id),
             embedding = sub.embeddings.model;
         if (embedding !== "inherit") {
           var scale = sub.par3d.scale;
           self.normMatrix.scale(1/scale[0], 1/scale[1], 1/scale[2]);
           self.normMatrix.multRight(sub.par3d.userMatrix);
         }
         if (embedding !== "replace")
           recurse(sub.parent);
       };
       self.normMatrix.makeIdentity();
       recurse(subsceneid);
     };

    /**
     * Set the combined projection-model-view matrix
     */
    rglwidgetClass.prototype.setprmvMatrix = function() {
       this.prmvMatrix = new CanvasMatrix4( this.mvMatrix );
       this.prmvMatrix.multRight( this.prMatrix );
     };

    /**
     * Count clipping planes in a scene
     * @returns {number}
     */
    rglwidgetClass.prototype.countClipplanes = function() {
      return this.countObjs("clipplanes");
    };

    /**
     * Count lights in a scene
     * @returns { number }
     */
    rglwidgetClass.prototype.countLights = function() {
      return this.countObjs("light");
    };

    /**
     * Count objects of specific type in a scene
     * @returns { number }
     * @param { string } type - Type of object to count
     */
    rglwidgetClass.prototype.countObjs = function(type) {
      var self = this,
          bound = 0;

      Object.keys(this.scene.objects).forEach(
        function(key) {
          if (self.getObj(parseInt(key, 10)).type === type)
            bound = bound + 1;
        });
      return bound;
    };

    /**
     * Initialize a subscene
     * @param { number } id - id of subscene.
     */
    rglwidgetClass.prototype.initSubscene = function(id) {
      var sub = this.getObj(id),
          i, obj;

      if (sub.type !== "subscene")
        return;

      sub.par3d.userMatrix = this.toCanvasMatrix4(sub.par3d.userMatrix);
      sub.par3d.userProjection = this.toCanvasMatrix4(sub.par3d.userProjection);
      sub.par3d.userProjection.transpose();
      sub.par3d.listeners = [].concat(sub.par3d.listeners);
      sub.backgroundId = undefined;
      sub.subscenes = [];
      sub.clipplanes = [];
      sub.transparent = [];
      sub.opaque = [];
      sub.lights = [];
      for (i=0; i < sub.objects.length; i++) {
        obj = this.getObj(sub.objects[i]);
        if (typeof obj === "undefined") {
          sub.objects.splice(i, 1);
          i--;
        } else if (obj.type === "background")
          sub.backgroundId = obj.id;
        else
          sub[this.whichList(obj.id)].push(obj.id);
      }
    };

    /**
     * Copy object
     * @param { number } id - id of object to copy
     * @param { string } reuse - Document id of scene to reuse
     */
    rglwidgetClass.prototype.copyObj = function(id, reuse) {
      var obj = this.getObj(id),
          prev = document.getElementById(reuse);
      if (prev !== null) {
        prev = prev.rglinstance;
        var
          prevobj = prev.getObj(id),
          fields = ["flags", "type",
                    "colors", "vertices", "centers",
                    "normals", "offsets",
                    "texts", "cex", "family", "font", "adj",
                    "material",
                    "radii",
                    "texcoords",
                    "userMatrix", "ids",
                    "dim",
                    "par3d", "userMatrix",
                    "viewpoint", "finite",
                    "pos"],
          i;
        for (i = 0; i < fields.length; i++) {
          if (typeof prevobj[fields[i]] !== "undefined")
            obj[fields[i]] = prevobj[fields[i]];
        }
      } else
        console.warn("copyObj failed");
    };

    /**
     * Update the triangles used to display a plane
     * @param { number } id - id of the plane
     * @param { Object } bbox - bounding box in which to display the plane
     */
    rglwidgetClass.prototype.planeUpdateTriangles = function(id, bbox) {
      var perms = [[0,0,1], [1,2,2], [2,1,0]],
          x, xrow, elem, A, d, nhits, i, j, k, u, v, w, intersect, which, v0, v2, vx, reverse,
          face1 = [], face2 = [], normals = [],
          obj = this.getObj(id),
          nPlanes = obj.normals.length;
      obj.bbox = bbox;
      obj.vertices = [];
      obj.initialized = false;
      for (elem = 0; elem < nPlanes; elem++) {
//    Vertex Av = normal.getRecycled(elem);
        x = [];
        A = obj.normals[elem];
        d = obj.offsets[elem][0];
        nhits = 0;
        for (i=0; i<3; i++)
          for (j=0; j<2; j++)
            for (k=0; k<2; k++) {
              u = perms[0][i];
              v = perms[1][i];
              w = perms[2][i];
              if (A[w] !== 0.0) {
                intersect = -(d + A[u]*bbox[j+2*u] + A[v]*bbox[k+2*v])/A[w];
                if (bbox[2*w] < intersect && intersect < bbox[1+2*w]) {
                  xrow = [];
                  xrow[u] = bbox[j+2*u];
                  xrow[v] = bbox[k+2*v];
                  xrow[w] = intersect;
                  x.push(xrow);
                  face1[nhits] = j + 2*u;
                  face2[nhits] = k + 2*v;
                  nhits++;
                }
              }
            }

            if (nhits > 3) {
            /* Re-order the intersections so the triangles work */
              for (i=0; i<nhits-2; i++) {
                which = 0; /* initialize to suppress warning */
                for (j=i+1; j<nhits; j++) {
                  if (face1[i] == face1[j] || face1[i] == face2[j] ||
                      face2[i] == face1[j] || face2[i] == face2[j] ) {
                    which = j;
                    break;
                  }
                }
                if (which > i+1) {
                  this.swap(x, i+1, which);
                  this.swap(face1, i+1, which);
                  this.swap(face2, i+1, which);
                }
              }
            }
            if (nhits >= 3) {
      /* Put in order so that the normal points out the FRONT of the faces */
              v0 = [x[0][0] - x[1][0] , x[0][1] - x[1][1], x[0][2] - x[1][2]];
              v2 = [x[2][0] - x[1][0] , x[2][1] - x[1][1], x[2][2] - x[1][2]];
              /* cross-product */
              vx = this.xprod(v0, v2);
              reverse = this.dotprod(vx, A) > 0;

              for (i=0; i<nhits-2; i++) {
                obj.vertices.push(x[0]);
                normals.push(A);
                for (j=1; j<3; j++) {
                  obj.vertices.push(x[i + (reverse ? 3-j : j)]);
                  normals.push(A);
                }
              }
            }
      }
      obj.pnormals = normals;
    };
    
    rglwidgetClass.prototype.getAdj = function (pos, offset, text) {
      switch(pos) {
        case 1: return [0.5, 1 + offset];
        case 2: return [1 + offset/text.length, 0.5];
        case 3: return [0.5, -offset];
        case 4: return [-offset/text.length, 0.5];
      }
    }

    /**
     * Initialize object for display
     * @param { number } id - id of object to initialize
     */
    rglwidgetClass.prototype.initObj = function(id) {
      var obj = this.getObj(id),
          flags = obj.flags,
          type = obj.type,
          is_lit = flags & this.f_is_lit,
          is_lines = flags & this.f_is_lines,
          fat_lines = flags & this.f_fat_lines,
          has_texture = flags & this.f_has_texture,
          fixed_quads = flags & this.f_fixed_quads,
          is_transparent = obj.is_transparent,
          depth_sort = flags & this.f_depth_sort,
          sprites_3d = flags & this.f_sprites_3d,
          sprite_3d = flags & this.f_sprite_3d,
          fixed_size = flags & this.f_fixed_size,
          is_twosided = (flags & this.f_is_twosided) > 0,
          is_brush = flags & this.f_is_brush,
          gl = this.gl || this.initGL(),
          polygon_offset,
          texinfo, drawtype, nclipplanes, f, nrows, oldrows,
          i,j,v,v1,v2, mat, uri, matobj, pass, passes, pmode,
          dim, nx, nz, attr;

    if (typeof id !== "number") {
      this.alertOnce("initObj id is "+typeof id);
    }

    obj.initialized = true;

    if (type === "bboxdeco" || type === "subscene")
      return;

    if (type === "light") {
      obj.ambient = new Float32Array(obj.colors[0].slice(0,3));
      obj.diffuse = new Float32Array(obj.colors[1].slice(0,3));
      obj.specular = new Float32Array(obj.colors[2].slice(0,3));
      obj.lightDir = new Float32Array(obj.vertices[0]);
      return;
    }

    if (type === "clipplanes") {
      obj.vClipplane = this.flatten(this.cbind(obj.normals, obj.offsets));
      return;
    }

    if (type === "background" && typeof obj.ids !== "undefined") {
      obj.quad = this.flatten([].concat(obj.ids));
      return;
    }

    polygon_offset = this.getMaterial(id, "polygon_offset");
    if (polygon_offset[0] != 0 || polygon_offset[1] != 0)
      obj.polygon_offset = polygon_offset;

    if (is_transparent) {
      depth_sort = ["triangles", "quads", "surface",
                    "spheres", "sprites", "text"].indexOf(type) >= 0;
    }
    
    if (is_brush)
      this.initSelection(id);

    if (typeof obj.vertices === "undefined")
      obj.vertices = [];

    v = obj.vertices;
    obj.vertexCount = v.length;
    if (!obj.vertexCount) return;

    if (is_twosided) {
      if (typeof obj.userAttributes === "undefined")
        obj.userAttributes = {};
      v1 = Array(v.length);
      v2 = Array(v.length);
      if (obj.type == "triangles" || obj.type == "quads") {
      	if (obj.type == "triangles")
      	  nrow = 3;
      	else
      	  nrow = 4;
        for (i=0; i<Math.floor(v.length/nrow); i++)
          for (j=0; j<nrow; j++) {
            v1[nrow*i + j] = v[nrow*i + ((j+1) % nrow)];
            v2[nrow*i + j] = v[nrow*i + ((j+2) % nrow)];
          }
      } else if (obj.type == "surface") {
        dim = obj.dim[0];
        nx = dim[0];
        nz = dim[1];
        for (j=0; j<nx; j++) {
          for (i=0; i<nz; i++) {
            if (i+1 < nz && j+1 < nx) {
              v2[j + nx*i] = v[j + nx*(i+1)];
              v1[j + nx*i] = v[j+1 + nx*(i+1)];
            } else if (i+1 < nz) {
              v2[j + nx*i] = v[j-1 + nx*i];
              v1[j + nx*i] = v[j + nx*(i+1)];
            } else {
              v2[j + nx*i] = v[j + nx*(i-1)];
              v1[j + nx*i] = v[j-1 + nx*(i-1)];
            }
          }
        }
      }
      obj.userAttributes.aPos1 = v1;
      obj.userAttributes.aPos2 = v2;
    }

    if (!sprites_3d) {
      if (gl.isContextLost()) return;
      obj.prog = gl.createProgram();
      gl.attachShader(obj.prog, this.getShader( gl.VERTEX_SHADER,
        this.getVertexShader(id) ));
      gl.attachShader(obj.prog, this.getShader( gl.FRAGMENT_SHADER,
                      this.getFragmentShader(id) ));
      //  Force aPos to location 0, aCol to location 1
      gl.bindAttribLocation(obj.prog, 0, "aPos");
      gl.bindAttribLocation(obj.prog, 1, "aCol");
      gl.linkProgram(obj.prog);
      var linked = gl.getProgramParameter(obj.prog, gl.LINK_STATUS);
      if (!linked) {

        // An error occurred while linking
        var lastError = gl.getProgramInfoLog(obj.prog);
        console.warn("Error in program linking:" + lastError);

        gl.deleteProgram(obj.prog);
        return;
      }
    }

    if (type === "text") {
      texinfo = this.drawTextToCanvas(obj.texts,
                                      this.flatten(obj.cex),
                                      this.flatten(obj.family),
                                      this.flatten(obj.family));
    }

    if (fixed_quads && !sprites_3d) {
      obj.ofsLoc = gl.getAttribLocation(obj.prog, "aOfs");
    }

    if (sprite_3d) {
      obj.origLoc = gl.getUniformLocation(obj.prog, "uOrig");
      obj.sizeLoc = gl.getUniformLocation(obj.prog, "uSize");
      obj.usermatLoc = gl.getUniformLocation(obj.prog, "usermat");
    }

    if (has_texture || type == "text") {
      if (!obj.texture)
        obj.texture = gl.createTexture();
      obj.texLoc = gl.getAttribLocation(obj.prog, "aTexcoord");
      obj.sampler = gl.getUniformLocation(obj.prog, "uSampler");
    }

    if (has_texture) {
      mat = obj.material;
      if (typeof mat.uri !== "undefined")
        uri = mat.uri;
      else if (typeof mat.uriElementId === "undefined") {
        matobj = this.getObj(mat.uriId);
        if (typeof matobj !== "undefined") {
          uri = matobj.material.uri;
        } else {
          uri = "";
        }
      } else
        uri = document.getElementById(mat.uriElementId).rglinstance.getObj(mat.uriId).material.uri;

      this.loadImageToTexture(uri, obj.texture);
    }

    if (type === "text") {
      this.handleLoadedTexture(obj.texture, this.textureCanvas);
    }

    var stride = 3, nc, cofs, nofs, radofs, oofs, tofs, vnew, fnew,
        nextofs = -1, pointofs = -1, alias, colors, key, selection, filter, adj, pos, offset;

    obj.alias = undefined;
    
    colors = obj.colors;

    j = this.scene.crosstalk.id.indexOf(id);
    if (j >= 0) {
      key = this.scene.crosstalk.key[j];
      options = this.scene.crosstalk.options[j];
      colors = colors.slice(0); 
      for (i = 0; i < v.length; i++)
        colors[i] = obj.colors[i % obj.colors.length].slice(0);
      if ( (selection = this.scene.crosstalk.selection) &&
           (selection.length || !options.selectedIgnoreNone) )
        for (i = 0; i < v.length; i++) {
          if (!selection.includes(key[i])) {
            if (options.deselectedColor)
              colors[i] = options.deselectedColor.slice(0);
            colors[i][3] = colors[i][3]*options.deselectedFade;   /* default: mostly transparent if not selected */
          } else if (options.selectedColor)
            colors[i] = options.selectedColor.slice(0);
        }
      if ( (filter = this.scene.crosstalk.filter) )
        for (i = 0; i < v.length; i++) 
          if (!filter.includes(key[i])) {
            if (options.filteredColor)
              colors[i] = options.filteredColor.slice(0);
            colors[i][3] = colors[i][3]*options.filteredFade;   /* default: completely hidden if filtered */
          }
    }  
    
    nc = obj.colorCount = colors.length;
    if (nc > 1) {
      cofs = stride;
      stride = stride + 4;
      v = this.cbind(v, colors);
    } else {
      cofs = -1;
      obj.onecolor = this.flatten(colors);
    }

    if (typeof obj.normals !== "undefined") {
      nofs = stride;
      stride = stride + 3;
      v = this.cbind(v, typeof obj.pnormals !== "undefined" ? obj.pnormals : obj.normals);
    } else
      nofs = -1;

    if (typeof obj.radii !== "undefined") {
      radofs = stride;
      stride = stride + 1;
      // FIXME:  always concat the radii?
      if (obj.radii.length === v.length) {
        v = this.cbind(v, obj.radii);
      } else if (obj.radii.length === 1) {
        v = v.map(function(row, i, arr) { return row.concat(obj.radii[0]);});
      }
    } else
      radofs = -1;
      
    // Add default indices
    f = Array(v.length);
    for (i = 0; i < v.length; i++)
      f[i] = i;
    obj.f = [f,f];

    if (type == "sprites" && !sprites_3d) {
      tofs = stride;
      stride += 2;
      oofs = stride;
      stride += 2;
      vnew = new Array(4*v.length);
      fnew = new Array(4*v.length);
      alias = new Array(v.length);
      var rescale = fixed_size ? 72 : 1,
          size = obj.radii, s = rescale*size[0]/2;
      last = v.length;
      f = obj.f[0];
      for (i=0; i < v.length; i++) {
        if (size.length > 1)
          s = rescale*size[i]/2;
        vnew[i]  = v[i].concat([0,0,-s,-s]);
        fnew[4*i] = f[i];
        vnew[last]= v[i].concat([1,0, s,-s]);
        fnew[4*i+1] = last++;
        vnew[last]= v[i].concat([1,1, s, s]);
        fnew[4*i+2] = last++;
        vnew[last]= v[i].concat([0,1,-s, s]);
        fnew[4*i+3] = last++;
        alias[i] = [last-3, last-2, last-1];
      }
      v = vnew;
      obj.vertexCount = v.length;
      obj.f = [fnew, fnew];
    } else if (type === "text") {
      tofs = stride;
      stride += 2;
      oofs = stride;
      stride += 2;
      vnew = new Array(4*v.length);
      f = obj.f[0];
      fnew = new Array(4*f.length);
      alias = new Array(v.length);
      last = v.length;
      adj = this.flatten(obj.adj);
      if (typeof obj.pos !== "undefined") {
        pos = this.flatten(obj.pos);
        offset = adj[0];
      }
      for (i=0; i < v.length; i++) {
        if (typeof pos !== "undefined")
          adj = this.getAdj(pos[i % pos.length], offset, obj.texts[i]);
        vnew[i]  = v[i].concat([0,-0.5]).concat(adj);
        fnew[4*i] = f[i];
        vnew[last] = v[i].concat([1,-0.5]).concat(adj);
        fnew[4*i+1] = last++;
        vnew[last] = v[i].concat([1, 1.5]).concat(adj);
        fnew[4*i+2] = last++;
        vnew[last] = v[i].concat([0, 1.5]).concat(adj);
        fnew[4*i+3] = last++;
        alias[i] = [last-3, last-2, last-1];
        for (j=0; j < 4; j++) {
          v1 = vnew[fnew[4*i+j]];
          v1[tofs+2] = 2*(v1[tofs]-v1[tofs+2])*texinfo.widths[i];
          v1[tofs+3] = 2*(v1[tofs+1]-v1[tofs+3])*texinfo.textHeights[i];
          v1[tofs] = (texinfo.offsetsx[i] + v1[tofs]*texinfo.widths[i])/texinfo.canvasX;
          v1[tofs+1] = 1.0-(texinfo.offsetsy[i] -
              v1[tofs+1]*texinfo.textHeights[i])/texinfo.canvasY;
          vnew[fnew[4*i+j]] = v1;
        }
      }
      v = vnew;
      obj.vertexCount = v.length;
      obj.f = [fnew, fnew];
    } else if (typeof obj.texcoords !== "undefined") {
      tofs = stride;
      stride += 2;
      oofs = -1;
      v = this.cbind(v, obj.texcoords);
    } else {
      tofs = -1;
      oofs = -1;
    }
    
    obj.alias = alias;
                          
    if (typeof obj.userAttributes !== "undefined") {
      obj.userAttribOffsets = {};
      obj.userAttribLocations = {};
      obj.userAttribSizes = {};
      for (attr in obj.userAttributes) {
      	obj.userAttribLocations[attr] = gl.getAttribLocation(obj.prog, attr);
      	if (obj.userAttribLocations[attr] >= 0) { // Attribute may not have been used
      	  obj.userAttribOffsets[attr] = stride;
      	  v = this.cbind(v, obj.userAttributes[attr]);
      	  stride = v[0].length;
      	  obj.userAttribSizes[attr] = stride - obj.userAttribOffsets[attr];
      	}
      }
    }

    if (typeof obj.userUniforms !== "undefined") {
      obj.userUniformLocations = {};
      for (attr in obj.userUniforms)
        obj.userUniformLocations[attr] = gl.getUniformLocation(obj.prog, attr);
    }

    if (sprites_3d) {
      obj.userMatrix = new CanvasMatrix4(obj.userMatrix);
      obj.objects = this.flatten([].concat(obj.ids));
      is_lit = false;
      for (i=0; i < obj.objects.length; i++)
        this.initObj(obj.objects[i]);
    }

    if (is_lit && !fixed_quads) {
       obj.normLoc = gl.getAttribLocation(obj.prog, "aNorm");
    }

    nclipplanes = this.countClipplanes();
    if (nclipplanes && !sprites_3d) {
      obj.clipLoc = [];
      for (i=0; i < nclipplanes; i++)
        obj.clipLoc[i] = gl.getUniformLocation(obj.prog,"vClipplane" + i);
    }

    if (is_lit) {
      obj.emissionLoc = gl.getUniformLocation(obj.prog, "emission");
      obj.emission = new Float32Array(this.stringToRgb(this.getMaterial(id, "emission")));
      obj.shininessLoc = gl.getUniformLocation(obj.prog, "shininess");
      obj.shininess = this.getMaterial(id, "shininess");
      obj.nlights = this.countLights();
      obj.ambientLoc = [];
      obj.ambient = new Float32Array(this.stringToRgb(this.getMaterial(id, "ambient")));
      obj.specularLoc = [];
      obj.specular = new Float32Array(this.stringToRgb(this.getMaterial(id, "specular")));
      obj.diffuseLoc = [];
      obj.lightDirLoc = [];
      obj.viewpointLoc = [];
      obj.finiteLoc = [];
      for (i=0; i < obj.nlights; i++) {
        obj.ambientLoc[i] = gl.getUniformLocation(obj.prog, "ambient" + i);
        obj.specularLoc[i] = gl.getUniformLocation(obj.prog, "specular" + i);
        obj.diffuseLoc[i] = gl.getUniformLocation(obj.prog, "diffuse" + i);
        obj.lightDirLoc[i] = gl.getUniformLocation(obj.prog, "lightDir" + i);
        obj.viewpointLoc[i] = gl.getUniformLocation(obj.prog, "viewpoint" + i);
        obj.finiteLoc[i] = gl.getUniformLocation(obj.prog, "finite" + i);
      }
    }
    
    obj.passes = is_twosided + 1;
    obj.pmode = new Array(obj.passes);
    for (pass = 0; pass < obj.passes; pass++) {
      if (type === "triangles" || type === "quads" || type === "surface")
      	pmode = this.getMaterial(id, (pass === 0) ? "front" : "back");
      else pmode = "filled";
      obj.pmode[pass] = pmode;
    }
    
 
      obj.f.length = obj.passes;
      for (pass = 0; pass < obj.passes; pass++) {
      	f = fnew = obj.f[pass];
        pmode = obj.pmode[pass];
      	if (pmode === "culled")
      	  f = [];
        else if (pmode === "points") {
          // stay with default
        } else if ((type === "quads" || type === "text" ||
             type === "sprites") && !sprites_3d) {
          nrows = Math.floor(obj.vertexCount/4);
          if (pmode === "filled") {
            fnew = Array(6*nrows);
            for (i=0; i < nrows; i++) {
              fnew[6*i] = f[4*i];
              fnew[6*i+1] = f[4*i + 1];
              fnew[6*i+2] = f[4*i + 2];
              fnew[6*i+3] = f[4*i];
              fnew[6*i+4] = f[4*i + 2];
              fnew[6*i+5] = f[4*i + 3];
            }
          } else {
            fnew = Array(8*nrows);
            for (i=0; i < nrows; i++) {
              fnew[8*i] = f[4*i];
              fnew[8*i+1] = f[4*i + 1];
              fnew[8*i+2] = f[4*i + 1];
              fnew[8*i+3] = f[4*i + 2];
              fnew[8*i+4] = f[4*i + 2];
              fnew[8*i+5] = f[4*i + 3];
              fnew[8*i+6] = f[4*i + 3];
              fnew[8*i+7] = f[4*i];
            }
          }
        } else if (type === "triangles") {
          nrows = Math.floor(obj.vertexCount/3);
          if (pmode === "filled") {
            fnew = Array(3*nrows);
            for (i=0; i < fnew.length; i++) {
              fnew[i] = f[i];
            }
          } else if (pmode === "lines") {
            fnew = Array(6*nrows);
      	    for (i=0; i < nrows; i++) {
      	      fnew[6*i] = f[3*i];
      	      fnew[6*i + 1] = f[3*i + 1];
      	      fnew[6*i + 2] = f[3*i + 1];
      	      fnew[6*i + 3] = f[3*i + 2];
      	      fnew[6*i + 4] = f[3*i + 2];
      	      fnew[6*i + 5] = f[3*i];
      	    }
          }
        } else if (type === "spheres") {
          // default
        } else if (type === "surface") {
          dim = obj.dim[0];
          nx = dim[0];
          nz = dim[1];
          if (pmode === "filled") {
            fnew = [];
            for (j=0; j<nx-1; j++) {
              for (i=0; i<nz-1; i++) {
                fnew.push(f[j + nx*i],
                       f[j + nx*(i+1)],
                       f[j + 1 + nx*(i+1)],
                       f[j + nx*i],
                       f[j + 1 + nx*(i+1)],
                       f[j + 1 + nx*i]);
              }
            }
          } else if (pmode === "lines") {
            fnew = [];
            for (j=0; j<nx; j++) {
              for (i=0; i<nz; i++) {
                if (i+1 < nz)
                  fnew.push(f[j + nx*i],
                         f[j + nx*(i+1)]);
                if (j+1 < nx)
                  fnew.push(f[j + nx*i],
                         f[j+1 + nx*i]);
              }
            }
          }
        }
        obj.f[pass] = fnew;
        if (depth_sort) {
          drawtype = "DYNAMIC_DRAW";
        } else {
          drawtype = "STATIC_DRAW";
        }
      }
    
    
    if (fat_lines) {
      alias = undefined;
      obj.nextLoc = gl.getAttribLocation(obj.prog, "aNext");
      obj.pointLoc = gl.getAttribLocation(obj.prog, "aPoint");
      obj.aspectLoc = gl.getUniformLocation(obj.prog, "uAspect");
      obj.lwdLoc = gl.getUniformLocation(obj.prog, "uLwd");
      // Expand vertices to turn each segment into a pair of triangles
        
      	for (pass = 0; pass < obj.passes; pass++) {
      	  f = obj.f[pass];	
          oldrows = f.length;
      	  if (obj.pmode[pass] === "lines") 
      	    break;
      	}
      
      if (type === "linestrip") 
        nrows = 4*(oldrows - 1); 
      else
        nrows = 2*oldrows;
      vnew = new Array(nrows);
      fnew = new Array(1.5*nrows);
      var fnext = new Array(nrows),
          fpt = new Array(nrows), 
          pt, start, gap = type === "linestrip" ? 3 : 1;
      
      // We're going to turn each pair of vertices into 4 new ones, with the "next" and "pt" attributes
      // added.
      // We do this by copying the originals in the first pass, adding the new attributes, then in a 
      // second pass add new vertices at the end.

      for (i = 0; i < v.length; i++) {
        vnew[i] = v[i].concat([0,0,0,0,0]); 
      }

      nextofs = stride;
      pointofs = stride + 3;
      stride = stride + 5;
            
      // Now add the extras
      last = v.length - 1;
      ind = 0;
      alias = new Array(f.length);
      for (i = 0; i < f.length; i++)
        alias[i] = [];
      for (i = 0; i < f.length - 1; i++) {
      	if (type !== "linestrip" && i % 2 == 1)
      	  continue;
      	k = ++last;
      	vnew[k] = vnew[f[i]].slice();
      	for (j=0; j<3; j++)
      	  vnew[k][nextofs + j] = vnew[f[i+1]][j];
      	vnew[k][pointofs] = -1;
      	vnew[k][pointofs+1] = -1;
      	fnew[ind] = k;
      	last++;
      	vnew[last] = vnew[k].slice();
      	vnew[last][pointofs] = 1;
      	fnew[ind+1] = last;
      	alias[f[i]].push(last-1, last);
      	last++;
      	k = last;
      	vnew[k] = vnew[f[i+1]].slice();
      	for (j=0; j<3; j++)
      	  vnew[k][nextofs + j] = vnew[f[i]][j];
      	vnew[k][pointofs] = -1;
      	vnew[k][pointofs+1] = 1;
      	fnew[ind+2] = k;
      	fnew[ind+3] = fnew[ind+1];
      	last++;
      	vnew[last] = vnew[k].slice();
      	vnew[last][pointofs] = 1;
      	fnew[ind+4] = last;
      	fnew[ind+5] = fnew[ind+2];
      	ind += 6;
      	alias[f[i+1]].push(last-1, last);
      }
      vnew.length = last+1;
      v = vnew;
      obj.vertexCount = v.length;
      if (typeof alias !== "undefined" && typeof obj.alias !== "undefined") {  // Already have aliases from previous section?
        var oldalias = obj.alias, newalias = Array(obj.alias.length);
        for (i = 0; i < newalias.length; i++) {
          newalias[i] = oldalias[i].slice();
          for (j = 0; j < oldalias[i].length; j++)
            Array.prototype.push.apply(newalias[i], alias[oldalias[j]]); // pushes each element 
        }
        obj.alias = newalias;
      } else
        obj.alias = alias;
      
      for (pass = 0; pass < obj.passes; pass++)
      	if (type === "lines" || type === "linestrip" || obj.pmode[pass] == "lines") {
          obj.f[pass] = fnew;
        }
      
      if (depth_sort) 
        drawtype = "DYNAMIC_DRAW";
      else
        drawtype = "STATIC_DRAW";
    }
    
      for (pass = 0; pass < obj.passes; pass++) {
        if (obj.vertexCount > 65535) {
          if (this.index_uint) {
            obj.f[pass] = new Uint32Array(obj.f[pass]);
            obj.index_uint = true;
          } else
            this.alertOnce("Object has "+obj.vertexCount+" vertices, not supported in this browser.");
        } else {
          obj.f[pass] = new Uint16Array(obj.f[pass]);
          obj.index_uint = false;
        }
      }
    
    if (stride !== v[0].length) {
      this.alertOnce("problem in stride calculation");
    }

    obj.vOffsets = {vofs:0, cofs:cofs, nofs:nofs, radofs:radofs, oofs:oofs, tofs:tofs,
                    nextofs:nextofs, pointofs:pointofs, stride:stride};

    obj.values = new Float32Array(this.flatten(v));

    if (type !== "spheres" && !sprites_3d) {
      obj.buf = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
      gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW); //
      obj.ibuf = Array(obj.passes);
      obj.ibuf[0] = gl.createBuffer();
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[0]);
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[0], gl[drawtype]);
      if (is_twosided) {
      	obj.ibuf[1] = gl.createBuffer();
      	gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[1]);
      	gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, obj.f[1], gl[drawtype]);
      }
    }

    if (!sprites_3d) {
      obj.mvMatLoc = gl.getUniformLocation(obj.prog, "mvMatrix");
      obj.prMatLoc = gl.getUniformLocation(obj.prog, "prMatrix");
    }

    if (fixed_size) {
      obj.textScaleLoc = gl.getUniformLocation(obj.prog, "textScale");
    }

    if (is_lit && !sprites_3d) {
      obj.normMatLoc = gl.getUniformLocation(obj.prog, "normMatrix");
    }

    if (is_twosided) {
      obj.frontLoc = gl.getUniformLocation(obj.prog, "front");
    }
  };

    /**
     * Set gl depth test based on object's material
     * @param { number } id - object to use
     */
    rglwidgetClass.prototype.setDepthTest = function(id) {
      var gl = this.gl || this.initGL(),
          tests = {never: gl.NEVER,
                   less:  gl.LESS,
                   equal: gl.EQUAL,
                   lequal:gl.LEQUAL,
                   greater: gl.GREATER,
                   notequal: gl.NOTEQUAL,
                   gequal: gl.GEQUAL,
                   always: gl.ALWAYS},
           test = tests[this.getMaterial(id, "depth_test")];
      gl.depthFunc(test);
    };

    rglwidgetClass.prototype.mode4type = {points : "POINTS",
                     linestrip : "LINE_STRIP",
                     abclines : "LINES",
                     lines : "LINES",
                     sprites : "TRIANGLES",
                     planes : "TRIANGLES",
                     text : "TRIANGLES",
                     quads : "TRIANGLES",
                     surface : "TRIANGLES",
                     triangles : "TRIANGLES"};

    /**
     * Sort objects from back to front
     * @returns { number[] }
     * @param { Object } obj - object to sort
     */
    rglwidgetClass.prototype.depthSort = function(obj) {
      var n = obj.centers.length,
          depths = new Float32Array(n),
          result = new Array(n),
          compare = function(i,j) { return depths[j] - depths[i]; },
          z, w;
      for(i=0; i<n; i++) {
        z = this.prmvMatrix.m13*obj.centers[i][0] +
            this.prmvMatrix.m23*obj.centers[i][1] +
            this.prmvMatrix.m33*obj.centers[i][2] +
            this.prmvMatrix.m43;
        w = this.prmvMatrix.m14*obj.centers[i][0] +
            this.prmvMatrix.m24*obj.centers[i][1] +
            this.prmvMatrix.m34*obj.centers[i][2] +
            this.prmvMatrix.m44;
        depths[i] = z/w;
        result[i] = i;
      }
      result.sort(compare);
      return result;
    };
    
    rglwidgetClass.prototype.disableArrays = function(obj, enabled) {
      var gl = this.gl || this.initGL(),
          objLocs = ["normLoc", "texLoc", "ofsLoc", "pointLoc", "nextLoc"],
          thisLocs = ["posLoc", "colLoc"], i, attr;
      for (i = 0; i < objLocs.length; i++) 
        if (enabled[objLocs[i]]) gl.disableVertexAttribArray(obj[objLocs[i]]);
      for (i = 0; i < thisLocs.length; i++)
        if (enabled[thisLocs[i]]) gl.disableVertexAttribArray(this[objLocs[i]]);
      if (typeof obj.userAttributes !== "undefined") {
      	for (attr in obj.userAttribSizes) {  // Not all attributes may have been used
      	  gl.disableVertexAttribArray( obj.userAttribLocations[attr] );
      	}
      }
    }
    
    /**
     * Draw an object in a subscene
     * @param { number } id - object to draw
     * @param { number } subsceneid - id of subscene
     */
    rglwidgetClass.prototype.drawObj = function(id, subsceneid) {
      var obj = this.getObj(id),
          subscene = this.getObj(subsceneid),
          flags = obj.flags,
          type = obj.type,
          is_lit = flags & this.f_is_lit,
          has_texture = flags & this.f_has_texture,
          fixed_quads = flags & this.f_fixed_quads,
          is_transparent = flags & this.f_is_transparent,
          depth_sort = flags & this.f_depth_sort,
          sprites_3d = flags & this.f_sprites_3d,
          sprite_3d = flags & this.f_sprite_3d,
          is_lines = flags & this.f_is_lines,
          fat_lines = flags & this.f_fat_lines,
          is_points = flags & this.f_is_points,
          fixed_size = flags & this.f_fixed_size,
          is_twosided = (flags & this.f_is_twosided) > 0,
          gl = this.gl || this.initGL(),
          mat,
          sphereMV, baseofs, ofs, sscale, i, count, light,
          pass, mode, pmode, attr,
          enabled = {};

      if (typeof id !== "number") {
        this.alertOnce("drawObj id is "+typeof id);
      }

      if (type === "planes") {
        if (obj.bbox !== subscene.par3d.bbox || !obj.initialized) {
          this.planeUpdateTriangles(id, subscene.par3d.bbox);
        }
      }

      if (!obj.initialized)
        this.initObj(id);

      if (type === "clipplanes") {
        count = obj.offsets.length;
        var IMVClip = [];
        for (i=0; i < count; i++) {
          IMVClip[i] = this.multMV(this.invMatrix, obj.vClipplane.slice(4*i, 4*(i+1)));
         }
         obj.IMVClip = IMVClip;
        return;
      }

      if (type === "light" || type === "bboxdeco" || !obj.vertexCount)
        return;
    
      if (!is_transparent &&
    	  obj.someHidden) {
        is_transparent = true;
        depth_sort = ["triangles", "quads", "surface",
                      "spheres", "sprites", "text"].indexOf(type) >= 0;
      }        

      this.setDepthTest(id);
      
      if (sprites_3d) {
        var norigs = obj.vertices.length,
            savenorm = new CanvasMatrix4(this.normMatrix);
        this.origs = obj.vertices;
        this.usermat = new Float32Array(obj.userMatrix.getAsArray());
        this.radii = obj.radii;
        this.normMatrix = subscene.spriteNormmat;
        for (this.iOrig=0; this.iOrig < norigs; this.iOrig++) {
          for (i=0; i < obj.objects.length; i++) {
            this.drawObj(obj.objects[i], subsceneid);
          }
        }
        this.normMatrix = savenorm;
        return;
      } else {
        gl.useProgram(obj.prog);
      }

      if (typeof obj.polygon_offset !== "undefined") {
        gl.polygonOffset(obj.polygon_offset[0],
                          obj.polygon_offset[1]);
        gl.enable(gl.POLYGON_OFFSET_FILL);
      }

      if (sprite_3d) {
        gl.uniform3fv(obj.origLoc, new Float32Array(this.origs[this.iOrig]));
        if (this.radii.length > 1) {
          gl.uniform1f(obj.sizeLoc, this.radii[this.iOrig][0]);
        } else {
          gl.uniform1f(obj.sizeLoc, this.radii[0][0]);
        }
        gl.uniformMatrix4fv(obj.usermatLoc, false, this.usermat);
      }

      if (type === "spheres") {
        gl.bindBuffer(gl.ARRAY_BUFFER, this.sphere.buf);
      } else {
        gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
      }

      gl.uniformMatrix4fv( obj.prMatLoc, false, new Float32Array(this.prMatrix.getAsArray()) );
      gl.uniformMatrix4fv( obj.mvMatLoc, false, new Float32Array(this.mvMatrix.getAsArray()) );
      var clipcheck = 0,
          clipplaneids = subscene.clipplanes,
          clip, j;
      for (i=0; i < clipplaneids.length; i++) {
        clip = this.getObj(clipplaneids[i]);
        for (j=0; j < clip.offsets.length; j++) {
          gl.uniform4fv(obj.clipLoc[clipcheck + j], clip.IMVClip[j]);
        }
        clipcheck += clip.offsets.length;
      }
      if (typeof obj.clipLoc !== "undefined")
        for (i=clipcheck; i < obj.clipLoc.length; i++)
          gl.uniform4f(obj.clipLoc[i], 0,0,0,0);

      if (is_lit) {
        gl.uniformMatrix4fv( obj.normMatLoc, false, new Float32Array(this.normMatrix.getAsArray()) );
        gl.uniform3fv( obj.emissionLoc, obj.emission);
        gl.uniform1f( obj.shininessLoc, obj.shininess);
        for (i=0; i < subscene.lights.length; i++) {
          light = this.getObj(subscene.lights[i]);
          if (!light.initialized) this.initObj(subscene.lights[i]);
          gl.uniform3fv( obj.ambientLoc[i], this.componentProduct(light.ambient, obj.ambient));
          gl.uniform3fv( obj.specularLoc[i], this.componentProduct(light.specular, obj.specular));
          gl.uniform3fv( obj.diffuseLoc[i], light.diffuse);
          gl.uniform3fv( obj.lightDirLoc[i], light.lightDir);
          gl.uniform1i( obj.viewpointLoc[i], light.viewpoint);
          gl.uniform1i( obj.finiteLoc[i], light.finite);
        }
        for (i=subscene.lights.length; i < obj.nlights; i++) {
          gl.uniform3f( obj.ambientLoc[i], 0,0,0);
          gl.uniform3f( obj.specularLoc[i], 0,0,0);
          gl.uniform3f( obj.diffuseLoc[i], 0,0,0);
        }
      }

      if (fixed_size) {
        gl.uniform2f( obj.textScaleLoc, 0.75/this.vp.width, 0.75/this.vp.height);
      }

      gl.enableVertexAttribArray( this.posLoc );
      enabled.posLoc = true;

      var nc = obj.colorCount;
      count = obj.vertexCount;

      if (type === "spheres") {
        subscene = this.getObj(subsceneid);
        var scale = subscene.par3d.scale,
            scount = count, indices;
        gl.vertexAttribPointer(this.posLoc,  3, gl.FLOAT, false, 4*this.sphere.vOffsets.stride,  0);
        gl.enableVertexAttribArray(obj.normLoc );
        enabled.normLoc = true;
        gl.vertexAttribPointer(obj.normLoc,  3, gl.FLOAT, false, 4*this.sphere.vOffsets.stride,  0);
        gl.disableVertexAttribArray( this.colLoc );
        var sphereNorm = new CanvasMatrix4();
        sphereNorm.scale(scale[0], scale[1], scale[2]);
        sphereNorm.multRight(this.normMatrix);
        gl.uniformMatrix4fv( obj.normMatLoc, false, new Float32Array(sphereNorm.getAsArray()) );

        if (nc == 1) {
          gl.vertexAttrib4fv( this.colLoc, new Float32Array(obj.onecolor));
        }

        if (has_texture) {
          gl.enableVertexAttribArray( obj.texLoc );
          enabled.texLoc = true;
          gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*this.sphere.vOffsets.stride,
                                 4*this.sphere.vOffsets.tofs);
          gl.activeTexture(gl.TEXTURE0);
          gl.bindTexture(gl.TEXTURE_2D, obj.texture);
          gl.uniform1i( obj.sampler, 0);
        }

        if (depth_sort)
          indices = this.depthSort(obj);

        for (i = 0; i < scount; i++) {
          sphereMV = new CanvasMatrix4();

          if (depth_sort) {
            baseofs = indices[i]*obj.vOffsets.stride;
          } else {
            baseofs = i*obj.vOffsets.stride;
          }

          ofs = baseofs + obj.vOffsets.radofs;
          sscale = obj.values[ofs];

          sphereMV.scale(sscale/scale[0], sscale/scale[1], sscale/scale[2]);
          sphereMV.translate(obj.values[baseofs],
                             obj.values[baseofs+1],
                             obj.values[baseofs+2]);
          sphereMV.multRight(this.mvMatrix);
          gl.uniformMatrix4fv( obj.mvMatLoc, false, new Float32Array(sphereMV.getAsArray()) );

          if (nc > 1) {
            ofs = baseofs + obj.vOffsets.cofs;
            gl.vertexAttrib4f( this.colLoc, obj.values[ofs],
                                        obj.values[ofs+1],
                                       obj.values[ofs+2],
                                       obj.values[ofs+3] );
          }
          gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.sphere.ibuf);
          gl.drawElements(gl.TRIANGLES, this.sphere.sphereCount, gl.UNSIGNED_SHORT, 0);
        }
        
        this.disableArrays(obj, enabled);
        if (typeof obj.polygon_offset !== "undefined") 
          gl.disable(gl.POLYGON_OFFSET_FILL);
          
        return;
      } else {
        if (obj.colorCount === 1) {
          gl.disableVertexAttribArray( this.colLoc );
          gl.vertexAttrib4fv( this.colLoc, new Float32Array(obj.onecolor));
        } else {
          gl.enableVertexAttribArray( this.colLoc );
          enabled.colLoc = true;
          gl.vertexAttribPointer(this.colLoc, 4, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.cofs);
        }
      }

      if (is_lit && obj.vOffsets.nofs > 0) {
        gl.enableVertexAttribArray( obj.normLoc );
        enabled.normLoc = true;
        gl.vertexAttribPointer(obj.normLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.nofs);
      }

      if (has_texture || type === "text") {
        gl.enableVertexAttribArray( obj.texLoc );
        enabled.texLoc = true;
        gl.vertexAttribPointer(obj.texLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.tofs);
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, obj.texture);
        gl.uniform1i( obj.sampler, 0);
      }

      if (fixed_quads) {
        gl.enableVertexAttribArray( obj.ofsLoc );
        enabled.ofsLoc = true;
        gl.vertexAttribPointer(obj.ofsLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.oofs);
      }

      if (typeof obj.userAttributes !== "undefined") {
      	for (attr in obj.userAttribSizes) {  // Not all attributes may have been used
      	  gl.enableVertexAttribArray( obj.userAttribLocations[attr] );
      	  gl.vertexAttribPointer( obj.userAttribLocations[attr], obj.userAttribSizes[attr],
      	  			  gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.userAttribOffsets[attr]);
      	}
      }

      if (typeof obj.userUniforms !== "undefined") {
      	for (attr in obj.userUniformLocations) {
      	  var loc = obj.userUniformLocations[attr];
      	  if (loc !== null) {
      	    var uniform = obj.userUniforms[attr];
      	    if (typeof uniform.length === "undefined")
      	      gl.uniform1f(loc, uniform);
      	    else if (typeof uniform[0].length === "undefined") {
      	      uniform = new Float32Array(uniform);
      	      switch(uniform.length) {
      	      	case 2: gl.uniform2fv(loc, uniform); break;
      	      	case 3: gl.uniform3fv(loc, uniform); break;
      	      	case 4: gl.uniform4fv(loc, uniform); break;
      	      	default: console.warn("bad uniform length");
      	      }
      	    } else if (uniform.length == 4 && uniform[0].length == 4)
      	      gl.uniformMatrix4fv(loc, false, new Float32Array(uniform.getAsArray()));
      	    else
      	      console.warn("unsupported uniform matrix");
      	  }
      	}
      }

      for (pass = 0; pass < obj.passes; pass++) {
      	pmode = obj.pmode[pass];
        if (pmode === "culled")
          continue;

      	mode = fat_lines && (is_lines || pmode == "lines") ? "TRIANGLES" : this.mode4type[type];
        if (depth_sort && pmode == "filled") {// Don't try depthsorting on wireframe or points
          var faces = this.depthSort(obj),
              nfaces = faces.length,
              frowsize = Math.floor(obj.f[pass].length/nfaces);

          if (type !== "spheres") {
            var f = obj.index_uint ? new Uint32Array(obj.f[pass].length) : new Uint16Array(obj.f[pass].length);
            for (i=0; i<nfaces; i++) {
              for (j=0; j<frowsize; j++) {
                f[frowsize*i + j] = obj.f[pass][frowsize*faces[i] + j];
              }
            }
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[pass]);
            gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, f, gl.DYNAMIC_DRAW);
          }
        }

      	if (is_twosided)
      	  gl.uniform1i(obj.frontLoc, pass !== 0);

        if (type !== "spheres") 
          gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, obj.ibuf[pass]);

        if (type === "sprites" || type === "text" || type === "quads") {
          count = count * 6/4;
        } else if (type === "surface") {
          count = obj.f[pass].length;
        }

        count = obj.f[pass].length;
      	if (!is_lines && pmode === "lines" && !fat_lines) {
          mode = "LINES";
        } else if (pmode === "points") {
          mode = "POINTS";
        }
                          
        if ((is_lines || pmode === "lines") && fat_lines) {
          gl.enableVertexAttribArray(obj.pointLoc);
          enabled.pointLoc = true;
          gl.vertexAttribPointer(obj.pointLoc, 2, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.pointofs);
          gl.enableVertexAttribArray(obj.nextLoc );
          enabled.nextLoc = true;
          gl.vertexAttribPointer(obj.nextLoc, 3, gl.FLOAT, false, 4*obj.vOffsets.stride, 4*obj.vOffsets.nextofs);
          gl.uniform1f(obj.aspectLoc, this.vp.width/this.vp.height);
          gl.uniform1f(obj.lwdLoc, this.getMaterial(id, "lwd")/this.vp.height);
        }

        gl.vertexAttribPointer(this.posLoc,  3, gl.FLOAT, false, 4*obj.vOffsets.stride,  4*obj.vOffsets.vofs);

        gl.drawElements(gl[mode], count, obj.index_uint ? gl.UNSIGNED_INT : gl.UNSIGNED_SHORT, 0);
        this.disableArrays(obj, enabled);
     }
     
     if (typeof obj.polygon_offset !== "undefined") 
       gl.disable(gl.POLYGON_OFFSET_FILL);
   };

    /**
     * Draw the background for a subscene
     * @param { number } id - id of background object
     * @param { number } subsceneid - id of subscene
     */
    rglwidgetClass.prototype.drawBackground = function(id, subsceneid) {
      var gl = this.gl || this.initGL(),
          obj = this.getObj(id),
          bg, i;

      if (!obj.initialized)
        this.initObj(id);

      if (obj.colors.length) {
        bg = obj.colors[0];
        gl.clearColor(bg[0], bg[1], bg[2], bg[3]);
        gl.depthMask(true);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      }
      if (typeof obj.quad !== "undefined") {
        this.prMatrix.makeIdentity();
        this.mvMatrix.makeIdentity();
        gl.disable(gl.BLEND);
        gl.disable(gl.DEPTH_TEST);
        gl.depthMask(false);
        for (i=0; i < obj.quad.length; i++)
          this.drawObj(obj.quad[i], subsceneid);
      }
    };

    /**
     * Draw a subscene
     * @param { number } subsceneid - id of subscene
     * @param { boolean } opaquePass - is this the opaque drawing pass?
     */
    rglwidgetClass.prototype.drawSubscene = function(subsceneid, opaquePass) {
      var gl = this.gl || this.initGL(),
          sub = this.getObj(subsceneid),
          objects = this.scene.objects,
          subids = sub.objects,
          subscene_has_faces = false,
          subscene_needs_sorting = false,
          flags, i, obj;
      if (sub.par3d.skipRedraw)
        return;
      for (i=0; i < subids.length; i++) {
      	obj = objects[subids[i]];
        flags = obj.flags;
        if (typeof flags !== "undefined") {
          subscene_has_faces |= (flags & this.f_is_lit)
                           & !(flags & this.f_fixed_quads);
          obj.is_transparent = (flags & this.f_is_transparent) || obj.someHidden;
          subscene_needs_sorting |= (flags & this.f_depth_sort) || obj.is_transparent;
        }
      }

      this.setViewport(subsceneid);

      if (typeof sub.backgroundId !== "undefined" && opaquePass)
          this.drawBackground(sub.backgroundId, subsceneid);

      if (subids.length) {
        this.setprMatrix(subsceneid);
        this.setmvMatrix(subsceneid);

        if (subscene_has_faces) {
          this.setnormMatrix(subsceneid);
          if ((sub.flags & this.f_sprites_3d) &&
              typeof sub.spriteNormmat === "undefined") {
            sub.spriteNormmat = new CanvasMatrix4(this.normMatrix);
          }
        }

        if (subscene_needs_sorting)
          this.setprmvMatrix();

        var clipids = sub.clipplanes;
        if (typeof clipids === "undefined") {
          console.warn("bad clipids");
        }
        if (clipids.length > 0) {
          this.invMatrix = new CanvasMatrix4(this.mvMatrix);
          this.invMatrix.invert();
          for (i = 0; i < clipids.length; i++)
            this.drawObj(clipids[i], subsceneid);
        }

        subids = sub.opaque.concat(sub.transparent);
        if (opaquePass) {
          gl.enable(gl.DEPTH_TEST);
          gl.depthMask(true);
          gl.disable(gl.BLEND);
          for (i = 0; i < subids.length; i++) {
            if (!this.getObj(subids[i]).is_transparent)	
              this.drawObj(subids[i], subsceneid);
          }
        } else {
          gl.depthMask(false);
          gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA,
                               gl.ONE, gl.ONE);
          gl.enable(gl.BLEND);
          for (i = 0; i < subids.length; i++) {
            if (this.getObj(subids[i]).is_transparent)
              this.drawObj(subids[i], subsceneid);
          }
        }
        subids = sub.subscenes;
        for (i = 0; i < subids.length; i++) {
          this.drawSubscene(subids[i], opaquePass);
        }
      }
    };
    
    /**
     * Respond to brush change
     */
    rglwidgetClass.prototype.selectionChanged = function() {
      var i, j, k, id, subid = this.select.subscene, subscene,
          objids, obj,
          p1 = this.select.region.p1, p2 = this.select.region.p2,
          filter, selection = [], handle, keys, xmin, x, xmax, ymin, y, ymax, z, v,
          someHidden;
      if (!subid)
        return;
      subscene = this.getObj(subid);
      objids = subscene.objects;
      filter = this.scene.crosstalk.filter;
      this.setmvMatrix(subid);
      this.setprMatrix(subid);
      this.setprmvMatrix();
      xmin = Math.min(p1.x, p2.x);
      xmax = Math.max(p1.x, p2.x);
      ymin = Math.min(p1.y, p2.y);
      ymax = Math.max(p1.y, p2.y);
      for (i = 0; i < objids.length; i++) {
      	id = objids[i];
      	j = this.scene.crosstalk.id.indexOf(id);
      	if (j >= 0) {
      	  keys = this.scene.crosstalk.key[j];
      	  obj = this.getObj(id);
      	  someHidden = false;
      	  for (k = 0; k < keys.length; k++) {
      	    if (filter && filter.indexOf(keys[k]) < 0) {
      	      someHidden = true;
      	      continue;
      	    }
      	    v = [].concat(obj.vertices[k]).concat(1.0);
            v = this.multVM(v, this.prmvMatrix);
            x = v[0]/v[3];
            y = v[1]/v[3];
            z = v[2]/v[3];
            if (xmin <= x && x <= xmax && ymin <= y && y <= ymax && -1.0 <= z && z <= 1.0) {
              selection.push(keys[k]);
            } else
              someHidden = true;
      	  }
      	  obj.someHidden = someHidden && (filter || selection.length);
      	  obj.initialized = false;
      	  /* Who should we notify?  Only shared data in the current subscene, or everyone? */
      	  if (!this.equalArrays(selection, this.scene.crosstalk.selection)) {
      	    handle = this.scene.crosstalk.sel_handle[j];
      	    handle.set(selection, {rglSubsceneId: this.select.subscene});
      	  }
      	}
      }
    };
    
    /**
     * Respond to selection or filter change from crosstalk
     * @param { Object } event - crosstalk event
     * @param { boolean } filter - filter or selection?
     */
    rglwidgetClass.prototype.selection = function(event, filter) {
      	var i, j, ids, obj, keys, crosstalk = this.scene.crosstalk,
      	    selection, someHidden;

      	// Record the message and find out if this event makes some objects have mixed values:
      	
      	crosstalk = this.scene.crosstalk;
      	
      	if (filter) {
      	  filter = crosstalk.filter = event.value;
      	  selection = crosstalk.selection;
      	} else {  
          selection = crosstalk.selection = event.value;
          filter = crosstalk.filter;
      	}
        ids = crosstalk.id;
        for (i = 0; i < ids.length ; i++) {
          obj = this.getObj(ids[i]);
          obj.initialized = false;
          keys = crosstalk.key[i];
          someHidden = false;
          for (j = 0; j < keys.length && !someHidden; j++) {
            if ((filter && filter.indexOf(keys[j]) < 0) ||
                (selection.length && selection.indexOf(keys[j]) < 0))
                someHidden = true;
          }
          obj.someHidden = someHidden;
        }
        this.drawScene();
    };
    
    /**
     * Clear the selection brush
     * @param { number } except - Subscene that should ignore this request
     */
    rglwidgetClass.prototype.clearBrush = function(except) {
      if (this.select.subscene != except) {
        this.select.state = "inactive";
        this.delFromSubscene(this.scene.brushId, this.select.subscene);
      }
      this.drawScene();
    };

    /**
     * Compute mouse coordinates relative to current canvas
     * @returns { Object }
     * @param { Object } event - event object from mouse click
     */
    rglwidgetClass.prototype.relMouseCoords = function(event) {
      var totalOffsetX = 0,
      totalOffsetY = 0,
      currentElement = this.canvas;

      do {
        totalOffsetX += currentElement.offsetLeft;
        totalOffsetY += currentElement.offsetTop;
        currentElement = currentElement.offsetParent;
      }
      while(currentElement);

      var canvasX = event.pageX - totalOffsetX,
          canvasY = event.pageY - totalOffsetY;

      return {x:canvasX, y:canvasY};
    };

    /**
     * Set mouse handlers for the scene
     */
    rglwidgetClass.prototype.setMouseHandlers = function() {
      var self = this, activeSubscene, handler,
          handlers = {}, drag = 0;

      handlers.rotBase = 0;

      this.screenToVector = function(x, y) {
        var viewport = this.getObj(activeSubscene).par3d.viewport,
          width = viewport.width*this.canvas.width,
          height = viewport.height*this.canvas.height,
          radius = Math.max(width, height)/2.0,
          cx = width/2.0,
          cy = height/2.0,
          px = (x-cx)/radius,
          py = (y-cy)/radius,
          plen = Math.sqrt(px*px+py*py);
        if (plen > 1.e-6) {
          px = px/plen;
          py = py/plen;
        }
        var angle = (Math.SQRT2 - plen)/Math.SQRT2*Math.PI/2,
          z = Math.sin(angle),
          zlen = Math.sqrt(1.0 - z*z);
        px = px * zlen;
        py = py * zlen;
        return [px, py, z];
      };

      handlers.trackballdown = function(x,y) {
        var activeSub = this.getObj(activeSubscene),
            activeModel = this.getObj(this.useid(activeSub.id, "model")),
            i, l = activeModel.par3d.listeners;
        handlers.rotBase = this.screenToVector(x, y);
        this.saveMat = [];
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
        }
      };

      handlers.trackballmove = function(x,y) {
        var rotCurrent = this.screenToVector(x,y),
            rotBase = handlers.rotBase,
            dot = rotBase[0]*rotCurrent[0] +
                  rotBase[1]*rotCurrent[1] +
                  rotBase[2]*rotCurrent[2],
            angle = Math.acos( dot/this.vlen(rotBase)/this.vlen(rotCurrent) )*180.0/Math.PI,
            axis = this.xprod(rotBase, rotCurrent),
            objects = this.scene.objects,
            activeSub = this.getObj(activeSubscene),
            activeModel = this.getObj(this.useid(activeSub.id, "model")),
            l = activeModel.par3d.listeners,
            i;
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          activeSub.par3d.userMatrix.load(objects[l[i]].saveMat);
          activeSub.par3d.userMatrix.rotate(angle, axis[0], axis[1], axis[2]);
        }
        this.drawScene();
      };
      handlers.trackballend = 0;

      this.clamp = function(x, lo, hi) {
      	return Math.max(lo, Math.min(x, hi));
      };

      this.screenToPolar = function(x,y) {
        var viewport = this.getObj(activeSubscene).par3d.viewport,
          width = viewport.width*this.canvas.width,
          height = viewport.height*this.canvas.height,
    	  r = Math.min(width, height)/2,
    	  dx = this.clamp(x - width/2, -r, r),
    	  dy = this.clamp(y - height/2, -r, r);
    	  return [Math.asin(dx/r), Math.asin(-dy/r)];
      };

      handlers.polardown = function(x,y) {
        var activeSub = this.getObj(activeSubscene),
            activeModel = this.getObj(this.useid(activeSub.id, "model")),
            i, l = activeModel.par3d.listeners;
        handlers.dragBase = this.screenToPolar(x, y);
        this.saveMat = [];
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
          activeSub.camBase = [-Math.atan2(activeSub.saveMat.m13, activeSub.saveMat.m11),
                               Math.atan2(activeSub.saveMat.m32, activeSub.saveMat.m22)];
        }
      };

      handlers.polarmove = function(x,y) {
        var dragCurrent = this.screenToPolar(x,y),
            activeSub = this.getObj(activeSubscene),
            activeModel = this.getObj(this.useid(activeSub.id, "model")),
            objects = this.scene.objects,
            l = activeModel.par3d.listeners,
            i, changepos = [];
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          for (j=0; j<2; j++)
            changepos[j] = -(dragCurrent[j] - handlers.dragBase[j]);
          activeSub.par3d.userMatrix.makeIdentity();
          activeSub.par3d.userMatrix.rotate(changepos[0]*180/Math.PI, 0,-1,0);
          activeSub.par3d.userMatrix.multRight(objects[l[i]].saveMat);
          activeSub.par3d.userMatrix.rotate(changepos[1]*180/Math.PI, -1,0,0);
        }
        this.drawScene();
      };
      handlers.polarend = 0;

      handlers.axisdown = function(x,y) {
        handlers.rotBase = this.screenToVector(x, this.canvas.height/2);
        var activeSub = this.getObj(activeSubscene),
            activeModel = this.getObj(this.useid(activeSub.id, "model")),
            i, l = activeModel.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          activeSub.saveMat = new CanvasMatrix4(activeSub.par3d.userMatrix);
        }
      };

      handlers.axismove = function(x,y) {
        var rotCurrent = this.screenToVector(x, this.canvas.height/2),
            rotBase = handlers.rotBase,
            angle = (rotCurrent[0] - rotBase[0])*180/Math.PI,
            rotMat = new CanvasMatrix4();
        rotMat.rotate(angle, handlers.axis[0], handlers.axis[1], handlers.axis[2]);
        var activeSub = this.getObj(activeSubscene),
            activeModel = this.getObj(this.useid(activeSub.id, "model")),
            i, l = activeModel.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          activeSub.par3d.userMatrix.load(activeSub.saveMat);
          activeSub.par3d.userMatrix.multLeft(rotMat);
        }
        this.drawScene();
      };
      handlers.axisend = 0;

      handlers.y0zoom = 0;
      handlers.zoom0 = 0;
      handlers.zoomdown = function(x, y) {
        var activeSub = this.getObj(activeSubscene),
          activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
          i, l = activeProjection.par3d.listeners;
        handlers.y0zoom = y;
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          activeSub.zoom0 = Math.log(activeSub.par3d.zoom);
        }
      };
      handlers.zoommove = function(x, y) {
        var activeSub = this.getObj(activeSubscene),
            activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
            i, l = activeProjection.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          activeSub.par3d.zoom = Math.exp(activeSub.zoom0 + (y-handlers.y0zoom)/this.canvas.height);
        }
        this.drawScene();
      };
      handlers.zoomend = 0;

      handlers.y0fov = 0;
      handlers.fovdown = function(x, y) {
        handlers.y0fov = y;
        var activeSub = this.getObj(activeSubscene),
          activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
          i, l = activeProjection.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          activeSub.fov0 = activeSub.par3d.FOV;
        }
      };
      handlers.fovmove = function(x, y) {
        var activeSub = this.getObj(activeSubscene),
            activeProjection = this.getObj(this.useid(activeSub.id, "projection")),
            i, l = activeProjection.par3d.listeners;
        for (i = 0; i < l.length; i++) {
          activeSub = this.getObj(l[i]);
          activeSub.par3d.FOV = Math.max(1, Math.min(179, activeSub.fov0 +
             180*(y-handlers.y0fov)/this.canvas.height));
        }
        this.drawScene();
      };
      handlers.fovend = 0;
      
      handlers.selectingdown = function(x, y) {
      	var viewport = this.getObj(activeSubscene).par3d.viewport,
      	  width = viewport.width*this.canvas.width,
      	  height = viewport.height*this.canvas.height, 
          p = {x: 2.0*x/width - 1.0, y: 2.0*y/height - 1.0};
      	this.select.region = {p1: p, p2: p};
      	if (this.select.subscene && this.select.subscene != activeSubscene)
      	  this.delFromSubscene(this.scene.brushId, this.select.subscene);
      	this.select.subscene = activeSubscene;
      	this.addToSubscene(this.scene.brushId, activeSubscene);
      	this.select.state = "changing";
      	if (typeof this.scene.brushId !== "undefined")
      	  this.getObj(this.scene.brushId).initialized = false;
      	this.drawScene();
      };
      
      handlers.selectingmove = function(x, y) {
      	var viewport = this.getObj(activeSubscene).par3d.viewport,
      	  width = viewport.width*this.canvas.width,
      	  height = viewport.height*this.canvas.height;
      	if (this.select.state === "inactive") 
      	  return;
      	this.select.region.p2 = {x: 2.0*x/width - 1.0, y: 2.0*y/height - 1.0};
      	if (typeof this.scene.brushId !== "undefined")
      	  this.getObj(this.scene.brushId).initialized = false;
      	this.drawScene();
      };
      
      handlers.selectingend = 0;

      this.canvas.onmousedown = function ( ev ){
        if (!ev.which) // Use w3c defns in preference to MS
        switch (ev.button) {
          case 0: ev.which = 1; break;
          case 1:
          case 4: ev.which = 2; break;
          case 2: ev.which = 3;
        }
        drag = ["left", "middle", "right"][ev.which-1];
        var coords = self.relMouseCoords(ev);
        coords.y = self.canvas.height-coords.y;
        activeSubscene = self.whichSubscene(coords);
        var sub = self.getObj(activeSubscene), f;
        handler = sub.par3d.mouseMode[drag];
        switch (handler) {
        case "xAxis":
          handler = "axis";
          handlers.axis = [1.0, 0.0, 0.0];
          break;
        case "yAxis":
          handler = "axis";
          handlers.axis = [0.0, 1.0, 0.0];
          break;
        case "zAxis":
          handler = "axis";
          handlers.axis = [0.0, 0.0, 1.0];
          break;
        }
        f = handlers[handler + "down"];
        if (f) {
          coords = self.translateCoords(activeSubscene, coords);
          f.call(self, coords.x, coords.y);
          ev.preventDefault();
        } else
          console.warn("Mouse handler '" + handler + "' is not implemented.");

      };

      this.canvas.onmouseup = function ( ev ){
        if ( drag === 0 ) return;
        var f = handlers[handler + "end"];
        if (f) {
          f.call(self);
          ev.preventDefault();
        }
        drag = 0;
      };

      this.canvas.onmouseout = this.canvas.onmouseup;

      this.canvas.onmousemove = function ( ev ) {
        if ( drag === 0 ) return;
        var f = handlers[handler + "move"];
        if (f) {
          var coords = self.relMouseCoords(ev);
          coords.y = self.canvas.height - coords.y;
          coords = self.translateCoords(activeSubscene, coords);
          f.call(self, coords.x, coords.y);
        }
      };

      handlers.wheelHandler = function(ev) {
        var del = 1.02, i;
        if (ev.shiftKey) del = 1.002;
        var ds = ((ev.detail || ev.wheelDelta) > 0) ? del : (1 / del);
        if (typeof activeSubscene === "undefined")
          activeSubscene = self.scene.rootSubscene;
        var activeSub = self.getObj(activeSubscene),
            activeProjection = self.getObj(self.useid(activeSub.id, "projection")),
            l = activeProjection.par3d.listeners;

        for (i = 0; i < l.length; i++) {
          activeSub = self.getObj(l[i]);
          activeSub.par3d.zoom *= ds;
        }
        self.drawScene();
        ev.preventDefault();
      };

      this.canvas.addEventListener("DOMMouseScroll", handlers.wheelHandler, false);
      this.canvas.addEventListener("mousewheel", handlers.wheelHandler, false);
    };

    /**
     * Find a particular subscene by inheritance
     * @returns { number } id of subscene to use
     * @param { number } subsceneid - child subscene
     * @param { string } type - type of inheritance:  "projection" or "model"
     */
    rglwidgetClass.prototype.useid = function(subsceneid, type) {
      var sub = this.getObj(subsceneid);
      if (sub.embeddings[type] === "inherit")
        return(this.useid(sub.parent, type));
      else
        return subsceneid;
    };

    /**
     * Check whether point is in viewport of subscene
     * @returns {boolean}
     * @param { Object } coords - screen coordinates of point
     * @param { number } subsceneid - subscene to check
     */
    rglwidgetClass.prototype.inViewport = function(coords, subsceneid) {
      var viewport = this.getObj(subsceneid).par3d.viewport,
        x0 = coords.x - viewport.x*this.canvas.width,
        y0 = coords.y - viewport.y*this.canvas.height;
      return 0 <= x0 && x0 <= viewport.width*this.canvas.width &&
             0 <= y0 && y0 <= viewport.height*this.canvas.height;
    };

    /**
     * Find which subscene contains a point
     * @returns { number } subscene id
     * @param { Object } coords - coordinates of point
     */
    rglwidgetClass.prototype.whichSubscene = function(coords) {
      var self = this,
          recurse = function(subsceneid) {
            var subscenes = self.getChildSubscenes(subsceneid), i, id;
            for (i=0; i < subscenes.length; i++) {
              id = recurse(subscenes[i]);
              if (typeof(id) !== "undefined")
                return(id);
            }
            if (self.inViewport(coords, subsceneid))
              return(subsceneid);
            else
              return undefined;
          },
          rootid = this.scene.rootSubscene,
          result = recurse(rootid);
      if (typeof(result) === "undefined")
        result = rootid;
      return result;
    };

    /**
     * Translate from window coordinates to viewport coordinates
     * @returns { Object } translated coordinates
     * @param { number } subsceneid - which subscene to use?
     * @param { Object } coords - point to translate
     */
    rglwidgetClass.prototype.translateCoords = function(subsceneid, coords) {
      var viewport = this.getObj(subsceneid).par3d.viewport;
      return {x: coords.x - viewport.x*this.canvas.width,
              y: coords.y - viewport.y*this.canvas.height};
    };

    /**
     * Initialize the sphere object
     */
    rglwidgetClass.prototype.initSphere = function() {
      var verts = this.scene.sphereVerts,
          reuse = verts.reuse, result;
      if (typeof reuse !== "undefined") {
        var prev = document.getElementById(reuse).rglinstance.sphere;
        result = {values: prev.values, vOffsets: prev.vOffsets, it: prev.it};
      } else
        result = {values: new Float32Array(this.flatten(this.cbind(this.transpose(verts.vb),
                    this.transpose(verts.texcoords)))),
                  it: new Uint16Array(this.flatten(this.transpose(verts.it))),
                  vOffsets: {vofs:0, cofs:-1, nofs:-1, radofs:-1, oofs:-1,
                    tofs:3, nextofs:-1, pointofs:-1, stride:5}};

      result.sphereCount = result.it.length;
      this.sphere = result;
    };
    
    /**
     * Set the vertices in the selection box object
     */
    rglwidgetClass.prototype.initSelection = function(id) {
      if (typeof this.select.region === "undefined")
        return;
      var obj = this.getObj(id),
          width = this.canvas.width,
          height = this.canvas.height, 
          p1 = this.select.region.p1,
          p2 = this.select.region.p2;
          
      obj.vertices = [[p1.x, p1.y, 0.0],
                      [p2.x, p1.y, 0.0],
                      [p2.x, p2.y, 0.0],
                      [p1.x, p2.y, 0.0],
                      [p1.x, p1.y, 0.0]];
    };

    /**
     * Do the gl part of initializing the sphere
     */
    rglwidgetClass.prototype.initSphereGL = function() {
      var gl = this.gl || this.initGL(), sphere = this.sphere;
      if (gl.isContextLost()) return;
      sphere.buf = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, sphere.buf);
      gl.bufferData(gl.ARRAY_BUFFER, sphere.values, gl.STATIC_DRAW);
      sphere.ibuf = gl.createBuffer();
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, sphere.ibuf);
      gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, sphere.it, gl.STATIC_DRAW);
      return;
    };

    /**
     * Initialize the DOM object
     * @param { Object } el - the DOM object
     * @param { Object } x - the scene data sent by JSON from R
     */
    rglwidgetClass.prototype.initialize = function(el, x) {
      this.textureCanvas = document.createElement("canvas");
      this.textureCanvas.style.display = "block";
      this.scene = x;
      this.normMatrix = new CanvasMatrix4();
      this.saveMat = {};
      this.distance = null;
      this.posLoc = 0;
      this.colLoc = 1;
      if (el) {
        el.rglinstance = this;
        this.el = el;
        this.webGLoptions = el.rglinstance.scene.webGLoptions;
        this.initCanvas();
      }
      if (typeof Shiny !== "undefined") {
        var self = this;
        Shiny.addCustomMessageHandler("shinyGetPar3d",
          function(message) {
            var i, param, 
                subscene = self.getObj(message.subscene),
                parameters = [].concat(message.parameters),
                result = {tag: message.tag, subscene: message.subscene};
            if (typeof subscene !== "undefined") {
              for (i = 0; i < parameters.length; i++) {
                param = parameters[i];
                result[param] = subscene.par3d[param];
              };
            } else {
              console.log("subscene "+message.subscene+" undefined.")
            }
            Shiny.setInputValue("par3d:shinyPar3d", result, {priority: "event"});
          });
          
        Shiny.addCustomMessageHandler("shinySetPar3d",
          function(message) {
            var param = message.parameter, 
                subscene = self.getObj(message.subscene);
            if (typeof subscene !== "undefined") {
              subscene.par3d[param] = message.value;
              subscene.initialized = false;
              self.drawScene();
            } else {
              console.log("subscene "+message.subscene+" undefined.")
            }
          })
      }
    };

    /**
     * Restart the WebGL canvas
     */
    rglwidgetClass.prototype.restartCanvas = function() {
      var newcanvas = document.createElement("canvas"),
          self = this;
      newcanvas.width = this.el.width;
      newcanvas.height = this.el.height;
      newcanvas.addEventListener("webglcontextrestored",
        this.onContextRestored, false);
      newcanvas.addEventListener("webglcontextlost",
        this.onContextLost, false);
      while (this.el.firstChild) {
        this.el.removeChild(this.el.firstChild);
      }
      this.el.appendChild(newcanvas);
      this.canvas = newcanvas;
      this.setMouseHandlers();
      if (this.gl) 
        Object.keys(this.scene.objects).forEach(function(key){
          self.getObj(parseInt(key, 10)).texture = undefined; 
          });
      this.gl = null;
    };

    /**
     * Initialize the WebGL canvas
     */
    rglwidgetClass.prototype.initCanvas = function() {
      this.restartCanvas();
      var objs = this.scene.objects,
          self = this;
      Object.keys(objs).forEach(function(key){
        var id = parseInt(key, 10),
            obj = self.getObj(id);
        if (typeof obj.reuse !== "undefined")
          self.copyObj(id, obj.reuse);
      });
      Object.keys(objs).forEach(function(key){
        self.initSubscene(parseInt(key, 10));
      });
      this.setMouseHandlers();
      this.initSphere();

      this.onContextRestored = function(event) {
        self.initGL();
        self.drawScene();
      };

      this.onContextLost = function(event) {
        if (!self.drawing)
          this.gl = null;
        event.preventDefault();
      };

      this.initGL0();
      this.lazyLoadScene = function() {
      	if (typeof self.slide === "undefined")
      	  self.slide = self.getSlide();
      	if (self.isInBrowserViewport()) {
      	  if (!self.gl || self.gl.isContextLost())
      	    self.initGL();
      	  self.drawScene();
      	}
      };
      window.addEventListener("DOMContentLoaded", this.lazyLoadScene, false);
      window.addEventListener("load", this.lazyLoadScene, false);
      window.addEventListener("resize", this.lazyLoadScene, false);
      window.addEventListener("scroll", this.lazyLoadScene, false);
      this.slide = this.getSlide();
      if (this.slide) {
        if (typeof this.slide.rgl === "undefined")
          this.slide.rgl = [this];
        else
          this.slide.rgl.push(this);
        if (this.scene.context.rmarkdown) 
          if (this.scene.context.rmarkdown === "ioslides_presentation") {
            this.slide.setAttribute("slideenter", "this.rgl.forEach(function(scene) { scene.lazyLoadScene.call(window);})");
          } else if (this.scene.context.rmarkdown === "slidy_presentation") {
            // This method would also work in ioslides, but it gets triggered
            // something like 5 times per slide for every slide change, so
            // you'd need a quicker function than lazyLoadScene.
            var MutationObserver = window.MutationObserver || window.WebKitMutationObserver || window.MozMutationObserver,
            observer = new MutationObserver(function(mutations) {
              mutations.forEach(function(mutation) {
                self.slide.rgl.forEach(function(scene) { scene.lazyLoadScene.call(window); });});});
            observer.observe(this.slide, { attributes: true, attributeFilter:["class"] });
          }
      }
    };

    /**
     * Start the writeWebGL scene. This is only used by writeWebGL; rglwidget has
       no debug element and does the drawing in rglwidget.js.
     */
    rglwidgetClass.prototype.start = function() {
      if (typeof this.prefix !== "undefined") {
        this.debugelement = document.getElementById(this.prefix + "debug");
        this.debug("");
      }
      this.drag = 0;
      this.drawScene();
    };

    /**
     * Display a debug message
     * @param { string } msg - The message to display
     * @param { Object } [img] - Image to insert before message
     */
    rglwidgetClass.prototype.debug = function(msg, img) {
      if (typeof this.debugelement !== "undefined" && this.debugelement !== null) {
        this.debugelement.innerHTML = msg;
        if (typeof img !== "undefined") {
          this.debugelement.insertBefore(img, this.debugelement.firstChild);
        }
      } else if (msg !== "")
        alert(msg);
    };

    /**
     * Get the snapshot image of this scene
     * @returns { Object } The img DOM element
     */
    rglwidgetClass.prototype.getSnapshot = function() {
      var img;
      if (typeof this.scene.snapshot !== "undefined") {
        img = document.createElement("img");
        img.src = this.scene.snapshot;
        img.alt = "Snapshot";
      }
      return img;
    };

    /**
     * Initial test for WebGL
     */
    rglwidgetClass.prototype.initGL0 = function() {
      if (!window.WebGLRenderingContext){
        alert("Your browser does not support WebGL. See http://get.webgl.org");
        return;
      }
    };

    /**
     * If we are in an ioslides or slidy presentation, get the
     * DOM element of the current slide
     * @returns { Object }
     */
    rglwidgetClass.prototype.getSlide = function() {
      var result = this.el, done = false;
      while (result && !done && this.scene.context.rmarkdown) {
      	switch(this.scene.context.rmarkdown) {
          case "ioslides_presentation":
            if (result.tagName === "SLIDE") return result;
            break;
          case "slidy_presentation":
            if (result.tagName === "DIV" && result.classList.contains("slide"))
              return result;
            break;
          default: return null;
      	}
      	result = result.parentElement;
      }
      return null;
    };

    /**
     * Is this scene visible in the browser?
     * @returns { boolean }
     */
    rglwidgetClass.prototype.isInBrowserViewport = function() {
      var rect = this.canvas.getBoundingClientRect(),
          windHeight = (window.innerHeight || document.documentElement.clientHeight),
          windWidth = (window.innerWidth || document.documentElement.clientWidth);
      if (this.scene.context && this.scene.context.rmarkdown !== null) {
      	if (this.slide)
      	  return (this.scene.context.rmarkdown === "ioslides_presentation" &&
      	          this.slide.classList.contains("current")) ||
      	         (this.scene.context.rmarkdown === "slidy_presentation" &&
      	          !this.slide.classList.contains("hidden"));
      }
      return (
      	rect.top >= -windHeight &&
      	rect.left >= -windWidth &&
      	rect.bottom <= 2*windHeight &&
      	rect.right <= 2*windWidth);
    };

    /**
     * Initialize WebGL
     * @returns { Object } the WebGL context
     */
    rglwidgetClass.prototype.initGL = function() {
      var self = this;
      if (this.gl) {
      	if (!this.drawing && this.gl.isContextLost())
          this.restartCanvas();
        else
          return this.gl;
      }
      // if (!this.isInBrowserViewport()) return; Return what??? At this point we know this.gl is null.
      this.canvas.addEventListener("webglcontextrestored",
        this.onContextRestored, false);
      this.canvas.addEventListener("webglcontextlost",
        this.onContextLost, false);
      this.gl = this.canvas.getContext("webgl", this.webGLoptions) ||
               this.canvas.getContext("experimental-webgl", this.webGLoptions);
      this.index_uint = this.gl.getExtension("OES_element_index_uint");
      var save = this.startDrawing();
      this.initSphereGL();
      Object.keys(this.scene.objects).forEach(function(key){
        self.initObj(parseInt(key, 10));
        });
      this.stopDrawing(save);
      return this.gl;
    };

    /**
     * Resize the display to match element
     * @param { Object } el - DOM element to match
     */
    rglwidgetClass.prototype.resize = function(el) {
      this.canvas.width = el.width;
      this.canvas.height = el.height;
    };

    /**
     * Draw the whole scene
     */
    rglwidgetClass.prototype.drawScene = function() {
      var gl = this.gl || this.initGL(),
          wasDrawing = this.startDrawing();
      if (!wasDrawing) {
        if (this.select.state !== "inactive")
          this.selectionChanged();
        gl.enable(gl.DEPTH_TEST);
        gl.depthFunc(gl.LEQUAL);
        gl.clearDepth(1.0);
        gl.clearColor(1,1,1,1);
        gl.depthMask(true); // Must be true before clearing depth buffer
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        this.drawSubscene(this.scene.rootSubscene, true);
        this.drawSubscene(this.scene.rootSubscene, false);
      }
      this.stopDrawing(wasDrawing);
    };

    /**
     * Change the displayed subset
     * @param { Object } el - Element of the control; not used.
     * @param { Object } control - The subset control data.
     */
    rglwidgetClass.prototype.subsetSetter = function(el, control) {
      if (typeof control.subscenes === "undefined" ||
          control.subscenes === null)
        control.subscenes = this.scene.rootSubscene;
      var value = Math.round(control.value),
          subscenes = [].concat(control.subscenes),
          fullset = [].concat(control.fullset),
          i, j, entries, subsceneid,
          adds = [], deletes = [],
          ismissing = function(x) {
            return fullset.indexOf(x) < 0;
          },
          tointeger = function(x) {
            return parseInt(x, 10);
          };
      if (isNaN(value))
        value = control.value = 0;
      if (control.accumulate)
        for (i=0; i <= value; i++)
          adds = adds.concat(control.subsets[i]);
      else
        adds = adds.concat(control.subsets[value]);
      deletes = fullset.filter(function(x) { return adds.indexOf(x) < 0; });
      for (i = 0; i < subscenes.length; i++) {
        subsceneid = subscenes[i];
        if (typeof this.getObj(subsceneid) === "undefined")
          this.alertOnce("typeof object is undefined");
        for (j = 0; j < adds.length; j++)
          this.addToSubscene(adds[j], subsceneid);
        for (j = 0; j < deletes.length; j++)
          this.delFromSubscene(deletes[j], subsceneid);
      }
    };

    /**
     * Change the requested property
     * @param { Object } el - Element of the control; not used.
     * @param { Object } control - The property setter control data.
     */
    rglwidgetClass.prototype.propertySetter = function(el, control)  {
      var value = control.value,
          values = [].concat(control.values),
          svals = [].concat(control.param),
          direct = values[0] === null,
          entries = [].concat(control.entries),
          ncol = entries.length,
          nrow = values.length/ncol,
          properties = this.repeatToLen(control.properties, ncol),
          objids = this.repeatToLen(control.objids, ncol),
          property, objid = objids[0],
          obj = this.getObj(objid),
          propvals, i, v1, v2, p, entry, gl, needsBinding,
          newprop, newid,

          getPropvals = function() {
            if (property === "userMatrix")
              return obj.par3d.userMatrix.getAsArray();
            else if (property === "scale" || property === "FOV" || property === "zoom")
              return [].concat(obj.par3d[property]);
            else
              return [].concat(obj[property]);
          };

          putPropvals = function(newvals) {
            if (newvals.length == 1)
              newvals = newvals[0];
            if (property === "userMatrix")
              obj.par3d.userMatrix.load(newvals);
            else if (property === "scale" || property === "FOV" || property === "zoom")
              obj.par3d[property] = newvals;
            else
              obj[property] = newvals;
          };

      if (direct && typeof value === "undefined")
        return;

      if (control.interp) {
        values = values.slice(0, ncol).concat(values).
                 concat(values.slice(ncol*(nrow-1), ncol*nrow));
        svals = [-Infinity].concat(svals).concat(Infinity);
        for (i = 1; i < svals.length; i++) {
          if (value <= svals[i]) {
            if (svals[i] === Infinity)
              p = 1;
            else
              p = (svals[i] - value)/(svals[i] - svals[i-1]);
            break;
          }
        }
      } else if (!direct) {
        value = Math.round(value);
      }

      for (j=0; j<entries.length; j++) {
        entry = entries[j];
        newprop = properties[j];
        newid = objids[j];

        if (newprop !== property || newid != objid) {
          if (typeof property !== "undefined")
            putPropvals(propvals);
          property = newprop;
          objid = newid;
          obj = this.getObj(objid);
          propvals = getPropvals();
        }
        if (control.interp) {
          v1 = values[ncol*(i-1) + j];
          v2 = values[ncol*i + j];
          this.setElement(propvals, entry, p*v1 + (1-p)*v2);
        } else if (!direct) {
          this.setElement(propvals, entry, values[ncol*value + j]);
        } else {
          this.setElement(propvals, entry, value[j]);
        }
      }
      putPropvals(propvals);

      needsBinding = [];
      for (j=0; j < entries.length; j++) {
        if (properties[j] === "values" &&
            needsBinding.indexOf(objids[j]) === -1) {
          needsBinding.push(objids[j]);
        }
      }
      for (j=0; j < needsBinding.length; j++) {
        gl = this.gl || this.initGL();
        obj = this.getObj(needsBinding[j]);
        gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
        gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW);
      }
    };

    /**
     * Change the requested vertices
     * @param { Object } el - Element of the control; not used.
     * @param { Object } control - The vertext setter control data.
     */
    rglwidgetClass.prototype.vertexSetter = function(el, control)  {
      var svals = [].concat(control.param),
          j, k, p, a, propvals, stride, ofs, obj, entry,
          attrib,
          ofss    = {x:"vofs", y:"vofs", z:"vofs",
                     red:"cofs", green:"cofs", blue:"cofs",
                     alpha:"cofs", radii:"radofs",
                     nx:"nofs", ny:"nofs", nz:"nofs",
                     ox:"oofs", oy:"oofs", oz:"oofs",
                     ts:"tofs", tt:"tofs"},
          pos     = {x:0, y:1, z:2,
                     red:0, green:1, blue:2,
                     alpha:3,radii:0,
                     nx:0, ny:1, nz:2,
                     ox:0, oy:1, oz:2,
                     ts:0, tt:1},
        values = control.values,
        direct = values === null,
        ncol,
        interp = control.interp,
        vertices = [].concat(control.vertices),
        attributes = [].concat(control.attributes),
        value = control.value, newval, aliases, alias;

      ncol = Math.max(vertices.length, attributes.length);

      if (!ncol)
        return;

      vertices = this.repeatToLen(vertices, ncol);
      attributes = this.repeatToLen(attributes, ncol);

      if (direct)
        interp = false;

      /* JSON doesn't pass Infinity */
      svals[0] = -Infinity;
      svals[svals.length - 1] = Infinity;

      for (j = 1; j < svals.length; j++) {
        if (value <= svals[j]) {
          if (interp) {
            if (svals[j] === Infinity)
              p = 1;
            else
              p = (svals[j] - value)/(svals[j] - svals[j-1]);
          } else {
            if (svals[j] - value > value - svals[j-1])
              j = j - 1;
          }
          break;
        }
      }

      obj = this.getObj(control.objid);
      // First, make sure color attributes vary in original
      if (typeof obj.vOffsets !== "undefined") {
      	varies = true;
        for (k = 0; k < ncol; k++) {
          attrib = attributes[k];
          if (typeof attrib !== "undefined") {
            ofs = obj.vOffsets[ofss[attrib]];
            if (ofs < 0) {
              switch(attrib) {
              	case "alpha":
              	case "red":
              	case "green":
              	case "blue":
              	  obj.colors = [obj.colors[0], obj.colors[0]];
              	  break;
              }
              varies = false;
            }
          }
        }
        if (!varies)
          this.initObj(control.objid);
      }
      propvals = obj.values;
      aliases = obj.alias;
      if (typeof aliases === "undefined")
        aliases = [];
      for (k=0; k<ncol; k++) {
        if (direct) {
          newval = value;
        } else if (interp) {
          newval = p*values[j-1][k] + (1-p)*values[j][k];
        } else {
          newval = values[j][k];
        }      	
        attrib = attributes[k];
        vertex = vertices[k];
        alias = aliases[vertex];
        if (obj.type === "planes" || obj.type === "clipplanes") {
          ofs = ["nx", "ny", "nz", "offset"].indexOf(attrib);
          if (ofs >= 0) {
            if (ofs < 3) {
              if (obj.normals[vertex][ofs] != newval) {  // Assume no aliases here...
              	obj.normals[vertex][ofs] = newval;
              	obj.initialized = false;
              }
            } else {
              if (obj.offsets[vertex][0] != newval) {
              	obj.offsets[vertex][0] = newval;
              	obj.initialized = false;
              }
            }
            continue;
          }
        }
        // Not a plane setting...
        ofs = obj.vOffsets[ofss[attrib]];
        if (ofs < 0)
          this.alertOnce("Attribute '"+attrib+"' not found in object "+control.objid);
        else {
          stride = obj.vOffsets.stride;
          ofs = ofs + pos[attrib];
          entry = vertex*stride + ofs;
          propvals[entry] = newval;
          if (typeof alias !== "undefined")
            for (a = 0; a < alias.length; a++)
              propvals[alias[a]*stride + ofs] = newval;
        }
      }
      if (typeof obj.buf !== "undefined") {
        var gl = this.gl || this.initGL();
        gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
        gl.bufferData(gl.ARRAY_BUFFER, propvals, gl.STATIC_DRAW);
      }
    };

    /**
     * Change the requested vertex properties by age
     * @param { Object } el - Element of the control; not used.
     * @param { Object } control - The age setter control data.
     */
    rglwidgetClass.prototype.ageSetter = function(el, control) {
      var objids = [].concat(control.objids),
          nobjs = objids.length,
          time = control.value,
          births = [].concat(control.births),
          ages = [].concat(control.ages),
          steps = births.length,
          j = Array(steps),
          p = Array(steps),
          i, k, age, j0, propvals, stride, ofs, objid, obj,
          attrib, dim, varies, alias, aliases, a, d,
          attribs = ["colors", "alpha", "radii", "vertices",
                     "normals", "origins", "texcoords",
                     "x", "y", "z",
                     "red", "green", "blue"],
          ofss    = ["cofs", "cofs", "radofs", "vofs",
                     "nofs", "oofs", "tofs",
                     "vofs", "vofs", "vofs",
                     "cofs", "cofs", "cofs"],
          dims    = [3,1,1,3,
                     3,2,2,
                     1,1,1,
                     1,1,1],
          pos     = [0,3,0,0,
                     0,0,0,
                     0,1,2,
                     0,1,2];
      /* Infinity doesn't make it through JSON */
      ages[0] = -Infinity;
      ages[ages.length-1] = Infinity;
      for (i = 0; i < steps; i++) {
        if (births[i] !== null) {  // NA in R becomes null
          age = time - births[i];
          for (j0 = 1; age > ages[j0]; j0++);
          if (ages[j0] == Infinity)
            p[i] = 1;
          else if (ages[j0] > ages[j0-1])
            p[i] = (ages[j0] - age)/(ages[j0] - ages[j0-1]);
          else
            p[i] = 0;
          j[i] = j0;
        }
      }
      // First, make sure color attributes vary in original
      for (l = 0; l < nobjs; l++) {
      	objid = objids[l];
      	obj = this.getObj(objid);
      	varies = true;
        if (typeof obj.vOffsets === "undefined")
          continue;
        for (k = 0; k < attribs.length; k++) {
          attrib = control[attribs[k]];
          if (typeof attrib !== "undefined") {
            ofs = obj.vOffsets[ofss[k]];
            if (ofs < 0) {
              switch(attribs[k]) {
              	case "colors":
              	case "alpha":
              	case "red":
              	case "green":
              	case "blue":
              	  obj.colors = [obj.colors[0], obj.colors[0]];
              	  break;
              }
              varies = false;
            }
          }
        }
        if (!varies)
          this.initObj(objid);
      }
      for (l = 0; l < nobjs; l++) {
        objid = objids[l];
        obj = this.getObj(objid);
        if (typeof obj.vOffsets === "undefined")
          continue;
        aliases = obj.alias;
        if (typeof aliases === "undefined")
          aliases = [];
        propvals = obj.values;
        stride = obj.vOffsets.stride;
        for (k = 0; k < attribs.length; k++) {
          attrib = control[attribs[k]];
          if (typeof attrib !== "undefined") {
            ofs = obj.vOffsets[ofss[k]];
            if (ofs >= 0) {
              dim = dims[k];
              ofs = ofs + pos[k];
              for (i = 0; i < steps; i++) {
              	alias = aliases[i];
                if (births[i] !== null) {
                  for (d=0; d < dim; d++) {
                    propvals[i*stride + ofs + d] = p[i]*attrib[dim*(j[i]-1) + d] + (1-p[i])*attrib[dim*j[i] + d];
                    if (typeof alias !== "undefined")
                      for (a=0; a < alias.length; a++)
                        propvals[alias[a]*stride + ofs + d] = propvals[i*stride + ofs + d];
                  }
                }
              }
            } else
              this.alertOnce("\'"+attribs[k]+"\' property not found in object "+objid);
          }
        }
        obj.values = propvals;
        if (typeof obj.buf !== "undefined") {
          gl = this.gl || this.initGL();
          gl.bindBuffer(gl.ARRAY_BUFFER, obj.buf);
          gl.bufferData(gl.ARRAY_BUFFER, obj.values, gl.STATIC_DRAW);
        }
      }
    };

    /**
     * Bridge to old style control
     * @param { Object } el - Element of the control; not used.
     * @param { Object } control - The bridge control data.
     */
    rglwidgetClass.prototype.oldBridge = function(el, control) {
      var attrname, global = window[control.prefix + "rgl"];
      if (global)
        for (attrname in global)
          this[attrname] = global[attrname];
      window[control.prefix + "rgl"] = this;
    };

    /**
     * Set up a player control
     * @param { Object } el - The player control element
     * @param { Object } control - The player data.
     */
    rglwidgetClass.prototype.Player = function(el, control) {
      var
        self = this,
        components = [].concat(control.components),
        buttonLabels = [].concat(control.buttonLabels),

        Tick = function() { /* "this" will be a timer */
          var i,
              nominal = this.value,
              slider = this.Slider,
              labels = this.outputLabels,
              output = this.Output,
              step;
          if (typeof slider !== "undefined" && nominal != slider.value)
            slider.value = nominal;
          if (typeof output !== "undefined") {
            step = Math.round((nominal - output.sliderMin)/output.sliderStep);
            if (labels !== null) {
              output.innerHTML = labels[step];
            } else {
              step = step*output.sliderStep + output.sliderMin;
              output.innerHTML = step.toPrecision(output.outputPrecision);
            }
          }
          for (i=0; i < this.actions.length; i++) {
            this.actions[i].value = nominal;
          }
          self.applyControls(el, this.actions, false);
          self.drawScene();
        },

        OnSliderInput = function() { /* "this" will be the slider */
          this.rgltimer.value = Number(this.value);
          this.rgltimer.Tick();
        },

        addSlider = function(min, max, step, value) {
          var slider = document.createElement("input");
          slider.type = "range";
          slider.min = min;
          slider.max = max;
          slider.step = step;
          slider.value = value;
          slider.oninput = OnSliderInput;
          slider.sliderActions = control.actions;
          slider.sliderScene = this;
          slider.className = "rgl-slider";
          slider.id = el.id + "-slider";
          el.rgltimer.Slider = slider;
          slider.rgltimer = el.rgltimer;
          el.appendChild(slider);
        },

        addLabel = function(labels, min, step, precision) {
          var output = document.createElement("output");
          output.sliderMin = min;
          output.sliderStep = step;
          output.outputPrecision = precision;
          output.className = "rgl-label";
          output.id = el.id + "-label";
          el.rgltimer.Output = output;
          el.rgltimer.outputLabels = labels;
          el.appendChild(output);
        },

        addButton = function(which, label, active) {
          var button = document.createElement("input"),
              onclicks = {Reverse: function() { this.rgltimer.reverse();},
                    Play: function() { this.rgltimer.play();
                                       this.value = this.rgltimer.enabled ? this.inactiveValue : this.activeValue; },
                   Slower: function() { this.rgltimer.slower(); },
                   Faster: function() { this.rgltimer.faster(); },
                   Reset: function() { this.rgltimer.reset(); },
              	   Step:  function() { this.rgltimer.step(); }
              };
          button.rgltimer = el.rgltimer;
          button.type = "button";
          button.value = label;
          button.activeValue = label;
          button.inactiveValue = active;
          if (which === "Play")
            button.rgltimer.PlayButton = button;
          button.onclick = onclicks[which];
          button.className = "rgl-button";
          button.id = el.id + "-" + which;
          el.appendChild(button);
        };

        if (typeof control.reinit !== "undefined" && control.reinit !== null) {
          control.actions.reinit = control.reinit;
        }
        el.rgltimer = new rgltimerClass(Tick, control.start, control.interval, control.stop,
                                        control.step, control.value, control.rate, control.loop, control.actions);
        for (var i=0; i < components.length; i++) {
          switch(components[i]) {
            case "Slider": addSlider(control.start, control.stop,
                                   control.step, control.value);
              break;
            case "Label": addLabel(control.labels, control.start,
                                   control.step, control.precision);
              break;
            default:
              addButton(components[i], buttonLabels[i], control.pause);
          }
        }
        el.rgltimer.Tick();
    };

    /**
     * Apply all registered controls
     * @param { Object } el - DOM element of the control
     * @param { Object } x - List of actions to apply
     * @param { boolean } [draw=true] - Whether to redraw after applying
     */
    rglwidgetClass.prototype.applyControls = function(el, x, draw) {
      var self = this, reinit = x.reinit, i, control, type;
      for (i = 0; i < x.length; i++) {
        control = x[i];
        type = control.type;
        self[type](el, control);
      }
      if (typeof reinit !== "undefined" && reinit !== null) {
        reinit = [].concat(reinit);
        for (i = 0; i < reinit.length; i++)
          self.getObj(reinit[i]).initialized = false;
      }
      if (typeof draw === "undefined" || draw)
        self.drawScene();
    };

    /**
     * Handler for scene change
     * @param { Object } message - What sort of scene change to do?
     */
    rglwidgetClass.prototype.sceneChangeHandler = function(message) {
      var self = document.getElementById(message.elementId).rglinstance,
          objs = message.objects, mat = message.material,
          root = message.rootSubscene,
          initSubs = message.initSubscenes,
          redraw = message.redrawScene,
          skipRedraw = message.skipRedraw,
          deletes, subs, allsubs = [], i,j;
      if (typeof message.delete !== "undefined") {
        deletes = [].concat(message.delete);
        if (typeof message.delfromSubscenes !== "undefined")
          subs = [].concat(message.delfromSubscenes);
        else
          subs = [];
        for (i = 0; i < deletes.length; i++) {
          for (j = 0; j < subs.length; j++) {
            self.delFromSubscene(deletes[i], subs[j]);
          }
          delete self.scene.objects[deletes[i]];
        }
      }
      if (typeof objs !== "undefined") {
        Object.keys(objs).forEach(function(key){
          key = parseInt(key, 10);
          self.scene.objects[key] = objs[key];
          self.initObj(key);
          var obj = self.getObj(key),
              subs = [].concat(obj.inSubscenes), k;
          allsubs = allsubs.concat(subs);
          for (k = 0; k < subs.length; k++)
            self.addToSubscene(key, subs[k]);
        });
      }
      if (typeof mat !== "undefined") {
        self.scene.material = mat;
      }
      if (typeof root !== "undefined") {
        self.scene.rootSubscene = root;
      }
      if (typeof initSubs !== "undefined")
        allsubs = allsubs.concat(initSubs);
      allsubs = self.unique(allsubs);
      for (i = 0; i < allsubs.length; i++) {
        self.initSubscene(allsubs[i]);
      }
      if (typeof skipRedraw !== "undefined") {
        root = self.getObj(self.scene.rootSubscene);
        root.par3d.skipRedraw = skipRedraw;
      }
      if (redraw)
        self.drawScene();
    };
    
    /**
     * Set mouse mode for a subscene
     * @param { string } mode - name of mode
     * @param { number } button - button number (1 to 3)
     * @param { number } subscene - subscene id number
     * @param { number } stayActive - if truthy, don't clear brush
     */
    rglwidgetClass.prototype.setMouseMode = function(mode, button, subscene, stayActive) {
      var sub = this.getObj(subscene),
          which = ["left", "right", "middle"][button - 1];
      if (!stayActive && sub.par3d.mouseMode[which] === "selecting")
        this.clearBrush(null);
      sub.par3d.mouseMode[which] = mode;
    };

/**
 * The class of an rgl timer object
 * @class
*/

/**
 * Construct an rgltimerClass object
 * @constructor
 * @param { function } Tick - action when timer fires
 * @param { number } startTime - nominal start time in seconds
 * @param { number } interval - seconds between updates
 * @param { number } stopTime - nominal stop time in seconds
 * @param { number } stepSize - nominal step size
 * @param { number } value - current nominal time
 * @param { number } rate - nominal units per second
 * @param { string } loop - "none", "cycle" or "oscillate"
 * @param { Object } actions - list of actions
 */
rgltimerClass = function(Tick, startTime, interval, stopTime, stepSize, value, rate, loop, actions) {
  this.enabled = false;
  this.timerId = 0;
  /** nominal start time in seconds */
  this.startTime = startTime;   
  /** current nominal time */      
  this.value = value;
  /** seconds between updates */                 
  this.interval = interval;
  /** nominal stop time */           
  this.stopTime = stopTime;
  /** nominal step size */           
  this.stepSize = stepSize;
  /** nominal units per second */           
  this.rate = rate;
  /** "none", "cycle", or "oscillate" */                   
  this.loop = loop;
  /** real world start time */                   
  this.realStart = undefined;
  /** multiplier for fast-forward or reverse */         
  this.multiplier = 1;                
  this.actions = actions;
  this.Tick = Tick;
};

  /**
   * Start playing timer object
   */
  rgltimerClass.prototype.play = function() {
    if (this.enabled) {
      this.enabled = false;
      window.clearInterval(this.timerId);
      this.timerId = 0;
      return;
    }
    var tick = function(self) {
      var now = new Date();
      self.value = self.multiplier*self.rate*(now - self.realStart)/1000 + self.startTime;
      self.forceToRange();
      if (typeof self.Tick !== "undefined") {
        self.Tick(self.value);
      }

    };
    this.realStart = new Date() - 1000*(this.value - this.startTime)/this.rate/this.multiplier;
    this.timerId = window.setInterval(tick, 1000*this.interval, this);
    this.enabled = true;
  };

  /**
   * Force value into legal range
   */
  rgltimerClass.prototype.forceToRange = function() {
    if (this.value > this.stopTime + this.stepSize/2 || this.value < this.startTime - this.stepSize/2) {
      if (!this.loop) {
        this.reset();
      } else {
        var cycle = this.stopTime - this.startTime + this.stepSize,
            newval = (this.value - this.startTime) % cycle + this.startTime;
        if (newval < this.startTime) {
          newval += cycle;
        }
        this.realStart += (this.value - newval)*1000/this.multiplier/this.rate;
        this.value = newval;
      }
    }
  };

  /**
   * Reset to start values
   */
  rgltimerClass.prototype.reset = function() {
    this.value = this.startTime;
    this.newmultiplier(1);
    if (typeof this.Tick !== "undefined") {
        this.Tick(this.value);
    }
    if (this.enabled)
      this.play();  /* really pause... */
    if (typeof this.PlayButton !== "undefined")
      this.PlayButton.value = "Play";
  };

  /**
   * Increase the multiplier to play faster
   */
  rgltimerClass.prototype.faster = function() {
    this.newmultiplier(Math.SQRT2*this.multiplier);
  };

  /**
   * Decrease the multiplier to play slower
   */
  rgltimerClass.prototype.slower = function() {
    this.newmultiplier(this.multiplier/Math.SQRT2);
  };

  /**
   * Change sign of multiplier to reverse direction
   */
  rgltimerClass.prototype.reverse = function() {
    this.newmultiplier(-this.multiplier);
  };

  /**
   * Set multiplier for play speed
   * @param { number } newmult - new value
   */
  rgltimerClass.prototype.newmultiplier = function(newmult) {
    if (newmult != this.multiplier) {
      this.realStart += 1000*(this.value - this.startTime)/this.rate*(1/this.multiplier - 1/newmult);
      this.multiplier = newmult;
    }
  };

  /**
   * Take one step
   */
  rgltimerClass.prototype.step = function() {
    this.value += this.rate*this.multiplier;
    this.forceToRange();
    if (typeof this.Tick !== "undefined")
      this.Tick(this.value);
  };</script>

<div id="div" class="rglWebGL"></div>
<script type="text/javascript">
	var div = document.getElementById("div"),
      rgl = new rglwidgetClass();
  div.width = 600;
  div.height = 600;
  rgl.initialize(div,
                         {"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":false,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false,"polygon_offset":[0,0]},"rootSubscene":6,"objects":{"12":{"id":12,"type":"spheres","material":{},"vertices":[[68.36,22.72,34.48],[80.625,13.65,35.95],[67.16666,26.5,35.76667],[71.15,18.25,34],[59.76667,30.66667,39.85],[60,27.25,35],[80.63333,12.36667,34.33333],[47.26,39.58,39.72],[59.16667,27.53333,37.7],[74.1,17.1,33.63334],[71.95,21.85,41.05],[46.56667,40.46667,45.76667],[62.96667,27.63333,35.6],[72.575,21.525,31.85],[51.7,38.15,37],[59.7,26.3,40.1],[58,34.15,41.23333],[73.3,16.53333,30.9],[50.85,32.225,31.675],[75.8,18.175,33.55],[66.65,24.2,38.65],[56.85,34.3,39.1],[80.06667,13.16667,28.2],[57.025,36,32.475],[51.68,38.64,39.24],[48.925,39.625,38.925],[62.4,29.95,41.95],[47.2,41.05,43.15],[75.9,16.1,33.9],[79.5,12.9,40.1],[50.7,37,37.5],[60.63334,30.8,40.93333],[84.74,9.96,32.5],[52.35,31.25,33.8],[56.45,31.35,32.5],[71.95,14.75,33.7],[77.55,8.95,30.1],[88.65,4.9,39],[67.65,20.6,36.85],[48.35,39.05,43.75],[67.6,18.2,31.7],[69.95,14.7,32.35],[54.85,28.25,41.1],[66.2,19.6,38.1],[55.2,35.1,41],[56.5,32.6,46.3],[59.8,30.3,38.8],[50.35,39.2,40.6],[60.9,25.5,37.9],[60.7,27.5,39.9],[70.2,18,43.2],[45,37.4,37.9],[65.3,24.7,38.6],[61.9,27.6,38.4],[61.8,31.6,39.5],[51.7,39.4,40.1],[52.3,37.6,41.5],[61.95,25.25,29.475],[70.7,24.76667,32.4],[53.675,38.85,40.66667],[70,18.8,36.03333],[73.375,18.35,31.825],[67.96,23.2,35.9],[59.11666,29.15,36.45],[65.3,29,34.16667],[55.23333,35.86666,38.76667],[72.7,19.1,36.15],[71.38,19.42,36.4],[61.4,26.66667,37.8],[49.75,39.675,37.05],[1.8,52.35,33.2],[76.1,14.5,32.1],[61.83333,23.33333,36.2],[52.85,34.75,42.9],[80.75,14.45,34.775],[58.7,33.7,40.3],[71.72,19.82,34.3],[73.1,20.6,35.85],[79.8,10.6,34.55],[56.2,34.05,33.85],[79.62,12.94,32.225],[72.25,20.425,36.7],[44.33333,43.3,38.7],[55.35,32.8,36.45],[74.4,16.83333,38.63334],[75.4,17.9,37.6],[65.1,23.9,43.9],[49.5,33.7,40],[82.3,12.65,40.53333],[70.56667,18.73333,39.85],[64.875,26.7,33.8],[53.55,38.9,38.7],[81.25,11.3,36.05],[68.25,23.25,33.8],[42.75,41.75,42.8],[72,20.4,41.66667],[48.5,38.85,37.5],[62.75,24.6,36.96667],[61,28.6,40.75],[52.2,34.4,40.6],[66.1,25.96667,31.15],[70.93333,14.66667,36.2],[67.06667,21.8,31.325],[67.7,23.6,32.8],[70.63333,15.56667,32.8],[74.1,15.8,35.5],[49.95,38.25,38.75],[54.46667,35.8,35.8],[73.25,17.45,35.4],[80.15,11,33.65],[57.7,28.1,36.2],[68.3,21.3,42.45],[78.5,14.7,30.6],[75.4,14.675,35.45],[55.75,33.85,40.25],[56.15,30.35,38.65],[70,18,34.2],[75.8,19.6,41.9],[70,18.5,34.83333],[69.65,22.25,36.65],[80.675,13.525,29.875],[65.7,25.9,33.15],[61,27.175,35.56667],[59.56667,29.8,38.83333],[88.1,6.4,31.4],[67.3,25.175,35.95],[53.1,36.5,32.4],[54.3,35.4,32],[67.9,21.75,35.1],[54.4,30.8,35.45],[63,26.6,35.2],[64.3,26.6,38.35],[59.36666,31.1,36.1],[66.85,22.7,33.15],[56.65,32.75,36.75],[70.74,19.8,34.175],[76.7,16.65,37.275],[71.73333,20.76667,33.475],[66.93333,24.33333,38.66667],[63.35,26.4,35.8],[66.9,24.4,39.4],[68.9,20.53333,33.5],[70.36667,21.43333,34.9],[76.1,17.7,36.45],[69,24.33333,36.2],[71.575,21.9,33.725],[60.1,29.4,35],[66,22.6,35],[54.6,31.9,36.95],[46.65,40.05,39.5],[40.1,48.1,38.5],[60.2,29.5,30.1],[75.45,19.05,36.3],[54,37.13334,41.25],[69.3,18.83333,40.2],[75.75,16.45,29.9],[56.1,30.15,35.95],[68.4,21.1,40.76667],[68.1,23.2,41.9],[65.23333,26.96667,37],[63.1,28.8,36.15],[53.52,36.44,36.68],[66.3,23,41.75],[64.7,22.06667,37.53333],[62.275,27.1,38.3],[44.4,44.15,38.65],[48.2,38.15,36.26],[72.4,19.575,34.025],[61.76667,26.03333,37.5],[66.63333,26.16667,39.43333],[67.85,23,42.1],[78.58572,14.92857,36.77143],[80.8,13.55,40.8],[62.6,25.3,39.7],[82.7,13,33.95],[68.5,25.45,38.55],[55.8,34.3,40],[56.1,31.1,31.6],[57.1,30.5,38.7],[73.1,14.7,35.3],[63.5,28.4,41.3],[87.85,5.45,36.25],[85.5,8.9,29.6],[71.2,20.7,45.5],[61.5,30.45,42.53333],[60.9,30.6,44.5],[95.1,4.1,32.7],[87.13333,5.633333,29.52],[51.4,40,36],[91.01667,6.683333,29.01667],[70.5,18.2,33.1],[86.2,8.225,30.875],[87.66666,5.966667,31.1],[79.33334,10.26667,29.23333],[92.91666,5.15,28.92],[94.33334,3.233333,26.5],[86.8,6.25,31.8],[89.15,7.15,31],[88.5,7.7,27.1],[83.1,12.7,29.6],[93.72,2.88,26.42],[86.8,9.2,29.5],[64.5,15.2,35.5],[18.2,44.3,35.4],[95.7,2.1,26.7],[95.8,2.3,29.6],[95.5,2,26.9],[92.18,3.92,23.9],[88.57143,5.385714,26.575],[96.6,1.35,27.65],[90.725,4.075,30.65],[93.76667,2.2,25.575],[68.18,22.58,23.075],[91.2,3.7,27.5],[89.15,6.683333,29.08333],[92.2,4.7,34.8],[93.55,4.7,30.15],[87.1,8.6,24.1],[93,4.1,28.03333],[93.04,3.62,27.18333],[88.6,8.8,28.2],[94.54285,2.657143,24.67143],[69.45,16.75,38.65],[93.91111,2.877778,28.07778],[91.625,4.85,32.84],[88.4,5.333333,28.16667],[86.9,10,27.9],[94.78,1.58,22.31667],[91.6,4.575,27.6],[87.68,5.68,25.93333],[95.56667,1.466667,24.78],[89.425,6.125,30.7],[88.12,7.88,31.3],[94.88571,3.657143,24.86],[94.53333,3.166667,28.73333],[80.3,12.2,29.55],[86.375,5,28.925],[82.675,13.325,25.26667],[91.73333,6.5,30.96667],[81.75,14.625,31.16667],[95.225,1.95,29.2625],[90.55556,6.655556,26.46],[95.25,1.95,32.15],[94.33334,3.966667,22.76],[83.45,11.025,33.65],[88.85,5.5,26.675],[92.69167,2.825,23.50769],[64.7,26.7,29.4],[81.65,12.45,29.5],[88.85,4.1,31.1],[88.3,7.266667,31.3],[91.2,4.5,28.1],[84.26,11.98,26.92],[86.9,4.9,28.4],[90.7,3.833333,29],[90.1,7.1,21.5],[96.1,1.4,34.85],[90.3,4.7,31.9],[90.13333,5.833333,30.975],[95.3,2.25,28.8],[89.3,4.2,31.4],[88.4,9.6,26.55],[95.6,3.9,28.35],[92.96667,3.733333,28.9],[96.6,1.8,29.9],[90.7,6.52,27],[96.36667,1.3,28.05],[85.5,10.48,25.6],[93.15,1.5,23.95],[87.4,9.1,31.1],[92.1,3.766667,27.43333],[90.975,4.5,26.825],[79.8,9.65,32.25],[92.925,4.2,25.35],[84.2,13,35.2],[92.6,4.8,34.1],[92,3.2,25.6],[92.22222,5.433333,25.52222],[67.9,20.6,36.4],[87.3,7.15,31.25],[90.7,2.7,35.8],[64.9,18.5,32.4],[92.2,5.3,31.8],[87.9,10.7,29.3],[93.7,2.05,33.6],[89.9,6.65,31.15],[93.83334,3.333333,23.91429],[95.46667,2.066667,32.3],[95.825,1.525,31.1],[91.6,6.1,27.9],[94.48,2.2,23.21],[97.1,1.95,28.13333],[89.575,7.375,31.8],[93,3,23.8],[78.85,15.9,31.4],[93.1,2.7,26.9],[81,11.1,34.2],[55.1,25,23.2],[81.4,7.5,30.1],[77.3,18.9,30.3],[94.5,3.1,30.1],[85.7625,10.9125,23.65],[90.6,4.6,27],[93.85,3.65,18.55],[93,3.6,28.5],[98.8,0.15,23.2],[86.8,7.1,33],[93.65,2.85,24.4],[96,2.7,31.5],[87.12,4.74,32.64],[87.4,10.4,34.2],[89.95,5.15,24.2],[80.9,10.3,24.8],[92.75,3.4,30.95],[93.5,3.3,37.3],[95.8,1.9,31.9],[88.1,6.6,27.6],[91.7,5.35,27.4],[85.775,7.3,37.9],[93.2,3.95,29.26667],[96.1,2.1,34],[75.2,9.7,39.5],[89.26,4.78,31.25],[87.45715,8.1,25.25714],[92.8,4.033333,24.45],[93.3,2.65,30.85],[92.775,3.425,24],[70.7,15.6,38.6],[96.1,2,26.2],[94.2,3.56,22.18333],[94,3.8,29.1],[91,6.2,29.1],[94.9,1.3,31.7],[95.1,2.2,30.1],[82,11.5,33.1],[89.46667,4.8,26.66667],[93.05,2.4,22.7],[87.6,9.566667,25.6],[94,3.3,28.2],[90.4,5.9,32.06667],[96.375,1.8,26.2],[94.8,3.7,28.9],[94.375,3.725,25.6],[91.9,4.4,35.8],[91.6,5.225,22.7],[94.1,3.4,30],[89.24,7.3,30.1],[93.9,5.5,31.3],[91.25,4.375,22.93333],[79.5,15,30.7],[93.56,1.65,21.12222],[89,6.3,23.9],[95.15,2.3,27.26667],[87.7,8.3,26.93333]],"colors":[[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1]],"radii":[[1]],"centers":[[68.36,22.72,34.48],[80.625,13.65,35.95],[67.16666,26.5,35.76667],[71.15,18.25,34],[59.76667,30.66667,39.85],[60,27.25,35],[80.63333,12.36667,34.33333],[47.26,39.58,39.72],[59.16667,27.53333,37.7],[74.1,17.1,33.63334],[71.95,21.85,41.05],[46.56667,40.46667,45.76667],[62.96667,27.63333,35.6],[72.575,21.525,31.85],[51.7,38.15,37],[59.7,26.3,40.1],[58,34.15,41.23333],[73.3,16.53333,30.9],[50.85,32.225,31.675],[75.8,18.175,33.55],[66.65,24.2,38.65],[56.85,34.3,39.1],[80.06667,13.16667,28.2],[57.025,36,32.475],[51.68,38.64,39.24],[48.925,39.625,38.925],[62.4,29.95,41.95],[47.2,41.05,43.15],[75.9,16.1,33.9],[79.5,12.9,40.1],[50.7,37,37.5],[60.63334,30.8,40.93333],[84.74,9.96,32.5],[52.35,31.25,33.8],[56.45,31.35,32.5],[71.95,14.75,33.7],[77.55,8.95,30.1],[88.65,4.9,39],[67.65,20.6,36.85],[48.35,39.05,43.75],[67.6,18.2,31.7],[69.95,14.7,32.35],[54.85,28.25,41.1],[66.2,19.6,38.1],[55.2,35.1,41],[56.5,32.6,46.3],[59.8,30.3,38.8],[50.35,39.2,40.6],[60.9,25.5,37.9],[60.7,27.5,39.9],[70.2,18,43.2],[45,37.4,37.9],[65.3,24.7,38.6],[61.9,27.6,38.4],[61.8,31.6,39.5],[51.7,39.4,40.1],[52.3,37.6,41.5],[61.95,25.25,29.475],[70.7,24.76667,32.4],[53.675,38.85,40.66667],[70,18.8,36.03333],[73.375,18.35,31.825],[67.96,23.2,35.9],[59.11666,29.15,36.45],[65.3,29,34.16667],[55.23333,35.86666,38.76667],[72.7,19.1,36.15],[71.38,19.42,36.4],[61.4,26.66667,37.8],[49.75,39.675,37.05],[1.8,52.35,33.2],[76.1,14.5,32.1],[61.83333,23.33333,36.2],[52.85,34.75,42.9],[80.75,14.45,34.775],[58.7,33.7,40.3],[71.72,19.82,34.3],[73.1,20.6,35.85],[79.8,10.6,34.55],[56.2,34.05,33.85],[79.62,12.94,32.225],[72.25,20.425,36.7],[44.33333,43.3,38.7],[55.35,32.8,36.45],[74.4,16.83333,38.63334],[75.4,17.9,37.6],[65.1,23.9,43.9],[49.5,33.7,40],[82.3,12.65,40.53333],[70.56667,18.73333,39.85],[64.875,26.7,33.8],[53.55,38.9,38.7],[81.25,11.3,36.05],[68.25,23.25,33.8],[42.75,41.75,42.8],[72,20.4,41.66667],[48.5,38.85,37.5],[62.75,24.6,36.96667],[61,28.6,40.75],[52.2,34.4,40.6],[66.1,25.96667,31.15],[70.93333,14.66667,36.2],[67.06667,21.8,31.325],[67.7,23.6,32.8],[70.63333,15.56667,32.8],[74.1,15.8,35.5],[49.95,38.25,38.75],[54.46667,35.8,35.8],[73.25,17.45,35.4],[80.15,11,33.65],[57.7,28.1,36.2],[68.3,21.3,42.45],[78.5,14.7,30.6],[75.4,14.675,35.45],[55.75,33.85,40.25],[56.15,30.35,38.65],[70,18,34.2],[75.8,19.6,41.9],[70,18.5,34.83333],[69.65,22.25,36.65],[80.675,13.525,29.875],[65.7,25.9,33.15],[61,27.175,35.56667],[59.56667,29.8,38.83333],[88.1,6.4,31.4],[67.3,25.175,35.95],[53.1,36.5,32.4],[54.3,35.4,32],[67.9,21.75,35.1],[54.4,30.8,35.45],[63,26.6,35.2],[64.3,26.6,38.35],[59.36666,31.1,36.1],[66.85,22.7,33.15],[56.65,32.75,36.75],[70.74,19.8,34.175],[76.7,16.65,37.275],[71.73333,20.76667,33.475],[66.93333,24.33333,38.66667],[63.35,26.4,35.8],[66.9,24.4,39.4],[68.9,20.53333,33.5],[70.36667,21.43333,34.9],[76.1,17.7,36.45],[69,24.33333,36.2],[71.575,21.9,33.725],[60.1,29.4,35],[66,22.6,35],[54.6,31.9,36.95],[46.65,40.05,39.5],[40.1,48.1,38.5],[60.2,29.5,30.1],[75.45,19.05,36.3],[54,37.13334,41.25],[69.3,18.83333,40.2],[75.75,16.45,29.9],[56.1,30.15,35.95],[68.4,21.1,40.76667],[68.1,23.2,41.9],[65.23333,26.96667,37],[63.1,28.8,36.15],[53.52,36.44,36.68],[66.3,23,41.75],[64.7,22.06667,37.53333],[62.275,27.1,38.3],[44.4,44.15,38.65],[48.2,38.15,36.26],[72.4,19.575,34.025],[61.76667,26.03333,37.5],[66.63333,26.16667,39.43333],[67.85,23,42.1],[78.58572,14.92857,36.77143],[80.8,13.55,40.8],[62.6,25.3,39.7],[82.7,13,33.95],[68.5,25.45,38.55],[55.8,34.3,40],[56.1,31.1,31.6],[57.1,30.5,38.7],[73.1,14.7,35.3],[63.5,28.4,41.3],[87.85,5.45,36.25],[85.5,8.9,29.6],[71.2,20.7,45.5],[61.5,30.45,42.53333],[60.9,30.6,44.5],[95.1,4.1,32.7],[87.13333,5.633333,29.52],[51.4,40,36],[91.01667,6.683333,29.01667],[70.5,18.2,33.1],[86.2,8.225,30.875],[87.66666,5.966667,31.1],[79.33334,10.26667,29.23333],[92.91666,5.15,28.92],[94.33334,3.233333,26.5],[86.8,6.25,31.8],[89.15,7.15,31],[88.5,7.7,27.1],[83.1,12.7,29.6],[93.72,2.88,26.42],[86.8,9.2,29.5],[64.5,15.2,35.5],[18.2,44.3,35.4],[95.7,2.1,26.7],[95.8,2.3,29.6],[95.5,2,26.9],[92.18,3.92,23.9],[88.57143,5.385714,26.575],[96.6,1.35,27.65],[90.725,4.075,30.65],[93.76667,2.2,25.575],[68.18,22.58,23.075],[91.2,3.7,27.5],[89.15,6.683333,29.08333],[92.2,4.7,34.8],[93.55,4.7,30.15],[87.1,8.6,24.1],[93,4.1,28.03333],[93.04,3.62,27.18333],[88.6,8.8,28.2],[94.54285,2.657143,24.67143],[69.45,16.75,38.65],[93.91111,2.877778,28.07778],[91.625,4.85,32.84],[88.4,5.333333,28.16667],[86.9,10,27.9],[94.78,1.58,22.31667],[91.6,4.575,27.6],[87.68,5.68,25.93333],[95.56667,1.466667,24.78],[89.425,6.125,30.7],[88.12,7.88,31.3],[94.88571,3.657143,24.86],[94.53333,3.166667,28.73333],[80.3,12.2,29.55],[86.375,5,28.925],[82.675,13.325,25.26667],[91.73333,6.5,30.96667],[81.75,14.625,31.16667],[95.225,1.95,29.2625],[90.55556,6.655556,26.46],[95.25,1.95,32.15],[94.33334,3.966667,22.76],[83.45,11.025,33.65],[88.85,5.5,26.675],[92.69167,2.825,23.50769],[64.7,26.7,29.4],[81.65,12.45,29.5],[88.85,4.1,31.1],[88.3,7.266667,31.3],[91.2,4.5,28.1],[84.26,11.98,26.92],[86.9,4.9,28.4],[90.7,3.833333,29],[90.1,7.1,21.5],[96.1,1.4,34.85],[90.3,4.7,31.9],[90.13333,5.833333,30.975],[95.3,2.25,28.8],[89.3,4.2,31.4],[88.4,9.6,26.55],[95.6,3.9,28.35],[92.96667,3.733333,28.9],[96.6,1.8,29.9],[90.7,6.52,27],[96.36667,1.3,28.05],[85.5,10.48,25.6],[93.15,1.5,23.95],[87.4,9.1,31.1],[92.1,3.766667,27.43333],[90.975,4.5,26.825],[79.8,9.65,32.25],[92.925,4.2,25.35],[84.2,13,35.2],[92.6,4.8,34.1],[92,3.2,25.6],[92.22222,5.433333,25.52222],[67.9,20.6,36.4],[87.3,7.15,31.25],[90.7,2.7,35.8],[64.9,18.5,32.4],[92.2,5.3,31.8],[87.9,10.7,29.3],[93.7,2.05,33.6],[89.9,6.65,31.15],[93.83334,3.333333,23.91429],[95.46667,2.066667,32.3],[95.825,1.525,31.1],[91.6,6.1,27.9],[94.48,2.2,23.21],[97.1,1.95,28.13333],[89.575,7.375,31.8],[93,3,23.8],[78.85,15.9,31.4],[93.1,2.7,26.9],[81,11.1,34.2],[55.1,25,23.2],[81.4,7.5,30.1],[77.3,18.9,30.3],[94.5,3.1,30.1],[85.7625,10.9125,23.65],[90.6,4.6,27],[93.85,3.65,18.55],[93,3.6,28.5],[98.8,0.15,23.2],[86.8,7.1,33],[93.65,2.85,24.4],[96,2.7,31.5],[87.12,4.74,32.64],[87.4,10.4,34.2],[89.95,5.15,24.2],[80.9,10.3,24.8],[92.75,3.4,30.95],[93.5,3.3,37.3],[95.8,1.9,31.9],[88.1,6.6,27.6],[91.7,5.35,27.4],[85.775,7.3,37.9],[93.2,3.95,29.26667],[96.1,2.1,34],[75.2,9.7,39.5],[89.26,4.78,31.25],[87.45715,8.1,25.25714],[92.8,4.033333,24.45],[93.3,2.65,30.85],[92.775,3.425,24],[70.7,15.6,38.6],[96.1,2,26.2],[94.2,3.56,22.18333],[94,3.8,29.1],[91,6.2,29.1],[94.9,1.3,31.7],[95.1,2.2,30.1],[82,11.5,33.1],[89.46667,4.8,26.66667],[93.05,2.4,22.7],[87.6,9.566667,25.6],[94,3.3,28.2],[90.4,5.9,32.06667],[96.375,1.8,26.2],[94.8,3.7,28.9],[94.375,3.725,25.6],[91.9,4.4,35.8],[91.6,5.225,22.7],[94.1,3.4,30],[89.24,7.3,30.1],[93.9,5.5,31.3],[91.25,4.375,22.93333],[79.5,15,30.7],[93.56,1.65,21.12222],[89,6.3,23.9],[95.15,2.3,27.26667],[87.7,8.3,26.93333]],"ignoreExtent":false,"flags":3},"14":{"id":14,"type":"text","material":{"lit":false},"vertices":[[50.3,-9.763639,13.27982]],"colors":[[0,0,0,1]],"texts":[["Neutrophils(%)"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[50.3,-9.763639,13.27982]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"15":{"id":15,"type":"text","material":{"lit":false},"vertices":[[-16.6219,26.25,13.27982]],"colors":[[0,0,0,1]],"texts":[["Lymphocyte(%)"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-16.6219,26.25,13.27982]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"16":{"id":16,"type":"text","material":{"lit":false},"vertices":[[-16.6219,-9.763639,32.425]],"colors":[[0,0,0,1]],"texts":[["Albumin"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-16.6219,-9.763639,32.425]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"18":{"id":18,"type":"quads","material":{"lit":false,"back":"lines","uri":"data:image/png;base64,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"},"vertices":[[-1,-1,1],[1,-1,1],[1,1,1],[-1,1,1]],"colors":[[1,1,1,1]],"texcoords":[[0,0],[1,0],[1,1],[0,1]],"centers":[[0,0,1]],"ignoreExtent":true,"flags":8198},"10":{"id":10,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"9":{"id":9,"type":"background","material":{"fog":true},"colors":[[0.2980392,0.2980392,0.2980392,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"11":{"id":11,"type":"background","material":{"lit":false,"back":"lines"},"colors":[[1,1,1,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"17":{"id":17,"type":"background","material":{"lit":false,"back":"lines","uriId":18},"colors":[[1,1,1,1]],"ids":[[18]],"types":[["quads"]],"centers":[[0,0,0]],"objects":[{"id":{"id":18},"type":{"type":"quads"},"material":{"color":"#FFFFFF","lit":false,"texture":"/tmp/Rtmpaoa79l/file733a56671782.png","back":"lines"},"vertices":[[-1,-1,1],[1,-1,1],[1,1,1],[-1,1,1]],"colors":[[1,1,1,1]],"texcoords":[[0,0],[1,0],[1,1],[0,1]],"centers":[[0,0,1]],"ignoreExtent":true}],"sphere":false,"fogtype":"none","flags":0},"13":{"id":13,"type":"bboxdeco","material":{"front":"lines","back":"lines"},"vertices":[[20,"NA","NA"],[40,"NA","NA"],[60,"NA","NA"],[80,"NA","NA"],[100,"NA","NA"],["NA",0,"NA"],["NA",10,"NA"],["NA",20,"NA"],["NA",30,"NA"],["NA",40,"NA"],["NA",50,"NA"],["NA","NA",20],["NA","NA",25],["NA","NA",30],["NA","NA",35],["NA","NA",40],["NA","NA",45]],"colors":[[0,0,0,1]],"draw_front":true,"newIds":[26,27,28,29,30,31,32]},"6":{"id":6,"type":"subscene","par3d":{"antialias":8,"FOV":30,"ignoreExtent":false,"listeners":6,"mouseMode":{"left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,286.4459],"modelMatrix":[[0.6761268,0,0,-34.00918],[0,0.4297156,2.220868,-83.29167],[0,-1.180634,0.8083299,-281.6644],[0,0,0,1]],"projMatrix":[[3.732051,0,0,0],[0,3.732051,0,0],[0,0,-3.863703,-1032.604],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[1,0,0,0],[0,0.3420201,0.9396926,0],[0,-0.9396926,0.3420201,0],[0,0,0,1]],"userProjection":[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]],"scale":[0.6761268,1.256404,2.363398],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[0.3209875,100.279,-0.6459222,53.14592,18.12688,46.72312],"windowRect":[454,1203,710,1459],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"/home/pienta/R/x86_64-pc-linux-gnu-library/3.4/rgl/fonts/FreeSans.ttf","maxClipPlanes":8,"glVersion":3,"activeSubscene":0},"embeddings":{"viewport":"replace","projection":"replace","model":"replace","mouse":"replace"},"objects":[17,13,12,14,15,16,10,26,27,28,29,30,31,32],"subscenes":[],"flags":2643},"26":{"id":26,"type":"lines","material":{"lit":false},"vertices":[[20,-1.4528,17.69794],[100,-1.4528,17.69794],[20,-1.4528,17.69794],[20,-2.83794,16.96158],[40,-1.4528,17.69794],[40,-2.83794,16.96158],[60,-1.4528,17.69794],[60,-2.83794,16.96158],[80,-1.4528,17.69794],[80,-2.83794,16.96158],[100,-1.4528,17.69794],[100,-2.83794,16.96158]],"colors":[[0,0,0,1]],"centers":[[60,-1.4528,17.69794],[20,-2.14537,17.32976],[40,-2.14537,17.32976],[60,-2.14537,17.32976],[80,-2.14537,17.32976],[100,-2.14537,17.32976]],"ignoreExtent":true,"origId":13,"flags":64},"27":{"id":27,"type":"text","material":{"lit":false},"vertices":[[20,-5.60822,15.48888],[40,-5.60822,15.48888],[60,-5.60822,15.48888],[80,-5.60822,15.48888],[100,-5.60822,15.48888]],"colors":[[0,0,0,1]],"texts":[["20"],["40"],["60"],["80"],["100"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[20,-5.60822,15.48888],[40,-5.60822,15.48888],[60,-5.60822,15.48888],[80,-5.60822,15.48888],[100,-5.60822,15.48888]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"origId":13,"flags":2064},"28":{"id":28,"type":"lines","material":{"lit":false},"vertices":[[-1.178383,0,17.69794],[-1.178383,50,17.69794],[-1.178383,0,17.69794],[-3.752302,0,16.96158],[-1.178383,10,17.69794],[-3.752302,10,16.96158],[-1.178383,20,17.69794],[-3.752302,20,16.96158],[-1.178383,30,17.69794],[-3.752302,30,16.96158],[-1.178383,40,17.69794],[-3.752302,40,16.96158],[-1.178383,50,17.69794],[-3.752302,50,16.96158]],"colors":[[0,0,0,1]],"centers":[[-1.178383,25,17.69794],[-2.465343,0,17.32976],[-2.465343,10,17.32976],[-2.465343,20,17.32976],[-2.465343,30,17.32976],[-2.465343,40,17.32976],[-2.465343,50,17.32976]],"ignoreExtent":true,"origId":13,"flags":64},"29":{"id":29,"type":"text","material":{"lit":false},"vertices":[[-8.900141,0,15.48888],[-8.900141,10,15.48888],[-8.900141,20,15.48888],[-8.900141,30,15.48888],[-8.900141,40,15.48888],[-8.900141,50,15.48888]],"colors":[[0,0,0,1]],"texts":[["0"],["10"],["20"],["30"],["40"],["50"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-8.900141,0,15.48888],[-8.900141,10,15.48888],[-8.900141,20,15.48888],[-8.900141,30,15.48888],[-8.900141,40,15.48888],[-8.900141,50,15.48888]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"origId":13,"flags":2064},"30":{"id":30,"type":"lines","material":{"lit":false},"vertices":[[-1.178383,-1.4528,20],[-1.178383,-1.4528,45],[-1.178383,-1.4528,20],[-3.752302,-2.83794,20],[-1.178383,-1.4528,25],[-3.752302,-2.83794,25],[-1.178383,-1.4528,30],[-3.752302,-2.83794,30],[-1.178383,-1.4528,35],[-3.752302,-2.83794,35],[-1.178383,-1.4528,40],[-3.752302,-2.83794,40],[-1.178383,-1.4528,45],[-3.752302,-2.83794,45]],"colors":[[0,0,0,1]],"centers":[[-1.178383,-1.4528,32.5],[-2.465343,-2.14537,20],[-2.465343,-2.14537,25],[-2.465343,-2.14537,30],[-2.465343,-2.14537,35],[-2.465343,-2.14537,40],[-2.465343,-2.14537,45]],"ignoreExtent":true,"origId":13,"flags":64},"31":{"id":31,"type":"text","material":{"lit":false},"vertices":[[-8.900141,-5.60822,20],[-8.900141,-5.60822,25],[-8.900141,-5.60822,30],[-8.900141,-5.60822,35],[-8.900141,-5.60822,40],[-8.900141,-5.60822,45]],"colors":[[0,0,0,1]],"texts":[["20"],["25"],["30"],["35"],["40"],["45"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-8.900141,-5.60822,20],[-8.900141,-5.60822,25],[-8.900141,-5.60822,30],[-8.900141,-5.60822,35],[-8.900141,-5.60822,40],[-8.900141,-5.60822,45]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"origId":13,"flags":2064},"32":{"id":32,"type":"lines","material":{"lit":false},"vertices":[[-1.178383,-1.4528,17.69794],[-1.178383,53.9528,17.69794],[-1.178383,-1.4528,47.15206],[-1.178383,53.9528,47.15206],[-1.178383,-1.4528,17.69794],[-1.178383,-1.4528,47.15206],[-1.178383,53.9528,17.69794],[-1.178383,53.9528,47.15206],[-1.178383,-1.4528,17.69794],[101.7784,-1.4528,17.69794],[-1.178383,-1.4528,47.15206],[101.7784,-1.4528,47.15206],[-1.178383,53.9528,17.69794],[101.7784,53.9528,17.69794],[-1.178383,53.9528,47.15206],[101.7784,53.9528,47.15206],[101.7784,-1.4528,17.69794],[101.7784,53.9528,17.69794],[101.7784,-1.4528,47.15206],[101.7784,53.9528,47.15206],[101.7784,-1.4528,17.69794],[101.7784,-1.4528,47.15206],[101.7784,53.9528,17.69794],[101.7784,53.9528,47.15206]],"colors":[[0,0,0,1]],"centers":[[-1.178383,26.25,17.69794],[-1.178383,26.25,47.15206],[-1.178383,-1.4528,32.425],[-1.178383,53.9528,32.425],[50.3,-1.4528,17.69794],[50.3,-1.4528,47.15206],[50.3,53.9528,17.69794],[50.3,53.9528,47.15206],[101.7784,26.25,17.69794],[101.7784,26.25,47.15206],[101.7784,-1.4528,32.425],[101.7784,53.9528,32.425]],"ignoreExtent":true,"origId":13,"flags":64}},"snapshot":"data:image/png;base64,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","width":600,"height":600,"sphereVerts":{"vb":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1950903,-0.3826834,-0.5555702,-0.7071068,-0.8314696,-0.9238795,-0.9807853,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1],[0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1950903,-0.3826834,-0.5555702,-0.7071068,-0.8314696,-0.9238795,-0.9807853,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0]],"it":[[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270],[17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288],[18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271]],"material":[],"normals":null,"texcoords":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1]],"meshColor":"vertices"},"context":{"shiny":false,"rmarkdown":"html_document"},"crosstalk":{"key":[],"group":[],"id":[],"options":[]}});
  rgl.prefix = "";
</script>

	<p id="debug">
	You must enable Javascript to view this page properly.</p>
    </div>
    
	<br>Drag mouse to rotate model. Use mouse wheel or middle button
	to zoom it.
	<hr>
	<br>
	Object written from rgl 0.100.54 by writeWebGL.

	</body>
	</html><!--/html_preserve-->

## Most common biomarkers


```r
biomarker_popularity <- df %>% group_by(PATIENT_ID, outcome) %>% summarise_at(vars(`Hypersensitive cardiac troponinI`:creatinine), function(x) sum(!is.na(x)))
biomarker_popularity$sum_of_single_test <- rowSums(biomarker_popularity %>% ungroup() %>% select(-PATIENT_ID, -outcome))

most_tested_death <- biomarker_popularity %>% select(sum_of_single_test, outcome, `neutrophils(%)`,  `(%)lymphocyte`, albumin) %>% filter(outcome=='Death') %>% arrange(desc(sum_of_single_test)) %>% head(10)
most_tested_survival <- biomarker_popularity %>% select(sum_of_single_test, outcome, `neutrophils(%)`,  `(%)lymphocyte`, albumin) %>% filter(outcome=='Survival') %>% arrange(desc(sum_of_single_test)) %>% head(10)

foo <- rbind(most_tested_death, most_tested_survival)

patients_tests <- merge(foo %>% ungroup() %>% select(PATIENT_ID), df %>% ungroup() %>% select(PATIENT_ID, RE_DATE,`neutrophils(%)`,  `(%)lymphocyte`, albumin, outcome),  by="PATIENT_ID") %>% filter(!is.na(`neutrophils(%)`) |  !is.na(`(%)lymphocyte`) | !is.na(albumin))

#neutrophils_seq <- patients_tests %>% filter(!is.na(`neutrophils(%)`)) 

#neutrophils_seq %>% mutate(PATIENT_ID=as.factor(PATIENT_ID)) %>%
#  ggplot( aes(x=RE_DATE, y=`neutrophils(%)`, group=PATIENT_ID, color=PATIENT_ID)) +
#    geom_line() +
#    geom_point() +
#  facet_grid(rows=vars(outcome)) +
#    ggtitle("Neutrophils (%) during patient hospitalization") +
#    theme_ipsum() +
#    ylab("Neutrophils (%)") +
#    transition_reveal(RE_DATE)

#anim_save("neutrophils_seq.gif")

#lymphocyte_seq <- patients_tests %>% filter(!is.na(`(%)lymphocyte`)) 

#lymphocyte_seq %>% mutate(PATIENT_ID=as.factor(PATIENT_ID)) %>%
#  ggplot( aes(x=RE_DATE, y=`(%)lymphocyte`, group=PATIENT_ID, color=PATIENT_ID)) +
#    geom_line() +
#    geom_point() +
#  facet_grid(rows=vars(outcome)) +
#    ggtitle("Lymphocyte (%) during patient hospitalization") +
#    theme_ipsum() +
#    ylab("Lymphocyte (%)") +
#    transition_reveal(RE_DATE)

#anim_save("lymphocyte_seq.gif")

#albumin_seq <- patients_tests %>% filter(!is.na(albumin)) 

#albumin_seq %>% mutate(PATIENT_ID=as.factor(PATIENT_ID)) %>%
#  ggplot( aes(x=RE_DATE, y=albumin, group=PATIENT_ID, color=PATIENT_ID)) +
#    geom_line() +
#    geom_point() +
#  facet_grid(rows=vars(outcome)) +
#    ggtitle("Albumin value during patient hospitalization") +
#    theme_ipsum() +
#    ylab("Albumin") +
#    transition_reveal(RE_DATE)

#anim_save("albumin_seq.gif")
```

## Prediction model


```r
last_test_df <- df %>% group_by(PATIENT_ID, outcome, gender, age) %>% fill_(names(df)) %>% fill_(names(df), "up") %>% summarise_at(vars(`Hypersensitive cardiac troponinI`:creatinine), function(x) last(x,order_by = is.na(x)))

last_test_df %>% select(hemoglobin, `eosinophils(%)`, `Alkaline phosphatase`, albumin, `basophil(%)`, `Total bilirubin`, `Platelet count`, `monocytes(%)`, `neutrophils(%)`, `total protein`, `mean corpuscular volume`, hematocrit, `White blood cell count`, `mean corpuscular hemoglobin concentration`, Urea, `lymphocyte count`, `Red blood cell count`, `Eosinophil count`, `neutrophils count`, `Direct bilirubin`, `(%)lymphocyte`, `Total cholesterol`, `aspartate aminotransferase`, `Uric acid`, `HCO3-`, `Lactate dehydrogenase`, `monocytes count`, globulin, `γ-glutamyl transpeptidase`, `basophil count(#)`, `mean corpuscular hemoglobin`, `glutamic-pyruvic transaminase`, eGFR, creatinine)
```

<div class="kable-table">

| PATIENT_ID|outcome  |gender | hemoglobin| eosinophils(%)| Alkaline phosphatase| albumin| basophil(%)| Total bilirubin| Platelet count| monocytes(%)| neutrophils(%)| total protein| mean corpuscular volume| hematocrit| White blood cell count| mean corpuscular hemoglobin concentration|  Urea| lymphocyte count| Red blood cell count| Eosinophil count| neutrophils count| Direct bilirubin| (%)lymphocyte| Total cholesterol| aspartate aminotransferase| Uric acid| HCO3-| Lactate dehydrogenase| monocytes count| globulin| γ-glutamyl transpeptidase| basophil count(#)| mean corpuscular hemoglobin| glutamic-pyruvic transaminase|  eGFR| creatinine|
|----------:|:--------|:------|----------:|--------------:|--------------------:|-------:|-----------:|---------------:|--------------:|------------:|--------------:|-------------:|-----------------------:|----------:|----------------------:|-----------------------------------------:|-----:|----------------:|--------------------:|----------------:|-----------------:|----------------:|-------------:|-----------------:|--------------------------:|---------:|-----:|---------------------:|---------------:|--------:|-------------------------:|-----------------:|---------------------------:|-----------------------------:|-----:|----------:|
|          1|Survival |Male   |      131.0|            1.7|                   71|    37.6|         0.2|             6.3|            141|          7.9|           64.3|          67.7|                    93.8|       38.0|                   9.67|                                       345|  6.50|             2.50|                 4.05|             0.16|              6.23|              2.6|          25.9|              4.84|                         23|     376.0|  28.0|                   206|            0.76|     30.1|                        41|              0.02|                        32.3|                            30|  74.7|         88|
|          2|Survival |Male   |      149.0|            0.1|                   45|    37.2|         0.3|            13.3|            283|          5.7|           84.7|          70.4|                    94.2|       43.6|                  10.37|                                       342|  3.50|             0.95|                 4.63|             0.01|              8.79|              5.4|           9.2|              5.06|                         15|     261.0|  25.3|                   282|            0.59|     33.2|                        50|              0.03|                        32.2|                            22|  94.6|         74|
|          3|Survival |Female |      126.0|            0.1|                   69|    38.4|         0.1|            12.6|            238|          6.1|           66.4|          68.2|                    94.4|       35.7|                   7.68|                                       353|  4.22|             2.10|                 3.78|             0.01|              5.09|              5.1|          27.3|              4.47|                         20|     221.1|  25.3|                   226|            0.47|     29.8|                        53|              0.01|                        33.3|                            67|  84.6|         64|
|          4|Survival |Male   |      103.0|            2.5|                   79|    34.1|         0.3|             7.7|            300|          8.6|           72.1|          66.0|                   113.3|       29.8|                   6.78|                                       346|  3.60|             1.12|                 2.63|             0.17|              4.89|              4.4|          16.5|              3.13|                         28|     274.0|  27.4|                   249|            0.58|     31.9|                        14|              0.02|                        39.2|                            26|  74.2|         88|
|          5|Survival |Female |      130.0|            3.0|                   84|    39.5|         0.5|             6.2|            356|          7.4|           65.5|          70.3|                    87.3|       37.9|                   7.95|                                       343|  3.00|             1.88|                 4.34|             0.24|              5.20|              2.3|          23.6|              3.43|                         13|     292.0|  28.1|                   179|            0.59|     30.8|                        21|              0.04|                        30.0|                            18| 122.8|         54|
|          6|Survival |Female |       91.0|            2.4|                   56|    30.6|         0.3|             6.1|            239|          8.2|           58.1|          70.4|                    89.4|       27.7|                   2.94|                                       329|  5.20|             0.91|                 3.10|             0.07|              1.71|              2.5|          31.0|              4.13|                         20|     158.0|  26.2|                   231|            0.24|     39.8|                        15|              0.01|                        29.4|                            14|  85.8|         53|
|          7|Survival |Male   |      128.0|            0.0|                   37|    28.6|         0.1|            11.1|            238|          8.3|           78.3|          64.3|                    92.1|       35.0|                   9.52|                                       366|  5.10|             1.27|                 3.80|             0.00|              7.45|              3.9|          13.3|              4.79|                         22|      97.0|  27.5|                   313|            0.79|     35.7|                        41|              0.01|                        33.7|                            37| 118.7|         61|
|          8|Survival |Male   |      125.0|            3.2|                   58|    39.1|         0.9|             4.1|            167|          9.3|           49.4|          65.1|                    88.6|       34.3|                  19.40|                                       364|  2.80|             2.00|                 9.90|             0.17|              2.65|              1.9|          37.2|              4.49|                         17|     315.0|  27.6|                   181|            0.50|     26.0|                        32|              0.05|                        32.3|                            15| 109.4|         66|
|          9|Survival |Female |      124.0|            2.0|                   66|    39.5|         0.3|            11.5|            209|          9.0|           66.8|          69.9|                    91.7|       35.2|                 231.40|                                       352|  3.20|             1.31|                22.30|             0.12|              4.00|              3.8|          21.9|              3.98|                         19|     315.0|  24.1|                   170|            0.54|     30.4|                        31|              0.02|                        32.3|                            27| 116.8|         52|
|         10|Survival |Male   |      147.0|            1.0|                   57|    33.3|         0.1|             5.8|            233|          8.1|           66.3|          60.1|                    86.7|       43.0|                   8.77|                                       342|  3.80|             2.15|                 4.96|             0.09|              5.81|              2.5|          24.5|              5.39|                         22|     303.0|  27.8|                   220|            0.71|     26.8|                        74|              0.01|                        29.6|                            41| 120.4|         70|
|         11|Survival |Male   |      149.0|            0.2|                   47|    39.0|         0.0|             9.6|            144|          3.6|           85.4|          77.0|                    90.7|       42.8|                   4.72|                                       348|  3.80|             0.51|                 4.72|             0.01|              4.03|              4.3|          10.8|              2.64|                         24|     183.0|  20.1|                   273|            0.17|     38.0|                        18|              0.00|                        31.6|                            12| 120.9|         67|
|         12|Survival |Female |      129.0|            3.1|                   57|    47.6|         0.4|            29.1|            212|          7.4|           46.2|          77.2|                    85.5|       38.9|                   4.87|                                       332|  4.00|             2.09|                 4.55|             0.15|              2.25|              8.9|          42.9|              4.26|                         19|     244.0|  26.2|                   190|            0.36|     29.6|                        12|              0.02|                        28.4|                             9| 120.8|         58|
|         13|Survival |Female |      108.0|            1.0|                   91|    33.3|         0.3|             4.0|            288|          8.7|           70.3|          76.8|                   100.0|       30.6|                 229.40|                                       353|  4.40|             1.35|                10.30|             0.07|              4.83|              1.7|          19.7|              4.06|                         42|     221.0|  24.1|                   189|            0.60|     43.5|                        44|              0.02|                        35.3|                            55|  81.7|         69|
|         14|Survival |Female |      128.0|            0.7|                   52|    34.5|         0.1|             8.9|            263|          7.3|           68.8|          63.5|                    79.3|       38.7|                   8.79|                                       331|  6.30|             2.03|                 4.88|             0.06|              6.05|              3.7|          23.1|              7.30|                         14|     169.0|  26.2|                   209|            0.64|     29.0|                        33|              0.01|                        26.2|                            14| 101.3|         50|
|         15|Survival |Female |      117.0|            3.4|                   56|    34.5|         0.3|             5.5|            306|          7.6|           45.0|          55.8|                    91.6|       36.2|                   6.09|                                       323|  5.40|             2.66|                 3.95|             0.21|              2.74|              3.5|          43.7|              2.82|                         18|     273.0|  23.9|                   188|            0.46|     21.3|                        18|              0.02|                        29.6|                            23|  89.3|         62|
|         16|Survival |Male   |      141.0|            2.0|                   77|    40.1|         0.5|             5.7|            395|         13.1|           53.3|          71.3|                    86.0|       43.1|                   7.42|                                       327|  5.50|             2.31|                 5.01|             0.15|              3.95|              3.0|          31.1|              4.37|                         20|     203.0|  20.5|                   282|            0.97|     31.2|                       134|              0.04|                        28.1|                            47| 118.7|         57|
|         17|Survival |Female |      109.0|            1.1|                   63|    39.1|         0.3|            18.9|            371|          6.4|           62.8|          65.7|                    94.9|       33.3|                   6.42|                                       327|  3.90|             1.89|                 3.51|             0.07|              4.03|              5.3|          29.4|              5.26|                         12|     407.0|  25.4|                   152|            0.41|     26.6|                        37|              0.02|                        31.1|                            23| 126.1|         52|
|         18|Survival |Male   |       94.0|            0.9|                   56|    30.0|         0.0|             4.8|            363|         11.9|           65.4|          59.3|                    83.8|       29.9|                   5.31|                                       314|  4.80|             1.16|                 3.57|             0.05|              3.47|              2.2|          21.8|              3.76|                         34|     217.0|  28.7|                   250|            0.63|     29.3|                        18|              0.00|                        26.3|                            35|  84.3|         83|
|         19|Survival |Male   |       99.0|            5.0|                   68|    28.6|         0.8|            15.8|            112|         13.2|           40.1|          64.0|                    98.6|       28.1|                   2.42|                                       352|  5.78|             0.99|                 2.85|             0.12|              0.97|              7.0|          40.9|              3.66|                         24|     194.4|  23.6|                   180|            0.32|     35.4|                        75|              0.02|                        34.7|                            16|  83.6|         71|
|         20|Survival |Female |      103.0|            2.7|                  131|    30.6|         0.3|             5.6|            211|          4.3|           69.0|          59.7|                    90.2|       32.1|                   6.34|                                       321|  3.20|             1.50|                 3.56|             0.17|              4.38|              1.6|          23.7|              4.34|                         25|     160.0|  28.2|                   274|            0.27|     29.1|                        56|              0.02|                        28.9|                            47| 100.0|         58|
|         21|Survival |Male   |      135.0|            1.0|                   46|    34.8|         0.0|             4.1|            330|          8.4|           62.5|          70.0|                    89.8|       40.4|                   7.29|                                       334|  4.60|             2.05|                 4.50|             0.07|              4.56|              1.6|          28.1|              4.21|                         25|     267.0|  23.5|                   217|            0.61|     35.2|                        24|              0.00|                        30.0|                            39| 104.9|         81|
|         22|Survival |Female |      117.0|            2.9|                   50|    37.5|         0.3|             4.0|            308|          7.3|           55.3|          65.6|                    84.6|       36.2|                   8.00|                                       323|  3.40|             1.98|                 5.00|             0.17|              3.20|              2.1|          34.2|              3.86|                         22|     262.0|  25.2|                   158|            0.42|     28.1|                        18|              0.02|                        27.3|                            27| 126.1|         52|
|         23|Survival |Female |       82.0|            0.8|                   98|    26.9|         0.1|             5.9|            365|         10.1|           72.0|          52.2|                    85.0|       26.0|                   7.49|                                       315|  3.00|             1.27|                 3.06|             0.06|              5.39|              4.7|          17.0|              6.09|                         41|     203.0|  23.0|                   268|            0.76|     25.3|                        46|              0.01|                        26.8|                            47| 124.2|         44|
|         24|Survival |Male   |      119.0|            1.4|                   42|    32.7|         0.4|            10.4|            186|          7.5|           46.2|          64.5|                    96.9|       31.6|                   5.17|                                       377|  3.20|             2.30|                 3.26|             0.07|              2.39|              4.0|          44.5|              4.93|                         12|     153.0|  25.4|                   156|            0.39|     31.8|                        17|              0.02|                        36.5|                            17| 102.7|         65|
|         25|Survival |Female |      120.0|            2.0|                   79|    44.1|         0.3|             5.1|            263|          5.8|           72.1|          74.5|                    87.8|       35.1|                   8.85|                                       342|  2.50|             1.75|                 4.00|             0.18|              6.38|              2.2|          19.8|              4.42|                         24|     297.0|  27.1|                   204|            0.51|     30.4|                        59|              0.03|                        30.0|                            67| 117.0|         60|
|         26|Survival |Male   |      139.0|            1.9|                   73|    38.2|         0.5|             8.4|            171|         11.2|           46.7|          66.8|                    88.8|       39.5|                   4.21|                                       352|  3.40|             1.67|                 4.45|             0.08|              1.97|              3.3|          39.7|              4.32|                         41|     415.0|  28.9|                   212|            0.47|     28.6|                        28|              0.02|                        31.2|                            63|  86.3|         98|
|         27|Survival |Female |      116.0|            0.6|                   61|    40.3|         0.2|             3.9|            295|          6.1|           61.3|          69.9|                    94.3|       34.6|                   4.72|                                       335|  3.10|             1.50|                 3.67|             0.03|              2.89|              1.6|          31.8|              4.09|                         20|     246.0|  29.0|                   251|            0.29|     29.6|                        14|              0.01|                        31.6|                            15| 121.1|         54|
|         28|Survival |Male   |      135.0|            0.7|                   54|    43.1|         0.2|             3.8|            235|          8.3|           48.2|          72.0|                    91.2|       40.2|                   4.46|                                       336|  2.60|             1.90|                 4.41|             0.03|              2.15|              1.9|          42.6|              3.74|                         18|     344.6|  24.3|                   150|            0.37|     28.9|                        15|              0.01|                        30.6|                            22| 109.2|         84|
|         29|Survival |Male   |      140.0|            0.5|                   17|    33.9|         0.6|            12.5|            391|          7.1|           68.4|          66.1|                    92.1|       39.9|                   6.58|                                       351|  5.70|             1.54|                 4.33|             0.03|              4.50|              5.7|          23.4|              3.84|                         22|     290.0|  25.1|                   292|            0.47|     32.2|                        22|              0.04|                        32.3|                            15|  83.7|         83|
|         30|Survival |Male   |      147.0|            0.6|                   79|    41.5|         0.5|            16.7|            352|          7.3|           79.9|          68.1|                    85.6|       42.9|                   9.56|                                       343|  3.30|             1.12|                 5.01|             0.06|              7.63|              6.6|          11.7|              4.17|                         23|     419.0|  27.4|                   176|            0.70|     26.6|                        90|              0.05|                        29.3|                            64| 101.1|         85|
|         31|Survival |Female |      115.0|            2.5|                   42|    36.7|         0.4|             5.3|            173|          8.5|           52.4|          64.1|                    87.9|       33.4|                   4.47|                                       344|  3.20|             1.62|                 3.80|             0.11|              2.34|              2.0|          36.2|              3.27|                         26|     223.0|  26.9|                   153|            0.38|     27.4|                        16|              0.02|                        30.3|                            23| 112.4|         61|
|         32|Survival |Male   |      124.0|            2.9|                   45|    40.3|         0.5|             7.3|            222|         10.1|           51.5|          71.2|                    91.8|       36.9|                   1.00|                                       336|  4.00|             1.46|                 2.90|             0.12|              2.15|              2.5|          35.0|              4.81|                         25|     213.0|  25.5|                   214|            0.42|     30.9|                        23|              0.02|                        30.8|                            36| 108.7|         81|
|         33|Survival |Female |      151.0|            0.9|                   80|    35.5|         0.1|            20.4|            286|          6.2|           84.1|          70.5|                    91.1|       44.8|                  14.92|                                       337|  5.40|             1.30|                 4.92|             0.13|             12.55|              6.6|           8.7|              4.56|                         13|     391.0|  30.8|                   187|            0.92|     35.0|                        29|              0.02|                        30.7|                            15|  66.7|         82|
|         34|Survival |Female |      109.0|            4.7|                   39|    33.8|         0.5|             5.8|            310|         10.7|           56.2|          66.0|                    91.9|       31.8|                   3.84|                                       343|  2.70|             1.07|                 3.46|             0.18|              2.16|              2.6|          27.9|              3.19|                         13|     167.0|  23.7|                   211|            0.41|     32.2|                        12|              0.02|                        31.5|                            10| 108.0|         51|
|         35|Survival |Female |      113.0|            2.4|                   46|    33.7|         0.4|            13.2|            254|         11.5|           53.7|          59.8|                    94.4|       30.6|                   4.60|                                       369|  3.80|             1.47|                 3.24|             0.11|              2.47|              4.4|          32.0|              3.97|                         20|     185.3|  28.8|                   162|            0.53|     26.1|                        13|              0.02|                        34.9|                            29|  90.6|         58|
|         36|Survival |Male   |      139.0|            1.8|                   86|    35.2|         0.3|            13.0|            303|         10.8|           70.3|          67.6|                    91.7|       38.9|                   6.49|                                       357|  3.30|             1.09|                 4.24|             0.12|              4.56|              6.3|          16.8|              3.85|                         29|     188.0|  21.8|                   224|            0.70|     32.4|                        61|              0.02|                        32.8|                            58|  95.0|         65|
|         37|Survival |Male   |      159.0|            1.1|                   44|    30.1|         1.5|            11.0|            225|         13.6|           70.8|          63.2|                    85.3|       45.7|                   8.25|                                       348|  5.90|             1.07|                 5.36|             0.09|              5.85|              4.0|          13.0|              2.95|                         20|     295.0|  18.9|                   362|            1.12|     33.1|                        40|              0.12|                        29.7|                            21|  97.7|         83|
|         38|Survival |Female |      100.0|            2.0|                   73|    39.3|         0.2|             6.5|            161|          4.8|           87.4|          71.1|                    89.4|       29.6|                  10.29|                                       338| 44.70|             0.58|                 3.31|             0.21|              8.99|              2.6|           5.6|              3.27|                          7|     527.0|  14.8|                   235|            0.49|     31.8|                        12|              0.02|                        30.2|                             6|   2.3|       1256|
|         39|Survival |Female |      132.0|            2.6|                   51|    36.0|         0.4|             8.0|            247|         12.8|           59.1|          69.6|                    82.0|       35.9|                   4.91|                                       368|  2.90|             1.23|                 4.38|             0.13|              2.90|              3.5|          25.1|              4.02|                         19|     192.2|  25.2|                   254|            0.63|     33.6|                        77|              0.02|                        30.1|                            24| 114.5|         54|
|         40|Survival |Male   |      163.0|            1.9|                   63|    42.8|         0.6|            23.6|            214|         11.2|           47.6|          65.9|                    84.7|       47.5|                   6.26|                                       343|  4.70|             2.42|                 5.61|             0.12|              2.98|              8.0|          38.7|              2.81|                         17|     307.2|  23.3|                   142|            0.70|     23.1|                        17|              0.04|                        29.1|                            27|  83.4|        105|
|         41|Survival |Male   |         NA|             NA|                   63|    39.2|          NA|             4.1|             NA|           NA|             NA|          77.9|                      NA|         NA|                     NA|                                        NA| 20.50|               NA|                   NA|               NA|                NA|              2.2|            NA|              4.85|                          8|     551.2|  23.2|                   208|              NA|     38.7|                        18|                NA|                          NA|                             6|   5.9|        764|
|         42|Survival |Female |      101.0|            2.0|                   97|    31.4|         0.4|             2.8|            242|         11.2|           60.8|          63.0|                    94.2|       30.8|                   5.08|                                       328|  3.10|             1.30|                 3.27|             0.10|              3.09|              1.8|          25.6|              3.09|                         18|     239.2|  25.1|                   139|            0.57|     31.6|                        53|              0.02|                        30.9|                            20|  93.1|         57|
|         43|Survival |Male   |      114.0|            2.5|                   76|    29.9|         0.4|             7.1|            278|         10.6|           71.5|          64.1|                    90.6|       32.7|                   5.54|                                       349|  4.50|             0.83|                 3.61|             0.14|              3.96|              3.4|          15.0|              4.41|                         26|     268.2|  25.5|                   269|            0.59|     34.2|                        52|              0.02|                        31.6|                            35|  80.8|         84|
|         44|Survival |Male   |      121.0|            3.3|                   66|    40.8|         1.1|             5.2|            323|         14.3|           52.7|          69.0|                    87.2|       35.3|                   5.53|                                       343|  4.70|             1.58|                 4.05|             0.18|              2.92|              2.0|          28.6|              3.98|                         22|     474.7|  22.2|                   192|            0.79|     28.2|                        69|              0.06|                        29.9|                            34|  82.5|         96|
|         45|Survival |Female |      102.0|            2.4|                  620|    38.1|         0.3|             9.0|            112|         11.5|           66.2|          70.6|                    95.6|       30.4|                   3.82|                                       336| 38.70|             0.75|                 3.18|             0.09|              2.53|              3.3|          19.6|              2.77|                          8|     719.5|  16.1|                   187|            0.44|     32.5|                        12|              0.01|                        32.1|                             5|   2.4|       1291|
|         46|Survival |Male   |      138.0|            0.4|                   54|    41.0|         0.4|            18.8|            216|          8.9|           55.2|          63.4|                    89.9|       39.1|                   8.40|                                       353|  5.40|             1.69|                 1.60|             0.02|              2.66|              6.4|          35.1|              2.92|                         22|     234.8|  28.5|                   180|            0.43|     22.4|                        18|              0.02|                        31.7|                            39|  99.6|         70|
|         47|Survival |Male   |      135.0|            1.2|                   70|    46.3|         0.4|            14.1|            233|          9.3|           56.5|          74.3|                    81.6|       39.0|                   6.74|                                       346|  5.20|             2.20|                 4.78|             0.08|              3.80|              5.0|          32.6|              4.50|                         24|     398.3|  23.3|                   207|            0.63|     28.0|                        43|              0.03|                        28.2|                            37| 105.4|         85|
|         48|Survival |Female |      131.0|            1.5|                   72|    38.8|         0.2|             9.4|            269|          8.2|           59.8|          68.1|                    87.9|       36.2|                   5.24|                                       362|  4.90|             1.59|                 4.12|             0.08|              3.13|              3.4|          30.3|              5.52|                         20|     227.9|  27.5|                   245|            0.43|     29.3|                        26|              0.01|                        31.8|                            22|  94.8|         60|
|         49|Survival |Female |      130.0|            2.2|                   64|    39.3|         0.5|             7.2|            262|          8.9|           50.1|          63.0|                    89.5|       37.5|                   5.48|                                       347|  4.40|             2.10|                 4.19|             0.12|              2.74|              2.5|          38.3|              4.83|                         31|     205.7|  27.2|                   225|            0.49|     23.7|                        29|              0.03|                        31.0|                            44| 104.6|         55|
|         50|Survival |Male   |      122.0|            3.7|                   49|    37.9|         0.5|             4.5|            344|          9.4|           60.9|          65.7|                    86.9|       35.7|                   4.36|                                       342|  4.30|             1.11|                 4.11|             0.16|              2.66|              1.9|          25.5|              4.58|                         23|     257.1|  26.9|                   172|            0.41|     27.8|                        11|              0.02|                        29.7|                            17|  93.7|         65|
|         51|Survival |Male   |      128.0|            2.0|                   52|    39.9|         0.2|             6.6|            161|          9.6|           60.7|          65.1|                    83.1|       35.5|                   6.66|                                       361|  6.60|             1.83|                 4.27|             0.13|              4.05|              2.4|          27.5|              3.62|                         19|     440.4|  18.9|                   205|            0.64|     25.2|                        23|              0.01|                        30.0|                            12|  46.7|        145|
|         52|Survival |Male   |      134.0|            2.0|                   61|    43.2|         0.8|            11.2|            236|          9.0|           70.2|          67.6|                    91.8|       38.2|                   3.56|                                       351|  3.50|             0.64|                 4.16|             0.07|              2.50|              3.9|          18.0|              4.32|                         34|     243.4|  25.3|                   204|            0.32|     24.4|                        13|              0.03|                        32.2|                            49|  95.2|         67|
|         53|Survival |Male   |      141.0|            1.9|                   87|    37.9|         0.7|             7.7|            221|         15.0|           45.0|          60.8|                    91.9|       40.9|                   4.12|                                       345|  6.30|             1.54|                 4.45|             0.08|              1.85|              3.2|          37.4|              3.72|                         17|     282.5|  22.7|                   196|            0.62|     22.9|                        14|              0.03|                        31.7|                            12|  96.3|         83|
|         54|Survival |Male   |      141.0|            1.7|                   58|    38.6|         0.2|             7.9|            279|          8.1|           65.3|          73.1|                    90.3|       39.0|                   5.90|                                       362|  7.10|             1.46|                 4.32|             0.10|              3.85|              4.9|          24.7|              2.43|                         45|     322.6|  23.5|                   223|            0.48|     34.5|                        63|              0.01|                        32.6|                            36|  67.2|         95|
|         55|Survival |Female |      127.0|            2.5|                   54|    38.4|         0.5|             7.7|            267|          7.5|           61.9|          62.1|                    90.3|       36.3|                   3.98|                                       350|  3.70|             1.10|                 4.02|             0.10|              2.46|              3.4|          27.6|              3.16|                         20|     202.8|  23.4|                   172|            0.30|     23.7|                        10|              0.02|                        31.6|                            15| 115.7|         46|
|         56|Survival |Female |      116.0|            1.8|                   NA|      NA|         0.8|              NA|            231|          8.8|           47.0|            NA|                    86.3|       33.5|                   5.14|                                       346|    NA|             2.14|                 3.88|             0.09|              2.42|               NA|          41.6|                NA|                         NA|        NA|    NA|                    NA|            0.45|       NA|                        NA|              0.04|                        29.9|                            NA|    NA|         NA|
|         57|Survival |Female |      152.0|            1.4|                   50|    39.5|         0.3|             4.3|            383|          4.9|           61.8|          65.6|                    89.2|       43.7|                   6.30|                                       348|  3.20|             1.99|                 4.90|             0.09|              3.89|              2.0|          31.6|              4.75|                         16|     205.4|  25.8|                   196|            0.31|     26.1|                        11|              0.02|                        31.0|                            10| 105.6|         66|
|         58|Survival |Male   |      134.0|            1.2|                   50|    40.1|         0.6|             7.9|            297|          7.1|           51.7|          58.6|                    85.2|       37.5|                   4.90|                                       357|  3.80|             1.93|                 4.40|             0.06|              2.53|              3.3|          39.4|              3.18|                         20|     283.1|  27.5|                   190|            0.35|     18.5|                        12|              0.03|                        30.5|                            16| 118.9|         76|
|         59|Survival |Male   |      143.0|            2.0|                   66|    41.5|         0.2|             4.5|            134|          7.9|           52.3|          65.8|                    87.0|       40.9|                   5.93|                                       350|  6.10|             2.23|                 4.70|             0.12|              3.10|              1.9|          37.6|              3.00|                         14|     338.7|  21.8|                   172|            0.47|     24.3|                        47|              0.01|                        30.4|                            27|  84.4|         97|
|         60|Survival |Male   |      125.0|            4.3|                   76|    34.1|         0.4|             7.8|            302|         15.5|           35.4|          74.3|                    98.5|       38.4|                   4.66|                                       326|  3.90|             2.07|                 3.90|             0.20|              1.65|              3.6|          44.4|              4.31|                         30|     167.0|  23.8|                   242|            0.72|     40.2|                        17|              0.02|                        32.1|                            21|  70.8|         91|
|         61|Survival |Male   |      148.0|            1.3|                   51|    34.3|         0.6|             9.0|            205|          5.8|           52.1|          71.6|                    89.8|       43.8|                   4.80|                                       338|  4.70|             2.14|                 3.10|             0.07|              2.77|              3.4|          40.2|              6.56|                         34|     278.0|  28.7|                   173|            0.31|     37.3|                        72|              0.03|                        30.3|                            90|  99.8|         82|
|         62|Survival |Female |      129.0|            2.4|                   75|    41.5|         0.2|            13.8|            381|          7.8|           41.1|          69.2|                    84.5|       37.2|                   5.30|                                       347|  3.30|             1.99|                 0.70|             0.10|              1.68|              5.4|          48.5|              3.93|                         16|     375.0|  26.2|                   214|            0.32|     27.7|                        34|              0.01|                        29.3|                            13|  81.1|         69|
|         63|Survival |Male   |      140.0|            2.1|                   47|    35.4|         0.5|             4.7|            306|         12.8|           59.1|          68.8|                    96.6|       37.1|                   3.76|                                       377|  3.30|             0.96|                 3.84|             0.08|              2.22|              2.8|          25.5|              3.32|                         24|     216.0|  25.6|                   209|            0.48|     33.4|                        49|              0.02|                        36.5|                            45|  98.5|         66|
|         64|Survival |Male   |      117.0|            0.7|                  167|    35.9|         0.1|            14.4|            322|          8.6|           53.3|          66.6|                   104.7|       33.5|                   8.93|                                       349|  9.00|             3.33|                 3.20|             0.06|              4.76|              5.1|          37.3|              5.64|                         27|     375.0|  24.2|                   252|            0.77|     30.7|                       386|              0.01|                        36.6|                            57|  72.0|         94|
|         65|Survival |Female |      111.0|            2.2|                   96|    38.2|         1.3|            17.0|            299|          6.9|           70.2|          72.0|                    73.8|       35.4|                  13.20|                                       314|  2.70|             1.74|                 2.50|             0.20|              6.31|              5.5|          19.4|              4.88|                         30|     334.0|  25.8|                   211|            0.62|     33.8|                        40|              0.12|                        23.1|                            30|  95.4|         67|
|         66|Survival |Female |      121.0|            0.9|                   66|    35.4|         0.2|             6.6|            214|          6.0|           79.4|          69.5|                    89.9|       37.3|                   6.54|                                       324|  2.00|             0.88|                 4.15|             0.06|              5.20|              2.5|          13.5|              3.84|                         12|     214.0|  24.6|                   190|            0.39|     34.1|                        16|              0.01|                        29.2|                            12| 118.5|         53|
|         67|Survival |Female |      126.0|            0.0|                   55|    30.9|         0.0|            16.2|            256|          3.2|           78.6|          67.9|                    84.2|       35.1|                   4.73|                                       359|  6.80|             0.86|                 4.17|             0.00|              3.72|              8.6|          18.2|              5.86|                         16|     195.0|  25.7|                   164|            0.15|     37.0|                        57|              0.00|                        30.2|                            30|  97.2|         50|
|         68|Survival |Male   |      139.0|            0.7|                  103|    39.8|         0.5|             7.8|            172|          4.3|           63.6|          69.7|                    93.4|       42.6|                   4.44|                                       326|  5.40|             1.37|                 4.56|             0.03|              2.83|              3.5|          30.9|              6.30|                         28|     303.0|  30.6|                   215|            0.19|     29.9|                       126|              0.02|                        30.5|                           121| 119.4|         69|
|         69|Survival |Male   |      143.0|            0.1|                   94|    36.2|         0.1|             7.2|            210|          7.6|           72.3|          67.1|                    85.7|       40.1|                   6.73|                                       357|  4.50|             1.34|                 4.68|             0.01|              4.86|              3.1|          19.9|              4.49|                         17|     217.0|  24.7|                   169|            0.51|     30.9|                        33|              0.01|                        30.6|                            25| 103.3|         71|
|         70|Survival |Male   |      123.0|            1.7|                   58|    39.3|         1.1|             6.7|            226|          7.0|           61.0|          72.0|                    88.8|       36.3|                   5.28|                                       339|  5.30|             1.54|                 4.09|             0.09|              3.22|              2.8|          29.2|              4.30|                         24|     454.0|  29.7|                   200|            0.37|     32.7|                        68|              0.06|                        30.1|                            37|  81.3|         85|
|         71|Survival |Female |      124.0|            3.9|                   93|    37.3|         0.3|            10.3|            287|          8.8|           55.1|          66.2|                    96.1|       36.9|                   3.85|                                       336|  3.00|             1.23|                 3.84|             0.15|              2.12|              3.1|          31.9|              4.95|                         26|     319.0|  29.4|                   202|            0.34|     28.9|                        39|              0.01|                        32.3|                            24|  92.8|         55|
|         72|Survival |Male   |      123.0|            0.8|                   NA|      NA|         0.2|              NA|            453|          8.4|           66.8|            NA|                    83.7|       37.4|                   6.50|                                       329|    NA|             2.46|                 1.00|             0.08|              6.90|               NA|          23.8|                NA|                         NA|        NA|    NA|                    NA|            0.87|       NA|                        NA|              0.02|                        27.5|                            NA|    NA|         NA|
|         73|Survival |Female |      122.0|            2.3|                  115|    39.3|         0.8|            12.7|            278|          8.1|           44.3|          72.8|                    89.3|       35.1|                   3.93|                                       348|  3.00|             1.75|                 3.93|             0.09|              1.74|              4.3|          44.5|              6.11|                         17|     300.0|  23.1|                   201|            0.32|     33.5|                        44|              0.03|                        31.0|                            10| 103.3|         68|
|         74|Survival |Male   |       98.0|            0.3|                   54|    33.2|         0.1|             6.7|             39|         53.0|            1.9|          66.2|                    97.3|       32.1|                  75.36|                                       305|  5.90|            33.69|                 3.30|             0.19|              1.45|              2.7|          44.7|              4.46|                         10|     412.0|  24.4|                   339|           39.92|     33.0|                        37|              0.11|                        29.7|                            10| 100.4|         76|
|         75|Survival |Female |      110.0|            0.4|                   46|    32.1|         0.4|            13.2|            206|          8.6|           76.1|          64.2|                    91.6|       31.6|                   5.38|                                       348|  3.80|             0.78|                 3.45|             0.02|              4.10|              6.4|          14.5|              4.31|                         13|     289.0|  25.3|                   184|            0.46|     32.1|                        12|              0.02|                        31.9|                            10|  79.3|         58|
|         76|Survival |Male   |      171.0|            0.6|                   61|    37.4|         0.4|            25.4|            403|         13.4|           67.7|          80.1|                    84.3|       48.8|                  10.00|                                       350|  4.40|             1.79|                 5.79|             0.06|              6.77|             12.0|          17.9|              3.61|                         22|     445.0|  25.9|                   213|            1.34|     42.7|                        55|              0.04|                        29.5|                            49|  72.3|         97|
|         77|Survival |Male   |      156.0|            0.0|                   80|    42.9|         0.2|            18.7|            396|         10.4|           60.3|          72.2|                    93.8|       45.2|                   3.50|                                       345|  4.80|             2.81|                 2.90|             0.00|              5.82|              6.5|          29.1|              3.40|                         31|     290.0|  27.3|                   193|            1.00|     29.3|                        22|              0.02|                        32.4|                            32|  90.3|         85|
|         78|Survival |Female |      128.0|            2.0|                   35|    36.9|         0.0|            11.1|            312|          5.9|           59.1|          68.5|                    88.9|       38.4|                   9.10|                                       333|  4.90|             1.46|                19.40|             0.09|              2.61|              4.6|          33.0|              7.16|                         14|     165.0|  33.2|                   161|            0.26|     31.6|                        26|              0.00|                        29.6|                            33| 105.0|         51|
|         79|Survival |Female |      141.0|            1.9|                   75|    40.3|         0.2|             9.4|            324|          5.5|           58.7|          73.1|                    90.6|       42.4|                  18.70|                                       333|  2.80|             1.97|                 5.60|             0.11|              3.44|              2.6|          33.7|              4.28|                         20|     437.0|  24.0|                   323|            0.32|     32.8|                         7|              0.01|                        30.1|                            13| 109.9|         65|
|         80|Survival |Male   |      135.0|            2.5|                   45|    32.2|         0.4|            10.7|            180|          8.2|           53.9|          60.5|                    91.7|       39.6|                   5.23|                                       341|  3.20|             1.83|                 4.32|             0.13|              2.82|              5.1|          35.0|              4.36|                         21|     223.0|  27.6|                   211|            0.43|     28.3|                        41|              0.02|                        31.3|                            21| 110.7|         60|
|         81|Survival |Female |      112.0|            0.2|                   39|    33.4|         0.1|             5.0|            274|          8.2|           64.3|          62.3|                    95.6|       32.6|                   9.16|                                       344|  3.60|             2.49|                 3.41|             0.02|              5.89|              2.2|          27.2|              7.06|                         14|     328.0|  31.3|                   190|            0.75|     28.9|                        35|              0.01|                        32.8|                            32| 111.9|         51|
|         82|Survival |Female |      125.0|            0.7|                   58|    35.6|         0.3|            17.1|            177|         10.0|           75.7|          65.2|                    97.4|       37.3|                   4.10|                                       335|  3.50|             0.99|                 5.10|             0.05|              5.66|              5.8|          13.3|              3.64|                         10|     264.0|  27.3|                   139|            0.75|     29.6|                        28|              0.02|                        32.6|                            22|  70.3|         75|
|         83|Survival |Female |      119.0|            0.3|                   46|    30.6|         0.2|            14.3|            237|         10.1|           41.0|          66.6|                    93.8|       34.8|                  16.30|                                       342|  4.50|             2.83|                 7.20|             0.02|              2.40|              4.9|          48.4|              3.77|                         19|     224.0|  29.0|                   271|            0.59|     36.0|                        39|              0.01|                        32.1|                            21|  97.8|         62|
|         84|Survival |Female |      101.0|            1.6|                   36|    33.0|         0.1|            11.5|            247|          9.4|           75.5|          59.1|                    93.4|       28.3|                   8.65|                                       357|  4.30|             1.16|                 3.03|             0.14|              6.53|              4.2|          13.4|              4.63|                         16|     259.0|  28.0|                   202|            0.81|     26.1|                        25|              0.01|                        33.3|                            23| 110.8|         61|
|         85|Survival |Female |      110.0|            1.7|                   72|    37.5|         0.4|             8.2|            165|          8.5|           64.8|          62.9|                    87.8|       33.2|                   5.50|                                       331|  4.60|             1.16|                 1.80|             0.08|              3.06|              2.7|          24.6|              4.80|                         17|     195.0|  25.8|                   196|            0.40|     25.4|                        24|              0.02|                        29.1|                            35|  98.7|         53|
|         86|Survival |Female |      117.0|            2.4|                   47|    39.3|         0.4|            15.8|            181|         11.4|           38.9|          67.9|                    89.7|       34.9|                   4.58|                                       335|  4.50|             2.15|                 3.89|             0.11|              1.78|              5.8|          46.9|              3.16|                         13|     322.0|  26.9|                   113|            0.52|     28.6|                        16|              0.02|                        30.1|                            21| 120.9|         52|
|         87|Survival |Female |      128.0|            1.4|                   78|    36.2|         0.4|             6.0|            303|         11.9|           48.4|          65.3|                    85.6|       36.9|                   5.70|                                       347|  3.10|             2.16|                 4.31|             0.08|              2.76|              3.0|          37.9|              3.73|                         26|     132.0|  29.0|                   204|            0.68|     29.1|                       158|              0.02|                        29.7|                            85| 119.1|         49|
|         88|Survival |Female |       93.0|            0.8|                   64|    43.9|         0.2|             4.9|            159|          7.2|           76.8|          65.7|                    70.3|       29.4|                  10.92|                                       316|  2.40|             1.64|                 4.18|             0.09|              8.38|              2.1|          15.0|              4.66|                         10|     229.0|  22.2|                   173|            0.79|     21.8|                        31|              0.02|                        22.2|                             9| 135.8|         39|
|         89|Survival |Male   |      129.0|            0.0|                   50|    37.6|         0.2|             4.5|            149|          6.5|           75.4|          77.1|                    65.2|       40.5|                   4.46|                                       319|  5.70|             0.80|                 6.21|             0.00|              3.36|              2.4|          17.9|              2.45|                         23|     354.0|  21.0|                   268|            0.29|     39.5|                        17|              0.01|                        20.8|                            13|  92.2|         95|
|         90|Survival |Male   |      149.0|            1.5|                   84|    43.9|         0.4|             9.5|            257|          9.1|           65.1|          77.0|                    91.3|       41.8|                   4.60|                                       356|  6.20|             1.11|                13.00|             0.07|              3.02|              3.0|          23.9|              4.34|                         20|     260.0|  23.7|                   211|            0.42|     33.1|                        32|              0.02|                        32.5|                            32|  93.8|         73|
|         91|Survival |Male   |      138.0|            8.6|                   98|    39.4|         0.4|             7.8|            199|         10.7|           48.4|          71.6|                    90.4|       40.4|                   5.33|                                       342|  3.20|             1.70|                 4.47|             0.46|              2.58|              3.4|          31.9|              2.63|                         21|     224.0|  27.5|                   166|            0.57|     32.2|                        75|              0.02|                        30.9|                            25| 102.4|         60|
|         92|Survival |Male   |      147.0|            0.1|                   69|    39.3|         0.0|            10.8|            313|          9.0|           78.2|          75.1|                    85.2|       41.9|                   2.00|                                       351| 13.40|             1.72|                 7.40|             0.01|             10.61|              4.2|          12.7|              6.33|                         16|     400.0|  25.3|                   140|            1.22|     35.8|                        64|              0.00|                        29.9|                            41|  52.3|        129|
|         93|Survival |Female |      125.0|            1.8|                   75|    39.3|         0.2|            10.4|            293|         10.5|           71.7|          70.2|                    93.1|       37.8|                   5.13|                                       331|  3.20|             0.81|                 4.06|             0.09|              3.68|              4.5|          15.8|              4.52|                         21|     223.0|  27.3|                   242|            0.54|     30.9|                        40|              0.01|                        30.8|                            29|  88.0|         63|
|         94|Survival |Female |      110.0|            1.3|                   47|    35.5|         0.3|             7.6|            246|          7.1|           66.6|          68.2|                    89.7|       32.1|                   6.88|                                       343|  3.40|             1.70|                 3.58|             0.09|              4.58|              3.7|          24.7|              3.35|                         25|     294.0|  23.3|                   208|            0.49|     32.7|                        47|              0.02|                        30.7|                            49| 132.0|         39|
|         95|Survival |Female |      111.0|            0.7|                   40|    38.8|         0.0|            12.4|            266|          8.7|           48.7|          67.0|                    90.8|       32.6|                   4.03|                                       340|  3.70|             1.69|                 3.59|             0.03|              1.96|              4.3|          41.9|              3.75|                         19|     193.0|  26.9|                   217|            0.35|     28.2|                        16|              0.00|                        30.9|                            17| 116.1|         54|
|         96|Survival |Male   |      142.0|            0.3|                   76|    37.0|         0.1|             7.2|            251|          5.3|           87.0|          68.2|                    93.0|       40.9|                   8.95|                                       347|  4.10|             0.65|                 4.40|             0.03|              7.79|              2.8|           7.3|              4.11|                         27|     166.0|  29.1|                   240|            0.47|     31.2|                        38|              0.01|                        32.3|                            40|  93.5|         70|
|         97|Survival |Male   |      136.0|            1.8|                   46|    38.8|         0.2|            12.1|            161|          8.0|           55.6|          65.1|                    95.7|       39.9|                   3.60|                                       341|  3.90|             1.89|                 6.50|             0.10|              3.05|              4.2|          34.4|              4.71|                         11|     300.0|  30.2|                   139|            0.44|     26.3|                        30|              0.01|                        32.6|                             6| 111.6|         64|
|         98|Survival |Female |      118.0|            7.1|                   86|    40.5|         0.5|             4.0|            259|          9.0|           37.7|          70.6|                    81.2|       37.1|                   6.70|                                       318|  3.90|             1.88|                 6.40|             0.29|              1.55|              1.6|          45.7|              4.24|                         25|     248.0|  27.7|                   230|            0.37|     30.1|                        23|              0.02|                        25.8|                            40|  85.1|         78|
|         99|Survival |Female |      138.0|            1.4|                   78|    38.9|         0.2|             4.9|            287|          6.7|           73.1|          69.3|                    86.8|       40.8|                   6.44|                                       338|  3.40|             1.20|                 4.70|             0.09|              4.71|              2.8|          18.6|              4.35|                         48|     345.0|  24.9|                   227|            0.43|     30.4|                        51|              0.01|                        29.4|                            55| 110.8|         61|
|        100|Survival |Female |      127.0|            0.7|                   72|    39.1|         0.3|             9.8|            407|         10.1|           51.8|          71.4|                    92.7|       35.6|                  21.30|                                       357|  2.80|             2.24|                 1.50|             0.04|              3.12|              3.5|          37.1|              4.86|                         18|     335.0|  26.2|                   238|            0.61|     32.3|                        49|              0.02|                        33.1|                            17| 111.0|         48|
|        101|Survival |Male   |      137.0|            2.0|                   54|    34.6|         0.3|             5.4|            463|         14.3|           56.3|          67.6|                    88.5|       39.3|                   8.62|                                       349|  4.60|             2.34|                 4.44|             0.17|              4.85|              2.4|          27.1|              3.31|                         20|     343.0|  28.7|                   184|            1.23|     33.0|                        52|              0.03|                        30.9|                            52|  78.6|         97|
|        102|Survival |Female |      138.0|            0.7|                   54|    40.4|         0.0|             7.1|            211|          9.7|           61.0|          71.6|                    90.0|       40.5|                 341.80|                                       341|  3.70|             1.56|               234.00|             0.04|              3.33|              2.6|          28.6|              4.27|                         18|     328.0|  28.4|                   180|            0.53|     31.2|                        18|              0.00|                        30.7|                            29| 128.3|         55|
|        103|Survival |Male   |      153.0|            4.9|                   74|    42.9|         0.6|             8.5|            161|         10.5|           50.3|          73.3|                    85.4|       43.8|                   7.00|                                       349|  5.60|             2.25|                 8.00|             0.33|              3.35|              3.6|          33.7|              5.21|                         20|     228.0|  24.8|                   162|            0.70|     30.4|                        26|              0.04|                        29.8|                            22|  88.5|         83|
|        104|Survival |Male   |      129.0|            3.4|                   73|    30.3|         0.0|            10.6|            247|          5.0|           63.2|          62.3|                    93.9|       32.5|                   4.43|                                       397|  3.70|             1.26|                 3.46|             0.15|              2.80|              4.0|          28.4|              3.34|                         25|     181.0|  22.9|                   249|            0.22|     32.0|                        61|              0.00|                        37.3|                            38|  99.6|         70|
|        105|Survival |Male   |      123.0|            3.8|                   87|    37.7|         0.8|            11.3|            253|         14.9|           63.2|          70.1|                    92.5|       36.0|                   4.75|                                       342|  4.00|             0.82|                 3.89|             0.18|              3.00|              4.5|          17.3|              4.57|                         38|     352.0|  27.4|                   266|            0.71|     32.4|                        39|              0.04|                        31.6|                            76|  62.7|        106|
|        106|Survival |Female |      111.0|            1.5|                   64|    35.1|         1.0|             6.1|            257|         11.4|           53.5|          70.8|                    92.3|       33.7|                   4.02|                                       329|  3.10|             1.31|                 3.65|             0.06|              2.15|              2.0|          32.6|              5.25|                         20|     162.0|  25.7|                   223|            0.46|     35.7|                        25|              0.04|                        30.4|                            19|  93.5|         55|
|        107|Survival |Female |      111.0|            0.9|                  120|    32.8|         0.2|             5.8|            188|          7.6|           67.7|          59.0|                    97.0|       32.8|                  10.10|                                       338|  2.90|             1.27|                11.10|             0.05|              3.63|              2.2|          23.6|              4.75|                         29|     280.0|  21.9|                   283|            0.41|     26.2|                        30|              0.01|                        32.8|                            31| 127.7|         47|
|        108|Survival |Male   |      145.0|            1.7|                   71|    33.2|         0.4|             9.8|            251|         13.4|           71.7|          63.3|                    88.8|       42.7|                  10.30|                                       340|  4.40|             0.90|                 5.90|             0.12|              5.02|              3.6|          12.8|              4.52|                         14|     262.0|  25.8|                   195|            0.94|     30.1|                        24|              0.03|                        30.1|                            28| 100.4|         76|
|        109|Survival |Male   |      148.0|            1.3|                  120|    40.9|         0.5|            25.5|            544|         12.1|           65.5|          80.3|                    91.9|       41.1|                   7.87|                                       360|  4.80|             1.62|                 4.47|             0.10|              5.16|              9.7|          20.6|              5.70|                         34|     358.0|  29.2|                   279|            0.95|     39.4|                       230|              0.04|                        33.1|                            79|  91.0|         80|
|        110|Survival |Male   |      151.0|            1.9|                   64|    41.9|         0.5|             7.7|            241|          8.9|           56.2|          83.4|                    92.9|       43.4|                   3.72|                                       348|  4.50|             1.21|                 4.67|             0.07|              2.09|              3.3|          32.5|              5.30|                         39|     290.0|  27.4|                   173|            0.33|     41.5|                        32|              0.02|                        32.3|                            68|  92.3|         85|
|        111|Survival |Female |      116.0|            2.2|                   44|    35.1|         0.2|             7.4|            194|          8.3|           64.3|          61.5|                    91.3|       33.7|                   4.92|                                       344|  5.10|             1.23|                 3.69|             0.11|              3.16|              2.5|          25.0|              3.98|                         43|     265.0|  25.4|                   141|            0.41|     26.4|                        12|              0.01|                        31.4|                            42| 108.6|         57|
|        112|Survival |Male   |      135.0|            0.1|                   55|    36.8|         0.4|             9.1|            269|          8.7|           68.3|          67.2|                    89.3|       40.7|                   7.73|                                       332|  6.30|             1.74|                 4.56|             0.01|              5.28|              3.3|          22.5|              3.93|                         15|     518.0|  25.8|                   200|            0.67|     30.4|                        29|              0.03|                        29.6|                            21|  75.6|        102|
|        113|Survival |Male   |      136.0|            0.0|                   49|    32.4|         0.1|             7.4|            294|          7.7|           85.0|          61.2|                    88.3|       40.9|                  11.26|                                       333|  6.50|             0.81|                 4.63|             0.00|              9.57|              3.0|           7.2|              4.36|                         14|     260.0|  27.9|                   170|            0.87|     28.8|                        23|              0.01|                        29.4|                            29| 102.5|         63|
|        114|Survival |Male   |      161.0|            2.0|                   61|    37.9|         0.8|             7.7|            342|         10.5|           54.0|          72.1|                    89.6|       47.2|                   3.91|                                       341|  7.00|             1.28|                 5.27|             0.08|              2.11|              3.3|          32.7|              4.11|                         45|     415.0|  26.5|                   299|            0.41|     34.2|                        47|              0.03|                        30.6|                            83|  66.7|        108|
|        115|Survival |Female |      125.0|            0.2|                   68|    41.9|         0.2|             5.0|            160|         10.4|           70.3|          66.7|                    89.8|       37.0|                   4.81|                                       338|  5.10|             0.91|                 4.12|             0.01|              3.38|              2.4|          18.9|              4.67|                         73|     139.0|  26.7|                   214|            0.50|     24.8|                        37|              0.01|                        30.3|                            78| 104.6|         55|
|        116|Survival |Female |      121.0|            0.0|                   85|    35.4|         0.2|            10.4|            127|          8.0|           80.1|          63.0|                    85.4|       34.0|                   5.11|                                       356|  5.20|             0.60|                 3.98|             0.00|              4.09|              5.6|          11.7|              3.08|                         16|     204.0|  29.2|                   222|            0.41|     27.6|                        52|              0.01|                        30.4|                            26|  90.6|         58|
|        117|Survival |Male   |      136.0|            0.0|                   50|    35.7|         0.2|             7.4|            367|          6.9|           80.9|          78.0|                    92.8|       40.1|                   0.80|                                       339|  8.00|             1.40|                 0.10|             0.00|              9.45|              3.1|          12.0|              4.08|                         23|     325.0|  28.1|                   179|            0.81|     42.3|                        46|              0.02|                        31.5|                            61|  70.8|        104|
|        118|Survival |Female |      132.0|            1.2|                   49|    43.1|         0.4|             9.7|            255|          8.9|           54.9|          66.6|                    89.9|       39.3|                   5.14|                                       336|  2.50|             1.78|                 4.37|             0.06|              2.82|              3.4|          34.6|              2.90|                         23|     296.0|  23.8|                   183|            0.46|     23.5|                        17|              0.02|                        30.2|                            26| 122.4|         57|
|        119|Survival |Male   |      145.0|            2.4|                   68|    40.7|         0.4|             9.5|            185|         11.0|           54.0|          69.6|                    93.9|       40.3|                   5.10|                                       360|  4.20|             1.64|                 4.29|             0.12|              2.76|              3.5|          32.2|              4.34|                         40|     304.0|  26.7|                   179|            0.56|     28.9|                        74|              0.02|                        33.8|                            67| 103.9|         60|
|        120|Survival |Female |      127.0|            2.1|                   63|    34.7|         0.2|            17.2|            396|         10.5|           60.0|          72.9|                    85.7|       38.2|                   4.27|                                       332|  6.10|             1.16|                 4.46|             0.09|              2.56|              6.8|          27.2|              4.16|                         28|     198.0|  24.8|                   270|            0.45|     38.2|                        27|              0.01|                        28.5|                            66|  89.4|         64|
|        121|Survival |Female |      128.0|            0.0|                   30|    41.9|         0.1|             7.5|            306|          4.5|           75.8|          80.3|                    84.1|       38.5|                   7.38|                                       332|  6.10|             1.45|                 4.58|             0.00|              5.59|              3.4|          19.6|              4.43|                         22|     247.0|  27.6|                   242|            0.33|     38.4|                        20|              0.01|                        27.9|                            15| 105.6|         46|
|        122|Survival |Male   |      121.0|            5.8|                   73|    32.6|         0.2|            15.8|            146|          6.9|           69.6|          78.1|                    90.2|       31.2|                   5.38|                                       388|  8.60|             0.94|                 3.46|             0.31|              3.75|              5.7|          17.5|              3.24|                         20|     700.0|  21.6|                   209|            0.37|     45.5|                        45|              0.01|                        35.0|                            15|  47.1|        135|
|        123|Survival |Female |      126.0|            1.3|                   81|    38.3|         0.4|             7.2|            292|          6.4|           68.2|          65.1|                    94.3|       36.6|                   4.55|                                       344|  4.70|             1.08|                 3.88|             0.06|              3.10|              2.7|          23.7|              4.56|                         18|     286.0|  26.2|                   202|            0.29|     26.8|                        15|              0.02|                        32.5|                            16|  92.8|         60|
|        124|Survival |Male   |      114.0|            0.3|                   71|    30.6|         0.2|             6.5|            134|          6.2|           75.6|          56.1|                    96.3|       34.1|                   9.85|                                       334|  5.80|             1.74|                 3.54|             0.03|              7.45|              2.8|          17.7|              4.48|                         15|     259.0|  29.0|                   177|            0.61|     25.5|                        33|              0.02|                        32.2|                            33|  55.5|        120|
|        125|Survival |Male   |      153.0|            0.7|                   68|    33.8|         0.2|            10.9|             -1|         10.2|           61.4|          65.4|                    83.9|       44.8|                   6.80|                                       342|  4.80|             1.69|                 3.60|             0.04|              3.78|              4.6|          27.5|              3.11|                         21|     177.0|  27.0|                   334|            0.63|     31.6|                        74|              0.01|                        28.7|                            49| 116.0|         74|
|        126|Survival |Male   |      135.0|            1.1|                   42|    36.9|         0.6|             6.5|            104|          8.7|           60.5|          68.6|                    86.2|       39.4|                   3.58|                                       343|  2.70|             1.04|                 4.57|             0.04|              2.17|              2.7|          29.1|              2.94|                         15|     342.0|  28.2|                   201|            0.31|     31.7|                        27|              0.02|                        29.5|                            17| 112.0|         78|
|        127|Survival |Female |      111.0|            1.5|                   42|    39.8|         0.7|            13.1|            250|          8.2|           60.2|          80.9|                    91.5|       31.2|                   5.51|                                       356|  3.90|             1.62|                 3.41|             0.08|              3.32|              3.9|          29.4|              4.95|                         18|     341.0|  25.3|                   156|            0.45|     41.1|                        18|              0.04|                        32.6|                            14| 111.9|         52|
|        128|Survival |Female |      125.0|            0.2|                   53|    31.4|         0.3|            12.1|            237|          5.0|           88.1|          66.7|                    89.6|       37.0|                   3.40|                                       338|  4.20|             0.60|                 6.00|             0.02|              8.21|              5.8|           6.4|              2.96|                         15|      94.0|  26.9|                   322|            0.47|     35.3|                        10|              0.03|                        30.3|                            19| 136.6|         33|
|        129|Survival |Female |      126.0|            0.7|                   51|    35.8|         0.0|            12.1|            208|         10.0|           48.7|          61.9|                   100.9|       35.4|                   4.19|                                       356|  5.20|             1.70|                 3.51|             0.03|              2.04|              3.9|          40.6|              5.32|                         16|     407.0|  26.0|                   168|            0.42|     26.1|                        16|              0.00|                        35.9|                            20|  91.8|         66|
|        130|Survival |Female |      109.0|            2.2|                   53|    32.4|         0.0|             4.1|            256|          8.2|           53.1|          66.4|                    83.6|       33.6|                   3.64|                                       324|  2.60|             1.33|                 4.02|             0.08|              1.93|              1.6|          36.5|              3.82|                         23|     175.0|  22.1|                   248|            0.30|     34.0|                        30|              0.00|                        27.1|                            19| 116.6|         44|
|        131|Survival |Male   |      119.0|            1.1|                   45|    32.0|         0.2|             7.3|            398|          9.0|           54.3|          69.4|                    87.6|       34.7|                   4.35|                                       343|  2.80|             1.54|                 3.96|             0.05|              2.36|              2.9|          35.4|              3.61|                         17|     171.0|  21.8|                   252|            0.39|     37.4|                        22|              0.01|                        30.1|                            25| 116.6|         66|
|        132|Survival |Male   |      142.0|            2.3|                   47|    35.7|         0.5|            13.0|            172|         10.8|           63.6|          62.0|                    87.2|       41.6|                   6.59|                                       341|  3.30|             1.50|                 4.77|             0.15|              4.20|              4.6|          22.8|              4.26|                         22|     325.0|  24.0|                   159|            0.71|     26.3|                        27|              0.03|                        29.8|                            41|  86.0|         86|
|        133|Survival |Male   |      132.0|            5.7|                   86|    36.8|         0.8|            10.4|            125|         10.7|           42.6|          63.1|                    92.6|       38.6|                   3.66|                                       342|  3.50|             1.47|                 4.17|             0.21|              1.56|              4.6|          40.2|              2.68|                         17|     339.0|  25.5|                   186|            0.39|     26.3|                        13|              0.03|                        31.7|                            18|  82.0|         90|
|        134|Survival |Female |      130.0|            1.1|                   47|    38.0|         0.6|             8.3|            229|          7.4|           66.5|          68.9|                    86.8|       39.3|                   7.16|                                       331|  2.90|             1.75|                 4.53|             0.08|              4.76|              3.0|          24.4|              3.76|                         16|     198.0|  24.4|                   179|            0.53|     30.9|                        13|              0.04|                        28.7|                            15| 120.9|         51|
|        135|Survival |Female |      108.0|            0.7|                   47|    38.1|         0.7|             4.3|            418|          7.8|           55.3|          69.5|                    84.8|       34.1|                   9.20|                                       317|  2.50|             1.55|                41.80|             0.03|              2.42|              1.6|          35.5|              5.20|                         21|     214.0|  27.3|                   235|            0.34|     31.4|                        16|              0.03|                        26.9|                            28| 116.6|         49|
|        136|Survival |Female |      140.0|            0.5|                   58|    39.0|         0.5|            20.7|            380|          5.3|           70.7|          71.9|                    89.8|       40.5|                   6.65|                                       346|  2.90|             1.53|                 4.51|             0.03|              4.71|              6.9|          23.0|              4.09|                         22|     240.0|  26.3|                   222|            0.35|     32.9|                        14|              0.03|                        31.0|                            18| 117.0|         60|
|        137|Survival |Male   |      138.0|            1.0|                   75|    33.3|         0.3|            12.8|            163|          9.8|           69.0|          61.0|                    91.5|       39.0|                   4.10|                                       354|  3.10|             1.36|                 9.20|             0.07|              4.71|              5.2|          19.9|              2.81|                         21|     360.0|  25.8|                   231|            0.67|     27.7|                        26|              0.02|                        32.4|                            41|  96.4|         72|
|        138|Survival |Female |      142.0|            2.1|                   48|    38.7|         0.0|             9.4|            217|         10.7|           51.5|          72.2|                    88.6|       41.2|                   3.36|                                       345|  4.30|             1.20|                 4.65|             0.07|              1.73|              3.4|          35.7|              4.30|                         19|     235.0|  26.9|                   177|            0.36|     33.5|                        12|              0.00|                        30.5|                            16| 100.6|         66|
|        139|Survival |Female |      125.0|            1.0|                   59|    35.8|         0.5|             7.1|            231|          7.4|           71.3|          65.0|                    93.4|       36.9|                1726.60|                                       339|  2.60|             1.55|                 3.10|             0.08|              5.57|              2.4|          19.8|              3.69|                         22|     329.0|  22.9|                   207|            0.58|     29.2|                        19|              0.04|                        31.6|                            30| 102.9|         64|
|        140|Survival |Female |      127.0|            1.4|                   45|    38.8|         0.1|            13.2|            225|          8.0|           61.2|          68.3|                    83.6|       36.3|                   7.37|                                       350|  3.30|             2.16|                 4.34|             0.10|              4.51|              5.9|          29.3|              5.03|                         25|     385.0|  30.9|                   172|            0.59|     29.5|                        39|              0.01|                        29.3|                            29|  97.1|         49|
|        141|Survival |Male   |      159.0|            0.3|                   77|    32.8|         0.4|             8.6|            203|          6.9|           63.9|          68.4|                    92.5|       46.6|                   7.34|                                       341|  4.80|             2.09|                 5.04|             0.02|              4.69|              3.9|          28.5|              4.66|                         26|     357.0|  29.7|                   188|            0.51|     35.6|                        69|              0.03|                        31.5|                           121|  90.2|         95|
|        142|Survival |Female |      110.0|            1.2|                   52|    36.6|         0.3|             5.1|            254|          7.4|           67.2|          57.9|                    90.5|       31.6|                   3.39|                                       348|  4.50|             0.81|                 3.49|             0.04|              2.28|              2.3|          23.9|              3.89|                         14|     202.0|  23.9|                   154|            0.25|     21.3|                         8|              0.01|                        31.5|                             7|  99.7|         56|
|        143|Survival |Female |      122.0|            2.8|                   88|    35.8|         0.8|             6.9|            165|          8.0|           63.0|          68.2|                    90.0|       35.2|                   3.89|                                       347|  3.00|             0.99|                 3.91|             0.11|              2.45|              2.7|          25.4|              4.86|                         35|     200.0|  27.1|                   292|            0.31|     32.4|                        34|              0.03|                        31.2|                            34|  96.5|         50|
|        144|Survival |Male   |      119.0|            1.3|                  102|    38.2|         0.6|             5.9|            220|          7.4|           65.7|          66.3|                    97.8|       36.2|                   5.41|                                       329|  5.90|             1.35|                 3.70|             0.07|              3.56|              1.9|          25.0|              3.77|                         27|     380.0|  24.1|                   246|            0.40|     28.1|                        45|              0.03|                        32.2|                            25|  51.9|        121|
|        145|Survival |Female |      116.0|            1.8|                   47|    39.9|         0.3|             7.1|            148|          9.2|           59.2|          74.8|                    93.5|       34.4|                   9.40|                                       337|  4.60|             1.16|                 1.50|             0.07|              2.33|              2.6|          29.5|              7.13|                         11|     313.0|  26.1|                   177|            0.36|     34.9|                        21|              0.01|                        31.5|                             7|  67.7|         83|
|        146|Survival |Female |      133.0|            0.0|                   55|    30.7|         0.1|            13.6|            170|          9.2|           79.0|          62.8|                    89.3|       39.9|                  12.20|                                       333|  9.70|             1.11|                 4.80|             0.00|              7.51|              5.4|          11.7|              6.67|                         21|     252.0|  32.1|                   261|            0.87|     32.1|                        26|              0.01|                        29.8|                            22|  84.7|         65|
|        147|Survival |Male   |      135.0|            0.0|                   51|    39.2|         0.1|            12.6|            213|          8.6|           66.4|          63.4|                    95.3|       38.9|                   8.04|                                       347|  5.60|             2.00|                 4.08|             0.00|              5.34|              4.4|          24.9|              5.13|                         14|     370.0|  28.3|                   163|            0.69|     24.2|                        43|              0.01|                        33.1|                            16| 102.5|         84|
|        148|Survival |Male   |      145.0|            0.0|                   55|    32.2|         0.0|            10.0|            289|          5.0|           83.9|          66.8|                    92.5|       43.0|                   8.55|                                       337|  5.40|             0.95|                 4.65|             0.00|              7.17|              3.8|          11.1|              5.89|                         23|     209.0|  24.6|                   243|            0.43|     34.6|                        45|              0.00|                        31.2|                            50| 106.1|         75|
|        149|Survival |Male   |      135.0|            0.0|                   39|    32.3|         0.1|             8.5|            204|          4.7|           85.2|          71.3|                    92.1|       38.7|                   3.00|                                       349|  6.10|             0.88|                 3.10|             0.00|              7.49|              3.6|          10.0|              4.97|                          9|     162.0|  23.8|                   142|            0.41|     39.0|                        33|              0.01|                        32.1|                            19| 126.1|         46|
|        150|Survival |Female |      121.0|            1.5|                   89|    35.4|         0.6|             8.4|            137|          9.2|           56.2|          61.8|                    87.6|       35.2|                   3.26|                                       344|  2.40|             1.06|                 4.02|             0.05|              1.83|              2.7|          32.5|              3.79|                         14|     267.0|  25.5|                   208|            0.30|     26.4|                        12|              0.02|                        30.1|                             9|  89.3|         62|
|        151|Survival |Female |      129.0|            1.7|                  125|    35.0|         0.3|            12.9|            554|          9.4|           66.0|          65.4|                    83.6|       37.3|                  22.60|                                       346|  3.60|             1.59|                 4.20|             0.12|              4.64|              5.0|          22.6|              6.16|                         11|     218.0|  25.5|                   230|            0.66|     30.4|                        41|              0.02|                        28.9|                            19| 100.2|         54|
|        152|Survival |Female |      131.0|            2.1|                   79|    37.6|         0.8|            23.7|            203|         11.1|           55.2|          69.0|                    89.9|       40.1|                   3.86|                                       327|  3.60|             1.19|                 4.46|             0.08|              2.13|              7.5|          30.8|              4.74|                         19|     307.0|  26.3|                   139|            0.43|     31.4|                        14|              0.03|                        29.4|                            16|  94.0|         59|
|        153|Survival |Male   |      107.0|            3.4|                   63|    39.5|         0.9|             7.4|            141|          9.3|           46.6|          65.9|                    87.9|       31.2|                   3.22|                                       343|  3.20|             1.28|                 3.55|             0.11|              1.50|              2.9|          39.8|              4.37|                         14|     330.0|  23.5|                   159|            0.30|     26.4|                        20|              0.03|                        30.1|                             5|  77.4|         89|
|        154|Survival |Female |      126.0|            0.6|                   62|    38.5|         0.6|            12.5|            142|          9.2|           43.6|          68.8|                    89.3|       35.9|                   3.37|                                       351|  3.60|             1.55|                 4.02|             0.02|              1.47|              4.5|          46.0|              2.91|                         15|     217.0|  23.5|                   179|            0.31|     30.3|                        12|              0.02|                        31.3|                             9| 111.0|         45|
|        155|Survival |Female |      125.0|            0.6|                   78|    30.1|         0.4|            10.4|            370|          9.3|           60.2|          59.2|                    98.7|       37.9|                   5.08|                                       330|  3.20|             1.50|                 3.84|             0.03|              3.06|              8.1|          29.5|              4.69|                         16|     184.0|  25.3|                   181|            0.47|     29.1|                       140|              0.02|                        32.6|                            22| 125.1|         49|
|        156|Survival |Male   |      130.0|            0.0|                   81|    31.1|         0.1|            10.4|            206|          8.7|           64.4|          70.3|                    86.0|       39.2|                  24.40|                                       332|  7.10|             2.33|                 4.10|             0.00|              5.60|              4.0|          26.8|              4.72|                         39|     295.0|  29.3|                   203|            0.76|     39.2|                       209|              0.01|                        28.5|                            76|  97.6|         84|
|        157|Survival |Female |      130.0|            0.5|                   74|    44.6|         0.5|             7.6|            229|          8.4|           66.7|          74.0|                    93.3|       39.1|                   4.39|                                       332|  3.00|             1.05|                 4.19|             0.02|              2.93|              3.5|          23.9|              4.08|                         30|     242.0|  23.5|                   175|            0.37|     29.4|                        48|              0.02|                        31.0|                            34| 206.9|         14|
|        158|Survival |Male   |      116.0|            1.6|                   54|    36.4|         0.4|             8.4|            207|         11.0|           63.9|          65.4|                    92.4|       34.0|                   4.93|                                       341|  5.60|             1.14|                 3.68|             0.08|              3.15|              3.4|          23.1|              4.58|                         21|     382.0|  29.1|                   142|            0.54|     29.0|                        65|              0.02|                        31.5|                            29|  77.1|         93|
|        159|Survival |Male   |      127.0|            0.2|                   46|    37.2|         0.0|            20.0|            122|          9.0|           67.4|          75.0|                    90.5|       36.4|                   1.50|                                       349|  5.70|             1.28|                 1.30|             0.01|              3.69|              9.1|          23.4|              2.86|                         20|     222.0|  28.4|                   172|            0.49|     37.8|                        24|              0.00|                        31.6|                            46|  89.3|         73|
|        160|Survival |Male   |      144.0|            0.0|                   58|    33.3|         0.2|             6.4|            163|         14.4|           52.4|          72.5|                    91.0|       41.4|                   4.57|                                       348|  4.50|             1.51|                 4.55|             0.00|              2.39|              2.6|          33.0|              3.72|                         21|     213.0|  24.5|                   194|            0.66|     39.2|                        38|              0.01|                        31.6|                            21|  88.5|         85|
|        161|Survival |Male   |      138.0|            3.8|                   53|    36.8|         0.8|            16.6|            218|         10.3|           59.9|          80.1|                    95.0|       38.4|                   5.32|                                       359|  5.00|             1.34|                 4.04|             0.20|              3.19|              5.2|          25.2|              4.50|                         21|     284.0|  22.9|                   187|            0.55|     43.3|                        40|              0.04|                        34.2|                            12|  82.5|         90|
|        162|Survival |Male   |      163.0|            1.2|                   66|    41.9|         0.5|             4.5|            222|          7.0|           68.1|          71.4|                    88.2|       47.0|                   5.73|                                       347|  8.36|             1.33|                 5.33|             0.07|              3.90|              2.1|          23.2|              4.33|                         34|     332.8|  23.5|                   179|            0.40|     29.5|                        36|              0.03|                        30.6|                            56|  86.0|         86|
|        163|Survival |Female |      119.0|            0.7|                   39|    34.5|         0.2|             6.4|            297|          7.8|           65.1|          64.9|                    88.5|       36.0|                  37.00|                                       331|  4.30|             2.31|                29.30|             0.06|              5.74|              2.5|          26.2|              5.80|                         37|     138.0|  29.3|                   200|            0.69|     30.4|                        76|              0.02|                        29.2|                            98| 102.9|         64|
|        164|Survival |Female |      127.0|            0.0|                   56|    36.0|         0.3|             7.2|            257|          4.7|           77.8|          80.4|                    82.0|       37.4|                   6.86|                                       340|  3.30|             1.18|                 4.56|             0.00|              5.34|              2.8|          17.2|              3.89|                         47|     215.0|  25.9|                   255|            0.32|     44.4|                        16|              0.02|                        27.9|                            47| 114.1|         57|
|        165|Survival |Female |      116.0|            1.8|                   56|    33.4|         0.5|             8.9|            130|          8.2|           53.5|          57.5|                    92.3|       32.3|                   4.39|                                       359|  3.70|             1.58|                 3.50|             0.08|              2.35|              3.8|          36.0|              1.46|                         23|     216.0|  25.1|                   147|            0.36|     24.1|                        21|              0.02|                        33.1|                            31| 112.0|         59|
|        166|Survival |Male   |      154.0|            3.4|                   77|    44.0|         0.2|             8.6|            210|          7.4|           67.8|          70.8|                    83.7|       44.2|                   6.50|                                       348|  4.70|             1.38|                 5.28|             0.22|              4.41|              3.7|          21.2|              3.26|                         18|     226.0|  29.9|                   194|            0.48|     26.8|                        35|              0.01|                        29.2|                            25|  94.9|         88|
|        167|Survival |Female |      107.0|            1.3|                   45|    36.8|         0.8|             8.8|            190|         10.2|           64.2|          67.9|                    92.5|       32.2|                   7.42|                                       332|  4.60|             1.74|                 3.48|             0.10|              4.76|              3.2|          23.5|              4.25|                         24|     214.0|  21.2|                   143|            0.76|     31.1|                        35|              0.06|                        30.7|                            20|  85.8|         71|
|        168|Survival |Female |      109.0|            1.4|                   49|    36.1|         0.5|             4.7|            255|          6.8|           63.1|          68.4|                    89.8|       32.5|                   5.92|                                       335|  2.20|             1.67|                 3.62|             0.08|              3.74|              1.9|          28.2|              4.44|                         21|     283.0|  24.7|                   184|            0.40|     32.3|                        32|              0.03|                        30.1|                            17|  77.6|         79|
|        169|Survival |Female |      115.0|            1.2|                   53|    35.6|         0.0|             4.4|            233|          5.6|           59.9|          60.7|                    86.6|       35.0|                   3.21|                                       329|  1.70|             1.07|                 4.04|             0.04|              1.92|              2.1|          33.3|              2.75|                         19|     138.0|  21.8|                   184|            0.18|     25.1|                         8|              0.00|                        28.5|                            21| 113.5|         51|
|        170|Survival |Female |      104.0|            7.2|                   87|    38.5|         1.6|            15.7|            201|          7.9|           41.6|          70.5|                    77.9|       30.7|                  14.60|                                       339|  4.10|             1.80|                 1.00|             0.31|              1.80|              4.9|          41.7|              4.27|                         17|     351.0|  23.5|                   162|            0.34|     32.0|                        24|              0.07|                        26.4|                            15|  71.4|         74|
|        171|Survival |Male   |      127.0|            0.1|                   65|    34.9|         0.1|            18.4|            248|          7.8|           67.0|          73.5|                    91.1|       36.9|                   8.80|                                       344|  4.10|             1.88|                33.60|             0.01|              5.03|              6.4|          25.0|              4.25|                         15|     231.0|  29.1|                   168|            0.59|     38.6|                        44|              0.01|                        31.4|                            53|  90.5|         72|
|        172|Survival |Male   |      137.0|            2.5|                   54|    38.5|         0.6|             5.6|            258|          8.5|           63.3|          73.7|                    97.0|       32.8|                   5.17|                                       418|  3.84|             1.30|                 3.38|             0.13|              3.27|              1.6|          25.1|              4.11|                         25|     293.2|  25.4|                   224|            0.44|     35.2|                        27|              0.03|                        40.5|                            23|  72.8|         97|
|        173|Survival |Female |      131.0|            0.9|                   53|    37.0|         0.1|             4.4|            227|          4.0|           73.2|          70.2|                    88.7|       37.6|                   6.92|                                       348|  4.40|             1.51|                 4.24|             0.06|              5.06|              1.7|          21.8|              4.45|                         13|     221.0|  24.9|                   149|            0.28|     33.2|                        28|              0.01|                        30.9|                            13| 109.7|         59|
|        174|Survival |Female |      142.0|            0.0|                   55|    38.8|         0.1|             5.0|            227|          9.9|           68.5|          63.5|                    96.8|       39.4|                   7.16|                                       360|  4.60|             1.54|                 4.07|             0.00|              4.90|              2.0|          21.5|              4.24|                         12|     183.0|  24.6|                   255|            0.71|     24.7|                        28|              0.01|                        34.9|                            17|  76.7|         84|
|        175|Survival |Female |      122.0|            1.4|                   43|    36.6|         0.4|            10.2|            161|          5.7|           67.8|          66.7|                    91.6|       36.0|                   9.86|                                       339|  4.10|             2.44|                 3.93|             0.14|              6.68|              3.8|          24.7|              4.92|                         13|     306.0|  26.9|                   193|            0.56|     30.1|                        24|              0.04|                        31.0|                            19| 120.2|         53|
|        176|Survival |Female |      123.0|            0.2|                   60|    38.1|         0.0|             2.9|            324|          1.8|           90.3|          65.8|                    90.6|       37.8|                   9.45|                                       325|  3.70|             0.73|                 4.17|             0.02|              8.53|              1.6|           7.7|              6.36|                         13|     270.0|  25.6|                   212|            0.17|     27.7|                        28|              0.00|                        29.5|                            18| 133.4|         42|
|        177|Survival |Female |      115.0|            2.7|                   44|    39.6|         0.6|            21.0|            285|         10.1|           57.1|          72.4|                    83.7|       34.3|                   4.84|                                       335|  3.20|             1.43|                 4.10|             0.13|              2.76|              6.0|          29.5|              4.35|                         16|     256.0|  26.1|                   176|            0.49|     32.8|                        12|              0.03|                        28.0|                             8| 123.8|         55|
|        178|Survival |Female |      117.0|            0.0|                   38|    28.7|         0.0|             3.0|            439|          4.3|           89.2|          56.8|                    86.8|       36.1|                   9.51|                                       324|  3.90|             0.62|                 4.16|             0.00|              8.48|              1.6|           6.5|              4.30|                         12|      98.0|  23.2|                   173|            0.41|     28.1|                        15|              0.00|                        28.1|                             9| 120.7|         47|
|        179|Survival |Female |      106.0|            0.0|                   56|    37.5|         0.0|             8.3|            329|          4.5|           81.3|          70.7|                    78.7|       33.2|                  10.11|                                       319|  4.00|             1.44|                 4.22|             0.00|              8.21|              3.3|          14.2|              3.12|                          8|     107.0|  19.5|                   183|            0.46|     33.2|                        11|              0.00|                        25.1|                             5| 124.2|         49|
|        180|Survival |Female |      110.0|            2.9|                   74|    40.0|         0.3|             2.8|            379|          6.7|           55.8|          71.7|                    95.0|       32.3|                   6.45|                                       341|  4.20|             2.21|                 3.40|             0.19|              3.60|              1.7|          34.3|              3.38|                         19|     294.7|  26.8|                   192|            0.43|     31.7|                        19|              0.02|                        32.4|                            31|  88.2|         64|
|        181|Survival |Female |      103.0|            2.3|                   47|    31.6|         0.2|             3.4|            421|         10.3|           56.1|          62.5|                    93.6|       30.5|                   4.76|                                       338|  3.90|             1.48|                 3.26|             0.11|              2.67|              1.9|          31.1|              3.79|                         20|     120.0|  22.8|                   246|            0.49|     30.9|                        11|              0.01|                        31.6|                            13| 104.2|         49|
|        182|Survival |Female |      133.0|            1.7|                   62|    38.7|         0.6|             9.8|            404|         10.1|           57.1|          70.8|                    83.5|       40.4|                   5.24|                                       329|  3.10|             1.60|                 4.84|             0.09|              2.99|              3.4|          30.5|              5.78|                         12|     236.9|  28.3|                   243|            0.53|     32.1|                        32|              0.03|                        27.5|                            11|  98.3|         56|
|        183|Survival |Female |       98.0|            0.2|                   55|    35.8|         0.4|             5.8|            181|         11.6|           73.1|          78.4|                    95.1|       30.9|                   4.57|                                       317| 33.20|             0.67|                 3.25|             0.01|              3.34|              2.5|          14.7|              3.34|                         15|     507.0|  16.8|                   285|            0.53|     42.6|                        16|              0.02|                        30.2|                             5|   2.0|       1497|
|        184|Survival |Male   |      143.0|            2.0|                   59|    41.3|         0.5|            12.0|            212|          5.6|           63.5|          74.3|                    90.6|       39.3|                   4.44|                                       364|  3.50|             1.26|                 4.34|             0.09|              2.82|              4.6|          28.4|              3.53|                         26|     200.6|  26.8|                   167|            0.25|     33.0|                        18|              0.02|                        32.9|                            33| 112.2|         58|
|        185|Survival |Male   |      131.0|            0.5|                   53|    37.4|         0.1|             8.3|            337|          5.4|           88.3|          68.7|                    92.3|       38.5|                  10.21|                                       340|  9.30|             0.58|                 4.17|             0.05|              9.02|              3.1|           5.7|              4.29|                         20|     268.0|  22.5|                   216|            0.55|     31.3|                        29|              0.01|                        31.4|                            36|  59.7|        136|
|        186|Survival |Male   |      126.0|            0.0|                   88|    29.6|         0.2|             8.0|            404|          5.4|           85.5|          61.2|                    90.9|       37.0|                   5.94|                                       341|  2.70|             0.53|                 4.07|             0.00|              5.08|              4.6|           8.9|              2.98|                        202|     149.0|  23.2|                   664|            0.32|     31.6|                        47|              0.01|                        31.0|                           112| 105.0|         51|
|        187|Survival |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        188|Survival |Female |      147.0|            0.6|                  105|    45.5|         0.1|             4.8|            197|          7.4|           71.2|          78.9|                    91.6|       46.6|                   7.25|                                       315|  4.80|             1.50|                 5.09|             0.04|              5.16|              2.1|          20.7|              5.01|                         29|     349.0|  23.8|                   256|            0.54|     33.4|                        29|              0.01|                        28.9|                            21|  56.0|         97|
|        189|Survival |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        190|Survival |Female |      117.0|            0.8|                   46|    39.2|         0.2|             9.7|            260|          6.6|           61.5|          68.2|                    89.9|       33.8|                   6.22|                                       346|  3.62|             1.92|                 3.76|             0.05|              3.83|              3.6|          30.9|              3.65|                         10|     196.1|  22.5|                   110|            0.41|     29.0|                         9|              0.01|                        31.1|                             6| 118.9|         56|
|        191|Survival |Female |      129.0|            0.7|                   67|    44.5|         0.2|             5.9|            203|          7.6|           60.9|          72.0|                    94.3|       39.4|                   4.45|                                       327|  3.70|             1.36|                 4.18|             0.03|              2.71|              2.8|          30.6|              4.78|                         32|     167.0|  24.8|                   288|            0.34|     27.5|                        45|              0.01|                        30.9|                            37| 105.6|         47|
|        192|Survival |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        193|Survival |Female |      125.0|            0.0|                  106|    32.7|         0.1|             5.8|            330|          0.7|           95.1|          71.1|                    79.8|       34.3|                  20.22|                                       364|  8.00|             0.82|                 4.30|             0.00|             19.24|              2.8|           4.1|              3.23|                         33|     450.0|  14.1|                   632|            0.14|     38.4|                        23|              0.02|                        29.1|                             9|  87.4|         57|
|        194|Survival |Male   |      109.0|            0.8|                   51|    32.0|         0.1|            18.4|            150|         11.0|           82.0|          62.8|                    94.8|       31.0|                  12.05|                                       352|  6.20|             0.73|                 3.27|             0.10|              9.89|              7.3|           6.1|              3.06|                         37|     130.0|  26.4|                   234|            1.32|     30.8|                        24|              0.01|                        33.3|                            55|  95.5|         55|
|        195|Survival |Female |      136.0|            0.5|                   75|    36.0|         0.3|            20.2|            279|          7.8|           51.4|          72.9|                    87.4|       41.6|                   6.17|                                       327|  4.00|             2.47|                 4.76|             0.03|              3.17|              6.8|          40.0|              4.06|                         21|     270.0|  25.3|                   211|            0.48|     36.9|                        18|              0.02|                        28.6|                            16| 130.4|         48|
|        196|Survival |Male   |      116.0|            0.0|                   48|    34.2|         0.0|            17.2|            152|          4.6|           87.8|          61.1|                    88.7|       33.8|                   6.57|                                       343|  5.20|             0.50|                 3.81|             0.00|              5.77|              6.3|           7.6|              3.83|                         10|      90.0|  24.4|                   423|            0.30|     26.9|                        19|              0.00|                        30.4|                            15| 117.5|         46|
|        197|Survival |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        198|Survival |Male   |      124.0|            1.6|                   65|    33.1|         0.2|            13.1|            147|          9.5|           70.5|          65.6|                    91.7|       34.2|                   4.40|                                       363|  4.30|             0.80|                 3.73|             0.07|              3.10|              5.6|          18.2|              4.14|                         20|     186.0|  26.3|                   245|            0.42|     32.5|                        13|              0.01|                        33.2|                            17|  80.2|         76|
|        199|Survival |Male   |      151.0|            0.1|                   82|    28.3|         0.0|            12.1|            308|          3.7|           92.5|          75.6|                    85.0|       45.2|                  15.15|                                       334|  6.70|             0.56|                 5.32|             0.02|             14.01|              5.8|           3.7|              3.72|                         20|     162.0|  23.7|                   290|            0.56|     47.3|                        33|              0.00|                        28.4|                            23| 116.2|         60|
|        200|Survival |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        201|Survival |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        202|Death    |Male   |      158.0|            0.1|                  172|    28.7|         0.2|            16.3|             80|          6.3|           91.1|          57.8|                    95.6|       45.2|                  12.62|                                       350| 20.00|             0.29|                 4.73|             0.01|             11.50|              7.0|           2.3|              3.60|                         30|     492.0|  15.8|                  1313|            0.80|     29.1|                        59|              0.02|                        33.4|                            20|  51.1|        117|
|        203|Death    |Male   |      124.0|            0.0|                   99|    32.1|         0.1|             7.2|            252|          8.0|           86.3|          70.1|                    87.1|       36.6|                  11.08|                                       339| 13.70|             0.62|                 4.20|             0.00|              9.56|              4.2|           5.6|              3.22|                         24|     342.0|  21.2|                   570|            0.89|     38.0|                        32|              0.01|                        29.5|                            26|  78.0|         84|
|        204|Death    |Male   |      107.0|            0.0|                  172|    32.8|         0.1|            70.5|             24|          1.5|           94.9|          77.6|                    85.6|       31.5|                  12.44|                                       340|  8.30|             0.43|                 3.68|             0.00|             11.81|             39.3|           3.5|              2.19|                         72|     189.0|  30.3|                  1438|            0.19|     44.8|                        35|              0.01|                        29.1|                            36|  75.8|         87|
|        205|Death    |Female |       93.0|            0.0|                  149|    26.3|         0.4|             7.2|             80|          1.9|           95.0|          65.4|                    88.9|       26.3|                  13.00|                                       354| 19.60|             0.35|                 2.96|             0.00|             12.35|              5.4|           2.7|              3.40|                         40|     330.3|  13.2|                   602|            0.25|     39.1|                        56|              0.05|                        31.4|                            17|  19.4|        220|
|        206|Death    |Male   |      141.0|            0.0|                   94|    34.2|         0.3|            10.6|            169|          5.5|           90.7|          75.9|                    85.7|       40.8|                  11.03|                                       346|  7.10|             0.39|                 4.76|             0.00|             10.00|              6.3|           3.5|              2.92|                         48|     244.0|  23.1|                   566|            0.61|     41.7|                        78|              0.03|                        29.6|                            78|  89.6|         79|
|        207|Death    |Female |      142.0|            0.0|                   78|    31.0|         0.1|            16.9|             87|          2.5|           92.8|          74.1|                    90.0|       42.2|                  12.49|                                       336|  9.40|             0.58|                 4.69|             0.00|             11.59|              9.3|           4.6|              4.15|                         40|     283.0|  23.4|                   566|            0.31|     43.1|                        97|              0.01|                        30.3|                            25|  92.8|         54|
|        208|Death    |Male   |       98.0|            0.2|                  187|    27.1|         0.0|            19.9|             29|          3.6|           88.5|          66.7|                    99.6|       27.3|                   6.07|                                       359|  8.20|             0.47|                 2.74|             0.01|              5.37|             13.0|           7.7|              2.97|                         86|     225.0|  18.9|                   504|            0.22|     39.6|                       100|              0.00|                        35.8|                            42|  63.7|         97|
|        209|Death    |Male   |      122.0|            0.0|                   48|    29.6|         0.2|            14.2|            164|          4.0|           83.1|          57.5|                    83.5|       34.5|                   5.04|                                       354|  2.70|             0.64|                 4.13|             0.00|              4.19|              7.6|          12.7|              2.71|                         94|     123.0|  22.7|                   639|            0.20|     27.9|                        33|              0.01|                        29.5|                            85| 109.9|         58|
|        210|Death    |Female |      116.0|            1.1|                  189|    15.3|         0.1|            18.9|             79|          1.1|           97.0|          50.4|                    94.5|       36.0|                  25.49|                                       322| 16.00|             0.19|                 3.81|             0.28|             24.71|             13.8|           0.7|              2.62|                         26|     162.0|  18.1|                   539|            0.29|     35.1|                        99|              0.02|                        30.4|                            14|  19.0|        216|
|        211|Death    |Female |      136.0|            0.0|                   45|    29.5|         0.0|            10.0|            156|          4.0|           86.8|          62.2|                    85.3|       39.4|                   5.56|                                       345|  4.00|             0.51|                 4.62|             0.00|              4.83|              5.2|           9.2|              2.55|                         75|     244.0|  21.4|                   877|            0.22|     32.7|                        26|              0.00|                        29.4|                            29|  94.3|         61|
|        212|Death    |Male   |      108.0|            1.6|                   47|    35.5|         0.2|             5.9|            159|         18.5|           64.5|          80.2|                    75.9|       34.3|                   4.33|                                       315| 16.70|             0.66|                 4.52|             0.07|              2.79|              3.4|          15.2|              2.85|                         64|     369.0|  13.1|                   363|            0.80|     44.7|                        72|              0.01|                        23.9|                            18|  26.3|        184|
|        213|Death    |Male   |      108.0|            0.6|                   74|    35.4|         1.7|             7.5|             31|         35.2|           18.2|          64.5|                    92.6|       31.2|                   1.76|                                       346|  2.50|             0.78|                 3.37|             0.01|              0.32|              4.2|          44.3|              2.26|                         27|     393.0|  18.5|                  1548|            0.62|     29.1|                        50|              0.03|                        32.0|                            11| 130.8|         69|
|        214|Death    |Male   |      101.0|            0.0|                  175|    26.7|         0.2|            11.9|            107|          2.0|           95.7|          62.4|                   104.0|       20.7|                  15.30|                                       488| 12.30|             0.32|                 1.99|             0.00|             14.65|              6.8|           2.1|              2.58|                         46|     203.0|  18.3|                   858|            0.30|     35.7|                        25|              0.03|                        50.8|                            24|  74.3|         91|
|        215|Death    |Male   |      123.0|            0.0|                  127|    28.9|         0.1|            29.8|            152|          1.8|           95.8|          51.9|                    90.8|       34.7|                   9.11|                                       354| 15.90|             0.21|                 3.82|             0.00|              8.73|             13.2|           2.3|              2.35|                         60|     248.0|  21.9|                  1437|            0.16|     23.0|                       120|              0.01|                        32.2|                            57|  69.6|         95|
|        216|Death    |Male   |      123.0|            0.0|                  132|    26.9|         0.2|            19.6|            222|          2.3|           95.5|          75.1|                    89.1|       35.0|                  26.36|                                       351|  6.80|             0.52|                 3.93|             0.01|             25.19|             11.2|           2.0|              3.58|                        108|     244.0|  24.8|                   890|            0.60|     48.2|                        46|              0.04|                        31.3|                            55|  77.4|         88|
|        217|Death    |Male   |       50.0|            0.2|                   55|    26.9|         0.0|            32.6|             16|          1.5|           91.3|          37.0|                    98.7|       15.6|                   6.55|                                       321|  4.60|             0.46|                 1.58|             0.01|              5.98|             16.0|           7.0|              0.10|                         71|     381.0|  10.1|                   476|            0.10|     10.1|                        10|              0.00|                        31.6|                            20| 103.3|         71|
|        218|Death    |Male   |      124.0|            0.1|                   98|    26.1|         0.3|            85.9|             65|          4.6|           92.6|          63.4|                    98.3|       35.4|                  15.13|                                       350| 21.30|             0.36|                 3.60|             0.01|             14.02|             80.7|           2.4|              4.25|                         43|     241.0|  19.9|                   302|            0.70|     37.3|                        97|              0.04|                        34.4|                            23|  17.6|        289|
|        219|Death    |Male   |      154.0|            0.2|                  256|    24.0|         0.2|            18.3|             51|          2.0|           95.9|          62.0|                    94.9|       48.7|                  23.16|                                       316| 67.20|             0.40|                 5.13|             0.04|             22.20|             13.2|           1.7|              2.90|                        173|    1176.0|  15.6|                  1867|            0.47|     38.0|                       133|              0.05|                        30.0|                            32|   6.1|        736|
|        220|Death    |Male   |      118.0|            0.0|                  108|    29.3|         0.1|            18.8|             22|          3.4|           95.0|          56.6|                   101.2|       34.5|                  28.06|                                       342| 11.40|             0.43|                 3.41|             0.00|             26.64|              7.3|           1.5|              5.09|                         43|     220.0|  21.0|                  1640|            0.96|     27.3|                        45|              0.03|                        34.6|                            48|  75.8|         88|
|        221|Death    |Female |      113.0|            0.2|                   83|    18.3|         0.2|            24.4|             33|          2.9|           93.0|          39.0|                   106.7|       35.1|                  29.71|                                       322| 39.20|             1.09|                 3.29|             0.05|             27.64|             13.4|           3.7|              1.81|                        270|     502.0|  10.0|                  1588|            0.86|     20.7|                        22|              0.07|                        34.3|                            60|  11.1|        309|
|        222|Death    |Male   |       84.0|            0.0|                  284|    27.4|         0.0|           107.7|             13|          6.3|           63.7|          54.3|                    93.7|       23.9|                   0.80|                                       351| 27.20|             0.24|                 2.55|             0.00|              0.51|            103.0|          30.0|              2.90|                        318|     567.0|  11.0|                  1752|            0.05|     26.9|                       191|              0.00|                        32.9|                            54|  24.1|        270|
|        223|Death    |Male   |      171.0|            0.0|                   73|    26.8|         0.2|            12.8|            251|          2.9|           94.1|          59.2|                    91.3|       51.7|                  13.31|                                       331| 15.00|             0.37|                 5.66|             0.00|             12.54|              4.1|           2.8|              4.56|                         30|     246.0|  29.2|                   504|            0.38|     32.4|                        42|              0.02|                        30.2|                            28|  60.1|        100|
|        224|Death    |Female |      108.0|            0.0|                   74|    24.6|         0.1|            13.3|            287|          1.3|           97.7|          60.4|                    81.3|       32.2|                  16.30|                                       335|  5.90|             0.23|                32.50|             0.00|             26.09|              8.9|           0.9|              4.15|                         20|     145.0|  23.3|                   417|            0.34|     35.8|                        33|              0.04|                        27.3|                            20|  91.1|         47|
|        225|Death    |Female |      110.0|            0.0|                  111|    35.1|         0.1|            21.0|            202|          3.7|           91.2|          68.9|                    81.9|       33.0|                  13.82|                                       333| 10.90|             0.69|                 4.03|             0.00|             12.61|             12.4|           5.0|              2.27|                         30|     462.0|  19.5|                   884|            0.51|     33.8|                        79|              0.01|                        27.3|                            25|  70.4|         74|
|        226|Death    |Female |       92.0|            0.0|                   97|    27.3|         0.2|            22.3|            168|          1.7|           92.2|          66.8|                    86.8|       28.9|                  10.00|                                       318|  9.20|             0.59|                 3.33|             0.00|              9.22|              9.5|           5.9|              3.38|                         28|     445.0|  21.8|                   783|            0.17|     39.5|                        72|              0.02|                        27.6|                            10|  54.0|         96|
|        227|Death    |Male   |      129.0|            0.0|                   69|    24.1|         0.1|            10.2|            225|          4.2|           87.1|          68.5|                    89.3|       37.6|                   9.34|                                       343|  8.20|             0.80|                 4.21|             0.00|              8.14|              5.8|           8.6|              2.21|                         59|     246.0|  23.5|                   297|            0.39|     44.4|                        27|              0.01|                        30.6|                            48|  79.3|         78|
|        228|Death    |Female |      127.0|            0.5|                   43|    26.9|         0.1|            10.2|            176|          4.0|           90.8|          59.5|                    97.9|       37.6|                  15.12|                                       338|  9.00|             0.69|                 3.84|             0.07|             13.74|              5.2|           4.6|              4.37|                         14|     366.0|  21.5|                   368|            0.61|     32.6|                        35|              0.01|                        33.1|                            11|  89.3|         51|
|        229|Death    |Male   |      105.0|            1.0|                   73|    20.1|         0.6|            14.0|             32|          1.0|           94.1|          50.8|                    91.3|       32.4|                   7.92|                                       324| 28.40|             0.26|                 3.55|             0.08|              7.45|              4.2|           3.3|              2.47|                         47|     457.0|  21.1|                  1313|            0.08|     30.7|                        29|              0.05|                        29.6|                            21|  13.3|        384|
|        230|Death    |Female |      114.0|            0.0|                   42|    28.2|         0.1|            11.2|            234|          2.5|           88.6|          63.7|                    76.7|       33.6|                  14.36|                                       339|  4.40|             1.27|                 4.38|             0.00|             12.72|              5.0|           8.8|              2.75|                         30|     254.0|  17.7|                   608|            0.36|     35.5|                        24|              0.01|                        26.0|                            15|  91.3|         52|
|        231|Death    |Male   |      120.0|            0.0|                  103|    21.1|         0.1|            41.8|             10|          2.3|           94.3|          51.1|                   100.8|       38.4|                   7.99|                                       313| 15.10|             0.26|                 3.81|             0.00|              7.54|             41.7|           3.3|              3.41|                         34|     187.0|  25.3|                   772|            0.18|     30.0|                        84|              0.01|                        31.5|                            31|  66.0|        107|
|        232|Death    |Female |      134.0|            0.0|                   63|    36.4|         0.1|            16.9|            184|          7.1|           82.5|          67.9|                    86.0|       35.6|                   8.32|                                       376|  7.00|             0.86|                 4.14|             0.00|              6.86|              8.5|          10.3|              3.16|                         29|     134.0|  16.4|                   764|            0.59|     31.5|                        33|              0.01|                        32.4|                            20|  76.5|         63|
|        233|Death    |Male   |      119.0|            0.1|                   66|    21.8|         0.2|            21.8|             35|          3.0|           94.3|          50.5|                    97.9|       37.3|                  12.15|                                       319| 11.50|             0.29|                 3.81|             0.01|             11.46|             19.5|           2.4|              4.06|                         45|     109.0|  30.2|                   648|            0.37|     28.7|                        55|              0.02|                        31.2|                            56|  95.8|         67|
|        234|Death    |Male   |      162.0|            0.2|                   88|    31.6|         0.1|            11.1|             71|          2.0|           95.3|          61.9|                    96.2|       47.6|                  16.39|                                       340| 40.30|             0.39|                 4.95|             0.03|             15.63|              7.0|           2.4|              5.43|                         18|     713.0|  18.4|                   331|            0.32|     30.3|                        54|              0.02|                        32.7|                            49|  15.1|        305|
|        235|Death    |Female |      115.0|            0.1|                  114|    28.7|         0.1|            12.7|            256|          6.5|           87.3|          62.8|                    93.1|       33.8|                  19.71|                                       340|  3.20|             1.19|                 3.63|             0.01|             17.20|              4.8|           6.0|              4.65|                         24|     119.0|  26.4|                   530|            1.29|     34.1|                        61|              0.02|                        31.7|                            20| 105.9|         41|
|        236|Death    |Female |      127.0|            0.0|                   47|    27.9|         0.1|            17.3|            149|          3.0|           86.9|          61.0|                    91.2|       38.5|                  10.15|                                       330|  8.00|             1.01|                 4.22|             0.00|              8.83|              8.8|          10.0|              3.50|                         58|     105.0|  23.4|                   432|            0.30|     33.1|                        51|              0.01|                        30.1|                            41|  68.2|         76|
|        237|Death    |Male   |      112.0|            0.1|                  121|    17.6|         0.2|           119.3|              7|          4.5|           93.7|          48.9|                    97.7|       34.1|                  24.82|                                       328| 62.30|             0.37|                 3.49|             0.02|             23.25|            107.2|           1.5|              2.01|                        219|     815.0|  21.8|                  1820|            1.12|     31.3|                        26|              0.06|                        32.1|                           149|   6.3|        699|
|        238|Death    |Male   |      121.0|            1.2|                   94|    22.9|         0.2|             7.3|            170|          3.3|           91.9|          57.3|                    94.1|       34.9|                  13.80|                                       347| 16.80|             0.47|                 3.71|             0.16|             12.69|              5.8|           3.4|              4.32|                         24|     395.0|  22.1|                   458|            0.45|     34.4|                        33|              0.03|                        32.6|                            13|  16.7|        334|
|        239|Death    |Female |      139.0|            0.3|                   97|    20.1|         0.9|            22.3|             68|          1.3|           90.4|          51.6|                    88.5|       42.3|                  13.42|                                       329| 20.90|             0.95|                 4.78|             0.04|             12.13|             15.0|           7.1|              2.10|                         36|     204.0|  14.5|                   660|            0.18|     31.5|                        34|              0.12|                        29.1|                            10|  17.5|        227|
|        240|Death    |Male   |      122.0|            0.0|                   73|    26.4|         0.1|            13.1|             40|          2.1|           96.4|          69.0|                   103.1|       40.5|                  16.81|                                       301| 22.80|             0.23|                 3.93|             0.00|             16.21|              1.7|           1.4|              2.45|                         30|     166.0|  23.8|                   803|            0.35|     42.6|                        23|              0.02|                        31.0|                             9|  61.0|        106|
|        241|Death    |Female |      113.0|            0.1|                  128|    23.3|         0.1|            11.3|            154|          2.7|           92.9|          55.1|                    93.4|       32.8|                  14.03|                                       345| 15.90|             0.59|                 3.51|             0.01|             13.04|              8.9|           4.2|              4.22|                         29|     353.0|  21.2|                   678|            0.38|     31.8|                       109|              0.01|                        32.2|                            16|  23.9|        182|
|        242|Death    |Male   |      137.0|            0.0|                   90|    25.5|         0.1|            28.5|             70|          2.0|           95.4|          58.9|                   102.4|       42.0|                  20.95|                                       326| 26.50|             0.52|                 4.10|             0.00|             19.99|             26.2|           2.5|              3.77|                        179|     710.0|  20.3|                  1080|            0.41|     33.4|                       131|              0.03|                        33.4|                           173|  10.2|        475|
|        243|Death    |Male   |       82.0|            0.0|                  100|    19.1|         0.0|            75.5|             20|          1.4|           87.3|          49.9|                    96.3|       23.5|                   0.71|                                       349| 34.60|             0.08|                 2.44|             0.00|              0.62|             56.7|          11.3|              2.40|                         46|     427.0|  14.5|                   830|            0.01|     30.8|                        13|              0.00|                        33.6|                            30|  13.4|        370|
|        244|Death    |Female |       91.0|            0.0|                   88|    27.5|         0.0|            34.5|             79|          1.3|           96.4|          48.4|                   108.2|       27.8|                  11.00|                                       327| 12.30|             0.25|                 2.57|             0.00|             10.61|             21.0|           2.3|              4.05|                         41|     189.0|  18.6|                   834|            0.14|     20.9|                       126|              0.00|                        35.4|                            44|  83.7|         56|
|        245|Death    |Male   |       53.0|            0.1|                   83|    22.9|         0.0|           276.0|             52|          4.7|           76.4|          36.7|                   117.6|       16.0|                  11.23|                                       331| 28.60|             2.11|                 1.36|             0.01|              8.58|            216.3|          18.8|              0.71|                       1858|     525.0|   9.7|                  1764|            0.53|     13.8|                        33|              0.00|                        39.0|                           188|  19.4|        288|
|        246|Death    |Male   |      124.0|            0.7|                  193|    22.2|         0.4|            13.0|            168|          6.3|           87.6|          55.3|                    98.5|       38.5|                  16.85|                                       322| 22.80|             0.85|                 3.91|             0.12|             14.75|              9.1|           5.0|              2.56|                         18|     240.0|  15.0|                   471|            1.06|     33.1|                       168|              0.07|                        31.7|                             9|  12.9|        414|
|        247|Death    |Male   |      107.0|            0.0|                   44|    16.4|         0.1|            15.5|             44|          2.5|           81.7|          31.8|                   103.6|       31.9|                  15.64|                                       335|  8.00|             2.45|                 3.08|             0.00|             12.78|             10.0|          15.7|              1.35|                         19|      79.0|  14.3|                   345|            0.39|     15.4|                        11|              0.02|                        34.7|                             9|  90.7|         50|
|        248|Death    |Male   |      111.0|            0.0|                  107|    26.4|         0.1|             7.8|             54|          0.9|           96.2|          58.8|                    96.7|       34.9|                  13.77|                                       318| 17.60|             0.38|                 3.61|             0.00|             13.26|              6.6|           2.8|              2.05|                         24|     231.0|  22.1|                   594|            0.12|     32.4|                        33|              0.01|                        30.7|                             9|  16.5|        314|
|        249|Death    |Male   |         NA|             NA|                   61|    31.4|          NA|             6.7|             NA|           NA|             NA|          68.6|                      NA|         NA|                     NA|                                        NA| 24.00|               NA|                   NA|               NA|                NA|              2.1|            NA|              3.62|                        120|     678.0|  16.8|                   881|              NA|     37.2|                        48|                NA|                          NA|                            51|  21.0|        237|
|        250|Death    |Male   |      117.0|            0.0|                   48|    31.6|         0.1|            19.9|            464|          3.8|           89.6|          64.0|                    81.7|       36.2|                   8.52|                                       323|  5.50|             0.55|                 4.43|             0.00|              7.64|             10.3|           6.5|              3.35|                         18|     256.0|  26.1|                   450|            0.32|     32.4|                        18|              0.01|                        26.4|                            16|  97.3|         78|
|        251|Death    |Male   |      157.0|            0.2|                   51|    26.1|         0.1|            15.9|             36|          2.2|           95.8|          60.5|                   106.3|       33.5|                   9.65|                                       469| 15.10|             0.16|                 3.15|             0.02|              9.25|              8.3|           1.7|              3.12|                         27|     184.0|  33.8|                   403|            0.21|     34.4|                        19|              0.01|                        49.8|                            21|  84.1|         79|
|        252|Death    |Male   |      113.0|            0.3|                  116|    24.1|         0.0|            12.2|             86|          1.4|           85.7|          50.5|                    89.4|       34.4|                   7.93|                                       328| 13.40|             1.00|                 3.85|             0.02|              6.80|              9.5|          12.6|              2.86|                         29|      98.0|  25.6|                   492|            0.11|     27.4|                        43|              0.00|                        29.4|                            39|  94.5|         51|
|        253|Death    |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        254|Death    |Male   |      163.0|            0.0|                  112|    32.7|         0.2|            21.7|             68|          3.0|           95.0|          66.3|                    91.3|       47.1|                  12.50|                                       346| 25.30|             0.22|                 5.16|             0.00|             11.87|             15.1|           1.8|              2.94|                         44|     705.2|  15.4|                   637|            0.38|     33.6|                        35|              0.03|                        31.6|                            44|  34.0|        164|
|        255|Death    |Female |      148.0|            0.0|                   60|    23.7|         0.1|            20.5|             24|          2.0|           94.2|          59.3|                   100.9|       45.7|                  20.20|                                       324| 25.80|             0.75|                 4.53|             0.00|             19.01|             11.5|           3.7|              2.40|                         38|     280.0|  20.9|                   833|            0.41|     35.6|                        27|              0.03|                        32.7|                            72|  49.5|         94|
|        256|Death    |Male   |      109.0|            0.1|                   61|    31.6|         0.0|            10.4|            204|          6.0|           81.5|          80.4|                    83.0|       35.7|                   6.84|                                       305| 13.50|             0.85|                 4.30|             0.01|              5.57|              5.8|          12.4|              3.66|                         34|     260.0|  21.6|                   259|            0.41|     48.8|                        29|              0.00|                        25.3|                             6|  50.4|        117|
|        257|Death    |Male   |      110.0|            0.0|                  113|    25.8|         0.1|            10.6|             42|          1.3|           95.6|          62.9|                    93.4|       26.8|                  14.63|                                       410| 18.30|             0.44|                 2.87|             0.00|             13.99|              5.0|           3.0|              3.37|                         64|     495.7|  21.6|                   555|            0.19|     36.6|                        70|              0.01|                        38.3|                           144|  41.5|        151|
|        258|Death    |Male   |       98.0|            0.1|                  174|    19.8|         0.3|           157.9|              4|          6.2|           87.8|          41.3|                    88.7|       31.4|                   9.94|                                       312| 14.10|             0.56|                 3.54|             0.01|              8.72|            132.2|           5.6|              1.23|                        325|     302.0|  20.5|                   798|            0.62|     21.5|                        58|              0.03|                        27.7|                           169|  44.7|        141|
|        259|Death    |Male   |      140.0|            0.2|                   67|    29.4|         0.0|            10.8|            161|          8.4|           64.7|          63.1|                    84.2|       38.9|                   5.24|                                       360|  5.10|             1.40|                 4.62|             0.01|              3.39|              5.8|          26.7|              3.80|                         46|     193.0|  21.3|                   469|            0.44|     33.7|                        63|              0.00|                        30.3|                            27|  97.7|         64|
|        260|Death    |Female |      123.0|            0.0|                   64|    29.5|         0.0|            12.0|            138|          5.5|           86.3|          62.2|                    81.5|       35.2|                   3.30|                                       349|  6.20|             0.27|                 4.32|             0.00|              2.85|              5.3|           8.2|              3.37|                         58|     198.0|  21.0|                   405|            0.18|     32.7|                        14|              0.00|                        28.5|                            22|  87.0|         59|
|        261|Death    |Male   |       91.0|            0.0|                   35|    32.2|         0.3|            15.0|            287|          5.7|           90.5|          64.3|                    99.0|       28.6|                  34.84|                                       318| 36.50|             1.21|                 2.89|             0.00|             31.52|              9.0|           3.5|              3.03|                         40|     875.0|  30.0|                   645|            2.00|     32.1|                        32|              0.11|                        31.5|                            30|  14.1|        342|
|        262|Death    |Male   |      119.0|            0.0|                  116|    25.6|         0.1|            14.4|             75|          1.0|           96.9|          59.2|                    81.8|       35.0|                  12.26|                                       340|  9.20|             0.25|                 4.28|             0.00|             11.88|              6.6|           2.0|              3.62|                         35|      86.0|  24.7|                  1202|            0.12|     33.6|                        33|              0.01|                        27.8|                            22| 100.3|         58|
|        263|Death    |Female |      114.0|            0.0|                   71|    28.1|         0.1|            14.6|            392|          4.2|           91.2|          61.5|                    89.6|       32.7|                  20.05|                                       349|  3.80|             0.90|                 3.65|             0.00|             18.28|              6.2|           4.5|              4.18|                         28|     146.0|  28.8|                   663|            0.85|     33.4|                        38|              0.02|                        31.2|                            23|  97.8|         49|
|        264|Death    |Male   |      137.0|            1.1|                   53|    27.2|         0.2|            15.5|             92|          3.2|           82.1|          68.9|                    96.6|       39.5|                  10.05|                                       347|  6.30|             1.35|                 4.09|             0.11|              8.25|              5.7|          13.4|              3.09|                         39|     166.0|  17.6|                   418|            0.32|     41.7|                        38|              0.02|                        33.5|                            46|  66.3|         96|
|        265|Death    |Female |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        266|Death    |Male   |       95.0|            0.0|                  166|    28.4|         0.1|           295.4|             69|          8.1|           86.9|          51.9|                   107.6|       25.5|                  10.27|                                       373| 13.30|             0.50|                 2.37|             0.00|              8.93|            242.9|           4.9|              1.66|                         69|     216.0|  20.7|                   411|            0.83|     23.5|                        61|              0.01|                        40.1|                            37|  61.6|        114|
|        267|Death    |Female |      109.0|            0.1|                  210|    26.7|         0.2|            25.9|             43|          5.7|           90.2|          58.0|                    96.1|       34.3|                  18.07|                                       318| 68.40|             0.68|                 3.57|             0.01|             16.32|             20.0|           3.8|              3.54|                        141|     632.0|   6.3|                  1867|            1.03|     31.3|                       187|              0.03|                        30.5|                            42|   6.2|        537|
|        268|Death    |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        269|Death    |Male   |       93.0|            0.0|                   70|    21.5|         0.1|             2.8|            181|          2.7|           90.1|          60.9|                    88.1|       27.4|                  12.71|                                       339| 15.70|             0.90|                 3.11|             0.00|             11.46|              1.7|           7.1|              3.51|                         85|     320.8|  16.1|                   589|            0.34|     39.4|                        55|              0.01|                        29.9|                            30|  15.3|        316|
|        270|Death    |Male   |      119.0|            0.0|                   54|    30.8|         0.1|            10.2|            256|          2.4|           96.1|          61.5|                    93.0|       35.9|                  11.80|                                       331|  9.30|             0.17|                 3.86|             0.00|             11.34|              4.6|           1.4|              3.65|                          6|      97.0|  22.8|                   321|            0.28|     30.7|                        17|              0.01|                        30.8|                             9| 118.6|         45|
|        271|Death    |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                  17.30|                                        NA|    NA|               NA|                56.80|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        272|Death    |Male   |      146.0|            0.0|                   45|    31.9|         0.1|             7.6|            228|          4.9|           90.3|          67.7|                    87.1|       42.5|                  10.59|                                       344|  6.20|             0.50|                 4.88|             0.00|              9.56|              4.0|           4.7|              3.88|                         46|     180.0|  22.0|                   489|            0.52|     35.8|                        25|              0.01|                        29.9|                            36|  91.7|         65|
|        273|Death    |Female |      100.0|            0.0|                  190|    35.6|         0.0|            58.9|             81|          2.7|           94.9|          63.5|                    83.1|       30.1|                   6.57|                                       332| 15.60|             0.16|                 3.62|             0.00|              6.23|             29.1|           2.4|              2.94|                         53|     213.0|  20.5|                  1574|            0.18|     27.9|                        50|              0.00|                        27.6|                            27|  31.4|        130|
|        274|Death    |Female |      103.0|            0.0|                   81|    25.0|         0.0|             3.0|            122|          2.4|           95.8|          50.3|                    87.7|       30.0|                   7.82|                                       343| 20.60|             0.14|                 3.42|             0.00|              7.49|              1.8|           1.8|              3.19|                         20|     412.0|  12.6|                   367|            0.19|     25.3|                        56|              0.00|                        30.1|                            15|  19.2|        230|
|        275|Death    |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        276|Death    |Male   |      148.0|            0.6|                   60|    31.4|         0.1|            15.0|            241|          5.8|           89.3|          61.2|                    87.6|       41.5|                  14.78|                                       357|  4.50|             0.62|                 4.74|             0.09|             13.19|              7.1|           4.2|              3.72|                         81|     204.0|  23.9|                   457|            0.86|     29.8|                        32|              0.02|                        31.2|                            33|  81.4|         82|
|        277|Death    |Female |      123.0|            0.0|                  136|    23.5|         0.0|            19.3|            203|          1.9|           95.5|          63.0|                    84.6|       35.2|                  20.84|                                       349|  4.90|             0.55|                 4.16|             0.00|             19.88|              9.2|           2.6|              3.47|                         22|     357.0|  28.0|                   733|            0.40|     39.5|                       117|              0.01|                        29.6|                            29|  96.9|         61|
|        278|Death    |Male   |      122.0|            0.0|                  167|    29.2|         0.0|            21.7|            129|          0.7|           93.2|          64.4|                    91.7|       33.0|                  24.64|                                       370| 20.30|             1.50|                 3.60|             0.00|             22.95|             11.9|           6.1|              2.91|                         59|     381.6|  15.4|                   974|            0.18|     35.2|                        96|              0.01|                        33.9|                            89|  47.2|        120|
|        279|Death    |Male   |      119.0|            0.0|                  131|    27.4|         0.1|            35.5|             10|          2.5|           95.2|          49.3|                    98.9|       34.7|                  29.74|                                       343| 24.00|             0.65|                 3.51|             0.00|             28.32|             22.7|           2.2|              2.44|                         32|     763.6|  17.5|                  1331|            0.75|     21.9|                        17|              0.02|                        33.9|                            19|  57.7|        110|
|        280|Death    |Male   |       96.0|            0.0|                  104|    29.9|         0.1|           104.6|             -1|          1.5|           96.6|          56.4|                    97.4|       29.5|                  10.56|                                       325| 18.30|             0.19|                 3.03|             0.00|             10.20|             70.0|           1.8|              4.51|                         85|     194.0|  23.9|                  1196|            0.16|     26.5|                        50|              0.01|                        31.7|                            46|  67.5|        102|
|        281|Death    |Female |      112.0|            0.0|                  105|    23.6|         0.7|            42.5|             20|          4.1|           86.6|          72.8|                    93.1|       32.4|                   2.69|                                       346| 15.90|             0.23|                 3.48|             0.00|              2.33|             27.7|           8.6|              2.36|                         78|     220.9|  20.6|                   731|            0.11|     49.2|                        76|              0.02|                        32.2|                            29|  43.0|        105|
|        282|Death    |Male   |        6.4|            0.0|                  424|    26.9|         0.2|            76.4|             16|          2.5|           96.6|          61.0|                    85.2|       37.9|                  27.20|                                       348| 11.13|             0.18|                 4.45|             0.00|             26.28|             49.1|           0.7|              4.59|                        104|     238.9|  17.8|                  1867|            0.69|     34.1|                       257|              0.05|                        29.7|                            47|  84.9|         89|
|        283|Death    |Female |      131.0|            0.2|                  220|    23.0|         0.1|            44.0|             17|          2.4|           90.1|          66.2|                    87.1|       39.8|                  20.77|                                       329|  7.80|             1.49|                 4.57|             0.04|             18.72|             22.7|           7.2|              5.22|                         18|     180.0|  21.0|                   944|            0.50|     43.2|                        33|              0.02|                        28.7|                             7|  73.5|         71|
|        284|Death    |Male   |      153.0|            0.7|                  157|    23.4|         0.2|            22.0|            103|          4.0|           94.3|          53.1|                    90.5|       44.6|                  30.55|                                       343| 44.60|             0.23|                 4.93|             0.21|             28.84|             12.0|           0.8|              3.88|                        230|     693.0|  16.2|                  1867|            1.21|     29.7|                        85|              0.06|                        31.0|                            79|   7.3|        564|
|        285|Death    |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        286|Death    |Male   |      162.0|            0.0|                  166|    31.1|         0.3|            17.5|             90|          3.2|           87.4|          67.6|                    86.3|       44.8|                  17.18|                                       362| 26.90|             1.57|                 5.19|             0.00|             15.01|              9.3|           9.1|              4.01|                        176|     743.0|  13.2|                  1798|            0.55|     36.5|                        15|              0.05|                        31.2|                            59|  26.1|        210|
|        287|Death    |Male   |      107.0|            0.0|                   99|    24.5|         0.4|             5.0|            306|          0.9|           97.0|          57.2|                    94.9|       33.4|                  26.55|                                       320| 30.80|             0.45|                 3.52|             0.00|             25.76|              4.0|           1.7|              1.34|                         23|     219.3|  24.4|                   387|            0.24|     32.7|                        60|              0.10|                        30.4|                            29|  46.5|        148|
|        288|Death    |Male   |      119.0|            0.0|                   56|    26.1|         0.1|            20.4|            225|          4.0|           93.8|          72.7|                    88.1|       34.1|                  18.04|                                       349|  7.30|             0.38|                 3.87|             0.00|             16.92|              8.7|           2.1|              3.25|                         22|     129.0|  19.9|                   464|            0.72|     46.6|                        30|              0.02|                        30.7|                            29|  96.1|         70|
|        289|Death    |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        290|Death    |Female |      124.0|            0.0|                   84|    30.2|         0.2|            10.1|            163|          9.1|           84.7|          71.4|                    91.9|       36.5|                  13.14|                                       340| 11.40|             0.79|                 3.97|             0.00|             11.14|              5.3|           6.0|              3.53|                         31|     381.0|  23.8|                   610|            1.19|     41.2|                        18|              0.02|                        31.2|                            13|  74.4|         62|
|        291|Death    |Male   |      139.0|            0.6|                  119|    23.2|         0.0|            17.4|            119|          1.3|           93.1|          58.1|                    89.8|       40.5|                  14.32|                                       343|  5.40|             0.71|                 4.51|             0.08|             13.34|              9.3|           5.0|              3.64|                         43|      54.0|  25.2|                   620|            0.19|     34.9|                       159|              0.00|                        30.8|                            40| 119.8|         53|
|        292|Death    |Female |      115.0|            0.0|                   48|    35.2|         0.0|             6.6|            201|          2.8|           84.2|          62.4|                    87.9|       32.1|                   3.92|                                       358|  2.90|             0.51|                 3.65|             0.00|              3.30|              2.9|          13.0|              3.05|                         47|     239.0|  20.9|                   577|            0.11|     27.2|                        61|              0.00|                        31.5|                            41|  96.8|         55|
|        293|Death    |Male   |      103.0|            0.0|                  118|    34.1|         0.2|            23.5|            113|          2.4|           92.6|          75.3|                    88.1|       28.8|                  16.75|                                       358| 17.70|             0.80|                 3.27|             0.00|             15.51|             10.9|           4.8|              1.68|                         40|     162.0|  22.3|                   232|            0.41|     41.2|                        53|              0.03|                        31.5|                            17|  55.6|        103|
|        294|Death    |Male   |      151.0|            0.0|                  103|    25.6|         0.5|            12.1|            420|          4.3|           92.0|          62.3|                    94.4|       45.2|                  15.49|                                       334|  4.50|             0.50|                 4.79|             0.00|             14.26|              7.8|           3.2|              2.66|                         19|     145.0|  24.1|                   439|            0.66|     36.7|                       145|              0.07|                        31.5|                            33| 102.0|         66|
|        295|Death    |Male   |      109.0|            0.0|                  183|    22.2|         0.0|            34.3|             19|          1.8|           89.4|          64.7|                   103.0|       34.4|                   2.17|                                       317| 17.60|             0.19|                 3.34|             0.00|              1.94|             31.9|           8.8|              2.11|                         38|     110.0|  22.3|                   493|            0.04|     42.5|                       226|              0.00|                        32.6|                            86|  42.7|        144|
|        296|Death    |Male   |      129.0|            0.0|                   43|    36.4|         0.0|             5.5|            113|         11.5|           67.9|          60.7|                    82.3|       36.7|                   2.10|                                       351|  4.70|             0.72|                 0.80|             0.00|              2.37|              3.3|          20.6|              2.52|                         22|     279.0|  24.5|                   246|            0.40|     24.3|                         8|              0.00|                        28.9|                            13| 111.4|         58|
|        297|Death    |Male   |      139.0|            0.0|                  178|    31.3|         0.2|            12.3|            103|          2.6|           94.6|          63.8|                    88.9|       41.0|                  23.08|                                       339| 11.60|             0.60|                 4.61|             0.00|             21.83|              6.6|           2.6|              3.88|                         69|     346.9|  20.8|                  1692|            0.61|     32.5|                        86|              0.04|                        30.2|                            29|  65.6|        114|
|        298|Death    |Male   |      163.0|            0.0|                   73|    35.8|         0.2|            12.0|            209|          6.4|           90.7|          72.4|                    88.9|       49.6|                  21.72|                                       329| 24.10|             0.59|                 5.58|             0.00|             19.69|              5.4|           2.7|              4.56|                         23|     476.0|  22.1|                   591|            1.39|     36.6|                        21|              0.05|                        29.2|                            20|  45.1|        140|
|        299|Death    |Male   |      156.0|            0.0|                   54|    32.4|         0.2|            13.3|            179|         16.4|           64.9|          73.4|                    84.6|       40.6|                   6.15|                                       384|  4.80|             1.14|                 4.80|             0.00|              3.99|              6.7|          18.5|              2.61|                         41|     225.0|  26.5|                   486|            1.01|     41.0|                        21|              0.01|                        32.5|                            15|  67.7|         87|
|        300|Death    |Female |      147.0|            0.0|                  109|    31.8|         0.3|            17.6|             81|          2.2|           92.2|          70.2|                    82.7|       44.1|                  11.28|                                       333| 13.40|             0.60|                 5.33|             0.00|             10.40|              8.7|           5.3|              3.45|                         54|     320.0|  18.0|                  1138|            0.25|     38.4|                        35|              0.03|                        27.6|                            42|  48.2|         95|
|        301|Death    |Female |      112.0|            0.0|                   92|    29.3|         0.1|             7.0|            168|          1.3|           87.9|          64.3|                    84.6|       32.5|                  10.17|                                       345|  9.70|             1.09|                 3.84|             0.00|              8.94|              3.4|          10.7|              4.08|                         67|     259.0|  19.5|                   714|            0.13|     35.0|                        42|              0.01|                        29.2|                            59|  62.2|         82|
|        302|Death    |Male   |      159.0|            0.0|                   81|    33.6|         0.2|            12.4|             65|          3.6|           95.3|          71.1|                    95.2|       50.1|                  33.81|                                       317| 27.00|             0.31|                 5.26|             0.00|             32.22|              4.8|           0.9|              6.11|                         52|     694.0|  16.4|                   999|            1.21|     37.5|                        52|              0.07|                        30.2|                            59|  36.5|        165|
|        303|Death    |Male   |      150.0|            0.0|                  118|    32.6|         0.1|            29.1|            115|          2.5|           92.5|          71.4|                    87.7|       41.9|                   8.70|                                       358|  8.00|             0.43|                 4.78|             0.00|              8.04|             13.5|           4.9|              2.82|                         42|     180.0|  22.4|                   932|            0.22|     38.8|                        20|              0.01|                        31.4|                            22|  73.7|         85|
|        304|Death    |Male   |      112.0|            0.1|                  170|    20.7|         0.4|             6.1|             45|          2.1|           93.7|          56.8|                   106.3|       39.1|                  18.55|                                       286| 32.00|             0.68|                 3.68|             0.01|             17.40|              4.8|           3.7|              3.84|                         33|     396.0|  28.3|                  1126|            0.39|     36.1|                        13|              0.07|                        30.4|                            16|  10.6|        458|
|        305|Death    |Male   |      150.0|            0.0|                   85|    34.1|         0.1|            29.2|             57|          0.8|           98.0|          70.0|                    89.7|       41.9|                  13.11|                                       358| 17.40|             0.14|                 4.67|             0.00|             12.85|             23.7|           1.1|              3.05|                         57|     341.0|  15.0|                   693|            0.11|     35.9|                        33|              0.01|                        32.1|                            33|  53.0|        113|
|        306|Death    |Male   |      137.0|            0.0|                  151|    29.8|         0.1|             8.8|             72|          1.8|           97.0|          63.6|                   101.0|       42.0|                  10.97|                                       326| 56.60|             0.12|                 4.16|             0.00|             10.64|              6.4|           1.1|              2.72|                         29|    1001.0|  19.4|                   752|            0.20|     33.8|                        28|              0.01|                        32.9|                             8|   8.3|        529|
|        307|Death    |Female |      142.0|            0.0|                   69|    27.9|         0.1|             6.9|            361|          2.2|           91.6|          73.1|                    89.8|       42.1|                  20.43|                                       337|  5.60|             1.24|                 4.69|             0.00|             18.73|              4.1|           6.1|              3.56|                         67|     373.0|  17.3|                   568|            0.44|     45.2|                        49|              0.02|                        30.3|                            21| 112.7|         51|
|        308|Death    |Male   |      133.0|            0.0|                   59|    25.4|         0.2|             7.4|             48|          5.1|           91.7|          58.9|                    95.5|       38.1|                   4.94|                                       349| 26.50|             0.15|                 3.99|             0.00|              4.53|              5.5|           3.0|              1.88|                         46|     332.0|  24.2|                   398|            0.25|     33.5|                        25|              0.01|                        33.3|                            14|  11.4|        422|
|        309|Death    |Male   |      120.0|            0.0|                  130|    24.6|         0.2|            11.4|             61|          0.9|           97.8|          60.9|                    95.9|       34.7|                  31.73|                                       346| 29.00|             0.34|                 3.62|             0.00|             31.04|              6.7|           1.1|              2.30|                         56|     520.0|  12.8|                   705|            0.28|     36.3|                        33|              0.07|                        33.1|                            39|  24.4|        199|
|        310|Death    |Female |       96.0|            0.0|                   65|    27.9|         0.1|             9.1|            219|          5.6|           88.1|          61.6|                    83.2|       26.7|                   7.71|                                       360|  3.60|             0.48|                 3.21|             0.00|              6.79|              5.6|           6.2|              3.26|                         21|     267.0|  21.6|                   385|            0.43|     33.7|                        34|              0.01|                        29.9|                            14|  67.7|         76|
|        311|Death    |Female |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        312|Death    |Male   |      149.0|            0.0|                  183|    23.8|         0.2|            11.7|            111|          3.8|           93.0|          66.8|                    96.8|       45.5|                  21.67|                                       327| 38.80|             0.65|                 4.70|             0.01|             20.13|              6.9|           3.0|              3.21|                        783|     993.0|  10.0|                  1867|            0.83|     43.0|                        96|              0.05|                        31.7|                           421|  14.4|        342|
|        313|Death    |Male   |      102.0|            0.0|                   75|    31.0|         0.0|            12.8|            138|          4.6|           83.6|          69.0|                    90.7|       32.2|                  63.20|                                       317| 11.90|             0.54|               749.50|             0.00|              3.83|              7.0|          11.8|              2.73|                         61|     329.0|  17.0|                   577|            0.21|     38.0|                        31|              0.00|                        28.7|                            28|  84.1|         59|
|        314|Death    |Male   |      146.0|            0.0|                  130|    26.9|         0.1|            17.4|             68|          4.1|           93.1|          60.0|                    94.8|       41.7|                  11.16|                                       350| 12.70|             0.30|                 4.40|             0.00|             10.39|              8.9|           2.7|              3.50|                         16|     300.0|  20.6|                   807|            0.46|     33.1|                        35|              0.01|                        33.2|                            18|  70.2|         96|
|        315|Death    |Male   |      149.0|            3.7|                   71|    34.2|         0.2|            12.7|            221|          4.0|           81.0|          72.4|                    83.7|       42.0|                  10.17|                                       355| 10.90|             1.13|                 5.02|             0.38|              8.23|              2.6|          11.1|              3.16|                         38|     274.0|  18.0|                   561|            0.41|     38.2|                        57|              0.02|                        29.7|                            23|  72.5|         94|
|        316|Death    |Male   |      109.0|            0.2|                  129|    23.2|         0.3|             6.4|             25|         19.4|           55.1|          63.8|                    89.6|       31.9|                   6.20|                                       342|  2.10|             1.55|                 3.56|             0.01|              3.42|              2.1|          25.0|              1.76|                         54|      98.0|  23.6|                   664|            1.20|     40.6|                        46|              0.02|                        30.6|                             8| 126.3|         32|
|        317|Death    |Female |       88.0|            0.0|                   57|    30.1|         0.0|            10.2|            128|         11.1|           81.4|          61.6|                    95.9|       25.7|                   7.22|                                       342|  2.40|             0.54|                 2.68|             0.00|              5.88|              7.4|           7.5|              2.94|                         38|     164.0|  24.4|                   401|            0.80|     31.5|                         9|              0.00|                        32.8|                             9| 107.0|         61|
|        318|Death    |Male   |      126.0|            0.2|                   81|    30.3|         0.4|            12.3|            145|          3.2|           77.3|          66.8|                    68.5|       37.2|                  13.41|                                       339|  5.30|             2.54|                 5.43|             0.03|             10.36|              5.7|          18.9|              2.53|                         42|     203.3|  22.9|                   987|            0.43|     36.5|                        34|              0.05|                        23.2|                            63|  78.5|         83|
|        319|Death    |Female |      116.0|            0.0|                   95|    30.1|         0.1|             6.1|            125|          2.3|           94.5|          62.6|                    89.0|       31.4|                  22.15|                                       369|  5.70|             0.69|                 3.53|             0.00|             20.92|              3.5|           3.1|              3.23|                         25|     386.0|  21.1|                   617|            0.51|     32.5|                        72|              0.03|                        32.9|                            19|  94.6|         59|
|        320|Death    |Male   |      113.0|            0.2|                   88|    20.1|         0.5|            14.2|             16|          1.9|           91.5|          50.4|                   104.3|       31.5|                   4.22|                                       359| 11.50|             0.25|                 3.02|             0.01|              3.86|             10.7|           5.9|              2.09|                         93|      66.0|  32.6|                   678|            0.08|     30.3|                        79|              0.02|                        37.4|                            98| 106.1|         58|
|        321|Death    |Male   |      124.0|            0.0|                   47|    27.0|         0.2|             7.2|            219|          4.6|           90.6|          61.4|                    92.7|       35.5|                  11.77|                                       349| 13.60|             0.54|                 3.83|             0.00|             10.67|              5.0|           4.6|              2.60|                         25|     353.8|  20.4|                   415|            0.54|     34.4|                        16|              0.02|                        32.4|                            17|  87.7|         79|
|        322|Death    |Female |      126.0|            0.0|                   60|    18.6|         0.1|            10.2|             56|          3.2|           93.1|          60.2|                    98.0|       39.9|                  10.49|                                       316|  7.60|             0.38|                 4.07|             0.00|              9.76|              3.7|           3.6|              3.18|                         21|      98.0|  20.5|                   487|            0.34|     41.6|                        14|              0.01|                        31.0|                            13| 105.3|         29|
|        323|Death    |Male   |      145.0|            0.6|                  133|    28.5|         0.2|            22.2|            117|          2.6|           93.0|          67.4|                    89.5|       43.3|                  12.89|                                       335| 10.70|             0.46|                 4.84|             0.08|             11.99|             11.1|           3.6|              4.31|                         38|     296.0|  22.5|                   669|            0.33|     38.9|                       155|              0.03|                        30.0|                            52|  85.5|         83|
|        324|Death    |Male   |      103.0|            0.0|                   56|    22.0|         0.1|            26.2|             58|          0.7|           98.9|          45.6|                   104.7|       30.9|                  16.65|                                       333| 29.80|             0.05|                 2.95|             0.00|             16.46|             19.4|           0.3|              2.42|                         32|     412.0|  20.2|                   862|            0.12|     23.6|                        17|              0.02|                        34.9|                            24|  28.3|        200|
|        325|Death    |Female |      136.0|            0.1|                   67|    33.0|         0.4|            29.8|             99|          5.6|           86.8|          70.5|                    88.3|       39.3|                  11.41|                                       346|  4.60|             0.81|                 4.45|             0.01|              9.90|             12.1|           7.1|              4.60|                         26|     160.0|  25.3|                   436|            0.64|     37.5|                        15|              0.05|                        30.6|                            18|  92.5|         49|
|        326|Death    |Male   |      103.0|            1.1|                   59|    25.3|         0.1|             7.4|            159|          2.7|           92.8|          60.0|                    95.5|       32.1|                  11.40|                                       321| 16.10|             0.47|                68.30|             0.15|             13.24|              5.3|           3.3|              2.85|                         88|     528.3|  23.5|                   431|            0.39|     34.7|                        10|              0.01|                        30.7|                            44|  40.3|        150|
|        327|Death    |Female |      120.0|            0.0|                   84|    31.5|         0.1|             8.4|             87|          1.2|           96.0|          62.2|                    88.8|       34.9|                  13.36|                                       344| 11.40|             0.36|                 3.93|             0.00|             12.83|              5.6|           2.7|              4.75|                         33|     309.0|  20.9|                   671|            0.16|     30.7|                        49|              0.01|                        30.5|                            29|  46.9|        100|
|        328|Death    |Male   |       16.0|            0.0|                  134|    35.6|         0.0|             7.4|            172|          2.8|           94.6|          65.8|                    93.3|       37.9|                   6.87|                                       338|  3.30|             0.18|                 4.06|             0.00|              6.50|              4.7|           2.6|              3.36|                         40|     232.0|  20.4|                   348|            0.19|     30.2|                        63|              0.00|                        31.5|                            31| 103.1|         58|
|        329|Death    |Male   |      116.0|            0.0|                   68|    34.2|         0.1|            13.5|            103|          2.1|           87.4|          71.4|                    96.0|       33.7|                  14.20|                                       344|  9.20|             0.80|                10.00|             0.00|              6.69|              7.8|          10.4|              2.65|                         76|     245.0|  19.4|                   455|            0.16|     37.2|                        33|              0.01|                        33.0|                            34|  45.5|        126|
|        330|Death    |Male   |       72.0|            0.0|                  123|    24.2|         0.1|            11.5|            305|          4.8|           89.5|          57.6|                    95.7|       19.9|                  16.34|                                       362| 12.40|             0.92|                 2.08|             0.00|             14.62|              6.4|           5.6|              2.98|                         24|     318.0|  15.0|                   261|            0.78|     33.4|                        77|              0.02|                        34.6|                            27|  29.2|        184|
|        331|Death    |Male   |       96.0|            0.0|                  162|    24.8|         0.2|            16.8|            159|          8.6|           80.9|          69.6|                    74.5|       32.1|                   5.56|                                       299|  2.70|             0.57|                 4.31|             0.00|              4.50|             10.1|          10.3|              2.95|                         57|     128.0|  17.4|                   578|            0.48|     44.8|                        74|              0.01|                        22.3|                            17| 110.0|         63|
|        332|Death    |Male   |      119.0|            0.0|                  195|    30.1|         0.1|            79.7|             11|          2.6|           95.9|          64.4|                    96.4|       34.6|                  18.69|                                       344| 40.20|             0.26|                 3.59|             0.00|             17.93|             64.5|           1.4|              2.83|                       1593|    1009.0|  14.8|                  1867|            0.48|     34.3|                        75|              0.02|                        33.1|                           666|  12.3|        387|
|        333|Death    |Male   |      153.0|            0.0|                   54|    37.3|         0.1|            13.3|            143|          3.1|           93.5|          69.9|                    92.6|       43.9|                  12.16|                                       349|  5.60|             0.40|                 4.74|             0.00|             11.37|              6.7|           3.3|              3.79|                         76|     172.0|  22.6|                   353|            0.38|     32.6|                        18|              0.01|                        32.3|                            74|  80.7|         87|
|        334|Death    |Male   |      158.0|            0.0|                   75|    30.7|         0.1|            18.9|            112|          3.2|           93.8|          59.0|                    85.1|       44.7|                   8.93|                                       353| 11.10|             0.26|                 5.25|             0.00|              8.37|              5.3|           2.9|              5.54|                         24|     189.0|  26.2|                   703|            0.29|     28.3|                        50|              0.01|                        30.1|                            22|  79.6|         87|
|        335|Death    |Female |      113.0|            0.0|                   76|    27.6|         0.0|             6.5|            121|          5.3|           88.1|          63.6|                    91.1|       32.8|                   7.78|                                       345|  6.70|             0.51|                 3.60|             0.00|              6.86|              4.1|           6.6|              2.67|                         60|     373.0|  15.8|                  1106|            0.41|     36.0|                        37|              0.00|                        31.4|                            32|  41.7|        126|
|        336|Death    |Male   |      137.0|            0.0|                  210|    27.4|         0.1|            16.1|            235|          2.3|           93.9|          64.1|                    62.3|       41.0|                  16.15|                                       334|  7.00|             0.59|                 6.58|             0.00|             15.17|             11.6|           3.7|              2.73|                         57|     243.0|  19.4|                   544|            0.37|     36.7|                       254|              0.02|                        20.8|                            57|  56.4|        117|
|        337|Death    |Male   |      105.0|            0.4|                   92|    38.4|         0.2|            25.3|            184|          5.1|           92.7|          73.5|                    97.3|       32.6|                 605.80|                                       322| 25.80|             0.41|               109.40|             0.11|             23.26|             20.3|           1.6|              3.16|                         87|     487.0|  12.7|                  1544|            1.27|     35.1|                        26|              0.06|                        31.3|                            61|  11.0|        467|
|        338|Death    |Male   |      119.0|            1.2|                  283|    26.9|         0.1|             7.6|             93|          1.4|           95.1|          70.3|                    96.2|       35.7|                  14.53|                                       333|  5.50|             0.32|                 3.71|             0.18|             13.81|              3.9|           2.2|              3.05|                         19|      52.0|  25.6|                   863|            0.21|     43.4|                        49|              0.01|                        32.1|                            21| 112.0|         50|
|        339|Death    |Male   |      142.0|            0.0|                  259|    34.0|         0.1|            26.3|             79|          1.7|           96.1|          68.4|                    86.8|       39.3|                  10.55|                                       361|  4.90|             0.22|                 4.53|             0.00|             10.14|             14.6|           2.1|              3.55|                         46|     170.0|  23.7|                   887|            0.18|     34.4|                       195|              0.01|                        31.3|                            79|  93.9|         67|
|        340|Death    |Male   |      131.0|            0.0|                   93|    39.5|         0.3|             9.5|            234|         14.8|           75.2|          70.6|                    82.3|       35.9|                   7.08|                                       365|  1.70|             0.69|                 4.36|             0.00|              5.32|              5.1|           9.7|              2.85|                         39|     137.0|  27.5|                   266|            1.05|     31.1|                        27|              0.02|                        30.0|                            18| 122.8|         45|
|        341|Death    |Male   |      134.0|            0.0|                   59|    33.3|         0.0|            10.3|            169|          6.0|           90.0|          68.7|                    92.1|       40.7|                   9.54|                                       329| 13.00|             0.38|                 4.42|             0.00|              8.59|              5.3|           4.0|              3.39|                         16|     349.4|  20.3|                   369|            0.57|     35.4|                        13|              0.00|                        30.3|                             9|  68.2|         88|
|        342|Death    |Female |       72.0|            0.3|                   44|    26.1|         0.1|            14.6|            114|          2.4|           85.5|          47.6|                    69.7|       23.2|                   7.80|                                       310|  6.10|             0.91|                 3.33|             0.02|              6.67|              7.5|          11.7|              2.71|                         23|     113.0|  30.1|                   471|            0.19|     21.5|                        20|              0.01|                        21.6|                            20| 105.9|         41|
|        343|Death    |Female |      124.0|            0.0|                   79|    23.2|         0.3|            30.1|              6|          2.6|           94.7|          51.2|                    84.9|       35.9|                  32.99|                                       345| 19.70|             0.79|                 4.23|             0.00|             31.24|             22.6|           2.4|              3.84|                         14|     284.0|  21.6|                   874|            0.86|     28.0|                        19|              0.10|                        29.3|                            17|  33.9|        128|
|        344|Death    |Male   |      139.0|            0.0|                   81|    30.6|         0.1|            10.6|            161|          3.8|           94.9|          54.5|                    88.5|       37.8|                  19.07|                                       368| 13.50|             0.22|                 4.27|             0.00|             18.12|              5.5|           1.2|              3.48|                         34|     401.0|  18.1|                   655|            0.72|     23.9|                        12|              0.01|                        32.6|                            26|  79.1|         82|
|        345|Death    |Male   |      120.0|            0.0|                   82|    23.1|         0.0|            12.8|             28|          2.6|           93.4|          51.2|                   100.8|       38.2|                   6.05|                                       314| 13.20|             0.24|                 3.79|             0.00|              5.65|              7.9|           4.0|              2.23|                         31|     213.0|  12.4|                   426|            0.16|     28.1|                        21|              0.00|                        31.7|                            30|  96.4|         66|
|        346|Death    |Male   |      146.0|            0.0|                   68|    38.6|         0.0|            13.1|            124|         13.7|           70.7|          74.3|                    89.0|       42.0|                   3.15|                                       348| 11.80|             0.49|                 4.72|             0.00|              2.23|              7.6|          15.6|              3.15|                         90|     223.0|  22.0|                  1160|            0.43|     35.7|                        31|              0.00|                        30.9|                            25|  73.4|         93|
|        347|Death    |Female |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        348|Death    |Male   |      129.0|            0.0|                  120|    26.2|         0.1|             8.9|            106|          1.8|           96.1|          60.6|                    92.5|       38.1|                  15.97|                                       339| 11.40|             0.32|                 4.12|             0.00|             15.35|              3.9|           2.0|              4.30|                         33|     363.0|  19.0|                   574|            0.29|     34.4|                        27|              0.01|                        31.3|                            41|  90.6|         73|
|        349|Death    |Male   |      101.0|            0.0|                  105|    24.8|         0.1|             6.5|             74|          1.7|           96.5|          56.5|                    98.7|       30.3|                  12.27|                                       333| 11.90|             0.21|                 3.07|             0.00|             11.84|              4.8|           1.7|              5.27|                         39|     154.0|  28.0|                   428|            0.21|     31.7|                       104|              0.01|                        32.9|                            33|  58.3|        102|
|        350|Death    |Male   |      143.0|            0.0|                   56|    29.1|         0.2|            11.7|            135|          2.0|           94.0|          67.8|                    92.8|       41.4|                  11.26|                                       345| 12.90|             0.43|                 4.46|             0.00|             10.58|              3.7|           3.8|              2.59|                         40|     317.0|  15.7|                   739|            0.23|     38.7|                        40|              0.02|                        32.1|                            30|  48.9|        125|
|        351|Death    |Female |      110.0|            0.0|                   99|    29.1|         0.2|             9.0|            124|          2.6|           91.0|          56.0|                    86.6|       31.8|                  13.30|                                       346|  5.40|             0.83|                 3.67|             0.00|             12.10|              3.8|           6.2|              3.46|                         34|     226.0|  22.9|                   801|            0.34|     26.9|                        49|              0.03|                        30.0|                            47| 107.9|         62|
|        352|Death    |Male   |      115.0|            0.0|                   62|    31.7|         0.1|             9.3|            288|          3.7|           94.9|          70.7|                    84.4|       32.5|                   3.40|                                       354| 10.30|             0.26|                 6.90|             0.00|             19.49|              5.4|           1.3|              2.85|                         63|     265.0|  23.9|                   435|            0.76|     39.0|                        20|              0.02|                        29.9|                            47|  63.2|         96|
|        353|Death    |Male   |      137.0|            0.0|                  159|    30.1|         0.1|            23.7|             41|          2.6|           95.1|          64.7|                   106.6|       37.0|                  15.55|                                       370| 11.30|             0.34|                 3.47|             0.00|             14.79|             11.6|           2.2|              2.84|                         38|     331.0|  17.5|                   704|            0.40|     34.6|                       165|              0.02|                        39.5|                            41|  85.6|         73|
|        354|Death    |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        355|Death    |Male   |      178.0|            0.0|                  104|    33.1|         0.1|            12.6|            158|          6.4|           82.0|          66.5|                    91.3|       51.3|                   6.87|                                       347|  7.30|             0.79|                 5.62|             0.00|              5.63|              5.3|          11.5|              4.52|                         95|     335.0|  21.7|                  1594|            0.44|     33.4|                        46|              0.01|                        31.7|                            35|  68.8|         97|
|        356|Death    |Male   |      133.0|            0.0|                  123|    27.7|         0.2|            13.4|             75|          6.1|           90.8|          54.2|                    93.1|       40.6|                  34.60|                                       328| 15.60|             0.53|               195.00|             0.00|             16.46|              8.4|           2.9|              3.86|                         14|     419.0|  13.4|                   426|            1.10|     26.5|                        81|              0.03|                        30.5|                            15|  68.5|         94|
|        357|Death    |Male   |      146.0|            0.1|                  113|    21.3|         0.1|            16.2|            159|          2.8|           95.7|          54.7|                    94.7|       41.4|                  16.69|                                       353| 11.80|             0.22|                 4.37|             0.01|             15.98|             11.3|           1.3|              2.91|                         44|     199.0|  18.4|                   699|            0.46|     33.4|                        49|              0.02|                        33.4|                            27|  86.3|         78|
|        358|Death    |Male   |      127.0|            0.0|                  113|    28.2|         0.2|             9.2|             95|          2.9|           79.8|          67.9|                    91.3|       39.0|                   5.56|                                       326| 19.20|             0.95|                 4.27|             0.00|              4.44|              4.0|          17.1|              2.45|                        266|     536.0|  28.2|                   830|            0.16|     39.7|                       194|              0.01|                        29.7|                            62|  43.4|        147|
|        359|Death    |Male   |         NA|             NA|                   NA|      NA|          NA|              NA|             NA|           NA|             NA|            NA|                      NA|         NA|                     NA|                                        NA|    NA|               NA|                   NA|               NA|                NA|               NA|            NA|                NA|                         NA|        NA|    NA|                    NA|              NA|       NA|                        NA|                NA|                          NA|                            NA|    NA|         NA|
|        360|Death    |Male   |      166.0|            0.0|                  132|    28.2|         0.1|            27.4|             74|          2.6|           94.0|          70.0|                    99.6|       47.1|                  20.09|                                       352| 20.80|             0.67|                 4.73|             0.00|             18.87|             20.5|           3.3|              3.36|                        713|     556.0|  17.0|                  1346|            0.53|     41.8|                       141|              0.02|                        35.1|                           744|  25.5|        213|
|        361|Death    |Male   |      110.0|            0.0|                  102|    30.6|         0.2|            13.5|            113|          1.9|           91.7|          75.7|                    88.2|       32.1|                   5.79|                                       343|  8.80|             0.36|                 3.64|             0.00|              5.31|              6.6|           6.2|              2.65|                         14|      71.3|  23.6|                   535|            0.11|     45.1|                        22|              0.01|                        30.2|                            15| 115.5|         48|
|        362|Death    |Male   |      130.0|            0.0|                  127|    23.0|         0.0|           174.1|             76|          0.8|           98.2|          56.8|                    85.5|       37.1|                  10.20|                                       350| 55.30|             0.10|                 4.34|             0.00|             10.02|            167.0|           1.0|              3.28|                        176|    1103.0|  10.6|                  1416|            0.08|     33.8|                       139|              0.00|                        30.0|                           119|   9.7|        489|
|        363|Death    |Female |      131.0|            0.1|                   90|    28.9|         0.2|            12.2|            254|          1.2|           94.8|          69.8|                    83.7|       37.4|                  12.99|                                       350|  4.90|             0.48|                 4.47|             0.01|             12.31|              7.3|           3.7|              2.45|                         23|     205.1|  24.5|                   794|            0.16|     40.9|                       114|              0.03|                        29.3|                            22| 104.2|         49|
|        364|Death    |Female |      116.0|            0.0|                   23|    24.0|         0.1|             7.3|             74|          2.3|           94.2|          57.5|                    89.6|       33.6|                  11.50|                                       345|  4.10|             0.39|                 3.75|             0.00|             10.83|              2.0|           3.4|              2.57|                         21|      79.0|  27.4|                   491|            0.27|     33.5|                        39|              0.01|                        30.9|                             6| 114.9|         38|
|        365|Death    |Male   |      167.0|            0.0|                   87|    35.8|         0.4|            22.0|             72|          3.3|           91.9|          71.1|                   100.6|       52.3|                  15.33|                                       319| 10.10|             0.67|                 5.20|             0.00|             14.09|              9.6|           4.4|              5.57|                        117|     331.0|  10.4|                  1867|            0.51|     35.3|                        72|              0.06|                        32.1|                            78|  66.8|        100|
|        366|Death    |Female |       83.0|            0.0|                  131|    13.6|         0.1|            11.1|             11|          3.9|           92.6|          60.2|                    88.5|       23.9|                  16.46|                                       347| 11.30|             0.56|                 2.70|             0.00|             15.25|              5.1|           3.4|              3.31|                         39|     299.0|  21.8|                   702|            0.64|     34.4|                        25|              0.01|                        30.7|                            38|  96.4|         49|
|        367|Death    |Male   |      111.0|            0.0|                  251|    30.0|         0.1|            10.5|            494|          2.4|           94.1|          72.7|                    93.4|       32.6|                  12.45|                                       340|  7.70|             0.42|                 3.49|             0.00|             11.72|              7.8|           3.4|              4.23|                         77|     230.0|  19.3|                   506|            0.30|     42.7|                       732|              0.01|                        31.8|                           114| 104.2|         66|
|        368|Death    |Male   |      113.0|            0.0|                  153|    27.6|         0.2|            17.9|            119|          1.8|           96.1|          72.3|                    86.7|       29.3|                  31.80|                                       386| 38.20|             0.59|                 3.38|             0.00|             30.60|              7.9|           1.9|              0.66|                         40|     565.0|  24.0|                  1514|            0.56|     44.7|                        37|              0.05|                        33.4|                            25|   6.4|        703|
|        369|Death    |Male   |       93.0|            0.0|                  107|    31.3|         0.0|             4.0|            120|          0.6|           93.9|          65.4|                    95.3|       28.1|                   4.69|                                       331| 57.20|             0.26|                 2.95|             0.00|              4.40|              2.4|           5.5|              3.42|                         49|     473.0|  10.0|                   568|            0.03|     34.1|                        59|              0.00|                        31.5|                            16|   5.1|        809|
|        370|Death    |Female |       98.0|            0.0|                   68|    23.6|         0.1|            10.6|             21|          4.1|           93.1|          51.1|                    90.4|       30.0|                  12.04|                                       327| 13.00|             0.33|                 3.32|             0.00|             11.21|              6.4|           2.7|              3.63|                         53|     132.0|  17.1|                   905|            0.49|     27.5|                        62|              0.01|                        29.5|                            24|  47.8|         98|
|        371|Death    |Male   |      143.0|            0.0|                   65|    30.7|         0.0|             7.1|            149|          5.5|           79.5|          64.0|                    87.9|       41.3|                   5.12|                                       346|  5.30|             0.77|                 4.70|             0.00|              4.07|              4.2|          15.0|              3.28|                         50|     203.0|  24.8|                   573|            0.28|     33.3|                        27|              0.00|                        30.4|                            31|  88.6|         81|
|        372|Death    |Male   |      102.0|            0.2|                  180|    16.7|         0.2|            35.2|             48|          0.9|           98.1|          46.3|                    86.9|       29.1|                  28.04|                                       351| 31.30|             0.17|                 3.35|             0.06|             27.48|             31.1|           0.6|              2.41|                        332|     531.0|  13.0|                   607|            0.26|     29.6|                       135|              0.07|                        30.4|                           121|  12.9|        364|
|        373|Death    |Female |      100.0|            0.1|                  141|    23.9|         0.1|             9.0|            136|          4.5|           89.0|          60.8|                    80.9|       29.3|                  12.13|                                       341|  5.20|             0.76|                 3.62|             0.01|             10.80|              4.9|           6.3|              2.57|                         31|      84.2|  25.4|                   702|            0.55|     36.9|                        39|              0.01|                        27.6|                             9| 101.3|         47|
|        374|Death    |Male   |      121.0|            0.0|                  143|    27.4|         0.3|            26.0|             54|          2.9|           94.5|          57.5|                    88.1|       34.7|                  14.49|                                       349| 18.40|             0.33|                 3.94|             0.00|             13.75|             18.4|           2.3|              2.79|                        756|     458.0|  22.0|                  1867|            0.42|     30.1|                       176|              0.04|                        30.7|                          1508|  69.4|        118|
|        375|Death    |Male   |      155.0|            0.0|                   94|    21.3|         0.6|            60.0|             17|          1.0|           91.7|          49.0|                    97.1|       47.7|                   8.25|                                       325|  9.80|             0.55|                 4.91|             0.00|              7.57|             52.8|           6.7|              2.20|                         25|      57.0|  19.7|                   783|            0.08|     27.7|                        33|              0.05|                        31.6|                            17|  88.6|         77|

</div>

```r
#Taking these that has at most 19 NA's (18 it's common lowest level of NA among all columns)

#IMO that's already cleaned right now

cleaned_df <- last_test_df %>% ungroup() %>% select(outcome, age, gender, hemoglobin, `eosinophils(%)`, `Alkaline phosphatase`, albumin, `basophil(%)`, `Total bilirubin`, `Platelet count`, `monocytes(%)`, `neutrophils(%)`, `total protein`, `mean corpuscular volume`, hematocrit, `White blood cell count`, `mean corpuscular hemoglobin concentration`, Urea, `lymphocyte count`, `Red blood cell count`, `Eosinophil count`, `neutrophils count`, `Direct bilirubin`, `(%)lymphocyte`, `Total cholesterol`, `aspartate aminotransferase`, `Uric acid`, `HCO3-`, `Lactate dehydrogenase`, `monocytes count`, globulin, `γ-glutamyl transpeptidase`, `basophil count(#)`, `mean corpuscular hemoglobin`, `glutamic-pyruvic transaminase`, eGFR, creatinine) %>% filter_all(function(x) !is.na(x)) 

cleaned_df %>% summary()
```

```
##      outcome         age           gender      hemoglobin    eosinophils(%)  
##  Death   :162   Min.   :18.00   Female:147   Min.   :  6.4   Min.   :0.0000  
##  Survival:192   1st Qu.:46.25   Male  :207   1st Qu.:112.0   1st Qu.:0.0000  
##                 Median :62.00                Median :125.0   Median :0.2000  
##                 Mean   :58.88                Mean   :124.4   Mean   :0.8997  
##                 3rd Qu.:70.00                3rd Qu.:138.0   3rd Qu.:1.5000  
##                 Max.   :95.00                Max.   :178.0   Max.   :8.6000  
##  Alkaline phosphatase    albumin       basophil(%)     Total bilirubin  
##  Min.   : 17.00       Min.   :13.60   Min.   :0.0000   Min.   :  2.800  
##  1st Qu.: 54.00       1st Qu.:28.20   1st Qu.:0.1000   1st Qu.:  7.225  
##  Median : 71.50       Median :33.20   Median :0.2000   Median : 10.600  
##  Mean   : 85.75       Mean   :32.66   Mean   :0.2633   Mean   : 16.563  
##  3rd Qu.: 98.00       3rd Qu.:37.60   3rd Qu.:0.4000   3rd Qu.: 16.175  
##  Max.   :620.00       Max.   :47.60   Max.   :1.7000   Max.   :295.400  
##  Platelet count   monocytes(%)    neutrophils(%)  total protein  
##  Min.   : -1.0   Min.   : 0.600   Min.   : 1.90   Min.   :31.80  
##  1st Qu.:113.0   1st Qu.: 2.925   1st Qu.:61.83   1st Qu.:61.20  
##  Median :189.0   Median : 6.200   Median :78.00   Median :65.95  
##  Mean   :192.2   Mean   : 6.513   Mean   :75.50   Mean   :65.24  
##  3rd Qu.:257.0   3rd Qu.: 8.900   3rd Qu.:91.97   3rd Qu.:70.40  
##  Max.   :554.0   Max.   :53.000   Max.   :98.90   Max.   :83.40  
##  mean corpuscular volume   hematocrit    White blood cell count
##  Min.   : 62.30          Min.   :15.60   Min.   :   0.710      
##  1st Qu.: 87.03          1st Qu.:33.00   1st Qu.:   5.112      
##  Median : 90.45          Median :36.30   Median :   7.940      
##  Mean   : 90.64          Mean   :36.58   Mean   :  19.009      
##  3rd Qu.: 94.28          3rd Qu.:40.17   3rd Qu.:  13.185      
##  Max.   :117.60          Max.   :52.30   Max.   :1726.600      
##  mean corpuscular hemoglobin concentration      Urea       lymphocyte count
##  Min.   :286.0                             Min.   : 1.70   Min.   : 0.050  
##  1st Qu.:332.0                             1st Qu.: 3.80   1st Qu.: 0.520  
##  Median :342.0                             Median : 5.40   Median : 0.975  
##  Mean   :342.2                             Mean   : 9.81   Mean   : 1.159  
##  3rd Qu.:349.0                             3rd Qu.:11.47   3rd Qu.: 1.540  
##  Max.   :488.0                             Max.   :68.40   Max.   :33.690  
##  Red blood cell count Eosinophil count  neutrophils count Direct bilirubin 
##  Min.   :  0.100      Min.   :0.00000   Min.   : 0.320    Min.   :  1.600  
##  1st Qu.:  3.553      1st Qu.:0.00000   1st Qu.: 3.105    1st Qu.:  3.125  
##  Median :  4.100      Median :0.02000   Median : 5.380    Median :  4.800  
##  Mean   :  8.347      Mean   :0.05362   Mean   : 8.020    Mean   :  9.810  
##  3rd Qu.:  4.645      3rd Qu.:0.09000   3rd Qu.:11.307    3rd Qu.:  7.475  
##  Max.   :749.500      Max.   :0.46000   Max.   :32.220    Max.   :242.900  
##  (%)lymphocyte    Total cholesterol aspartate aminotransferase   Uric acid     
##  Min.   : 0.300   Min.   :0.100     Min.   :   6.00            Min.   :  52.0  
##  1st Qu.: 4.125   1st Qu.:2.950     1st Qu.:  19.00            1st Qu.: 198.2  
##  Median :13.850   Median :3.720     Median :  25.00            Median : 260.0  
##  Mean   :16.824   Mean   :3.745     Mean   :  54.16            Mean   : 294.3  
##  3rd Qu.:27.500   3rd Qu.:4.370     3rd Qu.:  41.00            3rd Qu.: 346.4  
##  Max.   :48.500   Max.   :7.300     Max.   :1858.00            Max.   :1176.0  
##      HCO3-       Lactate dehydrogenase monocytes count      globulin    
##  Min.   : 6.30   Min.   : 110.0        Min.   : 0.0100   Min.   :10.10  
##  1st Qu.:20.93   1st Qu.: 197.0        1st Qu.: 0.3100   1st Qu.:28.93  
##  Median :23.90   Median : 273.5        Median : 0.4300   Median :32.40  
##  Mean   :23.22   Mean   : 476.1        Mean   : 0.5956   Mean   :32.55  
##  3rd Qu.:26.38   3rd Qu.: 615.2        3rd Qu.: 0.6100   3rd Qu.:35.70  
##  Max.   :33.80   Max.   :1867.0        Max.   :39.9200   Max.   :49.20  
##  γ-glutamyl transpeptidase basophil count(#) mean corpuscular hemoglobin
##  Min.   :  7.00            Min.   :0.00000   Min.   :20.80              
##  1st Qu.: 21.00            1st Qu.:0.01000   1st Qu.:29.70              
##  Median : 33.00            Median :0.02000   Median :30.90              
##  Mean   : 49.54            Mean   :0.02136   Mean   :31.02              
##  3rd Qu.: 55.00            3rd Qu.:0.03000   3rd Qu.:32.20              
##  Max.   :732.00            Max.   :0.12000   Max.   :50.80              
##  glutamic-pyruvic transaminase      eGFR          creatinine    
##  Min.   :   5.00               Min.   :  2.00   Min.   :  14.0  
##  1st Qu.:  17.00               1st Qu.: 66.90   1st Qu.:  58.0  
##  Median :  26.00               Median : 89.50   Median :  74.0  
##  Mean   :  42.74               Mean   : 82.12   Mean   : 117.5  
##  3rd Qu.:  41.75               3rd Qu.:105.00   3rd Qu.:  97.0  
##  Max.   :1508.00               Max.   :206.90   Max.   :1497.0
```

```r
set.seed(23)
inTraining <- 
    createDataPartition(
        y = cleaned_df$outcome,
        p = .75,
        list = FALSE)


training <- cleaned_df[ inTraining,]
testing  <- cleaned_df[-inTraining,]

rfGrid <- expand.grid(mtry = 10:30)
ctrl <- trainControl(
    method = "repeatedcv",
    classProbs = TRUE,
    number = 2,
    repeats = 5)

set.seed(23)
fit <- train(outcome ~ .,
             data = training,
             method = "rf",
             metric = "ROC",
             preProc = c("center", "scale"),
             trControl = ctrl,
             tuneGrid = rfGrid,
             ntree = 30)
rfClasses <- predict(fit, newdata = testing)
confusionMatrix(data = rfClasses, testing$outcome)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction Death Survival
##   Death       40        3
##   Survival     0       45
##                                           
##                Accuracy : 0.9659          
##                  95% CI : (0.9036, 0.9929)
##     No Information Rate : 0.5455          
##     P-Value [Acc > NIR] : <2e-16          
##                                           
##                   Kappa : 0.9317          
##                                           
##  Mcnemar's Test P-Value : 0.2482          
##                                           
##             Sensitivity : 1.0000          
##             Specificity : 0.9375          
##          Pos Pred Value : 0.9302          
##          Neg Pred Value : 1.0000          
##              Prevalence : 0.4545          
##          Detection Rate : 0.4545          
##    Detection Prevalence : 0.4886          
##       Balanced Accuracy : 0.9688          
##                                           
##        'Positive' Class : Death           
## 
```

```r
#df %>% group_by(PATIENT_ID) %>% summarise_at(vars(`Hypersensitive cardiac troponinI`:creatinine), function(x) ))
```
