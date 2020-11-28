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
summarise(PATIENT_ID = sum(PATIENT_ID, na.rm = TRUE), `Total records` = n()) 

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

patients_df <- df %>% group_by(PATIENT_ID, outcome, `Admission time`, `Discharge time`, gender, age) %>% summarise(missing_test=is.na(RE_DATE)) %>%
  group_by(PATIENT_ID, outcome, `Admission time`, `Discharge time`, gender, age) %>%
  summarise(test_provided = ifelse(missing_test, 0, 1)) %>%
  group_by(PATIENT_ID, outcome, `Admission time`, `Discharge time`, gender, age) %>%
  summarise(`Total blood tests`=sum(test_provided)) %>%
  mutate(`Days in hospital` = ceiling(difftime(`Discharge time`, `Admission time`, units = "days")))

last_test_df <- df %>% 
  group_by(PATIENT_ID, outcome, gender, age) %>% 
  fill_(names(df)) %>% 
  fill_(names(df), "up") %>% 
  summarise_at(vars(`Hypersensitive cardiac troponinI`:creatinine), function(x) last(x,order_by = is.na(x)))


cleaned_df <- last_test_df %>% ungroup() %>% select(PATIENT_ID, outcome, age, gender, hemoglobin, `eosinophils(%)`, `Alkaline phosphatase`, albumin, `basophil(%)`, `Total bilirubin`, `Platelet count`, `monocytes(%)`, `neutrophils(%)`, `total protein`, `mean corpuscular volume`, hematocrit, `White blood cell count`, `mean corpuscular hemoglobin concentration`, Urea, `lymphocyte count`, `Red blood cell count`, `Eosinophil count`, `neutrophils count`, `Direct bilirubin`, `(%)lymphocyte`, `Total cholesterol`, `aspartate aminotransferase`, `Uric acid`, `HCO3-`, `Lactate dehydrogenase`, `monocytes count`, globulin, `γ-glutamyl transpeptidase`, `basophil count(#)`, `mean corpuscular hemoglobin`, `glutamic-pyruvic transaminase`, eGFR, creatinine) %>% filter_all(function(x) !is.na(x)) 
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

#rfhzboblej .gt_table {
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

#rfhzboblej .gt_heading {
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

#rfhzboblej .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rfhzboblej .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rfhzboblej .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rfhzboblej .gt_col_headings {
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

#rfhzboblej .gt_col_heading {
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

#rfhzboblej .gt_column_spanner_outer {
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

#rfhzboblej .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rfhzboblej .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rfhzboblej .gt_column_spanner {
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

#rfhzboblej .gt_group_heading {
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

#rfhzboblej .gt_empty_group_heading {
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

#rfhzboblej .gt_from_md > :first-child {
  margin-top: 0;
}

#rfhzboblej .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rfhzboblej .gt_row {
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

#rfhzboblej .gt_stub {
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

#rfhzboblej .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rfhzboblej .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#rfhzboblej .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rfhzboblej .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rfhzboblej .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rfhzboblej .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rfhzboblej .gt_footnotes {
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

#rfhzboblej .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#rfhzboblej .gt_sourcenotes {
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

#rfhzboblej .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#rfhzboblej .gt_left {
  text-align: left;
}

#rfhzboblej .gt_center {
  text-align: center;
}

#rfhzboblej .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rfhzboblej .gt_font_normal {
  font-weight: normal;
}

#rfhzboblej .gt_font_bold {
  font-weight: bold;
}

#rfhzboblej .gt_font_italic {
  font-style: italic;
}

#rfhzboblej .gt_super {
  font-size: 65%;
}

#rfhzboblej .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="rfhzboblej" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Overall</strong>, N = 375<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Death</strong>, N = 174<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Survival</strong>, N = 201<sup class="gt_footnote_marks">1</sup></th>
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
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
      <td class="gt_row gt_center">151 (40%)</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">48 (28%)</td>
      <td class="gt_row gt_center">103 (51%)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
      <td class="gt_row gt_center">224 (60%)</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">126 (72%)</td>
      <td class="gt_row gt_center">98 (49%)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Total blood tests</td>
      <td class="gt_row gt_center">16 (9, 21)</td>
      <td class="gt_row gt_center">375</td>
      <td class="gt_row gt_center">14 (7, 24)</td>
      <td class="gt_row gt_center">16 (12, 20)</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Days in hospital</td>
      <td class="gt_row gt_center">10 (5, 16)</td>
      <td class="gt_row gt_center">375</td>
      <td class="gt_row gt_center">6 (3, 10)</td>
      <td class="gt_row gt_center">14 (10, 18)</td>
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

## Removed data


```r
patients_df %>% filter(`Total blood tests`==0) 
```

<div class="kable-table">

| PATIENT_ID|outcome  |Admission time      |Discharge time      |gender | age| Total blood tests|Days in hospital |
|----------:|:--------|:-------------------|:-------------------|:------|---:|-----------------:|:----------------|
|        187|Survival |2020-02-17 18:56:09 |2020-02-20 20:55:31 |Male   |  44|                 0|4 days           |
|        189|Survival |2020-02-10 04:37:30 |2020-02-10 13:54:23 |Male   |  61|                 0|1 days           |
|        192|Survival |2020-02-16 17:14:30 |2020-02-16 21:05:17 |Male   |  34|                 0|1 days           |
|        197|Survival |2020-02-10 05:01:15 |2020-02-11 15:40:41 |Male   |  67|                 0|2 days           |
|        200|Survival |2020-02-16 04:41:21 |2020-02-16 15:26:13 |Male   |  25|                 0|1 days           |
|        201|Survival |2020-02-17 21:30:07 |2020-02-20 13:05:11 |Male   |  39|                 0|3 days           |
|        253|Death    |2020-02-13 21:05:54 |2020-02-14 11:00:05 |Male   |  51|                 0|1 days           |
|        268|Death    |2020-02-14 11:46:36 |2020-02-15 10:15:28 |Male   |  69|                 0|1 days           |
|        285|Death    |2020-01-31 23:20:40 |2020-02-01 03:16:34 |Male   |  63|                 0|1 days           |
|        289|Death    |2020-02-01 02:12:05 |2020-02-01 10:54:57 |Male   |  63|                 0|1 days           |
|        311|Death    |2020-02-11 23:45:15 |2020-02-15 09:02:41 |Female |  77|                 0|4 days           |
|        347|Death    |2020-02-11 22:25:20 |2020-02-15 10:03:32 |Female |  80|                 0|4 days           |
|        354|Death    |2020-02-03 21:22:41 |2020-02-04 01:03:11 |Male   |  57|                 0|1 days           |
|        359|Death    |2020-02-11 01:42:48 |2020-02-14 09:38:13 |Male   |  65|                 0|4 days           |

</div>

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

#llmgqhurfo .gt_table {
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

#llmgqhurfo .gt_heading {
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

#llmgqhurfo .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#llmgqhurfo .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#llmgqhurfo .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#llmgqhurfo .gt_col_headings {
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

#llmgqhurfo .gt_col_heading {
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

#llmgqhurfo .gt_column_spanner_outer {
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

#llmgqhurfo .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#llmgqhurfo .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#llmgqhurfo .gt_column_spanner {
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

#llmgqhurfo .gt_group_heading {
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

#llmgqhurfo .gt_empty_group_heading {
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

#llmgqhurfo .gt_from_md > :first-child {
  margin-top: 0;
}

#llmgqhurfo .gt_from_md > :last-child {
  margin-bottom: 0;
}

#llmgqhurfo .gt_row {
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

#llmgqhurfo .gt_stub {
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

#llmgqhurfo .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#llmgqhurfo .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#llmgqhurfo .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#llmgqhurfo .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#llmgqhurfo .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#llmgqhurfo .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#llmgqhurfo .gt_footnotes {
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

#llmgqhurfo .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#llmgqhurfo .gt_sourcenotes {
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

#llmgqhurfo .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#llmgqhurfo .gt_left {
  text-align: left;
}

#llmgqhurfo .gt_center {
  text-align: center;
}

#llmgqhurfo .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#llmgqhurfo .gt_font_normal {
  font-weight: normal;
}

#llmgqhurfo .gt_font_bold {
  font-weight: bold;
}

#llmgqhurfo .gt_font_italic {
  font-style: italic;
}

#llmgqhurfo .gt_super {
  font-size: 65%;
}

#llmgqhurfo .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="llmgqhurfo" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
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

Finding correlation between outcome and other variables. Ignoring `Admission time`, `PATIENT_ID`, `RE_DATE` and `Discharge time`, and dropping `outcome`, which correlation with itself is equal to `1.0`.


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
#TODO: Take for correlation cleaned_df and check if it's consistent with predictive model

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

#oacdjucfqi .gt_table {
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

#oacdjucfqi .gt_heading {
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

#oacdjucfqi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#oacdjucfqi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#oacdjucfqi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oacdjucfqi .gt_col_headings {
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

#oacdjucfqi .gt_col_heading {
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

#oacdjucfqi .gt_column_spanner_outer {
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

#oacdjucfqi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#oacdjucfqi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#oacdjucfqi .gt_column_spanner {
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

#oacdjucfqi .gt_group_heading {
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

#oacdjucfqi .gt_empty_group_heading {
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

#oacdjucfqi .gt_from_md > :first-child {
  margin-top: 0;
}

#oacdjucfqi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#oacdjucfqi .gt_row {
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

#oacdjucfqi .gt_stub {
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

#oacdjucfqi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oacdjucfqi .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#oacdjucfqi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#oacdjucfqi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#oacdjucfqi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#oacdjucfqi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#oacdjucfqi .gt_footnotes {
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

#oacdjucfqi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#oacdjucfqi .gt_sourcenotes {
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

#oacdjucfqi .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#oacdjucfqi .gt_left {
  text-align: left;
}

#oacdjucfqi .gt_center {
  text-align: center;
}

#oacdjucfqi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#oacdjucfqi .gt_font_normal {
  font-weight: normal;
}

#oacdjucfqi .gt_font_bold {
  font-weight: bold;
}

#oacdjucfqi .gt_font_italic {
  font-style: italic;
}

#oacdjucfqi .gt_super {
  font-size: 65%;
}

#oacdjucfqi .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="oacdjucfqi" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
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
                         {"material":{"color":"#000000","alpha":1,"lit":true,"ambient":"#000000","specular":"#FFFFFF","emission":"#000000","shininess":50,"smooth":true,"front":"filled","back":"filled","size":3,"lwd":1,"fog":false,"point_antialias":false,"line_antialias":false,"texture":null,"textype":"rgb","texmipmap":false,"texminfilter":"linear","texmagfilter":"linear","texenvmap":false,"depth_mask":true,"depth_test":"less","isTransparent":false,"polygon_offset":[0,0]},"rootSubscene":6,"objects":{"12":{"id":12,"type":"spheres","material":{},"vertices":[[68.36,22.72,34.48],[80.625,13.65,35.95],[67.16666,26.5,35.76667],[71.15,18.25,34],[59.76667,30.66667,39.85],[60,27.25,35],[80.63333,12.36667,34.33333],[47.26,39.58,39.72],[59.16667,27.53333,37.7],[74.1,17.1,33.63334],[71.95,21.85,41.05],[46.56667,40.46667,45.76667],[62.96667,27.63333,35.6],[72.575,21.525,31.85],[51.7,38.15,37],[59.7,26.3,40.1],[58,34.15,41.23333],[73.3,16.53333,30.9],[50.85,32.225,31.675],[75.8,18.175,33.55],[66.65,24.2,38.65],[56.85,34.3,39.1],[80.06667,13.16667,28.2],[57.025,36,32.475],[51.68,38.64,39.24],[48.925,39.625,38.925],[62.4,29.95,41.95],[47.2,41.05,43.15],[75.9,16.1,33.9],[79.5,12.9,40.1],[50.7,37,37.5],[60.63334,30.8,40.93333],[84.74,9.96,32.5],[52.35,31.25,33.8],[56.45,31.35,32.5],[71.95,14.75,33.7],[77.55,8.95,30.1],[88.65,4.9,39],[67.65,20.6,36.85],[48.35,39.05,43.75],[67.6,18.2,31.7],[69.95,14.7,32.35],[54.85,28.25,41.1],[66.2,19.6,38.1],[55.2,35.1,41],[56.5,32.6,46.3],[59.8,30.3,38.8],[50.35,39.2,40.6],[60.9,25.5,37.9],[60.7,27.5,39.9],[70.2,18,43.2],[45,37.4,37.9],[65.3,24.7,38.6],[61.9,27.6,38.4],[61.8,31.6,39.5],[51.7,39.4,40.1],[52.3,37.6,41.5],[61.95,25.25,29.475],[70.7,24.76667,32.4],[53.675,38.85,40.66667],[70,18.8,36.03333],[73.375,18.35,31.825],[67.96,23.2,35.9],[59.11666,29.15,36.45],[65.3,29,34.16667],[55.23333,35.86666,38.76667],[72.7,19.1,36.15],[71.38,19.42,36.4],[61.4,26.66667,37.8],[49.75,39.675,37.05],[1.8,52.35,33.2],[76.1,14.5,32.1],[61.83333,23.33333,36.2],[52.85,34.75,42.9],[80.75,14.45,34.775],[58.7,33.7,40.3],[71.72,19.82,34.3],[73.1,20.6,35.85],[79.8,10.6,34.55],[56.2,34.05,33.85],[79.62,12.94,32.225],[72.25,20.425,36.7],[44.33333,43.3,38.7],[55.35,32.8,36.45],[74.4,16.83333,38.63334],[75.4,17.9,37.6],[65.1,23.9,43.9],[49.5,33.7,40],[82.3,12.65,40.53333],[70.56667,18.73333,39.85],[64.875,26.7,33.8],[53.55,38.9,38.7],[81.25,11.3,36.05],[68.25,23.25,33.8],[42.75,41.75,42.8],[72,20.4,41.66667],[48.5,38.85,37.5],[62.75,24.6,36.96667],[61,28.6,40.75],[52.2,34.4,40.6],[66.1,25.96667,31.15],[70.93333,14.66667,36.2],[67.06667,21.8,31.325],[67.7,23.6,32.8],[70.63333,15.56667,32.8],[74.1,15.8,35.5],[49.95,38.25,38.75],[54.46667,35.8,35.8],[73.25,17.45,35.4],[80.15,11,33.65],[57.7,28.1,36.2],[68.3,21.3,42.45],[78.5,14.7,30.6],[75.4,14.675,35.45],[55.75,33.85,40.25],[56.15,30.35,38.65],[70,18,34.2],[75.8,19.6,41.9],[70,18.5,34.83333],[69.65,22.25,36.65],[80.675,13.525,29.875],[65.7,25.9,33.15],[61,27.175,35.56667],[59.56667,29.8,38.83333],[88.1,6.4,31.4],[67.3,25.175,35.95],[53.1,36.5,32.4],[54.3,35.4,32],[67.9,21.75,35.1],[54.4,30.8,35.45],[63,26.6,35.2],[64.3,26.6,38.35],[59.36666,31.1,36.1],[66.85,22.7,33.15],[56.65,32.75,36.75],[70.74,19.8,34.175],[76.7,16.65,37.275],[71.73333,20.76667,33.475],[66.93333,24.33333,38.66667],[63.35,26.4,35.8],[66.9,24.4,39.4],[68.9,20.53333,33.5],[70.36667,21.43333,34.9],[76.1,17.7,36.45],[69,24.33333,36.2],[71.575,21.9,33.725],[60.1,29.4,35],[66,22.6,35],[54.6,31.9,36.95],[46.65,40.05,39.5],[40.1,48.1,38.5],[60.2,29.5,30.1],[75.45,19.05,36.3],[54,37.13334,41.25],[69.3,18.83333,40.2],[75.75,16.45,29.9],[56.1,30.15,35.95],[68.4,21.1,40.76667],[68.1,23.2,41.9],[65.23333,26.96667,37],[63.1,28.8,36.15],[53.52,36.44,36.68],[66.3,23,41.75],[64.7,22.06667,37.53333],[62.275,27.1,38.3],[44.4,44.15,38.65],[48.2,38.15,36.26],[72.4,19.575,34.025],[61.76667,26.03333,37.5],[66.63333,26.16667,39.43333],[67.85,23,42.1],[78.58572,14.92857,36.77143],[80.8,13.55,40.8],[62.6,25.3,39.7],[82.7,13,33.95],[68.5,25.45,38.55],[55.8,34.3,40],[56.1,31.1,31.6],[57.1,30.5,38.7],[73.1,14.7,35.3],[63.5,28.4,41.3],[87.85,5.45,36.25],[85.5,8.9,29.6],[71.2,20.7,45.5],[61.5,30.45,42.53333],[60.9,30.6,44.5],[95.1,4.1,32.7],[87.13333,5.633333,29.52],[51.4,40,36],[91.01667,6.683333,29.01667],[70.5,18.2,33.1],[86.2,8.225,30.875],[87.66666,5.966667,31.1],[79.33334,10.26667,29.23333],[92.91666,5.15,28.92],[94.33334,3.233333,26.5],[86.8,6.25,31.8],[89.15,7.15,31],[88.5,7.7,27.1],[83.1,12.7,29.6],[93.72,2.88,26.42],[86.8,9.2,29.5],[64.5,15.2,35.5],[18.2,44.3,35.4],[95.7,2.1,26.7],[95.8,2.3,29.6],[95.5,2,26.9],[92.18,3.92,23.9],[88.57143,5.385714,26.575],[96.6,1.35,27.65],[90.725,4.075,30.65],[93.76667,2.2,25.575],[68.18,22.58,23.075],[91.2,3.7,27.5],[89.15,6.683333,29.08333],[92.2,4.7,34.8],[93.55,4.7,30.15],[87.1,8.6,24.1],[93,4.1,28.03333],[93.04,3.62,27.18333],[88.6,8.8,28.2],[94.54285,2.657143,24.67143],[69.45,16.75,38.65],[93.91111,2.877778,28.07778],[91.625,4.85,32.84],[88.4,5.333333,28.16667],[86.9,10,27.9],[94.78,1.58,22.31667],[91.6,4.575,27.6],[87.68,5.68,25.93333],[95.56667,1.466667,24.78],[89.425,6.125,30.7],[88.12,7.88,31.3],[94.88571,3.657143,24.86],[94.53333,3.166667,28.73333],[80.3,12.2,29.55],[86.375,5,28.925],[82.675,13.325,25.26667],[91.73333,6.5,30.96667],[81.75,14.625,31.16667],[95.225,1.95,29.2625],[90.55556,6.655556,26.46],[95.25,1.95,32.15],[94.33334,3.966667,22.76],[83.45,11.025,33.65],[88.85,5.5,26.675],[92.69167,2.825,23.50769],[64.7,26.7,29.4],[81.65,12.45,29.5],[88.85,4.1,31.1],[88.3,7.266667,31.3],[91.2,4.5,28.1],[84.26,11.98,26.92],[86.9,4.9,28.4],[90.7,3.833333,29],[90.1,7.1,21.5],[96.1,1.4,34.85],[90.3,4.7,31.9],[90.13333,5.833333,30.975],[95.3,2.25,28.8],[89.3,4.2,31.4],[88.4,9.6,26.55],[95.6,3.9,28.35],[92.96667,3.733333,28.9],[96.6,1.8,29.9],[90.7,6.52,27],[96.36667,1.3,28.05],[85.5,10.48,25.6],[93.15,1.5,23.95],[87.4,9.1,31.1],[92.1,3.766667,27.43333],[90.975,4.5,26.825],[79.8,9.65,32.25],[92.925,4.2,25.35],[84.2,13,35.2],[92.6,4.8,34.1],[92,3.2,25.6],[92.22222,5.433333,25.52222],[67.9,20.6,36.4],[87.3,7.15,31.25],[90.7,2.7,35.8],[64.9,18.5,32.4],[92.2,5.3,31.8],[87.9,10.7,29.3],[93.7,2.05,33.6],[89.9,6.65,31.15],[93.83334,3.333333,23.91429],[95.46667,2.066667,32.3],[95.825,1.525,31.1],[91.6,6.1,27.9],[94.48,2.2,23.21],[97.1,1.95,28.13333],[89.575,7.375,31.8],[93,3,23.8],[78.85,15.9,31.4],[93.1,2.7,26.9],[81,11.1,34.2],[55.1,25,23.2],[81.4,7.5,30.1],[77.3,18.9,30.3],[94.5,3.1,30.1],[85.7625,10.9125,23.65],[90.6,4.6,27],[93.85,3.65,18.55],[93,3.6,28.5],[98.8,0.15,23.2],[86.8,7.1,33],[93.65,2.85,24.4],[96,2.7,31.5],[87.12,4.74,32.64],[87.4,10.4,34.2],[89.95,5.15,24.2],[80.9,10.3,24.8],[92.75,3.4,30.95],[93.5,3.3,37.3],[95.8,1.9,31.9],[88.1,6.6,27.6],[91.7,5.35,27.4],[85.775,7.3,37.9],[93.2,3.95,29.26667],[96.1,2.1,34],[75.2,9.7,39.5],[89.26,4.78,31.25],[87.45715,8.1,25.25714],[92.8,4.033333,24.45],[93.3,2.65,30.85],[92.775,3.425,24],[70.7,15.6,38.6],[96.1,2,26.2],[94.2,3.56,22.18333],[94,3.8,29.1],[91,6.2,29.1],[94.9,1.3,31.7],[95.1,2.2,30.1],[82,11.5,33.1],[89.46667,4.8,26.66667],[93.05,2.4,22.7],[87.6,9.566667,25.6],[94,3.3,28.2],[90.4,5.9,32.06667],[96.375,1.8,26.2],[94.8,3.7,28.9],[94.375,3.725,25.6],[91.9,4.4,35.8],[91.6,5.225,22.7],[94.1,3.4,30],[89.24,7.3,30.1],[93.9,5.5,31.3],[91.25,4.375,22.93333],[79.5,15,30.7],[93.56,1.65,21.12222],[89,6.3,23.9],[95.15,2.3,27.26667],[87.7,8.3,26.93333]],"colors":[[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0,0.5450981,0.5450981,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1],[0.282353,0.4627451,1,1]],"radii":[[1]],"centers":[[68.36,22.72,34.48],[80.625,13.65,35.95],[67.16666,26.5,35.76667],[71.15,18.25,34],[59.76667,30.66667,39.85],[60,27.25,35],[80.63333,12.36667,34.33333],[47.26,39.58,39.72],[59.16667,27.53333,37.7],[74.1,17.1,33.63334],[71.95,21.85,41.05],[46.56667,40.46667,45.76667],[62.96667,27.63333,35.6],[72.575,21.525,31.85],[51.7,38.15,37],[59.7,26.3,40.1],[58,34.15,41.23333],[73.3,16.53333,30.9],[50.85,32.225,31.675],[75.8,18.175,33.55],[66.65,24.2,38.65],[56.85,34.3,39.1],[80.06667,13.16667,28.2],[57.025,36,32.475],[51.68,38.64,39.24],[48.925,39.625,38.925],[62.4,29.95,41.95],[47.2,41.05,43.15],[75.9,16.1,33.9],[79.5,12.9,40.1],[50.7,37,37.5],[60.63334,30.8,40.93333],[84.74,9.96,32.5],[52.35,31.25,33.8],[56.45,31.35,32.5],[71.95,14.75,33.7],[77.55,8.95,30.1],[88.65,4.9,39],[67.65,20.6,36.85],[48.35,39.05,43.75],[67.6,18.2,31.7],[69.95,14.7,32.35],[54.85,28.25,41.1],[66.2,19.6,38.1],[55.2,35.1,41],[56.5,32.6,46.3],[59.8,30.3,38.8],[50.35,39.2,40.6],[60.9,25.5,37.9],[60.7,27.5,39.9],[70.2,18,43.2],[45,37.4,37.9],[65.3,24.7,38.6],[61.9,27.6,38.4],[61.8,31.6,39.5],[51.7,39.4,40.1],[52.3,37.6,41.5],[61.95,25.25,29.475],[70.7,24.76667,32.4],[53.675,38.85,40.66667],[70,18.8,36.03333],[73.375,18.35,31.825],[67.96,23.2,35.9],[59.11666,29.15,36.45],[65.3,29,34.16667],[55.23333,35.86666,38.76667],[72.7,19.1,36.15],[71.38,19.42,36.4],[61.4,26.66667,37.8],[49.75,39.675,37.05],[1.8,52.35,33.2],[76.1,14.5,32.1],[61.83333,23.33333,36.2],[52.85,34.75,42.9],[80.75,14.45,34.775],[58.7,33.7,40.3],[71.72,19.82,34.3],[73.1,20.6,35.85],[79.8,10.6,34.55],[56.2,34.05,33.85],[79.62,12.94,32.225],[72.25,20.425,36.7],[44.33333,43.3,38.7],[55.35,32.8,36.45],[74.4,16.83333,38.63334],[75.4,17.9,37.6],[65.1,23.9,43.9],[49.5,33.7,40],[82.3,12.65,40.53333],[70.56667,18.73333,39.85],[64.875,26.7,33.8],[53.55,38.9,38.7],[81.25,11.3,36.05],[68.25,23.25,33.8],[42.75,41.75,42.8],[72,20.4,41.66667],[48.5,38.85,37.5],[62.75,24.6,36.96667],[61,28.6,40.75],[52.2,34.4,40.6],[66.1,25.96667,31.15],[70.93333,14.66667,36.2],[67.06667,21.8,31.325],[67.7,23.6,32.8],[70.63333,15.56667,32.8],[74.1,15.8,35.5],[49.95,38.25,38.75],[54.46667,35.8,35.8],[73.25,17.45,35.4],[80.15,11,33.65],[57.7,28.1,36.2],[68.3,21.3,42.45],[78.5,14.7,30.6],[75.4,14.675,35.45],[55.75,33.85,40.25],[56.15,30.35,38.65],[70,18,34.2],[75.8,19.6,41.9],[70,18.5,34.83333],[69.65,22.25,36.65],[80.675,13.525,29.875],[65.7,25.9,33.15],[61,27.175,35.56667],[59.56667,29.8,38.83333],[88.1,6.4,31.4],[67.3,25.175,35.95],[53.1,36.5,32.4],[54.3,35.4,32],[67.9,21.75,35.1],[54.4,30.8,35.45],[63,26.6,35.2],[64.3,26.6,38.35],[59.36666,31.1,36.1],[66.85,22.7,33.15],[56.65,32.75,36.75],[70.74,19.8,34.175],[76.7,16.65,37.275],[71.73333,20.76667,33.475],[66.93333,24.33333,38.66667],[63.35,26.4,35.8],[66.9,24.4,39.4],[68.9,20.53333,33.5],[70.36667,21.43333,34.9],[76.1,17.7,36.45],[69,24.33333,36.2],[71.575,21.9,33.725],[60.1,29.4,35],[66,22.6,35],[54.6,31.9,36.95],[46.65,40.05,39.5],[40.1,48.1,38.5],[60.2,29.5,30.1],[75.45,19.05,36.3],[54,37.13334,41.25],[69.3,18.83333,40.2],[75.75,16.45,29.9],[56.1,30.15,35.95],[68.4,21.1,40.76667],[68.1,23.2,41.9],[65.23333,26.96667,37],[63.1,28.8,36.15],[53.52,36.44,36.68],[66.3,23,41.75],[64.7,22.06667,37.53333],[62.275,27.1,38.3],[44.4,44.15,38.65],[48.2,38.15,36.26],[72.4,19.575,34.025],[61.76667,26.03333,37.5],[66.63333,26.16667,39.43333],[67.85,23,42.1],[78.58572,14.92857,36.77143],[80.8,13.55,40.8],[62.6,25.3,39.7],[82.7,13,33.95],[68.5,25.45,38.55],[55.8,34.3,40],[56.1,31.1,31.6],[57.1,30.5,38.7],[73.1,14.7,35.3],[63.5,28.4,41.3],[87.85,5.45,36.25],[85.5,8.9,29.6],[71.2,20.7,45.5],[61.5,30.45,42.53333],[60.9,30.6,44.5],[95.1,4.1,32.7],[87.13333,5.633333,29.52],[51.4,40,36],[91.01667,6.683333,29.01667],[70.5,18.2,33.1],[86.2,8.225,30.875],[87.66666,5.966667,31.1],[79.33334,10.26667,29.23333],[92.91666,5.15,28.92],[94.33334,3.233333,26.5],[86.8,6.25,31.8],[89.15,7.15,31],[88.5,7.7,27.1],[83.1,12.7,29.6],[93.72,2.88,26.42],[86.8,9.2,29.5],[64.5,15.2,35.5],[18.2,44.3,35.4],[95.7,2.1,26.7],[95.8,2.3,29.6],[95.5,2,26.9],[92.18,3.92,23.9],[88.57143,5.385714,26.575],[96.6,1.35,27.65],[90.725,4.075,30.65],[93.76667,2.2,25.575],[68.18,22.58,23.075],[91.2,3.7,27.5],[89.15,6.683333,29.08333],[92.2,4.7,34.8],[93.55,4.7,30.15],[87.1,8.6,24.1],[93,4.1,28.03333],[93.04,3.62,27.18333],[88.6,8.8,28.2],[94.54285,2.657143,24.67143],[69.45,16.75,38.65],[93.91111,2.877778,28.07778],[91.625,4.85,32.84],[88.4,5.333333,28.16667],[86.9,10,27.9],[94.78,1.58,22.31667],[91.6,4.575,27.6],[87.68,5.68,25.93333],[95.56667,1.466667,24.78],[89.425,6.125,30.7],[88.12,7.88,31.3],[94.88571,3.657143,24.86],[94.53333,3.166667,28.73333],[80.3,12.2,29.55],[86.375,5,28.925],[82.675,13.325,25.26667],[91.73333,6.5,30.96667],[81.75,14.625,31.16667],[95.225,1.95,29.2625],[90.55556,6.655556,26.46],[95.25,1.95,32.15],[94.33334,3.966667,22.76],[83.45,11.025,33.65],[88.85,5.5,26.675],[92.69167,2.825,23.50769],[64.7,26.7,29.4],[81.65,12.45,29.5],[88.85,4.1,31.1],[88.3,7.266667,31.3],[91.2,4.5,28.1],[84.26,11.98,26.92],[86.9,4.9,28.4],[90.7,3.833333,29],[90.1,7.1,21.5],[96.1,1.4,34.85],[90.3,4.7,31.9],[90.13333,5.833333,30.975],[95.3,2.25,28.8],[89.3,4.2,31.4],[88.4,9.6,26.55],[95.6,3.9,28.35],[92.96667,3.733333,28.9],[96.6,1.8,29.9],[90.7,6.52,27],[96.36667,1.3,28.05],[85.5,10.48,25.6],[93.15,1.5,23.95],[87.4,9.1,31.1],[92.1,3.766667,27.43333],[90.975,4.5,26.825],[79.8,9.65,32.25],[92.925,4.2,25.35],[84.2,13,35.2],[92.6,4.8,34.1],[92,3.2,25.6],[92.22222,5.433333,25.52222],[67.9,20.6,36.4],[87.3,7.15,31.25],[90.7,2.7,35.8],[64.9,18.5,32.4],[92.2,5.3,31.8],[87.9,10.7,29.3],[93.7,2.05,33.6],[89.9,6.65,31.15],[93.83334,3.333333,23.91429],[95.46667,2.066667,32.3],[95.825,1.525,31.1],[91.6,6.1,27.9],[94.48,2.2,23.21],[97.1,1.95,28.13333],[89.575,7.375,31.8],[93,3,23.8],[78.85,15.9,31.4],[93.1,2.7,26.9],[81,11.1,34.2],[55.1,25,23.2],[81.4,7.5,30.1],[77.3,18.9,30.3],[94.5,3.1,30.1],[85.7625,10.9125,23.65],[90.6,4.6,27],[93.85,3.65,18.55],[93,3.6,28.5],[98.8,0.15,23.2],[86.8,7.1,33],[93.65,2.85,24.4],[96,2.7,31.5],[87.12,4.74,32.64],[87.4,10.4,34.2],[89.95,5.15,24.2],[80.9,10.3,24.8],[92.75,3.4,30.95],[93.5,3.3,37.3],[95.8,1.9,31.9],[88.1,6.6,27.6],[91.7,5.35,27.4],[85.775,7.3,37.9],[93.2,3.95,29.26667],[96.1,2.1,34],[75.2,9.7,39.5],[89.26,4.78,31.25],[87.45715,8.1,25.25714],[92.8,4.033333,24.45],[93.3,2.65,30.85],[92.775,3.425,24],[70.7,15.6,38.6],[96.1,2,26.2],[94.2,3.56,22.18333],[94,3.8,29.1],[91,6.2,29.1],[94.9,1.3,31.7],[95.1,2.2,30.1],[82,11.5,33.1],[89.46667,4.8,26.66667],[93.05,2.4,22.7],[87.6,9.566667,25.6],[94,3.3,28.2],[90.4,5.9,32.06667],[96.375,1.8,26.2],[94.8,3.7,28.9],[94.375,3.725,25.6],[91.9,4.4,35.8],[91.6,5.225,22.7],[94.1,3.4,30],[89.24,7.3,30.1],[93.9,5.5,31.3],[91.25,4.375,22.93333],[79.5,15,30.7],[93.56,1.65,21.12222],[89,6.3,23.9],[95.15,2.3,27.26667],[87.7,8.3,26.93333]],"ignoreExtent":false,"flags":3},"14":{"id":14,"type":"text","material":{"lit":false},"vertices":[[50.3,-9.763639,13.27982]],"colors":[[0,0,0,1]],"texts":[["Neutrophils(%)"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[50.3,-9.763639,13.27982]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"15":{"id":15,"type":"text","material":{"lit":false},"vertices":[[-16.6219,26.25,13.27982]],"colors":[[0,0,0,1]],"texts":[["Lymphocyte(%)"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-16.6219,26.25,13.27982]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"16":{"id":16,"type":"text","material":{"lit":false},"vertices":[[-16.6219,-9.763639,32.425]],"colors":[[0,0,0,1]],"texts":[["Albumin"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-16.6219,-9.763639,32.425]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"flags":2064},"18":{"id":18,"type":"quads","material":{"lit":false,"back":"lines","uri":"data:image/png;base64,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"},"vertices":[[-1,-1,1],[1,-1,1],[1,1,1],[-1,1,1]],"colors":[[1,1,1,1]],"texcoords":[[0,0],[1,0],[1,1],[0,1]],"centers":[[0,0,1]],"ignoreExtent":true,"flags":8198},"10":{"id":10,"type":"light","vertices":[[0,0,1]],"colors":[[1,1,1,1],[1,1,1,1],[1,1,1,1]],"viewpoint":true,"finite":false},"9":{"id":9,"type":"background","material":{"fog":true},"colors":[[0.2980392,0.2980392,0.2980392,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"11":{"id":11,"type":"background","material":{"lit":false,"back":"lines"},"colors":[[1,1,1,1]],"centers":[[0,0,0]],"sphere":false,"fogtype":"none","flags":0},"17":{"id":17,"type":"background","material":{"lit":false,"back":"lines","uriId":18},"colors":[[1,1,1,1]],"ids":[[18]],"types":[["quads"]],"centers":[[0,0,0]],"objects":[{"id":{"id":18},"type":{"type":"quads"},"material":{"color":"#FFFFFF","lit":false,"texture":"/tmp/RtmpBTc3go/file13561b623a78.png","back":"lines"},"vertices":[[-1,-1,1],[1,-1,1],[1,1,1],[-1,1,1]],"colors":[[1,1,1,1]],"texcoords":[[0,0],[1,0],[1,1],[0,1]],"centers":[[0,0,1]],"ignoreExtent":true}],"sphere":false,"fogtype":"none","flags":0},"13":{"id":13,"type":"bboxdeco","material":{"front":"lines","back":"lines"},"vertices":[[20,"NA","NA"],[40,"NA","NA"],[60,"NA","NA"],[80,"NA","NA"],[100,"NA","NA"],["NA",0,"NA"],["NA",10,"NA"],["NA",20,"NA"],["NA",30,"NA"],["NA",40,"NA"],["NA",50,"NA"],["NA","NA",20],["NA","NA",25],["NA","NA",30],["NA","NA",35],["NA","NA",40],["NA","NA",45]],"colors":[[0,0,0,1]],"draw_front":true,"newIds":[26,27,28,29,30,31,32]},"6":{"id":6,"type":"subscene","par3d":{"antialias":8,"FOV":30,"ignoreExtent":false,"listeners":6,"mouseMode":{"left":"trackball","right":"zoom","middle":"fov","wheel":"pull"},"observer":[0,0,286.4459],"modelMatrix":[[0.6761268,0,0,-34.00918],[0,0.4297156,2.220868,-83.29167],[0,-1.180634,0.8083299,-281.6644],[0,0,0,1]],"projMatrix":[[3.732051,0,0,0],[0,3.732051,0,0],[0,0,-3.863703,-1032.604],[0,0,-1,0]],"skipRedraw":false,"userMatrix":[[1,0,0,0],[0,0.3420201,0.9396926,0],[0,-0.9396926,0.3420201,0],[0,0,0,1]],"userProjection":[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]],"scale":[0.6761268,1.256404,2.363398],"viewport":{"x":0,"y":0,"width":1,"height":1},"zoom":1,"bbox":[0.3209875,100.279,-0.6459222,53.14592,18.12688,46.72312],"windowRect":[492,1241,748,1497],"family":"sans","font":1,"cex":1,"useFreeType":true,"fontname":"/home/pienta/R/x86_64-pc-linux-gnu-library/3.4/rgl/fonts/FreeSans.ttf","maxClipPlanes":8,"glVersion":3,"activeSubscene":0},"embeddings":{"viewport":"replace","projection":"replace","model":"replace","mouse":"replace"},"objects":[17,13,12,14,15,16,10,26,27,28,29,30,31,32],"subscenes":[],"flags":2643},"26":{"id":26,"type":"lines","material":{"lit":false},"vertices":[[20,-1.4528,17.69794],[100,-1.4528,17.69794],[20,-1.4528,17.69794],[20,-2.83794,16.96158],[40,-1.4528,17.69794],[40,-2.83794,16.96158],[60,-1.4528,17.69794],[60,-2.83794,16.96158],[80,-1.4528,17.69794],[80,-2.83794,16.96158],[100,-1.4528,17.69794],[100,-2.83794,16.96158]],"colors":[[0,0,0,1]],"centers":[[60,-1.4528,17.69794],[20,-2.14537,17.32976],[40,-2.14537,17.32976],[60,-2.14537,17.32976],[80,-2.14537,17.32976],[100,-2.14537,17.32976]],"ignoreExtent":true,"origId":13,"flags":64},"27":{"id":27,"type":"text","material":{"lit":false},"vertices":[[20,-5.60822,15.48888],[40,-5.60822,15.48888],[60,-5.60822,15.48888],[80,-5.60822,15.48888],[100,-5.60822,15.48888]],"colors":[[0,0,0,1]],"texts":[["20"],["40"],["60"],["80"],["100"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[20,-5.60822,15.48888],[40,-5.60822,15.48888],[60,-5.60822,15.48888],[80,-5.60822,15.48888],[100,-5.60822,15.48888]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"origId":13,"flags":2064},"28":{"id":28,"type":"lines","material":{"lit":false},"vertices":[[-1.178383,0,17.69794],[-1.178383,50,17.69794],[-1.178383,0,17.69794],[-3.752302,0,16.96158],[-1.178383,10,17.69794],[-3.752302,10,16.96158],[-1.178383,20,17.69794],[-3.752302,20,16.96158],[-1.178383,30,17.69794],[-3.752302,30,16.96158],[-1.178383,40,17.69794],[-3.752302,40,16.96158],[-1.178383,50,17.69794],[-3.752302,50,16.96158]],"colors":[[0,0,0,1]],"centers":[[-1.178383,25,17.69794],[-2.465343,0,17.32976],[-2.465343,10,17.32976],[-2.465343,20,17.32976],[-2.465343,30,17.32976],[-2.465343,40,17.32976],[-2.465343,50,17.32976]],"ignoreExtent":true,"origId":13,"flags":64},"29":{"id":29,"type":"text","material":{"lit":false},"vertices":[[-8.900141,0,15.48888],[-8.900141,10,15.48888],[-8.900141,20,15.48888],[-8.900141,30,15.48888],[-8.900141,40,15.48888],[-8.900141,50,15.48888]],"colors":[[0,0,0,1]],"texts":[["0"],["10"],["20"],["30"],["40"],["50"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-8.900141,0,15.48888],[-8.900141,10,15.48888],[-8.900141,20,15.48888],[-8.900141,30,15.48888],[-8.900141,40,15.48888],[-8.900141,50,15.48888]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"origId":13,"flags":2064},"30":{"id":30,"type":"lines","material":{"lit":false},"vertices":[[-1.178383,-1.4528,20],[-1.178383,-1.4528,45],[-1.178383,-1.4528,20],[-3.752302,-2.83794,20],[-1.178383,-1.4528,25],[-3.752302,-2.83794,25],[-1.178383,-1.4528,30],[-3.752302,-2.83794,30],[-1.178383,-1.4528,35],[-3.752302,-2.83794,35],[-1.178383,-1.4528,40],[-3.752302,-2.83794,40],[-1.178383,-1.4528,45],[-3.752302,-2.83794,45]],"colors":[[0,0,0,1]],"centers":[[-1.178383,-1.4528,32.5],[-2.465343,-2.14537,20],[-2.465343,-2.14537,25],[-2.465343,-2.14537,30],[-2.465343,-2.14537,35],[-2.465343,-2.14537,40],[-2.465343,-2.14537,45]],"ignoreExtent":true,"origId":13,"flags":64},"31":{"id":31,"type":"text","material":{"lit":false},"vertices":[[-8.900141,-5.60822,20],[-8.900141,-5.60822,25],[-8.900141,-5.60822,30],[-8.900141,-5.60822,35],[-8.900141,-5.60822,40],[-8.900141,-5.60822,45]],"colors":[[0,0,0,1]],"texts":[["20"],["25"],["30"],["35"],["40"],["45"]],"cex":[[1]],"adj":[[0.5,0.5]],"centers":[[-8.900141,-5.60822,20],[-8.900141,-5.60822,25],[-8.900141,-5.60822,30],[-8.900141,-5.60822,35],[-8.900141,-5.60822,40],[-8.900141,-5.60822,45]],"family":[["sans"]],"font":[[1]],"ignoreExtent":true,"origId":13,"flags":2064},"32":{"id":32,"type":"lines","material":{"lit":false},"vertices":[[-1.178383,-1.4528,17.69794],[-1.178383,53.9528,17.69794],[-1.178383,-1.4528,47.15206],[-1.178383,53.9528,47.15206],[-1.178383,-1.4528,17.69794],[-1.178383,-1.4528,47.15206],[-1.178383,53.9528,17.69794],[-1.178383,53.9528,47.15206],[-1.178383,-1.4528,17.69794],[101.7784,-1.4528,17.69794],[-1.178383,-1.4528,47.15206],[101.7784,-1.4528,47.15206],[-1.178383,53.9528,17.69794],[101.7784,53.9528,17.69794],[-1.178383,53.9528,47.15206],[101.7784,53.9528,47.15206],[101.7784,-1.4528,17.69794],[101.7784,53.9528,17.69794],[101.7784,-1.4528,47.15206],[101.7784,53.9528,47.15206],[101.7784,-1.4528,17.69794],[101.7784,-1.4528,47.15206],[101.7784,53.9528,17.69794],[101.7784,53.9528,47.15206]],"colors":[[0,0,0,1]],"centers":[[-1.178383,26.25,17.69794],[-1.178383,26.25,47.15206],[-1.178383,-1.4528,32.425],[-1.178383,53.9528,32.425],[50.3,-1.4528,17.69794],[50.3,-1.4528,47.15206],[50.3,53.9528,17.69794],[50.3,53.9528,47.15206],[101.7784,26.25,17.69794],[101.7784,26.25,47.15206],[101.7784,-1.4528,32.425],[101.7784,53.9528,32.425]],"ignoreExtent":true,"origId":13,"flags":64}},"snapshot":"data:image/png;base64,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","width":600,"height":600,"sphereVerts":{"vb":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1950903,-0.3826834,-0.5555702,-0.7071068,-0.8314696,-0.9238795,-0.9807853,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1],[0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1950903,-0.3826834,-0.5555702,-0.7071068,-0.8314696,-0.9238795,-0.9807853,-1,-0.9807853,-0.9238795,-0.8314696,-0.7071068,-0.5555702,-0.3826834,-0.1950903,-0,-0,-0.18024,-0.3535534,-0.51328,-0.6532815,-0.7681778,-0.8535534,-0.9061274,-0.9238795,-0.9061274,-0.8535534,-0.7681778,-0.6532815,-0.51328,-0.3535534,-0.18024,-0,-0,-0.1379497,-0.2705981,-0.3928475,-0.5,-0.5879378,-0.6532815,-0.6935199,-0.7071068,-0.6935199,-0.6532815,-0.5879378,-0.5,-0.3928475,-0.2705981,-0.1379497,-0,-0,-0.07465783,-0.1464466,-0.2126075,-0.2705981,-0.3181896,-0.3535534,-0.3753303,-0.3826834,-0.3753303,-0.3535534,-0.3181896,-0.2705981,-0.2126075,-0.1464466,-0.07465783,-0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.07465783,0.1464466,0.2126075,0.2705981,0.3181896,0.3535534,0.3753303,0.3826834,0.3753303,0.3535534,0.3181896,0.2705981,0.2126075,0.1464466,0.07465783,0,0,0.1379497,0.2705981,0.3928475,0.5,0.5879378,0.6532815,0.6935199,0.7071068,0.6935199,0.6532815,0.5879378,0.5,0.3928475,0.2705981,0.1379497,0,0,0.18024,0.3535534,0.51328,0.6532815,0.7681778,0.8535534,0.9061274,0.9238795,0.9061274,0.8535534,0.7681778,0.6532815,0.51328,0.3535534,0.18024,0,0,0.1950903,0.3826834,0.5555702,0.7071068,0.8314696,0.9238795,0.9807853,1,0.9807853,0.9238795,0.8314696,0.7071068,0.5555702,0.3826834,0.1950903,0]],"it":[[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270],[17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288],[18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270,271]],"material":[],"normals":null,"texcoords":[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.0625,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.125,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.1875,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.3125,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.4375,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.5625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.625,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.6875,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.8125,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.875,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,0.9375,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1,0,0.0625,0.125,0.1875,0.25,0.3125,0.375,0.4375,0.5,0.5625,0.625,0.6875,0.75,0.8125,0.875,0.9375,1]],"meshColor":"vertices"},"context":{"shiny":false,"rmarkdown":"html_document"},"crosstalk":{"key":[],"group":[],"id":[],"options":[]}});
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

## Common biomarkers' change in time


```r
biomarker_popularity <- df %>% group_by(PATIENT_ID, outcome) %>% summarise_at(vars(`Hypersensitive cardiac troponinI`:creatinine), function(x) sum(!is.na(x)))
biomarker_popularity$sum_of_single_test <- rowSums(biomarker_popularity %>% ungroup() %>% select(-PATIENT_ID, -outcome))

most_tested_death <- biomarker_popularity %>% select(sum_of_single_test, outcome, `neutrophils(%)`,  `(%)lymphocyte`, albumin) %>% filter(outcome=='Death') %>% arrange(desc(sum_of_single_test)) %>% head(10)
most_tested_survival <- biomarker_popularity %>% select(sum_of_single_test, outcome, `neutrophils(%)`,  `(%)lymphocyte`, albumin) %>% filter(outcome=='Survival') %>% arrange(desc(sum_of_single_test)) %>% head(10)

foo <- rbind(most_tested_death, most_tested_survival)

patients_tests <- merge(foo %>% ungroup() %>% select(PATIENT_ID), df %>% ungroup() %>% select(PATIENT_ID, RE_DATE,`neutrophils(%)`,  `(%)lymphocyte`, albumin, outcome),  by="PATIENT_ID") %>% filter(!is.na(`neutrophils(%)`) |  !is.na(`(%)lymphocyte`) | !is.na(albumin))

neutrophils_seq <- patients_tests %>% filter(!is.na(`neutrophils(%)`)) 

neutrophils_seq %>% mutate(PATIENT_ID=as.factor(PATIENT_ID)) %>%
  ggplot( aes(x=RE_DATE, y=`neutrophils(%)`, group=PATIENT_ID, color=PATIENT_ID)) +
    geom_line() +
    geom_point() +
  facet_grid(rows=vars(outcome)) +
    ggtitle("Neutrophils (%) during patient hospitalization") +
    theme_ipsum() +
    ylab("Neutrophils (%)") +
    transition_reveal(RE_DATE)
```

![](COVID-19-analysis_files/figure-html/most_common_biomarkers-1.gif)<!-- -->

```r
#anim_save("neutrophils_seq.gif")

lymphocyte_seq <- patients_tests %>% filter(!is.na(`(%)lymphocyte`)) 

lymphocyte_seq %>% mutate(PATIENT_ID=as.factor(PATIENT_ID)) %>%
  ggplot( aes(x=RE_DATE, y=`(%)lymphocyte`, group=PATIENT_ID, color=PATIENT_ID)) +
    geom_line() +
    geom_point() +
  facet_grid(rows=vars(outcome)) +
    ggtitle("Lymphocyte (%) during patient hospitalization") +
    theme_ipsum() +
    ylab("Lymphocyte (%)") +
    transition_reveal(RE_DATE)
```

![](COVID-19-analysis_files/figure-html/most_common_biomarkers-2.gif)<!-- -->

```r
#anim_save("lymphocyte_seq.gif")

albumin_seq <- patients_tests %>% filter(!is.na(albumin)) 

albumin_seq %>% mutate(PATIENT_ID=as.factor(PATIENT_ID)) %>%
  ggplot( aes(x=RE_DATE, y=albumin, group=PATIENT_ID, color=PATIENT_ID)) +
    geom_line() +
    geom_point() +
  facet_grid(rows=vars(outcome)) +
    ggtitle("Albumin value during patient hospitalization") +
    theme_ipsum() +
    ylab("Albumin") +
    transition_reveal(RE_DATE)
```

![](COVID-19-analysis_files/figure-html/most_common_biomarkers-3.gif)<!-- -->

```r
#anim_save("albumin_seq.gif")
```

## Prediction model

In order to build an appropriate model, there is a need for the data cleaning. There shouldn't be any `NA` in the data set. Moreover, every considered patient should have all of considered biomarkers tested at least once. For building the model purpose, the last existing test of each biomarker for each patient was taken. As it was shown before, many biomarkers are unsuitable because of containing too many `NA` in theirs columns. Which means that they should be removed from the data set. Below is the list of remaining columns:

<ul>
<b>
  <li>outcome</li>
  <li>age</li>
  <li>gender</li>
  <li>hemoglobin</li>
  <li>eosinophils(%)</li>
  <li>Alkaline phosphatas</li>
  <li>albumin</li>
  <li>basophil(%)</li>
  <li>Total bilirubin</li>
  <li>Platelet count</li>
  <li>monocytes(%)</li>
  <li>neutrophils(%)</li>
  <li>total protein</li>
  <li>mean corpuscular volume</li>
  <li>hematocrit</li>
  <li>White blood cell count</li>
  <li>mean corpuscular hemoglobin concentration</li>
  <li>Urea</li>
  <li>lymphocyte count</li>
  <li>Red blood cell count</li>
  <li>Eosinophil count</li>
  <li>neutrophils count</li>
  <li>Direct bilirubin</li>
  <li>(%)lymphocyte</li>
  <li>Total cholesterol</li>
  <li>aspartate aminotransferase</li>
  <li>Uric acid</li>
  <li>HCO3-</li>
  <li>Lactate dehydrogenase</li>
  <li>monocytes count</li>
  <li>globulin</li>
  <li>γ-glutamyl transpeptidase</li>
  <li>basophil count(#)</li>
  <li>mean corpuscular hemoglobin</li>
  <li>glutamic-pyruvic transaminase</li>
  <li>eGFR</li>
  <li>creatinine</li>
  </b>
</ul>  

With condition of existing at least one of listed above biomarkers tests, 19 patients should be ignored in building model process.


```r
cleaned_df <- cleaned_df %>% select(-PATIENT_ID)
set.seed(23)
inTraining <- 
    createDataPartition(
        y = cleaned_df$outcome,
        p = .70,
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
##   Death       48        1
##   Survival     0       56
##                                           
##                Accuracy : 0.9905          
##                  95% CI : (0.9481, 0.9998)
##     No Information Rate : 0.5429          
##     P-Value [Acc > NIR] : <2e-16          
##                                           
##                   Kappa : 0.9808          
##                                           
##  Mcnemar's Test P-Value : 1               
##                                           
##             Sensitivity : 1.0000          
##             Specificity : 0.9825          
##          Pos Pred Value : 0.9796          
##          Neg Pred Value : 1.0000          
##              Prevalence : 0.4571          
##          Detection Rate : 0.4571          
##    Detection Prevalence : 0.4667          
##       Balanced Accuracy : 0.9912          
##                                           
##        'Positive' Class : Death           
## 
```


### Imporance of attributes


```r
varImp(fit)
```

```
## rf variable importance
## 
##   only 20 most important variables shown (out of 36)
## 
##                              Overall
## `Lactate dehydrogenase`      100.000
## `(%)lymphocyte`               56.749
## `neutrophils(%)`              31.971
## `Eosinophil count`            17.594
## albumin                       15.974
## `neutrophils count`           14.715
## `monocytes(%)`                14.162
## `Platelet count`              13.067
## `lymphocyte count`            10.040
## `aspartate aminotransferase`   9.925
## `eosinophils(%)`               6.399
## age                            3.518
## `Direct bilirubin`             3.514
## `Total cholesterol`            3.081
## `HCO3-`                        2.540
## Urea                           2.374
## eGFR                           2.171
## `γ-glutamyl transpeptidase`    1.967
## `Alkaline phosphatase`         1.607
## `total protein`                1.047
```