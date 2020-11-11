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
df$gender<-ifelse(df$gender==1, 'Male', 'Female') 
df <- df %>% mutate(gender = as.factor(gender))
df$outcome<-ifelse(df$outcome==1, 'Death', 'Survival')
df <- df %>% mutate(outcome = as.factor(outcome))
# TODO: Calculate how long hospital therapy took instead of all dates?
df <- df %>% mutate(RE_DATE = as.Date(RE_DATE))
df <- df %>% mutate(`Admission time` = as.Date(`Admission time`))
df <- df %>% mutate(`Discharge time` =  as.Date(`Discharge time`))
```

## Data summary

```r
#This below does not handle dates well
no_dates_df <- df %>% select(-c('Admission time', 'Discharge time', 'RE_DATE' ))
tbl_summary(
    no_dates_df,
    by = outcome
            ) %>%
    add_n() %>%
    add_p() %>%
    modify_header(label = "**Variable**") %>%
    bold_labels() 
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#gpyxzuaouv .gt_table {
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

#gpyxzuaouv .gt_heading {
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

#gpyxzuaouv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gpyxzuaouv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gpyxzuaouv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gpyxzuaouv .gt_col_headings {
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

#gpyxzuaouv .gt_col_heading {
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

#gpyxzuaouv .gt_column_spanner_outer {
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

#gpyxzuaouv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gpyxzuaouv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gpyxzuaouv .gt_column_spanner {
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

#gpyxzuaouv .gt_group_heading {
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

#gpyxzuaouv .gt_empty_group_heading {
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

#gpyxzuaouv .gt_from_md > :first-child {
  margin-top: 0;
}

#gpyxzuaouv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gpyxzuaouv .gt_row {
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

#gpyxzuaouv .gt_stub {
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

#gpyxzuaouv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gpyxzuaouv .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#gpyxzuaouv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gpyxzuaouv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gpyxzuaouv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gpyxzuaouv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gpyxzuaouv .gt_footnotes {
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

#gpyxzuaouv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#gpyxzuaouv .gt_sourcenotes {
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

#gpyxzuaouv .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#gpyxzuaouv .gt_left {
  text-align: left;
}

#gpyxzuaouv .gt_center {
  text-align: center;
}

#gpyxzuaouv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gpyxzuaouv .gt_font_normal {
  font-weight: normal;
}

#gpyxzuaouv .gt_font_bold {
  font-weight: bold;
}

#gpyxzuaouv .gt_font_italic {
  font-style: italic;
}

#gpyxzuaouv .gt_super {
  font-size: 65%;
}

#gpyxzuaouv .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="gpyxzuaouv" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Variable</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Death</strong>, N = 2,905<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Survival</strong>, N = 3,215<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">PATIENT_ID</td>
      <td class="gt_row gt_center">375</td>
      <td class="gt_row gt_center">288 (245, 332)</td>
      <td class="gt_row gt_center">101 (51, 151)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,731</td>
      <td class="gt_row gt_center">3,014</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">age</td>
      <td class="gt_row gt_center">6,120</td>
      <td class="gt_row gt_center">70 (63, 78)</td>
      <td class="gt_row gt_center">51 (37, 62)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">gender</td>
      <td class="gt_row gt_center">6,120</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Female</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">751 (26%)</td>
      <td class="gt_row gt_center">1,639 (51%)</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Male</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,154 (74%)</td>
      <td class="gt_row gt_center">1,576 (49%)</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Hypersensitive cardiac troponinI</td>
      <td class="gt_row gt_center">507</td>
      <td class="gt_row gt_center">70 (18, 631)</td>
      <td class="gt_row gt_center">3 (2, 7)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,572</td>
      <td class="gt_row gt_center">3,041</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">hemoglobin</td>
      <td class="gt_row gt_center">975</td>
      <td class="gt_row gt_center">123 (110, 135)</td>
      <td class="gt_row gt_center">127 (116, 138)</td>
      <td class="gt_row gt_center">0.002</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,438</td>
      <td class="gt_row gt_center">2,707</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Serum chloride</td>
      <td class="gt_row gt_center">975</td>
      <td class="gt_row gt_center">104 (100, 111)</td>
      <td class="gt_row gt_center">101 (99, 103)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,403</td>
      <td class="gt_row gt_center">2,742</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Prothrombin time</td>
      <td class="gt_row gt_center">662</td>
      <td class="gt_row gt_center">16.3 (15.0, 18.2)</td>
      <td class="gt_row gt_center">13.6 (13.1, 14.1)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,531</td>
      <td class="gt_row gt_center">2,927</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">procalcitonin</td>
      <td class="gt_row gt_center">459</td>
      <td class="gt_row gt_center">0.38 (0.14, 1.13)</td>
      <td class="gt_row gt_center">0.04 (0.02, 0.06)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,681</td>
      <td class="gt_row gt_center">2,980</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">eosinophils(%)</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.00 (0.00, 0.10)</td>
      <td class="gt_row gt_center">0.70 (0.00, 1.80)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 2 receptor</td>
      <td class="gt_row gt_center">268</td>
      <td class="gt_row gt_center">1,180 (807, 1,603)</td>
      <td class="gt_row gt_center">529 (400, 742)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,804</td>
      <td class="gt_row gt_center">3,048</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Alkaline phosphatase</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">83 (64, 123)</td>
      <td class="gt_row gt_center">60 (50, 75)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,448</td>
      <td class="gt_row gt_center">2,742</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">albumin</td>
      <td class="gt_row gt_center">934</td>
      <td class="gt_row gt_center">28 (24, 31)</td>
      <td class="gt_row gt_center">36 (34, 39)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,444</td>
      <td class="gt_row gt_center">2,742</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">basophil(%)</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.10 (0.10, 0.20)</td>
      <td class="gt_row gt_center">0.20 (0.10, 0.40)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 10</td>
      <td class="gt_row gt_center">267</td>
      <td class="gt_row gt_center">11 (6, 17)</td>
      <td class="gt_row gt_center">5 (5, 8)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,805</td>
      <td class="gt_row gt_center">3,048</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Total bilirubin</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">14 (10, 25)</td>
      <td class="gt_row gt_center">8 (6, 12)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,448</td>
      <td class="gt_row gt_center">2,742</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Platelet count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">112 (55, 174)</td>
      <td class="gt_row gt_center">229 (176, 290)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">monocytes(%)</td>
      <td class="gt_row gt_center">958</td>
      <td class="gt_row gt_center">3.0 (2.0, 4.7)</td>
      <td class="gt_row gt_center">8.2 (6.3, 10.0)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,719</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">antithrombin</td>
      <td class="gt_row gt_center">330</td>
      <td class="gt_row gt_center">80 (70, 92)</td>
      <td class="gt_row gt_center">93 (86, 103)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,688</td>
      <td class="gt_row gt_center">3,102</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 8</td>
      <td class="gt_row gt_center">268</td>
      <td class="gt_row gt_center">30 (18, 61)</td>
      <td class="gt_row gt_center">11 (7, 19)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,804</td>
      <td class="gt_row gt_center">3,048</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">indirect bilirubin</td>
      <td class="gt_row gt_center">906</td>
      <td class="gt_row gt_center">6.2 (4.2, 9.2)</td>
      <td class="gt_row gt_center">4.9 (3.4, 7.1)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,453</td>
      <td class="gt_row gt_center">2,761</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Red blood cell distribution width</td>
      <td class="gt_row gt_center">923</td>
      <td class="gt_row gt_center">13.20 (12.40, 14.40)</td>
      <td class="gt_row gt_center">12.20 (11.80, 12.80)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,451</td>
      <td class="gt_row gt_center">2,746</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">neutrophils(%)</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">92 (88, 95)</td>
      <td class="gt_row gt_center">66 (56, 76)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">total protein</td>
      <td class="gt_row gt_center">931</td>
      <td class="gt_row gt_center">62 (57, 68)</td>
      <td class="gt_row gt_center">68 (65, 72)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,448</td>
      <td class="gt_row gt_center">2,741</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Quantification of Treponema pallidum antibodies</td>
      <td class="gt_row gt_center">279</td>
      <td class="gt_row gt_center">0.06 (0.04, 0.07)</td>
      <td class="gt_row gt_center">0.05 (0.04, 0.07)</td>
      <td class="gt_row gt_center">0.031</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,777</td>
      <td class="gt_row gt_center">3,064</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Prothrombin activity</td>
      <td class="gt_row gt_center">659</td>
      <td class="gt_row gt_center">66 (56, 78)</td>
      <td class="gt_row gt_center">94 (88, 103)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,534</td>
      <td class="gt_row gt_center">2,927</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">HBsAg</td>
      <td class="gt_row gt_center">279</td>
      <td class="gt_row gt_center">0.01 (0.00, 0.02)</td>
      <td class="gt_row gt_center">0.00 (0.00, 0.01)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,776</td>
      <td class="gt_row gt_center">3,065</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">mean corpuscular volume</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">91.3 (87.1, 96.4)</td>
      <td class="gt_row gt_center">89.8 (86.8, 91.9)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">hematocrit</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">35.9 (32.5, 39.8)</td>
      <td class="gt_row gt_center">37.1 (34.3, 39.9)</td>
      <td class="gt_row gt_center">0.003</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">White blood cell count</td>
      <td class="gt_row gt_center">1,127</td>
      <td class="gt_row gt_center">12 (8, 17)</td>
      <td class="gt_row gt_center">6 (4, 8)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,393</td>
      <td class="gt_row gt_center">2,600</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Tumor necrosis factorα</td>
      <td class="gt_row gt_center">268</td>
      <td class="gt_row gt_center">11 (8, 17)</td>
      <td class="gt_row gt_center">8 (6, 10)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,804</td>
      <td class="gt_row gt_center">3,048</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">mean corpuscular hemoglobin concentration</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">342 (331, 350)</td>
      <td class="gt_row gt_center">343 (335, 350)</td>
      <td class="gt_row gt_center">0.066</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">fibrinogen</td>
      <td class="gt_row gt_center">566</td>
      <td class="gt_row gt_center">3.92 (2.44, 5.63)</td>
      <td class="gt_row gt_center">4.40 (3.56, 5.34)</td>
      <td class="gt_row gt_center">0.004</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,578</td>
      <td class="gt_row gt_center">2,976</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 1β</td>
      <td class="gt_row gt_center">268</td>
      <td class="gt_row gt_center">5.0 (5.0, 5.0)</td>
      <td class="gt_row gt_center">5.0 (5.0, 5.0)</td>
      <td class="gt_row gt_center">0.7</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,804</td>
      <td class="gt_row gt_center">3,048</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Urea</td>
      <td class="gt_row gt_center">936</td>
      <td class="gt_row gt_center">11 (7, 17)</td>
      <td class="gt_row gt_center">4 (3, 5)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,430</td>
      <td class="gt_row gt_center">2,754</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">lymphocyte count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.46 (0.31, 0.69)</td>
      <td class="gt_row gt_center">1.25 (0.87, 1.62)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">PH value</td>
      <td class="gt_row gt_center">384</td>
      <td class="gt_row gt_center">6.50 (6.00, 7.41)</td>
      <td class="gt_row gt_center">6.50 (6.00, 7.00)</td>
      <td class="gt_row gt_center">0.070</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,730</td>
      <td class="gt_row gt_center">3,006</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Red blood cell count</td>
      <td class="gt_row gt_center">1,127</td>
      <td class="gt_row gt_center">4.0 (3.6, 4.6)</td>
      <td class="gt_row gt_center">4.2 (3.8, 4.7)</td>
      <td class="gt_row gt_center">0.003</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,393</td>
      <td class="gt_row gt_center">2,600</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Eosinophil count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.00 (0.00, 0.01)</td>
      <td class="gt_row gt_center">0.03 (0.00, 0.09)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Corrected calcium</td>
      <td class="gt_row gt_center">914</td>
      <td class="gt_row gt_center">2.35 (2.27, 2.44)</td>
      <td class="gt_row gt_center">2.37 (2.27, 2.44)</td>
      <td class="gt_row gt_center">0.6</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,453</td>
      <td class="gt_row gt_center">2,753</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Serum potassium</td>
      <td class="gt_row gt_center">980</td>
      <td class="gt_row gt_center">4.60 (4.04, 5.27)</td>
      <td class="gt_row gt_center">4.28 (3.92, 4.62)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,401</td>
      <td class="gt_row gt_center">2,739</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">glucose</td>
      <td class="gt_row gt_center">775</td>
      <td class="gt_row gt_center">9.1 (6.9, 13.3)</td>
      <td class="gt_row gt_center">5.7 (5.0, 7.6)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,535</td>
      <td class="gt_row gt_center">2,810</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">neutrophils count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">10.8 (7.0, 15.2)</td>
      <td class="gt_row gt_center">3.5 (2.4, 5.2)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Direct bilirubin</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">8 (5, 14)</td>
      <td class="gt_row gt_center">4 (2, 5)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,448</td>
      <td class="gt_row gt_center">2,742</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Mean platelet volume</td>
      <td class="gt_row gt_center">862</td>
      <td class="gt_row gt_center">11.30 (10.70, 12.20)</td>
      <td class="gt_row gt_center">10.40 (9.90, 11.00)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,533</td>
      <td class="gt_row gt_center">2,725</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">ferritin</td>
      <td class="gt_row gt_center">283</td>
      <td class="gt_row gt_center">1,636 (928, 2,517)</td>
      <td class="gt_row gt_center">504 (235, 834)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,811</td>
      <td class="gt_row gt_center">3,026</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">RBC distribution width SD</td>
      <td class="gt_row gt_center">923</td>
      <td class="gt_row gt_center">43.7 (39.9, 48.5)</td>
      <td class="gt_row gt_center">39.5 (37.6, 41.4)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,451</td>
      <td class="gt_row gt_center">2,746</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Thrombin time</td>
      <td class="gt_row gt_center">566</td>
      <td class="gt_row gt_center">17.30 (15.80, 19.75)</td>
      <td class="gt_row gt_center">16.40 (15.60, 17.30)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,578</td>
      <td class="gt_row gt_center">2,976</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">(%)lymphocyte</td>
      <td class="gt_row gt_center">958</td>
      <td class="gt_row gt_center">4 (2, 7)</td>
      <td class="gt_row gt_center">24 (16, 33)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,719</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">HCV antibody quantification</td>
      <td class="gt_row gt_center">279</td>
      <td class="gt_row gt_center">0.07 (0.04, 0.11)</td>
      <td class="gt_row gt_center">0.06 (0.04, 0.08)</td>
      <td class="gt_row gt_center">0.022</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,776</td>
      <td class="gt_row gt_center">3,065</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">D-D dimer</td>
      <td class="gt_row gt_center">630</td>
      <td class="gt_row gt_center">19 (3, 21)</td>
      <td class="gt_row gt_center">1 (0, 1)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,549</td>
      <td class="gt_row gt_center">2,941</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Total cholesterol</td>
      <td class="gt_row gt_center">931</td>
      <td class="gt_row gt_center">3.32 (2.72, 3.88)</td>
      <td class="gt_row gt_center">3.93 (3.39, 4.48)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,448</td>
      <td class="gt_row gt_center">2,741</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">aspartate aminotransferase</td>
      <td class="gt_row gt_center">935</td>
      <td class="gt_row gt_center">38 (25, 59)</td>
      <td class="gt_row gt_center">21 (17, 29)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,444</td>
      <td class="gt_row gt_center">2,741</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Uric acid</td>
      <td class="gt_row gt_center">934</td>
      <td class="gt_row gt_center">245 (166, 374)</td>
      <td class="gt_row gt_center">240 (193, 304)</td>
      <td class="gt_row gt_center">0.7</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,432</td>
      <td class="gt_row gt_center">2,754</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">HCO3-</td>
      <td class="gt_row gt_center">934</td>
      <td class="gt_row gt_center">21.8 (18.8, 24.7)</td>
      <td class="gt_row gt_center">24.7 (22.8, 26.7)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,432</td>
      <td class="gt_row gt_center">2,754</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">calcium</td>
      <td class="gt_row gt_center">979</td>
      <td class="gt_row gt_center">2.00 (1.90, 2.08)</td>
      <td class="gt_row gt_center">2.17 (2.10, 2.25)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,400</td>
      <td class="gt_row gt_center">2,741</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Amino-terminal brain natriuretic peptide precursor(NT-proBNP)</td>
      <td class="gt_row gt_center">475</td>
      <td class="gt_row gt_center">1,467 (516, 4,578)</td>
      <td class="gt_row gt_center">64 (23, 166)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,586</td>
      <td class="gt_row gt_center">3,059</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Lactate dehydrogenase</td>
      <td class="gt_row gt_center">934</td>
      <td class="gt_row gt_center">593 (431, 840)</td>
      <td class="gt_row gt_center">220 (189, 278)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,444</td>
      <td class="gt_row gt_center">2,742</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">platelet large cell ratio</td>
      <td class="gt_row gt_center">862</td>
      <td class="gt_row gt_center">35 (30, 42)</td>
      <td class="gt_row gt_center">28 (23, 33)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,533</td>
      <td class="gt_row gt_center">2,725</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Interleukin 6</td>
      <td class="gt_row gt_center">272</td>
      <td class="gt_row gt_center">66 (30, 142)</td>
      <td class="gt_row gt_center">8 (2, 21)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,805</td>
      <td class="gt_row gt_center">3,043</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Fibrin degradation products</td>
      <td class="gt_row gt_center">330</td>
      <td class="gt_row gt_center">114 (18, 150)</td>
      <td class="gt_row gt_center">4 (4, 4)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,688</td>
      <td class="gt_row gt_center">3,102</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">monocytes count</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.36 (0.20, 0.58)</td>
      <td class="gt_row gt_center">0.43 (0.32, 0.58)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">PLT distribution width</td>
      <td class="gt_row gt_center">862</td>
      <td class="gt_row gt_center">13.60 (12.10, 15.93)</td>
      <td class="gt_row gt_center">11.70 (10.70, 13.00)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,533</td>
      <td class="gt_row gt_center">2,725</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">globulin</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">34.1 (30.2, 38.2)</td>
      <td class="gt_row gt_center">31.8 (29.5, 35.2)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,448</td>
      <td class="gt_row gt_center">2,742</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">γ-glutamyl transpeptidase</td>
      <td class="gt_row gt_center">930</td>
      <td class="gt_row gt_center">42 (27, 79)</td>
      <td class="gt_row gt_center">29 (19, 46)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,448</td>
      <td class="gt_row gt_center">2,742</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">International standard ratio</td>
      <td class="gt_row gt_center">659</td>
      <td class="gt_row gt_center">1.31 (1.17, 1.48)</td>
      <td class="gt_row gt_center">1.04 (0.99, 1.09)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,534</td>
      <td class="gt_row gt_center">2,927</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">basophil count(#)</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">0.010 (0.010, 0.030)</td>
      <td class="gt_row gt_center">0.010 (0.010, 0.020)</td>
      <td class="gt_row gt_center">0.009</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">2019-nCoV nucleic acid detection</td>
      <td class="gt_row gt_center">501</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">-1</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">57 (100%)</td>
      <td class="gt_row gt_center">444 (100%)</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,848</td>
      <td class="gt_row gt_center">2,771</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">mean corpuscular hemoglobin</td>
      <td class="gt_row gt_center">957</td>
      <td class="gt_row gt_center">31.20 (29.90, 32.70)</td>
      <td class="gt_row gt_center">30.70 (29.60, 31.90)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,443</td>
      <td class="gt_row gt_center">2,720</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">Activation of partial thromboplastin time</td>
      <td class="gt_row gt_center">568</td>
      <td class="gt_row gt_center">40 (36, 45)</td>
      <td class="gt_row gt_center">39 (35, 43)</td>
      <td class="gt_row gt_center">0.043</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,576</td>
      <td class="gt_row gt_center">2,976</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">High sensitivity C-reactive protein</td>
      <td class="gt_row gt_center">737</td>
      <td class="gt_row gt_center">114 (65, 191)</td>
      <td class="gt_row gt_center">7 (2, 35)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,548</td>
      <td class="gt_row gt_center">2,835</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">HIV antibody quantification</td>
      <td class="gt_row gt_center">278</td>
      <td class="gt_row gt_center">0.08 (0.07, 0.11)</td>
      <td class="gt_row gt_center">0.09 (0.08, 0.11)</td>
      <td class="gt_row gt_center">0.055</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,776</td>
      <td class="gt_row gt_center">3,066</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">serum sodium</td>
      <td class="gt_row gt_center">975</td>
      <td class="gt_row gt_center">142 (138, 148)</td>
      <td class="gt_row gt_center">140 (138, 141)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,403</td>
      <td class="gt_row gt_center">2,742</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">thrombocytocrit</td>
      <td class="gt_row gt_center">862</td>
      <td class="gt_row gt_center">0.15 (0.10, 0.21)</td>
      <td class="gt_row gt_center">0.24 (0.19, 0.30)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,533</td>
      <td class="gt_row gt_center">2,725</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">ESR</td>
      <td class="gt_row gt_center">383</td>
      <td class="gt_row gt_center">36 (16, 59)</td>
      <td class="gt_row gt_center">26 (13, 40)</td>
      <td class="gt_row gt_center">0.003</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,746</td>
      <td class="gt_row gt_center">2,991</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">glutamic-pyruvic transaminase</td>
      <td class="gt_row gt_center">931</td>
      <td class="gt_row gt_center">26 (18, 44)</td>
      <td class="gt_row gt_center">21 (15, 36)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,448</td>
      <td class="gt_row gt_center">2,741</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">eGFR</td>
      <td class="gt_row gt_center">936</td>
      <td class="gt_row gt_center">72 (43, 91)</td>
      <td class="gt_row gt_center">100 (85, 114)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,430</td>
      <td class="gt_row gt_center">2,754</td>
      <td class="gt_row gt_center"></td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="font-weight: bold;">creatinine</td>
      <td class="gt_row gt_center">936</td>
      <td class="gt_row gt_center">88 (68, 130)</td>
      <td class="gt_row gt_center">64 (54, 83)</td>
      <td class="gt_row gt_center"><0.001</td>
    </tr>
    <tr>
      <td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Unknown</td>
      <td class="gt_row gt_center"></td>
      <td class="gt_row gt_center">2,430</td>
      <td class="gt_row gt_center">2,754</td>
      <td class="gt_row gt_center"></td>
    </tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="5">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Statistics presented: Median (IQR); n (%)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Statistical tests performed: Wilcoxon rank-sum test; chi-square test of independence
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table></div><!--/html_preserve-->

```r
df_1 <- df[0:10]
df_2 <- df[11:20]
df_3 <- df[21:30]
df_4 <- df[31:40]
df_5 <- df[41:50]
df_6 <- df[51:60]
df_7 <- df[61:70]
df_8 <- df[71:80]


kable(as.data.frame(summarytools::descr(df_1, stats = "common")))
```



|          |        age| hemoglobin| Hypersensitive cardiac troponinI| PATIENT_ID| Serum chloride|
|:---------|----------:|----------:|--------------------------------:|----------:|--------------:|
|Mean      |   59.44330|  123.12492|                      1223.229980| 188.000000|     103.135692|
|Std.Dev   |   16.37223|   23.73799|                      5391.612528| 108.397417|       7.746938|
|Min       |   18.00000|    6.40000|                         1.900000|   1.000000|      71.500000|
|Median    |   62.00000|  125.00000|                        20.600000| 188.000000|     102.100000|
|Max       |   95.00000|  178.00000|                     50000.000000| 375.000000|     140.400000|
|N.Valid   | 6120.00000|  975.00000|                       507.000000| 375.000000|     975.000000|
|Pct.Valid |  100.00000|   15.93137|                         8.284314|   6.127451|      15.931373|

```r
kable(as.data.frame(summarytools::descr(df_2, stats = "common")))
```



|          |    albumin| Alkaline phosphatase| basophil(%)| eosinophils(%)| Interleukin 10| Interleukin 2 receptor| Platelet count| procalcitonin| Prothrombin time| Total bilirubin|
|:---------|----------:|--------------------:|-----------:|--------------:|--------------:|----------------------:|--------------:|-------------:|----------------:|---------------:|
|Mean      |  32.006317|             82.46774|   0.2099269|      0.6289446|      16.068539|             907.164179|      184.31034|      1.106601|        16.674924|        16.69538|
|Std.Dev   |   6.249461|             46.56562|   0.2172857|      1.0668200|      68.441138|             788.009337|      104.48167|      4.663411|         9.501093|        26.56593|
|Min       |  13.600000|             17.00000|   0.0000000|      0.0000000|       5.000000|              61.000000|       -1.00000|      0.020000|        11.500000|         2.50000|
|Median    |  32.200000|             69.50000|   0.2000000|      0.1000000|       5.900000|             676.500000|      178.00000|      0.100000|        14.800000|        10.70000|
|Max       |  48.600000|            620.00000|   1.7000000|      8.6000000|    1000.000000|            7500.000000|      558.00000|     57.170000|       120.000000|       505.70000|
|N.Valid   | 934.000000|            930.00000| 957.0000000|    957.0000000|     267.000000|             268.000000|      957.00000|    459.000000|       662.000000|       930.00000|
|Pct.Valid |  15.261438|             15.19608|  15.6372549|     15.6372549|       4.362745|               4.379085|       15.63725|      7.500000|        10.816994|        15.19608|

```r
kable(as.data.frame(summarytools::descr(df_3, stats = "common")))
```



|          | antithrombin|      HBsAg| indirect bilirubin| Interleukin 8| monocytes(%)| neutrophils(%)| Prothrombin activity| Quantification of Treponema pallidum antibodies| Red blood cell distribution width| total protein|
|:---------|------------:|----------:|------------------:|-------------:|------------:|--------------:|--------------------:|-----------------------------------------------:|---------------------------------:|-------------:|
|Mean      |    85.318182|   8.306308|           6.888962|     83.087687|     6.155219|       77.60146|             78.55083|                                       0.1324014|                         13.074540|     65.303115|
|Std.Dev   |    18.288268|  42.929819|           7.023537|    553.152505|     4.075424|       16.64369|             22.14339|                                       0.7699385|                          1.727006|      7.625481|
|Min       |    20.000000|   0.000000|           0.100000|      5.000000|     0.300000|        1.70000|              6.00000|                                       0.0200000|                         10.600000|     31.800000|
|Median    |    86.000000|   0.010000|           5.400000|     16.000000|     5.700000|       82.40000|             81.00000|                                       0.0500000|                         12.600000|     65.900000|
|Max       |   136.000000| 250.000000|         145.100000|   6795.000000|    53.000000|       98.90000|            142.00000|                                      11.9500000|                         27.100000|     88.700000|
|N.Valid   |   330.000000| 279.000000|         906.000000|    268.000000|   958.000000|      957.00000|            659.00000|                                     279.0000000|                        923.000000|    931.000000|
|Pct.Valid |     5.392157|   4.558823|          14.803922|      4.379085|    15.653595|       15.63725|             10.76797|                                       4.5588235|                         15.081699|     15.212418|

```r
kable(as.data.frame(summarytools::descr(df_4, stats = "common")))
```



|          | fibrinogen| hematocrit| Interleukin 1β| lymphocyte count| mean corpuscular hemoglobin concentration| mean corpuscular volume|    PH value| Tumor necrosis factorα|       Urea| White blood cell count|
|:---------|----------:|----------:|--------------:|----------------:|-----------------------------------------:|-----------------------:|-----------:|----------------------:|----------:|----------------------:|
|Mean      |   4.294435|  36.544514|       6.509702|         1.016782|                                 342.80355|               90.388192|   6.4835339|              11.577612|   9.589241|               15.59944|
|Std.Dev   |   1.888833|   5.314735|       7.012155|         2.059922|                                  17.79971|                6.504046|   0.7267371|              13.061641|   9.440427|               67.74697|
|Min       |   0.500000|  14.500000|       5.000000|         0.000000|                                 286.00000|               61.600000|   5.0000000|               4.000000|   0.800000|                0.13000|
|Median    |   4.120000|  36.600000|       5.000000|         0.800000|                                 343.00000|               90.100000|   6.5000000|               8.600000|   5.985000|                7.72000|
|Max       |  10.780000|  52.300000|      88.500000|        52.420000|                                 514.00000|              118.900000|   7.5650000|             168.000000|  68.400000|             1726.60000|
|N.Valid   | 566.000000| 957.000000|     268.000000|       957.000000|                                 957.00000|              957.000000| 384.0000000|             268.000000| 936.000000|             1127.00000|
|Pct.Valid |   9.248366|  15.637255|       4.379085|        15.637255|                                  15.63725|               15.637255|   6.2745098|               4.379085|  15.294118|               18.41503|

```r
kable(as.data.frame(summarytools::descr(df_5, stats = "common")))
```



|          | Corrected calcium| Direct bilirubin| Eosinophil count|     ferritin|    glucose| Mean platelet volume| neutrophils count| RBC distribution width SD| Red blood cell count| Serum potassium|
|:---------|-----------------:|----------------:|----------------:|------------:|----------:|--------------------:|-----------------:|-------------------------:|--------------------:|---------------:|
|Mean      |         2.3548687|         9.886774|         0.038652|  1379.144170|   8.889174|            10.909281|          7.809582|                 42.444204|              9.28780|       4.5088469|
|Std.Dev   |         0.1285858|        21.408093|         0.061654|  3454.399541|   5.221573|             1.088321|          6.070367|                  6.300563|             36.30541|       0.8516411|
|Min       |         1.6500000|         1.600000|         0.000000|    17.800000|   1.000000|             8.500000|          0.060000|                 31.300000|              0.10000|       2.7600000|
|Median    |         2.3600000|         4.800000|         0.010000|   711.000000|   6.990000|            10.800000|          5.850000|                 40.900000|              4.14000|       4.4100000|
|Max       |         2.7900000|       360.600000|         0.490000| 50000.000000|  43.010000|            15.000000|         33.880000|                113.300000|            749.50000|      12.8000000|
|N.Valid   |       914.0000000|       930.000000|       957.000000|   283.000000| 775.000000|           862.000000|        957.000000|                923.000000|           1127.00000|     980.0000000|
|Pct.Valid |        14.9346405|        15.196078|        15.637255|     4.624183|  12.663399|            14.084967|         15.637255|                 15.081699|             18.41503|      16.0130719|

```r
kable(as.data.frame(summarytools::descr(df_6, stats = "common")))
```



|          | (%)lymphocyte| Amino-terminal brain natriuretic peptide precursor(NT-proBNP)| aspartate aminotransferase|     calcium|  D-D dimer|      HCO3-| HCV antibody quantification| Thrombin time| Total cholesterol|  Uric acid|
|:---------|-------------:|-------------------------------------------------------------:|--------------------------:|-----------:|----------:|----------:|---------------------------:|-------------:|-----------------:|----------:|
|Mean      |      15.39165|                                                   3669.368421|                   46.52834|   2.0779571|   7.943413|  23.141113|                   0.1170968|     18.170318|         3.6893555|  276.12709|
|Std.Dev   |      12.93374|                                                   9695.000276|                  103.28073|   0.1629913|   9.211021|   4.409833|                   0.2350643|      8.894196|         0.9815529|  151.53874|
|Min       |       0.00000|                                                      5.000000|                    6.00000|   1.1700000|   0.210000|   6.300000|                   0.0200000|     13.000000|         0.1000000|   43.00000|
|Median    |      11.45000|                                                    585.000000|                   27.00000|   2.0800000|   2.155000|  23.500000|                   0.0600000|     16.800000|         3.6300000|  243.70000|
|Max       |      60.00000|                                                  70000.000000|                 1858.00000|   2.6200000|  60.000000|  36.300000|                   2.0900000|    161.900000|         7.3000000| 1176.00000|
|N.Valid   |     958.00000|                                                    475.000000|                  935.00000| 979.0000000| 630.000000| 934.000000|                 279.0000000|    566.000000|       931.0000000|  934.00000|
|Pct.Valid |      15.65359|                                                      7.761438|                   15.27778|  15.9967320|  10.294118|  15.261438|                   4.5588235|      9.248366|        15.2124183|   15.26144|

```r
kable(as.data.frame(summarytools::descr(df_7, stats = "common")))
```



|          | basophil count(#)| Fibrin degradation products|   globulin| Interleukin 6| International standard ratio| Lactate dehydrogenase| monocytes count| platelet large cell ratio| PLT distribution width| γ-glutamyl transpeptidase|
|:---------|-----------------:|---------------------------:|----------:|-------------:|----------------------------:|---------------------:|---------------:|-------------------------:|----------------------:|-------------------------:|
|Mean      |         0.0170846|                   61.354242|  33.241613|    112.307978|                    1.3127618|             474.22805|       0.5258516|                 31.765661|              13.005220|                  55.34409|
|Std.Dev   |         0.0171678|                   65.645267|   5.509276|    537.205847|                    0.8083756|             369.13698|       1.6943098|                  8.565205|               2.791223|                  69.45158|
|Min       |         0.0000000|                    4.000000|  10.100000|      1.500000|                    0.8400000|             110.00000|       0.0100000|                 11.200000|               8.000000|                   3.00000|
|Median    |         0.0100000|                   17.900000|  32.700000|     19.265000|                    1.1400000|             340.00000|       0.4100000|                 30.900000|              12.400000|                  34.00000|
|Max       |         0.1200000|                  190.800000|  50.600000|   5000.000000|                   13.4800000|            1867.00000|      39.9200000|                 62.200000|              25.300000|                 732.00000|
|N.Valid   |       957.0000000|                  330.000000| 930.000000|    272.000000|                  659.0000000|             934.00000|     957.0000000|                862.000000|             862.000000|                 930.00000|
|Pct.Valid |        15.6372549|                    5.392157|  15.196078|      4.444444|                   10.7679739|              15.26144|      15.6372549|                 14.084967|              14.084967|                  15.19608|

```r
kable(as.data.frame(summarytools::descr(df_8, stats = "common")))
```



|          | 2019-nCoV nucleic acid detection| Activation of partial thromboplastin time|      eGFR|       ESR| glutamic-pyruvic transaminase| High sensitivity C-reactive protein| HIV antibody quantification| mean corpuscular hemoglobin| serum sodium| thrombocytocrit|
|:---------|--------------------------------:|-----------------------------------------:|---------:|---------:|-----------------------------:|-----------------------------------:|---------------------------:|---------------------------:|------------:|---------------:|
|Mean      |                        -1.000000|                                 41.520070|  81.56432|  33.68930|                      38.86037|                            76.24383|                   0.1001439|                    30.99760|   141.549641|       0.2123086|
|Std.Dev   |                         0.000000|                                 11.783239|  32.22278|  24.60345|                      83.76664|                            81.08041|                   0.0400809|                     2.90913|     7.333611|       0.0928407|
|Min       |                        -1.000000|                                 21.800000|   2.00000|   1.00000|                       5.00000|                             0.10000|                   0.0500000|                    20.40000|   115.400000|       0.0100000|
|Median    |                        -1.000000|                                 39.200000|  87.90000|  28.00000|                      24.00000|                            51.50000|                   0.0900000|                    30.90000|   140.400000|       0.2100000|
|Max       |                        -1.000000|                                144.000000| 224.00000| 110.00000|                    1600.00000|                           320.00000|                   0.2700000|                    50.80000|   179.700000|       0.5100000|
|N.Valid   |                       501.000000|                                568.000000| 936.00000| 383.00000|                     931.00000|                           737.00000|                 278.0000000|                   957.00000|   975.000000|     862.0000000|
|Pct.Valid |                         8.186274|                                  9.281046|  15.29412|   6.25817|                      15.21242|                            12.04248|                   4.5424837|                    15.63725|    15.931373|      14.0849673|

## Age and gender distribution

```r
ggplot(df, aes(x=gender, fill=gender)) + geom_histogram(stat = "count") + ylab("Number of patients") +
  xlab("Gender") + theme_minimal()
```

![](COVID-19-analysis_files/figure-html/gender_distribution-1.png)<!-- -->

```r
 ggplot(df, aes(x=age,fill=gender)) + geom_histogram(binwidth = 1) + facet_grid(. ~ gender) + scale_x_continuous(name="Age", limits=c(min(df$age), max(df$age)), breaks = seq(0, 100, by=10)) +
   ylab("Number of patients") +
   theme_minimal()
```

![](COVID-19-analysis_files/figure-html/gender_distribution-2.png)<!-- -->
