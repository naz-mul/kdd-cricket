# Knowledge Discovery in Databases: Cricket
In cricket, there is a wealth of statistical data, such as batting records, bowling records, individual player records, match scorecard etc. To put this data to proper use, it can be accrued and analysed to make accurate predictions for a game that is known to be unpredictable. In this study, I assessed the numerous aspects affecting the outcome of men’s one day international cricket matches by classifying the key elements from the data set and using several data-mining techniques. I mainly concentrated on the importance of home advantage and the magnitude of the toss decision whether to bat or bowl first. Although, winning the toss is a significant characteristic of a cricket match, I discovered, other aspects incline to overshadow the outcome of the match result, specifically when factoring the merit of the team and the situation of the tournament.

## Steps
- Step 1: Selection (data into target data)
- Step 2: Preprocessing (target data into processed data)
- Step 3: Transformation (processed data into transformed data)
- Step 4: Data Mining (transformed data into patterns)
- Step 5: Interpretation and/or Evaluation patterns into knowledge)

## Definitions
1. Data Integration: First of all the data are collected and integrated from all the different sources.

2. Data Selection: We may not all the data we have collected in the first step. In this step we select only those data which we think useful for data mining.

3. Data Cleaning: The data we have collected are not clean and may contain errors, missing values, noisy or inconsistent data. Therefore we need to apply different techniques to get rid of such anomalies.

4. Data Transformation: The data even after cleaning are not ready for mining as we need to transform them into forms appropriate for mining. The techniques used to accomplish this are smoothing, aggregation, normalization etc.

5. Data Mining: Now we are ready to apply data mining techniques on the data to discover the interesting patterns. The techniques like clustering and association analysis are among the many different techniques used for data mining.

6. Pattern Evaluation and Knowledge Presentation: This step involves visualization, transformation, removing redundant patterns etc from the patterns we generated.

7. Decisions / Use of Discovered Knowledge: This step helps user to make use of the knowledge acquired to take better decisions.

## Dataset
|   team_a   | team_b | gender | venue_country |    date    | toss_winner | toss_decision | day_night | rain | duckworth_lewis |          outcome          | winner |
|------------|--------|--------|---------------|------------|-------------|---------------|-----------|------|-----------------|---------------------------|--------|
| Bangladesh | India  | male   | Bangladesh    | 2017-05-05 | BDESH       | bat           |         0 |    0 |               0 | Bangladesh won by 45 runs | BDESH  |
