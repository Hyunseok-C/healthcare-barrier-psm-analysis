#------------------------------------------------------------------------------
# 헬스케어 텀프로젝트 - 10조: 김태하, 최현석, 하승민
#------------------------------------------------------------------------------
# 주제: 의료비 장벽이 건강상태에 미치는 영향
#       성향점수 매칭 기반 로지스틱 회귀분석
#------------------------------------------------------------------------------
# 데이터 불러오기
df <- read.csv("C:\\Users\\chs02\\OneDrive\\바탕 화면\\헬스케어빅데이터\\텀프로젝트\\diabetes_binary_health_indicators_BRFSS2021.csv")
head(df)

dim(df) # 236378행, 22열

# 결측치 확인
colSums(is.na(df))

# 범주형 처리
df$bgh <- ifelse(df$GenHlth %in% c(1, 2, 3), 0, 1)

df$Education <- ifelse(df$Education <= 4, 4, df$Education)
df$Income <- ifelse(df$Income < 5, "C",
                    ifelse(df$Income < 9, "B", "A"))

df$Education <- as.factor(df$Education)
df$Income <- as.factor(df$Income)

# 자료형 확인
str(df) 

# 요약통계량 확인
summary(df)

#------------------------------------------------------------------------------
# 데이터 시각화
#------------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(dplyr)
library(patchwork)

# 범주형 변수 목록 (이진변수)
cat_vars <- c("Sex", "HighBP", "HighChol", "Smoker", "Stroke", "HeartDiseaseorAttack",
              "CholCheck", "PhysActivity", "Fruits", "Veggies",
              "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "Diabetes_binary")
# 수치형 변수 목록
num_vars <- c("BMI", "Age")

## [시각화1] GenHlth과 범주형변수들의 table 분할표 시각화
plot_list <- list()

vars1 <- c("Sex", "HighBP", "HighChol", "Smoker", "Stroke", "HeartDiseaseorAttack",
           "CholCheck", "PhysActivity", "Fruits", "Veggies",
           "HvyAlcoholConsump","NoDocbcCost", "AnyHealthcare", "Diabetes_binary", "Income", "Education")

for (var in vars1) {
  tab <- as.data.frame(table(df$bgh, df[[var]]))
  colnames(tab) <- c("bgh", "Category", "Freq")
  
  p <- ggplot(tab, aes(x = Category, y = factor(bgh), fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 3) +
    scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
    guides(fill = "none") + 
    labs(
      title = var,
      x = var,
      y = "bgh"
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8)
    )
  
  plot_list[[var]] <- p
}

wrap_plots(plot_list, ncol = 4)


## [시각화2] 공변량 각 그룹 간 GenHlth=1 비율 차이
# 2-1. 범주형 변수 결과 (비율차이)
df_bgh_diff <- df %>%
  select(bgh, all_of(cat_vars)) %>%
  pivot_longer(-bgh, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, bgh) %>%
  summarise(Value = mean(Value == 1) * 100, .groups = "drop") %>%
  pivot_wider(names_from = bgh, values_from = Value, names_prefix = "Group_") %>%
  mutate(Diff = Group_1 - Group_0,
         Type = "Categorical")

# 2-2. 연속형 변수 결과 (평균차이)
df_cont_diff <- df %>%
  select(bgh, all_of(num_vars)) %>%
  pivot_longer(-bgh, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, bgh) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = bgh, values_from = Value, names_prefix = "Group_") %>%
  mutate(Diff = Group_1 - Group_0,
         Type = "Continuous")  

# 2-3. 합치기
df_all_diff <- bind_rows(df_bgh_diff, df_cont_diff)

# 2-4. 가로막대그래프 시각화
ggplot(df_all_diff, aes(x = reorder(Variable, Diff), y = Diff, fill = Diff > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "#D55E00", "FALSE" = "#0072B2"),
    labels = c("FALSE" = "Good Health", "TRUE" = "Poor Health")
  ) +
  labs(
    title = "Covariate Differences (Proportions and Means) by Health Status",
    x = "Covariate",
    y = "Difference (Poor - Good)",
    fill = "Higher Group"
  ) +
  theme_minimal()

## [시각화3] 건강 변수들의 교육/소득 수준별 비율 분포
# 3-1. 그래프 1: 교육 수준별 건강 상태
p1 <- ggplot(df, aes(x = Education, fill = factor(bgh))) +
  geom_bar(position = "fill") +
  labs(
    title = "Binary General Health by Education Level",
    x = "Education Level",
    y = "Proportion",
    fill = "Health Status\n(0 = Not Bad, 1 = Bad)"
  ) +
  theme_minimal()

# 3-2. 그래프 2: 소득 수준별 건강 상태
p2 <- ggplot(df, aes(x = Income, fill = factor(bgh))) +
  geom_bar(position = "fill") +
  labs(
    title = "Binary General Health by Income Level",
    x = "Income Level",
    y = "Proportion",
    fill = "Health Status\n(0 = Not Bad, 1 = Bad)"
  ) +
  theme_minimal()

p1 + p2 

## [시각화4] NoDocbcCost 자체 빈도 막대그래프
ggplot(df, aes(x = factor(NoDocbcCost))) +
  geom_bar(fill = c("lightblue", "lightcoral")) +
  labs(title = "Frequency of Treatment Variable (NoDocbcCost)",
       x = "NoDocbcCost (0 = No, 1 = Yes)",
       y = "Count") +
  theme_minimal()

## [시각화5] NoDocbcCost과 범주형변수들의 분할표 시각화
plot_list <- list()

vars2 <- c("Sex", "HighBP", "HighChol", "Smoker", "Stroke", "HeartDiseaseorAttack",
           "CholCheck", "PhysActivity", "Fruits", "Veggies",
           "HvyAlcoholConsump", "AnyHealthcare", "Diabetes_binary", "Income", "Education")

for (var in vars2) {
  tab <- as.data.frame(table(df$NoDocbcCost, df[[var]]))
  colnames(tab) <- c("NoDocbcCost", "Category", "Freq")
  
  p <- ggplot(tab, aes(x = Category, y = factor(NoDocbcCost), fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 3) +
    scale_fill_gradient(low = "#deebf7", high = "#3182bd") +
    guides(fill = "none") + 
    labs(
      title = var,
      x = var,
      y = "NoDocbcCost"
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8)
    )
  
  plot_list[[var]] <- p
}

wrap_plots(plot_list, ncol = 4)

# [시각화6] 공변량 각 그룹 간 NoDocbcCost=1 비율 차이
# 6-1. 범주형 변수 결과 (비율 차이)
df_percent_diff2 <- df %>%
  select(NoDocbcCost, all_of(cat_vars)) %>%
  pivot_longer(-NoDocbcCost, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, NoDocbcCost) %>%
  summarise(Percent = mean(Value == 1) * 100, .groups = "drop") %>%
  pivot_wider(names_from = NoDocbcCost, values_from = Percent, names_prefix = "Group_") %>%
  mutate(Diff = Group_1 - Group_0,
        Type = "Categorical")

# 6-2. 연속형 변수 결과 (평균 차이)
df_cont_diff2 <- df %>%
  select(NoDocbcCost, all_of(num_vars)) %>%
  pivot_longer(-NoDocbcCost, names_to = "Variable", values_to = "Value") %>%
  group_by(Variable, NoDocbcCost) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = NoDocbcCost, values_from = Value, names_prefix = "Group_") %>%
  mutate(Diff = Group_1 - Group_0,
         Type = "Continuous")  

df_all_diff2 <- bind_rows(df_percent_diff2, df_cont_diff2)

# 6-3. 가로막대그래프 시각화
ggplot(df_all_diff2, aes(x = reorder(Variable, Diff), y = Diff, fill = Diff > 0)) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(
    values = c("TRUE" = "#D55E00", "FALSE" = "#0072B2"),
    labels = c("FALSE" = "Yes", "TRUE" = "No")
  ) +
  labs(
    title = "Covariate Differences (Proportions and Means) by NoDocbcCost",
    x = "Covariate",
    y = "Difference (Yes - No)",
    fill = "Higher Group"
  ) +
  theme_minimal()


## [시각화7] NoDocbcCost의 교육/소득 수준별 비율 분포
# 7-1. 그래프 1: 교육 수준별 건강 상태
p1 <- ggplot(df, aes(x = Education, fill = factor(NoDocbcCost))) +
  geom_bar(position = "fill") +
  labs(
    title = "NoDocbcCost by Education Level",
    x = "Education Level",
    y = "Proportion",
    fill = "NoDocbcCost\n(0 = No, 1 = Yes)"
  ) +
  theme_minimal()

# 7-2. 그래프 2: 소득 수준별 건강 상태
p2 <- ggplot(df, aes(x = Income, fill = factor(NoDocbcCost))) +
  geom_bar(position = "fill") +
  labs(
    title = "NoDocbcCost by Income Level",
    x = "Income Level",
    y = "Proportion",
    fill = "NoDocbcCost\n(0 = No, 1 = Yes)"
  ) +
  theme_minimal()

p1 + p2 

#------------------------------------------------------------------------------
# 성향 점수 분석 
#------------------------------------------------------------------------------
## 1. 매칭 전 공변량 분포
# [시각화 5]로 확인 가능
#------------------------------------------------------------------------------
## 2. 공변량 선택
vars <- c("Diabetes_binary", "HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke",
          "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies",
          "HvyAlcoholConsump", "AnyHealthcare", "Sex", "Age", "Education", "Income")
factorvars <- c("Diabetes_binary", "HighBP", "HighChol", "CholCheck", "Smoker", "Stroke",
                "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies",
                "HvyAlcoholConsump", "AnyHealthcare", "Sex", "Education", "Income")

#------------------------------------------------------------------------------
## 3. 최근접 이웃 매칭
# 성향 점수 계산 
library(MatchIt)
ps_formula <- NoDocbcCost ~ Diabetes_binary + HighBP + HighChol + CholCheck + BMI + Smoker + 
  Stroke + HeartDiseaseorAttack + PhysActivity + Fruits + Veggies +
  + HvyAlcoholConsump + AnyHealthcare + Sex + Age + Education + Income
ps_model <- glm(ps_formula, data = df, family = binomial())
df$pscore <- predict(ps_model, type = "response")

# 성향점수 분포 시각화
par(mfrow = c(1, 2))  # 1행 2열로 그래프 배치
hist(df[df$NoDocbcCost == 1, ]$pscore,
     main = "NoDocbcCost = 1", xlim = c(0, 1),
     col = "lightcoral", breaks = 30, xlab = "pscore", ylab = "count")
hist(df[df$NoDocbcCost == 0, ]$pscore,
     main = "NoDocbcCost = 0", xlim = c(0, 1),
     col = "lightblue", breaks = 30, xlab = "pscore", ylab = "count")

# 성향 점수 매칭
match_model <- matchit(ps_formula, data = df, method = "nearest", distance = df$pscore, caliper = sd(df$pscore) * 0.25)
matched_df <- match.data(match_model)
summary(match_model)

# + 성향 점수 가중치
prop <- mean(df$NoDocbcCost == 1)
prop/0.002
(1-prop)/(1-0.1)

summary(df$pscore)

df$sw <- ifelse(df$NoDocbcCost == 1, prop/df$pscore, (1-prop)/(1-df$pscore))
df$id <- 1:nrow(df)
svydes.sw <- survey::svydesign(id=~id, weights=~sw, data=df, strata=~NoDocbcCost)
svy.sw.tableone <- tableone::svyCreateTableOne(strata="NoDocbcCost", vars=vars, factorVars=factorvars,
                                               data=svydes.sw, test=T)
print(svy.sw.tableone, smd=T)

#------------------------------------------------------------------------------
## 4. 매칭 전/후 공변량 균형 비교
library(tableone)
unmatched_table <- CreateTableOne(vars = vars, strata = "NoDocbcCost", factorVars = factorvars, data = df)
print(unmatched_table, smd = TRUE) # 매칭 전

matched_table <- CreateTableOne(vars = vars, strata = "NoDocbcCost", factorVars = factorvars, data = matched_df)
print(matched_table, smd = TRUE) # 매칭 후

# 매칭 전/후 공변량 비교 시각화
library(cobalt)
love.plot(match_model,
          stat = "mean.diffs",        # 표준화 평균 차이(SMD)
          abs = TRUE,                
          threshold = 0.1,            # SMD 기준선
          var.order = "unadjusted",   
          line = TRUE,    
          stars = "std",
          colors = c("gray60", "black"),    
          shapes = c("triangle", "circle"),  
          legend = "top",            
          sample.names = c("매칭 전", "매칭 후"),
          title = "공변량 균형 비교 (매칭 전 vs 매칭 후)",  
          xlab = "절댓값 기준 표준화 평균 차이(SMD)",  
          size = 3                   
)

#------------------------------------------------------------------------------
## 5. 매칭 전/후  공변량 분포 비교
library(ggplot2)
library(cowplot)

Treatment.1          <- df[df$NoDocbcCost == 1, ]
Treatment.0          <- df[df$NoDocbcCost == 0, ]
matched_Treatment.1  <- matched_df[matched_df$NoDocbcCost == 1, ]
matched_Treatment.0  <- matched_df[matched_df$NoDocbcCost == 0, ]

## (5-1) 박스플롯 - age (수치형)
# 매칭 전 치료군
p1 <- ggplot(Treatment.1, aes(y = Age)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Treatment before Matching") +
  theme_minimal()

# 매칭 전 대조군
p2 <- ggplot(Treatment.0, aes(y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Control before Matching") +
  theme_minimal()

# 매칭 후 치료군
p3 <- ggplot(matched_Treatment.1, aes(y = Age)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Treatment after Matching") +
  theme_minimal()

# 매칭 후 대조군
p4 <- ggplot(matched_Treatment.0, aes(y = Age)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Control after Matching") +
  theme_minimal()

# 4개 박스플롯 정렬
plot_grid(p1, p2, p3, p4,
          ncol = 2,
          labels = c("A", "B", "C", "D"),
          label_size = 12,
          align = "hv")


## (5-2) 막대그래프 - income (범주형)
# 매칭 전
before_df <- rbind(
  data.frame(Income = Treatment.1$Income, Group = "Treatment"),
  data.frame(Income = Treatment.0$Income, Group = "Control")
)

# 매칭 후
after_df <- rbind(
  data.frame(Income = matched_Treatment.1$Income, Group = "Treatment"),
  data.frame(Income = matched_Treatment.0$Income, Group = "Control")
)

# 매칭 전
bar_before <- ggplot(before_df, aes(x = Income, fill = Group)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Treatment" = "lightcoral", "Control" = "lightblue")) +
  labs(title = "Income Distribution Before Matching", x = "Income Level", y = "Count") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# 매칭 후
bar_after <- ggplot(after_df, aes(x = Income, fill = Group)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Treatment" = "lightcoral", "Control" = "lightblue")) +
  labs(title = "Income Distribution After Matching", x = "Income Level", y = "Count") +
  theme_minimal()

# 그래프 정렬 출력
plot_grid(bar_before, bar_after, ncol = 2)

#------------------------------------------------------------------------------
## 6. 매칭 전/후  성향점수 분포 비교
## (6-1) 매칭 전/후 성향점수 히스토그램
# 매칭 전 치료군
T1before <- ggplot(Treatment.1, aes(x = pscore)) +
  geom_histogram(bins = 70, fill = "lightcoral") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,45000)) +
  labs(title = "Treatment before Matching") +
  theme_minimal()

# 매칭 전 대조군
T0before <- ggplot(Treatment.0, aes(x = pscore)) +
  geom_histogram(bins = 70, fill = "lightblue") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,45000)) +
  labs(title = "Control before Matching") +
  theme_minimal()

# 매칭 후 치료군
T1after <- ggplot(matched_Treatment.1, aes(x = pscore)) +
  geom_histogram(bins = 30, fill = "lightcoral") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Treatment after Matching") +
  theme_minimal()

# 매칭 후 대조군
T0after <- ggplot(matched_Treatment.0, aes(x = pscore)) +
  geom_histogram(bins = 30, fill = "lightblue") +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Control after Matching") +
  theme_minimal()

plot_grid(
  T1before, T0before, T1after,T0after, ncol = 2,
  labels = c("A", "B", "C", "D"), label_size = 12, align = "hv"
)

## (6-2) 매칭 전/후 성향점수 밀도곡선
# 매칭 전 데이터 결합
before_df <- rbind(
  data.frame(pscore = Treatment.1$pscore, group = "Treatment"),
  data.frame(pscore = Treatment.0$pscore, group = "Control")
)

# 매칭 후 데이터 결합
after_df <- rbind(
  data.frame(pscore = matched_Treatment.1$pscore, group = "Treatment"),
  data.frame(pscore = matched_Treatment.0$pscore, group = "Control")
)

# 매칭 전 밀도곡선
before_plot <- ggplot(before_df, aes(x = pscore, color = group, fill = group)) +
  geom_density(alpha = 0.4) +
  scale_color_manual(values = c("Treatment" = "lightcoral", "Control" = "steelblue")) +
  scale_fill_manual(values = c("Treatment" = "lightcoral", "Control" = "steelblue")) +
  labs(title = "Before Matching", x = "Propensity Score", y = "Density") +
  theme_minimal()

# 매칭 후 밀도곡선
after_plot <- ggplot(after_df, aes(x = pscore, color = group, fill = group)) +
  geom_density(alpha = 0.4) +
  scale_color_manual(values = c("Treatment" = "lightcoral", "Control" = "steelblue")) +
  scale_fill_manual(values = c("Treatment" = "lightcoral", "Control" = "steelblue")) +
  labs(title = "After Matching", x = "Propensity Score", y = "Density") +
  theme_minimal()

# 두 개의 밀도곡선 그래프 정렬
plot_grid(before_plot, after_plot, ncol = 2)


#------------------------------------------------------------------------------
# 7. 결과분석: 매칭된 데이터를 이용한 회귀 분석
matched_model <- glm(bgh ~ Diabetes_binary
                      + HighBP + HighChol + CholCheck + BMI + Smoker + Stroke + HeartDiseaseorAttack 
                      + NoDocbcCost + PhysActivity + Fruits + Veggies +
                        + HvyAlcoholConsump + AnyHealthcare + Age + Education + Income + Sex,
                      data = matched_df, family = binomial())
summary(matched_model)


# 7-1 결과분석 1단계: 단순 회귀에서 공변량 (p<0.2)
vars <- c("Diabetes_binary", "HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke",
          "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies",
          "HvyAlcoholConsump", "AnyHealthcare", "Sex", "Age", "Education", "Income")

for (v in vars) {
  fml <- as.formula(paste("bgh ~", v))
  model <- glm(fml, family = binomial(link = "logit"), data = matched_df)
  
  sm <- summary(model)
  
  coef_p <- sm$coefficients[-1, , drop=FALSE]  # Intercept 제외
  
  pvals_over_02 <- coef_p[coef_p[,4] >= 0.2, , drop=FALSE]
  
  if (nrow(pvals_over_02) > 0) {
    cat("\n[", v, "] 변수의 p-value >= 0.2 인 결과\n")
    print(pvals_over_02)
  }
}
# [Sex] 변수가 유의확률이 0.68으로 0.2보다 크므로 Sex 제거

# 7-2 결과분석 2단계: 유의수준 5%에서 주효과
summary(glm(bgh ~ Diabetes_binary + HighBP + HighChol + CholCheck + BMI + Smoker + Stroke + HeartDiseaseorAttack 
                      + NoDocbcCost + PhysActivity + Fruits + Veggies +
                      + HvyAlcoholConsump + AnyHealthcare + Age + Education + Income,
                      data = matched_df, family = binomial()))
# Fruits 제거 (유의확률 0.6826)

summary(glm(bgh ~ Diabetes_binary + HighBP + HighChol + CholCheck + BMI + Smoker + Stroke + HeartDiseaseorAttack 
            + NoDocbcCost + PhysActivity + Veggies +
              + HvyAlcoholConsump + AnyHealthcare + Age + Education + Income,
            data = matched_df, family = binomial()))
# HvyAlcoholConsump 제거 (유의확률 0.3565)

summary(glm(bgh ~ Diabetes_binary + HighBP + HighChol + CholCheck + BMI + Smoker + Stroke + HeartDiseaseorAttack 
            + NoDocbcCost + PhysActivity + Veggies +
              + AnyHealthcare + Age + Education + Income,
            data = matched_df, family = binomial()))
# CholCheck 제거 (유의확률 0.1981)

summary(glm(bgh ~ Diabetes_binary + HighBP + HighChol + BMI + Smoker + Stroke + HeartDiseaseorAttack 
            + NoDocbcCost + PhysActivity + Veggies +
              + AnyHealthcare + Age + Education + Income,
            data = matched_df, family = binomial()))
# AnyHealthcare 제거 (유의확률 0.0115, 대용량, 관련성 등)

summary(glm(bgh ~ Diabetes_binary + HighBP + HighChol + BMI + Smoker + Stroke + HeartDiseaseorAttack 
            + NoDocbcCost + PhysActivity + Veggies +
              + Age + Education + Income,
            data = matched_df, family = binomial()))

#------------------------------------------------------------------------------
## 8. 최종모형
library(jtools)
final_model <- glm(bgh ~ Diabetes_binary
                      + HighBP + HighChol + BMI + Smoker + Stroke + HeartDiseaseorAttack 
                      + NoDocbcCost + PhysActivity + Veggies +
                        + Age + Education + Income,
                      data = matched_df, family = binomial())
summary(final_model)
jtools::summ(final_model, exp=TRUE)

# 해석은 ppt