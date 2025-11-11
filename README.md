## 🩺 **의료장벽이 건강상태에 미치는 영향 분석** 
### 성향점수 매칭(PSM) 기반 로지스틱 회귀분석

![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
![IDE](https://img.shields.io/badge/RStudio-75AADB?logo=rstudio&logoColor=white)

---

### 👥 팀 프로젝트
- 멤버: 최현석, 김태하, 하승민
- 과목: 헬스케어빅데이터 입문 (Introduction to Healthcare Big Data)
- 제출일: 2025.05.29

---

### 👤 역할 분담
- 최현석: 데이터 수집, 전체 코드 작성, PPT 제작, 발표
- 김태하: PPT 제작, 결론 도출 및 결과 해석, 발표
- 하승민: PPT 제작, 시각화 코드 작성과 결과 정리, 발표

---

### 📊 데이터 소개
- 출처: BRFSS
- 데이터: `diabetes_binary_health_indicators_BRFSS2021.csv`
  - 미국 성인 설문 응답자 236,378명
  - 22개 변수 (당뇨병, 고혈압 등)
  
---

### 🎯 연구 주제
- **의료비 장벽(NoDocbcCost)이 건강상태가 나쁨(bgh=1)에 미치는 영향을 추정**
- 관측연구의 교란을 줄이기 위해 성향점수 매칭(PSM) 후 로지스틱 회귀로 효과를 추정

---

### 🔍 분석 방법
- 성향점수 매칭(PSM) 기반으로 로지스틱 회귀분석을 수행
  - 공변랑 선정과 성향점수(PS) 추정
  - 최근접 이웃 1:1 매칭 실행 후, 균형 진단
  - 매칭 표본에서 로지스틱 회귀를 적합하고 유의수준 5% 기준 주효과 후진제거로 최종 변수 확정

---

### 🧠 분석 결과
<img align="left" alt="Data Analysis Illustration" 
     src="https://raw.githubusercontent.com/Hyunseok-C/healthcare-barrier-psm-analysis/main/model_final.png" 
     width="340px"/>


| 🔺 건강 나쁠 확률 높이는 요인 | 🔻 건강 나쁠 확률 낮추는 요인 |
|:---------------------------------:|:---------------------------------:|
| Diabetes_binary (당뇨병)         | PhysActivity (신체활동)         |
| HighBP (고혈압)                 | Veggies (채소 섭취)             |
| HighChol (고지혈증)             | Education (학력 수준)           |
| BMI (체질량지수)                |                                  |
| Smoker (흡연 여부)              |                                  |
| Stroke (뇌졸중 경험)            |                                  |
| HeartDiseaseorAttack (심장질환) |                                  |
| NoDocbcCost (비용원인비진료)  |                                  |
| Age (연령)                      |                                  |
| Income (소득수준)               |                                  |

#### 핵심 효과 (처치변수)
- **비용 때문에 진료를 받지 못한 경험(NoDocbcCost=1)** 은  
  건강상태 **‘나쁨’(bgh=1)** 의 오즈를 **약 2.26배 (95% CI: 2.13–2.40)** 유의하게 증가시킴  
  → **의료비 장벽이 건강 악화와 통계적으로 강하게 연관**

---

### 📈 요약 인사이트

- **질병 요인(만성질환)** → 건강상태 악화에 가장 큰 영향  
- **생활습관 요인** → 흡연은 악화, 신체활동·채소 섭취는 완화  
- **사회경제적 요인** → 저소득층·의료비 장벽 경험자는 건강 불평등 심화  
- **정책적 시사점**: 의료비 부담 완화 및 건강생활 실천 환경 조성이 건강격차 완화에 핵심적

---
