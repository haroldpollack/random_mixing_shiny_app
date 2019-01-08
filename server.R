library(UsingR)
library("dplyr")
library("knitr")
library("ggplot2")
library("xts")
library("questionr")
library("janitor")
library("lubridate")
library("epiDisplay")
library("reshape2")
library("plotly")
library("formattable")
shinyServer(
  function(input, output) {
    my.reactive <- reactive ({
      RR <- input$RR
      cost_per_person_per_week<-input$cost_weekly
      #
      #    years covered
      #
      timeframe<-50
      #
      #      Intervals
      #
      intervals<-500
      #
      #    interval (d_t)
      #
      d_t<- timeframe/intervals
      #
      #    Discount rate
      #
      discount_rate<-0.03
      #
      # Population size
      #
      N0<-1000
      #
      #    Initial infection rate
      #
      pi0<- input$pi_0
      #
      #   I(0) is initial number of infecteds
      #
      I0 <- pi0*N0
      #
      #     kappa efficiency of transmission per risk event
      #
      kappa<-0.003
      #
      #     lambda=frequency of risk contacts 1/week per interval
      #     Put into year units 
      #
      weekly_lambda<-input$weekly_risk_events
      lambda<-weekly_lambda*52.0
      #
      #   career_duration (yrs)
      #
      career_duration<-11
      #
      #    define exit rate per year
      #
      delta<-1/career_duration
      #
      # cost per person per week of intervention
      #
      cost_per_person_per_year<-cost_per_person_per_week*52.0
      #
      #    Risk Reduction
      #
      epidplot<-data.frame()[1:intervals, ]
      epidplot$time_yr<-rep(0, intervals)
      epidplot$I_0<-rep(0, intervals)
      epidplot$I_RR<-rep(0, intervals)
      epidplot$incidence0<-rep(0, intervals)
      epidplot$incidenceRR<-rep(0, intervals)
      epidplot$pi_0<-rep(0, intervals)
      epidplot$pi_RR<-rep(0, intervals)
      epidplot$PDV_diff_incidence<-rep(0, intervals)
      
      epidplot$pi_0[1]<-I0/N0
      epidplot$pi_RR[1]<-I0/N0
      epidplot$I_0[1]<-I0
      epidplot$I_RR[1]<-I0
      epidplot$time_yr[1]<-0
      epidplot$incidence0[1]<-kappa*lambda*epidplot$pi_0[1]*(N0-epidplot$I_0[1])
      epidplot$incidenceRR[1]<-kappa*lambda*epidplot$pi_0[1]*(N0-epidplot$I_0[1])*(1-RR)
      epidplot$diff_incidence[1]<-epidplot$incidence0[1]-epidplot$incidenceRR[1]
      pdv_diff_sum<-0
      pdv_cost_sum<-cost_per_person_per_year*d_t
      epidplot$PDV_diff_incidence[1]<-epidplot$diff_incidence[1]*d_t
      epidplot$PDV_cost[1]<-cost_per_person_per_year*d_t
      for (k in 2:intervals) 
      {epidplot$incidence0[k]<-kappa*lambda*epidplot$pi_0[k-1]*(N0-epidplot$I_0[k-1])
      epidplot$I_0[k]<-epidplot$I_0[k-1]+epidplot$incidence0[k]*d_t-delta*epidplot$I_0[k-1]*d_t
      epidplot$pi_0[k]<-epidplot$I_0[k]/N0
      epidplot$time_yr[k]<-k*d_t
      #
      #    Now the RR trend. Note that incidence declines by fraction RR
      #
      epidplot$incidenceRR[k]<-kappa*lambda*epidplot$pi_RR[k-1]*(N0-epidplot$I_RR[k-1])*(1-RR)
      epidplot$I_RR[k]<-epidplot$I_RR[k-1]+epidplot$incidenceRR[k]*d_t-delta*epidplot$I_RR[k-1]*d_t
      epidplot$pi_RR[k]<-epidplot$I_RR[k]/N0
      #
      #     Examine change of incidence and PDV issues
      #
      epidplot$diff_incidence[k]<-epidplot$incidence0[k]-epidplot$incidenceRR[k]
      epidplot$PDV_cost[k]<-cost_per_person_per_year*exp(-discount_rate*epidplot$time_yr[k])*d_t
      epidplot$PDV_diff_incidence[k]<-epidplot$diff_incidence[k]*exp(-discount_rate*epidplot$time_yr[k])*d_t
      pdv_diff_sum<-pdv_diff_sum+epidplot$PDV_diff_incidence[k]
      pdv_cost_sum<-pdv_cost_sum+epidplot$PDV_cost[k]
      
      }
      #
      #    Now compute the sum
      #
      # cost_per_averted_inf<-N0*pdv_cost_sum/pdv_diff_sum
      cost_per_averted_inf<-N0*sum(epidplot$PDV_cost)/sum(epidplot$PDV_diff_incidence) 
      #
      #    Now format for currency
      #
      cost_per_averted_inf<-currency(cost_per_averted_inf,digits = 0L)
        
      df1 <- data.frame(years=epidplot$time_yr,prevalence=epidplot$pi_0,scenario="Prevalence under baseline")
      df2 <- data.frame(years=epidplot$time_yr,prevalence=epidplot$pi_RR,scenario="Prevalence with prevention")
      df_prevalence <- rbind(df1,df2)
      #
      #     Now add incidence graph
      #
      df3 <- data.frame(years=epidplot$time_yr,incidence=epidplot$incidence0,scenario="Incidence under baseline")
      df4 <- data.frame(years=epidplot$time_yr,incidence=epidplot$incidenceRR,scenario="Incidence with prevention")
      df5 <- data.frame(years=epidplot$time_yr,incidence=epidplot$diff_incidence,scenario="**Reduced** annual incidence from prevention")
      df_Incidence <- rbind(df3,df4,df5)
      #
      #    Store df_prevalence and df_incidence and cost per averted infection in a list
      #
      my.list<-list("prevalence time series"=df_prevalence,"cost-effectiveness"=cost_per_averted_inf,"incidence time series"=df_Incidence)
      #
      # return my.list to the global environment
      #
      return(my.list)
  #
  #    End reactive component
  #
    }) 
    output$g_prev <- renderPlot({
      qplot(years,prevalence,data=my.reactive()[["prevalence time series"]],geom=c("point"),color=scenario)
    })  
    output$g_incidence <- renderPlot({
      qplot(years,incidence,data=my.reactive()[["incidence time series"]],geom=c("point"),color=scenario)
    })  

    output$cost_per_averted_infection <- renderText({my.reactive()[["cost-effectiveness"]]})
  }
)