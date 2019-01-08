shinyUI(pageWithSidebar(
  headerPanel("Random mixing & HIV prevention for PWID"),
  sidebarPanel(
    sliderInput('RR', 'Posited risk reduction from intervention',value = 0.33, min = 0, max = 1, step =0.05),
    sliderInput('weekly_risk_events', 'Frequency of risk behavior/week',value = 1, min = 0.2, max = 5, step =0.1),
    sliderInput('pi_0', 'Initial HIV prevalence',value = 0.2, min = 0.01, max = 0.99, step =0.01),
    sliderInput('cost_weekly', 'Intervention cost per person per week',value = 25, min = 1, max = 100, step =1)
  ),
  mainPanel(
    h4('This tool implements a random mixing model of HIV among people who inject drugs(PWID). It is designed for simple risk-reduction policy simulations.'),
    h4('The above sliders allow us to explore the impact of interventions that reduce risk by a fixed relative fraction per event at different frequencies of needle sharing, and different initial HIV prevalences in a posited population of 1,000 PWID. It computes the epidemic trajectory with and without intervention.'),
    h4('Values are calibrated to 11yr mean drug-use career, a discount rate of 3%, efficiency of transmission=0.3% when needle-sharing occurs between HIV-discordant people.'),
    h4('The shiny app does the math under the hood to integrate along the epidemic curve to find the present-discounted-value cost per averted infection of the associated risk-reduction intervention:'),
    verbatimTextOutput('cost_per_averted_infection'),
    plotOutput("g_prev"),
    plotOutput("g_incidence"),
    h3('Supporting documentation'),
    h4('This is a standard random mixing model. For details, see Harold Pollack, Cost-effectiveness of Harm Reduction in Preventing Hepatitis C among Injection Drug Users. Medical Decision Making, (2001) 21(5), 357–367. See also Edward H. Kaplan, A method for evaluating needle exchange programmes. Statist. Med. (1994) 13: 2179-2187.'),
    h4('For more information on syringe exchanges, see Fernandes RM, Cary M, Duarte G, et al. Effectiveness of needle and syringe Programmes in people who inject drugs - An overview of systematic reviews. BMC Public Health. 2017;17(1):309. For broader discussion of epidemiological models, see Roy Anderson and Robert May, Infectious Diseases of Humans:
Dynamics and Control. Oxford University Press, 1992.')
  )
))