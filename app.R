#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(htmltools)
library(tmap)
library(tidyverse)
library(shinythemes)
library(showtext)
setwd(".")

Sys.setlocale("LC_ALL","C")
Sys.setlocale("LC_ALL","Korean")

font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

dong_rds <- readRDS("dong.RDS")
sigun_rds <- readRDS("sigun_list.RDS")
sido_rds <- readRDS("sido_list.RDS")
no_council <- tm_shape(filter(sido_rds,
                              CTP_KOR_NM=="제주특별자치도"|CTP_KOR_NM=="세종특별자치시")) +
  tm_fill(col = "black", alpha = .8)

partyselect = list("정의당", "진보당", "녹색당","노동당", "무소속")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                     
                     # Application title
                     titlePanel("2022년 제8회 전국동시지방선거 출마 현황", windowTitle = "election22"),
                     
                     # Sidebar with a slider input for number of bins 
                     sidebarLayout(
                       sidebarPanel(
                         tags$a(href="http://info.nec.go.kr/", target = "blank"),
                         h4("8회 지방선거에서 진보정당의 출마 현황을 분석하기 위하여 만든 페이지입니다."),
                         h4("지도에 대한 설명과 주의점을 참고하여 이용하시길 바랍니다."),
                         h4("1) 제작자 개인의 부족함으로 인하여 출마현황, 후보자 정보 등의 오류가 있을 수 있습니다.
            수시로 정보를 확인하고 갱신하기 위하여 노력하겠지만 양해를 미리 구합니다."),
                         h4("2) 4월 9일 현재 선거구 획정이 되지 않은 상황이며,
            선거법 개정과 광역의회에서의 획정에 따라서 변동이 예상됩니다."),
                         h4(strong("3) 해당 웹페이지의 무단 이용 및 전재를 금지합니다."),
                            h4("4) 지도의 행정구역 클릭하면, 행정구역 및 해당 지역 출마자의 정보를 확인할 수 있습니다.")),
                         h5("- 현역 : I=현역, E=전직, C=도전자"),
                         h5("- 성별 : F=여성, M=남성"),
                         h5("* 성별 표기는 예비후보 등록 정보에 따르되 후보자/캠프 요청에 따라 변경 가능."),
                         h5("- 청년 : Y=18세~39세, N=40세 이상"),
                         h4("5) 확대 축소 버튼 아래 레이어 창을 통해 배경을 변경할 수 있습니다."), 
                         checkboxGroupInput("정당",
                                            h3(strong("정당을 선택하세요:")),
                                            choices = c("정의당", "진보당", "노동당", "녹색당", "무소속"),
                                            selected = partyselect), br(),
                         h5("최근 업데이트일 : 4월 10일"),
                         h5("출마자 통계, 광역의회 출마 상황 등 업데이트 예정."),
                         h5("추가적인 문의 및 정보 수정 요청을 환영합니다!"), 
                         h4("연락처: 20korando@gmail.com")
                       ),
                       
                       # Show a plot of the generated distribution
                       mainPanel(
                         tabsetPanel(
                           tabPanel("광역단체장", tmapOutput("governor", width = "100%", height = 700)),
                           tabPanel("기초단체장", tmapOutput("mayor", width = "100%", height = 700)),
                           tabPanel("기초의회(지역구)", tmapOutput("council_con", width = "100%", height = 700))
                         )
                       )
                     )
)

server <- function(input, output) {
  partydong <- reactive({
    d <- dong_rds %>% filter(dong_rds$정당 == input$정당)
    return(d)
  })
  partysigungu <- reactive({
    s <- sigun_list %>% filter(sigun_rds$정당 == input$정당)
    return(s)
  })
  
  party_d <- reactive(
    dong_rds[dong_rds$정당 %in% input$정당, ])
  party_s <- reactive(
    sigun_rds[sigun_rds$정당 %in% input$정당, ])
  party_r <- reactive(
    sido_rds[sido_rds$정당 %in% input$정당, ])
  
  
  
  output$council_con <- renderTmap({
    tm_shape(dong_rds) +
      tm_borders(col = "black",lwd = .2) +
      party_d() %>% tm_shape() +
      tm_fill(col = "정당", alpha =.6,
              palette=c(진보당='#F26522', 정의당='#FFED00',
                           녹색당='#5CB32E', 노동당='#FF0000',
                           무소속='#546E7A'),
              id="읍면동",
              popup.vars=
                c("기초의회_출마자", "기초의원선거구", 
                  "현역", "출마횟수", "성별", "청년", "특이사항"),
              popup.format = list(html.escape=FALSE),
              legend.show = F) +
      tm_shape(sido_rds) +
      tm_borders(col = "black",lwd = .6) + 
      no_council +
      tm_add_legend(type="fill", 
                    labels=c('정의당', '진보당', '노동당', '녹색당', '무소속',
                             '기초자치단체가 없는 지역'), 
                    col=c(정의당='#FFED00', 진보당='#F26522',
                             노동당='#FF0000', 녹색당='#5CB32E',
                             무소속='#546E7A', `기초자치단체가 없는 지역`='black'), alpha = .6,
                    border.col = "black", border.lwd = 1, border.alpha = .6,
                    title = "8회(2022년) 지방선거 <br> 기초의원 출마자 <br> (4/9 업데이트)") +
      tm_view(set.view = c(127.900, 36.000, 7))
  })
  
  output$mayor <- renderTmap({
    tm_shape(sigun_rds) +
      tm_borders(col = "black",lwd = .3) +
      party_s() %>% tm_shape() +
      tm_fill(col = "정당", alpha =.6,
              palette=c(정의당='#FFED00', 진보당='#F26522',
                           녹색당='#5CB32E', 노동당='#FF0000',
                           무소속='#546E7A'),
              id="시군구",
              popup.vars=c("출마자", "정당", "성별", "특이사항"),
              popup.format = list(html.escape=FALSE),
              legend.show = F) +
      tm_borders(col = "black", lwd = 1) +
      tm_add_legend(type="fill", 
                    labels=c('정의당', '진보당'), 
                    col=c(정의당='#FFED00', 진보당='#F26522'), alpha = .6,
                    border.col = "black", border.lwd = 1, border.alpha = .6,
                    title = "8회(2022년) 지방선거 <br> 기초단체장 출마자 <br> (4/9 업데이트)") +
      tm_view(set.view = c(127.900, 36.000, 7))
  })
  
  output$governor <- renderTmap({
    tm_shape(sido_rds) +
      tm_borders(col = "black",lwd = .3) +
      party_r() %>% tm_shape() +
      tm_fill(col = "정당", alpha =.6,
              palette=c(진보당='#F26522', 정의당='#FFED00', 녹색당="#5CB32E"),
              id="시도",
              popup.vars=c("출마자", "정당", "성별", "특이사항"),
              popup.format = list(html.escape=FALSE),
              legend.show = F) +
      tm_borders(col = "black",lwd = 1) +
      tm_add_legend(type="fill", 
                    labels=c('정의당', '진보당', '녹색당'), 
                    col=c(정의당='#FFED00', 진보당='#F26522', 녹색당='#5CB32E'), 
                    alpha = .6,
                    border.col = "black", border.lwd = 1, border.alpha = .6,
                    title = "8회(2022년) 지방선거 <br> 광역단체장 출마자 <br> (4/9 업데이트)") +
      tm_view(set.view = c(127.900, 36.000, 7))
  })
}

shinyApp(ui = ui, server = server)


