library(plotly)

Austrija<- read.csv("JPD-Austria.csv")
Nemačka <- read.csv("JPD-Nemačka.csv")
Švajcarska<- read.csv("JPD-Švajcarska.csv")


jpdData<- left_join(left_join(Austrija %>% select(Code, Godina, Austrija), Nemačka %>%  select(Code, Godina, Nemačka), by=c("Code", "Godina")),Švajcarska %>% select(Code, Godina, Švajcarska),by=c("Code", "Godina"))

jpdData$Zajedno <- (jpdData$Austrija+jpdData$Nemačka+jpdData$Švajcarska)
jpdData$Intenzitet =(100- jpdData$Zajedno) /100

regioni<- read.xlsx("sifarnici-mesta-opstine-okruzi-delatnosti.xlsx", sheet = "Regioni")
opstine<- read.xlsx("sifarnici-mesta-opstine-okruzi-delatnosti.xlsx", sheet = "Opština")

jpdData<- left_join(jpdData, regioni)
jpdData<- left_join(jpdData, opstine, by=c("Code"="OpstinaSifra"))


axis <- function(title) {
  list(
    title = title
  )
}

fig <- jpdData %>% plot_ly() %>% 
  add_trace(
    type = 'scatterternary',
    mode = 'markers',
    uid= ~OpstinaNazivLat,
    #ids= ~OpstinaNazivLat,
    a = ~Austrija,
    b = ~Nemačka,
    c = ~Švajcarska,
    frame=~Godina,
    text = ~paste0("<b>",OpstinaNazivLat,"</b>\n","Autrija: ", format(Austrija, digits=1,nsmall=1, decimal.mark=","),"%\nNemačka: ", format(Nemačka,digits=1, nsmall=1, decimal.mark=","),"%\nŠvajcarska: ", format(Švajcarska,digits=1, nsmall=1, decimal.mark=","),"%","\nDruge zemlje: ", format(100-Austrija-Nemačka-Švajcarska,digits=1, nsmall=1, decimal.mark=","),"%"),
    color = ~Region,
    size = ~Zajedno^5,
    sizes=c(1, 500),
    marker = list(
      line=list(color= '#f3f3f3', width=1),
      opacity= ~0.3+Intenzitet*2),
    hovertemplate = paste0("%{text}")
  )  %>% layout(
    title = NULL,
    ternary = list(
      sum = 100,
      aaxis = list(showticklabels=FALSE, title="Austrija"),
      baxis = list(showticklabels=FALSE, title="Nemačka"),
      caxis =list(showticklabels=FALSE, title="Švajcarska")
    )
  )

fig%>% animation_opts(frame = 1500,transition = 1500) %>% animation_button(label = "Pokreni")



