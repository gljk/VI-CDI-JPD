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
  marker = list(
    size = ~Zajedno/2.5,
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










journalist <- c(75,70,75,5,10,10,20,10,15,10,20)
developer <- c(25,10,20,60,80,90,70,20,5,10,10)
designer <- c(0,20,5,35,10,0,10,70,80,80,70)
label <- c('point 1','point 2','point 3','point 4','point 5','point 6',
           'point 7','point 8','point 9','point 10','point 11')


df <- data.frame(journalist,developer,designer,label)

# axis layout


fig <- df %>% plot_ly()
fig <- fig %>% add_trace(
  type = 'scatterternary',
  mode = 'markers',
  a = ~journalist,
  b = ~developer,
  c = ~designer,
  text = ~label,
  marker = list( 
    symbol = 100,
    color = '#DB7365',
    size = 14,
    line = list('width' = 2)
  )
)
fig <- fig %>% layout(
  title = "Simple Ternary Plot with Markers",
  ternary = list(
    sum = 100,
    aaxis = axis('Journalist'),
    baxis = axis('Developer'),
    caxis = axis('Designer')
  )
)

fig