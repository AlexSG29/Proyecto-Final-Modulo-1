# Instala los paquetes si no los tienes
# install.packages(c("shiny", "ggplot2", "plotly", "dplyr", "lubridate"))

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(readxl)


df_final <- read_xlsx("df_final.xlsx")

ui <- fluidPage(
  titlePanel("Dashboard de Fallas en el Sistema de Transporte"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ruta", "Selecciona una ruta:", choices = c("Todas", unique(df_final$ruta))),
      selectInput("grafico", "Tipo de gráfico:",
                  choices = c("Fallas en el tiempo", "Fallas por sistema reportado",
                              "Fallas por tipología", "Distribución de costos",
                              "Boxplot de costo por sistema", "Usuarios por ruta",
                              "Pasajeros por viaje", "Retrasos y varados",
                              "Kilometraje por tipología")),
      dateRangeInput("rango_fecha", "Rango de fechas:", 
                     start = min(df_final$fecha), end = max(df_final$fecha))
    ),
    mainPanel(
      plotlyOutput("grafico_salida")
    )
  )
)

server <- function(input, output) {
  
  datos_filtrados <- reactive({
    df <- df_final %>%
      mutate(fecha = as.Date(fecha)) %>%
      filter(fecha >= input$rango_fecha[1] & fecha <= input$rango_fecha[2])
    
    if (input$ruta != "Todas") {
      df <- df %>% filter(ruta == input$ruta)
    }
    df
  })
  
  output$grafico_salida <- renderPlotly({
    df <- datos_filtrados()
    
    p <- switch(input$grafico,
                
                "Fallas en el tiempo" = df %>%
                  mutate(fecha = as.Date(fecha)) %>%
                  group_by(fecha) %>%
                  summarise(n = n()) %>%
                  ggplot(aes(x = fecha, y = n)) +
                  geom_line(color = "orange") +  
                  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +  
                  theme_classic() +
                  labs(x = "Fecha", y = "Cantidad de registros",title = "Cantidad de fallas por día") +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)),
                
                "Fallas por sistema reportado" = df %>% 
                  group_by(sistema) %>% 
                  summarise(frec = n()) %>% 
                  arrange(desc(frec)) %>% 
                  ggplot(aes(x = reorder(sistema,frec) ,y= frec))+
                  geom_bar(stat = "identity", position = "dodge", fill = "green")+
                  coord_flip() +
                  theme_classic()+
                  xlab("Numero de fallas") +
                  ylab("Rutas")+ 
                  labs(title = "Fallas por sistema reportado"),
                
                "Fallas por tipología" = ggplot(df, aes(x = tipologia)) +
                  geom_bar(fill = "tomato") + labs(x= "Tipologia",
                                                   y="Numero de fallas",
                                                   title = "Distribución por tipología"),
                
                "Distribución de costos" = df%>% 
                  mutate(mes_falla = month(fecha, label = TRUE)) %>%  
                  group_by(year_falla, mes_falla) %>%
                  summarise(total_costo = sum(costo_x_perdida, na.rm = TRUE)/1000000, .groups = "drop") %>%
                  ggplot(aes(x = mes_falla, y = total_costo, group = year_falla, color = as.factor(year_falla))) +
                  geom_line(size = 1) + 
                  geom_point() +
                  theme_classic() +
                  labs(x = "Mes", y = "Costo Total en millones ", color = "Año") +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) ,
                
                "Boxplot de costo por sistema" = ggplot(df %>% mutate(costo_x_perdida = costo_x_perdida/1000000), aes(x = sistema, y = costo_x_perdida)) +
                  geom_boxplot(fill = "orange") +
                  coord_flip() + 
                  theme_classic()+
                  labs(x= "Sistemas",y= "Costo de barada en millones de pesos",title = "Boxplot de costos por sistema"),
                
                "Usuarios por ruta" = ggplot(df %>% arrange(desc(total_usuarios_por_ruta)),aes(x = ruta, y = total_usuarios_por_ruta))+ 
                  geom_col(fill = "purple") + 
                  coord_flip() + 
                  theme_classic()+
                  labs(x = "Ruta",y="Numero de ususarios potenciales",title = "Total de usuarios perdidos por ruta"),
              
                "Pasajeros por viaje" = ggplot(df, aes(x = num_min_pasajeros, y = num_max_pasajeros)) +
                  geom_point(color = "darkblue") +
                  theme_classic() +
                  labs(title = "Pasajeros por viaje (mín vs máx)"),
                
                "Retrasos y varados" =  df %>% 
                  group_by(vehiculo) %>% 
                  summarise(frec = n()) %>% 
                  arrange(desc(frec)) %>%
                  head(10) %>% 
                  ggplot(aes(x = reorder(vehiculo,frec) ,y= frec))+
                  geom_bar(stat = "identity", position = "dodge", fill = "orange")+
                  coord_flip() +
                  theme_classic()+
                  xlab("Numero de fallas") +
                  ylab("Frecuencia de fallas"),
                
                "Kilometraje por tipología" = ggplot(df, aes(x = tipologia, y = kilometraje)) +
                  geom_boxplot(fill = "goldenrod") + 
                  coord_flip() + 
                  labs(y="",title = "Kilometraje por tipología")+
                  theme_classic()
    )
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
