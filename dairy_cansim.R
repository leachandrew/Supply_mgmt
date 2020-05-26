#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/energy_cansim")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/energy_cansim")
print(getwd())

#base templates
source("../andrew_base.R")
library(cansim)


################################
######## Employment ###########
################################


data_fetch<-function(id){get_cansim(id)%>% clean_names()%>% mutate(geo=as.factor(geo),
                                                             date=ymd(paste(ref_date,"01",sep="-")),
                                                             year=year(date))}


milk_data <- data_fetch(3210011301) %>% filter(dairy_distribution=="Milk production, total") %>%
  select(date,year,geo,commodity=dairy_distribution,uom,scalar_factor,value)

milk_data_2 <- data_fetch(32100113) %>% filter(dairy_distribution=="Milk production, total") %>%
  select(date,year,geo,commodity=dairy_distribution,uom,scalar_factor,value)


butter_data<-data_fetch(3210011101)%>% filter(commodity=="Creamery butter")%>%
  select(date,year,geo,commodity,uom,scalar_factor,value)

dairy_data<-data_fetch(3210011201)%>% filter(commodity=="Cheddar cheese") %>%
  select(date,year,geo,commodity,uom,scalar_factor,value)

  
egg_data<-data_fetch(32100121)  %>% filter(production_and_disposition=="Production of eggs in shell  [116111]") %>%
  select(date,year,geo,commodity=production_and_disposition,uom,scalar_factor,value) %>%
  mutate(commodity="Production of eggs in shell")
  
 ag_data<-bind_rows(milk_data,butter_data,dairy_data,egg_data)%>% mutate(commodity=as_factor(commodity))%>%
   group_by(year,geo,commodity,uom,scalar_factor) %>% summarize(value=sum(value,na.rm = T)) %>%
   group_by(geo,commodity,uom,scalar_factor) %>%
   mutate(value_1960=sum(value*(year==1960)))




poultry_data<-data_fetch(32100117)%>% mutate(date=ymd(paste(ref_date,"-12-31",sep="")),year=year(date))%>%
  filter(commodity=="Chicken (including stewing hen)",production_and_disposition=="Production, total",
         uom=="Kilograms")



png<-1
if(png==1)#set these to only turn on if you're making PNG graphs
  png(file="supply_mgmt.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(filter(ag_data,geo=="Canada",year<=1980,year>=1960))+
  geom_line(aes(year,value/value_1960*100,group=commodity),size=1.5)+
  scale_colour_manual("",values=colors_tableau10())+
  scale_fill_manual("",values=colors_tableau10())+
  #scale_x_date(date_breaks = "5 years",date_labels = "%b\n%Y")+
  theme_minimal()+
  scale_y_continuous(breaks = pretty_breaks())+
  facet_wrap(~commodity,scales="free_y")+
  #expand_limits(x=ymd("2008-01-01"))+
  #guides(fill = guide_legend(nrow = 3))+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 8, face = "bold"),
    plot.caption=element_text(hjust = 0)
    )+
  labs(x="",y="Indexed production (1960=100)",
       title="Supply Managed Sectot Production Data",
       #subtitle="Canadian Refinery Production",
       caption="Source: CANSIM tables 32-10-00111, 32-10-00112,32-10-00113,32-10-00121, graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()
