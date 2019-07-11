#
shinyServer(function(input, output,session) {
    
    tab_list <- NULL
    
    relationship_tb<-reactive({
        
        
    tbl(ccdd,sql(paste0(" SELECT *
         FROM public.ccdd_mp_ntp_tm_relationship_release_candidate as ds
         WHERE ds.tm_formal_name =  '",input$select_tm, "'")))%>%collect()
    })
    
    
    output$current_tm<-renderTable({
        tb<-relationship_tb()
        result<-data.frame(names=c('Number of NTPs:','Number of MPs:'),
                           count=c(length(unique(tb$ntp_code)),length(unique(tb$mp_code))))
        
    },include.colnames=FALSE)
    
    
    #add javascript link to shiny from forcenetwork
    Myclickerjs<-"Shiny.setInputValue('node_clicked', d.name);"
    
   
    output$networkplot <- renderForceNetwork({

       tb<-relationship_tb()
       
       nodes<-data.frame(name=unique(c(tb$tm_formal_name,tb$ntp_formal_name,tb$mp_formal_name)))
       nodes$name<-as.character(nodes$name)
       nodes$id<-c(0:(nrow(nodes)-1))
       nodes<-nodes%>%
           mutate(group=case_when(name %in% tb$tm_formal_name ~ 1,
                                  name %in% tb$ntp_formal_name ~ 2,
                                  name %in% tb$mp_formal_name ~ 3))
       
       links<-data.frame(source=c(tb$tm_formal_name,tb$ntp_formal_name),target=c(tb$ntp_formal_name,tb$mp_formal_name))
       links[,]<-lapply(links[,],as.character)
       links$source<-nodes$id[match(links$source,nodes$name)]
       links$target<-nodes$id[match(links$target,nodes$name)]
       
       forceNetwork(Links = links, Nodes = nodes,
                    Source = "source", Target = "target",Group='group',
                    NodeID = "name",opacity = 0.8,
                    fontSize=12,zoom=T,
                    opacityNoHover = T,
                    clickAction = Myclickerjs
       )
       

    })
    
    
    #add tab when a node is clicked
    observeEvent(input$node_clicked,
                 {
                     
                     tab_title <- substr(input$node_clicked,1,20)
                     
                     if(tab_title %in% tab_list == FALSE){
                         
                         
                         
                         query1<-paste0("SELECT dpd_drug_code,din,brand_name_en,ntp_formal_name,mp_status, mp_status_effective_date
                                         FROM public.ccdd_mp_table_candidate as ds
                                         WHERE ds.mp_formal_name =  '",input$node_clicked, "'")
                         
                         tb1<-tbl(ccdd,sql(query1))%>%
                              collect()%>%
                              mutate(dpd_drug_code=as.integer(dpd_drug_code),
                                     mp_status_effective_date=as.character(mp_status_effective_date))
                                    
                         
                         drug_code<-tb1$dpd_drug_code
                         
                         company<-tbl(ccdd,sql(paste0("SELECT company_name
                                         FROM dpd.companies as ds
                                         WHERE ds.drug_code =  ",drug_code)))%>%collect()
                         
                         combo<-tbl(ccdd,sql(paste0("SELECT *
                                         FROM ccdd.combination_products_csv as ds
                                         WHERE ds.drug_code =  ",drug_code)))%>%collect()
                         
                         if(is.null(combo)){
                             combination<-data.frame(combo='Combination Product',type='Yes')
                         }else{
                             combination<-data.frame(combo='Combination Product',type='No')
                         }
                         
                         dosage<-tbl(ccdd,sql(paste0(" SELECT dosage_form
                                         FROM public.ccdd_drug_dosage_form as ds
                                         WHERE ds.dpd_drug_code =  ",drug_code)))%>%collect()
                         
                         route<-tbl(ccdd,sql(paste0(" SELECT route_of_administration
                                         FROM dpd.route as ds
                                         WHERE ds.drug_code =  ",drug_code)))%>%collect()
                         
                         form<-tbl(ccdd,sql(paste0(" SELECT pharmaceutical_form
                                         FROM dpd.pharmaceutical_form as ds
                                         WHERE ds.drug_code =  ",drug_code)))%>%collect()
                         
                         uofp<-tbl(ccdd,sql(paste0("SELECT unit_of_presentation, uop_size, uop_unit_of_measure, calculation
                                         FROM ccdd.unit_of_presentation_csv as ds
                                         WHERE ds.drug_code =  ",drug_code)))%>%collect()
                         
                         if(is.null(uofp)){
                             uofp2<-data.frame(unit_of_presentation='Not available')
                         }else{
                             uofp2<-uofp
                         }
                         
                         ing<-tbl(ccdd,sql(paste0(" SELECT ingredient, strength, strength_unit, dosage_value, dosage_unit
                                         FROM dpd.active_ingredient as ds
                                         WHERE ds.drug_code =  ",drug_code)))%>%collect()
                         
                         status<-tbl(ccdd,sql(paste0(" SELECT current_status_flag, status, history_date 
                                         FROM dpd.status as ds
                                         WHERE ds.drug_code =  ",drug_code)))%>%
                                         collect()%>%
                                         mutate(history_date=as.character(history_date))%>%
                                         arrange(current_status_flag)
                         
                         appendTab(inputId = "tabs",
                                   tabPanel(
                                       tab_title,
                                       HTML('<b> This Page displays all information from Drug Product Database and CCDD source tables that contribute to 
                                                 generation of CCDD concept </b> <br><br>'),
                                       HTML(paste0('<b> MP Formal Name: </b> ',input$node_clicked, '<br><br>')),
                                       renderTable(tb1),
                                       renderTable(company),
                                       renderTable(combination,colnames=F),
                                       renderTable(dosage),
                                       renderTable(route),
                                       renderTable(form),
                                       renderTable(uofp2),
                                       renderTable(ing),
                                       renderTable(status)
                                   ))
                         
                         tab_list <<- c(tab_list, tab_title)
                         
                     }
                     
                     updateTabsetPanel(session, "tabs", selected = tab_title)
                     
                 })
    
    
    observeEvent(input$remove,{
        # Use purrr's walk command to cycle through each
        # panel tabs and remove them
        tab_list %>%
            walk(~removeTab("tabs", .x))
        tab_list <<- NULL
    })

})
