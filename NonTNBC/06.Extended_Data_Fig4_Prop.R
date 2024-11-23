
df<-table(dismal1K$major_type,dismal1K$patient_id)
df<-as.data.frame(df)
colnames(df)<-c("Type","Patient","Freq")
infor <- dismal1K@meta.data[,c('DFS','patient_id','Subtype')]
rownames(infor) <- NULL
infor <- infor %>% distinct()
df <- left_join(df,infor,by=c("Patient"="patient_id"))
# df$DFS<-factor(df$DFS,levels=c("0","1"),labels = c("Non-dismal","Dismal"))

df2<-table(dismal6K$major_type,dismal6K$patient_id)
df2<-as.data.frame(df2)
colnames(df2)<-c("Type","Patient","Freq")
infor <- dismal6K@meta.data[,c('DFS','patient_id','Subtype')]
rownames(infor) <- NULL
infor <- infor %>% distinct()
df2 <- left_join(df2,infor,by=c("Patient"="patient_id"))

df <- rbind(df,df2)

prop <- function(count, group) {
  count / tapply(count, group, sum)[group]
}


df<-df %>% mutate(prop=prop(Freq, Patient))

df2 <- df[,c("Patient",'DFS')]
df2 <- df2 %>% distinct()


df2$text_color <- ifelse(df2$DFS == "Dismal", '#D9565CFF', '#088BBEFF')
df$DFS<-factor(df$DFS,levels=c("Non-dismal","Dismal"))



df$Patient <- as.character(df$Patient)

df_nondismal_tn <- df %>% filter(Subtype=="TN") %>% filter(DFS=="Non-dismal")
dfs_order <- unique(df_nondismal_tn$Patient)
dfs_order_df <-as.data.frame(dfs_order)
colnames(dfs_order_df)<- c("Var1")

df_dismal_tn <- df %>% filter(Subtype=="TN") %>% filter(DFS=="Dismal")
dfs_order2 <- unique(df_dismal_tn$Patient)
dfs_order_df2 <-as.data.frame(dfs_order2)
colnames(dfs_order_df2)<- c("Var1")

df_nondismal_nn <- df %>% filter(Subtype!="TN") %>% filter(DFS=="Non-dismal")
dfs_order3 <- unique(df_nondismal_nn$Patient)
dfs_order_df3 <-as.data.frame(dfs_order3)
colnames(dfs_order_df3)<- c("Var1")

df_dismal_nt <- df %>% filter(Subtype!="TN") %>% filter(DFS=="Dismal")
dfs_order4 <- unique(df_dismal_nt$Patient)
dfs_order_df4 <-as.data.frame(dfs_order4)
colnames(dfs_order_df4)<- c("Var1")

df_order <-rbind(dfs_order_df,dfs_order_df2,dfs_order_df3,dfs_order_df4)

df_tmp <- df[,c('DFS','Patient')]
df_tmp <- df_tmp %>% distinct()
df_order <- left_join(df_order,df_tmp,by=c("Var1"="Patient"))
df$Patient <- factor(df$Patient, levels = unique(c(dfs_order, dfs_order2,dfs_order3,dfs_order4)))

df$Type <- factor(df$Type,levels=major_order)

# ggplot으로 그래프 그리기
# df$Subtype <- factor(df$Subtype,levels = c("HR+HER2-",'HR+HER2+','HR-HER2+'))
library(RColorBrewer)
my_orange = brewer.pal(n = 9, "YlOrRd")[c(3,5,7)]
p1 <- ggplot(df, aes(x = Patient, y = prop, fill = Type)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c(all_cols)) +
  guides(fill = guide_legend(ncol = 1,order=1))+
  theme_minimal() +
  theme(
    legend.key.size = unit(0.4, "cm"),  # Adjust the size of the legend keys
    legend.text = element_text(size = 10),  #
    axis.text.y = element_blank(),    # Remove y-axis text/labels
    axis.ticks.y = element_blank(),  # This removes the y-axis ticks if desired
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.text.x = element_text(angle = 45, hjust = 0.5) # Rotate x-axis labels
  )+
  labs(title = "", x = "", y = "", fill = "Cell type")+
  new_scale_fill()+
  geom_tile(data = df, aes(x = Patient, y = 1.02, fill = Subtype), 
            height = 0.02, width = 0.9) +
  scale_fill_manual(values = ihc_colors, name = "Subtype")+
  guides(fill = guide_legend(ncol = 1,order=3))+
  new_scale_fill()+
  geom_tile(data = df, aes(x = Patient, y = 1.04, fill = DFS), 
            height = 0.02, width = 0.9) +
  scale_fill_manual(values = response_colors, name = "Disease outcome")+
  guides(fill = guide_legend(ncol = 1,order=5))



pdf(paste0(output_folder, "/Overall_Prop_TN_NonTN.pdf"), width=10, height=6)
p1
dev.off()
