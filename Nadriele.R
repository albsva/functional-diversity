a = read.table("clipboard", head = T, dec=",", sep = "\t")
a

shapiro.test(a$Multi)
#' Normal: IndT, IndN, SpNat, Serrap, Abertura, 

vars = c("IndN", "SpNat", "Serrap", "Cobertura","Multi")

for (v in vars) {
  cat("\n### Variável:", v, "###\n")
  modelo = aov(as.formula(paste(v, "~ Classe")), data = a)
  print(summary(modelo))
}

# Tem diferença: IndN, SpNat, Abertura.

library(car)
leveneTest(Multi ~ Classe, data = a)
## MESMA LÓGICA DA PERMDISP
## Tudo certo!

for (v in vars) {
  cat("\n### Variável:", v, "###\n")
  
  modelo = aov(as.formula(paste(v, "~ Classe")), data = a)
  print(summary(modelo))
  
  cat("\n--- Teste de Tukey ---\n")
  print(TukeyHSD(modelo))
}


######
######

library(reshape2)  # ou tidyr
library(dplyr)

# Supondo que suas variáveis estão em vars
vars = c("IndN", "SpNat", "Serrap", "Cobertura", "Multi")

# Transformando em long format
a_long = a %>%
  select(Classe, all_of(vars)) %>%
  melt(id.vars = "Classe", variable.name = "Variavel", value.name = "Valor")

a_long$Classe = factor(a_long$Classe,
                       levels = c("Recent", "Interm.", "Old"))

library(multcompView)

# Função para gerar letras
get_letters = function(var_name){
  modelo = aov(as.formula(paste(var_name, "~ Classe")), data = a)
  tukey = TukeyHSD(modelo)
  pvals <- tukey$Classe[, "p adj"]
  letters <- multcompLetters(pvals, threshold = 0.05)$Letters
  return(data.frame(Classe = names(letters), Letras = letters, Variavel = var_name))
}

# Aplicando para todas as variáveis
letters_df = do.call(rbind, lapply(vars, get_letters))

# Aplicando para todas as variáveis
letters_plot = letters_df %>%
  mutate(Valor = NA)  # vamos definir depois a posição do texto

letters_plot$Classe = factor(letters_plot$Classe,
                             levels = c("Recent", "Interm.", "Old"))

max_values = a_long %>%
  group_by(Variavel, Classe) %>%
  summarise(Max = max(Valor, na.rm = TRUE), .groups = "drop")

letters_plot = left_join(letters_plot, max_values, by = c("Variavel", "Classe")) %>%
  mutate(Valor = Max + 0.05*Max)  # ajustar posição do texto acima do boxplot

# Plot final
library(ggplot2)

# Renomear variáveis para rótulos mais descritivos
a_long$Variavel = recode(a_long$Variavel,
                          "Cobertura" = "Canopy cover (%)",
                          "IndN" = "Native tree density (Ind./ha)",
                          "Serrap" = "Litter (g)",
                          "SpNat" = "Native species richness",
                         "Multi" = "Multifunctionality"
)

letters_plot$Variavel = recode(letters_plot$Variavel,
                                "Cobertura" = "Canopy cover (%)",
                                "IndN" = "Native tree density (Ind./ha)",
                                "Serrap" = "Litter (g)",
                                "SpNat" = "Native species richness",
                                "Multi" = "Multifunctionality"
)

# Paleta de cores personalizada para Classe
cores = c("Old" = "#8B0000",      # vermelho escuro
           "Interm."    = "#228B22",      # verde comum
           "Recent"   = "#C2A400")      # amarelo mostarda

# Plot final estilizado
c = ggplot(a_long, aes(x = Classe, y = Valor, fill = Classe)) +
  geom_boxplot(alpha = 0.8, color = "black", outlier.shape = 21, outlier.fill = "white") +
  geom_text(data = letters_plot, aes(x = Classe, y = Valor, label = Letras),
            inherit.aes = FALSE, size = 4.2, fontface = "bold") +
  # geom_hline(yintercept = 50, color = "purple", linetype = "solid", size = 0.7) +
  facet_wrap(~ Variavel, scales = "free_y") +
  scale_fill_manual(values = cores) +
  theme_classic(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white"),
    legend.position = "none"
  ) +
  ylab("Ecological indicator") +
  xlab("Class")
c


ggsave("Fig2.jpeg", plot = c, width = 10, height = 6, dpi = 400)
library(magick)
img = image_read("Fig2.jpeg")                     # Lê a imagem original
image_write(img, path = "Fig2.jpeg", density = 400)

#####
####

## LM
names(a)
b = lm(Multi ~ Rank, data = a)
summary(b)

# Definir cores para cada Classe
cores = c("Recent" = "blue", "Interm." = "green", "Old" = "red")

# Gráfico de dispersão com cores por Classe - EXPLORATÓRIO
plot(a$Multi, a$Rank,
     xlab = "Multi",
     ylab = "Rank",
     pch = 19,
     col = cores[a$Classe])

# Adiciona a linha de regressão
abline(b, col = "black", lwd = 2)

######
######
plot(b)
# Identifiquei o mais longe dos resíduos no Q-Q plot (longe dos 45° na linha)

rstud = rstudent(b)
# Autores lá do livro = corte > 2.

# REFAZER O MODELO, MAS SEM O PLOT 3 AGORA..

AIC(b)


### Gráfico real
# SEM O SAF 3 (RSTUDENT = 4.ALGO)
qq = ggplot(a, aes(x = Rank, y = Multi)) +
  geom_point(size = 2.5, color = "#8B0000", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#C2A400", se = TRUE, fill = "#C2A400", alpha = 0.1) +
  labs(
    x = "Compliance index",
    y = "Multifunctionality",
    title = " "
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank()
  )
qq


#Salvar
ggsave("Fig3.jpeg", plot = qq, width = 8, height = 5, dpi = 400)
library(magick)
img = image_read("Fig3.jpeg")                     # Lê a imagem original
image_write(img, path = "Fig3.jpeg", density = 300)

cor(a$Multi,a$Rank)


library(ggplot2)
str(dados)
dados$SAF = as.character(dados$SAF)
# ---- Gráfico 1: Cobertura ----
g1 = ggplot(dados, aes(x = SAF, y = Cobertura, color = Classe)) +
  geom_point(size = 2.8, shape = 19) +  # shape = 17 (triângulo); pode testar outros: 15, 16, 18, etc.
  geom_hline(yintercept = 50, linetype = "dashed", color = "black", size = 0.7) +  # linha mais grossa
  scale_color_manual(values = c("Old" = "#8B0000", "Recent" = "#228B22", "Interm." = "#C2A400")) +
  labs(y = "CC (%)", x = "Agroforestry system") +
  theme_classic(base_size = 13)+
  theme(legend.position = "none") 

g1


# ---- Gráfico 3: SpNat ----
g3 <- ggplot(dados, aes(x = SAF, y = SpNat, color = Classe)) +
  geom_point(size = 2.8, shape = 19) +  # shape = 17 (triângulo); pode testar outros: 15, 16, 18, etc.
  geom_hline(yintercept = 10, linetype = "dashed", color = "black", size = 0.7) +  # linha mais grossa
  scale_color_manual(name = "Class", values = c("Old" = "#8B0000", "Recent" = "#228B22", "Interm." = "#C2A400")) +
  labs(y = "NSR", x = "Agroforestry system") +
  theme_classic(base_size = 13)

g3

# ---- Gráfico 4: IndN ----
g4 <- ggplot(dados, aes(x = SAF, y = IndN, color = Classe)) +
  geom_point(size = 2.8, shape = 19) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "#C2A400", size = 0.8) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "#228B22", size = 0.8) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "#8B0000", size = 0.8) +
  scale_color_manual(values = c("Old" = "#8B0000", "Recent" = "#228B22", "Interm." = "#C2A400")) +
  labs(y = "NID", x = "Agroforestry system") +
  theme_classic(base_size = 13)+
  theme(legend.position = "none") 

g4

legend <- get_legend(g3)
g3_nolegend <- g3 + theme(legend.position = "none")

final_plot <- plot_grid(
  plot_grid(g1, g3_nolegend, g4, nrow = 3, labels = c("A", "B", "C")),
  legend,
  ncol = 2,
  rel_widths = c(1, 0.15)  # ajusta o espaço da legenda
)

final_plot


ggsave("meu_gridY.jpeg", bg = "white", plot = final_plot,
       width = 8, height = 6, dpi = 300)

library(magick)
img <- image_read("meu_gridY.jpeg")                     # Lê a imagem original
image_write(img, path = "figura_corrigida.jpeg", density = 300)


########3

#######

a = read.table("clipboard", head = T, dec=",", sep = "\t")
b = lm(Multi ~ Rank, data = a)
summary(b)
# Quero fazer um plot bonito desse lm
library(ggplot2)

q = ggplot(a, aes(x = Rank, y = Multi)) +
  geom_point(size = 2.5, color = "#8B0000", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#C2A400", se = TRUE, fill = "#C2A400", alpha = 0.2) +
  labs(
    x = "Compliance index",
    y = "Multifunctionality",
    title = " "
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank()
  )

ggsave("Graph_15_01_2026.jpeg", bg = "white", plot = q,
       width = 10, height = 6, dpi = 400)

library(magick)
img <- image_read("Graph_15_01_2026.jpeg")                     # Lê a imagem original
image_write(img, path = "Graph_15_01_2026.jpeg", density = 400)
