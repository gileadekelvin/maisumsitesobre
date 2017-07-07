---
title: "Filmes de Christian Bale"
subtitle: "Problema 3 - Checkpoint 1"
author: "Gileade Kelvin"
layout: post
published: true
excerpt_separator: ""
---




{% highlight r %}
library(dplyr)
library(rvest)
library(tibble)
library(ggplot2)
library(ggdendro)
{% endhighlight %}



{% highlight text %}
## Error in library(ggdendro): there is no package called 'ggdendro'
{% endhighlight %}



{% highlight r %}
library(cluster)
library(gridExtra)
library(highcharter)
theme_set(theme_minimal())
library(RColorBrewer)
{% endhighlight %}

## Conjunto de dados

Na análise de hoje buscaremos identificar quais os grupos de filmes do ator **Christian Bale**. Os grupos serão definidos quanto filmes de um mesmo grupo tem características semelhantes entre si mas que diferem de outros grupos.

Iremos utilizar os dados do Rotten Tomatoes, site conhecido mundialmente por seus reviews de filmes e pelo **tomatômetro** que é uma medida de 0 a 100 do nível de frescor ou podridão do tomate/filme. Foram coletados 20 filmes com informações sobre BOX OFFICE e RATING(Tomatômetro).


{% highlight r %}
from_page <- read_html("https://www.rottentomatoes.com/celebrity/christian_bale/") %>% 
    html_node("#filmographyTbl") %>% # A sintaxe da expressão é de um seletor à lá JQuery: https://rdrr.io/cran/rvest/man/htmhttps://www.rottentomatoes.com/celebrity/denzel_washington/l_nodes.html 
    html_table(fill=TRUE) %>% # Faz parse
    as.tibble()

filmes = from_page %>% 
    filter(RATING != "No Score Yet", 
           `BOX OFFICE` != "—", 
           CREDIT != "Executive Producer") %>%
    mutate(RATING = as.numeric(gsub("%", "", RATING)), 
           `BOX OFFICE` = as.numeric(gsub("[$|M]", "", `BOX OFFICE`))) %>% 
    filter(`BOX OFFICE` >= 1) # Tem dois filmes que não parecem ter sido lançados no mundo todo
{% endhighlight %}

## Variáveis

Iremos considerar duas variáveis para o agrupamento: **BOX OFFICE** que é a bilheteria nos EUA que o filme arrecadou, **RATING** que é o nível de frescor do tomate, 0 o tomate/filme está podre enquanto 100 o tomate/filme está fresco.


{% highlight r %}
p1 <- filmes %>%
  ggplot(aes(x = RATING)) +
  geom_histogram(binwidth = 5, fill = "#E65100") +
  labs(x = "Rating/Tomatômetro", y = "Número de filmes")

p2 <- filmes %>%
  ggplot(aes(x = `BOX OFFICE`)) +
  geom_histogram(binwidth = 50, fill = "#E65100") +
  labs(x = "Box Office em Milhões de dólares", y = "Número de filmes")

grid.arrange(p1, p2, ncol=2)
{% endhighlight %}

<img src="/portfolio/figure/source/bale-agrupamento/2017-06-27-agrupamento-p3cp1/unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display: block; margin: auto;" />
Na primeira visualização vemos a maioria dos filmes de Bale tem bons indíces no tomatômetro embora ainda existam alguns que não são tão bons assim. Na segunda visualização percebemos que a maioria dos filmes encontra-se entre 0 e 200 milhões de dólares no Box Office e apenas dois filmes ultrapassam os 400 milhões de dólares.


{% highlight r %}
filmes %>%
    hchart("scatter", hcaes(x = RATING, y = `BOX OFFICE`)) %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Rating vs Box Office", align = "center") %>%
    hc_xAxis(title = list(text = "Rating (tomatômetro)")) %>%
    hc_yAxis(title = list(text = "Box Office em Milhões de dólares")) %>%
    hc_tooltip(pointFormat = "{point.TITLE} <br> Rating: {point.x} <br> Box Office: {point.y}")
{% endhighlight %}

<div class="figure" style="text-align: center">
<!--html_preserve--><div id="htmlwidget-720f30395447f2998ea8" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-720f30395447f2998ea8">{"x":{"hc_opts":{"title":{"text":"Rating vs Box Office","align":"center"},"yAxis":{"title":{"text":"Box Office em Milhões de dólares"},"type":"linear"},"credits":{"enabled":false},"exporting":{"enabled":false},"plotOptions":{"series":{"turboThreshold":0,"showInLegend":false,"marker":{"enabled":true}},"treemap":{"layoutAlgorithm":"squarified"},"bubble":{"minSize":5,"maxSize":25},"scatter":{"marker":{"symbol":"circle"}}},"annotationsOptions":{"enabledButtons":false},"tooltip":{"delayForDisplay":10,"pointFormat":"{point.TITLE} <br> Rating: {point.x} <br> Box Office: {point.y}"},"series":[{"group":"group","data":[{"RATING":50,"TITLE":"The Promise","CREDIT":"Chris Myers","BOX OFFICE":8.2,"YEAR":2017,"x":50,"y":8.2},{"RATING":93,"TITLE":"American Hustle","CREDIT":"Irving Rosenfeld","BOX OFFICE":99.2,"YEAR":2013,"x":93,"y":99.2},{"RATING":53,"TITLE":"Out of the Furnace","CREDIT":"Rodney Baze, Jr.","BOX OFFICE":8.4,"YEAR":2013,"x":53,"y":8.4},{"RATING":87,"TITLE":"The Dark Knight Rises","CREDIT":"Bruce Wayne/Batman","BOX OFFICE":448.2,"YEAR":2012,"x":87,"y":448.2},{"RATING":90,"TITLE":"The Fighter","CREDIT":"Dicky Eklund","BOX OFFICE":93.6,"YEAR":2010,"x":90,"y":93.6},{"RATING":68,"TITLE":"Public Enemies","CREDIT":"Melvin Purvis","BOX OFFICE":97,"YEAR":2009,"x":68,"y":97},{"RATING":33,"TITLE":"Terminator Salvation","CREDIT":"John Connor","BOX OFFICE":125.3,"YEAR":2009,"x":33,"y":125.3},{"RATING":94,"TITLE":"The Dark Knight","CREDIT":"Batman/Bruce Wayne","BOX OFFICE":533.4,"YEAR":2008,"x":94,"y":533.4},{"RATING":76,"TITLE":"I'm Not There","CREDIT":"Jack Rollins/Pastor John","BOX OFFICE":4,"YEAR":2007,"x":76,"y":4},{"RATING":89,"TITLE":"3:10 to Yuma","CREDIT":"Dan Evans","BOX OFFICE":53.6,"YEAR":2007,"x":89,"y":53.6},{"RATING":91,"TITLE":"Rescue Dawn","CREDIT":"Dieter Dengler","BOX OFFICE":5.4,"YEAR":2007,"x":91,"y":5.4},{"RATING":48,"TITLE":"Harsh Times","CREDIT":"Jim Luther Davis\n                                    \n                                            Executive Producer","BOX OFFICE":3.4,"YEAR":2006,"x":48,"y":3.4},{"RATING":76,"TITLE":"The Prestige","CREDIT":"Alfred Borden","BOX OFFICE":53.1,"YEAR":2006,"x":76,"y":53.1},{"RATING":84,"TITLE":"Batman Begins","CREDIT":"Bruce Wayne/Batman","BOX OFFICE":204.2,"YEAR":2005,"x":84,"y":204.2},{"RATING":62,"TITLE":"The New World","CREDIT":"John Rolfe","BOX OFFICE":12.5,"YEAR":2005,"x":62,"y":12.5},{"RATING":77,"TITLE":"The Machinist","CREDIT":"Trevor Reznik","BOX OFFICE":1,"YEAR":2004,"x":77,"y":1},{"RATING":68,"TITLE":"Laurel Canyon","CREDIT":"Sam","BOX OFFICE":3.6,"YEAR":2003,"x":68,"y":3.6},{"RATING":40,"TITLE":"Reign of Fire","CREDIT":"Quinn Abercromby","BOX OFFICE":43,"YEAR":2002,"x":40,"y":43},{"RATING":29,"TITLE":"Captain Corelli's Mandolin","CREDIT":"Mandras","BOX OFFICE":25.3,"YEAR":2001,"x":29,"y":25.3},{"RATING":68,"TITLE":"Shaft","CREDIT":"Walter Wade Jr.","BOX OFFICE":70.4,"YEAR":2000,"x":68,"y":70.4}],"type":"scatter"}],"xAxis":{"type":"linear","title":{"text":"Rating (tomatômetro)"}}},"theme":{"colors":["#d35400","#2980b9","#2ecc71","#f1c40f","#2c3e50","#7f8c8d"],"chart":{"style":{"fontFamily":"Roboto"}},"title":{"align":"left","style":{"fontFamily":"Roboto Condensed","fontWeight":"bold"}},"subtitle":{"align":"left","style":{"fontFamily":"Roboto Condensed"}},"legend":{"align":"right","verticalAlign":"bottom"},"xAxis":{"gridLineWidth":1,"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"yAxis":{"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"plotOptions":{"line":{"marker":{"enabled":false},"states":{"hover":{"lineWidthPlus":1}}},"spline":{"marker":{"enabled":false},"states":{"hover":{"lineWidthPlus":1}}},"area":{"marker":{"enabled":false},"states":{"hover":{"lineWidthPlus":1}}},"areaspline":{"marker":{"enabled":false},"states":{"hover":{"lineWidthPlus":1}}}}},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":["Roboto","Roboto+Condensed"],"debug":false},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
<p class="caption">plot of chunk unnamed-chunk-4</p>
</div>
Acima podemos observar que não existem uma correlaçao linear clara entre as duas variáveis. Ainda é possível identificar *outliers* como The Dark Knight e Dark Knight Rises, maiores Box Offices do ator, e Captain Corelli's Mandolin e Terminator Salvation, menores Ratings do ator. Olhando para esse gráfico de dispersão é possível imaginar que alguns grupos poderão ser formados, por exemplo, existe um grupo que arrecadou "pouco" e tem um baixo índice no tomatômetro. É possível também visualizar um grupo de grandes sucessos em termos de crítica.

## Agrupamento 

Agora que conhecemos as variáveis e quais são alguns filmes que Christian Bale participou vamos agrupá-los da melhor forma de modo que que filmes parecidos entre si fiquem em um mesmo grupo e que esses grupos sejam diferentes entre si.

A técnica para o agrupamento usada é o agrupamento hierárquico, no qual na variável BOX OFFICE foi aplicada o log de 10, uma vez não estamos interessados nas diferenças absolutas das rendas dos filmes. Além disso, as duas variáveis foram padronizadas para uma mesma escala e com a mesma unidade (desvios-padrão). O método de distância utilizado foi o euclidean e o de agrupamento foi o ward.D.


{% highlight r %}
agrupamento_h_2d = filmes %>% 
    column_to_rownames("TITLE") %>%
    select(RATING, `BOX OFFICE`) %>% 
    mutate(`BOX OFFICE` = log10(`BOX OFFICE`)) %>%
    mutate_all(funs(scale)) %>% 
    dist(method = "euclidean") %>% 
    hclust(method = "ward.D")
{% endhighlight %}

A função abaixo é responsável por plotar várias visualizações com n grupos diferentes.

{% highlight r %}
plota_hclusts_2d = function(agrupamento,
                            dados_filme,
                            nome_colunas, # coluna usada para distâncias
                            dist_method = "euclidean", 
                            linkage_method = "complete", 
                            ks = 1:9){
    #' Retorna um ggplot das soluções de agrupamento de `dados_filme` 
    #' para as quantidades de grupos em `ks` usando `hclust`.
    library(ggplot2)
    library(dplyr, warn.conflicts = F)
    
    atribuicoes = tibble(k = ks) %>% 
        group_by(k) %>% 
        do(cbind(filmes, 
                 grupo = as.character(cutree(agrupamento, .$k)))) 
    
    atribuicoes %>% 
        ggplot(aes_string(x = nome_colunas[1], y = nome_colunas[2], colour = "grupo")) + 
        geom_jitter(width = .02, height = 0, size = 2, alpha = .6) + 
        facet_wrap(~ paste(k, " grupos")) + 
        xlab("") +
        ylab("Box Office (log 10)") +
        scale_color_brewer(palette='Set2') %>% 
        return()
}
{% endhighlight %}

### Escolha dos grupos

Abaixo podemos visualizar como seria a divisão dos filmes em grupos considerando de 1 a 8 grupos distintos.

{% highlight r %}
filmes2 = filmes %>% mutate(`BOX OFFICE` = log10(`BOX OFFICE`))

plota_hclusts_2d(agrupamento_h_2d, 
                 filmes2, 
                 c("RATING", "`BOX OFFICE`"), 
                 linkage_method = "ward.D", ks = 1:8) + scale_y_log10()
{% endhighlight %}

<img src="/portfolio/figure/source/bale-agrupamento/2017-06-27-agrupamento-p3cp1/unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" style="display: block; margin: auto;" />
Estamos visualizando a variável Box Office em escala de log pois não estamos interessados no valor absoluto da bilheteria em si mas na grandeza do quanto ela cresce com relação a outros filmes. 

Optamos por escolher a divisão em 4 grupos por estar em conformidade com a ideia inicial ao observarmos o gráfico de dispersão dos filmes. Mais tarde vamos explorar melhor esses 4 grupos.

### Silhouette

Uma visualização e métrica que usamos para escolher a divisão em 4 grupos é mostrada abaixo. Esse gráfico tem o objetivo de descrever quão parecidos são os componentes dentro de um grupo. O ideal é que todos os grupos tenham um valor de silhueta elevado.


{% highlight r %}
distancias = filmes %>% 
    column_to_rownames("TITLE") %>%
    select(RATING, `BOX OFFICE`) %>%
    mutate(`BOX OFFICE` = log10(`BOX OFFICE`)) %>%
    mutate_all(funs(scale)) %>% 
    dist(method = "euclidean")

colors <- brewer.pal(4, 'Set2')
plot(silhouette(cutree(agrupamento_h_2d, k = 4), distancias), col = colors, main = 'Silhouette - Divisão em 4 grupos') 
{% endhighlight %}

<img src="/portfolio/figure/source/bale-agrupamento/2017-06-27-agrupamento-p3cp1/unnamed-chunk-8-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />

Quanto maior é a medida da silhouette maior é a homogeneidade em um grupo, ou seja, os filmes de um grupo parecem mais com filmes do próprio grupo do que de outro grupo. A divisão em 4, é o máximo que podemos obter em termos da medida da silhouette e que faça sentido.

Outro fator importante que pode ser observado através desta visualização é o número de observações de cada grupo. Percebe-se a existência de um grupo maior com 9, dois com 4 filmes cada e um menor com 3 filmes.

## Grupos de filmes de Christian Bale

Agora que já conhecemos as variáveis e já decidimos em quantos grupos os filmes de **Christian Bale** se dividem vamos rotular, entender e exemplicar os grupos definidos acima.


{% highlight r %}
filmes_hclust <- filmes %>%
  mutate(cluster = as.factor(cutree(agrupamento_h_2d, k=4)))
{% endhighlight %}


{% highlight r %}
filmes_hclust %>%
    mutate("boxoffice" = `BOX OFFICE`) %>%
    hchart("scatter", hcaes(x = RATING, y = log10(`BOX OFFICE`), group = cluster)) %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Rating vs Box Office", align = "center") %>%
    hc_xAxis(title = list(text = "Rating (tomatômetro)")) %>%
    hc_yAxis(title = list(text = "Box Office (log 10)")) %>%
    hc_tooltip(pointFormat = "{point.TITLE} <br> Rating: {point.x} <br> Box Office: {point.boxoffice} <br> Ano: {point.YEAR}")%>%
    hc_legend(title = list(text = "Grupo"), align = "right", verticalAlign = "top",
            layout = "vertical", x = 0, y = 50)
{% endhighlight %}

<div class="figure" style="text-align: center">
<!--html_preserve--><div id="htmlwidget-0aa638eec0aea0a70fba" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-0aa638eec0aea0a70fba">{"x":{"hc_opts":{"title":{"text":"Rating vs Box Office","align":"center"},"yAxis":{"title":{"text":"Box Office (log 10)"},"type":"linear"},"credits":{"enabled":false},"exporting":{"enabled":false},"plotOptions":{"series":{"turboThreshold":0,"showInLegend":true,"marker":{"enabled":true}},"treemap":{"layoutAlgorithm":"squarified"},"bubble":{"minSize":5,"maxSize":25},"scatter":{"marker":{"symbol":"circle"}}},"annotationsOptions":{"enabledButtons":false},"tooltip":{"delayForDisplay":10,"pointFormat":"{point.TITLE} <br> Rating: {point.x} <br> Box Office: {point.boxoffice} <br> Ano: {point.YEAR}"},"series":[{"name":"1","data":[{"RATING":50,"TITLE":"The Promise","CREDIT":"Chris Myers","BOX OFFICE":8.2,"YEAR":2017,"cluster":"1","boxoffice":8.2,"x":50,"y":0.913813852383717},{"RATING":53,"TITLE":"Out of the Furnace","CREDIT":"Rodney Baze, Jr.","BOX OFFICE":8.4,"YEAR":2013,"cluster":"1","boxoffice":8.4,"x":53,"y":0.924279286061882},{"RATING":48,"TITLE":"Harsh Times","CREDIT":"Jim Luther Davis\n                                    \n                                            Executive Producer","BOX OFFICE":3.4,"YEAR":2006,"cluster":"1","boxoffice":3.4,"x":48,"y":0.531478917042255},{"RATING":62,"TITLE":"The New World","CREDIT":"John Rolfe","BOX OFFICE":12.5,"YEAR":2005,"cluster":"1","boxoffice":12.5,"x":62,"y":1.09691001300806}],"type":"scatter"},{"name":"2","data":[{"RATING":93,"TITLE":"American Hustle","CREDIT":"Irving Rosenfeld","BOX OFFICE":99.2,"YEAR":2013,"cluster":"2","boxoffice":99.2,"x":93,"y":1.99651167215418},{"RATING":87,"TITLE":"The Dark Knight Rises","CREDIT":"Bruce Wayne/Batman","BOX OFFICE":448.2,"YEAR":2012,"cluster":"2","boxoffice":448.2,"x":87,"y":2.65147185219904},{"RATING":90,"TITLE":"The Fighter","CREDIT":"Dicky Eklund","BOX OFFICE":93.6,"YEAR":2010,"cluster":"2","boxoffice":93.6,"x":90,"y":1.97127584873811},{"RATING":68,"TITLE":"Public Enemies","CREDIT":"Melvin Purvis","BOX OFFICE":97,"YEAR":2009,"cluster":"2","boxoffice":97,"x":68,"y":1.98677173426624},{"RATING":94,"TITLE":"The Dark Knight","CREDIT":"Batman/Bruce Wayne","BOX OFFICE":533.4,"YEAR":2008,"cluster":"2","boxoffice":533.4,"x":94,"y":2.72705301135386},{"RATING":89,"TITLE":"3:10 to Yuma","CREDIT":"Dan Evans","BOX OFFICE":53.6,"YEAR":2007,"cluster":"2","boxoffice":53.6,"x":89,"y":1.72916478969277},{"RATING":76,"TITLE":"The Prestige","CREDIT":"Alfred Borden","BOX OFFICE":53.1,"YEAR":2006,"cluster":"2","boxoffice":53.1,"x":76,"y":1.72509452108147},{"RATING":84,"TITLE":"Batman Begins","CREDIT":"Bruce Wayne/Batman","BOX OFFICE":204.2,"YEAR":2005,"cluster":"2","boxoffice":204.2,"x":84,"y":2.31005573775089},{"RATING":68,"TITLE":"Shaft","CREDIT":"Walter Wade Jr.","BOX OFFICE":70.4,"YEAR":2000,"cluster":"2","boxoffice":70.4,"x":68,"y":1.84757265914211}],"type":"scatter"},{"name":"3","data":[{"RATING":33,"TITLE":"Terminator Salvation","CREDIT":"John Connor","BOX OFFICE":125.3,"YEAR":2009,"cluster":"3","boxoffice":125.3,"x":33,"y":2.09795107099415},{"RATING":40,"TITLE":"Reign of Fire","CREDIT":"Quinn Abercromby","BOX OFFICE":43,"YEAR":2002,"cluster":"3","boxoffice":43,"x":40,"y":1.63346845557959},{"RATING":29,"TITLE":"Captain Corelli's Mandolin","CREDIT":"Mandras","BOX OFFICE":25.3,"YEAR":2001,"cluster":"3","boxoffice":25.3,"x":29,"y":1.40312052117582}],"type":"scatter"},{"name":"4","data":[{"RATING":76,"TITLE":"I'm Not There","CREDIT":"Jack Rollins/Pastor John","BOX OFFICE":4,"YEAR":2007,"cluster":"4","boxoffice":4,"x":76,"y":0.602059991327962},{"RATING":91,"TITLE":"Rescue Dawn","CREDIT":"Dieter Dengler","BOX OFFICE":5.4,"YEAR":2007,"cluster":"4","boxoffice":5.4,"x":91,"y":0.732393759822969},{"RATING":77,"TITLE":"The Machinist","CREDIT":"Trevor Reznik","BOX OFFICE":1,"YEAR":2004,"cluster":"4","boxoffice":1,"x":77,"y":0},{"RATING":68,"TITLE":"Laurel Canyon","CREDIT":"Sam","BOX OFFICE":3.6,"YEAR":2003,"cluster":"4","boxoffice":3.6,"x":68,"y":0.556302500767287}],"type":"scatter"}],"xAxis":{"type":"linear","title":{"text":"Rating (tomatômetro)"}},"legend":{"title":{"text":"Grupo"},"align":"right","verticalAlign":"top","layout":"vertical","x":0,"y":50}},"theme":{"colors":["#d35400","#2980b9","#2ecc71","#f1c40f","#2c3e50","#7f8c8d"],"chart":{"style":{"fontFamily":"Roboto"}},"title":{"align":"left","style":{"fontFamily":"Roboto Condensed","fontWeight":"bold"}},"subtitle":{"align":"left","style":{"fontFamily":"Roboto Condensed"}},"legend":{"align":"right","verticalAlign":"bottom"},"xAxis":{"gridLineWidth":1,"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"yAxis":{"gridLineColor":"#F3F3F3","lineColor":"#F3F3F3","minorGridLineColor":"#F3F3F3","tickColor":"#F3F3F3","tickWidth":1},"plotOptions":{"line":{"marker":{"enabled":false},"states":{"hover":{"lineWidthPlus":1}}},"spline":{"marker":{"enabled":false},"states":{"hover":{"lineWidthPlus":1}}},"area":{"marker":{"enabled":false},"states":{"hover":{"lineWidthPlus":1}}},"areaspline":{"marker":{"enabled":false},"states":{"hover":{"lineWidthPlus":1}}}}},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":["Roboto","Roboto+Condensed"],"debug":false},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
<p class="caption">plot of chunk unnamed-chunk-10</p>
</div>

*Você está visualizando o eixo y (Box office) na escala de log 10 e não na escala original. Para saber o valor na escala original basta passar o mouse sobre o ponto.*

#### Grupo 1 - Os Medianos
Esse é o grupo dos filmes que possuem o tomatômetro próximo a 50 %, ou seja não são frescos mas também não são podres. Em geral, os filmes desse grupo não obtiveram o sucesso desejado no BOX OFFICE como o esperado por seus produtores. Por exemplo, The New World teve um orçamento de 30 milhões de dólares mas só arrecadou nos EUA 12.5 milhões.

#### Grupo 2 - Os Grandes Sucessos
Nesse grupo estão presentes os filmes com maior bilheteria da carreira de Christian Bale, como A trilogia Batman e também os filmes mais aclamados pela crítica segundo o indíce do tomatômetro. Um filme bastante em que Bale é bastante conhecido por sua atuação brilhante é The Fighter, filme em que vive um irmão mais velho de um pugilista de boxe. Filme muito bom por sinal :).

#### Grupo 3 - Os Criticados famosos
Os integrantes desse grupo obtiveram altos índices de bilheteria mas não foram bem avaliados segundo o tomatômetro. Terminator Salvation é um exemplo disso, como é um filme da franquia **Terminator** (Exterminador do futuro) um grande hype havia na época, o que justifica a alta bilheteria embora o filme seja criticado até os dias de hoje, inclusive pelo próprio Arnold Schwarzenegger. 


#### Grupo 4 - Os ótimos menores
Aqui estão presentes os filmes de arrecadaram relativamente pouco se comparado a outros filmes do ator, mas que obtiveram ótimos índices de aprovação no Rotten Tomatoes. Rescue Dawn, que conta a história de um piloto americano que é capturado por inimigos durante a Guerra do Vietnã é um exemplo de filme com um "baixo" mas que é excelente.

**Essa foi a análise de hoje. Até a próxima !!!**
   
