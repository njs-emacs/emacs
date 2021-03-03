https://pictogrammers.github.io/@mdi/font/5.4.55/  ---> https://materialdesignicons.com/ (cheatsheet)

(insert-image (svg-icon "material" "attachment"))
 
(insert-image (svg-icon "octicons" "alert"))
 
(insert-image (svg-icon "octicons" "alert"))
(insert-image (svg-icon "material" "arrow-down-circle-outline" "white" "blue" 2))

(insert-image (svg-icon "material" "alpha-u-box" "white" "blue" 2))
(insert-image (svg-icon "material" "alpha-u-box-outline" "white" "blue" 2))

(insert-image (svg-icon "material" "video-input-svideo" "blue" "white" 4))

 
(insert-image (svg-icon "material" "snowflake" "blue" "white" 8)) 

(insert-image (svg-icon "material" "mushroom" "gold" "black" 8)) 
 
 
 


 
 


 
(insert-image (svg-icon "material" "scale" "white" "blue" 2)) 
 
 
;; Defining a "string icon"
(setq material-attach (propertize "--"
                      'display (svg-icon "material" "mushroom" "gold" "black")))

(setq material-attach (propertize "===="
                      'display (svg-icon "material" "mushroom" "gold" "black")))

(insert (format "[%s]" material-attach))[--]

(insert (format "[%s]" material-attach))[====]


(insert material-attach)


sd=ds
