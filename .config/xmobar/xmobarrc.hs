Config {  font = "xft:Roboto Mono:size=11, FontAwesome:size=11"
        , additionalFonts = [ "xft:FontAwesome:pixelsize=11" ]
        , borderColor = "black"
        , bgColor = "#090b1b"
        , fgColor = "#f4fffa"
        , position = TopW L 100
        , commands = [ Run Cpu [ "--template" , "Cpu: <total>%"
                               , "--Low"      , "50"         -- units: %
                               , "--High"     , "85"         -- units: %
                               , "--low"      , "#b8e399"
                               , "--normal"   , "orange"
                               , "--high"     , "red"
                               ] 10
                     
                     , Run Memory [ "--template" , "Mem: <usedratio>%"
                                  , "--Low"      , "20"        -- units: %
                                  , "--High"     , "90"        -- units: %
                                  , "--low"      , "darkgreen"
                                  , "--normal"   , "darkorange"
                                  , "--high"     , "darkred"
                                  ] 10
                     , Run Alsa "default" "Master" []
                     , Run Date "<fc=#eeeeee>%T</fc>" "date" 10
                     , Run Battery [ "--template", "Bat: <left>%"] 10
                     , Run StdinReader
                     ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "%StdinReader% }{  %alsa:default:Master%  %cpu%  %memory%  <fc=#ee9a00>%date%</fc>  %battery%"
        }
