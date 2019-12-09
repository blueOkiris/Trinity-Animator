module Init where

import Graphics.Gloss(Display(..), Picture(..), color, white, makeColor)

import GUI(Element(..), Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch)
import State(AppState(..), AppWindow(..))

startState :: AppState
startState =
    AppState    { window =  AppWindow   { bgColor = white
                                        , fps =     60
                                        , display = InWindow "Trinity Animator" (winWidth, winHeight) (200, 200)
                                        , width =   winWidth
                                        , height =  winHeight }
                , elements =    [ windowContainer
                                , Element   { borderWidth =     3
                                            , borderColor =     panelBorderColor
                                            , backColor =       panelBGColor
                                            , backImage =       Blank
                                            , horAlignment =    alignLeft
                                            , vertAlignment =   alignStretch
                                            , offset =          ((baseMargin, baseMargin + 32 + baseMargin), (256, baseMargin))
                                            , parent =          windowContainer }
                                , Element   { borderWidth =     3
                                            , borderColor =     panelBorderColor
                                            , backColor =       panelBGColor
                                            , backImage =       Blank
                                            , horAlignment =    alignRight
                                            , vertAlignment =   alignStretch
                                            , offset =          ((baseMargin, baseMargin + 32 + baseMargin), (256, baseMargin))
                                            , parent =          windowContainer }
                                , Element   { borderWidth =     3
                                            , borderColor =     panelBorderColor
                                            , backColor =       panelBGColor
                                            , backImage =       Blank
                                            , horAlignment =    alignStretch
                                            , vertAlignment =   alignStretch
                                            , offset =          ((baseMargin + 256 + baseMargin, baseMargin + 32 + baseMargin), (baseMargin + 256 + baseMargin, baseMargin + 128 + baseMargin))
                                            , parent =          windowContainer }
                                , Element   { borderWidth =     3
                                            , borderColor =     panelBorderColor
                                            , backColor =       panelBGColor
                                            , backImage =       Blank
                                            , horAlignment =    alignStretch
                                            , vertAlignment =   alignBottom
                                            , offset =          ((baseMargin + 256 + baseMargin, baseMargin), (baseMargin + 256 + baseMargin, 128))
                                            , parent =          windowContainer }
                                , Element   { borderWidth =     3
                                            , borderColor =     panelBorderColor
                                            , backColor =       panelBGColor
                                            , backImage =       Blank
                                            , horAlignment =    alignStretch
                                            , vertAlignment =   alignTop
                                            , offset =          ((baseMargin, baseMargin), (baseMargin, 32))
                                            , parent =          windowContainer } ] }
    where
        mainBGColor = makeColor (51/255) (51/255) (51/255) 1
        panelBorderColor = makeColor (140/255) (140/255) (140/255) 1
        panelBGColor = makeColor (230/255) (230/255) (230/255) 1

        winWidth = 1280
        winHeight = 720

        baseMargin = 10

        windowContainer =   Element { borderWidth =     0
                                    , borderColor =     white
                                    , backColor =       mainBGColor
                                    , backImage =       Blank
                                    , horAlignment =    alignStretch
                                    , vertAlignment =   alignStretch
                                    , offset =          ((0, 0), (0, 0))
                                    , parent =          GUI.False }