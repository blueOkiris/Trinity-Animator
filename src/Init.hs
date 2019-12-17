module Init where

import Graphics.Gloss(Display(..), Picture(..), color, white, black, makeColor)

import GUI  ( Element(..), DynamicElement(..)
            , Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch )
import GUIObjects(defaultElementEventHandler, drawPaneHandler, defaultElementUpdate)
import State(AppState(..), Vector(..), AppWindow(..), newDrawing, moveDrawing)

startState :: AppState
startState =
    AppState    { window =          AppWindow   { bgColor = white
                                                , fps =     60
                                                , display = InWindow "Trinity Animator" (winWidth, winHeight) (200, 200)
                                                , width =   winWidth
                                                , height =  winHeight }
                , elements =        [ windowContainer
                                    , centerPanel 
                                    , drawPane
                                    , leftPanel
                                    , rightPanel
                                    , bottomPanel
                                    , topPanel ] 
                , drawings =        []
                , currentDrawing =  Vector  {   pointList = [] }
                , drawTool =        newDrawing }
    where
        mainBGColor = makeColor (100/255) (100/255) (100/255) 1
        panelBorderColor = makeColor (140/255) (140/255) (140/255) 1
        panelBGColor = makeColor (230/255) (230/255) (230/255) 1

        winWidth = 1280
        winHeight = 720

        baseMargin = 10

        windowContainer =   
            DynamicElement  { elemCore = Element    { borderWidth =     0
                                                    , borderColor =     white
                                                    , backColor =       mainBGColor
                                                    , backImage =       Blank
                                                    , horAlignment =    alignStretch
                                                    , vertAlignment =   alignStretch
                                                    , offset =          ((0, 0), (0, 0))
                                                    , parent =          GUI.False }
                            , updateElem =      defaultElementUpdate
                            , keyEventElem =    defaultElementEventHandler }
        leftPanel =
            DynamicElement  { elemCore = Element    { borderWidth =     3
                                                    , borderColor =     panelBorderColor
                                                    , backColor =       panelBGColor
                                                    , backImage =       Blank
                                                    , horAlignment =    alignLeft
                                                    , vertAlignment =   alignStretch
                                                    , offset =          ((baseMargin, baseMargin + 32 + baseMargin), (256, baseMargin))
                                                    , parent =          elemCore windowContainer }
                            , updateElem =      defaultElementUpdate
                            , keyEventElem =    defaultElementEventHandler }
        rightPanel =
            DynamicElement  { elemCore = Element    { borderWidth =     3
                                                    , borderColor =     panelBorderColor
                                                    , backColor =       panelBGColor
                                                    , backImage =       Blank
                                                    , horAlignment =    alignRight
                                                    , vertAlignment =   alignStretch
                                                    , offset =          ((baseMargin, baseMargin + 32 + baseMargin), (256, baseMargin))
                                                    , parent =          elemCore windowContainer }
                            , updateElem =      defaultElementUpdate
                            , keyEventElem =    defaultElementEventHandler }
        centerPanel =
            DynamicElement  { elemCore = Element    { borderWidth =     3
                                                    , borderColor =     panelBorderColor
                                                    , backColor =       panelBGColor
                                                    , backImage =       Blank
                                                    , horAlignment =    alignStretch
                                                    , vertAlignment =   alignStretch
                                                    , offset =          ((baseMargin + 256 + baseMargin, baseMargin + 32 + baseMargin), (baseMargin + 256 + baseMargin, baseMargin + 128 + baseMargin))
                                                    , parent =          elemCore windowContainer }
                            , updateElem =      defaultElementUpdate
                            , keyEventElem =    defaultElementEventHandler }
        bottomPanel =
            DynamicElement  { elemCore = Element    { borderWidth =     3
                                                    , borderColor =     panelBorderColor
                                                    , backColor =       panelBGColor
                                                    , backImage =       Blank
                                                    , horAlignment =    alignStretch
                                                    , vertAlignment =   alignBottom
                                                    , offset =          ((baseMargin + 256 + baseMargin, baseMargin), (baseMargin + 256 + baseMargin, 128))
                                                    , parent =          elemCore windowContainer }
                            , updateElem =      defaultElementUpdate
                            , keyEventElem =    defaultElementEventHandler }
        topPanel =
            DynamicElement  { elemCore = Element    { borderWidth =     3
                                                    , borderColor =     panelBorderColor
                                                    , backColor =       panelBGColor
                                                    , backImage =       Blank
                                                    , horAlignment =    alignStretch
                                                    , vertAlignment =   alignTop
                                                    , offset =          ((baseMargin, baseMargin), (baseMargin, 32))
                                                    , parent =          elemCore windowContainer }
                            , updateElem =      defaultElementUpdate
                            , keyEventElem =    defaultElementEventHandler }
        drawPane =
            DynamicElement  { elemCore = Element    { borderWidth =     1
                                                    , borderColor =     black
                                                    , backColor =       white
                                                    , backImage =       Blank
                                                    , horAlignment =    alignCenter
                                                    , vertAlignment =   alignCenter
                                                    , offset =          ((0, 0), (640, 480))
                                                    , parent =          elemCore centerPanel }
                            , updateElem =      defaultElementUpdate
                            , keyEventElem =    drawPaneHandler }