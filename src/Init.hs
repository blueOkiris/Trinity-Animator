module Init where

import Graphics.Gloss(Display(..), Picture(..), color, white, black, makeColor)
import Debug.Trace

import GUI  ( Element(..), DynamicElement(..)
            , Alignment, alignCenter, alignLeft, alignRight, alignTop, alignBottom, alignStretch
            , pngToPicture )
import GUIObjects   ( defaultElementEventHandler, defaultElementUpdate
                    , drawPaneHandler
                    , updateDrawIconFunc, drawIconHandler
                    , updateMoveIconFunc, moveIconHandler 
                    , updateEditIconFunc, editIconHandler)
import State(AppState(..), AppVector(..), AppWindow(..), newDrawing, isMakingNewDrawing, moveDrawing)

startState :: AppState
startState =
    AppState    { window =              AppWindow   { bgColor = white
                                                    , fps =     framesPerSecond
                                                    , display = InWindow "Trinity Animator" (winWidth, winHeight) (0, 0)
                                                    , width =   winWidth
                                                    , height =  winHeight }
                , elements =            [ windowContainer
                                        , centerPanel 
                                        , drawPane
                                        , leftPanel
                                        , rightPanel
                                        , bottomPanel
                                        , topPanel
                                        , drawIconElem
                                        , moveIconElem
                                        , editIconElem ] 
                , drawings =            []
                , currentDrawing =      AppVector   { pointList = []
                                                    , smoothVersion = [] 
                                                    , selectedPoint = 0 }
                , drawTool =            newDrawing
                , drawIcon =            drawIconPic
                , drawIconSelected =    drawIconSelectedPic
                , moveIcon =            moveIconPic
                , moveIconSelected =    moveIconSelectedPic 
                , editIcon =            editIconPic
                , editIconSelected =    editIconSelectedPic
                , selectedDrawing =     0 
                , clickedDownPoint =    (0, 0) }
    where
        mainBGColor =       makeColor (100/255) (100/255)   (100/255)   1
        panelBorderColor =  makeColor (140/255) (140/255)   (140/255)   1
        panelBGColor =      makeColor (230/255) (230/255)   (230/255)   1
        clearColor =        makeColor 0         0           0           0

        winWidth = 1920
        winHeight = 1080

        baseMargin = 10

        framesPerSecond = 10000
        
        drawIconPic =           id $! pngToPicture "images/iconset.png" (0, 0)      (512, 512) (32, 32)
        drawIconSelectedPic =   id $! pngToPicture "images/iconset.png" (512, 0)    (512, 512) (32, 32)
        moveIconPic =           id $! pngToPicture "images/iconset.png" (0, 512)    (512, 512) (32, 32)
        moveIconSelectedPic =   id $! pngToPicture "images/iconset.png" (512, 512)  (512, 512) (32, 32)
        editIconPic =           id $! pngToPicture "images/iconset.png" (0, 1024)   (512, 512) (32, 32)
        editIconSelectedPic =   id $! pngToPicture "images/iconset.png" (512, 1024) (512, 512) (32, 32)

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
                                                    , offset =          ((0, 0), (1280, 720))
                                                    , parent =          elemCore centerPanel }
                            , updateElem =      defaultElementUpdate
                            , keyEventElem =    drawPaneHandler }
        drawIconElem =
            DynamicElement  { elemCore = Element    { borderWidth =     3
                                                    , borderColor =     clearColor
                                                    , backColor =       clearColor
                                                    , backImage =       drawIconPic
                                                    , horAlignment =    alignLeft
                                                    , vertAlignment =   alignTop
                                                    , offset =          ((15, 15), (32, 32))
                                                    , parent =          elemCore leftPanel }
                            , updateElem =      updateDrawIconFunc
                            , keyEventElem =    drawIconHandler }
        moveIconElem =
            DynamicElement  { elemCore = Element    { borderWidth =     3
                                                    , borderColor =     clearColor
                                                    , backColor =       clearColor
                                                    , backImage =       moveIconPic
                                                    , horAlignment =    alignLeft
                                                    , vertAlignment =   alignTop
                                                    , offset =          ((15 + 32 + 15, 15), (32, 32))
                                                    , parent =          elemCore leftPanel }
                            , updateElem =      updateMoveIconFunc
                            , keyEventElem =    moveIconHandler }
        editIconElem =
            DynamicElement  { elemCore = Element    { borderWidth =     3
                                                    , borderColor =     clearColor
                                                    , backColor =       clearColor
                                                    , backImage =       editIconPic
                                                    , horAlignment =    alignLeft
                                                    , vertAlignment =   alignTop
                                                    , offset =          ((15 + 32 + 15 + 32 + 15, 15), (32, 32))
                                                    , parent =          elemCore leftPanel }
                            , updateElem =      updateEditIconFunc
                            , keyEventElem =    editIconHandler }
                            