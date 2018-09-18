(defun c:buildFramework()
  	(DCLAndDataHandler)
	;操作后记忆数据
)


;Data
(defun DCLAndDataHandler()
	(setq sdt 0)
  	(setq id(load_dialog "G:\\工程二处\\drawDrillFramework\\BuildFrameWorkDCL.dcl"))
	(if (not (new_dialog "framework" id))(exit))
  	(action_tile "startBtn" "(configStartBtn) (done_dialog 1)")
	(action_tile "DFBtn" "(configDFBtn)  (done_dialog 2)")
	(action_tile "DLBtn" "(configDLBtn)  (done_dialog 3)")
	(action_tile "DDBtn" "(configDDBtn)  (done_dialog 4)")
	(action_tile "DRBtn" "(configDRBtn)  (done_dialog 5)")
	(action_tile "HoleNumber_List" "(configHolePopListAction)")
	(action_tile "RockType_List" "(configRockPopListAction)")
	(action_tile "cancelBtn" "(done_dialog -1)")
	;存储上次输入记录
  	(storageLastDCLValue)
 	(setq std (start_dialog))
  	(unload_dialog id)
	(cond 
					((= std 1)
						(progn
							(alert "外挂启动完毕，请开始蹂躏我吧！！")
							(command "measurement" 1)
						  	(c:buildFramework)
						)
					)
					
					((= std 2)
						(progn
							(drawFrame)
						  	(c:buildFramework)
						)
					)
					
					((= std 3)
						(progn
							(alert "画线喽！！！")
							(drawPLine)
						  	(c:buildFramework)
						)
					)
					
					((= std 4)
						(progn
							(alert "完井结构！！！")
							(drawDrillStructure)
						  	(c:buildFramework)
						)
					)
					
					((= std 5)
						(progn
							(drawRock)
						  	(c:buildFramework)
						)
					)
  					((= std -1)
					(progn
						(alert "你就这样离开我了么？？亚麻得！！")

					)
					)
	)
 	
  	(princ)
)

;记忆初次输入的DCL的数据
(defun storageLastDCLValue()
	;架构绘制
  	(if (null d) (print "nil") (set_tile "D" (rtos d 2 2)))
	(if (null dt) (print "nil") (set_tile "DT" (rtos dt 2 2)))
  	(if (null mc) (print "nil") (set_tile "MC" (rtos mc 2 2)))
  	(if (null wl) (print "nil") (set_tile "WL" (rtos wl 2 2)))
	;曲线绘制
	(if (null columnPLineOne) (print "nil") (set_tile "columnPLineOne" columnPLineOne))
  	(if (null columnPLineTwo) (print "nil") (set_tile "columnPLineTwo" columnPLineTwo))
  	(if (null lineNameNum) (print "nil") (set_tile "LineName_List" (itoa lineNameNum)))
  	(if (null lineTypeNum) (print "nil") (set_tile "LineType_List" (itoa lineTypeNum)))
  	(if (null lineWeightNum) (print "nil") (set_tile "LineWeight_List" (itoa lineWeightNum)))
  	(if (null HoleNum) (print "nil") (set_tile "HoleNumber_List" (itoa HoleNum)))
	;完井结构
	(if (null holeOneDrillDiameter) (print "nil") (set_tile "HoleOneDrillDiameter" holeOneDrillDiameterStr))
	(if (null holeOneDeep) (print "nil") (set_tile "HoleOneDeep" (rtos holeOneDeep 2 2)))
	(if (null holeOneBushDiameter) (print "nil") (set_tile "HoleOneBushDiameter" holeOneBushDiameterStr))
	(if (null holeOneBushDeep) (print "nil") (set_tile "HoleOneBushDeep" (rtos holeOneBushDeep 2 2)))
	(if (null holeOneCement) (print "nil") (set_tile "HoleOneCement" (rtos holeOneCement 2 2)))
	(if (null holeTwoDrillDiameter) (print "nil") (set_tile "HoleTwoDrillDiameter" holeTwoDrillDiameterStr))
	(if (null holeTwoDeep) (print "nil") (set_tile "HoleTwoDeep" (rtos holeTwoDeep 2 2)))
	(if (null holeTwoBushDiameter) (print "nil") (set_tile "HoleTwoBushDiameter" holeTwoBushDiameterStr))
	(if (null holeTwoBushDeep) (print "nil") (set_tile "HoleTwoBushDeep" (rtos holeTwoBushDeep 2 2)))
	(if (null holeTwoCement) (print "nil") (set_tile "HoleTwoCement" (rtos holeTwoCement 2 2)))
	(if (null holeArtificialDeep) (print "nil") (set_tile "HoleArtificialDeep" (rtos holeArtificialDeep 2 2)))
	(if (null holeDamperLocation) (print "nil") (set_tile "HoleDamperLocation" (rtos holeDamperLocation 2 2)))
	(if (null bushShoes) (print "nil") (set_tile "BushShoes" (rtos bushShoes 2 2)))
	(if (null holeThreeDrillDiameter) (print "nil") (set_tile "HoleThreeDrillDiameter" holeThreeDrillDiameterStr))
	(if (null holeThreeDeep) (print "nil") (set_tile "HoleThreeDeep" (rtos holeThreeDeep 2 2)))
	(if (null holeThreeBushDiameter) (print "nil") (set_tile "HoleThreeBushDiameter" holeThreeBushDiameterStr))
	(if (null holeThreeBushDeep) (print "nil") (set_tile "HoleThreeBushDeep" (rtos holeThreeBushDeep 2 2)))
	(if (null holeThreeCement) (print "nil") (set_tile "HoleThreeCement" (rtos holeThreeCement 2 2)))
	(cond ((= HoleNum 0)  
					(progn
						(mode_tile  "HoleThreeDrillDiameter" 1)
						(mode_tile  "HoleThreeDeep" 1)
						(mode_tile  "HoleThreeBushDiameter" 1)
						(mode_tile  "HoleThreeBushDeep" 1)
						(mode_tile  "HoleThreeCement" 1)
					  	(setq HoleNumFactor 1)
					) 
				)
			   ((= HoleNum 1)
					(progn 
						(mode_tile  "HoleThreeDrillDiameter" 0)
						(mode_tile  "HoleThreeDeep" 0)
						(mode_tile  "HoleThreeBushDiameter" 0)
						(mode_tile  "HoleThreeBushDeep" 0)
						(mode_tile  "HoleThreeCement" 0)
						(setq HoleNumFactor 2)
					)
				)	 
	)
	;柱状图
	(if (null rockTypeNum) (print "nil") (set_tile "RockType_List" (itoa rockTypeNum)))
	(if (null columnRockOne) (print "nil") (set_tile "columnRockOne" columnRockOne))
  	(if (null columnRockTwo) (print "nil") (set_tile "columnRockTwo" columnRockTwo))
	(if (null columnRockThree) (print "nil") (set_tile "columnRockThree" columnRockThree))
	(if (null columnRockFour) (print "nil") (set_tile "columnRockFour" columnRockFour))
	(if (null columnRockFive) (print "nil") (set_tile "columnRockFive" columnRockFive))
  	(if (null rockTextSize) (print "nil") (set_tile "RockTextSize_List" (itoa rockTextSize)))
	(cond ((= rockTypeNum 0)  
				(progn
					(mode_tile  "columnRockOne" 0)
					(mode_tile  "columnRockTwo" 0)
					(mode_tile  "columnRockThree" 0)
					(mode_tile  "columnRockFour" 0)
					(mode_tile  "columnRockFive" 0)
				) 
			)
		   ((= rockTypeNum 1)
				(progn 
					(mode_tile  "columnRockOne" 0)
					(mode_tile  "columnRockTwo" 0)
					(mode_tile  "columnRockThree" 1)
					(mode_tile  "columnRockFour" 1)
					(mode_tile  "columnRockFive" 1)
				)
			)
		   ((= rockTypeNum 2)  
				(progn 
					(print "待开发")
				)
			)
	)
)

(defun configStartBtn()
	;此处可增加初始化代码，预留位置
	(print 111111111)
  	;(setq d 788.0)
)

(defun configDFBtn()
	(setq d (atof (get_tile "D")))
	(setq dt (atof (get_tile "DT")))
 	(setq mc (atof (get_tile "MC")))
  	(setq wl (atof (get_tile "WL")))
)

;配置画线按钮属性
(defun configDLBtn()
	(setq PLFactor 1)
	(setq columnPLineOne  (get_tile "columnPLineOne"))
	(setq columnPLineTwo  (get_tile "columnPLineTwo"))
	(setq lineNameNum  (atoi(get_tile "LineName_List")))
	(setq PLXOffset -445)
	(cond ((= lineNameNum 0)  
					(progn
						(setq PLFactor (caculateFactor 12 (caculateMaxNum dt)))
						(setq PLXOffset -445)			
					) 
				)
			   ((= lineNameNum 1)
					(progn 
						(setq PLFactor (caculateFactor 10 (caculateMaxNum mc)))
						(setq PLXOffset -385)	
					)
				)
		       ((= lineNameNum 2)  
					(progn 
						(setq PLFactor (caculateFactor 10 (caculateMaxNum wl)))
						(setq PLXOffset -385)					
					)
				)
	)
	(setq lineTypeNum (atoi(get_tile "LineType_List")))
	(setq lineWeightNum (atoi(get_tile "LineWeight_List")))	
)


;配置画完井结构按钮属性
(defun configDDBtn()
	
	(setq holeOneDrillDiameterStr  (get_tile "HoleOneDrillDiameter"));一开钻头直径
	(setq holeOneDeep  (atof (get_tile "HoleOneDeep")));一开井深
	(setq holeOneBushDiameterStr (get_tile "HoleOneBushDiameter"));一开套管外径
	(setq holeOneBushDeep (atof (get_tile "HoleOneBushDeep")));一开套管下深
	(setq holeOneCement  (atof (get_tile "HoleOneCement")));一开水泥上返深度
	(setq holeTwoDrillDiameterStr (get_tile "HoleTwoDrillDiameter"));二开钻头直径
	(setq holeTwoDeep  (atof (get_tile "HoleTwoDeep")));二开井深
	(setq holeTwoBushDiameterStr (get_tile "HoleTwoBushDiameter"));二开套管外径
	(setq holeTwoBushDeep  (atof (get_tile "HoleTwoBushDeep")));二开套管下深
	(setq holeTwoCement  (atof (get_tile "HoleTwoCement")));二开水泥上返深度
	(setq holeArtificialDeep  (atof (get_tile "HoleArtificialDeep")));人工井底深度
	(setq holeDamperLocation  (atof (get_tile "HoleDamperLocation")));阻流环位置
	(setq bushShoes  (atof (get_tile "BushShoes")));套管鞋

	(setq HoleNum (atoi(get_tile "HoleNumber_List")))
	(cond ((= HoleNum 0)  
					(progn
						(mode_tile  "HoleThreeDrillDiameter" 1)
						(mode_tile  "HoleThreeDeep" 1)
						(mode_tile  "HoleThreeBushDiameter" 1)
						(mode_tile  "HoleThreeBushDeep" 1)
						(mode_tile  "HoleThreeCement" 1)
					  	(setq HoleNumFactor 1)
					) 
				)
			   ((= HoleNum 1)
					(progn 
						(mode_tile  "HoleThreeDrillDiameter" 0)
						(mode_tile  "HoleThreeDeep" 0)
						(mode_tile  "HoleThreeBushDiameter" 0)
						(mode_tile  "HoleThreeBushDeep" 0)
						(mode_tile  "HoleThreeCement" 0)
						(setq holeThreeDrillDiameterStr (get_tile "HoleThreeDrillDiameter"));三开钻头直径
						(setq holeThreeDeep  (atof (get_tile "HoleThreeDeep")));三开井深
						(setq holeThreeBushDiameterStr (get_tile "HoleThreeBushDiameter"));三开套管外径
						(setq holeThreeBushDeep  (atof (get_tile "HoleThreeBushDeep")));三开套管下深
						(setq holeThreeCement  (atof (get_tile "HoleThreeCement")));三开水泥上返深度
						(setq HoleNumFactor 2)
					)
				)
		       ((= HoleNum 2)  
					(progn 
							
					)
				)
	)
)

;配置井柱状图按钮属性
(defun configDRBtn()
	
	(setq columnRockOne  (get_tile "columnRockOne"))
	(setq columnRockTwo  (get_tile "columnRockTwo"))
	(setq columnRockThree  (get_tile "columnRockThree"))
	(setq columnRockFour (get_tile "columnRockFour"))
	(setq columnRockFive  (get_tile "columnRockFive"))
	(setq rockTypeNum (atoi(get_tile "RockType_List")))
	(setq rockTextSizeNum (atoi(get_tile "RockTextSize_List")))
	(setq rockTextSize 3)
	(setq RockXOffset -245)
	(cond ((= rockTextSizeNum 0)  
					(progn
						(setq rockTextSize 3)
					) 
				)
			   ((= rockTextSizeNum 1)
					(progn 
						(setq rockTextSize 2)
					)
				)
		       ((= rockTextSizeNum 2)  
					(progn 
						(setq rockTextSize 1)	
					)
				)
	)
	(cond ((= rockTypeNum 0)  
					(progn
						(setq RockXOffset -245)
					) 
				)
			   ((= rockTypeNum 1)
					(progn 
						(setq RockXOffset -255)
					)
				)
	)
)

;根据DCL配置多线段属性
(defun configPLineAttribute()
	;配置多线段线型
	(cond ((= lineTypeNum 0)
					(progn
							(command "LineType" "s"  "ByLayer" "")
					)
				)
				((= lineTypeNum 1)
					(progn
							(command "LineType" "s"  "ACAD_ISO02W100" "")
					)
				)
				((= lineTypeNum 2)
					(progn
							(command "LineType" "s"  "ByLayer" "")
					)
				)
	)
	;配置多线段线宽
	(cond ((= lineWeightNum 0)
					(progn
							(command "lweight" "ByLayer")
					)
				)
				((= lineWeightNum 1)
					(progn
							(command "lweight" 1.00)
					)
				)
				((= lineWeightNum 2)
					(progn
							(command "lweight" "ByLayer")
					)
				)
	)	
)

;配置二开三开popList动作属性
(defun configHolePopListAction()
    (setq HoleNum  (atoi(get_tile "HoleNumber_List")))
	(setq HoleNumFactor 1)
	(cond ((= HoleNum 0)  
					(progn
						(mode_tile  "HoleThreeDrillDiameter" 1)
						(mode_tile  "HoleThreeDeep" 1)
						(mode_tile  "HoleThreeBushDiameter" 1)
						(mode_tile  "HoleThreeBushDeep" 1)
						(mode_tile  "HoleThreeCement" 1)
						(setq HoleNumFactor 1)
					) 
				)
			   ((= HoleNum 1)
					(progn 
						(mode_tile  "HoleThreeDrillDiameter" 0)
						(mode_tile  "HoleThreeDeep" 0)
						(mode_tile  "HoleThreeBushDiameter" 0)
						(mode_tile  "HoleThreeBushDeep" 0)
						(mode_tile  "HoleThreeCement" 0)
						(setq HoleNumFactor 2)
					)
				)
		       ((= HoleNum 2)  
					(progn 
						(print "待开发")
					)
				)
	)

)

;配置岩性柱状图popList动作属性
(defun configRockPopListAction()
    (setq rockTypeNum  (atoi(get_tile "RockType_List")))
	(cond ((= rockTypeNum 0)  
					(progn
						(mode_tile  "columnRockOne" 0)
						(mode_tile  "columnRockTwo" 0)
						(mode_tile  "columnRockThree" 0)
						(mode_tile  "columnRockFour" 0)
						(mode_tile  "columnRockFive" 0)
					) 
				)
			   ((= rockTypeNum 1)
					(progn 
						(mode_tile  "columnRockOne" 0)
						(mode_tile  "columnRockTwo" 0)
						(mode_tile  "columnRockThree" 1)
						(mode_tile  "columnRockFour" 1)
						(mode_tile  "columnRockFive" 1)
					)
				)
		       ((= rockTypeNum 2)  
					(progn 
						(print "待开发")
					)
				)
	)

)


;UI
;UI-Frame
(defun drawFrame()
		(drawFrameLine)
		(drawFrameTextLabel)
		(command)
		(drawFrameBaseBlock)
)

(defun drawRock()
		(vl-load-com)
		(Setq EXCEL (Vlax-Get-Or-Create-Object "Excel.Application"))
		(Vlax-Put-Property EXCEL 'Visible -1 )
		(setq cells (Vlax-Get excel 'Cells ))
		(setq rockDataRangeOfY (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockOne))
		(setq rockDataThickness (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockTwo))
		(setq rockDataName (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockThree))
		(setq rockDataDescription (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockFour))
		(setq rockDataNumber (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockFive))
		(setq rockLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property rockDataRangeOfY 'Value2))))

	 
		(setq indexA 1)
		(setq rockListLength (length rockLst))
		(while (<= indexA rockListLength)
			(if (> indexA 0)
				  (progn
				  	 	(setq rockDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataRangeOfY 'Item indexA)) 'Value2))
						(setq rockThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataThickness 'Item indexA)) 'Value2))
						(setq rockName (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataName 'Item indexA)) 'Value2))
						(setq rockDescription (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataDescription 'Item indexA)) 'Value2))
						(setq rockNumber (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataNumber 'Item indexA)) 'Value2))
					(drawRockLine)
					(drawRockTextLabel)
					(hatchRock rockName rockDeep rockThickness)
				)
			)
			(setq indexA (1+ indexA))
		)

)

(defun drawDrillStructure()
	(drawDrillStructureLine)
	(drawDrillStructureTextLabel)
	(drawDrillStructureDimlinear)
  	(hatchDrillStructure)
)

;UI-PLine
(defun drawPLine()
	(vl-load-com)
	(Setq EXCEL (Vlax-Get-Or-Create-Object "Excel.Application"))
	(Vlax-Put-Property EXCEL 'Visible -1 )
	(setq cells (Vlax-Get excel 'Cells ))
	(setq plineDataRangeOfY (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnPLineOne))
	(setq plineDataRangeOfX (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnPLineTwo))
	(setq plineLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property plineDataRangeOfY 'Value2))))
 
	(setq indexA 1)
  	(setq plineListLength (length plineLst))
	(setq plineListPT '())
	(while (<= indexA plineListLength)
			(print (Vlax-Get (vlax-variant-value (Vlax-Get-Property plineDataRangeOfY 'Item indexA )) 'Value2 ))
		(if (> indexA 0)
			  (progn
				(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property plineDataRangeOfY 'Item indexA)) 'Value2))
				(setq pxA (Vlax-Get (vlax-variant-value (Vlax-Get-Property plineDataRangeOfX 'Item indexA)) 'Value2))
				(setq pyA (* pyA -5))
				(setq pxA (+ (* pxA PLFactor) PLXOffset))
				(setq ptA (strcat (strcat (strcat (strcat "(" (rtos pxA 2 6)) " ") (rtos pyA 2 6)) ")"))
				(setq plineListPT (cons (read ptA) plineListPT))
			)
		)
		(setq indexA (1+ indexA))
	)
				;配置多线段属性
				(configPLineAttribute)
	 ;;;;有顺序，不可改
				(entmake 
					(append 
						(list '(0 . "LWPOLYLINE")
							 '(100 . "AcDbEntity")
							 '(100 . "AcDbPolyline")
							 (cons 90 (length plineListPT)) ; 顶点个数
							'(70 . 0) ; 1 闭合 0 不闭合 不是必须，默认为0
						)
						(mapcar '(lambda (x) (cons 10 x)) plineListPT)
				 ;二维坐标列表 加组码 10
					)    ;append end
				)
  	(print plineListPT)
)


;UI-Line

(defun drawFrameLine()
  	(command "lweight" 1.00)
  	(command "line" "-450,140" "60,140" "")
  	(setq floatPyRough (+ (* d -5) -100));柱状图比例为1：:200
	(setq floatPyThinTop  (* d -5) );
	(setq floatPyThinDown (+ (* d -5) -95));
 	(setq strRoughPy (rtos floatPyRough 2 2));粗线Y坐标放缩后转换为字符串
	(setq strThinDownPy (rtos floatPyThinDown 2 2));细线Y下部坐标放缩后转换为字符串
	(setq strThinTopPy (rtos floatPyThinTop 2 2));细线Y上部坐标放缩后转换为字符串
  	(setq PdLeftRough (strcat "-450," strRoughPy));拼接坐标点PdLeftRough粗线左深度坐标
  	(setq PdRightRough (strcat "60," strRoughPy));拼接坐标点PdRightRough粗线右深度坐标
  	(command "line" "-450,140" PdLeftRough "")
  	(command "line" "60,140" PdRightRough "" )
	(command "line" PdLeftRough PdRightRough "")
	
  	(command "lweight" "ByLayer")
  	(command "line" "-445,135" "55,135" "")
	(setq PdLeftTopThin (strcat "-445," strThinTopPy));拼接坐标点PdLeftThin细线左深度坐标
 	(setq PdRightTopThin (strcat "55," strThinTopPy));拼接坐标点PdRightThin细线右深度坐标
	(command "line" PdLeftTopThin PdRightTopThin "")
  	(setq PdLeftDownThin (strcat "-445," strThinDownPy));拼接坐标点PdLeftThin细线左深度坐标
 	(setq PdRightDownThin (strcat "55," strThinDownPy));拼接坐标点PdRightThin细线右深度坐标
  	(command "line" "-445,135" PdLeftDownThin "")
  	(command "line" "55,135" PdRightDownThin "" )
  	(command "line" PdLeftDownThin PdRightDownThin "")

  	 
  	(command "line" "-445,30" "55,30" "");上下框线
	(command "line" "-445,0" "-388,0" "")
	
	(command "LineType" "s"  "ACAD_ISO02W100" "")
	(command "line" "-388,0" "-335,0" "")
	
	(command "LineType" "s"  "ByLayer" "")
  	(command "line" "-335,0" "55,0" "")
  
  	(command "line" "-385,30" (strcat "-385," strThinTopPy) "");钻时分界线
	(command "line" "-433,0" "-433,3" "")
	(command "line" "-421,0" "-421,3" "")
	(command "line" "-409,0" "-409,3" "")
	(command "line" "-397,0" "-397,3" "")

  	(command "line" "-335,30" (strcat "-335," strThinTopPy) "");水文观测分界线
  	
	(command "line" "-385,20" "-335,20" "");消耗量分界线
	(command "line" "-375,10" "-375,12" "")
	(command "line" "-365,10" "-365,12" "")
	(command "line" "-355,10" "-355,12" "")
	(command "line" "-345,10" "-345,12" "")
	
  	(command "line" "-385,10" "-335,10" "");水位分界线
	(command "line" "-375,0" "-375,2" "")
	(command "line" "-365,0" "-365,2" "")
	(command "line" "-355,0" "-355,2" "")
	(command "line" "-345,0" "-345,2" "")


  	(command "line" "-335,20" "-305,20" "");地层单位分界线
  	(command "line" "-325,20" (strcat "-325," strThinTopPy) "")
	(command "line" "-315,20" (strcat "-315," strThinTopPy) "")
	(command "line" "-305,30" (strcat "-305," strThinTopPy) "")
  
	(command "line" "-290,30" (strcat "-290," strThinTopPy) "");深度分界线
  
	(command "line" "-275,30" (strcat "-275," strThinTopPy) "");层厚分界线

  	(command "line" "-270,30" (strcat "-270," strThinTopPy) "");空白分界线
  
  	(command "line" "-255,30" (strcat "-255," strThinTopPy) "");测井柱状分界线
  
  	(command "line" "-245,30" (strcat "-245," strThinTopPy) "");标志层代号分界线
  
  	(command "line" "-230,30" (strcat "-230," strThinTopPy) "");钻井柱状分界线
  
  	(command "line" "-225,30" (strcat "-225," strThinTopPy) "");空白分界线
  
  	(command "line" "-210,30" (strcat "-210," strThinTopPy) "");深度分界线
  
  	(command "line" "-195,30" (strcat "-195," strThinTopPy) "");层厚分界线
  
  	(command "line" "-185,30" (strcat "-185," strThinTopPy) "");层号分界线
  
 	(command "line" "-160,30" (strcat "-160," strThinTopPy) "");岩石名称分界线
  
  	(command "line" "-60,30" (strcat "-60," strThinTopPy) "");岩屑描述分界线
  
  	(command "line" "0,30" (strcat "0," strThinTopPy) "");完井结构分界线

	(command "line" "0,15" "25,15" "")
  	(command "line" "25,30" (strcat "25," strThinTopPy) "");井斜方位分界线

  	(command "line" "55,30" (strcat "55," strThinTopPy) "");备注分界线
)


(defun drawRockLine()		
	;处理划线数据
	(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataRangeOfY 'Item indexA)) 'Value2))
	(setq pyA (* pyA -5))
	(setq pxA  RockXOffset)
	(cond ((= rockTypeNum 0)  
					(progn
						(setq pxB (+ RockXOffset 185))
					) 
				)
			   ((= rockTypeNum 1)
					(progn 
						(setq pxB (- RockXOffset 50))
					)
				)
	)
	(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
	(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
	(command "line" ptA ptB "")			
)

(defun drawDrillStructureLine()
	;HoleNumFactor
	(setq strBushShoesPtA (strcat "-40," (rtos (* BushShoes -5) 2 2)))
	(setq strBushShoesPtB (strcat "-20," (rtos (* BushShoes -5) 2 2)))
	;绘制阻流环
	(setq strHoleDamperLocationPtA (strcat "-40," (rtos (* holeDamperLocation -5) 2 2)))
	(setq strHoleDamperLocationPtB (strcat "-20," (rtos (* holeDamperLocation -5) 2 2)))
	(setq strHoleDamperLocationPtC (strcat "-40," (rtos (- (* holeDamperLocation -5) 2) 2 2)))
	(setq strHoleDamperLocationPtD (strcat "-20," (rtos (- (* holeDamperLocation -5) 2) 2 2)))

	;人工井底深度
	(setq strHoleArtificialDeepPtA (strcat "-40," (rtos (* holeArtificialDeep -5) 2 2)))
	(setq strHoleArtificialDeepPtB (strcat "-20," (rtos (* holeArtificialDeep -5) 2 2)))
	
	;无论是二开还是三开，内部套管长度是可以确定的，由套管鞋子来确定
	(command "lweight" 0.50)
	(command "line" "-40,0" strBushShoesPtA "")
	(command "line" "-20,0" strBushShoesPtB "")
	(command "lweight" "ByLayer")

	(command "line" strBushShoesPtA strBushShoesPtB "")
	(command "line" strHoleDamperLocationPtA strHoleDamperLocationPtB "")
	(command "line" strHoleDamperLocationPtC strHoleDamperLocationPtD "")
	(command "circle" (strcat "-30," (rtos (- (* holeDamperLocation -5) 4.5) 2 2)) 2.5)
	(command "line" strHoleArtificialDeepPtA strHoleArtificialDeepPtB "")
	
	(cond ((= HoleNumFactor 1)  
					(progn
							(command "lweight" "ByLayer")
							(command "line" (strcat "-43," (rtos (* d -5) 2 2)) (strcat "-43," (rtos (* HoleOneDeep -5) 2 2)) "")
							(command "line" (strcat "-17," (rtos (* d -5) 2 2)) (strcat "-17," (rtos (* HoleOneDeep -5) 2 2)) "")
							(command "line" "-49,0" (strcat "-49," (rtos (* HoleOneDeep -5) 2 2)) "")
							(command "line" "-11,0" (strcat "-11," (rtos (* HoleOneDeep -5) 2 2)) "")
							(command "lweight" 0.50)
							(command "line" "-46,0" (strcat "-46," (rtos (* HoleOneDeep -5) 2 2)) "")
							(command "line" "-14,0" (strcat "-14," (rtos (* HoleOneDeep -5) 2 2)) "")
							
					) 
				)
			   ((= HoleNumFactor 2)
					(progn 
							(command "lweight" "ByLayer")
							;一开井深
							(command "line" "-55,0" (strcat "-55," (rtos (* HoleOneDeep -5) 2 2)) "")
							(command "line" "-5,0" (strcat "-5," (rtos (* HoleOneDeep -5) 2 2)) "")
							;一开套管下深
							(command "lweight" 0.50)
							(command "line" "-52,0" (strcat "-52," (rtos (* HoleOneDeep -5) 2 2)) "")
							(command "line" "-8,0" (strcat "-8," (rtos (* HoleOneDeep -5) 2 2)) "")
							;二开井深
							(command "lweight" "ByLayer")
							(command "line" (strcat "-49," (rtos (* HoleOneDeep -5) 2 2)) (strcat "-49," (rtos (* HoleTwoDeep -5) 2 2)) "")
							(command "line" (strcat "-11," (rtos (* HoleOneDeep -5) 2 2)) (strcat "-11," (rtos (* HoleTwoDeep -5) 2 2)) "")
							(command "line" (strcat "-43," (rtos (* d -5) 2 2)) (strcat "-43," (rtos (* HoleTwoDeep -5) 2 2)) "")
							(command "line" (strcat "-17," (rtos (* d -5) 2 2)) (strcat "-17," (rtos (* HoleTwoDeep -5) 2 2)) "")
							;二开套管下深
							(command "lweight" 0.50)
							(command "line" (strcat "-46," (rtos (* HoleOneDeep -5) 2 2)) (strcat "-46," (rtos (* HoleTwoDeep -5) 2 2)) "")
							(command "line" (strcat "-14," (rtos (* HoleOneDeep -5) 2 2)) (strcat "-14," (rtos (* HoleTwoDeep -5) 2 2)) "")
							(command "lweight" "ByLayer")
					)
				)  
	)

)

;UI-TextLabel

(defun drawFrameTextLabel()
	(command "mtext" (list -434 25) "h" "3" "w" "60" "钻   时   曲   线" " " "     (min/m)" "")
  	(command "mtext" (list -375 27) "h" "2.5" "w" "60" "简 易 水 文 观 测" "")
  	(command "mtext" (list -374 18) "h" "2.5" "w" "60" "消 耗 量 （m /h）" "")
  	(command "text" "J" "C" (list -351.5 17) 1 0 "3")
  	(command "text" "J" "C" (list -359 5) 2.5 0 "水     位 （m）")
  	(command "text" "J" "C" (list -320 24) 2.8 0 "地 层 单 位")
 	(command "text" "J" "C" (list -330 10) 2.8 0 "系")
  	(command "text" "J" "C" (list -320 10) 2.8 0 "统")
  	(command "text" "J" "C" (list -310 10) 2.8 0 "组")
  	(command "mtext" (list -300 27) "h" "2.8" "w" "10" "层" " " "厚" " " "(m)" "")
  	(command "mtext" (list -285 27) "h" "2.8" "w" "10" "深" " " "度" " " "(m)" "")
  	(command "mtext" (list -267 27) "h" "2.8" "w" "10" "测 井" " " "柱 状" " " "1:200" "")
  	(command "mtext" (list -252 27) "h" "2.8" "w" "10" "标" "志" "层" "代" "号" "")
  	(command "mtext" (list -242 27) "h" "2.8" "w" "10" "钻 井" " " "柱 状" " " "1:200" "")
  	(command "mtext" (list -220 27) "h" "2.8" "w" "10" "层" " " "厚" " " "(m)" "")
  	(command "mtext" (list -205 27) "h" "2.8" "w" "10" "深" " " "度" " " "(m)" "")
  	(command "mtext" (list -192 27) "h" "2.8" "w" "10" "层" " " " " " " "号" "")
  	(command "mtext" (list -178 23) "h" "2.8" "w" "20" "岩  石" " " "名  称" " " "")
	(command "text" "J" "C" (list -110 15) 3.0 0 "岩       性     （屑）     描     述")
  	(command "text" "J" "C" (list -30 15) 3.0 0 "完   井   结   构")
  	(command "text" "J" "C" (list 12.5 17) 2.8 0 "井斜(°)/方位(°)")
  	(command "text" "J" "C" (list 12.5 11) 2.8 0 "井深(m)")
  	(command "text" "J" "C" (list 40 15) 3.0 0 "备      注")
		
	(drawDTTextLabel (caculateMaxNum dt))
	(drawWLTextLabel (caculateMaxNum wl))
	(drawMCTextLabel (caculateMaxNum mc))
  	
  	
)

(defun drawDTTextLabel(maxNumDivFive)
	(command "text" "J" "C" (list -433 4) 3 0 (rtos maxNumDivFive 2 0))
	(command "text" "J" "C" (list -421 4) 3 0 (rtos (* maxNumDivFive 2) 2 0))
	(command "text" "J" "C" (list -409 4) 3 0 (rtos (* maxNumDivFive 3) 2 0))
	(command "text" "J" "C" (list -397 4) 3 0 (rtos (* maxNumDivFive 4) 2 0))
)

(defun drawMCTextLabel(maxNumDivFive)
	(command "text" "J" "C" (list -375 12) 2 0 (rtos maxNumDivFive 2 2))
	(command "text" "J" "C" (list -365 12) 2 0 (rtos (* maxNumDivFive 2) 2 2))
	(command "text" "J" "C" (list -355 12) 2 0 (rtos (* maxNumDivFive 3) 2 2))
	(command "text" "J" "C" (list -345 12) 2 0 (rtos (* maxNumDivFive 4) 2 2))
)

(defun drawWLTextLabel(maxNumDivFive)
	(command "text" "J" "C" (list -375 2) 2 0 (rtos maxNumDivFive 2 0))
	(command "text" "J" "C" (list -365 2) 2 0 (rtos (* maxNumDivFive 2) 2 0))
	(command "text" "J" "C" (list -355 2) 2 0 (rtos (* maxNumDivFive 3) 2 0))
	(command "text" "J" "C" (list -345 2) 2 0 (rtos (* maxNumDivFive 4) 2 0))
)

(defun drawRockTextLabel()
	

	(cond ((= rockTypeNum 0)  
					(progn
						(command "text" "J" "L" (list -224 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
						(command "text" "J" "L" (list -209 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
						(command "text" "J" "L" (list -194 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockNumber 2 0))
						(command "text" "J" "L" (list -184 (+ (* rockDeep -5) 0.5)) rockTextSize 0 rockName)
						(command "mtext" (list -159 (+ (* rockDeep -5) (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" rockDescription "")
					) 
				)
			   ((= rockTypeNum 1)
					(progn 
						(command "text" "J" "L" (list -304 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
						(command "text" "J" "L" (list -289 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
					)
				)
	)
	
)

(defun drawDrillStructureTextLabel()
	(command "text" "J" "C" (list -30 (+ (* holeArtificialDeep -5) 2)) 3 0 (strcat (rtos  holeArtificialDeep 2 2) "m"))
	(command "text" "J" "C" (list -30 (+ (* holeDamperLocation -5) 2)) 3 0 (strcat (rtos  holeDamperLocation 2 2) "m"))
	(command "text" "J" "C" (list -30 (+ (* BushShoes -5) 2)) 3 0 (strcat (rtos  BushShoes 2 2) "m"))
	(command "text" "J" "C" (list -30 (+ (* d -5) 2)) 3 0 (strcat (rtos d 2 2) "m"))
)

;UI-Dimlinear
(defun drawDrillStructureDimlinear()
	(cond ((= HoleNum 0)  
					(progn
						(command "lweight" "ByLayer")			
						(command "dimtad" 1) 
						(command "dimasz" 2.5)
						;一开标注
						(command "dimlinear" (strcat "-49," (rtos (/ (* 1 (* HoleOneDeep -5)) 3) 2 2)) (strcat "-11," (rtos (/ (* 1 (* HoleOneDeep -5)) 3) 2 2)) "T" " " (strcat "-30," (rtos (/ (* 1 (* HoleOneDeep -5)) 3) 2 2))) ;
						(command "text" "J" "C" (list -29.5 (+ (/ (* 1 (* HoleOneDeep -5)) 3) 1.5)) 2.5 0 holeOneDrillDiameterStr)
						(command "text" "J" "C" (list -29.5 (- (/ (* 1 (* HoleOneDeep -5)) 3) 3.5)) 2.5 0 (strcat (rtos holeOneDeep 2 2) "m"))
						(command "dimlinear" (strcat "-46," (rtos (/ (* 2 (* HoleOneDeep -5)) 3) 2 2)) (strcat "-14," (rtos (/ (* 2 (* HoleOneDeep -5)) 3) 2 2)) "T" " " (strcat "-30," (rtos (/ (* 2 (* HoleOneDeep -5)) 3) 2 2))) ;
						(command "text" "J" "C" (list -29.5 (+ (/ (* 2 (* HoleOneDeep -5)) 3) 1.5)) 2.5 0 holeOneBushDiameterStr)
						(command "text" "J" "C" (list -29.5 (- (/ (* 2 (* HoleOneDeep -5)) 3) 3.5)) 2.5 0  (strcat (rtos holeOneDeep 2 2) "m"))
						;二开标注
						(command "dimlinear" (strcat "-43," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2)) (strcat "-17," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -29.5 (+ (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 1.5)) 2.5 0 holeTwoDrillDiameterStr)
						(command "text" "J" "C" (list -29.5 (- (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 3.5)) 2.5 0 (strcat (rtos HoleTwoDeep 2 2) "m"))
						(command "dimlinear" (strcat "-40," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2)) (strcat "-20," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -29.5 (+ (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 3) 1.5) 2.5 0 holeTwoBushDiameterStr)
						(command "text" "J" "C" (list -29.5 (- (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 3) 3.5) 2.5 0 (strcat (rtos HoleTwoDeep 2 2) "m"))
						
					 ))
				
			   ((= HoleNum 1)
					(progn 
						(command "lweight" "ByLayer")			
						(command "dimtad" 1) 
						(command "dimasz" 2.5)
						;一开标注
						(command "dimlinear" (strcat "-55," (rtos (/ (* 1 (* HoleOneDeep -5)) 3) 2 2)) (strcat "-5," (rtos (/ (* 1 (* HoleOneDeep -5)) 3) 2 2)) "T" " " (strcat "-30," (rtos (/ (* 1 (* HoleOneDeep -5)) 3) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (/ (* 1 (* HoleOneDeep -5)) 3) 1.5)) 2.5 0 holeOneDrillDiameterStr)
						(command "text" "J" "C" (list -30 (- (/ (* 1 (* HoleOneDeep -5)) 3) 3.5)) 2.5 0 (strcat (rtos holeOneDeep 2 2) "m"))
						(command "dimlinear" (strcat "-52," (rtos (/ (* 2 (* HoleOneDeep -5)) 3) 2 2)) (strcat "-8," (rtos (/ (* 2 (* HoleOneDeep -5)) 3) 2 2)) "T" " " (strcat "-30," (rtos (/ (* 2 (* HoleOneDeep -5)) 3) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (/ (* 2 (* HoleOneDeep -5)) 3) 1.5)) 2.5 0 holeOneBushDiameterStr)
						(command "text" "J" "C" (list -30 (- (/ (* 2 (* HoleOneDeep -5)) 3) 3.5)) 2.5 0  (strcat (rtos holeOneDeep 2 2) "m"))
						;二开标注
						(command "dimlinear" (strcat "-49," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2)) (strcat "-11," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 1.5)) 2.5 0 holeTwoDrillDiameterStr)
						(command "text" "J" "C" (list -30 (- (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 3.5)) 2.5 0 (strcat (rtos HoleTwoDeep 2 2) "m"))
						(command "dimlinear" (strcat "-46," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2)) (strcat "-14," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 1.5)) 2.5 0 holeTwoBushDiameterStr)
						(command "text" "J" "C" (list -30 (- (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 3.5)) 2.5 0  (strcat (rtos HoleTwoDeep 2 2) "m"))
						;三开标注
						(command "dimlinear" (strcat "-43," (rtos (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5) 2 2)) (strcat "-17," (rtos (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5) 1.5)) 2.5 0 holeThreeDrillDiameterStr)
						(command "text" "J" "C" (list -30 (- (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5)3.5)) 2.5 0 (strcat (rtos HoleThreeDeep 2 2) "m"))
						(command "dimlinear" (strcat "-40," (rtos (* (+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 2 2)) (strcat "-20," (rtos (* (+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (*(+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 1.5)) 2.5 0 holeThreeBushDiameterStr)
						(command "text" "J" "C" (list -30 (- (* (+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 3.5)) 2.5 0  (strcat (rtos HoleThreeDeep 2 2) "m"))
					))
	)
)


;UI-Hatch
(defun drawFrameBaseBlock()
	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(drawShaLiYan)
  	(command)

)

(defun hatchRock(rockName rockDeep rockThickness)
	(cond ((= rockTypeNum 0)  
					(progn
						(setq RockHatchXOffset -237.5)
					) 
				)
			   ((= rockTypeNum 1)
					(progn 
						(setq RockHatchXOffset -262.5)
					)
				)
	)
	
	(cond ((> rockThickness 1)  
							(progn
										(setq rockMaxThickness (+ (atoi  (rtos rockThickness 2 2)) 1))
										(setq rockFactor (/ rockThickness rockMaxThickness))
										(setq Selectionset (ssget "C" (list -329 (- (* d -5) 40.75)) (list -316 (- (* d -5) 45.25)))) ;
										(command)
										(creatBlockOrCheckBlock rockName (list -322.5 (- (* d -5) 45)) Selectionset)
										(command)
										;过薄计算需要找到插图框格的中位线
										(hatchRockWithMInsert rockName (list RockHatchXOffset (* rockDeep -5)) 1 rockFactor rockMaxThickness (* 5 rockFactor))
										(command)
										
							))
							 ((<= rockThickness 1)  
							(progn
										(setq Selectionset (ssget "C" (list -329 (- (* d -5) 40.75)) (list -316 (- (* d -5) 42)))) 
										(command)
										(creatBlockOrCheckBlock rockName (list -322.5 (- (* d -5) 45)) Selectionset)
										(command)
										(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (+ rockDeep (/ rockThickness 4)) -5)) 1 rockThickness 1 0)
										(command)
							))
	)
						
	
)

;Tool-Hatch
(defun hatchRockWithMInsert(BlockName InsertPoint xFactor yFactor rowCount rowSpace)  ;定义函数 
 (vl-load-com)  ;加载Vlisp函数  
  ;选择加密对象 		
			(command "minsert" BlockName InsertPoint  xFactor  yFactor 0 rowCount 1 rowSpace);用block命令制作块，然后用minsert命令制作多重引用块
		  ;将多用引用块改成匿名多重引用块。匿名块不是无名块，它是以*U命名的，在块 ;编辑框中不现实匿名块的块名，从而无法编辑。
			(command)
			;(command "minsert"   BlockName BasicPoint 1 1 0 1 1 0 )
			(vla-put-name (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (vla-get-name (vlax-ename->vla-object (entlast)))) "*U") 
			(command)
					;将多用引用块改成匿名多重引用块。匿名块不是无名块，它是以*U命名的，在块 ;编辑框中不现实匿名块的块名，从而无法编辑。
					(princ "\n加密完成")
			(command "zoom" "C" (list -222.5 (* d -5)) "250")
  			;加密完成后恢复参考图形
  			(cond ((= BlockName "黄土")  
					(progn
						(print "黄土")
							
					) 
				)
			   ((= BlockName "粉砂岩")
					(progn 
						(print "粉砂岩")
					)
				)  
				((= BlockName "细粒砂岩")
					(progn 
						(print "细粒砂岩")
					)
				)  
				((= BlockName "中粒砂岩")
					(progn 
						(print "中粒砂岩")
					)
				)  
				((= BlockName "粗粒砂岩")
					(progn 
						(print "粗粒砂岩")
					)
				)  
				((= BlockName "粉砂质泥岩")
					(progn 
						(print "粉砂质泥岩")
					)
				)  
				((= BlockName "泥质粉砂岩")
					(progn 
						(print "泥质粉砂岩")
					)
				)  
				((= BlockName "砂砾岩")
					(progn 
						(print "砂砾岩")
						(if (= rowCount 1) (drawShaLiYanOne) (drawShaLiYanTop))	
					)
				)  
				((= BlockName "泥岩")
					(progn 
						(print "泥岩")
					)
				)  
				((= BlockName "煤层")
					(progn 
						(print "煤层")
					)
				)  
				((= BlockName "炭质泥岩")
					(progn 
						(print "炭质泥岩")
					)
				)  
				((= BlockName "砂质泥岩")
					(progn 
						(print "砂质泥岩")
						(if (= rowCount 1) (drawShaZhiNiYanOne) (drawShaZhiNiYanTop))	
					)
				)  
				((= BlockName "泥质砂岩")
					(progn 
						(print "泥质砂岩")
					)
				)  
				((= BlockName "石灰岩")
					(progn 
						(print "石灰岩")
					)
				)  
				((= BlockName "泥质灰岩")
					(progn 
						(print "泥质灰岩")
					)
				)  
				((= BlockName "铝土质泥岩")
					(progn 
						(print "铝土质泥岩")
					)
				)  
				((= BlockName "假整合")
					(progn 
						(print "假整合")
					)
				)  
				((= BlockName "角度不整合")
					(progn 
						(print "角度不整合")
					)
				)  
	)
			
	
) 

(defun creatBlockOrCheckBlock(BlockName BasicPoint Selectionset) 
	(vl-load-com)  ;加载Vlisp函数  
	 (if Selectionset  ;选择加密对象 
			(progn    ;以当前时间作为块名  ;因为我们要用多重引用块进行加密，在制作多重引用块之前我们必须将我们需要 ;加密的图元制作成块，为避免与图档内块名冲突，引用了当前时间作为块名 
				(command "block" BlockName BasicPoint Selectionset "")
				(command)
			) 
			 (princ "\n没有选中图元")  
			
	) 
	 (princ)
)

(defun hatchDrillStructure()
	(cond ((= HoleNumFactor 1)  
						(progn
								;一开水泥反深
								(command "-hatch" "W" "N" "-49,0" "-46,0" (strcat "-46," (rtos (* holeOneDeep -5) 2 2))  (strcat "-49," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" "-11,0" "-14,0" (strcat "-14," (rtos (* holeOneDeep -5) 2 2))  (strcat "-11," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
						
								;二开水泥反深
								(command "-hatch" "W" "N" (strcat "-43," (rtos (* (- d holeTwoCement) -5) 2 2)) (strcat "-40," (rtos (* (- d holeTwoCement) -5) 2 2)) (strcat "-40," (rtos (* bushShoes -5) 2 2)) (strcat "-20," (rtos  (* bushShoes -5) 2 2)) (strcat "-20," (rtos (* (- d holeTwoCement) -5) 2 2)) (strcat "-17," (rtos (* (- d holeTwoCement) -5) 2 2)) (strcat "-17," (rtos  (* d -5) 2 2)) (strcat "-43," (rtos (* d -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" strHoleDamperLocationPtC (strcat "-30," (rtos (- (* holeDamperLocation -5) 2) 2 2)) "A" "@5<270" "@5<90" "L" strHoleDamperLocationPtD strBushShoesPtB strBushShoesPtA  "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" strHoleArtificialDeepPtA strHoleArtificialDeepPtB strHoleDamperLocationPtB strHoleDamperLocationPtA "C" "" "P" "DOTS" "1" "0" "")
						))
				   ((= HoleNumFactor 2)
						(progn 
							
								;一开水泥反深
								(command "-hatch" "W" "N" "-55,0" "-52,0" (strcat "-52," (rtos (* holeOneDeep -5) 2 2))  (strcat "-55," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" "-5,0" "-8,0" (strcat "-8," (rtos (* holeOneDeep -5) 2 2))  (strcat "-5," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
			
								;二开水泥反深
								(command "-hatch" "W" "N" (strcat "-49," (rtos (* holeTwoDeep -5) 2 2))(strcat "-46," (rtos (*  holeTwoDeep -5) 2 2)) (strcat "-46," (rtos (* (- holeTwoDeep holeTwoCement) -5) 2 2))  (strcat "-49," (rtos (* (- holeTwoDeep holeTwoCement) -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" (strcat "-14," (rtos (* holeTwoDeep -5) 2 2))(strcat "-11," (rtos (*  holeTwoDeep -5) 2 2)) (strcat "-11," (rtos (* (- holeTwoDeep holeTwoCement) -5) 2 2))  (strcat "-14," (rtos (* (- holeTwoDeep holeTwoCement) -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								
									
								;三开水泥反深
								(cond ((> holeTwoDeep holeThreeCement)  
													;水泥反深高于二开孔径
													(progn
														(setq strHoleThreeCementPlusPtA  (strcat "-46," (rtos (* (- holeTwoDeep holeThreeCement) -5) 2 2)))
														(setq strHoleThreeCementPlusPtB  (strcat "-40," (rtos (* (- holeTwoDeep holeThreeCement) -5) 2 2)))
														(setq strHoleThreeCementPlusPtC  (strcat "-40," (rtos (* holeTwoDeep -5) 2 2)))
														(setq strHoleThreeCementPlusPtD  (strcat "-46," (rtos (* holeTwoDeep -5) 2 2)))
														(setq strHoleThreeCementPlusPtE  (strcat "-20," (rtos (* (- holeTwoDeep holeThreeCement) -5) 2 2)))
														(setq strHoleThreeCementPlusPtF  (strcat "-14," (rtos (* (- holeTwoDeep holeThreeCement) -5) 2 2)))
														(setq strHoleThreeCementPlusPtG  (strcat "-14," (rtos (* holeTwoDeep -5) 2 2)))
														(setq strHoleThreeCementPlusPtH  (strcat "-20," (rtos (* holeTwoDeep -5) 2 2)))
														(command "-hatch" "W" "N" strHoleThreeCementPlusPtA strHoleThreeCementPlusPtB strHoleThreeCementPlusPtC strHoleThreeCementPlusPtD "C" "" "P" "DOTS" "1" "0" "")
														(command "-hatch" "W" "N" strHoleThreeCementPlusPtE strHoleThreeCementPlusPtF strHoleThreeCementPlusPtG strHoleThreeCementPlusPtH "C" "" "P" "DOTS" "1" "0" "")
														(command "-hatch" "W" "N" (strcat "-43," (rtos (* holeTwoDeep -5) 2 2)) (strcat "-40," (rtos (* holeTwoDeep -5) 2 2)) (strcat "-40," (rtos (* bushShoes -5) 2 2)) (strcat "-20," (rtos  (* bushShoes -5) 2 2)) (strcat "-20," (rtos (*  holeTwoDeep -5) 2 2)) (strcat "-17," (rtos (* holeTwoDeep -5) 2 2)) (strcat "-17," (rtos  (* d -5) 2 2)) (strcat "-43," (rtos (* d -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
													))
												
											((<= holeTwoDeep holeThreeCement)
													;水泥反深低于二开孔径
													(progn 
															;三开水泥反深
															(command "-hatch" "W" "N" (strcat "-43," (rtos (* holeThreeCement -5) 2 2)) (strcat "-40," (rtos (* holeThreeCement -5) 2 2)) (strcat "-40," (rtos (* bushShoes -5) 2 2)) (strcat "-20," (rtos  (* bushShoes -5) 2 2)) (strcat "-20," (rtos (* holeThreeCement -5) 2 2)) (strcat "-17," (rtos (* holeThreeCement -5) 2 2)) (strcat "-17," (rtos  (* d -5) 2 2)) (strcat "-43," (rtos (* d -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
													))
									)
									
									;浮箍浮鞋、、人工井底水泥填充
									(command "-hatch" "W" "N" strHoleDamperLocationPtC (strcat "-30," (rtos (- (* holeDamperLocation -5) 2) 2 2)) "A" "@5<270" "@5<90" "L" strHoleDamperLocationPtD strBushShoesPtB strBushShoesPtA  "C" "" "P" "DOTS" "1" "0" "")
									(command "-hatch" "W" "N" strHoleArtificialDeepPtA strHoleArtificialDeepPtB strHoleDamperLocationPtB strHoleDamperLocationPtA "C" "" "P" "DOTS" "1" "0" "")
						))  
		)

)




;Tools
(defun caculateMaxNum(num);计算最高位数的最高值
  	
  	(setq numDivTen (/ num 10))
	(cond 
		((and (>= numDivTen 10) (< numDivTen 100)) 
			(progn
				(setq numStr (rtos numDivTen 2 5));取出十位数并加1，并计算出单位一的因子数
				(setq numStr1 (substr numStr 2 1))
				(setq maxNumOfTen (* 10 (+ 1 (atof numStr1))))
			
				(setq numStr2 (substr numStr 1 1))
				(setq maxNumOfHundred (* 100 (atof numStr2)))
				(setq maxNumDivFive (/ (+ maxNumOfHundred maxNumOfTen) 5))
			)
		)

		((and (>= numDivTen 0.1) (< numDivTen 10))
				(progn
					(setq numStr (rtos numDivTen 2 5));取出十位数并加1，并计算出单位一的因子数
					(setq numStr1 (substr numStr 1 1))
					(setq maxNum (* 10 (+ 1 (atof numStr1))))
					(setq maxNumDivFive (/ maxNum 5))	
				)
		)
	
		((and (>= numDivTen 0) (< numDivTen 0.1))
				(progn
					(setq numStr (rtos numDivTen 2 5));取出十位数并加1，并计算出单位一的因子数
					(setq numStr1 (substr numStr 4 1))
					(setq maxNum (/ (+ 1 (atof numStr1)) 10))
					(setq maxNumDivFive (/ maxNum 5))
				)
		)
	)
)


(defun caculateFactor(num divnum);注意参数要是实数，若为整形计算值错误
	(setq factor (/ num divnum))							  		
)

(defun refresh()
	(command "regen")
 )

 ;BaseBlock
 
(defun drawShaLiYan()
	(drawShaLiYanTop)
  	(drawShaLiYanBottom)
 )

 (defun drawShaLiYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "circle" (strcat "-322.5," (rtos (- (* d -5) 41.25) 2 2)) 0.35)
	(command)
	(command "circle" (strcat "-325," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-326," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-320," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-319," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
	  (command "-hatch" "S" (ssget "W" (list -326.5 (- (* d -5) 40.75)) (list -324.5 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -320.5 (- (* d -5) 40.75)) (list -318.5 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
 )
 
(defun drawShaLiYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-330," (rtos (- (* d -5) 42.5) 2 2)) (strcat "-315," (rtos (- (* d -5) 42.5) 2 2)) "")
  	(command)
	(command "line" (strcat "-330," (rtos (- (* d -5) 45) 2 2)) (strcat "-315," (rtos (- (* d -5) 45) 2 2)) "")
	(command)
	;rowOne
	(command "circle" (strcat "-322.5," (rtos (- (* d -5) 41.25) 2 2)) 0.35)
	(command)
	(command "circle" (strcat "-325," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-326," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-320," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-319," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
	
	(command "-hatch" "S" (ssget "W" (list -326.5 (- (* d -5) 40.75)) (list -324.5 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -320.5 (- (* d -5) 40.75)) (list -318.5 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	;rowTwo
	(command "circle" (strcat "-325.5," (rtos (- (* d -5) 43.75) 2 2)) 0.35)
	(command)
	(command "circle" (strcat "-319.5," (rtos (- (* d -5) 43.75) 2 2)) 0.35)
	(command)
	(command "circle" (strcat "-323," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-322," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-317.5," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-316.5," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-327.5," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-328.5," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command)

	(command "-hatch" "S" (ssget "W" (list -323.5 (- (* d -5) 43.25)) (list -321.5 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -318 (- (* d -5) 43.25)) (list -316 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -329 (- (* d -5) 43.25)) (list -327 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
)

 (defun drawShaLiYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-330," (rtos (- (* d -5) 40) 2 2)) (strcat "-315," (rtos (- (* d -5) 50) 2 2)) "")
	(command)
	(command "line" (strcat "-330," (rtos (- (* d -5) 47.5) 2 2)) (strcat "-315," (rtos (- (* d -5) 47.5) 2 2)) "")
	(command)
	;rowThree
	(command "circle" (strcat "-322.5," (rtos (- (* d -5) 46.25) 2 2)) 0.35)
	(command)
	(command "circle" (strcat "-325," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-326," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-320," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-319," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command)
	
	(command "-hatch" "S" (ssget "W" (list -326.5 (- (* d -5) 45.75)) (list -324.5 (- (* d -5) 46.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -320.5 (- (* d -5) 45.75)) (list -318.5 (- (* d -5) 46.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	;rowFour
	(command "circle" (strcat "-325.5," (rtos (- (* d -5) 48.75) 2 2)) 0.35)
	(command)
	(command "circle" (strcat "-319.5," (rtos (- (* d -5) 48.75) 2 2)) 0.35)
	(command)
	(command "circle" (strcat "-323," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-322," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-317.5," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-316.5," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-327.5," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-328.5," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command)
   	(command "text" "J" "C" (list -322.5 (- (* d -5) 58.25)) 3 0 "砂砾岩")
   	(command)

	(command "-hatch" "S" (ssget "W" (list -323.5 (- (* d -5) 48.25)) (list -321.5 (- (* d -5) 49.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -318 (- (* d -5) 48.25)) (list -316 (- (* d -5) 49.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -329 (- (* d -5) 48.25)) (list -327 (- (* d -5) 49.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)

 )
 
 ;砂质泥岩
 (defun drawShaZhiNiYan()
	(drawShaZhiNiYanTop)
  	(drawShaZhiNiYanBottom)
 )
 
 (defun drawShaZhiNiYanOne()
	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-126," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-124.5," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command)
	(command "line" (strcat "-120.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-119," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command)
	(command "circle" (strcat "-122.5," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
  	(command "-hatch" "S" (ssget "W" (list -123 (- (* d -5) 40.75)) (list -122 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
 )
 
(defun drawShaZhiNiYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-130," (rtos (- (* d -5) 42.5) 2 2)) (strcat "-115," (rtos (- (* d -5) 42.5) 2 2)) "")
  	(command)
	(command "line" (strcat "-130," (rtos (- (* d -5) 45) 2 2)) (strcat "-115," (rtos (- (* d -5) 45) 2 2)) "")
	(command)
	;rowOne
	(command "line" (strcat "-126," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-124.5," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command)
	(command "line" (strcat "-120.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-119," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command)
	(command "circle" (strcat "-122.5," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command)
  	(command "-hatch" "S" (ssget "W" (list -123 (- (* d -5) 40.75)) (list -122 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	;rowTwo
	(command "line" (strcat "-127.5," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-129," (rtos (- (* d -5) 43.75) 2 2)) "")
	(command "line" (strcat "-121.75," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-123.25," (rtos (- (* d -5) 43.75) 2 2)) "")
	(command "line" (strcat "-117.75," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-116.25," (rtos (- (* d -5) 43.75) 2 2)) "")
	(command)
	(command "circle" (strcat "-125.25," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-119.75," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command)

	(command "-hatch" "S" (ssget "W" (list -120.5 (- (* d -5) 43.25)) (list -118.5 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -126.5 (- (* d -5) 43.25)) (list -124.5 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
)

(defun drawShaZhiNiYanBottom()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-130," (rtos (- (* d -5) 40) 2 2)) (strcat "-115," (rtos (- (* d -5) 50) 2 2)) "")
	(command)
	(command "line" (strcat "-130," (rtos (- (* d -5) 47.5) 2 2)) (strcat "-115," (rtos (- (* d -5) 47.5) 2 2)) "")
  	(command)
	(command "line" (strcat "-130," (rtos (- (* d -5) 50) 2 2)) (strcat "-115," (rtos (- (* d -5) 50) 2 2)) "")
	(command)
	;rowOne
	(command "line" (strcat "-126," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-124.5," (rtos (- (* d -5) 46.25) 2 2)) "")
	(command)
	(command "line" (strcat "-120.5," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-119," (rtos (- (* d -5) 46.25) 2 2)) "")
	(command)
	(command "circle" (strcat "-122.5," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command)
  	(command "-hatch" "S" (ssget "W" (list -123 (- (* d -5) 46.75)) (list -122 (- (* d -5) 45.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	;rowTwo
	(command "line" (strcat "-127.5," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-129," (rtos (- (* d -5) 48.75) 2 2)) "")
	(command "line" (strcat "-121.75," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-123.25," (rtos (- (* d -5) 48.75) 2 2)) "")
	(command "line" (strcat "-117.75," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-116.25," (rtos (- (* d -5) 48.75) 2 2)) "")
	(command)
	(command "circle" (strcat "-125.25," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command)
	(command "circle" (strcat "-119.75," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command)
	
	(command "-hatch" "S" (ssget "W" (list -120.5 (- (* d -5) 49.25)) (list -118.5 (- (* d -5) 48.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -126.5 (- (* d -5) 49.25)) (list -124.5 (- (* d -5) 48.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	
	 (command "text" "J" "C" (list -122.5 (- (* d -5) 58.25)) 3 0 "砂质泥岩")
   	(command)
)

;;;(defun beiyong()
;(setq damperShoesCenter (strcat "-30," (rtos (* (- bushShoes (/ (- bushShoes holeDamperLocation) 2)) -5) 2 2)));阻流环与浮鞋之间的矩形中心
								;(command "-hatch" "-30,-3875.00" "P" "DOTS" "15" "0" "")
								;(setq damperArtificialCenter (strcat "-30," (rtos (* (- holeDamperLocation (/ (- holeDamperLocation holeArtificialDeep) 2)) -5) 2 2)));阻流环与人工井底间的矩形中心
								;(command "-hatch" "-30,-3837.50" "P" "DOTS" "15" "0" "")
								;(entupd  (entlast))
						  		;(refresh)
						  		;(command "rectang" (strcat "-40," (rtos (- (* holeDamperLocation -5) 1.9) 2 2)) strBushShoesPtB);与圆环上部线交叉一点即可避开圆环填充
								;(setq eDamperShoes (entget (entlast)))
							  	;(setq eDamperShoesName (cdr (car eDamperShoes)))
								;(command "hatch" "DOTS" 15 0 "w" (strcat "-40," (rtos (- (* holeDamperLocation -5) 1.9) 2 2)) strBushShoesPtB "")
								;(command "_.Erase" eDamperShoesName "")
;;;						  		(command "rectang" strBushShoesPtA strHoleDamperLocationPtB);与圆环上部线交叉一点即可避开圆环填充
;;;								(setq eDamperArtificial (entget (entlast)))
;;;							  	(setq eDamperArtificialName (cdr (car eDamperArtificial)))
;;;								(command "hatch" "DOTS" 15 0 "w" strBushShoesPtA strHoleDamperLocationPtB "")
;;;								(command "_.Erase" eDamperArtificialName "")
;;;)

(defun hatchBeiyongPanduan()
(cond ((= rockName "黄土")  
					(progn
						(print "黄土")
							
					) 
				)
			   ((= rockName "粉砂岩")
					(progn 
						(print "粉砂岩")
					)
				)  
				((= rockName "细粒砂岩")
					(progn 
						(print "细粒砂岩")
					)
				)  
				((= rockName "中粒砂岩")
					(progn 
						(print "中粒砂岩")
					)
				)  
				((= rockName "粗粒砂岩")
					(progn 
						(print "粗粒砂岩")
					)
				)  
				((= rockName "粉砂质泥岩")
					(progn 
						(print "粉砂质泥岩")
					)
				)  
				((= rockName "泥质粉砂岩")
					(progn 
						(print "泥质粉砂岩")
					)
				)  
				((= rockName "砂砾岩")
					(progn 
						(print "砂砾岩");-316,-3984.5
					  
						
					) 
				)					
				((= rockName "泥岩")
					(progn 
						(print "泥岩")
					)
				)  
				((= rockName "煤层")
					(progn 
						(print "煤层")
					)
				)  
				((= rockName "炭质泥岩")
					(progn 
						(print "炭质泥岩")
					)
				)  
				((= rockName "砂质泥岩")
					(progn 
						(print "砂质泥岩")
					)
				)  
				((= rockName "泥质砂岩")
					(progn 
						(print "泥质砂岩")
					)
				)  
				((= rockName "石灰岩")
					(progn 
						(print "石灰岩")
					)
				)  
				((= rockName "泥质灰岩")
					(progn 
						(print "泥质灰岩")
					)
				)  
				((= rockName "铝土质泥岩")
					(progn 
						(print "铝土质泥岩")
					)
				)  
				((= rockName "假整合")
					(progn 
						(print "假整合")
					)
				)  
				((= rockName "角度不整合")
					(progn 
						(print "角度不整合")
					)
				)  
	)


)
