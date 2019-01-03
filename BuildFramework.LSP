(defun c:buildFramework()
  	(if (= loginSuccess 1) (DCLAndDataHandler) (loginStateDataHandler))
)

(defun loginStateDataHandler()
		(setq loginSdt 0)
	  	(setq loginId (load_dialog "G:\\工程二处\\drawDrillFramework\\LoginView.DCL"));BuildFrameWorkDCL testDCL Login
		(if (not (new_dialog "login" loginId)) (exit))
		(action_tile "loginBtn" "(configLoginBtn) (done_dialog 1)")
		(action_tile "closeBtn" "(done_dialog -1)")
		(setq loginSdt (start_dialog))
		(unload_dialog loginId)
		(cond 
						((= loginSdt 1)
								(progn
									(checkLoginState)
							)
						)
		  
						((= loginSdt -1)
							(progn
								(alert "外挂已退出")

							)
						)
		)
)

;Data
(defun DCLAndDataHandler()
	(setq std 0)
	(setq isMutiColor 1)
  	(setq id(load_dialog "G:\\工程二处\\drawDrillFramework\\AutoBuildFrameworkSimpleDCL.DCL"));BuildFrameWorkDCL testDCL Login AutoBuildFrameworkDCL
	(if (not (new_dialog "framework" id)) (exit))
	;配置启动按钮
  	(action_tile "startBtn" "(configStartBtn) (done_dialog 0)")
	;配置基础数据按钮
	(action_tile "baseDataBtn" "(configbaseDataBtn) (done_dialog 1)")
	;自动绘制按钮
	(action_tile "automationDrawBtn" "(done_dialog 2)")
	;配置框架按钮
	(action_tile "DFSBtn" "(configDFSBtn)  (done_dialog 3)")
	;配置曲线按钮
	(action_tile "DLBtn" "(configDLBtn)  (done_dialog 4)")
	;
	(action_tile "DRSBtn" "(configDRSBtn)  (done_dialog 5)")
	;
	(action_tile "DCDBtn" "(configDCDBtn) (done_dialog 6)")
	;
	(action_tile "DRSignBtn" "(configDRSignBtn)  (done_dialog 7)")
		;配置完井结构按钮
	(action_tile "DDCSBtn" "(configDDBtn)  (done_dialog 8)")
	;
	(action_tile "DDDBtn" "(configDDDBtn)  (done_dialog 9)")

	;配置单独清除已绘模型按钮群
	(action_tile "EFSBtn" "(done_dialog 10)")
	(action_tile "ELBtn" "(done_dialog 11)")
	(action_tile "ERSBtn" "(done_dialog 12)")
	(action_tile "ECDBtn" "(done_dialog 13)")
	(action_tile "ERSignBtn" "(done_dialog 14)")
	(action_tile "EDCSBtn" "(done_dialog 15)")
	(action_tile "EDDBtn" "(done_dialog 16)")	
	(action_tile "cancelBtn" "(done_dialog -1)")
	
	(if (= initSuccessValue 1)  
			(progn
				(mode_tile  "startBtn" 1)
				(mode_tile  "baseDataBtn" 0)
				(mode_tile  "automationDrawBtn" 0)
				(mode_tile  "DFSBtn" 0)
				(mode_tile  "DLBtn" 0)
				(mode_tile  "DDCSBtn" 0)
				(mode_tile  "DCDBtn" 0)
				(mode_tile  "DRSBtn" 0)
				(mode_tile  "DRSignBtn" 0)
				(mode_tile  "DDDBtn" 0)
				(mode_tile  "EFSBtn" 0)
				(mode_tile  "ELBtn" 0)
				(mode_tile  "ERSBtn" 0)
				(mode_tile  "ECDBtn" 0)
				(mode_tile  "ERSignBtn" 0)
				(mode_tile  "EDCSBtn" 0)
				(mode_tile  "EDDBtn" 0)

			)
			(progn
				(mode_tile  "startBtn" 0)
				(mode_tile  "baseDataBtn" 1)
				(mode_tile  "automationDrawBtn" 1)
				(mode_tile  "DFSBtn" 1)
				(mode_tile  "DLBtn" 1)
				(mode_tile  "DDCSBtn" 1)
				(mode_tile  "DCDBtn" 1)
				(mode_tile  "DRSBtn" 1)
				(mode_tile  "DRSignBtn" 1)
				(mode_tile  "DDDBtn" 1)
				(mode_tile  "EFSBtn" 1)
				(mode_tile  "ELBtn" 1)
				(mode_tile  "ERSBtn" 1)
				(mode_tile  "ECDBtn" 1)
				(mode_tile  "ERSignBtn" 1)
				(mode_tile  "EDCSBtn" 1)
				(mode_tile  "EDDBtn" 1)

			)
	)
	;存储上次输入记录
  	;(storageLastDCLValue)
 	(setq std (start_dialog))
  	(unload_dialog id)
	(cond 
					((= std 0)
						(progn
							(alert "辅助程序启动完毕，请开始绘图！！")
							(configInitValue)
							(configBaseData)
						  	(c:buildFramework)
						)
					)
					((= std 1)
						(progn
							(configBaseData)
						  	(c:buildFramework)
						)
					)
					((= std 2)
						(progn
							(alert "开始自动绘图了！！！！！")
							(drawHolePictureAutomation)
						  	(c:buildFramework)
						)
					)
					((= std 3)
						(progn
							(drawFrame)
							(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
						  	(c:buildFramework)
						)
					)
					
					((= std 4)
						(progn
							(drawPLine)
						  	(c:buildFramework)
						)
					)
					
					((= std 5)
						(progn
							(drawRockStructure)
						  	(c:buildFramework)
						)
					)
					
					((= std 6)
						(progn
							(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
							(drawColumnarDiagram)
							(c:buildFramework)
							;当已绘制柱状图后才设置已绘标记，用来允许单独删除该部分模型
						)
					)
					((= std 7)
						(progn					
							(drawRockSign)
							(c:buildFramework)
						)
					)
					((= std 8)
						(progn					
							(drawDrillStructure)
						  	(c:buildFramework)
						)
					)
					((= std 9)
						(progn		
							(drawDrillDeviation)
							(c:buildFramework)
						)
					)
					((= std 10)
						(progn		
							(eraseHolePicture)
							(c:buildFramework)
						)
					)
					((= std 11)
						(progn			
							;单独删除柱状图
							(eraseLine)
							(c:buildFramework)
						)
					)
					((= std 12)
						(progn			
							;单独删除柱状图
							(eraseDrillRockStructure)
							(c:buildFramework)
						)
					)
					((= std 13)
						(progn			
							;单独删除柱状图
						
							(eraseWellColumnarDiagram)
							(c:buildFramework)
						)
					)
					((= std 14)
						(progn	
							;单独删除标志层代号
							(eraseRockSign)
							(c:buildFramework)
						)
					)
					((= std 15)
						(progn				
							;单独删除完井结构
						    (EraseDrillCompletionStructure)
							(c:buildFramework)
						)
					)
					((= std 16)
						(progn			
							;单独删除井斜方位角
						    (eraseEraseDrillDeviation)
							(c:buildFramework)
						)
					)
  					((= std -1)
						(progn
							(alert "退出辅助程序成功！！")

						)
					)
	)
 	
  	(princ)
)

;配置初始项
(defun configInitValue()
	;配置填充比例统一英寸与毫米
	(command "measurement" 1)
	;配置-hatch命令初始值
	(command "zoom" "w" (strcat "-150," (rtos (- (* 100 -5) 40) 2 2)) (strcat "-125," (rtos (- (* 100 -5) 50) 2 2)) )
	(command "circle" (strcat "-137.5," (rtos (- (* 100 -5) 41.25) 2 2)) 0.10)
	(command "-hatch" "S" (ssget "W" (list -138 (- (* 100 -5) 40.75)) (list -137 (- (* 100 -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "." "")
	(eraseLastEnt)
	(eraseLastEnt)
	(setq initSuccessValue 1)
)

(defun resetHatchConfiguration()
	(command "zoom" "w" (strcat "-150," (rtos (- (* 100 -5) 40) 2 2)) (strcat "-125," (rtos (- (* 100 -5) 50) 2 2)) )
	(command "circle" (strcat "-137.5," (rtos (- (* 100 -5) 41.25) 2 2)) 0.10)
	(command "-hatch" "S" (ssget "W" (list -138 (- (* 100 -5) 40.75)) (list -137 (- (* 100 -5) 41.75))) "" "P" "SOLID" "CO"  "T" "255,255,255" "" "")
	(eraseLastEnt)
	(eraseLastEnt)
)

(defun configLoginBtn()
	(setq username (get_tile "username"))
	(setq password (get_tile "password"))
)

(defun configStartBtn()
	;此处可增加初始化代码，预留位置
	(print 111111)
)


(defun configbaseDataBtn()
	;此处可增加初始化代码，预留位置
	(print 111111)
)

(defun configDFSBtn()
	(print 111111)
)

;配置画线按钮属性
(defun configDLBtn()
  (print 11111)
)


;配置画完井结构按钮属性
(defun configDDBtn()
	(print 11111)
)

;配置系统组按钮属性
(defun configDRSBtn()
	(print 11111)
)

;配置井柱状图按钮属性
(defun configDCDBtn()
	(print 11111)
)

;配置标志层按钮属性
(defun configDRSignBtn()
  	(print 11111)
)

;配置井斜方位角按钮属性
(defun configDDDBtn()
 	(print 11111)
)

;配置二开三开popList动作属性
(defun configHolePopListAction()
  (print 11111)
)

;配置岩性柱状图popList动作属性
(defun configRockPopListAction()
  (print 11111)
)


;UI
;UI-Frame

(defun drawHolePictureAutomation()
		(drawFrame)
		(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
		(drawPLine)
		(drawRockStructure)
		(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
		(drawColumnarDiagram)
		(drawRockSign)
		(drawDrillStructure)
		(drawDrillDeviation)
)

(defun drawFrame()
	(configNormalLineType)
	(drawFrameLine)
	(drawFrameTextLabel)
	(drawBaseDataTable)
	(drawPictureDataTable)
)

;绘制系统组
(defun drawRockStructure()
	(configNormalLineType)
	(drawXiStructure)
	(drawTongStructure)
	(drawZuStructure)
)

;系
(defun drawXiStructure()
	(setq xiStructureLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property xiDataOfYRange 'Value2))))
	(setq xiIndexA 1)
	(setq xiStructureListLength (length xiStructureLst))
	(while (<= xiIndexA xiStructureListLength)
		(if (> xiIndexA 0)
			  (progn
					(setq xiStructureDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property xiDataOfYRange 'Item xiIndexA)) 'Value2))
					(setq xiStructureThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property xiDataThicknessRange 'Item xiIndexA)) 'Value2))
					(setq xiStructureHoleName (Vlax-Get (vlax-variant-value (Vlax-Get-Property xiDataHoleNameRange 'Item xiIndexA)) 'Value2))
					(setq xiStructureSimpleName (Vlax-Get (vlax-variant-value (Vlax-Get-Property xiDataSimpleNameRange 'Item xiIndexA)) 'Value2))
					(drawRockStructureLineAndTextLabel 0 xiDataOfYRange xiStructureDeep xiStructureThickness xiStructureHoleName xiStructureSimpleName -330 xiIndexA)
			)
		)
		(setq xiIndexA (1+ xiIndexA))
	)
)

;统
(defun drawTongStructure()
	(setq tongStructureLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property tongDataOfYRange 'Value2))))
	(setq tongIndexA 1)
	(setq tongStructureListLength (length tongStructureLst))
	(while (<= tongIndexA tongStructureListLength)
		(if (> tongIndexA 0)
			  (progn
					(setq tongStructureDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property tongDataOfYRange 'Item tongIndexA)) 'Value2))
					(setq tongStructureThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property tongDataThicknessRange 'Item tongIndexA)) 'Value2))
					(setq tongStructureHoleName (Vlax-Get (vlax-variant-value (Vlax-Get-Property tongDataHoleNameRange 'Item tongIndexA)) 'Value2))
					(setq tongStructureSimpleName (Vlax-Get (vlax-variant-value (Vlax-Get-Property tongDataSimpleNameRange 'Item tongIndexA)) 'Value2))
					(drawRockStructureLineAndTextLabel 1 tongDataOfYRange tongStructureDeep tongStructureThickness tongStructureHoleName tongStructureSimpleName -320 tongIndexA)
			)
		)
		(setq tongIndexA (1+ tongIndexA))
	)
)

;组
(defun drawZuStructure()
	(setq zuStructureLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property zuDataOfYRange 'Value2))))
	(setq zuIndexA 1)
	(setq zuStructureListLength (length zuStructureLst))
	(while (<= zuIndexA zuStructureListLength)
		(if (> xiIndexA 0)
			  (progn
					(setq zuStructureDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property zuDataOfYRange 'Item zuIndexA)) 'Value2))
					(setq zuStructureThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property zuDataThicknessRange 'Item zuIndexA)) 'Value2))
					(setq zuStructureHoleName (Vlax-Get (vlax-variant-value (Vlax-Get-Property zuDataHoleNameRange 'Item zuIndexA)) 'Value2))
					(setq zuStructureSimpleName (Vlax-Get (vlax-variant-value (Vlax-Get-Property zuDataSimpleNameRange 'Item zuIndexA)) 'Value2))
					(drawRockStructureLineAndTextLabel 2 zuDataOfYRange zuStructureDeep zuStructureThickness zuStructureHoleName zuStructureSimpleName -310 zuIndexA)
			)
		)
		(setq zuIndexA (1+ zuIndexA))
	)
)

;绘制钻井和测井柱状图
(defun drawColumnarDiagram()
	(configNormalLineType)
	(resetHatchConfiguration)
  	(drawWellLoggingColumnarDiagram)
	(drawWellDrillingColumnarDiagram)
)

;测井柱状图
(defun drawWellLoggingColumnarDiagram()
		(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
		(setq wlcdLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property wlcdDataOfYRange 'Value2))))
		(command "LineType" "s" "ByLayer" "")
		(command "lweight" "ByLayer")
	 
		(setq wlcdIndexA 1)
		(setq wlcdListLength (length wlcdLst))
		(while (<= wlcdIndexA wlcdListLength)
			(if (> wlcdIndexA 0)
				  (progn
				  	 	(setq wlcdRockDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property wlcdDataOfYRange 'Item wlcdIndexA)) 'Value2))
						(setq wlcdThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property wlcdDataThicknessRange 'Item wlcdIndexA)) 'Value2))
						(setq wlcdName (Vlax-Get (vlax-variant-value (Vlax-Get-Property wlcdDataNameRange 'Item wlcdIndexA)) 'Value2))
						
						(setq wlcdPyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property wlcdDataOfYRange 'Item wlcdIndexA)) 'Value2))	
						(drawWLCDLineAndTextLabel)
						(setq BlockCodeName (strcat (strcat (strcat wlcdName (itoa wlcdIndexA)) "测井柱状图") (rtos (getvar "CDATE") 2 10)))
						(hatchRock wlcdName wlcdRockDeep wlcdThickness -262.5)
					
				  )
			)
			(setq wlcdIndexA (1+ wlcdIndexA))
		)
	(drawFrameBaseBlock)
)

;钻井柱状图
(defun drawWellDrillingColumnarDiagramdrawWellDrillingColumnarDiagram()
		(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")		
		(setq wdcdLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property wdcdDataOfYRange 'Value2))))
		(command "LineType" "s" "ByLayer" "")
		(command "lweight" "ByLayer")
		;----------------测试多线程----------------------
		(setq wdcdDeep 0.0)
		;(Form1_load)
		;(setq loop Loop_Start)
		(exam)
		(setq wdcdIndexA 1)
		(setq wdcdListLength (length wdcdLst))
		(while (<= wdcdIndexA wdcdListLength)
			(if (> wdcdIndexA 0)
				  (progn
				  	 	(setq wdcdDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property wdcdDataOfYRange 'Item wdcdIndexA)) 'Value2))
						(setq wdcdThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property wdcdDataThicknessRange 'Item wdcdIndexA)) 'Value2))
						(setq wdcdName (Vlax-Get (vlax-variant-value (Vlax-Get-Property wdcdDataNameRange 'Item wdcdIndexA)) 'Value2))
						(setq wdcdDescription (Vlax-Get (vlax-variant-value (Vlax-Get-Property wdcdDataDescription 'Item wdcdIndexA)) 'Value2))
						(setq wdcdNumber (Vlax-Get (vlax-variant-value (Vlax-Get-Property wdcdDataNumber 'Item wdcdIndexA)) 'Value2))
						(setq wdcdPyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property wdcdDataOfYRange 'Item wdcdIndexA)) 'Value2))	
						;(setq loop Loop_Start)
						;---------------------预定义加载-------------------------------
						;(vlax-add-cmd "exam" 'dynamicZooming "examX" acrx_cmd_transparent)
						;---------------------预定义加载结束-------------------------------
						(drawWDCDLineAndTextLabel)
						(setq BlockCodeName (strcat (strcat (strcat wdcdName (itoa wdcdIndexA)) "钻井柱状图") (rtos (getvar "CDATE") 2 10)))
						(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")	
						(hatchRock wdcdName wdcdDeep wdcdThickness -237.5)
						
						(command "'exam")
					
						
						;---------------------预定义加载-------------------------------
						
						;---------------------预定义加载结束-------------------------------
						;------------------测试多线程----------------------
						;(setq loop loop_stop)	
								
					)
			)
			(setq wdcdIndexA (1+ wdcdIndexA))
		)
	;(removeExam)
	(drawFrameBaseBlock)
	;------------------测试多线程----------------------
	;(setq loop loop_stop)
		
)


;绘制系统组
(defun drawRockSign()
		(configNormalLineType)

		(setq rockSignLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property rockSignDataOfYRange 'Value2))))

		(setq RSignIndexA 1)
		(setq rockSignListLength (length rockSignLst))
		(while (<= RSignIndexA rockSignListLength)
			(if (> RSignIndexA 0)
				  (progn
				  	 	(setq rockSignDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockSignDataOfYRange 'Item RSignIndexA)) 'Value2))
						(setq rockSignThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockSignDataThicknessRange 'Item RSignIndexA)) 'Value2))
						(setq rockSignName (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockSignDataNameRange 'Item RSignIndexA)) 'Value2))
						(drawRockSignTextLabel)
				)
			)
			(setq RSignIndexA (+ 1 RSignIndexA))
		)
)

;完井结构
(defun drawDrillStructure()
	(configNormalLineType)
	(drawDrillStructureLine)
	(drawDrillStructureTextLabel)
	(drawDrillStructureDimlinear)
  	(hatchDrillStructure)
)

;绘制井斜方位
(defun drawDrillDeviation()
  		;(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))
	
		(configNormalLineType)
		(setq drillDeviationLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property drillDeviationDataOfYRange 'Value2))))

		(setq drillDeviationIndexA 1)
		(setq drillDeviationListLength (length drillDeviationLst))
		(while (<= drillDeviationIndexA drillDeviationListLength)
			(if (> drillDeviationIndexA 0)
				  (progn
				  	 	(setq drillDeviationDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillDeviationDataOfYRange 'Item drillDeviationIndexA)) 'Value2))
						(setq drillDeviationDeviation (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillDeviationDataDeviationRange 'Item drillDeviationIndexA)) 'Value2))
						(setq drillDeviationAzimuth (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillDeviationDataAzimuthRange 'Item drillDeviationIndexA)) 'Value2))
						(drawDrillDeviationLineAndTextLabel)
				)
			)
			(setq drillDeviationIndexA (+ 1 drillDeviationIndexA))
		)
)

;UI-PLine

(defun drawPLine()
	(configNormalLineType)
	(drawDrillingTimePLine)
	(drawConsumptionPline)
	(drawWaterLevelPline)
)
;drillingTimePline
(defun drawDrillingTimePLine()
	
	(command "LineType" "s" "ByLayer" "")
	(command "lweight" "ByLayer")
	(setq drillingTimePlineLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property drillingTimePlineDataRangeOfY 'Value2))))
	(setq drillingTimeIndexA 1)
  	(setq drillingTimePlineListLength (length drillingTimePlineLst))
	(setq drillingTimePlineListPT '())
	(while (<= drillingTimeIndexA drillingTimePlineListLength)
			(print (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillingTimePlineDataRangeOfY 'Item drillingTimeIndexA )) 'Value2 ))
		(if (> drillingTimeIndexA 0)
			  (progn
				(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillingTimePlineDataRangeOfY 'Item drillingTimeIndexA)) 'Value2))
				(setq pxA (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillingTimePlineDataRangeOfX 'Item drillingTimeIndexA)) 'Value2))
				(setq pyA (* pyA -5))
				;drillingTimeFactor
			
				(setq pxA (+ (* pxA drillingTimePLFactor) drillingTimeXOffset))
					
				(setq ptA (strcat (strcat (strcat (strcat "(" (rtos pxA 2 6)) " ") (rtos pyA 2 6)) ")"))
				(setq drillingTimePlineListPT (cons (read ptA) drillingTimePlineListPT))
			)
		)
		(setq drillingTimeIndexA (1+ drillingTimeIndexA))
	)
			
	 ;;;;有顺序，不可改
				(entmake 
					(append 
						(list '(0 . "LWPOLYLINE")
							 '(100 . "AcDbEntity")
							 '(100 . "AcDbPolyline")
							 (cons 90 (length drillingTimePlineListPT)) ; 顶点个数
							'(70 . 0) ; 1 闭合 0 不闭合 不是必须，默认为0
						)
						(mapcar '(lambda (x) (cons 10 x)) drillingTimePlineListPT)
				 ;二维坐标列表 加组码 10
					)    ;append end
				)
				
			(if (= isMutiColor 1) (Vlax-Put-Property (Vlax-Ename->Vla-Object (cdr (car (entget (entlast))))) 'Color 100 ))
				
)

;consumptionPline
(defun drawConsumptionPline()
	(command "LineType" "s" "ByLayer" "")
	(command "lweight" "ByLayer")

	(setq consumptionPlineLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property consumptionPlineDataRangeOfY 'Value2))))
	(setq consumptionIndexA 1)
  	(setq drillingTimePlineListLength (length consumptionPlineLst))
	(setq consumptionPlineListPT '())
	(while (<= consumptionIndexA drillingTimePlineListLength)
			(print (Vlax-Get (vlax-variant-value (Vlax-Get-Property consumptionPlineDataRangeOfY 'Item consumptionIndexA )) 'Value2 ))
		(if (> consumptionIndexA 0)
			  (progn
				(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property consumptionPlineDataRangeOfY 'Item consumptionIndexA)) 'Value2))
				(setq pxA (Vlax-Get (vlax-variant-value (Vlax-Get-Property consumptionPlineDataRangeOfX 'Item consumptionIndexA)) 'Value2))
				(setq pyA (* pyA -5))
				;drillingTimeFactor
			
				(setq pxA (+ (* pxA consumptionPLFactor) consumptionXOffset))
					
				(setq ptA (strcat (strcat (strcat (strcat "(" (rtos pxA 2 6)) " ") (rtos pyA 2 6)) ")"))
				(setq consumptionPlineListPT (cons (read ptA) consumptionPlineListPT))
			)
		)
		(setq consumptionIndexA (1+ consumptionIndexA))
	)
		
	 ;;;;有顺序，不可改
				(entmake 
					(append 
						(list '(0 . "LWPOLYLINE")
							 '(100 . "AcDbEntity")
							 '(100 . "AcDbPolyline")
							 (cons 90 (length consumptionPlineListPT)) ; 顶点个数
							'(70 . 0) ; 1 闭合 0 不闭合 不是必须，默认为0
						)
						(mapcar '(lambda (x) (cons 10 x)) consumptionPlineListPT)
				 ;二维坐标列表 加组码 10
					)    ;append end
				)
			(if (= isMutiColor 1) (Vlax-Put-Property (Vlax-Ename->Vla-Object (cdr (car (entget (entlast))))) 'Color 200 ))

)

;waterLevel
(defun drawWaterLevelPline()
	(command "LineType" "s" "ACAD_ISO02W100" "")
	(command "lweight" "ByLayer")
	(setq waterLevelPlineLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property waterLevelPlineDataRangeOfY 'Value2))))
	(setq waterLevelIndexA 1)
  	(setq waterLevelPlineListLength (length waterLevelPlineLst))
	(setq waterLevelPlineListPT '())
	(while (<= waterLevelIndexA waterLevelPlineListLength)
			(print (Vlax-Get (vlax-variant-value (Vlax-Get-Property waterLevelPlineDataRangeOfY 'Item waterLevelIndexA )) 'Value2 ))
		(if (> waterLevelIndexA 0)
			  (progn
				(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property waterLevelPlineDataRangeOfY 'Item waterLevelIndexA)) 'Value2))
				(setq pxA (Vlax-Get (vlax-variant-value (Vlax-Get-Property waterLevelPlineDataRangeOfX 'Item waterLevelIndexA)) 'Value2))
				(setq pyA (* pyA -5))
				;drillingTimeFactor
			
				(setq pxA (+ (* pxA waterLevelPLFactor) waterLevelXOffset))
					
				(setq ptA (strcat (strcat (strcat (strcat "(" (rtos pxA 2 6)) " ") (rtos pyA 2 6)) ")"))
				(setq waterLevelPlineListPT (cons (read ptA) waterLevelPlineListPT))
			)
		)
		(setq waterLevelIndexA (1+ waterLevelIndexA))
	)

	 ;;;;有顺序，不可改
				(entmake 
					(append 
						(list '(0 . "LWPOLYLINE")
							 '(100 . "AcDbEntity")
							 '(100 . "AcDbPolyline")
							 (cons 90 (length waterLevelPlineListPT)) ; 顶点个数
							'(70 . 0) ; 1 闭合 0 不闭合 不是必须，默认为0
						)
						(mapcar '(lambda (x) (cons 10 x)) waterLevelPlineListPT)
				 ;二维坐标列表 加组码 10
					)    ;append end
				)
				(if (= isMutiColor 1) (Vlax-Put-Property (Vlax-Ename->Vla-Object (cdr (car (entget (entlast))))) 'Color 50 ))
				(configNormalLineType)
)


;UI-Line

(defun drawFrameLine()
  	(command "lweight" 1.00)
  	(command "line" "-450,140" "60,140" "")
  	(setq floatPyRough (+ (* drillDeepMaxData -5) -100));柱状图比例为1：:200
	(setq floatPyThinTop  (* drillDeepMaxData -5) );
	(setq floatPyThinDown (+ (* drillDeepMaxData -5) -95));
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
	(if (= isMutiColor 1) (Vlax-Put-Property (Vlax-Ename->Vla-Object (cdr (car (entget (entlast))))) 'Color 100 ))

	
	(command "LineType" "s"  "ACAD_ISO02W100" "")
	(command "line" "-388,0" "-335,0" "")
	(if (= isMutiColor 1) (Vlax-Put-Property (Vlax-Ename->Vla-Object (cdr (car (entget (entlast))))) 'Color 50 ))

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
	(if (= isMutiColor 1) (Vlax-Put-Property (Vlax-Ename->Vla-Object (cdr (car (entget (entlast))))) 'Color 200))
	
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

	(command "lweight" 0.50)
  	(command "line" "-270,0" (strcat "-270," strThinTopPy) "");空白分界线
  
  	(command "line" "-255,0" (strcat "-255," strThinTopPy) "");测井柱状分界线
  
  	(command "line" "-245,0" (strcat "-245," strThinTopPy) "");标志层代号分界线
  
  	(command "line" "-230,0" (strcat "-230," strThinTopPy) "");钻井柱状分界线
	
	(command "lweight" "ByLayer")
	(command "line" "-270,30" (strcat "-270," "0") "");空白分界线
  
  	(command "line" "-255,30" (strcat "-255," "0") "");测井柱状分界线
  
  	(command "line" "-245,30" (strcat "-245," "0") "");标志层代号分界线
  
  	(command "line" "-230,30" (strcat "-230," "0") "");钻井柱状分界线
	
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

(defun drawRockStructureLineAndTextLabel(rockStructureTypeNum rockStructureDataRangeOfY rockStructureDeep rockStructureThickness rockStructureHoleName rockStructureSimpleName RockStructureXOffset indexRS)
	(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockStructureDataRangeOfY 'Item indexRS)) 'Value2))
	(setq pyA (* pyA -5))
	(setq pxA  RockStructureXOffset)
	(setq holeNameLength (/ (strlen  rockStructureHoleName) 2)) ;strlen中文单位为2
	(setq indexRockStructure 1.0)
	(setq rockStructureindexA 1.0)
	(command "line" (strcat (strcat (rtos (- RockStructureXOffset 5) 2 2) ",") (rtos pyA 2 2)) (strcat (strcat (rtos (+ RockStructureXOffset 5) 2 2) ",") (rtos pyA 2 2)) "")
	
	(if (<= rockStructureThickness (* (+ holeNameLength 2) 4))
		(progn
			(if (<= rockStructureThickness 8)
				(progn
					(cond ((= rockStructureTypeNum 0)
								(progn
									(command "text" "J" "C" (list pxA  (* (- rockStructureDeep (/ rockStructureThickness 4)) -5)) rockStructureTextSize 0 rockStructureSimpleName)
								))
								((= rockStructureTypeNum 1)
								(progn
									(command "text" "J" "C" (list pxA (* (- rockStructureDeep (/ rockStructureThickness 4)) -5)) rockStructureTextSize 0 (substr rockStructureSimpleName 1 1))							
									(command "text" "J" "C" (list (+ pxA 2) (* (- rockStructureDeep (/ rockStructureThickness 4)) -5)) 1 0 (substr rockStructureSimpleName 2))							

								))
								((= rockStructureTypeNum 2)
								(progn
									(command "text" "J" "C" (list pxA (* (- rockStructureDeep (/ rockStructureThickness 4)) -5)) rockStructureTextSize 0 (substr rockStructureSimpleName 1 1))							
									(command "text" "J" "C" (list (+ pxA 2) (* (- rockStructureDeep (/ rockStructureThickness 4)) -5)) 1 0 (substr rockStructureSimpleName 2))							
								))
					)
					
				)
				(progn
					;(command "text" "J" "C" (list pxA  (+ (* (- rockStructureDeep (/ rockStructureThickness 2)) -5) 8)) (- rockStructureTextSize 1) 0 rockStructureHoleName)

					(cond ((= rockStructureTypeNum 0)
								(progn
									(command "text" "J" "C" (list pxA  (+ (* (- rockStructureDeep (/ rockStructureThickness 2)) -5) 4)) rockStructureTextSize 0 rockStructureSimpleName)
								))
								((= rockStructureTypeNum 1)
								(progn
									(command "text" "J" "C" (list pxA (+ (* (- rockStructureDeep (/ rockStructureThickness 2)) -5) 4)) rockStructureTextSize 0 (substr rockStructureSimpleName 1 1))							
									(command "text" "J" "C" (list (+ pxA 2) (+ (* (- rockStructureDeep (/ rockStructureThickness 2)) -5) 4)) 1 0 (substr rockStructureSimpleName 2))							

								))
								((= rockStructureTypeNum 2)
								(progn
									(command "text" "J" "C" (list pxA (+ (* (- rockStructureDeep (/ rockStructureThickness 2)) -5) 4)) rockStructureTextSize 0 (substr rockStructureSimpleName 1 1))							
									(command "text" "J" "C" (list (+ pxA 2) (+ (* (- rockStructureDeep (/ rockStructureThickness 2)) -5) 4)) 1 0 (substr rockStructureSimpleName 2))							
								))
					)
				)
			)
		)
		(progn
			(while (<= indexRockStructure holeNameLength)
				(if (> indexRockStructure 0)
						(progn
							(cond ((= indexRockStructure 1)
								(progn
									;0.75*5 约等于4
									(command "text" "J" "C" (list pxA (- (* (- rockStructureDeep rockStructureThickness) -5) 10)) rockStructureTextSize 0 (substr rockStructureHoleName 1 2))

								))
								((and (> indexRockStructure 1) (< indexRockStructure holeNameLength))
								(progn
									(command "text" "J" "C" (list pxA  (* (- rockStructureDeep (* rockStructureThickness (- 1 (/ (- indexRockStructure 1) (- holeNameLength 1))))) -5)) rockStructureTextSize 0 (substr rockStructureHoleName (+ 1 (* 2 (- (fix indexRockStructure) 1))) 2))
									
								))
								((= indexRockStructure holeNameLength)   
								(progn
									(command "text" "J" "C" (list pxA (+ (* rockStructureDeep -5) 10)) rockStructureTextSize 0 (substr rockStructureHoleName (+ 1 (* 2 (- (fix indexRockStructure) 1))) 2))
										(cond ((= rockStructureTypeNum 0)
												(progn
													(command "text" "J" "C" (list pxA  (+ (* rockStructureDeep -5) 2)) rockStructureTextSize 0 rockStructureSimpleName)
												))
											((= rockStructureTypeNum 1)
												(progn
													(command "text" "J" "C" (list pxA (+ (* rockStructureDeep -5) 2)) rockStructureTextSize 0 (substr rockStructureSimpleName 1 1))							
													(command "text" "J" "C" (list (+ pxA 2) (+ (* rockStructureDeep -5) 2)) 1 0 (substr rockStructureSimpleName 2))							

												))
											((= rockStructureTypeNum 2)
												(progn
													(command "text" "J" "C" (list pxA (+ (* rockStructureDeep -5) 2)) rockStructureTextSize 0 (substr rockStructureSimpleName 1 1))							
													(command "text" "J" "C" (list (+ pxA 2) (+ (* rockStructureDeep -5) 2)) 1 0 (substr rockStructureSimpleName 2))							
												))
											)

								))
							)
						)
				)
				(setq indexRockStructure (1+ indexRockStructure))

			)
		)
	)
)

(defun drawRockLineAndTextLabel()		
	;处理划线数据

	(setq pyA (* pyA -5))
	(setq pxA  RockXOffset)


	(cond ((= rockTypeNum 0)  
					(progn
							(setq pxA (+ RockXOffset 20))
							(setq pxB (+ RockXOffset 185))
							(if (= rockName "黄土")  
								(progn
									(setq pxA (+ RockXOffset 15))
									(setq rockLineDeep 0)
									(setq rockLineDeep rockDeep)
									(command "SPLine" (strcat (strcat (rtos (+ RockXOffset 15) 2 2) ",") (rtos (* rockDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 13.5) 2 2) ",") (rtos (* (- rockDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 12) 2 2) ",") (rtos (* rockDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 10.5) 2 2) ",") (rtos (* (- rockDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 9) 2 2) ",") (rtos (* rockDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 7.5) 2 2) ",") (rtos (* (- rockDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 6) 2 2) ",") (rtos (* rockDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 4.5) 2 2) ",") (rtos (* (- rockDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 3) 2 2) ",") (rtos (* rockDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 1.5) 2 2) ",") (rtos (* (- rockDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 0) 2 2) ",") (rtos (* rockDeep -5) 2 2)) "" "" "") 
									(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
									(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
									(command "line" ptA ptB "")	
									
									(command "text" "J" "L" (list -224 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
									(command "text" "J" "L" (list -209 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
									(command "text" "J" "L" (list -194 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockNumber 2 0))
									(command "text" "J" "L" (list -184 (+ (* rockDeep -5) 0.5)) rockTextSize 0 rockName)
									(command "mtext" (list -159 (+ (* rockDeep -5) (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" rockDescription "")
								) 
							
								(progn 

									(if (< rockThickness 0.75) 
														(progn
																	(if (< (- rockDeep rockLineDeep) 0.75) 
																			(progn
																					;由于上层岩性改变对下层造成影响时，下层只跟随改变相应的位移量即可
																					(setq rockLineDeep  (+ rockDeep (- 0.75 (- rockDeep rockLineDeep))))
																					(setq pyA (* rockLineDeep -5))
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "line" ptA (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")			
																					(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* rockDeep -5) 2 6)) (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")	
																					
																					(command "text" "J" "L" (list -224 (+ pyA 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
																					(command "text" "J" "L" (list -209 (+ pyA 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
																					(command "text" "J" "L" (list -194 (+ pyA 0.5)) rockTextSize 0 (rtos rockNumber 2 0))
																					(command "text" "J" "L" (list -184 (+ pyA 0.5)) rockTextSize 0 rockName)
																					(command "mtext" (list -159 (+ pyA (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" rockDescription "")
																			)
																			(progn
																			;设计一个标记量来记录当前岩石描述线条深度与实际深度的区别
																					;(setq rockLineDeep (+ (+ rockLineDeep 0.75) rockThickness))
																					(setq rockLineDeep rockDeep)
																					(setq pyA (* rockLineDeep -5))
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "line" ptA (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")			
																					(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* rockDeep -5) 2 6)) (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")		
																					
																					(command "text" "J" "L" (list -224 (+ pyA 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
																					(command "text" "J" "L" (list -209 (+ pyA 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
																					(command "text" "J" "L" (list -194 (+ pyA 0.5)) rockTextSize 0 (rtos rockNumber 2 0))
																					(command "text" "J" "L" (list -184 (+ pyA 0.5)) rockTextSize 0 rockName)
																					(command "mtext" (list -159 (+ pyA (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" rockDescription "")
																		
																			)
																	)
														)
														(progn
																	;当下一层岩性深度小于已调整后的上一层最大显示的文本框的岩性深度，则也对该层进行改变
																	(if (< (- rockDeep rockLineDeep) 0.75) 
																			(progn
																					;由于上层岩性改变对下层造成影响时，下层只跟随改变相应的位移量即可
																					(setq rockLineDeep (+ rockDeep (- 0.75 (- rockDeep rockLineDeep))))
																					(setq pyA (* rockLineDeep -5))
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "line" ptA (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")			
																					(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* rockDeep -5) 2 6)) (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")	
																					
																					(command "text" "J" "L" (list -224 (+ pyA 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
																					(command "text" "J" "L" (list -209 (+ pyA 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
																					(command "text" "J" "L" (list -194 (+ pyA 0.5)) rockTextSize 0 (rtos rockNumber 2 0))
																					(command "text" "J" "L" (list -184 (+ pyA 0.5)) rockTextSize 0 rockName)
																					(command "mtext" (list -159 (+ pyA (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" rockDescription "")
																		
																			)
																			;当下一层岩性深度大于或等于已调整后的上一层最大显示的文本框的岩性深度，则根据当前深度正常计算
																			(progn
																					(setq rockLineDeep rockDeep)
																					;正常情况划线
																					(setq pxA RockXOffset)
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")	
																					(command "text" "J" "L" (list -224 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
																					(command "text" "J" "L" (list -209 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
																					(command "text" "J" "L" (list -194 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockNumber 2 0))
																					(command "text" "J" "L" (list -184 (+ (* rockDeep -5) 0.5)) rockTextSize 0 rockName)
																					(command "mtext" (list -159 (+ (* rockDeep -5) (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" rockDescription "")
								
																			)
																	)	
														)
									)
									;(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
									;(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
									;(command "line" ptA ptB "")	
								)
							)
					)
				) 
			   ((= rockTypeNum 1)
						(progn
							(if (= rockName "表层套管")  
								(progn
									;设计一个标记量来记录当前岩石描述线条深度与实际深度的区别
									(setq rockLineDeep 0)
									(setq rockLineDeep rockDeep)
									(setq pxA (- RockXOffset 15))
									(setq pxB (- RockXOffset 50))
									(command "Line" (strcat (strcat (rtos pxA 2 2) ",") (rtos (* rockDeep -5) 2 2)) (strcat (strcat (rtos RockXOffset 2 2) ",") (rtos (* rockDeep -5) 2 2)) "") 
									(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
									(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
									(command "line" ptA ptB "")	
									(if (> rockThickness 20)
										(progn
												(command "text" "J" "L" (list -289 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
												(command "text" "J" "L" (list -304 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
												(command "text" "J" "C" (list (+ pxA 7.5) (- (* (- rockDeep  rockThickness) -5) 8)) 3 0 "表")
												(command "text" "J" "C" (list (+ pxA 7.5)  (* (- rockDeep (* rockThickness  (/ 2.0 3.0))) -5)) 3 0 "层")
												(command "text" "J" "C" (list (+ pxA 7.5)  (* (- rockDeep (* rockThickness  (/ 1.0 3.0))) -5)) 3 0 "套")
												(command "text" "J" "C" (list (+ pxA 7.5)  (+ (* rockDeep -5) 8)) 3 0 "管")
										)
										(progn
												(command "text" "J" "C" (list (+ pxA 7.5) (* (- rockDeep  (/ rockThickness 2)) -5)) 2 0 "表层套管")

										)
									)
			
								) 
							
								(progn 							
									(setq pxB (- RockXOffset 50))
												;0.75是当文本框文字高度为3时能够完全战士出来的岩石厚度
												(if (< rockThickness 0.75) 
														(progn
																	(if (< (- rockDeep rockLineDeep) 0.75) 
																			(progn
																					;由于上层岩性改变对下层造成影响时，下层只跟随改变相应的位移量即可
																					(setq rockLineDeep  (+ rockDeep (- 0.75 (- rockDeep rockLineDeep))))
																					(setq pxA (- RockXOffset 20))
																					(setq pyA (* rockLineDeep -5))
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "line" ptA (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")			
																					(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* rockDeep -5) 2 6)) (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")	
	
																					(command "text" "J" "L" (list -289 (+ pyA 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
																					(command "text" "J" "L" (list -304 (+ pyA 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
																			)
																			(progn
																			;设计一个标记量来记录当前岩石描述线条深度与实际深度的区别
																					;(setq rockLineDeep (+ (+ rockLineDeep 0.75) rockThickness))
																					(setq rockLineDeep rockDeep)

																					(setq pxA (- RockXOffset 20))
																					(setq pyA (* rockLineDeep -5))
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "line" ptA (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")			
																					(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* rockDeep -5) 2 6)) (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")			
																			
																					(command "text" "J" "L" (list -289 (+ pyA 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
																					(command "text" "J" "L" (list -304 (+ pyA 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
																		
																			)
																	)
														)
														(progn
																	;当下一层岩性深度小于已调整后的上一层最大显示的文本框的岩性深度，则也对该层进行改变
																	(if (< (- rockDeep rockLineDeep) 0.75) 
																			(progn
																					;由于上层岩性改变对下层造成影响时，下层只跟随改变相应的位移量即可
																					(setq rockLineDeep (+ rockDeep (- 0.75 (- rockDeep rockLineDeep))))
																					(setq pxA (- RockXOffset 20))
																					(setq pyA (* rockLineDeep -5))
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "line" ptA (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")			
																					(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* rockDeep -5) 2 6)) (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* rockDeep -5) 2 6)) "")	

																					(command "text" "J" "L" (list -289 (+ pyA 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
																					(command "text" "J" "L" (list -304 (+ pyA 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
																		
																			)
																			;当下一层岩性深度大于或等于已调整后的上一层最大显示的文本框的岩性深度，则根据当前深度正常计算
																			(progn
																					(setq rockLineDeep rockDeep)
																					;正常情况划线
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "text" "J" "L" (list -289 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockDeep 2 2))
																					(command "text" "J" "L" (list -304 (+ (* rockDeep -5) 0.5)) rockTextSize 0 (rtos rockThickness 2 2))
																			)
																	)		
														)
												)

								)
							)
					)
				)
	)
		
)

;测井柱状图
(defun drawWLCDLineAndTextLabel()
	(setq pyA (* wlcdPyA -5))
	(setq RockXOffset -255)
	(setq pxA  RockXOffset)
	(if (= wlcdName "表层套管")  
									(progn
										;设计一个标记量来记录当前岩石描述线条深度与实际深度的区别
										(setq rockLineDeep 0)
										(setq rockLineDeep wlcdRockDeep)
										(setq pxA (- RockXOffset 15))
										(setq pxB (- RockXOffset 50))
										(command "Line" (strcat (strcat (rtos pxA 2 2) ",") (rtos (* wlcdRockDeep -5) 2 2)) (strcat (strcat (rtos RockXOffset 2 2) ",") (rtos (* wlcdRockDeep -5) 2 2)) "") 
										(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
										(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
										(command "line" ptA ptB "")	
										(if (> wlcdThickness 20)
											(progn
													(command "text" "J" "L" (list -289 (+ (* wlcdRockDeep -5) 0.5)) rockTextSize 0 (rtos wlcdRockDeep 2 2))
													(command "text" "J" "L" (list -304 (+ (* wlcdRockDeep -5) 0.5)) rockTextSize 0 (rtos wlcdThickness 2 2))
													(command "text" "J" "C" (list (+ pxA 7.5) (- (* (- wlcdRockDeep  wlcdThickness) -5) 8)) 3 0 "表")
													(command "text" "J" "C" (list (+ pxA 7.5)  (* (- wlcdRockDeep (* wlcdThickness  (/ 2.0 3.0))) -5)) 3 0 "层")
													(command "text" "J" "C" (list (+ pxA 7.5)  (* (- wlcdRockDeep (* wlcdThickness  (/ 1.0 3.0))) -5)) 3 0 "套")
													(command "text" "J" "C" (list (+ pxA 7.5)  (+ (* wlcdRockDeep -5) 8)) 3 0 "管")
											)
											(progn
													(command "text" "J" "C" (list (+ pxA 7.5) (* (- wlcdRockDeep  (/ wlcdThickness 2)) -5)) 2 0 "表层套管")

											)
										)
				
									) 
								
									(progn 							
										(setq pxB (- RockXOffset 50))
													;0.75是当文本框文字高度为3时能够完全战士出来的岩石厚度
													(if (< wlcdThickness 0.75) 
															(progn
																		(if (< (- wlcdRockDeep rockLineDeep) 0.75) 
																				(progn
																						;由于上层岩性改变对下层造成影响时，下层只跟随改变相应的位移量即可
																						(setq rockLineDeep  (+ wlcdRockDeep (- 0.75 (- wlcdRockDeep rockLineDeep))))
																						(setq pxA (- RockXOffset 20))
																						(setq pyA (* rockLineDeep -5))
																						(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																						(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																						(command "line" ptA ptB "")		
																						(command "line" ptA (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* wlcdRockDeep -5) 2 6)) "")			
																						(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* wlcdRockDeep -5) 2 6)) (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* wlcdRockDeep -5) 2 6)) "")	
		
																						(command "text" "J" "L" (list -289 (+ pyA 0.5)) rockTextSize 0 (rtos wlcdRockDeep 2 2))
																						(command "text" "J" "L" (list -304 (+ pyA 0.5)) rockTextSize 0 (rtos wlcdThickness 2 2))
																				)
																				(progn
																				;设计一个标记量来记录当前岩石描述线条深度与实际深度的区别
																						;(setq rockLineDeep (+ (+ rockLineDeep 0.75) wlcdThickness))
																						(setq rockLineDeep wlcdRockDeep)

																						(setq pxA (- RockXOffset 20))
																						(setq pyA (* rockLineDeep -5))
																						(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																						(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																						(command "line" ptA ptB "")		
																						(command "line" ptA (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* wlcdRockDeep -5) 2 6)) "")			
																						(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* wlcdRockDeep -5) 2 6)) (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* wlcdRockDeep -5) 2 6)) "")			
																				
																						(command "text" "J" "L" (list -289 (+ pyA 0.5)) rockTextSize 0 (rtos wlcdRockDeep 2 2))
																						(command "text" "J" "L" (list -304 (+ pyA 0.5)) rockTextSize 0 (rtos wlcdThickness 2 2))
																			
																				)
																		)
															)
															(progn
																		;当下一层岩性深度小于已调整后的上一层最大显示的文本框的岩性深度，则也对该层进行改变
																		(if (< (- wlcdRockDeep rockLineDeep) 0.75) 
																				(progn
																						;由于上层岩性改变对下层造成影响时，下层只跟随改变相应的位移量即可
																						(setq rockLineDeep (+ wlcdRockDeep (- 0.75 (- wlcdRockDeep rockLineDeep))))
																						(setq pxA (- RockXOffset 20))
																						(setq pyA (* rockLineDeep -5))
																						(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																						(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																						(command "line" ptA ptB "")		
																						(command "line" ptA (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* wlcdRockDeep -5) 2 6)) "")			
																						(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* wlcdRockDeep -5) 2 6)) (strcat (strcat (rtos (- RockXOffset 15) 2 6) ",") (rtos (* wlcdRockDeep -5) 2 6)) "")	

																						(command "text" "J" "L" (list -289 (+ pyA 0.5)) rockTextSize 0 (rtos wlcdRockDeep 2 2))
																						(command "text" "J" "L" (list -304 (+ pyA 0.5)) rockTextSize 0 (rtos wlcdThickness 2 2))
																			
																				)
																				;当下一层岩性深度大于或等于已调整后的上一层最大显示的文本框的岩性深度，则根据当前深度正常计算
																				(progn
																						(setq rockLineDeep wlcdRockDeep)
																						;正常情况划线
																						(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																						(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																						(command "line" ptA ptB "")		
																						(command "text" "J" "L" (list -289 (+ (* wlcdRockDeep -5) 0.5)) rockTextSize 0 (rtos wlcdRockDeep 2 2))
																						(command "text" "J" "L" (list -304 (+ (* wlcdRockDeep -5) 0.5)) rockTextSize 0 (rtos wlcdThickness 2 2))
																				)
																		)		
															)
													)

									)
								)
)

;钻井柱状图
(defun drawWDCDLineAndTextLabel()
	(setq pyA (* wdcdPyA -5))
	(setq RockXOffset -245)
	(setq pxA  RockXOffset)
	(setq pxA (+ RockXOffset 20))
							(setq pxB (+ RockXOffset 185))
							(if (= wdcdName "黄土")  
								(progn
									(setq pxA (+ RockXOffset 15))
									(setq rockLineDeep 0)
									(setq rockLineDeep wdcdDeep)
									(command "SPLine" (strcat (strcat (rtos (+ RockXOffset 15) 2 2) ",") (rtos (* wdcdDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 13.5) 2 2) ",") (rtos (* (- wdcdDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 12) 2 2) ",") (rtos (* wdcdDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 10.5) 2 2) ",") (rtos (* (- wdcdDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 9) 2 2) ",") (rtos (* wdcdDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 7.5) 2 2) ",") (rtos (* (- wdcdDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 6) 2 2) ",") (rtos (* wdcdDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 4.5) 2 2) ",") (rtos (* (- wdcdDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 3) 2 2) ",") (rtos (* wdcdDeep -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 1.5) 2 2) ",") (rtos (* (- wdcdDeep 0.2) -5) 2 2)) (strcat (strcat (rtos (+ RockXOffset 0) 2 2) ",") (rtos (* wdcdDeep -5) 2 2)) "" "" "") 
									(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
									(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
									(command "line" ptA ptB "")	
									
									(command "text" "J" "L" (list -224 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 (rtos wdcdDeep 2 2))
									(command "text" "J" "L" (list -209 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 (rtos wdcdThickness 2 2))
									(command "text" "J" "L" (list -194 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 (rtos wdcdNumber 2 0))
									(command "text" "J" "L" (list -184 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 wdcdName)
									(command "mtext" (list -159 (+ (* wdcdDeep -5) (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" wdcdDescription "")
									(setq mtextEName (cdr (car (entget (entlast)))))
									(setq mtextHeight (cdr (assoc 43 (entget (entlast)))))
									(if (> mtextHeight 5)
										(progn
											(command "move" mtextEName "" "D" "0,5.5,0" "")
										)
									)
								)									
							
								(progn 

									(if (< wdcdThickness 2.5) 
														(progn
																	(if (< (- wdcdDeep rockLineDeep) 2.5) 
																			(progn
																					;由于上层岩性改变对下层造成影响时，下层只跟随改变相应的位移量即可
																					(setq rockLineDeep  (+ wdcdDeep (- 2.5 (- wdcdDeep rockLineDeep))))
																					(setq pyA (* rockLineDeep -5))
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "line" ptA (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* wdcdDeep -5) 2 6)) "")			
																					(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* wdcdDeep -5) 2 6)) (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* wdcdDeep -5) 2 6)) "")	
																					
																					(command "text" "J" "L" (list -224 (+ pyA 0.5)) rockTextSize 0 (rtos wdcdDeep 2 2))
																					(command "text" "J" "L" (list -209 (+ pyA 0.5)) rockTextSize 0 (rtos wdcdThickness 2 2))
																					(command "text" "J" "L" (list -194 (+ pyA 0.5)) rockTextSize 0 (rtos wdcdNumber 2 0))
																					(command "text" "J" "L" (list -184 (+ pyA 0.5)) rockTextSize 0 wdcdName)
																					(command "mtext" (list -159 (+ pyA (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" wdcdDescription "")
																					(setq mtextEName (cdr (car (entget (entlast)))))
																					(setq mtextHeight (cdr (assoc 43 (entget (entlast)))))
																					(if (> mtextHeight 5)
																						(progn
																							(command "move" mtextEName "" "D" "0,5.5,0" "")
																						)
																					)
																			)
																			(progn
																			;设计一个标记量来记录当前岩石描述线条深度与实际深度的区别
																					;(setq rockLineDeep (+ (+ rockLineDeep 2.5) wdcdThickness))
																					(setq rockLineDeep wdcdDeep)
																					(setq pyA (* rockLineDeep -5))
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "line" ptA (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* wdcdDeep -5) 2 6)) "")			
																					(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* wdcdDeep -5) 2 6)) (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* wdcdDeep -5) 2 6)) "")		
																					
																					(command "text" "J" "L" (list -224 (+ pyA 0.5)) rockTextSize 0 (rtos wdcdDeep 2 2))
																					(command "text" "J" "L" (list -209 (+ pyA 0.5)) rockTextSize 0 (rtos wdcdThickness 2 2))
																					(command "text" "J" "L" (list -194 (+ pyA 0.5)) rockTextSize 0 (rtos wdcdNumber 2 0))
																					(command "text" "J" "L" (list -184 (+ pyA 0.5)) rockTextSize 0 wdcdName)
																					(command "mtext" (list -159 (+ pyA (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" wdcdDescription "")
																					(setq mtextEName (cdr (car (entget (entlast)))))
																					(setq mtextHeight (cdr (assoc 43 (entget (entlast)))))
																					(if (> mtextHeight 5)
																						(progn
																							(command "move" mtextEName "" "D" "0,5.5,0" "")
																						)
																					)
																			)
																	)
														)
														(progn
																	;当下一层岩性深度小于已调整后的上一层最大显示的文本框的岩性深度，则也对该层进行改变
																	(if (< (- wdcdDeep rockLineDeep) 2.5) 
																			(progn
																					;由于上层岩性改变对下层造成影响时，下层只跟随改变相应的位移量即可
																					(setq rockLineDeep (+ wdcdDeep (- 2.5 (- wdcdDeep rockLineDeep))))
																					(setq pyA (* rockLineDeep -5))
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")		
																					(command "line" ptA (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* wdcdDeep -5) 2 6)) "")			
																					(command "line"  (strcat (strcat (rtos RockXOffset 2 6) ",") (rtos (* wdcdDeep -5) 2 6)) (strcat (strcat (rtos (+ RockXOffset 15) 2 6) ",") (rtos (* wdcdDeep -5) 2 6)) "")	
																					
																					(command "text" "J" "L" (list -224 (+ pyA 0.5)) rockTextSize 0 (rtos wdcdDeep 2 2))
																					(command "text" "J" "L" (list -209 (+ pyA 0.5)) rockTextSize 0 (rtos wdcdThickness 2 2))
																					(command "text" "J" "L" (list -194 (+ pyA 0.5)) rockTextSize 0 (rtos wdcdNumber 2 0))
																					(command "text" "J" "L" (list -184 (+ pyA 0.5)) rockTextSize 0 wdcdName)
																					(command "mtext" (list -159 (+ pyA (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" wdcdDescription "")
																					(setq mtextEName (cdr (car (entget (entlast)))))
																					(setq mtextHeight (cdr (assoc 43 (entget (entlast)))))
																					(if (> mtextHeight 5)
																						(progn
																							(command "move" mtextEName "" "D" "0,5.5,0" "")
																						)
																					)															
																			)
																			;当下一层岩性深度大于或等于已调整后的上一层最大显示的文本框的岩性深度，则根据当前深度正常计算
																			(progn
																					(setq rockLineDeep wdcdDeep)
																					;正常情况划线
																					(setq pxA RockXOffset)
																					(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
																					(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
																					(command "line" ptA ptB "")	
																					(command "text" "J" "L" (list -224 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 (rtos wdcdDeep 2 2))
																					(command "text" "J" "L" (list -209 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 (rtos wdcdThickness 2 2))
																					(command "text" "J" "L" (list -194 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 (rtos wdcdNumber 2 0))
																					(command "text" "J" "L" (list -184 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 wdcdName)
																					(command "mtext" (list -159 (+ (* wdcdDeep -5) (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" wdcdDescription "")
																					(setq mtextEName (cdr (car (entget (entlast)))))
																					(setq mtextHeight (cdr (assoc 43 (entget (entlast)))))
																					(if (> mtextHeight 5)
																						(progn
																							(command "move" mtextEName "" "D" "0,5.5,0" "")
																						)
																					)
								
																			)
																	)	
														)
									)
									;(setq ptA (strcat (strcat (rtos pxA 2 6) ",") (rtos pyA 2 6)))
									;(setq ptB (strcat (strcat (rtos pxB 2 6) ",") (rtos pyA 2 6)))
									;(command "line" ptA ptB "")	
								)
							)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

;井斜方位角
(defun drawDrillDeviationLineAndTextLabel()
	(configNormalLineType)
	(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillDeviationDataOfYRange 'Item drillDeviationIndexA)) 'Value2))
	(setq pyA (* pyA -5))
	(setq pxA  DrillDeviationXOffset)
	;1.2 2.5 3.75
	(command "line" (strcat (strcat (rtos DrillDeviationXOffset 2 2) ",") (rtos pyA 2 2)) (strcat (strcat (rtos (+ DrillDeviationXOffset 25) 2 2) ",") (rtos pyA 2 2)) "")
	(if (null drillDeviationAzimuth)
		(progn
				(command "text" "J" "C" (list (+ pxA 12.5) (+ pyA 2)) 2 0 (strcat (rtos drillDeviationDeviation 2 2) "(°)"))
				(command "text" "J" "C" (list (+ pxA 12.5) (- pyA 4.5)) 2 0 (strcat (rtos drillDeviationDeep 2 2) "(m)"))
		)
		(progn
				(command "text" "J" "C" (list (+ pxA 12.5) (+ pyA 2)) 2 0 (strcat (strcat (rtos drillDeviationDeviation 2 2) "(°) / ") (strcat (rtos drillDeviationAzimuth 2 2) "(°)")))
				(command "text" "J" "C" (list (+ pxA 12.5) (- pyA 4.5)) 2 0 (strcat (rtos drillDeviationDeep 2 2) "(m)"))
		)
	)
)

(defun drawDrillStructureLine()
	;HoleNumFactor
	(setq strBushShoesPtA (strcat "-40," (rtos (* bushShoes -5) 2 2)))
	(setq strBushShoesPtB (strcat "-20," (rtos (* bushShoes -5) 2 2)))
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
							(command "line" (strcat "-43," (rtos (* drillDeepMaxData -5) 2 2)) (strcat "-43," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "line" (strcat "-17," (rtos (* drillDeepMaxData -5) 2 2)) (strcat "-17," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "line" "-49,0" (strcat "-49," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "line" "-11,0" (strcat "-11," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "lweight" 0.50)
							(command "line" "-46,0" (strcat "-46," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "line" "-14,0" (strcat "-14," (rtos (* holeOneDeep -5) 2 2)) "")
							;二开封口线
							(command "lweight" "ByLayer")
							(command "line" (strcat "-49," (rtos (* holeOneDeep -5) 2 2)) (strcat "-43," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "line" (strcat "-17," (rtos (* holeOneDeep -5) 2 2)) (strcat "-11," (rtos (* holeOneDeep -5) 2 2)) "")
					) 
				)
			   ((= HoleNumFactor 2)
					(progn 
							(command "lweight" "ByLayer")
							;一开井深
							(command "line" "-55,0" (strcat "-55," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "line" "-5,0" (strcat "-5," (rtos (* holeOneDeep -5) 2 2)) "")
							;一开套管下深
							(command "lweight" 0.50)
							(command "line" "-52,0" (strcat "-52," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "line" "-8,0" (strcat "-8," (rtos (* holeOneDeep -5) 2 2)) "")
							;二开井深
							(command "lweight" "ByLayer")
							(command "line" (strcat "-49," (rtos (* holeOneDeep -5) 2 2)) (strcat "-49," (rtos (* holeTwoDeep -5) 2 2)) "")
							(command "line" (strcat "-11," (rtos (* holeOneDeep -5) 2 2)) (strcat "-11," (rtos (* holeTwoDeep -5) 2 2)) "")
							(command "line" (strcat "-43," (rtos (* drillDeepMaxData -5) 2 2)) (strcat "-43," (rtos (* holeTwoDeep -5) 2 2)) "")
							(command "line" (strcat "-17," (rtos (* drillDeepMaxData -5) 2 2)) (strcat "-17," (rtos (* holeTwoDeep -5) 2 2)) "")
							;二开套管下深
							(command "lweight" 0.50)
							(command "line" (strcat "-46," (rtos (* holeOneDeep -5) 2 2)) (strcat "-46," (rtos (* holeTwoDeep -5) 2 2)) "")
							(command "line" (strcat "-14," (rtos (* holeOneDeep -5) 2 2)) (strcat "-14," (rtos (* holeTwoDeep -5) 2 2)) "")
							(command "lweight" "ByLayer")
							;二开封口线
							(command "line" (strcat "-55," (rtos (* holeOneDeep -5) 2 2)) (strcat "-49," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "line" (strcat "-5," (rtos (* holeOneDeep -5) 2 2)) (strcat "-11," (rtos (* holeOneDeep -5) 2 2)) "")
							;三开封口线
							(command "line" (strcat "-49," (rtos (* holeOneDeep -5) 2 2)) (strcat "-43," (rtos (* holeOneDeep -5) 2 2)) "")
							(command "line" (strcat "-11," (rtos (* holeOneDeep -5) 2 2)) (strcat "-17," (rtos (* holeOneDeep -5) 2 2)) "")
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
  	(command "mtext" (list -285 27) "h" "2.8" "w" "10" "层" " " "厚" " " "(m)" "")
  	(command "mtext" (list -300 27) "h" "2.8" "w" "10" "深" " " "度" " " "(m)" "")
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
		
	(drawDTTextLabel (caculateMaxNum drillingTimeMaxData))
	(drawWLTextLabel (caculateMaxNum waterLevelMaxData))
	(drawMCTextLabel (caculateMaxNum consumptionMaxData))
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
						(command "text" "J" "L" (list -224 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 (rtos wdcdDeep 2 2))
						(command "text" "J" "L" (list -209 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 (rtos wdcdThickness 2 2))
						(command "text" "J" "L" (list -194 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 (rtos wdcdNumber 2 0))
						(command "text" "J" "L" (list -184 (+ (* wdcdDeep -5) 0.5)) rockTextSize 0 wdcdName)
						(command "mtext" (list -159 (+ (* wdcdDeep -5) (* rockTextSize 1.5))) "h" (rtos rockTextSize 2 2) "w" "98" wdcdDescription "")
					) 
				)
			   ((= rockTypeNum 1)
					(progn 
						(command "text" "J" "L" (list -304 (+ (* wlcdRockDeep -5) 0.5)) rockTextSize 0 (rtos wlcdRockDeep 2 2))
						(command "text" "J" "L" (list -289 (+ (* wlcdRockDeep -5) 0.5)) rockTextSize 0 (rtos wlcdThickness 2 2))
					)
				)
	)
	
)

(defun drawRockSignTextLabel()
	(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockSignDataOfYRange 'Item RSignIndexA)) 'Value2))
	(setq pyA (* pyA -5))
	(setq pxA  RockSignXOffset)
	;1.2 2.5 3.75
	(command "text" "J" "C" (list pxA (* (- rockSignDeep (/ rockSignThickness 2)) -5)) 3 0 rockSignName)
)

(defun drawDrillStructureTextLabel()
	(command "text" "J" "C" (list -30 (+ (* holeArtificialDeep -5) 2)) 3 0 (strcat (rtos  holeArtificialDeep 2 2) "m"))
	(command "text" "J" "C" (list -30 (+ (* holeDamperLocation -5) 2)) 3 0 (strcat (rtos  holeDamperLocation 2 2) "m"))
	(command "text" "J" "C" (list -30 (+ (* bushShoes -5) 2)) 3 0 (strcat (rtos  bushShoes 2 2) "m"))
	(command "text" "J" "C" (list -30 (+ (* drillDeepMaxData -5) 2)) 3 0 (strcat (rtos drillDeepMaxData 2 2) "m"))
)

;UI-Dimlinear
(defun drawDrillStructureDimlinear()
	(cond ((= HoleNumFactor 1)  
					(progn
						(command "lweight" "ByLayer")			
						(command "dimtad" 1) 
						(command "dimasz" 2.5)
						;一开标注
						(command "dimlinear" (strcat "-49," (rtos (/ (* 1 (* holeOneDeep -5)) 3) 2 2)) (strcat "-11," (rtos (/ (* 1 (* holeOneDeep -5)) 3) 2 2)) "T" " " (strcat "-30," (rtos (/ (* 1 (* holeOneDeep -5)) 3) 2 2))) ;
						(command "text" "J" "C" (list -29.5 (+ (/ (* 1 (* holeOneDeep -5)) 3) 1.5)) 2.5 0 holeOneDrillDiameterStr)
						(command "text" "J" "C" (list -29.5 (- (/ (* 1 (* holeOneDeep -5)) 3) 3.5)) 2.5 0 (strcat (rtos holeOneDeep 2 2) "m"))
						(command "dimlinear" (strcat "-46," (rtos (/ (* 2 (* holeOneDeep -5)) 3) 2 2)) (strcat "-14," (rtos (/ (* 2 (* holeOneDeep -5)) 3) 2 2)) "T" " " (strcat "-30," (rtos (/ (* 2 (* holeOneDeep -5)) 3) 2 2))) ;
						(command "text" "J" "C" (list -29.5 (+ (/ (* 2 (* holeOneDeep -5)) 3) 1.5)) 2.5 0 holeOneBushDiameterStr)
						(command "text" "J" "C" (list -29.5 (- (/ (* 2 (* holeOneDeep -5)) 3) 3.5)) 2.5 0  (strcat (rtos holeOneDeep 2 2) "m"))
						;二开标注
						(command "dimlinear" (strcat "-43," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2)) (strcat "-17," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -29.5 (+ (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 1.5)) 2.5 0 holeTwoDrillDiameterStr)
						(command "text" "J" "C" (list -29.5 (- (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 3.5)) 2.5 0 (strcat (rtos holeTwoDeep 2 2) "m"))
						(command "dimlinear" (strcat "-40," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2)) (strcat "-20," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -29.5 (+ (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 3) 1.5) 2.5 0 holeTwoBushDiameterStr)
						(command "text" "J" "C" (list -29.5 (- (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 3) 3.5) 2.5 0 (strcat (rtos (+ holeTwoBushDeep 0.28) 2 2) "m"))
						;二开水泥反深标注
						(if (>= holeTwoCementHign holeOneDeep)
							(progn
								(command "line" (strcat "-43," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-40," (rtos (* holeTwoCementHign -5) 2 2)) "")
								(command "line" (strcat "-20," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-17," (rtos (* holeTwoCementHign -5) 2 2)) "")
							)
							(progn
								(command "line" (strcat "-46," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-40," (rtos (* holeTwoCementHign -5) 2 2)) "")
								(command "line" (strcat "-20," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-14," (rtos (* holeTwoCementHign -5) 2 2)) "")
							)
						)
						(command "text" "J" "C" (list -30 (- (* holeTwoCementHign -5) 1)) 2 0 (strcat (rtos holeTwoCementHign 2 2) "m"))

					 ))
				
			   ((= HoleNumFactor 2)
					(progn 
						(command "lweight" "ByLayer")			
						(command "dimtad" 1) 
						(command "dimasz" 2.5)
						;一开标注
						(command "dimlinear" (strcat "-55," (rtos (/ (* 1 (* holeOneDeep -5)) 3) 2 2)) (strcat "-5," (rtos (/ (* 1 (* holeOneDeep -5)) 3) 2 2)) "T" " " (strcat "-30," (rtos (/ (* 1 (* holeOneDeep -5)) 3) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (/ (* 1 (* holeOneDeep -5)) 3) 1.5)) 2.5 0 holeOneDrillDiameterStr)
						(command "text" "J" "C" (list -30 (- (/ (* 1 (* holeOneDeep -5)) 3) 3.5)) 2.5 0 (strcat (rtos holeOneDeep 2 2) "m"))
						(command "dimlinear" (strcat "-52," (rtos (/ (* 2 (* holeOneDeep -5)) 3) 2 2)) (strcat "-8," (rtos (/ (* 2 (* holeOneDeep -5)) 3) 2 2)) "T" " " (strcat "-30," (rtos (/ (* 2 (* holeOneDeep -5)) 3) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (/ (* 2 (* holeOneDeep -5)) 3) 1.5)) 2.5 0 holeOneBushDiameterStr)
						(command "text" "J" "C" (list -30 (- (/ (* 2 (* holeOneDeep -5)) 3) 3.5)) 2.5 0  (strcat (rtos holeOneDeep 2 2) "m"))
						;二开标注
						(command "dimlinear" (strcat "-49," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2)) (strcat "-11," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 1.5)) 2.5 0 holeTwoDrillDiameterStr)
						(command "text" "J" "C" (list -30 (- (* (+ (/ (- holeTwoDeep holeOneDeep) 3) holeOneDeep) -5) 3.5)) 2.5 0 (strcat (rtos holeTwoDeep 2 2) "m"))
						(command "dimlinear" (strcat "-46," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2)) (strcat "-14," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 1.5)) 2.5 0 holeTwoBushDiameterStr)
						(command "text" "J" "C" (list -30 (- (* (+ (* (/ (- holeTwoDeep holeOneDeep) 3) 2) holeOneDeep) -5) 3.5)) 2.5 0  (strcat (rtos holeTwoDeep 2 2) "m"))
						;二开水泥反深标注
						(command "line" (strcat "-49," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-46," (rtos (* holeTwoCementHign -5) 2 2)) "")
						(command "line" (strcat "-14," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-11," (rtos (* holeTwoCementHign -5) 2 2)) "")
						(command "text" "J" "C" (list -30 (- (* holeTwoCementHign -5) 1)) 2 0 (strcat (rtos holeTwoCementHign 2 2) "m"))
						
						;三开标注
						(command "dimlinear" (strcat "-43," (rtos (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5) 2 2)) (strcat "-17," (rtos (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5) 1.5)) 2.5 0 holeThreeDrillDiameterStr)
						(command "text" "J" "C" (list -30 (- (* (+ (/ (- holeThreeDeep holeTwoDeep) 3) holeTwoDeep) -5)3.5)) 2.5 0 (strcat (rtos holeThreeDeep 2 2) "m"))
						(command "dimlinear" (strcat "-40," (rtos (* (+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 2 2)) (strcat "-20," (rtos (* (+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 2 2)) "T" " " (strcat "-30," (rtos (* (+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 2 2))) ;
						(command "text" "J" "C" (list -30 (+ (*(+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 1.5)) 2.5 0 holeThreeBushDiameterStr)
						(command "text" "J" "C" (list -30 (- (* (+ (* (/ (- holeThreeDeep holeTwoDeep) 3) 2) holeTwoDeep) -5) 3.5)) 2.5 0  (strcat (rtos holeThreeDeep 2 2) "m"))
						;三开水泥反深标注
						(command "line" (strcat "-43," (rtos (* holeThreeCementHign -5) 2 2)) (strcat "-40," (rtos (* holeThreeCementHign -5) 2 2)) "")
						(command "line" (strcat "-17," (rtos (* holeThreeCementHign -5) 2 2)) (strcat "-20," (rtos (* holeThreeCementHign -5) 2 2)) "")
						(command "text" "J" "C" (list -30 (- (* holeThreeCementHign -5) 1)) 2 0 (strcat (rtos holeThreeCementHign 2 2) "m"))
						
					))
	)
)


;UI-Hatch
(defun drawFrameBaseBlock()
	(drawShaZhiNiYan)
	(drawShaLiYan)
	(drawXiLiShaYan)
	(drawFenShaYan)
	(drawZhongLiShaYan)
	(drawCuLiShaYan)
	(drawFenShaZhiNiYan)
	(drawNiZhiFenShaYan)
	(drawNiZhiShaYan)
	(drawShiHuiYan)
	(drawNiZhiHuiYan)
	(drawLvZhiNiYan)
	(drawTanZhiNiYan)
	(drawMeiCeng)
	(drawNiYan)
	(drawMeiCeng)
	(drawJiaoDuBuZhengHe)
	(drawJiaZhengHe)
	(drawHuangTuCeng)
)

(defun hatchRock(rockName rockDeep rockThickness RockHatchXOffset)
	;(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))

	(cond ((> rockThickness 1)  
							(progn
										(setq rockMaxThickness (+ (atoi  (rtos rockThickness 2 2)) 1))
										(setq rockFactor (/ rockThickness rockMaxThickness))
										(cond ((= rockName "黄土")  
															(progn
																(print "黄土")
																(drawHuangTuCengTop)
																(setq Selectionset (ssget "C" (list -394.75 (- (* drillDeepMaxData -5) 5.25)) (list -380.25 (- (* drillDeepMaxData -5) 8.75)))) 
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* drillDeepMaxData -5) 8.5)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (- rockDeep (- rockThickness 0.6))-5)) 1 1 1 0)	
																(drawHuangTuCengBottomLineWithRockThinkness)
															) 
														)
														  ((= rockName "表层套管")
															(progn 
																(print "表层套管")
																
															)
														)  
													   ((= rockName "粉砂岩")
															(progn 
																(print "粉砂岩")
																(setq rockBottomDist (- rockThickness (atoi  (rtos rockThickness 2 2))))
																(setq rockMaxThickness (+ (atoi  (rtos rockThickness 2 2)) 1))
																(setq rockFactor (/ rockThickness rockMaxThickness))
																(drawFenShaYanTop)
																(setq Selectionset (ssget "C" (list -344 (- (* drillDeepMaxData -5) 6.75)) (list -331 (- (* drillDeepMaxData -5) 10.25)))) 
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* drillDeepMaxData -5) 9.75)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset (* (- rockDeep (/ 0.35 2)) -5)) 1 rockFactor rockMaxThickness (* 5 rockFactor))
															)
														)  
														((= rockName "细粒砂岩")
															(progn 
																(print "细粒砂岩")
																(setq rockBottomDist (- rockThickness (atoi  (rtos rockThickness 2 2))))
																(setq rockMaxThickness (+ (atoi  (rtos rockThickness 2 2)) 1))
																(setq rockFactor (/ rockThickness rockMaxThickness))
																(drawXiLiShaYanTop)
																(setq Selectionset (ssget "C" (list -294.5 (- (* drillDeepMaxData -5) 6.75)) (list -280.5 (- (* drillDeepMaxData -5) 10.25)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* drillDeepMaxData -5) 9.75)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset (* (- rockDeep (/ 0.35 2)) -5)) 1 rockFactor rockMaxThickness (* 5 rockFactor))

										
															)
														)  
														((= rockName "中粒砂岩")
															(progn 
																(print "中粒砂岩")
																(setq rockBottomDist (- rockThickness (atoi  (rtos rockThickness 2 2))))
																(setq rockMaxThickness (+ (atoi  (rtos rockThickness 2 2)) 1))
																(setq rockFactor (/ rockThickness rockMaxThickness))
																(drawZhongLiShaYanTop)
																(setq Selectionset (ssget "C" (list -244 (- (* drillDeepMaxData -5) 6.75)) (list -231 (- (* drillDeepMaxData -5) 10.25)))) 
																(creatBlockOrCheckBlock rockName (list -237.5 (- (* drillDeepMaxData -5) 9.75)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset (* (- rockDeep (/ 0.35 2)) -5)) 1 rockFactor rockMaxThickness (* 5 rockFactor))	
															
															)
														)  
														((= rockName "粗粒砂岩")
															(progn 
																(print "粗粒砂岩")
																(setq rockBottomDist (- rockThickness (atoi  (rtos rockThickness 2 2))))
																(setq rockMaxThickness (+ (atoi  (rtos rockThickness 2 2)) 1))
																(setq rockFactor (/ rockThickness rockMaxThickness))
																(drawCuLiShaYanTop)
																(setq Selectionset (ssget "C" (list -194 (- (* drillDeepMaxData -5) 6.75)) (list -181 (- (* drillDeepMaxData -5) 10.25)))) 
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* drillDeepMaxData -5) 9.75)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset (* (- rockDeep (/ 0.35 2)) -5)) 1 rockFactor rockMaxThickness (* 5 rockFactor))	
															)
														)  
														((= rockName "粉砂质泥岩")
															(progn 
																(print "粉砂质泥岩")
																(drawFenShaZhiNiYanTop)
																(setq Selectionset (ssget "C" (list -144 (- (* drillDeepMaxData -5) 5.75)) (list -131 (- (* drillDeepMaxData -5) 10.25))))
																(creatBlockOrCheckBlock rockName (list -137.5 (- (* drillDeepMaxData -5) 10)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* drillDeepMaxData -5) 45))" " " "Selectionset)" " " ))
																(configBoldHatch)
															)
														)  
														((= rockName "泥质粉砂岩")
															(progn 
																(print "泥质粉砂岩")
																(drawNiZhiFenShaYanTop)
																(setq Selectionset (ssget "C" (list -394.75 (- (* drillDeepMaxData -5) 40.75)) (list -380.25 (- (* drillDeepMaxData -5) 45.25))))
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* drillDeepMaxData -5) 45)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* drillDeepMaxData -5) 45))" " " "Selectionset)" " " ))
																(configBoldHatch)
															)
														)  
														((= rockName "砂砾岩")
															(progn 
																(print "砂砾岩");-316,-3984.5
																(drawShaLiYanTop)
																(setq Selectionset (ssget "C" (list -344 (- (* drillDeepMaxData -5) 40.75)) (list -331 (- (* drillDeepMaxData -5) 45.25)))) 
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* drillDeepMaxData -5) 45)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -337.5 (- (* drillDeepMaxData -5) 45))" " " "Selectionset)" " " ))
																(configBoldHatch)
																
															) 
														)					
														((= rockName "泥岩")
															(progn 
																(print "泥岩")
																(drawNiYanTop)
																(setq Selectionset (ssget "C" (list -294.75 (- (* drillDeepMaxData -5) 40.75)) (list -280.25 (- (* drillDeepMaxData -5) 45.25)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* drillDeepMaxData -5) 45)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "煤层");！！！！！！！！！！！！！！！！！！！！！！！！！
															(progn 
																(print "煤层")
																;(drawMeiCeng)
																;(setq Selectionset (ssget "C" (list -244.75 (- (* drillDeepMaxData -5) 40.75)) (list -230.25 (- (* drillDeepMaxData -5) 45.25)))) 
																;(creatBlockOrCheckBlock rockName (list -237.5 (- (* drillDeepMaxData -5) 45)) Selectionset)
																;(hatchRockWithMInsert rockName (list RockHatchXOffset (* rockDeep -5)) 1 rockFactor rockMaxThickness (* 5 rockFactor))

																(command "-hatch" "W" "N" (strcat (strcat (rtos (- RockHatchXOffset 7.5) 2 2) ",") (rtos (* (- rockDeep rockThickness) -5) 2 2))  (strcat (strcat (rtos (+ RockHatchXOffset 7.5) 2 2) ",") (rtos (* (- rockDeep rockThickness) -5) 2 2))  (strcat (strcat (rtos (+ RockHatchXOffset 7.5) 2 2) ",") (rtos (* rockDeep -5) 2 2))   (strcat (strcat (rtos (- RockHatchXOffset 7.5) 2 2) ",") (rtos (* rockDeep -5) 2 2)) "C" "" "P" "SOLID" "CO" "T" "255,255,255" "")
															)
														)  
														((= rockName "炭质泥岩")
															(progn 
																(print "炭质泥岩")
																(drawTanZhiNiYanTop)
																(setq Selectionset (ssget "C" (list -194.75 (- (* drillDeepMaxData -5) 40.75)) (list -180.25 (- (* drillDeepMaxData -5) 45.25))))
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* drillDeepMaxData -5) 45)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "砂质泥岩")
															(progn 
																(print "砂质泥岩")
																(drawShaZhiNiYanTop)
																(setq Selectionset (ssget "C" (list -144 (- (* drillDeepMaxData -5) 40.75)) (list -131 (- (* drillDeepMaxData -5) 45.25))))
																(creatBlockOrCheckBlock rockName (list -137.5 (- (* drillDeepMaxData -5) 45)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* drillDeepMaxData -5) 45))" " " "Selectionset)" " " ))
																(configBoldHatch)
																
															)
														)  
														((= rockName "泥质砂岩")
															(progn 
																(print "泥质砂岩")
																(drawNiZhiShaYanTop)
																(setq Selectionset (ssget "C" (list -394.75 (- (* drillDeepMaxData -5) 75.75)) (list -380.25 (- (* drillDeepMaxData -5) 80.25))))
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* drillDeepMaxData -5) 80)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "石灰岩")
															(progn 
																(print "石灰岩")
																(drawShiHuiYanTop)
																(setq Selectionset (ssget "C" (list -344.75 (- (* drillDeepMaxData -5) 75.75)) (list -330.25 (- (* drillDeepMaxData -5) 80.25))))
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* drillDeepMaxData -5) 80)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "泥质灰岩")
															(progn 
																(print "泥质灰岩")
																(drawNiZhiHuiYanTop)
																(setq Selectionset (ssget "C" (list -294.75 (- (* drillDeepMaxData -5) 75.75)) (list -280.25 (- (* drillDeepMaxData -5) 80.25))))
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* drillDeepMaxData -5) 80)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "铝质泥岩")
															(progn 
																(print "铝质泥岩")
																(drawLvZhiNiYanTop)
																(setq Selectionset (ssget "C" (list -244.75 (- (* drillDeepMaxData -5) 75.75)) (list -230.25 (- (* drillDeepMaxData -5) 80))))
																(creatBlockOrCheckBlock rockName (list -237.5 (- (* drillDeepMaxData -5) 80)) Selectionset)
																(configBoldHatch)
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
										
					
										
							))
							 ((<= rockThickness 1)  
							(progn
										(cond ((= rockName "黄土")  
															(progn
																(print "黄土")
																(drawHuangTuCengTop)
																(setq Selectionset (ssget "C" (list -394.75 (- (* drillDeepMaxData -5) 5.25)) (list -380.25 (- (* drillDeepMaxData -5) 8.75)))) 
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* drillDeepMaxData -5) 8.5)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset  (* rockDeep -5)) 1 (/ (* rockThickness 5) 3) 1 0)
															) 
														)
														 ((= rockName "表层套管")
															(progn 
																(print "表层套管")
																
															)
														)  
													   ((= rockName "粉砂岩")
															(progn 
																(print "粉砂岩")
																(drawFenShaYanOne)
																(setq Selectionset (ssget "C" (list -344 (- (* drillDeepMaxData -5) 6.75)) (list -331 (- (* drillDeepMaxData -5) 7.75)))) 
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* drillDeepMaxData -5) 7.25)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -287.5 (- (* drillDeepMaxData -5) 9.75))" " " "Selectionset)" " " ))
																(configThickHatch)
															)
														)  
														((= rockName "细粒砂岩")
															(progn 
																(print "细粒砂岩")
																(drawXiLiShaYanOne)
																(setq Selectionset (ssget "C" (list -292.5 (- (* drillDeepMaxData -5) 6.75)) (list -282.5 (- (* drillDeepMaxData -5) 7.75)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* drillDeepMaxData -5) 7.25)) Selectionset)
																(configThickHatch)
																
															)
														)  
														((= rockName "中粒砂岩")
															(progn 
																(print "中粒砂岩")															
																(drawZhongLiShaYanOne)
																(setq Selectionset (ssget "C" (list -244 (- (* drillDeepMaxData -5) 6.75)) (list -231 (- (* drillDeepMaxData -5) 7.75)))) 
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* drillDeepMaxData -5) 7.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "粗粒砂岩")
															(progn 
																(print "粗粒砂岩")
																(drawCuLiShaYanOne)
																(setq Selectionset (ssget "C" (list -194 (- (* drillDeepMaxData -5) 6.75)) (list -181 (- (* drillDeepMaxData -5) 7.75)))) 
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* drillDeepMaxData -5) 7.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "粉砂质泥岩")
															(progn 
																(print "粉砂质泥岩")
																(drawFenShaZhiNiYanOne)
																(setq Selectionset (ssget "C" (list -144 (- (* drillDeepMaxData -5) 5.75)) (list -131 (- (* drillDeepMaxData -5) 7)))) 
															;	(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* drillDeepMaxData -5) 45))" " " "Selectionset)" " " ))	
																(creatBlockOrCheckBlock rockName (list -137.5 (- (* drillDeepMaxData -5) 6.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "泥质粉砂岩")
															(progn 
																(print "泥质粉砂岩")
																(drawNiZhiFenShaYanOne)
																(setq Selectionset (ssget "C" (list -394.75 (- (* drillDeepMaxData -5) 40.75)) (list -380.25 (- (* drillDeepMaxData -5) 42)))) 
															;	(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* drillDeepMaxData -5) 45))" " " "Selectionset)" " " ))	
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* drillDeepMaxData -5) 41.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "砂砾岩")
															(progn 
																(print "砂砾岩");-316,-3984.5
																(drawShaLiYanOne)
																(setq Selectionset (ssget "C" (list -344 (- (* drillDeepMaxData -5) 40.75)) (list -331 (- (* drillDeepMaxData -5) 42)))) 
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -337.5 (- (* drillDeepMaxData -5) 45))" " " "Selectionset)" " " ))
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* drillDeepMaxData -5) 41.25)) Selectionset)
																(configThickHatch)
															) 
														)					
														((= rockName "泥岩")
															(progn 
																(print "泥岩")
																(drawNiYanOne)
																(setq Selectionset (ssget "C" (list -294.75 (- (* drillDeepMaxData -5) 40.75)) (list -280.25 (- (* drillDeepMaxData -5) 42)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* drillDeepMaxData -5) 41.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "煤层")
															(progn 
																(print "煤层")
																;(drawMeiCeng)
																;(setq Selectionset (ssget "C" (list -244.75 (- (* drillDeepMaxData -5) 40.75)) (list -230.25 (- (* drillDeepMaxData -5) 42)))) 
																;(creatBlockOrCheckBlock rockName (list -237.5 (- (* drillDeepMaxData -5) 40)) Selectionset)
																;(hatchRockWithMInsert rockName (list RockHatchXOffset  (* rockDeep  -5)) 1 (/ rockThickness 2) 1 0)
																(command "-hatch" "W" "N" (strcat (strcat (rtos (- RockHatchXOffset 7.5) 2 2) ",") (rtos (* (- rockDeep rockThickness) -5) 2 2))  (strcat (strcat (rtos (+ RockHatchXOffset 7.5) 2 2) ",") (rtos (* (- rockDeep rockThickness) -5) 2 2))  (strcat (strcat (rtos (+ RockHatchXOffset 7.5) 2 2) ",") (rtos (* rockDeep -5) 2 2))   (strcat (strcat (rtos (- RockHatchXOffset 7.5) 2 2) ",") (rtos (* rockDeep -5) 2 2)) "C" "" "P" "SOLID" "")
															)
														)  
														((= rockName "炭质泥岩")
															(progn 
																(print "炭质泥岩")
																(drawTanZhiNiYanOne)
																(setq Selectionset (ssget "C" (list -194.75 (- (* drillDeepMaxData -5) 40.75)) (list -180.25 (- (* drillDeepMaxData -5) 42)))) 
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* drillDeepMaxData -5) 41.25)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (- rockDeep (/ rockThickness 2)) -5)) 1 (* rockThickness 2) 1 0)
															)
														)  
														((= rockName "砂质泥岩")
															(progn 
																(print "砂质泥岩")
																(drawShaZhiNiYanOne)
																(setq Selectionset (ssget "C" (list -144 (- (* drillDeepMaxData -5) 40.75)) (list -131 (- (* drillDeepMaxData -5) 42)))) 
																(creatBlockOrCheckBlock rockName (list -137.5 (- (* drillDeepMaxData -5) 41.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "泥质砂岩")
															(progn 
																(print "泥质砂岩")
																(drawNiZhiShaYanOne)
																(setq Selectionset (ssget "C" (list -394.75 (- (* drillDeepMaxData -5) 75.75)) (list -380.25 (- (* drillDeepMaxData -5) 77)))) 
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* drillDeepMaxData -5) 76.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "石灰岩")
															(progn 
																(print "石灰岩3")
																(drawShiHuiYanOne)
																(setq Selectionset (ssget "C" (list -344.75 (- (* drillDeepMaxData -5) 75.75)) (list -330.25 (- (* drillDeepMaxData -5) 77)))) 
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* drillDeepMaxData -5) 76.25)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (- rockDeep (/ rockThickness 2)) -5)) 1 (* rockThickness 2) 1 0)
															)
														)  
														((= rockName "泥质灰岩")
															(progn 
																(print "泥质灰岩")
																(drawNiZhiHuiYanOne)
																(setq Selectionset (ssget "C" (list -294.75 (- (* drillDeepMaxData -5) 75.75)) (list -280.25 (- (* drillDeepMaxData -5) 77)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* drillDeepMaxData -5) 76.25)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (- rockDeep (/ rockThickness 2)) -5)) 1 (* rockThickness 2) 1 0)
															)
														)  
														((= rockName "铝质泥岩")
															(progn 
																(print "铝质泥岩")
																(drawLvZhiNiYanOne)
																(setq Selectionset (ssget "C" (list -244.75 (- (* drillDeepMaxData -5) 75.75)) (list -230.25 (- (* drillDeepMaxData -5) 77)))) 
																(creatBlockOrCheckBlock rockName (list -237.5 (- (* drillDeepMaxData -5) 76.25)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (- rockDeep (/ rockThickness 2)) -5)) 1 (* rockThickness 2) 1 0)
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
							
										
							))
		)
						
	(vla-Update (vlax-ename->vla-object (entlast)))
)


(defun configBoldHatch()
	(hatchRockWithMInsert rockName (list RockHatchXOffset (* rockDeep -5)) 1 rockFactor rockMaxThickness (* 5 rockFactor))
	;(vla-SendCommand thisdrawing (strcat "(hatchRockWithMInsert" " " "rockName" " " "(list RockHatchXOffset  (* (+ rockDeep (/ rockThickness 4)) -5))" " " "1" " " "rockFactor " " " "rockMaxThickness" " " "(* 5 rockFactor))" " "))
)

(defun configThickHatch()
;过薄计算需要找到插图框格的中位线
	
	;(vla-SendCommand thisdrawing (strcat "(hatchRockWithMInsert" " " "rockName" " " "(list RockHatchXOffset (* rockDeep -5))" " " "1" " " "1" " " "1" " " "0)" " "))
	(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (- rockDeep (/ rockThickness 2)) -5)) 1 1 1 0)
)

;Tool-Hatch
(defun hatchRockWithMInsert(BlockName InsertPoint xFactor yFactor rowCount rowSpace)  ;定义函数 
			(vl-load-com)  ;加载Vlisp函数  
			;选择加密对象
 			(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
			(if (= rowCount 1) (command "minsert" BlockCodeName InsertPoint  xFactor  yFactor 0 rowCount 1) (command "minsert" BlockCodeName InsertPoint  xFactor  yFactor 0 rowCount 1 rowSpace))
			(if (= isMutiColor 1) (modifyLastBlockColor BlockName))
			;(command "minsert" BlockName InsertPoint  xFactor  yFactor 0 rowCount 1 rowSpace);用block命令制作块，然后用minsert命令制作多重引用块
		  ;将多用引用块改成匿名多重引用块。匿名块不是无名块，它是以*U命名的，在块 ;编辑框中不现实匿名块的块名，从而无法编辑。
			;(command "minsert"   BlockName BasicPoint 1 1 0 1 1 0 )
			;(vla-put-name (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (vla-get-name (vlax-ename->vla-object (entlast)))) "*U") 
					;将多用引用块改成匿名多重引用块。匿名块不是无名块，它是以*U命名的，在块 ;编辑框中不现实匿名块的块名，从而无法编辑。
		
  			;加密完成后恢复参考图形
) 

(defun creatBlockOrCheckBlock(BlockName BasicPoint Selectionset) 
	(vl-load-com)  ;加载Vlisp函数  
	
	 (if Selectionset  ;选择加密对象 
			(progn    ;以当前时间作为块名  ;因为我们要用多重引用块进行加密，在制作多重引用块之前我们必须将我们需要 ;加密的图元制作成块，为避免与图档内块名冲突，引用了当前时间作为块名 
				(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
				(command "block" BlockCodeName BasicPoint Selectionset "")
				;hahaha

			) 
			 (princ "\n没有选中图元")  
			
	) 
	 (princ)
)

(defun hatchDrillStructure()
	(command "measurement" 1)
	(cond ((= HoleNumFactor 1)  
						(progn
								;一开水泥反深
								(command "-hatch" "W" "N" (strcat "-49," (rtos (* holeOneCementHign -5) 2 2)) (strcat "-46," (rtos (* holeOneCementHign -5) 2 2)) (strcat "-46," (rtos (* holeOneDeep -5) 2 2))  (strcat "-49," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" (strcat "-11," (rtos (* holeOneCementHign -5) 2 2)) (strcat "-14," (rtos (* holeOneCementHign -5) 2 2)) (strcat "-14," (rtos (* holeOneDeep -5) 2 2))  (strcat "-11," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								
								;二开水泥反深
								(command "-hatch" "W" "N" (strcat "-43," (rtos (*  holeTwoCementHign -5) 2 2)) (strcat "-40," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-40," (rtos (* bushShoes -5) 2 2)) (strcat "-20," (rtos  (* bushShoes -5) 2 2)) (strcat "-20," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-17," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-17," (rtos  (* drillDeepMaxData -5) 2 2)) (strcat "-43," (rtos (* drillDeepMaxData -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" strHoleDamperLocationPtC (strcat "-30," (rtos (- (* holeDamperLocation -5) 2) 2 2)) "A" "@5<270" "@5<90" "L" strHoleDamperLocationPtD strBushShoesPtB strBushShoesPtA  "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" strHoleArtificialDeepPtA strHoleArtificialDeepPtB strHoleDamperLocationPtB strHoleDamperLocationPtA "C" "" "P" "DOTS" "1" "0" "")
						
							(if (< holeTwoCementHign holeOneDeep)
								(progn
									(command "-hatch" "W" "N" (strcat "-46," (rtos (*  holeTwoCementHign -5) 2 2)) (strcat "-40," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-40," (rtos (* holeOneDeep -5) 2 2)) (strcat "-20," (rtos  (* holeOneDeep -5) 2 2)) (strcat "-20," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-14," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-14," (rtos  (* holeOneDeep -5) 2 2)) (strcat "-46," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								)
							)
							
						))
				   ((= HoleNumFactor 2)
						(progn 
							
								;一开水泥反深
								(command "-hatch" "W" "N" (strcat "-55," (rtos (* holeOneCementHign -5) 2 2)) (strcat "-52," (rtos (* holeOneCementHign -5) 2 2)) (strcat "-52," (rtos (* holeOneDeep -5) 2 2))  (strcat "-55," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" (strcat "-5," (rtos (* holeOneCementHign -5) 2 2)) (strcat "-8," (rtos (* holeOneCementHign -5) 2 2)) (strcat "-8," (rtos (* holeOneDeep -5) 2 2))  (strcat "-5," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
			
								;二开水泥反深
								(command "-hatch" "W" "N" (strcat "-49," (rtos (* holeTwoDeep -5) 2 2))(strcat "-46," (rtos (*  holeTwoDeep -5) 2 2)) (strcat "-46," (rtos (* holeTwoCementHign -5) 2 2))  (strcat "-49," (rtos (* holeTwoCementHign -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								(command "-hatch" "W" "N" (strcat "-14," (rtos (* holeTwoDeep -5) 2 2))(strcat "-11," (rtos (*  holeTwoDeep -5) 2 2)) (strcat "-11," (rtos (* holeTwoCementHign -5) 2 2))  (strcat "-14," (rtos (* holeTwoCementHign -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								(if (< holeTwoCementHign holeOneDeep)
								(progn
									(command "-hatch" "W" "N" (strcat "-52," (rtos (*  holeTwoCementHign -5) 2 2)) (strcat "-46," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-46," (rtos (* holeOneDeep -5) 2 2)) (strcat "-14," (rtos  (* holeOneDeep -5) 2 2)) (strcat "-14," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-8," (rtos (* holeTwoCementHign -5) 2 2)) (strcat "-8," (rtos  (* holeOneDeep -5) 2 2)) (strcat "-52," (rtos (* holeOneDeep -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
								)
							)
									
								;三开水泥反深
								(cond ((> holeTwoDeep holeThreeCementHign)  
													;水泥反深高于二开孔径
													(progn
														(setq strholeThreeCementHignPlusPtA  (strcat "-46," (rtos (* (- holeTwoDeep holeThreeCementHign) -5) 2 2)))
														(setq strholeThreeCementHignPlusPtB  (strcat "-40," (rtos (* (- holeTwoDeep holeThreeCementHign) -5) 2 2)))
														(setq strholeThreeCementHignPlusPtC  (strcat "-40," (rtos (* holeTwoDeep -5) 2 2)))
														(setq strholeThreeCementHignPlusPtD  (strcat "-46," (rtos (* holeTwoDeep -5) 2 2)))
														(setq strholeThreeCementHignPlusPtE  (strcat "-20," (rtos (* (- holeTwoDeep holeThreeCementHign) -5) 2 2)))
														(setq strholeThreeCementHignPlusPtF  (strcat "-14," (rtos (* (- holeTwoDeep holeThreeCementHign) -5) 2 2)))
														(setq strholeThreeCementHignPlusPtG  (strcat "-14," (rtos (* holeTwoDeep -5) 2 2)))
														(setq strholeThreeCementHignPlusPtH  (strcat "-20," (rtos (* holeTwoDeep -5) 2 2)))
														(command "-hatch" "W" "N" strholeThreeCementHignPlusPtA strholeThreeCementHignPlusPtB strholeThreeCementHignPlusPtC strholeThreeCementHignPlusPtD "C" "" "P" "DOTS" "1" "0" "")
														(command "-hatch" "W" "N" strholeThreeCementHignPlusPtE strholeThreeCementHignPlusPtF strholeThreeCementHignPlusPtG strholeThreeCementHignPlusPtH "C" "" "P" "DOTS" "1" "0" "")
														(command "-hatch" "W" "N" (strcat "-43," (rtos (* holeTwoDeep -5) 2 2)) (strcat "-40," (rtos (* holeTwoDeep -5) 2 2)) (strcat "-40," (rtos (* bushShoes -5) 2 2)) (strcat "-20," (rtos  (* bushShoes -5) 2 2)) (strcat "-20," (rtos (*  holeTwoDeep -5) 2 2)) (strcat "-17," (rtos (* holeTwoDeep -5) 2 2)) (strcat "-17," (rtos  (* drillDeepMaxData -5) 2 2)) (strcat "-43," (rtos (* drillDeepMaxData -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
													))
												
											((<= holeTwoDeep holeThreeCementHign)
													;水泥反深低于二开孔径
													(progn 
															;三开水泥反深
															(command "-hatch" "W" "N" (strcat "-43," (rtos (* holeThreeCementHign -5) 2 2)) (strcat "-40," (rtos (* holeThreeCementHign -5) 2 2)) (strcat "-40," (rtos (* bushShoes -5) 2 2)) (strcat "-20," (rtos  (* bushShoes -5) 2 2)) (strcat "-20," (rtos (* holeThreeCementHign -5) 2 2)) (strcat "-17," (rtos (* holeThreeCementHign -5) 2 2)) (strcat "-17," (rtos  (* drillDeepMaxData -5) 2 2)) (strcat "-43," (rtos (* drillDeepMaxData -5) 2 2)) "C" "" "P" "DOTS" "1" "0" "")
													))
									)
									
									;浮箍浮鞋、、人工井底水泥填充
									(command "-hatch" "W" "N" strHoleDamperLocationPtC (strcat "-30," (rtos (- (* holeDamperLocation -5) 2) 2 2)) "A" "@5<270" "@5<90" "L" strHoleDamperLocationPtD strBushShoesPtB strBushShoesPtA  "C" "" "P" "DOTS" "1" "0" "")
									(command "-hatch" "W" "N" strHoleArtificialDeepPtA strHoleArtificialDeepPtB strHoleDamperLocationPtB strHoleDamperLocationPtA "C" "" "P" "DOTS" "1" "0" "")
						))  
		)

)

;drawBaseDataTable
(defun drawBaseDataTable()
	(command "line" "-360,80" "-30,80" "")
	(command "line" "-360,70" "-275,70" "")
	(command "line" "-360,60" "-275,60" "")
	(command "line" "-360,50" "-30,50" "")
	(command "line" "-360,50" "-360,80" "")
	(command "line" "-340,50" "-340,80" "")
	(command "line" "-275,80" "-275,50" "")
	(command "line" "-265,80" "-265,50" "")
	(command "line" "-265,70" "-30,70" "")
	(command "line" "-265,60" "-30,60" "")
	(command "line" "-215,80" "-215,50" "")
	(command "line" "-185,80" "-185,50" "")
	(command "line" "-90,50" "-90,80" "")
	(command "line" "-60,50" "-60,80" "")
	(command "line" "-30,50" "-30,80" "")
	(command "text" "J" "C" (list -350 73) 3 0 "井    号")
	(command "text" "J" "C" (list -350 63) 3 0 "井    位")
	(command "text" "J" "C" (list -350 53) 3 0 "井    深")
	(command "text" "J" "C" (list -270 73) 3 0 "坐")
	(command "text" "J" "C" (list -270 53) 3 0 "标")
	(command "text" "J" "C" (list -200 73) 3 0 "施 工 单 位")
	(command "text" "J" "C" (list -200 63) 3 0 "开 工 日 期")
	(command "text" "J" "C" (list -200 53) 3 0 "完 井 日 期")
	(command "text" "J" "C" (list -75 73) 3 0 "钻 机 类 型")
	(command "text" "J" "C" (list -75 63) 3 0 "水       泵")
	(command "text" "J" "C" (list -75 53) 3 0 "机  动  力")
	(command "text" "J" "C" (list -195 100) 19 0 (strcat DN "井钻井柱状图"))
	(command "text" "J" "C" (list -307.5 73) 3 0 DN)
	(command "text" "J" "C" (list -307.5 63) 3 0 DL)
	(command "text" "J" "C" (list -307.5 53) 3 0 (rtos drillDeepMaxData 2 2))
	(command "text" "J" "C" (list -240.5 73) 3 0 (strcat "X:  "LX))
	(command "text" "J" "C" (list -240.5 63) 3 0 (strcat "Y:  "LY))
	(command "text" "J" "C" (list -240.5 53) 3 0 (strcat "H:  "LH))
	(command "text" "J" "C" (list -137.5 73) 3 0 Company)
	(command "text" "J" "C" (list -137.5 63) 3 0 StartDay)
	(command "text" "J" "C" (list -137.5 53) 3 0 EndDay)
	(command "text" "J" "C" (list -45 73) 3 0 DrillType)
	(command "text" "J" "C" (list -45 63) 3 0 WaterPump)
	(command "text" "J" "C" (list -45 53) 3 0 MachinePower)


)

(defun drawPictureDataTable()
	(command "line"  (strcat "-75," (rtos (- (* drillDeepMaxData -5) 15) 2 2)) (strcat "-75," (rtos (- (* drillDeepMaxData -5) 95) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* drillDeepMaxData -5) 15) 2 2)) (strcat "55," (rtos (- (* drillDeepMaxData -5) 15) 2 2)) "")
	(command "line"  (strcat "-50," (rtos (- (* drillDeepMaxData -5) 35) 2 2)) (strcat "-50," (rtos (- (* drillDeepMaxData -5) 95) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* drillDeepMaxData -5) 35) 2 2)) (strcat "55," (rtos (- (* drillDeepMaxData -5) 35) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* drillDeepMaxData -5) 47) 2 2)) (strcat "55," (rtos (- (* drillDeepMaxData -5) 47) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* drillDeepMaxData -5) 59) 2 2)) (strcat "55," (rtos (- (* drillDeepMaxData -5) 59) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* drillDeepMaxData -5) 71) 2 2)) (strcat "55," (rtos (- (* drillDeepMaxData -5) 71) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* drillDeepMaxData -5) 83) 2 2)) (strcat "55," (rtos (- (* drillDeepMaxData -5) 83) 2 2)) "")
	(command "line"  (strcat "-20," (rtos (- (* drillDeepMaxData -5) 35) 2 2)) (strcat "-20," (rtos (- (* drillDeepMaxData -5) 95) 2 2)) "")
	(command "line"  (strcat "5," (rtos (- (* drillDeepMaxData -5) 35) 2 2)) (strcat "5," (rtos (- (* drillDeepMaxData -5) 95) 2 2)) "")
	(command "text" "J" "C" (list -62.5 (- (* drillDeepMaxData -5) 43)) 3 0 "制    图")
	(command "text" "J" "C" (list -62.5 (- (* drillDeepMaxData -5) 55)) 3 0 "校    对")
	(command "text" "J" "C" (list -62.5 (- (* drillDeepMaxData -5) 67)) 3 0 "审    核")
	(command "text" "J" "C" (list -62.5 (- (* drillDeepMaxData -5) 79)) 3 0 "机    长")
	(command "text" "J" "C" (list -62.5 (- (* drillDeepMaxData -5) 91)) 3 0 "技术负责")
	(command "text" "J" "C" (list -7.5 (- (* drillDeepMaxData -5) 43)) 3 0 "项目经理")
	(command "text" "J" "C" (list -7.5 (- (* drillDeepMaxData -5) 55)) 3 0 "图    号")
	(command "text" "J" "C" (list -7.5 (- (* drillDeepMaxData -5) 67)) 3 0 "比    例")
	(command "text" "J" "C" (list -7.5 (- (* drillDeepMaxData -5) 79)) 3 0 "制作日期")
	(command "text" "J" "C" (list -7.5 (- (* drillDeepMaxData -5) 91)) 3 0 "资料来源")
	(command "text" "J" "C" (list -7.5 (- (* drillDeepMaxData -5) 22)) 3.5 0 Company)
	(command "text" "J" "C" (list -7.5 (- (* drillDeepMaxData -5) 31)) 5.5 0 (strcat DN "井钻井柱状图"))
	(command "text" "J" "C" (list -35 (- (* drillDeepMaxData -5) 43)) 3 0 PictureMaker)
	(command "text" "J" "C" (list 30 (- (* drillDeepMaxData -5) 43)) 3 0 DrillManager)
	(command "text" "J" "C" (list -35 (- (* drillDeepMaxData -5) 55)) 3 0 PictureProofreader)
	(command "text" "J" "C" (list 30 (- (* drillDeepMaxData -5) 55)) 3 0 PictureNumber)
	(command "text" "J" "C" (list -35 (- (* drillDeepMaxData -5) 67)) 3 0 PictureChecker)
	(command "text" "J" "C" (list 30 (- (* drillDeepMaxData -5) 67)) 3 0 Proportion)
	(command "text" "J" "C" (list -35 (- (* drillDeepMaxData -5) 79)) 3 0 DrillCommander)
	(command "text" "J" "C" (list 30 (- (* drillDeepMaxData -5) 79)) 3 0 PictureTime)
	(command "text" "J" "C" (list -35 (- (* drillDeepMaxData -5) 91)) 3 0 TechnicalLeader)
	(command "mtext" (list 6 (- (* drillDeepMaxData -5) 84)) "h" 3 "w" "48" DataSource "")

)
 
 ;BaseBlock
 
  (defun drawShaLiYanVla()
	(vla-SendCommand thisdrawing (strcat "(drawShaLiYan)" " "))
)
 
(defun drawShaLiYan()
	(drawShaLiYanTop)
  	(drawShaLiYanBottom)
 )


 
 ;砂砾岩
(defun drawShaLiYanOneVla()
	(vla-SendCommand thisdrawing (strcat "(drawShaLiYanOne)" " "))
)

 (defun drawShaLiYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "circle" (strcat "-337.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.35)
	
	(command "circle" (strcat "-340," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-341," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	 (command "-hatch" "S" (ssget "W" (list -341.5 (- (* drillDeepMaxData -5) 40.75)) (list -339.5 (- (* drillDeepMaxData -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -335.5 (- (* drillDeepMaxData -5) 40.75)) (list -333.5 (- (* drillDeepMaxData -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )
 
 (defun drawShaLiYanTopVla()
	(vla-SendCommand thisdrawing (strcat "(drawShaLiYanTop)" " "))
)
 
(defun drawShaLiYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-345," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) (strcat "-330," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) "")
  	
	(command "line" (strcat "-345," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) (strcat "-330," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) "")
	
	;rowOne
	(command "circle" (strcat "-337.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.35)
	
	(command "circle" (strcat "-340," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-341," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	
	(command "-hatch" "S" (ssget "W" (list -341.5 (- (* drillDeepMaxData -5) 40.75)) (list -339.5 (- (* drillDeepMaxData -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -335.5 (- (* drillDeepMaxData -5) 40.75)) (list -333.5 (- (* drillDeepMaxData -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	;rowTwo
	(command "circle" (strcat "-340.5," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.35)
	
	(command "circle" (strcat "-334.5," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.35)
	
	(command "circle" (strcat "-338," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-337," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-332.5," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-331.5," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-342.5," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-343.5," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	

	(command "-hatch" "S" (ssget "W" (list -338.5 (- (* drillDeepMaxData -5) 43.25)) (list -336.5 (- (* drillDeepMaxData -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -333 (- (* drillDeepMaxData -5) 43.25)) (list -331 (- (* drillDeepMaxData -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* drillDeepMaxData -5) 43.25)) (list -342 (- (* drillDeepMaxData -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
)

 (defun drawShaLiYanBottomVla()
	(vla-SendCommand thisdrawing (strcat "(drawShaLiYanBottom)" " "))
)

 (defun drawShaLiYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-345," (rtos (- (* drillDeepMaxData -5) 40) 2 2)) (strcat "-330," (rtos (- (* drillDeepMaxData -5) 50) 2 2)) " ")
	
	(command "line" (strcat "-345," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) (strcat "-330," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) "")
	
	;rowThree
	(command "circle" (strcat "-337.5," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.35)
	
	(command "circle" (strcat "-340," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-341," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	
	
	(command "-hatch" "S" (ssget "W" (list -341.5 (- (* drillDeepMaxData -5) 45.75)) (list -339.5 (- (* drillDeepMaxData -5) 46.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -335.5 (- (* drillDeepMaxData -5) 45.75)) (list -333.5 (- (* drillDeepMaxData -5) 46.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	;rowFour
	(command "circle" (strcat "-340.5," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.35)
	
	(command "circle" (strcat "-334.5," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.35)
	
	(command "circle" (strcat "-338," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-337," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-332.5," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-331.5," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-342.5," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-343.5," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
   	(command "text" "J" "C" (list -337.5 (- (* drillDeepMaxData -5) 56.5)) 3 0 "砂砾岩" "")
   	

	(command "-hatch" "S" (ssget "W" (list -338.5 (- (* drillDeepMaxData -5) 48.25)) (list -336.5 (- (* drillDeepMaxData -5) 49.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -333 (- (* drillDeepMaxData -5) 48.25)) (list -331 (- (* drillDeepMaxData -5) 49.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* drillDeepMaxData -5) 48.25)) (list -342 (- (* drillDeepMaxData -5) 49.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	

 ) 

 
 ;砂质泥岩
 
 (defun drawShaZhiNiYan()
	(drawShaZhiNiYanTop)
  	(drawShaZhiNiYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
 )
 
 (defun drawShaZhiNiYanOne()
	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-141," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-139.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	
	(command "line" (strcat "-135.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-134," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	
	(command "circle" (strcat "-137.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
  	(command "-hatch" "S" (ssget "W" (list -138 (- (* drillDeepMaxData -5) 40.75)) (list -137 (- (* drillDeepMaxData -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )
 
(defun drawShaZhiNiYanTop()
  	;(command "zoom" "w" (strcat "-150," (rtos (- (* drillDeepMaxData -5) 40) 2 2)) (strcat "-125," (rtos (- (* drillDeepMaxData -5) 50) 2 2)) )
	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) "")
  	
	(command "line" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) "")
	
	;rowOne
	(command "line" (strcat "-141," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-139.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	
	(command "line" (strcat "-135.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-134," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	
	(command "circle" (strcat "-137.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -138 (- (* drillDeepMaxData -5) 40.75)) (list -137 (- (* drillDeepMaxData -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")


	

	;rowTwo
	(command "line" (strcat "-142.5," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-144," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")
	(command "line" (strcat "-136.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-138.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")
	(command "line" (strcat "-132.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-131.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")
	
	(command "circle" (strcat "-140.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-134.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -135.5 (- (* drillDeepMaxData -5) 43.25)) (list -133.5 (- (* drillDeepMaxData -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -141.5 (- (* drillDeepMaxData -5) 43.25)) (list -139.5 (- (* drillDeepMaxData -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
)

(defun drawShaZhiNiYanBottom()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 40) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 50) 2 2)) " ")
	(command "line" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) "")
  		
	;rowThree
	(command "line" (strcat "-141," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) (strcat "-139.5," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) "")
	
	(command "line" (strcat "-135.5," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) (strcat "-134," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) "")
	
	(command "circle" (strcat "-137.5," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	
  	(command "-hatch" "S" (ssget "W" (list -138 (- (* drillDeepMaxData -5) 46.75)) (list -137 (- (* drillDeepMaxData -5) 45.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	;rowFour
	(command "line" (strcat "-142.5," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-144," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	(command "line" (strcat "-136.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-138.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	(command "line" (strcat "-132.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-131.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	
	(command "circle" (strcat "-140.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-134.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
	
	(command "-hatch" "S" (ssget "W" (list -135.5 (- (* drillDeepMaxData -5) 49.25)) (list -133.5 (- (* drillDeepMaxData -5) 48.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -141.5 (- (* drillDeepMaxData -5) 49.25)) (list -139.5 (- (* drillDeepMaxData -5) 48.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	
	(command "text" "J" "C" (list -137.5 (- (* drillDeepMaxData -5) 56.5)) 3 0 "砂质泥岩" "")
   	
)


;细粒砂岩

(defun drawXiLiShaYanVla()
	(vla-SendCommand thisdrawing (strcat "(drawXiLiShaYan)" " "))
	

)

 (defun drawXiLiShaYan()
	(drawXiLiShaYanTop)
	(drawXiLiShaYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)
 
 (defun drawXiLiShaYanOneVla()
	(vla-SendCommand thisdrawing (strcat "(drawXiLiShaYanOne)" " "))

)
 
 (defun drawXiLiShaYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "circle" (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-290.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-284.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -292.5 (- (* drillDeepMaxData -5) 7.75)) (list -282.5 (- (* drillDeepMaxData -5) 6.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )
 
  (defun drawXiLiShaYanTopVla()
	(vla-SendCommand thisdrawing (strcat "(drawXiLiShaYanTop)" " "))
	
)
 
(defun drawXiLiShaYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	(command "circle" (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-290.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-284.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	
	;rowTwo
	(command "circle" (strcat "-287.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-286.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-288.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-292.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-293.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-282.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-281.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	

	(command "-hatch" "S" (ssget "W" (list -294.5 (- (* drillDeepMaxData -5) 6.75)) (list -280.5 (- (* drillDeepMaxData -5) 10.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	

)

  (defun drawXiLiShaYanBottomVla()
	(vla-SendCommand thisdrawing (strcat "(drawXiLiShaYanBottom)" " "))
)

 (defun drawXiLiShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-295," (rtos (- (* drillDeepMaxData -5) 5) 2 2)) (strcat "-280," (rtos (- (* drillDeepMaxData -5) 15) 2 2)) " ")
	
	;rowThree
	(command "circle" (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-290.5," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-284.5," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -292.5 (- (* drillDeepMaxData -5) 11.75)) (list -282.5 (- (* drillDeepMaxData -5) 12.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	

	(command "text" "J" "C" (list -287.5 (- (* drillDeepMaxData -5) 21.5)) 3 0 "细粒砂岩" "")
   	
 ) 
 
;粉砂岩
(defun drawFenShaYan()
	(drawFenShaYanTop)
	(drawFenShaYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawFenShaYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	;left
	(command "circle" (strcat "-339.25," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-339.75," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.25," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.75," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	;right
	(command "circle" (strcat "-335.75," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335.25," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.75," (rtos (- (* drillDeepMaxData -5)7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.25," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* drillDeepMaxData -5) 6.75)) (list -331 (- (* drillDeepMaxData -5) 7.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )

 (defun drawFenShaYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	
	;rowOne
	;left
	(command "circle" (strcat "-339.25," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-339.75," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.25," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.75," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	;right
	(command "circle" (strcat "-335.75," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335.25," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.75," (rtos (- (* drillDeepMaxData -5)7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.25," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	;rowTwo
	;mid
	(command "circle" (strcat "-336.75," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-337.25," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-337.75," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-338.25," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	;left
	(command "circle" (strcat "-341.75," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-342.25," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-342.75," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-343.25," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	;right
	(command "circle" (strcat "-332.75," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-332.25," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-331.75," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-331.25," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)	
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* drillDeepMaxData -5) 6.75)) (list -331 (- (* drillDeepMaxData -5) 10.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
)

 (defun drawFenShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-345," (rtos (- (* drillDeepMaxData -5) 5) 2 2)) (strcat "-330," (rtos (- (* drillDeepMaxData -5) 15) 2 2)) " ")
	
	;rowThree
	;left
	(command "circle" (strcat "-339.25," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-339.75," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.25," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.75," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	;right
	(command "circle" (strcat "-335.75," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335.25," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.75," (rtos (- (* drillDeepMaxData -5)12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.25," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* drillDeepMaxData -5) 11.75)) (list -331 (- (* drillDeepMaxData -5) 12.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -337.5 (- (* drillDeepMaxData -5) 21.5)) 3 0 "粉砂岩" "")

 ) 
 
 ;中粒砂岩
(defun drawZhongLiShaYan()
	(drawZhongLiShaYanTop)
	(drawZhongLiShaYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawZhongLiShaYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	;left
	(command "circle" (strcat "-241," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	(command "circle" (strcat "-240," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-235," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-234," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	
	(command "-hatch" "S" (ssget "W" (list -244 (- (* drillDeepMaxData -5) 6.75)) (list -231 (- (* drillDeepMaxData -5) 7.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )

 (defun drawZhongLiShaYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	
	;rowOne
	;left
	(command "circle" (strcat "-241," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	(command "circle" (strcat "-240," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-235," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	(command "circle" (strcat "-234," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	;rowTwo
	;mid
	(command "circle" (strcat "-237," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	(command "circle" (strcat "-238," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	;left
	(command "circle" (strcat "-242," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-243," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-232," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-233," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -244 (- (* drillDeepMaxData -5) 6.75)) (list -231 (- (* drillDeepMaxData -5) 10.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
)

 (defun drawZhongLiShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-245," (rtos (- (* drillDeepMaxData -5) 5) 2 2)) (strcat "-230," (rtos (- (* drillDeepMaxData -5) 15) 2 2)) " ")
	
	;rowThree
	;left
	(command "circle" (strcat "-241," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	(command "circle" (strcat "-240," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-235," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	(command "circle" (strcat "-234," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -244 (- (* drillDeepMaxData -5) 11.75)) (list -231 (- (* drillDeepMaxData -5) 12.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -237.5 (- (* drillDeepMaxData -5) 21.5)) 3 0 "中粒砂岩" "")

 ) 
 
  ;粗粒砂岩
(defun drawCuLiShaYan()
	(drawCuLiShaYanTop)
	(drawCuLiShaYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawCuLiShaYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	;left
	(command "circle" (strcat "-190.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-184.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)

	(command "-hatch" "S" (ssget "W" (list -194 (- (* drillDeepMaxData -5) 6.75)) (list -181 (- (* drillDeepMaxData -5) 7.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )

 (defun drawCuLiShaYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	
	;rowOne
	;left
	(command "circle" (strcat "-190.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-184.5," (rtos (- (* drillDeepMaxData -5) 7.25) 2 2)) 0.10)
	
	;rowTwo
	;mid
	(command "circle" (strcat "-187.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	;left
	(command "circle" (strcat "-192.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-182.5," (rtos (- (* drillDeepMaxData -5) 9.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -194 (- (* drillDeepMaxData -5) 6.75)) (list -181 (- (* drillDeepMaxData -5) 10.25))) "" "P" "SOLID" "CO" "ByLayer" "")
)

 (defun drawCuLiShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-195," (rtos (- (* drillDeepMaxData -5) 5) 2 2)) (strcat "-180," (rtos (- (* drillDeepMaxData -5) 15) 2 2)) " ")
	
	;rowThree
	;left
	(command "circle" (strcat "-190.5," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-184.5," (rtos (- (* drillDeepMaxData -5) 12.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -194 (- (* drillDeepMaxData -5) 11.75)) (list -181 (- (* drillDeepMaxData -5) 12.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -187.5 (- (* drillDeepMaxData -5) 21.5)) 3 0 "粗粒砂岩" "")

 ) 
 
  ;粉砂质泥岩
(defun drawFenShaZhiNiYan()
	(drawFenShaZhiNiYanTop)
	(drawFenShaZhiNiYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawFenShaZhiNiYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	;mid
	(command "circle" (strcat "-138.25," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.75," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.25," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-136.75," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-139.75," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) (strcat "-141.25," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) "")

	;right
	(command "line" (strcat "-135.25," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) (strcat "-133.75," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) "")

	(command "-hatch" "S" (ssget "W" (list -144 (- (* drillDeepMaxData -5) 5.75)) (list -131 (- (* drillDeepMaxData -5) 6.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )

 (defun drawFenShaZhiNiYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 7.5) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 7.5) 2 2)) "")
	(command "line" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 10) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 10) 2 2)) "")

	;rowOne
	;mid
	(command "circle" (strcat "-138.25," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.75," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.25," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-136.75," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-139.75," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) (strcat "-141.25," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) "")

	;right
	(command "line" (strcat "-135.25," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) (strcat "-133.75," (rtos (- (* drillDeepMaxData -5) 6.25) 2 2)) "")
	
	;rowTwo
	;mid
	(command "line" (strcat "-138.25," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) (strcat "-136.75," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-139.75," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-140.25," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-140.75," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-141.25," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) 0.10)
	
	(command "line" (strcat "-142.75," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) (strcat "-144.25," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) "")


	;right
	(command "circle" (strcat "-135.25," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-134.75," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-134.25," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-133.75," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) 0.10)
	
	(command "line" (strcat "-132.25," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) (strcat "-130.75," (rtos (- (* drillDeepMaxData -5) 8.75) 2 2)) "")	
	(command "-hatch" "S" (ssget "W" (list -144 (- (* drillDeepMaxData -5) 5.75)) (list -131 (- (* drillDeepMaxData -5) 9.25))) "" "P" "SOLID" "CO" "ByLayer" "")
)

 (defun drawFenShaZhiNiYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 5) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 15) 2 2)) " ")
	(command "line" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 12.5) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 12.5) 2 2)) "")

	(command "line" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 7.5) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 7.5) 2 2)) "")
	(command "line" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 10) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 10) 2 2)) "")

	;rowThree
	;mid
	(command "circle" (strcat "-138.25," (rtos (- (* drillDeepMaxData -5) 11.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.75," (rtos (- (* drillDeepMaxData -5) 11.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.25," (rtos (- (* drillDeepMaxData -5) 11.25) 2 2)) 0.10)
	(command "circle" (strcat "-136.75," (rtos (- (* drillDeepMaxData -5) 11.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-139.75," (rtos (- (* drillDeepMaxData -5) 11.25) 2 2)) (strcat "-141.25," (rtos (- (* drillDeepMaxData -5) 11.25) 2 2)) "")

	;right
	(command "line" (strcat "-135.25," (rtos (- (* drillDeepMaxData -5) 11.25) 2 2)) (strcat "-133.75," (rtos (- (* drillDeepMaxData -5) 11.25) 2 2)) "")
	
	;rowTwo
	;mid
	(command "line" (strcat "-138.25," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) (strcat "-136.75," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-139.75," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-140.25," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-140.75," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-141.25," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) 0.10)
	
	(command "line" (strcat "-142.75," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) (strcat "-144.25," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) "")


	;right
	(command "circle" (strcat "-135.25," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-134.75," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-134.25," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-133.75," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) 0.10)
	
	(command "line" (strcat "-132.25," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) (strcat "-130.75," (rtos (- (* drillDeepMaxData -5) 13.75) 2 2)) "")	
	(command "-hatch" "S" (ssget "W" (list -144 (- (* drillDeepMaxData -5) 10.75)) (list -131 (- (* drillDeepMaxData -5) 14.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -137.5 (- (* drillDeepMaxData -5) 21.5)) 3 0 "粉砂质泥岩" "")

 ) 
 
  ;泥质粉砂岩
(defun drawNiZhiFenShaYan()
	(drawNiZhiFenShaYanTop)
	(drawNiZhiFenShaYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawNiZhiFenShaYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	;mid
	(command "circle" (strcat "-387.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-388.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-387.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-386.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-389.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-391.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	(command "circle" (strcat "-392.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-394.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)

	;right
	(command "line" (strcat "-385.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-383.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	(command "circle" (strcat "-382.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-380.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)

	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* drillDeepMaxData -5) 40.75)) (list -380.25 (- (* drillDeepMaxData -5) 42))) "" "P" "SOLID" "CO" "ByLayer" "")	
 )

 (defun drawNiZhiFenShaYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-395," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) (strcat "-380," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) "")
	(command "line" (strcat "-395," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) (strcat "-380," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) "")

	;rowOne
	;mid
	(command "circle" (strcat "-387.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-388.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-387.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-386.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-389.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-391.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	(command "circle" (strcat "-392.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-394.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)

	;right
	(command "line" (strcat "-385.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-383.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	(command "circle" (strcat "-382.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.25," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-380.75," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	;rowTwo
	;mid
	(command "line" (strcat "-388.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-386.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-389.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-390.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-390.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-391.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-385.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-384.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-384.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-383.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* drillDeepMaxData -5) 40.75)) (list -380.25 (- (* drillDeepMaxData -5) 45.25))) "" "P" "SOLID" "CO" "ByLayer" "")
)

 (defun drawNiZhiFenShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-395," (rtos (- (* drillDeepMaxData -5) 40) 2 2)) (strcat "-380," (rtos (- (* drillDeepMaxData -5) 50) 2 2)) " ")
	(command "line" (strcat "-395," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) (strcat "-380," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) "")

	;rowThree
	;mid
	(command "circle" (strcat "-387.75," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-388.25," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-387.25," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-386.75," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-389.75," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) (strcat "-391.25," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) "")
	(command "circle" (strcat "-392.75," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.25," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.75," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-394.25," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)

	;right
	(command "line" (strcat "-385.25," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) (strcat "-383.75," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) "")
	(command "circle" (strcat "-382.25," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.75," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.25," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-380.75," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	
	;rowFour
	;mid
	(command "line" (strcat "-388.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-386.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-389.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-390.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-390.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-391.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-385.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-384.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-384.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-383.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* drillDeepMaxData -5) 45.75)) (list -380.25 (- (* drillDeepMaxData -5) 50.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -387.5 (- (* drillDeepMaxData -5) 56.5)) 3 0 "泥质粉砂岩" "") 
 ) 
 
   ;泥质砂岩
(defun drawNiZhiShaYan()
	(drawNiZhiShaYanTop)
	(drawNiZhiShaYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawNiZhiShaYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	;mid
	(command "circle" (strcat "-387.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 0.10)

	;left
	(command "line" (strcat "-389.75," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-391.25," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")

	(command "circle" (strcat "-393.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 0.10)


	;right
	(command "line" (strcat "-385.25," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-383.75," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")
	(command "circle" (strcat "-381.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 0.10)


	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* drillDeepMaxData -5) 75.75)) (list -380.25 (- (* drillDeepMaxData -5) 77))) "" "P" "SOLID" "CO" "ByLayer" "")	
 )

 (defun drawNiZhiShaYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-395," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-380," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-395," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-380," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")

	;rowOne
	;mid
	(command "circle" (strcat "-387.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 0.10)

	;left
	(command "line" (strcat "-389.75," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-391.25," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")

	(command "circle" (strcat "-393.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 0.10)


	;right
	(command "line" (strcat "-385.25," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-383.75," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")
	(command "circle" (strcat "-381.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 0.10)

	
	;rowTwo
	;mid
	(command "line" (strcat "-388.25," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) (strcat "-386.75," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-390.5," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-384.5," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* drillDeepMaxData -5) 75.75)) (list -380.25 (- (* drillDeepMaxData -5) 80.25))) "" "P" "SOLID" "CO" "ByLayer" "")
)

 (defun drawNiZhiShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-395," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-380," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) " ")
	(command "line" (strcat "-395," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-380," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")

	;rowThree
	;mid
	(command "circle" (strcat "-387.5," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) 0.10)

	;left
	(command "line" (strcat "-389.75," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) (strcat "-391.25," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) "")

	(command "circle" (strcat "-393.5," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) 0.10)


	;right
	(command "line" (strcat "-385.25," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) (strcat "-383.75," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) "")
	(command "circle" (strcat "-381.5," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) 0.10)

	
	;rowFour
	;mid
	(command "line" (strcat "-388.25," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) (strcat "-386.75," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-390.5," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-384.5," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* drillDeepMaxData -5) 80.75)) (list -380.25 (- (* drillDeepMaxData -5) 85.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -387.5 (- (* drillDeepMaxData -5) 91.5)) 3 0 "泥质砂岩" "")
 ) 
 
    ;石灰岩
(defun drawShiHuiYan()
	(drawShiHuiYanTop)
	(drawShiHuiYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawShiHuiYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	(command "line" (strcat "-344.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-344.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-340.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-340.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-336.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-336.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-332.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-332.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
 )

 (defun drawShiHuiYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-345," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-330," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-345," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-330," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")

	;rowOne
	(command "line" (strcat "-344.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-344.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-340.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-340.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-336.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-336.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-332.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-332.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")

	;rowTwo
	(command "line" (strcat "-330.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-330.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	(command "line" (strcat "-334.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-334.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	(command "line" (strcat "-338.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-338.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	(command "line" (strcat "-342.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-342.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	
)

 (defun drawShiHuiYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-345," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-330," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) " ")
	(command "line" (strcat "-345," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-330," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")

	;rowThree
	(command "line" (strcat "-344.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-344.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")
	(command "line" (strcat "-340.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-340.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")
	(command "line" (strcat "-336.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-336.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")
	(command "line" (strcat "-332.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-332.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")

	;rowFour
	(command "line" (strcat "-330.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-330.5," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) "")
	(command "line" (strcat "-334.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-334.5," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) "")
	(command "line" (strcat "-338.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-338.5," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) "")
	(command "line" (strcat "-342.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-342.5," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) "")
	
	(command "text" "J" "C" (list -337.5 (- (* drillDeepMaxData -5) 91.5)) 3 0 "石灰岩" "")
 ) 
 
     ;泥质灰岩
(defun drawNiZhiHuiYan()
	(drawNiZhiHuiYanTop)
	(drawNiZhiHuiYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawNiZhiHuiYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	(command "line" (strcat "-287.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-287.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-288.75," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-290.25," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")
	(command "line" (strcat "-286.25," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-284.75," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")
 )

 (defun drawNiZhiHuiYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-295," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-280," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-295," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-280," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")

	;rowOne
	(command "line" (strcat "-287.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-287.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-288.75," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-290.25," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")
	(command "line" (strcat "-286.25," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-284.75," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")


	;rowTwo
	(command "line" (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	(command "line" (strcat "-293.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-293.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	(command "line" (strcat "-281.5," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-281.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	
)

 (defun drawNiZhiHuiYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-295," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-280," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) " ")
	(command "line" (strcat "-295," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-280," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")

	;rowThree
	(command "line" (strcat "-287.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-287.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")
	(command "line" (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-291.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")
	(command "line" (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-283.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")
	(command "line" (strcat "-288.75," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) (strcat "-290.25," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) "")
	(command "line" (strcat "-286.25," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) (strcat "-284.75," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) "")


	;rowFour
	(command "line" (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) "")
	(command "line" (strcat "-293.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-293.5," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) "")
	(command "line" (strcat "-281.5," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-281.5," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) "")
	
	(command "text" "J" "C" (list -287.5 (- (* drillDeepMaxData -5) 91.5)) 3 0 "泥质灰岩" "")
 ) 
 
      ;泥岩
(defun drawNiYan()
	(drawNiYanTop)
	(drawNiYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawNiYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	(command "line" (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-291," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-284," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")

 )

 (defun drawNiYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-295," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) (strcat "-280," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) "")
	(command "line" (strcat "-295," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) (strcat "-280," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) "")

	;rowOne
	(command "line" (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-291," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-284," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")



	;rowTwo
	(command "line" (strcat "-288.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-286.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")
	(command "line" (strcat "-292.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-293.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")
	(command "line" (strcat "-282.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-281.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")

)

 (defun drawNiYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-295," (rtos (- (* drillDeepMaxData -5) 40) 2 2)) (strcat "-280," (rtos (- (* drillDeepMaxData -5) 50) 2 2)) " ")
	(command "line" (strcat "-295," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) (strcat "-280," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) "")

	;rowThree
	(command "line" (strcat "-289.5," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) (strcat "-291," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) (strcat "-284," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) "")

	;rowFour
	(command "line" (strcat "-288.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-286.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	(command "line" (strcat "-292.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-293.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	(command "line" (strcat "-282.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-281.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	
	(command "text" "J" "C" (list -287.5 (- (* drillDeepMaxData -5) 56.5)) 3 0 "泥岩" "")
 ) 
 
       ;铝质泥岩
(defun drawLvZhiNiYan()
	(drawLvZhiNiYanTop)
	(drawLvZhiNiYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

 (defun drawLvZhiNiYanOne()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	(command "circle" (strcat "-237.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 0.30)
	(command "circle" (strcat "-237.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 1.0)

	(command "line" (strcat "-239.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-241," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")
	(command "line" (strcat "-235.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-234," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")

 )

 (defun drawLvZhiNiYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-245," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) (strcat "-230," (rtos (- (* drillDeepMaxData -5) 77.5) 2 2)) "")
	(command "line" (strcat "-245," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-230," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")

	;rowOne
	(command "circle" (strcat "-237.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 0.30)
	(command "circle" (strcat "-237.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) 1.0)

	(command "line" (strcat "-239.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-241," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")
	(command "line" (strcat "-235.5," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) (strcat "-234," (rtos (- (* drillDeepMaxData -5) 76.25) 2 2)) "")



	;rowTwo
	;mid
	(command "line" (strcat "-238.25," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) (strcat "-236.75," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) "")
	;left
	(command "circle" (strcat "-240.25," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) 0.30)
	(command "circle" (strcat "-240.25," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) 1.0)
	(command "line" (strcat "-242.25," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) (strcat "-243.75," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) "")
	;right
	(command "circle" (strcat "-234.75," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) 0.30)
	(command "circle" (strcat "-234.75," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) 1.0)
	(command "line" (strcat "-232.75," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) (strcat "-231.25," (rtos (- (* drillDeepMaxData -5) 78.75) 2 2)) "")
)

 (defun drawLvZhiNiYanBottom()
   	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-245," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-230," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) " ")
	(command "line" (strcat "-245," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) (strcat "-230," (rtos (- (* drillDeepMaxData -5) 82.5) 2 2)) "")

	;rowThree
	(command "circle" (strcat "-237.5," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) 0.30)
	(command "circle" (strcat "-237.5," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) 1.0)

	(command "line" (strcat "-239.5," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) (strcat "-241," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) "")
	(command "line" (strcat "-235.5," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) (strcat "-234," (rtos (- (* drillDeepMaxData -5) 81.25) 2 2)) "")

	;rowFour
	;mid
	(command "line" (strcat "-238.25," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) (strcat "-236.75," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) "")
	;left
	(command "circle" (strcat "-240.25," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) 0.30)
	(command "circle" (strcat "-240.25," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) 1.0)
	(command "line" (strcat "-242.25," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) (strcat "-243.75," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) "")
	;right
	(command "circle" (strcat "-234.75," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) 0.30)
	(command "circle" (strcat "-234.75," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) 1.0)
	(command "line" (strcat "-232.75," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) (strcat "-231.25," (rtos (- (* drillDeepMaxData -5) 83.75) 2 2)) "")
	
	(command "text" "J" "C" (list -237.5 (- (* drillDeepMaxData -5) 91.5)) 3 0 "铝质泥岩" "")
 ) 
 
  ;炭质泥岩
 
 (defun drawTanZhiNiYan()
	(drawTanZhiNiYanTop)
  	(drawTanZhiNiYanBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
 )
 
 (defun drawTanZhiNiYanOne()
	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	;rowOne
	(command "line" (strcat "-191," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-189.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	
	(command "line" (strcat "-185.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-184," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	
	(command "circle" (strcat "-187.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	(command "-hatch" "W" "N" (strcat "-187.9," (rtos (- (* drillDeepMaxData -5) 40.5) 2 2)) (strcat "-186.7," (rtos (- (* drillDeepMaxData -5) 40.5) 2 2)) (strcat "-187.1," (rtos (- (* drillDeepMaxData -5) 42) 2 2))  (strcat "-188.3," (rtos (- (* drillDeepMaxData -5) 42) 2 2)) "C" "" "P" "SOLID" "")
 )
 
(defun drawTanZhiNiYanTop()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "line" (strcat "-195," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) (strcat "-180," (rtos (- (* drillDeepMaxData -5) 42.5) 2 2)) "")
	(command "line" (strcat "-195," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) (strcat "-180," (rtos (- (* drillDeepMaxData -5) 45) 2 2)) "")
	
	;rowOne
	(command "line" (strcat "-191," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-189.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	
	(command "line" (strcat "-185.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) (strcat "-184," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) "")
	
	(command "circle" (strcat "-187.5," (rtos (- (* drillDeepMaxData -5) 41.25) 2 2)) 0.10)
	
	(command "-hatch" "W" "N" (strcat "-187.5," (rtos (- (* drillDeepMaxData -5) 40.5) 2 2)) (strcat "-186.3," (rtos (- (* drillDeepMaxData -5) 40.5) 2 2)) (strcat "-187," (rtos (- (* drillDeepMaxData -5) 42) 2 2))  (strcat "-188.2," (rtos (- (* drillDeepMaxData -5) 42) 2 2)) "C" "" "P" "SOLID" "")

	;rowTwo
	(command "line" (strcat "-192.5," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-194," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")
	(command "line" (strcat "-186.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-188.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")
	(command "line" (strcat "-182.75," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) (strcat "-181.25," (rtos (- (* drillDeepMaxData -5) 43.75) 2 2)) "")

)

(defun drawTanZhiNiYanBottom()
  	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-195," (rtos (- (* drillDeepMaxData -5) 40) 2 2)) (strcat "-180," (rtos (- (* drillDeepMaxData -5) 50) 2 2)) " ")
	(command "line" (strcat "-195," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) (strcat "-180," (rtos (- (* drillDeepMaxData -5) 47.5) 2 2)) "")
	
	;rowThree
	(command "line" (strcat "-191," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) (strcat "-189.5," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) "")
	
	(command "line" (strcat "-185.5," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) (strcat "-184," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) "")
	
	(command "circle" (strcat "-187.5," (rtos (- (* drillDeepMaxData -5) 46.25) 2 2)) 0.10)
	
	(command "-hatch" "W" "N" (strcat "-187.5," (rtos (- (* drillDeepMaxData -5) 45.5) 2 2)) (strcat "-186.3," (rtos (- (* drillDeepMaxData -5) 45.5) 2 2)) (strcat "-187," (rtos (- (* drillDeepMaxData -5) 47) 2 2))  (strcat "-188.2," (rtos (- (* drillDeepMaxData -5) 47) 2 2)) "C" "" "P" "SOLID" "")

	;rowFour
	(command "line" (strcat "-192.5," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-194," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	(command "line" (strcat "-186.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-188.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	(command "line" (strcat "-182.75," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) (strcat "-181.25," (rtos (- (* drillDeepMaxData -5) 48.75) 2 2)) "")
	
	
	(command "text" "J" "C" (list -187.5 (- (* drillDeepMaxData -5) 56.5)) 3 0 "炭质泥岩" "")
   	
)

  ;煤层
 
 (defun drawMeiCeng()
	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "-hatch" "W" "Y" (strcat "-245," (rtos (- (* drillDeepMaxData -5) 40) 2 2)) (strcat "-230," (rtos (- (* drillDeepMaxData -5) 40) 2 2)) (strcat "-230," (rtos (- (* drillDeepMaxData -5) 50) 2 2))  (strcat "-245," (rtos (- (* drillDeepMaxData -5) 50) 2 2)) "C" "" "P" "SOLID" "")
	(command "text" "J" "C" (list -237.5 (- (* drillDeepMaxData -5) 56.5)) 3 0 "煤层" "")
	(vla-Update (vlax-ename->vla-object (entlast)))
 )
 
   ;假整合
(defun drawJiaZhengHe()
	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-195," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-180," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) " ")
	(command "line" (strcat "-195," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-192.85," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	(command "line" (strcat "-190.7," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-188.55," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	(command "line" (strcat "-186.4," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-184.25," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")
	(command "line" (strcat "-182.1," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) (strcat "-180," (rtos (- (* drillDeepMaxData -5) 80) 2 2)) "")

	(command "text" "J" "C" (list -187.5 (- (* drillDeepMaxData -5) 91.5)) 3 0 "假整合" "")
	(vla-Update (vlax-ename->vla-object (entlast)))
)

   ;角度不整合
(defun drawJiaoDuBuZhengHe()
	(command "zoom" "C" (list -222.5 (* drillDeepMaxData -5)) "250")
	(command "Rectang" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 75) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 85) 2 2)) " ")
	(command "SPLine" (strcat "-145," (rtos (- (* drillDeepMaxData -5) 79) 2 2)) (strcat "-143," (rtos (- (* drillDeepMaxData -5) 81) 2 2)) (strcat "-141," (rtos (- (* drillDeepMaxData -5) 79) 2 2)) (strcat "-139," (rtos (- (* drillDeepMaxData -5) 81) 2 2)) (strcat "-137," (rtos (- (* drillDeepMaxData -5) 79) 2 2)) (strcat "-135," (rtos (- (* drillDeepMaxData -5) 81) 2 2)) (strcat "-133," (rtos (- (* drillDeepMaxData -5) 79) 2 2)) (strcat "-130," (rtos (- (* drillDeepMaxData -5) 81) 2 2)) "" "" "")
	(command "text" "J" "C" (list -137.5 (- (* drillDeepMaxData -5) 91.5)) 3 0 "角度不整合" "")
	(vla-Update (vlax-ename->vla-object (entlast)))
)

;黄土层
(defun drawHuangTuCeng()
	(drawHuangTuCengTop)
	(drawHuangTuCengBottom)
	(vla-Update (vlax-ename->vla-object (entlast)))
)

(defun drawHuangTuCengTop()
	(command "line" (strcat "-394.5," (rtos (- (* drillDeepMaxData -5) 6.5) 2 2)) (strcat "-393," (rtos (- (* drillDeepMaxData -5) 5.5) 2 2)) "")
	(command "line" (strcat "-394," (rtos (- (* drillDeepMaxData -5) 7.5) 2 2)) (strcat "-391," (rtos (- (* drillDeepMaxData -5) 5.5) 2 2)) "")
	(command "line" (strcat "-393.5," (rtos (- (* drillDeepMaxData -5) 7.165) 2 2)) "@1.5<300" "")
	(command "line" (strcat "-392.5," (rtos (- (* drillDeepMaxData -5) 6.5) 2 2)) "@1.5<300" "")
	(command "-hatch" "W" "Y" (strcat "-388," (rtos (- (* drillDeepMaxData -5) 5.5) 2 2)) (strcat "-390," (rtos (- (* drillDeepMaxData -5) 5.5) 2 2)) (strcat "-389," (rtos (- (* drillDeepMaxData -5) 8) 2 2)) "C" "" "P" "SOLID" "")
	
	
	(command "line" (strcat "-387," (rtos (- (* drillDeepMaxData -5) 6.5) 2 2)) (strcat "-385.5," (rtos (- (* drillDeepMaxData -5) 5.5) 2 2)) "")
	(command "line" (strcat "-386.5," (rtos (- (* drillDeepMaxData -5) 7.5) 2 2)) (strcat "-383.5," (rtos (- (* drillDeepMaxData -5) 5.5) 2 2)) "")
	(command "line" (strcat "-386," (rtos (- (* drillDeepMaxData -5) 7.165) 2 2)) "@1.5<300" "")
	(command "line" (strcat "-385," (rtos (- (* drillDeepMaxData -5) 6.5) 2 2)) "@1.5<300" "")
	(command "-hatch" "W" "Y" (strcat "-380.5," (rtos (- (* drillDeepMaxData -5) 5.5) 2 2)) (strcat "-382.5," (rtos (- (* drillDeepMaxData -5) 5.5) 2 2)) (strcat "-381.5," (rtos (- (* drillDeepMaxData -5) 8) 2 2)) "C" "" "P" "SOLID" "")
) 

(defun drawHuangTuCengBottom()
	(command "Rectang" (strcat "-395," (rtos (- (* drillDeepMaxData -5) 5) 2 2)) (strcat "-380," (rtos (- (* drillDeepMaxData -5) 15) 2 2)) " ")
	(command "text" "J" "C" (list -387.5 (- (* drillDeepMaxData -5) 21.5)) 3 0 "黄土" "")
	
	(command "line" (strcat "-393.5," (rtos (- (* drillDeepMaxData -5) 9) 2 2)) "@5.5<270" "")
	(command "line" (strcat "-390.5," (rtos (- (* drillDeepMaxData -5) 9) 2 2)) "@5.5<270" "")
	(command "line" (strcat "-387.5," (rtos (- (* drillDeepMaxData -5) 9) 2 2)) "@5.5<270" "")
	(command "line" (strcat "-384.5," (rtos (- (* drillDeepMaxData -5) 9) 2 2)) "@5.5<270" "")
	(command "line" (strcat "-381.5," (rtos (- (* drillDeepMaxData -5) 9) 2 2)) "@5.5<270" "")

)

(defun drawHuangTuCengBottomLineWithRockThinkness()
	(command "line" (strcat (strcat (rtos (- RockHatchXOffset 4.5) 2 2) ",") (rtos (* (- rockDeep (- rockThickness 0.8)) -5) 2 2)) (strcat (strcat "@" (rtos (* (- rockThickness 0.8) 5) 2 2)) "<270") "") 
	(command "line" (strcat (strcat (rtos (- RockHatchXOffset 1.5) 2 2) ",") (rtos (* (- rockDeep (- rockThickness 0.8)) -5) 2 2)) (strcat (strcat "@" (rtos (* (- rockThickness 0.8) 5) 2 2)) "<270") "") 
	(command "line" (strcat (strcat (rtos (+ RockHatchXOffset 1.5) 2 2) ",") (rtos (* (- rockDeep (- rockThickness 0.8)) -5) 2 2)) (strcat (strcat "@" (rtos (* (- rockThickness 0.8) 5) 2 2)) "<270") "") 
	(command "line" (strcat (strcat (rtos (+ RockHatchXOffset 4.5) 2 2) ",") (rtos (* (- rockDeep (- rockThickness 0.8)) -5) 2 2)) (strcat (strcat "@" (rtos (* (- rockThickness 0.8) 5) 2 2)) "<270") "") 
)

;Data
(defun configBaseData()
	(getBaseDataFromExcelFile)
	(configBaseFrameworkData)
	(configPlineData)
	(configRockStrctureData)
	(configColumnarDiagramData)
	(configRockSignData)
	(configDrillStructureData)
	(configDrillDeviationData)
)

(defun configBaseFrameworkData()
	;钻时
	(setq drillingTimeMaxData (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "C2"))
	;消耗量
	(setq consumptionMaxData (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "D2"))
	;水位
	(setq waterLevelMaxData (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "E2"))
	;井深
	(setq drillDeepMaxData (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "F2"))

	(setq DN (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B1"))
	(setq DL (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B2"))
	(setq LX (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B3"))
	(setq LY (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B4"))
	(setq LH (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B5"))
	(setq Company (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B6"))
	(setq StartDay (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B7"))
	(setq EndDay (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B8"))
	(setq DrillType (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B9"))
	(setq WaterPump (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B10"))
	(setq MachinePower (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B11"))
	;
	(setq PictureMaker (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B12"))
	(setq PictureProofreader (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B13"))
	(setq PictureChecker (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B14"))
	(setq DrillCommander (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B15"))
	(setq TechnicalLeader (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B16"))
	(setq DrillManager (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B17"))
	(setq PictureNumber (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B18"))
	(setq Proportion (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B19"))
	(setq PictureTime (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B20"))
	(setq DataSource (getBaseDataCellValueWithSheetNameAndRowName "柱状图基本信息" "B21"))
)

(defun configPlineData()
	(configDrillingTimePlineData)
	(configConsumptionPlineData)
	(configWaterLevelPlineData)
)

(defun configDrillingTimePlineData()
	(setq drillingTimePlineDataRangeOfY (getBaseDataRangeForMaxRowWithSheetNameAndRowName "钻时" "A"))
	(setq drillingTimePlineDataRangeOfX (getBaseDataRangeForMaxRowWithSheetNameAndRowName "钻时" "B"))
	(setq drillingTimePLFactor (caculateFactor 12 (caculateMaxNum drillingTimeMaxData)))
	(setq drillingTimeXOffset -445)
)

(defun configConsumptionPlineData()
	(setq consumptionPlineDataRangeOfY (getBaseDataRangeForMaxRowWithSheetNameAndRowName "消耗量" "A"))
	(setq consumptionPlineDataRangeOfX (getBaseDataRangeForMaxRowWithSheetNameAndRowName "消耗量" "B"))
	(setq consumptionPLFactor (caculateFactor 10 (caculateMaxNum consumptionMaxData)))
	(setq consumptionXOffset -385)
)

(defun configWaterLevelPlineData()
	(setq waterLevelPlineDataRangeOfY (getBaseDataRangeForMaxRowWithSheetNameAndRowName "水位" "A"))
	(setq waterLevelPlineDataRangeOfX (getBaseDataRangeForMaxRowWithSheetNameAndRowName "水位" "B"))
	(setq waterLevelPLFactor (caculateFactor 10 (caculateMaxNum waterLevelMaxData)))
	(setq waterLevelXOffset -385)
)

(defun configRockStrctureData()
	(setq rockStructureTextSize 3)
	(configXiData)
	(configZuData)
	(configTongData)
)

(defun configXiData()
	(setq xiDataOfYRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "系" "A"))
	(setq xiDataThicknessRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "系" "B"))
	(setq xiDataHoleNameRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "系" "C"))
	(setq xiDataSimpleNameRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "系" "D"))
)

(defun configTongData()
	(setq tongDataOfYRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "统" "A"))
	(setq tongDataThicknessRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "统" "B"))
	(setq tongDataHoleNameRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "统" "C"))
	(setq tongDataSimpleNameRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "统" "D"))
)

(defun configZuData()
	(setq zuDataOfYRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "组" "A"))
	(setq zuDataThicknessRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "组" "B"))
	(setq zuDataHoleNameRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "组" "C"))
	(setq zuDataSimpleNameRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "组" "D"))
)

;柱状图数据
(defun configColumnarDiagramData()
	(setq rockTextSize 3)
	(configWellLoggingColumnarDiagramData)
	(configWellDrillingColumnarDiagramData)
)

;测井柱状图数据
(defun configWellLoggingColumnarDiagramData()
	(setq wlcdDataOfYRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "测井柱状图" "A"))
	(setq wlcdDataThicknessRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "测井柱状图" "B"))
	(setq wlcdDataNameRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "测井柱状图" "C"))
	
)

;钻井柱状图数据
(defun configWellDrillingColumnarDiagramData()
	(setq wdcdDataOfYRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "钻井柱状图" "A"))
	(setq wdcdDataThicknessRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "钻井柱状图" "B"))
	(setq wdcdDataNameRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "钻井柱状图" "C"))
	(setq wdcdDataDescription (getBaseDataRangeForMaxRowWithSheetNameAndRowName "钻井柱状图" "D"))
	(setq wdcdDataNumber (getBaseDataRangeForMaxRowWithSheetNameAndRowName "钻井柱状图" "E"))
)

;标志层代号数据
(defun configRockSignData()
	(setq RockSignXOffset -250)
	(setq rockSignDataOfYRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "标志层代号" "A"))
	(setq rockSignDataThicknessRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "标志层代号" "B"))
	(setq rockSignDataNameRange  (getBaseDataRangeForMaxRowWithSheetNameAndRowName "标志层代号" "D"))
)

;完井结构数据
(defun configDrillStructureData()
	(setq holeNum (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B1")) 
	(setq holeOneDrillDiameterStr (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B2"))
	(setq holeOneDeep (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B3"))
	(setq holeOneBushDiameterStr (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B4")) 	
	(setq holeOneBushDeep (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B5") )
	(setq holeOneCementHign  (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B6"))
	(setq holeTwoDrillDiameterStr (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B7"))
	(setq holeTwoDeep (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B8"))
	(setq holeTwoBushDiameterStr (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B9"))
	(setq holeTwoBushDeep (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B10"))
	(setq holeTwoCementHign (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B11"))
	(setq holeArtificialDeep (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "D1"))
	(setq holeDamperLocation (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "D2"))
	(setq bushShoes (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "D3"))
	(setq holeThreeDrillDiameterStr (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B12"))
	(setq holeThreeDeep (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B13"))
	(setq holeThreeBushDiameter (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B14"))
	(setq holeThreeBushDeep (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B15"))
	(setq holeThreeCementHign (getBaseDataCellValueWithSheetNameAndRowName "完井结构" "B16"))
	(cond ((= holeNum 2.0)  
					(progn
					  	(setq HoleNumFactor 1)
					) 
				)
			   ((= holeNum 3.0)
					(progn 
						(setq HoleNumFactor 2)
					)
				)	 
	)
)

(defun configDrillDeviationData()
	(setq DrillDeviationXOffset 0)
	(setq drillDeviationDataOfYRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "井斜方位角" "A"))
	(setq drillDeviationDataDeviationRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "井斜方位角" "B"))
	(setq drillDeviationDataAzimuthRange (getBaseDataRangeForMaxRowWithSheetNameAndRowName "井斜方位角" "C"))
)

;Data-UserClient
(defun checkLoginState()
	(setq loginSuccess 0)
	(loginWithUsernameAndPassword username password)
	(if (= responseStatus 200) 
		(progn
			(setq loginSuccess 1)
			(alert "登录成功！！！外挂即将启动！！！")
			(DCLAndDataHandler)
		)
		(progn
			(setq loginSuccess 0)
			(alert "登录失败，请检查账号密码是否正确！！！如确认无错误，请与开发者联系，邮箱we1019000306@gmail.com")
			(loginStateDataHandler)
		)
	)
)

(defun loginWithUsernameAndPassword(username password / xml)
  (setq xml (vlax-create-object "Microsoft.XMLHTTP"))
  (setq httpURL (strcat (strcat (strcat "https://api2.bmob.cn/1/login?username=" username) "&password=") password))
  (vlax-invoke xml 'open "GET" httpURL 0)
  (vlax-invoke xml 'setRequestHeader "X-Bmob-Application-Id" "c095f89edf7e0b4be4d046a2b758ffc6")
  (vlax-invoke xml 'setRequestHeader "X-Bmob-REST-API-Key" "7bf79c64d21a78cdd102e3766fa2985f")
  (vlax-invoke xml 'send)
  (setq responseStatus (vlax-get xml "status"))
  (vlax-release-object xml)
)

;Tools
(defun getBaseDataFromExcelFile()
	(vl-load-com)
	(setq baseDataExcelFilePathName (getfiled "选择数据源Excel文件" "c:" "" 0))
	(Setq excel (Vlax-Get-Object "Excel.Application"))
	(Vlax-Put-Property excel 'Visible -1)
	(setq wbs (vlax-get-property excel "WorkBooks"))
	(setq wb (vlax-invoke-method wbs "open" baseDataExcelFilePathName))
	(setq shs (vlax-get-property wb "Sheets"))
)

(defun getBaseDataRangeForMaxRowWithSheetNameAndRowName(sheetName rowName)
	(setq sh (vlax-get-property shs 'Item sheetName))
	(setq rg (vlax-get-property sh 'Range "A65536"))
	(setq dataRg (vlax-get-property rg 'end -4162))
	(setq finalRow (vlax-get-property dataRg 'row))
	(setq finalDataRg (vlax-get-property sh 'Range (strcat (strcat rowName "2:") (strcat rowName (itoa finalRow)))))
)

(defun getBaseDataCellValueWithSheetNameAndRowName(sheetName rowName)
	(setq sh (vlax-get-property shs 'Item sheetName))
	(setq rg (vlax-get-property sh 'Range (strcat (strcat rowName ":") rowName)))
	(Vlax-Get (vlax-variant-value (Vlax-Get-Property rg 'Item 1)) 'Value2)
)

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
 
 (defun configNormalLineType()
	(command "LineType" "s" "ByLayer" "")
	(command "lweight" "ByLayer")
 )

 (defun eraseLastEnt()
	(setq entName (cdr (car (entget (entlast)))))
	(command "erase" entName "")
 )
 
 ;单独删除多段线
 (defun eraseLine()
	(command "zoom" "A")
	;erase 床交时能识别的最近距离为5！！！
	(command "erase" "C" (list -440 -5) (list -390 (+ (* drillDeepMaxData -5) 5)) "")
	(command "erase" "C" (list -380 -5) (list -340 (+ (* drillDeepMaxData -5) 5)) "")
 )
 
 ;删除全图
 (defun eraseHolePicture()
	(command "zoom" "A")
	;erase 床交时能识别的最近距离为5！！！
	(command "erase" "C" (list -460 150) (list 70 (- (* drillDeepMaxData -5) 150)) "")
 )
 ;单独删除系统组
 (defun eraseDrillRockStructure()
	(cond ((= rockStructureTypeNum 0)
					(progn
						(eraseXi)
					)
				)
				((= rockStructureTypeNum 1)
					(progn
						(eraseTong)
					)
				)
				((= rockStructureTypeNum 2)
					(progn
						(eraseZu)
					)
				)
	)
 )
 
 (defun eraseXi()
 	(command "zoom" "A")
	(command "erase" "F" (list -330 -1) (list -330 (+ (* drillDeepMaxData -5) 1)) "" "")
	(command "erase" "F" (list -330.25 -1) (list -330.25 (+ (* drillDeepMaxData -5) 1)) "" "")
	(command "erase" "F" (list -329.75 -1) (list -329.75 (+ (* drillDeepMaxData -5) 1)) "" "")
 )
 
  (defun eraseTong()
  	(command "zoom" "A")
	(command "erase" "F" (list -320 -1) (list -320 (+ (* drillDeepMaxData -5) 1)) "" "")
	(command "erase" "F" (list -320.25 -1) (list -320.25 (+ (* drillDeepMaxData -5) 1)) "" "")
	(command "erase" "F" (list -319.75 -1) (list -319.75 (+ (* drillDeepMaxData -5) 1)) "" "")

 )
 
 (defun eraseZu()
 	(command "zoom" "A")
	(command "erase" "F" (list -310 -1) (list -310 (+ (* drillDeepMaxData -5) 1)) "" "")
	(command "erase" "F" (list -310.25 -1) (list -310.25 (+ (* drillDeepMaxData -5) 1)) "" "")
	(command "erase" "F" (list -309.75 -1) (list -309.75 (+ (* drillDeepMaxData -5) 1)) "" "")
 )
 ;单独删除柱状图
 (defun eraseWellColumnarDiagram()
	(eraseWellDrillingColumnarDiagram) 
	(eraseWellLoggingColumnarDiagram)
 )
 
  ;删除已画钻井柱状图
 (defun eraseWellDrillingColumnarDiagram()
	(command "zoom" "A")
	;erase 床交时能识别的最近距离为5！！！
	(command "erase" "C" (list -240 -5) (list -235 (+ (* drillDeepMaxData -5) 5)) "")
	(command "erase" "C" (list -220 -5) (list -215 (+ (* drillDeepMaxData -5) 5)) "")
	(command "erase" "C" (list -205 -5) (list -200 (+ (* drillDeepMaxData -5) 5)) "")
	(command "erase" "F" (list -193.5 -5) (list -193.5 (+ (* drillDeepMaxData -5) 1)) "" "")
	(command "erase" "C" (list -180 -5) (list -165 (+ (* drillDeepMaxData -5) 5)) "")
	(command "erase" "C" (list -155 -5) (list -65 (+ (* drillDeepMaxData -5) 5)) "")
	;(command "erase" "F" (list -210 (- (* drillDeepMaxData -5) 1)) (list -193.5 (+ (* drillDeepMaxData -5) 1)) "" "")

 )
 
 ;删除已画测井柱状图
 (defun eraseWellLoggingColumnarDiagram()
	(command "zoom" "A")
	(command "erase" "C" (list -300 -5) (list -295 (+ (* drillDeepMaxData -5) 5)) "")
	(command "erase" "C" (list -285 -5) (list -280 (+ (* drillDeepMaxData -5) 5)) "")
	(command "erase" "F" (list -272.5 -5) (list -272.5 (+ (* drillDeepMaxData -5) 5)) "" "")
	(command "erase" "C" (list -265 -5) (list -261 (+ (* drillDeepMaxData -5) 5)) "")
 )
 
 (defun eraseRockSign()
	(command "zoom" "A")
	(command "erase" "F" (list -250 -5) (list -250 (+ (* drillDeepMaxData -5) 5)) "" "")
	(command "erase" "F" (list -250.25 -5) (list -250.25 (+ (* drillDeepMaxData -5) 5)) "" "")
	(command "erase" "F" (list -249.75 -5) (list -249.75 (+ (* drillDeepMaxData -5) 5)) "" "")
 )
 
 ;删除已画完井结构
 (defun EraseDrillCompletionStructure()
	(command "zoom" "A")
	(command "erase" "C" (list -55 -5) (list -5 (+ (* drillDeepMaxData -5) 5)) "")
 )
 
 ;删除已画井斜方位角
 (defun eraseEraseDrillDeviation()
 	(command "zoom" "A")
	(command "erase" "C" (list 5 -5) (list 20 (+ (* drillDeepMaxData -5) 5)) "")
 )
 
 ;更改各种块的颜色 
 (defun c:tt()
	(setq ent (car (entsel)))
	(setq blk_name (cdr (assoc 2 (entget ent))))
	(blk_modifyColor blk_name 200)
	(entupd ent)
 )
 
 (defun modifyLastBlockColor(blkName)
 	(setq ent (cdr (car (entget (entlast)))))
	(setq blk_name (cdr (assoc 2 (entget (entlast)))))
	
	(cond ((= blkName "黄土")  
				(progn
					;土黄色
					(blk_modifyColor blk_name 40)
				)
			   )
			((= blkName "表层套管")
				(progn 
					(print "表层套管")
				)
			)  
		   ((= blkName "粉砂岩")
				(progn 
					;浅灰色
					(blk_modifyColor blk_name 10)					
				)
			)  
			((= blkName "细粒砂岩")
				(progn 
					;浅灰色
					(blk_modifyColor blk_name 30)					
				)
			)  
			((= blkName "中粒砂岩")
				(progn 
					;灰白色
					(blk_modifyColor blk_name 210)					
				)
			)  
			((= blkName "粗粒砂岩")
				(progn 
					;灰色
					(blk_modifyColor blk_name 241)					
				)
			)  
			((= blkName "粉砂质泥岩")
				(progn 
					;?????????????
					(blk_modifyColor blk_name 34)
				)
			)  
			((= blkName "泥质粉砂岩")
				(progn 
					;???????????
					(blk_modifyColor blk_name 34)

				)
			)  
			((= blkName "砂砾岩")
				(progn 
					;浅棕红
					(blk_modifyColor blk_name 242)					
					
				) 
			)					
			((= blkName "泥岩")
				(progn 
					;灰黑色
					(blk_modifyColor blk_name 251)					
				)
			)  
			((= blkName "煤层")
				(progn 
					;黑色
					(blk_modifyColor blk_name 250)					
				)
			)  
			((= blkName "炭质泥岩")
				(progn 
					;浅黑色
					(blk_modifyColor blk_name 48)
				)
			)  
			((= blkName "砂质泥岩")
				(progn 
					;灰绿色
					(blk_modifyColor blk_name 120)
				)
			)  
			((= blkName "泥质砂岩")
				(progn 
					;浅棕红色
					(blk_modifyColor blk_name 34)

				)
			)  
			((= blkName "石灰岩")
				(progn 
					;灰色
					(blk_modifyColor blk_name 252)
				)
			)  
			((= blkName "泥质灰岩")
				(progn 
					;棕红色
					(blk_modifyColor blk_name 24)
				)
			)  
			((= blkName "铝质泥岩")
				(progn 
					;灰黑色
					(blk_modifyColor blk_name 251)			
				)
			)  
			((= blkName "假整合")
				(progn 

				)
			)  
			((= blkName "角度不整合")
				(progn 

				)
			)  
	)
	
	(entupd ent)
 )

 (defun blk_modifyColor(blk_name colorNum)

	(setq blk (Vlax-Invoke-Method (Vlax-Get (Vlax-Get (Vlax-Get-Acad-Object) 'ActiveDocument) 'Blocks) 'Item blk_name))
	(vlax-for item blk 
		(setq typ (Vlax-Get item 'ObjectName))
		(cond
			((= typ "AcDbBlockReference")
				;找到了子块
				(blk_modifyColor (Vlax-Get item 'Name) colorNum)
			)
			((= typ "AcDbHatch")
				;找到了子块
				(Vlax-Put-Property item 'Color colorNum)
			)
			(1
				(Vlax-Put-Property item 'Color colorNum)
			)
		)
	)
	
 )
 
 
 
 
 
 ;---------------------------------------测试多线程--------------------------------------------------
 
(defun Loop nil nil)
(defun Loop_stop nil nil)
;nonsmall 作品: Visual DCL 对话框自动连接程序
;本代码全部由Visual DCL程序自动生成

(defun Form1_load()
	(vl-load-com)
	(Loop_init)

	(princ);防止函数回显
)
(defun Loop_init()	
	(setq ob_shell (vlax-create-object "WScript.Shell"))
	(if Vbs_File (vl-file-delete Vbs_File))
	(setq Vbs_File (vl-filename-mktemp nil nil ".Vbs"))
	(setq fresh_time "200");每200毫秒刷新一次
	(write-line
		(strcat "On Error Resume Next\nSet nonsmall = GetObject(, \"AutoCAD.Application\").GetInterfaceObject(\"VL.Application.16\").ActiveDocument.Functions.Item(\"Loop\")\nDo\nWScript.Sleep " fresh_time "\nnonsmall.Funcall\nLoop")
		(setq f (open Vbs_File "w"))
	)
	(close f)
	(vlax-invoke-method ob_shell 'run Vbs_File 0)
)

(defun kill_wscript( / )  
	(setq WMI (vlax-create-object "WbemScripting.SWbemLocator") 
	con (vlax-invoke WMI 'ConnectServer nil nil nil nil nil nil nil nil) 
	Win32_Process (vlax-invoke con 'ExecQuery "Select * from Win32_Process"))
	(vlax-for item Win32_Process 
		(if (= "wscript.exe" (vlax-get item 'name))
   	     	(vlax-invoke item 'terminate)
    	) 
  	)
	(vlax-release-object WMI )
	(vl-file-delete Vbs_File)
)

(defun Loop_Start()
	
	(if (null wdcdDeep) 
			(progn
						(print "11111111111111")

			)
			(progn
						(print "2222222222222222")
						(print (rtos wdcdDeep 2 2))
						;(command "zoom" "C" (list -222.5 (* wdcdDeep -5)) "250")		
			)
		)

)

(defun exam()
	(defun dynamicZooming()
		;(vla-regen (vla-get-ActiveDocument (VLAX-GET-ACAD-OBJECT)) acAllViewports)
		;(vla-Update (vlax-ename->vla-object (entlast)))
		(Vlax-Invoke-Method (Vlax-Get-Acad-Object) 'ZoomCenter (Vlax-3d-Point (list -222.5 (* wdcdDeep -5) 0)) 150)
	)
	(vlax-add-cmd "exam" 'dynamicZooming "examX" acrx_cmd_transparent)
	(princ)
)

(defun removeExam()
  	;(print "我停下来执行zoom了！！！！！！！！！！！！！！！")
		;(Vlax-Invoke-Method (Vlax-Get-Acad-Object) 'getString "hahahaha")
		;(getstring "hahaha")
		;WHIPTHREAD
	(vlax-remove-cmd "exam")
)
