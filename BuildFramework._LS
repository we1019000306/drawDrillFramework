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
  	(setq id(load_dialog "G:\\工程二处\\drawDrillFramework\\BuildFrameWorkDCL.DCL"));BuildFrameWorkDCL testDCL Login
	(if (not (new_dialog "framework" id)) (exit))
	;配置启动按钮
  	(action_tile "startBtn" "(configStartBtn) (done_dialog 1)")
	;配置基础数据按钮
	(action_tile "baseDataBtn" "(configbaseDataBtn) (done_dialog 2)")
	;配置框架按钮
	(action_tile "DFBtn" "(configDFBtn)  (done_dialog 3)")
	;配置曲线按钮
	(action_tile "DLBtn" "(configDLBtn)  (done_dialog 4)")
	;
	(action_tile "DRSBtn" "(configDRSBtn)  (done_dialog 5)")
	;
	(action_tile "DRBtn" "(configDRBtn)  (done_dialog 6)")
	;
	(action_tile "DRSignBtn" "(configDRSignBtn)  (done_dialog 7)")
	;
	(action_tile "DDDBtn" "(configDDDBtn)  (done_dialog 8)")
	;配置完井结构按钮
	(action_tile "DDCSBtn" "(configDDBtn)  (done_dialog 9)")
	;配置单独清除已绘模型按钮群
	(action_tile "ELBtn" "(done_dialog 11)")
	(action_tile "ERSBtn" "(done_dialog 12)")
	(action_tile "ECDBtn" "(done_dialog 13)")
	(action_tile "ERSignBtn" "(done_dialog 14)")
	(action_tile "EDCSBtn" "(done_dialog 15)")
	(action_tile "EDDBtn" "(done_dialog 16)")
	
	(action_tile "LineName_List" "(configLineNamePopListAction)")
	(action_tile "RockStructureType_List" "(configRockStructurePopListAction)")

	(action_tile "HoleNumber_List" "(configHolePopListAction)")
	;
	(action_tile "RockType_List" "(configRockPopListAction)")
	;
	(action_tile "cancelBtn" "(done_dialog -1)")
	


	(if (= initSuccessValue 1)  
			(progn
				(mode_tile  "startBtn" 1)
				(mode_tile  "baseDataBtn" 0)
				(mode_tile  "DFBtn" 0)
				(mode_tile  "DLBtn" 0)
				(mode_tile  "DDCSBtn" 0)
				(mode_tile  "DRBtn" 0)
				(mode_tile  "DRSBtn" 0)
				(mode_tile  "DRSignBtn" 0)
				(mode_tile  "DDDBtn" 0)
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
				(mode_tile  "DFBtn" 1)
				(mode_tile  "DLBtn" 1)
				(mode_tile  "DDCSBtn" 1)
				(mode_tile  "DRBtn" 1)
				(mode_tile  "DRSBtn" 1)
				(mode_tile  "DRSignBtn" 1)
				(mode_tile  "DDDBtn" 1)
				(mode_tile  "ELBtn" 1)
				(mode_tile  "ERSBtn" 1)
				(mode_tile  "ECDBtn" 1)
				(mode_tile  "ERSignBtn" 1)
				(mode_tile  "EDCSBtn" 1)
				(mode_tile  "EDDBtn" 1)

			)
	)
	;存储上次输入记录
  	(storageLastDCLValue)
 	(setq std (start_dialog))
  	(unload_dialog id)
	(cond 
					((= std 1)
						(progn
							(alert "辅助程序启动完毕，请开始绘图！！")
							(configInitValue)
						  	(c:buildFramework)
						)
					)
					((= std 2)
						(progn
							(alert "基础数据设置或更新完毕，可以进行绘图操作啦！！")
						  	(c:buildFramework)
						)
					)
					((= std 3)
						(progn
							(drawFrame)
							(command "zoom" "C" (list -222.5 (* d -5)) "250")
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
							(command "")
							(command "zoom" "C" (list -222.5 (* d -5)) "250")
							(drawRock)
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
							(drawDrillDeviation)
							(c:buildFramework)
						)
					)
					((= std 9)
						(progn		
							(drawDrillStructure)
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

;记忆初次输入的DCL的数据
(defun storageLastDCLValue()
	;架构绘制
  	(if (null d) (print "nil") (set_tile "D" (rtos d 2 2)))
	(if (null dt) (print "nil") (set_tile "DT" (rtos dt 2 2)))
  	(if (null mc) (print "nil") (set_tile "MC" (rtos mc 2 2)))
  	(if (null wl) (print "nil") (set_tile "WL" (rtos wl 2 2)))
	;钻井基础信息
	(if (null DN) (print "nil") (set_tile "DN" DN))
	(if (null DL) (print "nil") (set_tile "DL" DL))
	(if (null LX) (print "nil") (set_tile "LX" LX))
	(if (null LY) (print "nil") (set_tile "LY" LY))
	(if (null LH) (print "nil") (set_tile "LH" LH))
	(if (null Company) (print "nil") (set_tile "Company" Company))
	(if (null StartDay) (print "nil") (set_tile "StartDay" StartDay))
	(if (null EndDay) (print "nil") (set_tile "EndDay" EndDay))
	(if (null DrillType) (print "nil") (set_tile "DrillType" DrillType))
	(if (null WaterPump) (print "nil") (set_tile "WaterPump" WaterPump))
	(if (null MachinePower) (print "nil") (set_tile "MachinePower" MachinePower))

	;绘图基础信息
	(if (null PictureMaker) (print "nil") (set_tile "PictureMaker" PictureMaker))
	(if (null PictureProofreader) (print "nil") (set_tile "PictureProofreader" PictureProofreader))
	(if (null PictureChecker) (print "nil") (set_tile "PictureChecker" PictureChecker))
	(if (null DrillCommander) (print "nil") (set_tile "DrillCommander" DrillCommander))
	(if (null TechnicalLeader) (print "nil") (set_tile "TechnicalLeader" TechnicalLeader))
	(if (null DrillManager) (print "nil") (set_tile "DrillManager" DrillManager))
	(if (null PictureNumber) (print "nil") (set_tile "PictureNumber" PictureNumber))
	(if (null Proportion) (print "nil") (set_tile "Proportion" Proportion))
	(if (null PictureTime) (print "nil") (set_tile "PictureTime" PictureTime))
	(if (null DataSource) (print "nil") (set_tile "DataSource" DataSource))

	;曲线绘制
	(if (null columnPLineOne) (print "nil") (set_tile "columnPLineOne" columnPLineOne))
  	(if (null columnPLineTwo) (print "nil") (set_tile "columnPLineTwo" columnPLineTwo))
  	(if (null lineNameNum) (setq lineNameNum 0) (set_tile "LineName_List" (itoa lineNameNum)))
  	;(if (null lineTypeNum) (print "nil") (set_tile "LineType_List" (itoa lineTypeNum)))
  	;(if (null lineWeightNum) (print "nil") (set_tile "LineWeight_List" (itoa lineWeightNum)))
	
	;系统组
	(if (null rockStructureTypeNum) (setq rockStructureTypeNum 0) (set_tile "RockStructureType_List" (itoa rockStructureTypeNum)))
	(if (null columnRockStructureOne) (print "nil") (set_tile "columnRockStructureOne" columnRockStructureOne))
  	(if (null columnRockStructureTwo) (print "nil") (set_tile "columnRockStructureTwo" columnRockStructureTwo))
	(if (null columnRockStructureThree) (print "nil") (set_tile "columnRockStructureThree" columnRockStructureThree))
	(if (null columnRockStructureFour) (print "nil") (set_tile "columnRockStructureFour" columnRockStructureFour))
	
	;测井钻井柱状图
	(if (null rockTypeNum) (setq rockTypeNum 0) (set_tile "RockType_List" (itoa rockTypeNum)))
	(if (null columnRockOne) (print "nil") (set_tile "columnRockOne" columnRockOne))
  	(if (null columnRockTwo) (print "nil") (set_tile "columnRockTwo" columnRockTwo))
	(if (null columnRockThree) (print "nil") (set_tile "columnRockThree" columnRockThree))
	(if (null columnRockFour) (print "nil") (set_tile "columnRockFour" columnRockFour))
	(if (null columnRockFive) (print "nil") (set_tile "columnRockFive" columnRockFive))
  	;(if (null rockTextSize) (print "nil") (set_tile "RockTextSize_List" (itoa rockTextSize)))
	(cond ((= rockTypeNum 0)  
				(progn
					(mode_tile  "columnRockOne" 0)
					(mode_tile  "columnRockTwo" 0)
					(mode_tile  "columnRockThree" 0)
					(mode_tile  "columnRockFour" 0)
					(mode_tile  "columnRockFive" 0)
					(setq RockTypeName "钻井柱状图")
				) 
			)
		   ((= rockTypeNum 1)
				(progn 
					(mode_tile  "columnRockOne" 0)
					(mode_tile  "columnRockTwo" 0)
					(mode_tile  "columnRockThree" 0)
					(mode_tile  "columnRockFour" 1)
					(mode_tile  "columnRockFive" 1)
					(setq RockTypeName "测井柱状图")
				)
			)
		   ((= rockTypeNum 2)  
				(progn 
					(print "待开发")
				)
			)
	)
	
	;标志层代号
	(if (null columnRockSignOne) (print "nil") (set_tile "columnRockSignOne" columnRockSignOne))
  	(if (null columnRockSignTwo) (print "nil") (set_tile "columnRockSignTwo" columnRockSignTwo))
	(if (null columnRockSignThree) (print "nil") (set_tile "columnRockSignThree" columnRockSignThree))
	
	;完井结构
	(if (null HoleNum) (setq HoleNum 0) (set_tile "HoleNumber_List" (itoa HoleNum)))
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
	
	;井斜方位角
	(if (null columnDrillDeviationOne) (print "nil") (set_tile "columnDrillDeviationOne" columnDrillDeviationOne))
  	(if (null columnDrillDeviationTwo) (print "nil") (set_tile "columnDrillDeviationTwo" columnDrillDeviationTwo))
	(if (null columnDrillDeviationThree) (print "nil") (set_tile "columnDrillDeviationThree" columnDrillDeviationThree))
)

(defun configLoginBtn()
	(setq username (get_tile "username"))
	(setq password (get_tile "password"))
)

(defun configStartBtn()
	;此处可增加初始化代码，预留位置
	(setq d (atof (get_tile "D")))
	(setq dt (atof (get_tile "DT")))
 	(setq mc (atof (get_tile "MC")))
  	(setq wl (atof (get_tile "WL")))
	(print "click startButton")

)


(defun configbaseDataBtn()
	;此处可增加初始化代码，预留位置
	(setq d (atof (get_tile "D")))
	(setq dt (atof (get_tile "DT")))
 	(setq mc (atof (get_tile "MC")))
  	(setq wl (atof (get_tile "WL")))
)

(defun configDFBtn()
	(setq DN (get_tile "DN"))
	(setq DL (get_tile "DL"))
	(setq LX (get_tile "LX"))
	(setq LY (get_tile "LY"))
	(setq LH (get_tile "LH"))
	(setq Company (get_tile "Company"))
	(setq StartDay (get_tile "StartDay"))
	(setq EndDay (get_tile "EndDay"))
	(setq DrillType (get_tile "DrillType"))
	(setq WaterPump (get_tile "WaterPump"))
	(setq MachinePower (get_tile "MachinePower"))
	(setq PictureMaker (get_tile "PictureMaker"))
	(setq PictureProofreader (get_tile "PictureProofreader"))
	(setq PictureChecker (get_tile "PictureChecker"))
	(setq DrillCommander (get_tile "DrillCommander"))
	(setq TechnicalLeader (get_tile "TechnicalLeader"))
	(setq DrillManager (get_tile "DrillManager"))
	(setq PictureNumber (get_tile "PictureNumber"))
	(setq Proportion (get_tile "Proportion"))
	(setq PictureTime (get_tile "PictureTime"))
	(setq DataSource (get_tile "DataSource"))
	
)

;配置画线按钮属性
(defun configDLBtn()
	(setq PLFactor 1)
	(setq columnPLineOne  (get_tile "columnPLineOne"))
	(setq columnPLineTwo  (get_tile "columnPLineTwo"))
	(setq lineNameNum  (atoi (get_tile "LineName_List")))
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

;配置系统组按钮属性
(defun configDRSBtn()
	
	(setq columnRockStructureOne  (get_tile "columnRockStructureOne"))
	(setq columnRockStructureTwo  (get_tile "columnRockStructureTwo"))
	(setq columnRockStructureThree  (get_tile "columnRockStructureThree"))
	(setq columnRockStructureFour (get_tile "columnRockStructureFour"))
	(setq rockStructureTypeNum (atoi(get_tile "RockStructureType_List")))
	;(setq rockTextSizeNum (atoi(get_tile "RockTextSize_List")))
	(setq rockStructureTextSize 3)
	(setq RockStructureXOffset -330)

	(cond ((= rockStructureTypeNum 0)  
					(progn
						(setq RockStructureXOffset -330)
					) 
				)
			   ((= rockStructureTypeNum 1)
					(progn 
						(setq RockStructureXOffset -320)
					)
				)
				 ((= rockStructureTypeNum 2)
					(progn 
						(setq RockStructureXOffset -310)
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
	;(setq rockTextSizeNum (atoi(get_tile "RockTextSize_List")))
	(setq rockTextSize 3)
	(setq RockXOffset -245)

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

;配置标志层按钮属性
(defun configDRSignBtn()
	(setq columnRockSignOne  (get_tile "columnRockSignOne"))
	(setq columnRockSignTwo  (get_tile "columnRockSignTwo"))
	(setq columnRockSignThree  (get_tile "columnRockSignThree"))
	(setq RockSignXOffset -250)
)

;配置井斜方位角按钮属性
(defun configDDDBtn()
	(setq columnDrillDeviationOne  (get_tile "columnDrillDeviationOne"))
	(setq columnDrillDeviationTwo  (get_tile "columnDrillDeviationTwo"))
	(setq columnDrillDeviationThree  (get_tile "columnDrillDeviationThree"))
	(setq DrillDeviationXOffset 0)
)


;根据DCL配置多线段属性
(defun configPLineAttribute()
;配置多线段线型
	(cond ((= lineNameNum 0)
					(progn
							(command "LineType" "s" "ByLayer" "")
							(command "lweight" "ByLayer")

					)
				)
				((= lineNameNum 1)
					(progn
							(command "LineType" "s" "ByLayer" "")
							(command "lweight" "ByLayer")

					)
				)
				((= lineNameNum 2)
					(progn 
							(command "LineType" "s" "ACAD_ISO02W100" "")
							(command "lweight" "ByLayer")

					)   
				)
	)
)

;备用配置多段线属性
(defun configPLineAttributeBeiyong()
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

;配置线型popList动作属性
(defun configLineNamePopListAction()
    (setq lineNameNum (atoi (get_tile "LineName_List")))

)

;配置系统组popList动作属性
(defun configRockStructurePopListAction()
    (setq rockStructureTypeNum  (atoi (get_tile "RockStructureType_List")))
	

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
						(setq RockTypeName "钻井柱状图")

					) 
				)
			   ((= rockTypeNum 1)
					(progn 
						(mode_tile  "columnRockOne" 0)
						(mode_tile  "columnRockTwo" 0)
						(mode_tile  "columnRockThree" 0)
						(mode_tile  "columnRockFour" 1)
						(mode_tile  "columnRockFive" 1)
						(setq RockTypeName "钻井柱状图")
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
  		;(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))
		(drawFrameLine)
		(drawFrameTextLabel)
		(drawBaseDataTable)
		(drawPictureDataTable)
)

;绘制系统组
(defun drawRockStructure()
  		;(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))
		(vl-load-com)
		(Setq EXCEL (Vlax-Get-Or-Create-Object "Excel.Application"))
		(Vlax-Put-Property EXCEL 'Visible -1 )
		(setq cells (Vlax-Get excel 'Cells ))
		(setq rockStructureDataRangeOfY (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockStructureOne))
		(setq rockStructureDataThickness (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockStructureTwo))
		(setq rockStructureDataHoleName (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockStructureThree))
		(setq rockStructureDataSimpleName (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockStructureFour))
		(setq rockStructureLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property rockStructureDataRangeOfY 'Value2))))

	 
		(setq indexRS 1)
		(setq rockStructureListLength (length rockStructureLst))
		(while (<= indexRS rockStructureListLength)
			(if (> indexRS 0)
				  (progn
				  	 	(setq rockStructureDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockStructureDataRangeOfY 'Item indexRS)) 'Value2))
						(setq rockStructureThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockStructureDataThickness 'Item indexRS)) 'Value2))
						(setq rockStructureHoleName (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockStructureDataHoleName 'Item indexRS)) 'Value2))
						(setq rockStructureSimpleName (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockStructureDataSimpleName 'Item indexRS)) 'Value2))
						(drawRockStructureLineAndTextLabel)
					
				)
			)
			(setq indexRS (1+ indexRS))
		)
)

;绘制钻井和测井柱状图
(defun drawRock()
  		;(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))
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
		(command "LineType" "s" "ByLayer" "")
		(command "lweight" "ByLayer")
	 
		(setq indexR 1)
		(setq rockListLength (length rockLst))
		(while (<= indexR rockListLength)
			(if (> indexR 0)
				  (progn
				  	 	(setq rockDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataRangeOfY 'Item indexR)) 'Value2))
						(setq rockThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataThickness 'Item indexR)) 'Value2))
						(setq rockName (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataName 'Item indexR)) 'Value2))
						(setq rockDescription (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataDescription 'Item indexR)) 'Value2))
						(setq rockNumber (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataNumber 'Item indexR)) 'Value2))
					(drawRockLineAndTextLabel)
					(setq BlockCodeName (strcat (strcat (strcat rockName (itoa indexR)) RockTypeName) (rtos (getvar "CDATE") 2 10)))
					(hatchRock rockName rockDeep rockThickness)
					
				)
			)
			(setq indexR (1+ indexR))
		)
	(drawFrameBaseBlock)
)

;绘制系统组
(defun drawRockSign()
  		;(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))
		(vl-load-com)
		(Setq EXCEL (Vlax-Get-Or-Create-Object "Excel.Application"))
		(Vlax-Put-Property EXCEL 'Visible -1 )
		(setq cells (Vlax-Get excel 'Cells ))
		(setq rockSignDataRangeOfY (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockSignOne))
		(setq rockSignDataThickness (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockSignTwo))
		(setq rockSignDataName (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockSignThree))
		(setq rockSignLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property rockSignDataRangeOfY 'Value2))))

		(setq indexRSign 1)
		(setq rockSignListLength (length rockSignLst))
		(while (<= indexRSign rockSignListLength)
			(if (> indexRSign 0)
				  (progn
				  	 	(setq rockSignDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockSignDataRangeOfY 'Item indexRSign)) 'Value2))
						(setq rockSignThickness (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockSignDataThickness 'Item indexRSign)) 'Value2))
						(setq rockSignName (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockSignDataName 'Item indexRSign)) 'Value2))
						(drawRockSignTextLabel)
				)
			)
			(setq indexRSign (+ 1 indexRSign))
		)
)

;完井结构
(defun drawDrillStructure()
	(command "LineType" "s" "ByLayer" "")
	(drawDrillStructureLine)
	(drawDrillStructureTextLabel)
	(drawDrillStructureDimlinear)
  	(hatchDrillStructure)
)

;绘制井斜方位
(defun drawDrillDeviation()
  		;(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))
		(vl-load-com)
		(Setq EXCEL (Vlax-Get-Or-Create-Object "Excel.Application"))
		(Vlax-Put-Property EXCEL 'Visible -1 )
		(setq cells (Vlax-Get excel 'Cells ))
		(setq drillDeviationDataRangeOfY (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnDrillDeviationOne))
		(setq drillDeviationDataDeviation (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnDrillDeviationTwo))
		(setq drillDeviationDataAzimuth (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnDrillDeviationThree))
		(setq drillDeviationLst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property drillDeviationDataRangeOfY 'Value2))))

		(setq indexDDD 1)
		(setq drillDeviationListLength (length drillDeviationLst))
		(while (<= indexDDD drillDeviationListLength)
			(if (> indexDDD 0)
				  (progn
				  	 	(setq drillDeviationDeep (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillDeviationDataRangeOfY 'Item indexDDD)) 'Value2))
						(setq drillDeviationDeviation (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillDeviationDataDeviation 'Item indexDDD)) 'Value2))
						(setq drillDeviationAzimuth (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillDeviationDataAzimuth 'Item indexDDD)) 'Value2))
						(drawDrillDeviationLineAndTextLabel)
				)
			)
			(setq indexDDD (+ 1 indexDDD))
		)
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

(defun drawRockStructureLineAndTextLabel()
	(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockStructureDataRangeOfY 'Item indexRS)) 'Value2))
	(setq pyA (* pyA -5))
	(setq pxA  RockStructureXOffset)
	(setq holeNameLength (/ (strlen  rockStructureHoleName) 2)) ;strlen中文单位为2
	(setq indexRockStructure 1.0)
	(command "line" (strcat (strcat (rtos (- RockStructureXOffset 5) 2 2) ",") (rtos pyA 2 2)) (strcat (strcat (rtos (+ RockStructureXOffset 5) 2 2) ",") (rtos pyA 2 2)) "")
	
	(if (<= rockStructureThickness (* (+ holeNameLength 2) 4))
		(progn
			(if (<= rockStructureThickness 8)
				(progn
					(command "text" "J" "C" (list pxA  (* (- rockStructureDeep (/ rockStructureThickness 4)) -5)) rockStructureTextSize 0 rockStructureSimpleName)

				)
				(progn
					(command "text" "J" "C" (list pxA  (+ (* (- rockStructureDeep (/ rockStructureThickness 2)) -5) 8)) (- rockStructureTextSize 1) 0 rockStructureHoleName)
					(command "text" "J" "C" (list pxA  (+ (* (- rockStructureDeep (/ rockStructureThickness 2)) -5) 4)) (- rockStructureTextSize 1) 0 rockStructureSimpleName)

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
									(command "text" "J" "C" (list pxA (+ (* rockStructureDeep -5) 2)) (- rockStructureTextSize 1) 0 rockStructureSimpleName)

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
	(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockDataRangeOfY 'Item indexR)) 'Value2))
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

;井斜方位角
(defun drawDrillDeviationLineAndTextLabel()
	(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property drillDeviationDataRangeOfY 'Item indexDDD)) 'Value2))
	(setq pyA (* pyA -5))
	(setq pxA  DrillDeviationXOffset)
	;1.2 2.5 3.75
	(command "line" (strcat (strcat (rtos DrillDeviationXOffset 2 2) ",") (rtos pyA 2 2)) (strcat (strcat (rtos (+ DrillDeviationXOffset 25) 2 2) ",") (rtos pyA 2 2)) "")
	(if (null drillDeviationAzimuth)
		(progn
				(command "text" "J" "C" (list (+ pxA 12.5) (+ pyA 2)) 2 0 (strcat (rtos drillDeviationDeviation 2 2) "(°)"))
				(command "text" "J" "C" (list (+ pxA 12.5) (- pyA 4.5)) 2 0 (strcat (rtos drillDeviationDeviation 2 2) "(m)"))
		)
		(progn
				(command "text" "J" "C" (list (+ pxA 12.5) (+ pyA 2)) 2 0 (strcat (strcat (rtos drillDeviationDeviation 2 2) "(°) / ") (strcat (rtos drillDeviationAzimuth 2 2) "(°)")))
				(command "text" "J" "C" (list (+ pxA 12.5) (- pyA 4.5)) 2 0 (strcat (rtos drillDeviationDeviation 2 2) "(m)"))
		)
	)
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

(defun drawRockSignTextLabel()
	(setq pyA (Vlax-Get (vlax-variant-value (Vlax-Get-Property rockSignDataRangeOfY 'Item indexRSign)) 'Value2))
	(setq pyA (* pyA -5))
	(setq pxA  RockSignXOffset)
	;1.2 2.5 3.75

	(cond ((<= rockSignThickness 0.24)
				(progn
					;0.75*5 约等于4
					(command "text" "J" "C" (list pxA (* (- rockSignDeep (/ rockSignThickness 2)) -5)) 0.5 0 rockSignName)

				))
				((and (> rockSignThickness 0.24) (<= rockSignThickness 0.5))
				(progn
					(command "text" "J" "C" (list pxA (* (- rockSignDeep (/ rockSignThickness 2)) -5)) 1 0 rockSignName)
					
				))
				((and (> rockSignThickness 0.5) (<= rockSignThickness 0.75))   
				(progn
					(command "text" "J" "C" (list pxA (* (- rockSignDeep (/ rockSignThickness 2)) -5)) 2 0 rockSignName)
				))
				((> rockSignThickness 0.75)  
				(progn
					(command "text" "J" "C" (list pxA (* (- rockSignDeep (/ rockSignThickness 2)) -5)) 3 0 rockSignName)
				))
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

(defun hatchRock(rockName rockDeep rockThickness)
	;(setq thisdrawing (vla-get-activedocument (vlax-get-acad-object)))
	
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
										(cond ((= rockName "黄土")  
															(progn
																(print "黄土")
																(drawHuangTuCengTop)
																(setq Selectionset (ssget "C" (list -394.75 (- (* d -5) 5.25)) (list -380.25 (- (* d -5) 8.75)))) 
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* d -5) 8.5)) Selectionset)
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
																(setq Selectionset (ssget "C" (list -344 (- (* d -5) 6.75)) (list -331 (- (* d -5) 10.25)))) 
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* d -5) 9.75)) Selectionset)
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
																(setq Selectionset (ssget "C" (list -294.5 (- (* d -5) 6.75)) (list -280.5 (- (* d -5) 10.25)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* d -5) 9.75)) Selectionset)
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
																(setq Selectionset (ssget "C" (list -244 (- (* d -5) 6.75)) (list -231 (- (* d -5) 10.25)))) 
																(creatBlockOrCheckBlock rockName (list -237.5 (- (* d -5) 9.75)) Selectionset)
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
																(setq Selectionset (ssget "C" (list -194 (- (* d -5) 6.75)) (list -181 (- (* d -5) 10.25)))) 
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* d -5) 9.75)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset (* (- rockDeep (/ 0.35 2)) -5)) 1 rockFactor rockMaxThickness (* 5 rockFactor))	
															)
														)  
														((= rockName "粉砂质泥岩")
															(progn 
																(print "粉砂质泥岩")
																(drawFenShaZhiNiYanTop)
																(setq Selectionset (ssget "C" (list -144 (- (* d -5) 5.75)) (list -131 (- (* d -5) 10.25))))
																(creatBlockOrCheckBlock rockName (list -137.5 (- (* d -5) 10)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* d -5) 45))" " " "Selectionset)" " " ))
																(configBoldHatch)
															)
														)  
														((= rockName "泥质粉砂岩")
															(progn 
																(print "泥质粉砂岩")
																(drawNiZhiFenShaYanTop)
																(setq Selectionset (ssget "C" (list -394.75 (- (* d -5) 40.75)) (list -380.25 (- (* d -5) 45.25))))
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* d -5) 45)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* d -5) 45))" " " "Selectionset)" " " ))
																(configBoldHatch)
															)
														)  
														((= rockName "砂砾岩")
															(progn 
																(print "砂砾岩");-316,-3984.5
																(drawShaLiYanTop)
																(setq Selectionset (ssget "C" (list -344 (- (* d -5) 40.75)) (list -331 (- (* d -5) 45.25)))) 
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* d -5) 45)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -337.5 (- (* d -5) 45))" " " "Selectionset)" " " ))
																(configBoldHatch)
																
															) 
														)					
														((= rockName "泥岩")
															(progn 
																(print "泥岩")
																(drawNiYanTop)
																(setq Selectionset (ssget "C" (list -294.75 (- (* d -5) 40.75)) (list -280.25 (- (* d -5) 45.25)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* d -5) 45)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "煤层");！！！！！！！！！！！！！！！！！！！！！！！！！
															(progn 
																(print "煤层")
																;(drawMeiCeng)
																;(setq Selectionset (ssget "C" (list -244.75 (- (* d -5) 40.75)) (list -230.25 (- (* d -5) 45.25)))) 
																;(creatBlockOrCheckBlock rockName (list -237.5 (- (* d -5) 45)) Selectionset)
																;(hatchRockWithMInsert rockName (list RockHatchXOffset (* rockDeep -5)) 1 rockFactor rockMaxThickness (* 5 rockFactor))

																(command "-hatch" "W" "N" (strcat (strcat (rtos (- RockHatchXOffset 7.5) 2 2) ",") (rtos (* (- rockDeep rockThickness) -5) 2 2))  (strcat (strcat (rtos (+ RockHatchXOffset 7.5) 2 2) ",") (rtos (* (- rockDeep rockThickness) -5) 2 2))  (strcat (strcat (rtos (+ RockHatchXOffset 7.5) 2 2) ",") (rtos (* rockDeep -5) 2 2))   (strcat (strcat (rtos (- RockHatchXOffset 7.5) 2 2) ",") (rtos (* rockDeep -5) 2 2)) "C" "" "P" "SOLID" "")
															)
														)  
														((= rockName "炭质泥岩")
															(progn 
																(print "炭质泥岩")
																(drawTanZhiNiYanTop)
																(setq Selectionset (ssget "C" (list -194.75 (- (* d -5) 40.75)) (list -180.25 (- (* d -5) 45.25))))
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* d -5) 45)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "砂质泥岩")
															(progn 
																(print "砂质泥岩")
																(drawShaZhiNiYanTop)
																(setq Selectionset (ssget "C" (list -144 (- (* d -5) 40.75)) (list -131 (- (* d -5) 45.25))))
																(creatBlockOrCheckBlock rockName (list -137.5 (- (* d -5) 45)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* d -5) 45))" " " "Selectionset)" " " ))
																(configBoldHatch)
																
															)
														)  
														((= rockName "泥质砂岩")
															(progn 
																(print "泥质砂岩")
																(drawNiZhiShaYanTop)
																(setq Selectionset (ssget "C" (list -394.75 (- (* d -5) 75.75)) (list -380.25 (- (* d -5) 80.25))))
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* d -5) 80)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "石灰岩")
															(progn 
																(print "石灰岩")
																(drawShiHuiYanTop)
																(setq Selectionset (ssget "C" (list -344.75 (- (* d -5) 75.75)) (list -330.25 (- (* d -5) 80.25))))
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* d -5) 80)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "泥质灰岩")
															(progn 
																(print "泥质灰岩")
																(drawNiZhiHuiYanTop)
																(setq Selectionset (ssget "C" (list -294.75 (- (* d -5) 75.75)) (list -280.25 (- (* d -5) 80.25))))
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* d -5) 80)) Selectionset)
																(configBoldHatch)
															)
														)  
														((= rockName "铝质泥岩")
															(progn 
																(print "铝质泥岩")
																(drawLvZhiNiYanTop)
																(setq Selectionset (ssget "C" (list -244.75 (- (* d -5) 75.75)) (list -230.25 (- (* d -5) 80))))
																(creatBlockOrCheckBlock rockName (list -237.5 (- (* d -5) 80)) Selectionset)
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
																(setq Selectionset (ssget "C" (list -394.75 (- (* d -5) 5.25)) (list -380.25 (- (* d -5) 8.75)))) 
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* d -5) 8.5)) Selectionset)
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
																(setq Selectionset (ssget "C" (list -344 (- (* d -5) 6.75)) (list -331 (- (* d -5) 7.75)))) 
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* d -5) 7.25)) Selectionset)
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -287.5 (- (* d -5) 9.75))" " " "Selectionset)" " " ))
																(configThickHatch)
															)
														)  
														((= rockName "细粒砂岩")
															(progn 
																(print "细粒砂岩")
																(drawXiLiShaYanOne)
																(setq Selectionset (ssget "C" (list -292.5 (- (* d -5) 6.75)) (list -282.5 (- (* d -5) 7.75)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* d -5) 7.25)) Selectionset)
																(configThickHatch)
																
															)
														)  
														((= rockName "中粒砂岩")
															(progn 
																(print "中粒砂岩")															
																(drawZhongLiShaYanOne)
																(setq Selectionset (ssget "C" (list -244 (- (* d -5) 6.75)) (list -181 (- (* d -5) 7.75)))) 
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* d -5) 7.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "粗粒砂岩")
															(progn 
																(print "粗粒砂岩")
																(drawCuLiShaYanOne)
																(setq Selectionset (ssget "C" (list -194 (- (* d -5) 6.75)) (list -181 (- (* d -5) 7.75)))) 
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* d -5) 7.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "粉砂质泥岩")
															(progn 
																(print "粉砂质泥岩")
																(drawFenShaZhiNiYanOne)
																(setq Selectionset (ssget "C" (list -144 (- (* d -5) 5.75)) (list -131 (- (* d -5) 7)))) 
															;	(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* d -5) 45))" " " "Selectionset)" " " ))	
																(creatBlockOrCheckBlock rockName (list -137.5 (- (* d -5) 6.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "泥质粉砂岩")
															(progn 
																(print "泥质粉砂岩")
																(drawNiZhiFenShaYanOne)
																(setq Selectionset (ssget "C" (list -394.75 (- (* d -5) 40.75)) (list -380.25 (- (* d -5) 42)))) 
															;	(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -137.5 (- (* d -5) 45))" " " "Selectionset)" " " ))	
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* d -5) 41.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "砂砾岩")
															(progn 
																(print "砂砾岩");-316,-3984.5
																(drawShaLiYanOne)
																(setq Selectionset (ssget "C" (list -344 (- (* d -5) 40.75)) (list -331 (- (* d -5) 42)))) 
																;(vla-SendCommand thisdrawing (strcat "(creatBlockOrCheckBlock" " " "rockName" " " "(list -337.5 (- (* d -5) 45))" " " "Selectionset)" " " ))
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* d -5) 41.25)) Selectionset)
																(configThickHatch)
															) 
														)					
														((= rockName "泥岩")
															(progn 
																(print "泥岩")
																(drawNiYanOne)
																(setq Selectionset (ssget "C" (list -294.75 (- (* d -5) 40.75)) (list -280.25 (- (* d -5) 42)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* d -5) 41.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "煤层")
															(progn 
																(print "煤层")
																;(drawMeiCeng)
																;(setq Selectionset (ssget "C" (list -244.75 (- (* d -5) 40.75)) (list -230.25 (- (* d -5) 42)))) 
																;(creatBlockOrCheckBlock rockName (list -237.5 (- (* d -5) 40)) Selectionset)
																;(hatchRockWithMInsert rockName (list RockHatchXOffset  (* rockDeep  -5)) 1 (/ rockThickness 2) 1 0)
																(command "-hatch" "W" "N" (strcat (strcat (rtos (- RockHatchXOffset 7.5) 2 2) ",") (rtos (* (- rockDeep rockThickness) -5) 2 2))  (strcat (strcat (rtos (+ RockHatchXOffset 7.5) 2 2) ",") (rtos (* (- rockDeep rockThickness) -5) 2 2))  (strcat (strcat (rtos (+ RockHatchXOffset 7.5) 2 2) ",") (rtos (* rockDeep -5) 2 2))   (strcat (strcat (rtos (- RockHatchXOffset 7.5) 2 2) ",") (rtos (* rockDeep -5) 2 2)) "C" "" "P" "SOLID" "")
															)
														)  
														((= rockName "炭质泥岩")
															(progn 
																(print "炭质泥岩")
																(drawTanZhiNiYanOne)
																(setq Selectionset (ssget "C" (list -194.75 (- (* d -5) 40.75)) (list -180.25 (- (* d -5) 42)))) 
																(creatBlockOrCheckBlock rockName (list -187.5 (- (* d -5) 41.25)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (- rockDeep (/ rockThickness 2)) -5)) 1 (* rockThickness 2) 1 0)
															)
														)  
														((= rockName "砂质泥岩")
															(progn 
																(print "砂质泥岩")
																(drawShaZhiNiYanOne)
																(setq Selectionset (ssget "C" (list -144 (- (* d -5) 40.75)) (list -131 (- (* d -5) 42)))) 
																(creatBlockOrCheckBlock rockName (list -137.5 (- (* d -5) 41.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "泥质砂岩")
															(progn 
																(print "泥质砂岩")
																(drawNiZhiShaYanOne)
																(setq Selectionset (ssget "C" (list -394.75 (- (* d -5) 75.75)) (list -380.25 (- (* d -5) 77)))) 
																(creatBlockOrCheckBlock rockName (list -387.5 (- (* d -5) 76.25)) Selectionset)
																(configThickHatch)
															)
														)  
														((= rockName "石灰岩")
															(progn 
																(print "石灰岩3")
																(drawShiHuiYanOne)
																(setq Selectionset (ssget "C" (list -344.75 (- (* d -5) 75.75)) (list -330.25 (- (* d -5) 77)))) 
																(creatBlockOrCheckBlock rockName (list -337.5 (- (* d -5) 76.25)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (- rockDeep (/ rockThickness 2)) -5)) 1 (* rockThickness 2) 1 0)
															)
														)  
														((= rockName "泥质灰岩")
															(progn 
																(print "泥质灰岩")
																(drawNiZhiHuiYanOne)
																(setq Selectionset (ssget "C" (list -294.75 (- (* d -5) 75.75)) (list -280.25 (- (* d -5) 77)))) 
																(creatBlockOrCheckBlock rockName (list -287.5 (- (* d -5) 76.25)) Selectionset)
																(hatchRockWithMInsert rockName (list RockHatchXOffset  (* (- rockDeep (/ rockThickness 2)) -5)) 1 (* rockThickness 2) 1 0)
															)
														)  
														((= rockName "铝质泥岩")
															(progn 
																(print "铝质泥岩")
																(drawLvZhiNiYanOne)
																(setq Selectionset (ssget "C" (list -244.75 (- (* d -5) 75.75)) (list -230.25 (- (* d -5) 77)))) 
																(creatBlockOrCheckBlock rockName (list -237.5 (- (* d -5) 76.25)) Selectionset)
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
 			(command "zoom" "C" (list -222.5 (* d -5)) "250")
			(if (= rowCount 1) (command "minsert" BlockCodeName InsertPoint  xFactor  yFactor 0 rowCount 1) (command "minsert" BlockCodeName InsertPoint  xFactor  yFactor 0 rowCount 1 rowSpace))
			;(command "minsert" BlockName InsertPoint  xFactor  yFactor 0 rowCount 1 rowSpace);用block命令制作块，然后用minsert命令制作多重引用块
		  ;将多用引用块改成匿名多重引用块。匿名块不是无名块，它是以*U命名的，在块 ;编辑框中不现实匿名块的块名，从而无法编辑。
			;(command "minsert"   BlockName BasicPoint 1 1 0 1 1 0 )
			;(vla-put-name (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (vla-get-name (vlax-ename->vla-object (entlast)))) "*U") 
					;将多用引用块改成匿名多重引用块。匿名块不是无名块，它是以*U命名的，在块 ;编辑框中不现实匿名块的块名，从而无法编辑。
			(princ "\n加密完成")
  			;加密完成后恢复参考图形
) 

(defun creatBlockOrCheckBlock(BlockName BasicPoint Selectionset) 
	(vl-load-com)  ;加载Vlisp函数  
	
	 (if Selectionset  ;选择加密对象 
			(progn    ;以当前时间作为块名  ;因为我们要用多重引用块进行加密，在制作多重引用块之前我们必须将我们需要 ;加密的图元制作成块，为避免与图档内块名冲突，引用了当前时间作为块名 
				(command "zoom" "C" (list -222.5 (* d -5)) "250")
				(command "block" BlockCodeName BasicPoint Selectionset "")
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

;drawBaseDataTable
(defun drawBaseDataTable()
	(command "text" "J" "C" (list -195 100) 20 0 drillName)
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
	(command "text" "J" "C" (list -307.5 53) 3 0 (rtos d 2 2))
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
	(command "line"  (strcat "-75," (rtos (- (* d -5) 15) 2 2)) (strcat "-75," (rtos (- (* d -5) 95) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* d -5) 15) 2 2)) (strcat "55," (rtos (- (* d -5) 15) 2 2)) "")
	(command "line"  (strcat "-50," (rtos (- (* d -5) 35) 2 2)) (strcat "-50," (rtos (- (* d -5) 95) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* d -5) 35) 2 2)) (strcat "55," (rtos (- (* d -5) 35) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* d -5) 47) 2 2)) (strcat "55," (rtos (- (* d -5) 47) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* d -5) 59) 2 2)) (strcat "55," (rtos (- (* d -5) 59) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* d -5) 71) 2 2)) (strcat "55," (rtos (- (* d -5) 71) 2 2)) "")
	(command "line"  (strcat "-75," (rtos (- (* d -5) 83) 2 2)) (strcat "55," (rtos (- (* d -5) 83) 2 2)) "")
	(command "line"  (strcat "-20," (rtos (- (* d -5) 35) 2 2)) (strcat "-20," (rtos (- (* d -5) 95) 2 2)) "")
	(command "line"  (strcat "5," (rtos (- (* d -5) 35) 2 2)) (strcat "5," (rtos (- (* d -5) 95) 2 2)) "")
	(command "text" "J" "C" (list -62.5 (- (* d -5) 43)) 3 0 "制    图")
	(command "text" "J" "C" (list -62.5 (- (* d -5) 55)) 3 0 "校    对")
	(command "text" "J" "C" (list -62.5 (- (* d -5) 67)) 3 0 "审    核")
	(command "text" "J" "C" (list -62.5 (- (* d -5) 79)) 3 0 "机    长")
	(command "text" "J" "C" (list -62.5 (- (* d -5) 91)) 3 0 "技术负责")
	(command "text" "J" "C" (list -7.5 (- (* d -5) 43)) 3 0 "项目经理")
	(command "text" "J" "C" (list -7.5 (- (* d -5) 55)) 3 0 "图    号")
	(command "text" "J" "C" (list -7.5 (- (* d -5) 67)) 3 0 "比    例")
	(command "text" "J" "C" (list -7.5 (- (* d -5) 79)) 3 0 "制作日期")
	(command "text" "J" "C" (list -7.5 (- (* d -5) 91)) 3 0 "资料来源")
	(command "text" "J" "C" (list -7.5 (- (* d -5) 22)) 3.5 0 Company)
	(command "text" "J" "C" (list -7.5 (- (* d -5) 31)) 5.5 0 (strcat DN "井钻井柱状图"))
	(command "text" "J" "C" (list -35 (- (* d -5) 43)) 3 0 PictureMaker)
	(command "text" "J" "C" (list 30 (- (* d -5) 43)) 3 0 DrillManager)
	(command "text" "J" "C" (list -35 (- (* d -5) 55)) 3 0 PictureProofreader)
	(command "text" "J" "C" (list 30 (- (* d -5) 55)) 3 0 PictureNumber)
	(command "text" "J" "C" (list -35 (- (* d -5) 67)) 3 0 PictureChecker)
	(command "text" "J" "C" (list 30 (- (* d -5) 67)) 3 0 Proportion)
	(command "text" "J" "C" (list -35 (- (* d -5) 79)) 3 0 DrillCommander)
	(command "text" "J" "C" (list 30 (- (* d -5) 79)) 3 0 PictureTime)
	(command "text" "J" "C" (list -35 (- (* d -5) 91)) 3 0 DrillManager)
	(command "mtext" (list 6 (- (* d -5) 84)) "h" 3 "w" "48" DataSource "")

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

 (defun eraseLastEnt()
	(setq entName (cdr (car (entget (entlast)))))
	(command "erase" entName "")
 )
 
 ;单独删除多段线
 (defun eraseLine()
	 (cond ((= lineNameNum 0)
						(progn
							(eraseDrillTimeLine)
						)
					)
					((= lineNameNum 1)
						(progn
							(eraseConsumptionLine)
						)
					)
					((= lineNameNum 2)
						(progn
							(eraseWaterLevel)
						)
					)
	)
 )
 
 (defun eraseDrillTimeLine()
	(alert "钻时")
 )
 
 (defun eraseConsumptionLine()
 	(alert "消耗量")

 )
 
 (defun eraseWaterLevel()
 	(alert "水位")

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
	(command "erase" "F" (list -330 -1) (list -330 (+ (* d -5) 1)) "" "")
	(command "erase" "F" (list -330.25 -1) (list -330.25 (+ (* d -5) 1)) "" "")
	(command "erase" "F" (list -329.75 -1) (list -329.75 (+ (* d -5) 1)) "" "")
 )
 
  (defun eraseTong()
  	(command "zoom" "A")
	(command "erase" "F" (list -320 -1) (list -320 (+ (* d -5) 1)) "" "")
	(command "erase" "F" (list -320.25 -1) (list -320.25 (+ (* d -5) 1)) "" "")
	(command "erase" "F" (list -319.75 -1) (list -319.75 (+ (* d -5) 1)) "" "")

 )
 
 (defun eraseZu()
 	(command "zoom" "A")
	(command "erase" "F" (list -310 -1) (list -310 (+ (* d -5) 1)) "" "")
	(command "erase" "F" (list -310.25 -1) (list -310.25 (+ (* d -5) 1)) "" "")
	(command "erase" "F" (list -309.75 -1) (list -309.75 (+ (* d -5) 1)) "" "")
 )
 ;单独删除柱状图
 (defun eraseWellColumnarDiagram()
 	(if (= rockTypeNum 0) (eraseWellDrillingColumnarDiagram) (eraseWellLoggingColumnarDiagram))
 )
 
  ;删除已画钻井柱状图
 (defun eraseWellDrillingColumnarDiagram()
	(command "zoom" "A")
	;erase 床交时能识别的最近距离为5！！！
	(command "erase" "C" (list -240 -5) (list -235 (+ (* d -5) 5)) "")
	(command "erase" "C" (list -220 -5) (list -215 (+ (* d -5) 5)) "")
	(command "erase" "C" (list -205 -5) (list -200 (+ (* d -5) 5)) "")
	(command "erase" "F" (list -193.5 -5) (list -193.5 (+ (* d -5) 1)) "" "")
	(command "erase" "C" (list -180 -5) (list -165 (+ (* d -5) 5)) "")
	(command "erase" "C" (list -155 -5) (list -65 (+ (* d -5) 5)) "")
	;(command "erase" "F" (list -210 (- (* d -5) 1)) (list -193.5 (+ (* d -5) 1)) "" "")

 )
 
 ;删除已画测井柱状图
 (defun eraseWellLoggingColumnarDiagram()
	(command "zoom" "A")
	(command "erase" "C" (list -300 -5) (list -295 (+ (* d -5) 5)) "")
	(command "erase" "C" (list -285 -5) (list -280 (+ (* d -5) 5)) "")
	(command "erase" "F" (list -272.5 -5) (list -272.5 (+ (* d -5) 5)) "" "")
	(command "erase" "C" (list -265 -5) (list -261 (+ (* d -5) 5)) "")
 )
 
 (defun eraseRockSign()
	(command "zoom" "A")
	(command "erase" "F" (list -250 -5) (list -250 (+ (* d -5) 5)) "" "")
	(command "erase" "F" (list -250.25 -5) (list -250.25 (+ (* d -5) 5)) "" "")
	(command "erase" "F" (list -249.75 -5) (list -249.75 (+ (* d -5) 5)) "" "")
 )
 
 ;删除已画完井结构
 (defun EraseDrillCompletionStructure()
	(command "zoom" "A")
	(command "erase" "C" (list -55 -5) (list -5 (+ (* d -5) 5)) "")
 )
 
 ;删除已画井斜方位角
 (defun eraseEraseDrillDeviation()
 	(command "zoom" "A")
	(command "erase" "C" (list 5 -5) (list 20 (+ (* d -5) 5)) "")
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
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "circle" (strcat "-337.5," (rtos (- (* d -5) 41.25) 2 2)) 0.35)
	
	(command "circle" (strcat "-340," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-341," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	 (command "-hatch" "S" (ssget "W" (list -341.5 (- (* d -5) 40.75)) (list -339.5 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -335.5 (- (* d -5) 40.75)) (list -333.5 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )
 
 (defun drawShaLiYanTopVla()
	(vla-SendCommand thisdrawing (strcat "(drawShaLiYanTop)" " "))
)
 
(defun drawShaLiYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-345," (rtos (- (* d -5) 42.5) 2 2)) (strcat "-330," (rtos (- (* d -5) 42.5) 2 2)) "")
  	
	(command "line" (strcat "-345," (rtos (- (* d -5) 45) 2 2)) (strcat "-330," (rtos (- (* d -5) 45) 2 2)) "")
	
	;rowOne
	(command "circle" (strcat "-337.5," (rtos (- (* d -5) 41.25) 2 2)) 0.35)
	
	(command "circle" (strcat "-340," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-341," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	
	(command "-hatch" "S" (ssget "W" (list -341.5 (- (* d -5) 40.75)) (list -339.5 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	(command)
	(command "-hatch" "S" (ssget "W" (list -335.5 (- (* d -5) 40.75)) (list -333.5 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	;rowTwo
	(command "circle" (strcat "-340.5," (rtos (- (* d -5) 43.75) 2 2)) 0.35)
	
	(command "circle" (strcat "-334.5," (rtos (- (* d -5) 43.75) 2 2)) 0.35)
	
	(command "circle" (strcat "-338," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-337," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-332.5," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-331.5," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-342.5," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-343.5," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	

	(command "-hatch" "S" (ssget "W" (list -338.5 (- (* d -5) 43.25)) (list -336.5 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -333 (- (* d -5) 43.25)) (list -331 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* d -5) 43.25)) (list -342 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
)

 (defun drawShaLiYanBottomVla()
	(vla-SendCommand thisdrawing (strcat "(drawShaLiYanBottom)" " "))
)

 (defun drawShaLiYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-345," (rtos (- (* d -5) 40) 2 2)) (strcat "-330," (rtos (- (* d -5) 50) 2 2)) " ")
	
	(command "line" (strcat "-345," (rtos (- (* d -5) 47.5) 2 2)) (strcat "-330," (rtos (- (* d -5) 47.5) 2 2)) "")
	
	;rowThree
	(command "circle" (strcat "-337.5," (rtos (- (* d -5) 46.25) 2 2)) 0.35)
	
	(command "circle" (strcat "-340," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-341," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	
	
	(command "-hatch" "S" (ssget "W" (list -341.5 (- (* d -5) 45.75)) (list -339.5 (- (* d -5) 46.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -335.5 (- (* d -5) 45.75)) (list -333.5 (- (* d -5) 46.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	;rowFour
	(command "circle" (strcat "-340.5," (rtos (- (* d -5) 48.75) 2 2)) 0.35)
	
	(command "circle" (strcat "-334.5," (rtos (- (* d -5) 48.75) 2 2)) 0.35)
	
	(command "circle" (strcat "-338," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-337," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-332.5," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-331.5," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-342.5," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-343.5," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
   	(command "text" "J" "C" (list -337.5 (- (* d -5) 56.5)) 3 0 "砂砾岩")
   	

	(command "-hatch" "S" (ssget "W" (list -338.5 (- (* d -5) 48.25)) (list -336.5 (- (* d -5) 49.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -333 (- (* d -5) 48.25)) (list -331 (- (* d -5) 49.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* d -5) 48.25)) (list -342 (- (* d -5) 49.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	

 ) 

 
 ;砂质泥岩
 
 (defun drawShaZhiNiYan()
	(drawShaZhiNiYanTop)
  	(drawShaZhiNiYanBottom)
 )
 
 (defun drawShaZhiNiYanOne()
	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-141," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-139.5," (rtos (- (* d -5) 41.25) 2 2)) "")
	
	(command "line" (strcat "-135.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-134," (rtos (- (* d -5) 41.25) 2 2)) "")
	
	(command "circle" (strcat "-137.5," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
  	(command "-hatch" "S" (ssget "W" (list -138 (- (* d -5) 40.75)) (list -137 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )
 
(defun drawShaZhiNiYanTop()
  	;(command "zoom" "w" (strcat "-150," (rtos (- (* d -5) 40) 2 2)) (strcat "-125," (rtos (- (* d -5) 50) 2 2)) )
	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-145," (rtos (- (* d -5) 42.5) 2 2)) (strcat "-130," (rtos (- (* d -5) 42.5) 2 2)) "")
  	
	(command "line" (strcat "-145," (rtos (- (* d -5) 45) 2 2)) (strcat "-130," (rtos (- (* d -5) 45) 2 2)) "")
	
	;rowOne
	(command "line" (strcat "-141," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-139.5," (rtos (- (* d -5) 41.25) 2 2)) "")
	
	(command "line" (strcat "-135.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-134," (rtos (- (* d -5) 41.25) 2 2)) "")
	
	(command "circle" (strcat "-137.5," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -138 (- (* d -5) 40.75)) (list -137 (- (* d -5) 41.75))) "" "P" "SOLID" "CO" "ByLayer" "")


	

	;rowTwo
	(command "line" (strcat "-142.5," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-144," (rtos (- (* d -5) 43.75) 2 2)) "")
	(command "line" (strcat "-136.75," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-138.25," (rtos (- (* d -5) 43.75) 2 2)) "")
	(command "line" (strcat "-132.75," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-131.25," (rtos (- (* d -5) 43.75) 2 2)) "")
	
	(command "circle" (strcat "-140.25," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-134.75," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -135.5 (- (* d -5) 43.25)) (list -133.5 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -141.5 (- (* d -5) 43.25)) (list -139.5 (- (* d -5) 44.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
)

(defun drawShaZhiNiYanBottom()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-145," (rtos (- (* d -5) 40) 2 2)) (strcat "-130," (rtos (- (* d -5) 50) 2 2)) " ")
	(command "line" (strcat "-145," (rtos (- (* d -5) 47.5) 2 2)) (strcat "-130," (rtos (- (* d -5) 47.5) 2 2)) "")
  		
	;rowThree
	(command "line" (strcat "-141," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-139.5," (rtos (- (* d -5) 46.25) 2 2)) "")
	
	(command "line" (strcat "-135.5," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-134," (rtos (- (* d -5) 46.25) 2 2)) "")
	
	(command "circle" (strcat "-137.5," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	
  	(command "-hatch" "S" (ssget "W" (list -138 (- (* d -5) 46.75)) (list -137 (- (* d -5) 45.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	;rowFour
	(command "line" (strcat "-142.5," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-144," (rtos (- (* d -5) 48.75) 2 2)) "")
	(command "line" (strcat "-136.75," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-138.25," (rtos (- (* d -5) 48.75) 2 2)) "")
	(command "line" (strcat "-132.75," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-131.25," (rtos (- (* d -5) 48.75) 2 2)) "")
	
	(command "circle" (strcat "-140.25," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-134.75," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
	
	(command "-hatch" "S" (ssget "W" (list -135.5 (- (* d -5) 49.25)) (list -133.5 (- (* d -5) 48.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "-hatch" "S" (ssget "W" (list -141.5 (- (* d -5) 49.25)) (list -139.5 (- (* d -5) 48.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	
	(command "text" "J" "C" (list -137.5 (- (* d -5) 56.5)) 3 0 "砂质泥岩")
   	
)


;细粒砂岩

(defun drawXiLiShaYanVla()
	(vla-SendCommand thisdrawing (strcat "(drawXiLiShaYan)" " "))
	

)

 (defun drawXiLiShaYan()
	(drawXiLiShaYanTop)
	(drawXiLiShaYanBottom)
)
 
 (defun drawXiLiShaYanOneVla()
	(vla-SendCommand thisdrawing (strcat "(drawXiLiShaYanOne)" " "))

)
 
 (defun drawXiLiShaYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "circle" (strcat "-291.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-290.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-289.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-285.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-284.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-283.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -292.5 (- (* d -5) 7.75)) (list -282.5 (- (* d -5) 6.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )
 
  (defun drawXiLiShaYanTopVla()
	(vla-SendCommand thisdrawing (strcat "(drawXiLiShaYanTop)" " "))
	
)
 
(defun drawXiLiShaYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	(command "circle" (strcat "-291.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-290.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-289.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-285.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-284.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-283.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	
	;rowTwo
	(command "circle" (strcat "-287.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-286.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-288.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-291.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-292.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-293.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-283.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-282.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-281.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	

	(command "-hatch" "S" (ssget "W" (list -294.5 (- (* d -5) 6.75)) (list -280.5 (- (* d -5) 10.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	

)

  (defun drawXiLiShaYanBottomVla()
	(vla-SendCommand thisdrawing (strcat "(drawXiLiShaYanBottom)" " "))
)

 (defun drawXiLiShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-295," (rtos (- (* d -5) 5) 2 2)) (strcat "-280," (rtos (- (* d -5) 15) 2 2)) " ")
	
	;rowThree
	(command "circle" (strcat "-291.5," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-290.5," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-289.5," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-285.5," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-284.5," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-283.5," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -292.5 (- (* d -5) 11.75)) (list -282.5 (- (* d -5) 12.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	

	(command "text" "J" "C" (list -287.5 (- (* d -5) 21.5)) 3 0 "细粒砂岩")
   	
 ) 
 
;粉砂岩
(defun drawFenShaYan()
	(drawFenShaYanTop)
	(drawFenShaYanBottom)
)

 (defun drawFenShaYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	;left
	(command "circle" (strcat "-339.25," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-339.75," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.25," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.75," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	;right
	(command "circle" (strcat "-335.75," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335.25," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.75," (rtos (- (* d -5)7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.25," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* d -5) 6.75)) (list -331 (- (* d -5) 7.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )

 (defun drawFenShaYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	
	;rowOne
	;left
	(command "circle" (strcat "-339.25," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-339.75," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.25," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.75," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	;right
	(command "circle" (strcat "-335.75," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335.25," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.75," (rtos (- (* d -5)7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.25," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	;rowTwo
	;mid
	(command "circle" (strcat "-336.75," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-337.25," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-337.75," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-338.25," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	;left
	(command "circle" (strcat "-341.75," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-342.25," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-342.75," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-343.25," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	;right
	(command "circle" (strcat "-332.75," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-332.25," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-331.75," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-331.25," (rtos (- (* d -5) 9.75) 2 2)) 0.10)	
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* d -5) 6.75)) (list -331 (- (* d -5) 10.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
)

 (defun drawFenShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-345," (rtos (- (* d -5) 5) 2 2)) (strcat "-330," (rtos (- (* d -5) 15) 2 2)) " ")
	
	;rowThree
	;left
	(command "circle" (strcat "-339.25," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-339.75," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.25," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-340.75," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	;right
	(command "circle" (strcat "-335.75," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-335.25," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.75," (rtos (- (* d -5)12.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-334.25," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -344 (- (* d -5) 11.75)) (list -331 (- (* d -5) 12.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -337.5 (- (* d -5) 21.5)) 3 0 "粉砂岩")

 ) 
 
 ;中粒砂岩
(defun drawZhongLiShaYan()
	(drawZhongLiShaYanTop)
	(drawZhongLiShaYanBottom)
)

 (defun drawZhongLiShaYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	;left
	(command "circle" (strcat "-241," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	(command "circle" (strcat "-240," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-235," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	(command "circle" (strcat "-234," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	
	(command "-hatch" "S" (ssget "W" (list -244 (- (* d -5) 6.75)) (list -231 (- (* d -5) 7.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )

 (defun drawZhongLiShaYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	
	;rowOne
	;left
	(command "circle" (strcat "-241," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	(command "circle" (strcat "-240," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-235," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	(command "circle" (strcat "-234," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	;rowTwo
	;mid
	(command "circle" (strcat "-237," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	(command "circle" (strcat "-238," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	;left
	(command "circle" (strcat "-242," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-243," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-232," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "circle" (strcat "-233," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -244 (- (* d -5) 6.75)) (list -231 (- (* d -5) 10.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
)

 (defun drawZhongLiShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-245," (rtos (- (* d -5) 5) 2 2)) (strcat "-230," (rtos (- (* d -5) 15) 2 2)) " ")
	
	;rowThree
	;left
	(command "circle" (strcat "-241," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	(command "circle" (strcat "-240," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-235," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	(command "circle" (strcat "-234," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -244 (- (* d -5) 11.75)) (list -231 (- (* d -5) 12.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -237.5 (- (* d -5) 21.5)) 3 0 "中粒砂岩")

 ) 
 
  ;粗粒砂岩
(defun drawCuLiShaYan()
	(drawCuLiShaYanTop)
	(drawCuLiShaYanBottom)
)

 (defun drawCuLiShaYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	;left
	(command "circle" (strcat "-190.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-184.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)

	(command "-hatch" "S" (ssget "W" (list -194 (- (* d -5) 6.75)) (list -181 (- (* d -5) 7.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )

 (defun drawCuLiShaYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	
	;rowOne
	;left
	(command "circle" (strcat "-190.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-184.5," (rtos (- (* d -5) 7.25) 2 2)) 0.10)
	
	;rowTwo
	;mid
	(command "circle" (strcat "-187.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	;left
	(command "circle" (strcat "-192.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-182.5," (rtos (- (* d -5) 9.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -194 (- (* d -5) 6.75)) (list -181 (- (* d -5) 10.25))) "" "P" "SOLID" "CO" "ByLayer" "")
)

 (defun drawCuLiShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-195," (rtos (- (* d -5) 5) 2 2)) (strcat "-180," (rtos (- (* d -5) 15) 2 2)) " ")
	
	;rowThree
	;left
	(command "circle" (strcat "-190.5," (rtos (- (* d -5) 12.25) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-184.5," (rtos (- (* d -5) 12.25) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -194 (- (* d -5) 11.75)) (list -181 (- (* d -5) 12.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -187.5 (- (* d -5) 21.5)) 3 0 "粗粒砂岩")

 ) 
 
  ;粉砂质泥岩
(defun drawFenShaZhiNiYan()
	(drawFenShaZhiNiYanTop)
	(drawFenShaZhiNiYanBottom)
)

 (defun drawFenShaZhiNiYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	;mid
	(command "circle" (strcat "-138.25," (rtos (- (* d -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.75," (rtos (- (* d -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.25," (rtos (- (* d -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-136.75," (rtos (- (* d -5) 6.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-139.75," (rtos (- (* d -5) 6.25) 2 2)) (strcat "-141.25," (rtos (- (* d -5) 6.25) 2 2)) "")

	;right
	(command "line" (strcat "-135.25," (rtos (- (* d -5) 6.25) 2 2)) (strcat "-133.75," (rtos (- (* d -5) 6.25) 2 2)) "")

	(command "-hatch" "S" (ssget "W" (list -144 (- (* d -5) 5.75)) (list -131 (- (* d -5) 6.75))) "" "P" "SOLID" "CO" "ByLayer" "")
	
 )

 (defun drawFenShaZhiNiYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-145," (rtos (- (* d -5) 7.5) 2 2)) (strcat "-130," (rtos (- (* d -5) 7.5) 2 2)) "")
	(command "line" (strcat "-145," (rtos (- (* d -5) 10) 2 2)) (strcat "-130," (rtos (- (* d -5) 10) 2 2)) "")

	;rowOne
	;mid
	(command "circle" (strcat "-138.25," (rtos (- (* d -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.75," (rtos (- (* d -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.25," (rtos (- (* d -5) 6.25) 2 2)) 0.10)
	(command "circle" (strcat "-136.75," (rtos (- (* d -5) 6.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-139.75," (rtos (- (* d -5) 6.25) 2 2)) (strcat "-141.25," (rtos (- (* d -5) 6.25) 2 2)) "")

	;right
	(command "line" (strcat "-135.25," (rtos (- (* d -5) 6.25) 2 2)) (strcat "-133.75," (rtos (- (* d -5) 6.25) 2 2)) "")
	
	;rowTwo
	;mid
	(command "line" (strcat "-138.25," (rtos (- (* d -5) 8.75) 2 2)) (strcat "-136.75," (rtos (- (* d -5) 8.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-139.75," (rtos (- (* d -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-140.25," (rtos (- (* d -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-140.75," (rtos (- (* d -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-141.25," (rtos (- (* d -5) 8.75) 2 2)) 0.10)
	
	(command "line" (strcat "-142.75," (rtos (- (* d -5) 8.75) 2 2)) (strcat "-144.25," (rtos (- (* d -5) 8.75) 2 2)) "")


	;right
	(command "circle" (strcat "-135.25," (rtos (- (* d -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-134.75," (rtos (- (* d -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-134.25," (rtos (- (* d -5) 8.75) 2 2)) 0.10)
	(command "circle" (strcat "-133.75," (rtos (- (* d -5) 8.75) 2 2)) 0.10)
	
	(command "line" (strcat "-132.25," (rtos (- (* d -5) 8.75) 2 2)) (strcat "-130.75," (rtos (- (* d -5) 8.75) 2 2)) "")	
	(command "-hatch" "S" (ssget "W" (list -144 (- (* d -5) 5.75)) (list -131 (- (* d -5) 9.25))) "" "P" "SOLID" "CO" "ByLayer" "")
)

 (defun drawFenShaZhiNiYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-145," (rtos (- (* d -5) 5) 2 2)) (strcat "-130," (rtos (- (* d -5) 15) 2 2)) " ")
	(command "line" (strcat "-145," (rtos (- (* d -5) 12.5) 2 2)) (strcat "-130," (rtos (- (* d -5) 12.5) 2 2)) "")

	
	(command "line" (strcat "-145," (rtos (- (* d -5) 7.5) 2 2)) (strcat "-130," (rtos (- (* d -5) 7.5) 2 2)) "")
	(command "line" (strcat "-145," (rtos (- (* d -5) 10) 2 2)) (strcat "-130," (rtos (- (* d -5) 10) 2 2)) "")

	;rowThree
	;mid
	(command "circle" (strcat "-138.25," (rtos (- (* d -5) 11.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.75," (rtos (- (* d -5) 11.25) 2 2)) 0.10)
	(command "circle" (strcat "-137.25," (rtos (- (* d -5) 11.25) 2 2)) 0.10)
	(command "circle" (strcat "-136.75," (rtos (- (* d -5) 11.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-139.75," (rtos (- (* d -5) 11.25) 2 2)) (strcat "-141.25," (rtos (- (* d -5) 11.25) 2 2)) "")

	;right
	(command "line" (strcat "-135.25," (rtos (- (* d -5) 11.25) 2 2)) (strcat "-133.75," (rtos (- (* d -5) 11.25) 2 2)) "")
	
	;rowTwo
	;mid
	(command "line" (strcat "-138.25," (rtos (- (* d -5) 13.75) 2 2)) (strcat "-136.75," (rtos (- (* d -5) 13.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-139.75," (rtos (- (* d -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-140.25," (rtos (- (* d -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-140.75," (rtos (- (* d -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-141.25," (rtos (- (* d -5) 13.75) 2 2)) 0.10)
	
	(command "line" (strcat "-142.75," (rtos (- (* d -5) 13.75) 2 2)) (strcat "-144.25," (rtos (- (* d -5) 13.75) 2 2)) "")


	;right
	(command "circle" (strcat "-135.25," (rtos (- (* d -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-134.75," (rtos (- (* d -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-134.25," (rtos (- (* d -5) 13.75) 2 2)) 0.10)
	(command "circle" (strcat "-133.75," (rtos (- (* d -5) 13.75) 2 2)) 0.10)
	
	(command "line" (strcat "-132.25," (rtos (- (* d -5) 13.75) 2 2)) (strcat "-130.75," (rtos (- (* d -5) 13.75) 2 2)) "")	
	(command "-hatch" "S" (ssget "W" (list -144 (- (* d -5) 10.75)) (list -131 (- (* d -5) 14.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -137.5 (- (* d -5) 21.5)) 3 0 "粉砂质泥岩")

 ) 
 
  ;泥质粉砂岩
(defun drawNiZhiFenShaYan()
	(drawNiZhiFenShaYanTop)
	(drawNiZhiFenShaYanBottom)
)

 (defun drawNiZhiFenShaYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	;mid
	(command "circle" (strcat "-387.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-388.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-387.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-386.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-389.75," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-391.25," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command "circle" (strcat "-392.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-394.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)

	;right
	(command "line" (strcat "-385.25," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-383.75," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command "circle" (strcat "-382.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-380.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)

	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* d -5) 40.75)) (list -380.25 (- (* d -5) 42))) "" "P" "SOLID" "CO" "ByLayer" "")	
 )

 (defun drawNiZhiFenShaYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-395," (rtos (- (* d -5) 42.5) 2 2)) (strcat "-380," (rtos (- (* d -5) 42.5) 2 2)) "")
	(command "line" (strcat "-395," (rtos (- (* d -5) 45) 2 2)) (strcat "-380," (rtos (- (* d -5) 45) 2 2)) "")

	;rowOne
	;mid
	(command "circle" (strcat "-387.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-388.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-387.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-386.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-389.75," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-391.25," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command "circle" (strcat "-392.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-394.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)

	;right
	(command "line" (strcat "-385.25," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-383.75," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command "circle" (strcat "-382.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.25," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	(command "circle" (strcat "-380.75," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	;rowTwo
	;mid
	(command "line" (strcat "-388.25," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-386.75," (rtos (- (* d -5) 43.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-389.75," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-390.25," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-390.75," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-391.25," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-385.25," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-384.75," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-384.25," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	(command "circle" (strcat "-383.75," (rtos (- (* d -5) 43.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* d -5) 40.75)) (list -380.25 (- (* d -5) 45.25))) "" "P" "SOLID" "CO" "ByLayer" "")
)

 (defun drawNiZhiFenShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-395," (rtos (- (* d -5) 40) 2 2)) (strcat "-380," (rtos (- (* d -5) 50) 2 2)) " ")
	(command "line" (strcat "-395," (rtos (- (* d -5) 47.5) 2 2)) (strcat "-380," (rtos (- (* d -5) 47.5) 2 2)) "")

	;rowThree
	;mid
	(command "circle" (strcat "-387.75," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-388.25," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-387.25," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-386.75," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	;left
	(command "line" (strcat "-389.75," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-391.25," (rtos (- (* d -5) 46.25) 2 2)) "")
	(command "circle" (strcat "-392.75," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.25," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-393.75," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-394.25," (rtos (- (* d -5) 46.25) 2 2)) 0.10)

	;right
	(command "line" (strcat "-385.25," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-383.75," (rtos (- (* d -5) 46.25) 2 2)) "")
	(command "circle" (strcat "-382.25," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.75," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-381.25," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	(command "circle" (strcat "-380.75," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	
	;rowFour
	;mid
	(command "line" (strcat "-388.25," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-386.75," (rtos (- (* d -5) 48.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-389.75," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-390.25," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-390.75," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-391.25," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
	;right
	(command "circle" (strcat "-385.25," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-384.75," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-384.25," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	(command "circle" (strcat "-383.75," (rtos (- (* d -5) 48.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* d -5) 45.75)) (list -380.25 (- (* d -5) 50.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -387.5 (- (* d -5) 56.5)) 3 0 "泥质粉砂岩") 
 ) 
 
   ;泥质砂岩
(defun drawNiZhiShaYan()
	(drawNiZhiShaYanTop)
	(drawNiZhiShaYanBottom)
)

 (defun drawNiZhiShaYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	;mid
	(command "circle" (strcat "-387.5," (rtos (- (* d -5) 76.25) 2 2)) 0.10)

	;left
	(command "line" (strcat "-389.75," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-391.25," (rtos (- (* d -5) 76.25) 2 2)) "")

	(command "circle" (strcat "-393.5," (rtos (- (* d -5) 76.25) 2 2)) 0.10)


	;right
	(command "line" (strcat "-385.25," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-383.75," (rtos (- (* d -5) 76.25) 2 2)) "")
	(command "circle" (strcat "-381.5," (rtos (- (* d -5) 76.25) 2 2)) 0.10)


	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* d -5) 75.75)) (list -380.25 (- (* d -5) 77))) "" "P" "SOLID" "CO" "ByLayer" "")	
 )

 (defun drawNiZhiShaYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-395," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-380," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-395," (rtos (- (* d -5) 80) 2 2)) (strcat "-380," (rtos (- (* d -5) 80) 2 2)) "")

	;rowOne
	;mid
	(command "circle" (strcat "-387.5," (rtos (- (* d -5) 76.25) 2 2)) 0.10)

	;left
	(command "line" (strcat "-389.75," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-391.25," (rtos (- (* d -5) 76.25) 2 2)) "")

	(command "circle" (strcat "-393.5," (rtos (- (* d -5) 76.25) 2 2)) 0.10)


	;right
	(command "line" (strcat "-385.25," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-383.75," (rtos (- (* d -5) 76.25) 2 2)) "")
	(command "circle" (strcat "-381.5," (rtos (- (* d -5) 76.25) 2 2)) 0.10)

	
	;rowTwo
	;mid
	(command "line" (strcat "-388.25," (rtos (- (* d -5) 78.75) 2 2)) (strcat "-386.75," (rtos (- (* d -5) 78.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-390.5," (rtos (- (* d -5) 78.75) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-384.5," (rtos (- (* d -5) 78.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* d -5) 75.75)) (list -380.25 (- (* d -5) 80.25))) "" "P" "SOLID" "CO" "ByLayer" "")
)

 (defun drawNiZhiShaYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-395," (rtos (- (* d -5) 75) 2 2)) (strcat "-380," (rtos (- (* d -5) 85) 2 2)) " ")
	(command "line" (strcat "-395," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-380," (rtos (- (* d -5) 82.5) 2 2)) "")

	;rowThree
	;mid
	(command "circle" (strcat "-387.5," (rtos (- (* d -5) 81.25) 2 2)) 0.10)

	;left
	(command "line" (strcat "-389.75," (rtos (- (* d -5) 81.25) 2 2)) (strcat "-391.25," (rtos (- (* d -5) 81.25) 2 2)) "")

	(command "circle" (strcat "-393.5," (rtos (- (* d -5) 81.25) 2 2)) 0.10)


	;right
	(command "line" (strcat "-385.25," (rtos (- (* d -5) 81.25) 2 2)) (strcat "-383.75," (rtos (- (* d -5) 81.25) 2 2)) "")
	(command "circle" (strcat "-381.5," (rtos (- (* d -5) 81.25) 2 2)) 0.10)

	
	;rowFour
	;mid
	(command "line" (strcat "-388.25," (rtos (- (* d -5) 83.75) 2 2)) (strcat "-386.75," (rtos (- (* d -5) 83.75) 2 2)) "")
	
	;left
	(command "circle" (strcat "-390.5," (rtos (- (* d -5) 83.75) 2 2)) 0.10)

	;right
	(command "circle" (strcat "-384.5," (rtos (- (* d -5) 83.75) 2 2)) 0.10)
	
	(command "-hatch" "S" (ssget "W" (list -394.75 (- (* d -5) 80.75)) (list -380.25 (- (* d -5) 85.25))) "" "P" "SOLID" "CO" "ByLayer" "")
	
	(command "text" "J" "C" (list -387.5 (- (* d -5) 91.5)) 3 0 "泥质砂岩")
 ) 
 
    ;石灰岩
(defun drawShiHuiYan()
	(drawShiHuiYanTop)
	(drawShiHuiYanBottom)
)

 (defun drawShiHuiYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	(command "line" (strcat "-344.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-344.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-340.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-340.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-336.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-336.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-332.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-332.5," (rtos (- (* d -5) 77.5) 2 2)) "")
 )

 (defun drawShiHuiYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-345," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-330," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-345," (rtos (- (* d -5) 80) 2 2)) (strcat "-330," (rtos (- (* d -5) 80) 2 2)) "")

	;rowOne
	(command "line" (strcat "-344.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-344.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-340.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-340.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-336.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-336.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-332.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-332.5," (rtos (- (* d -5) 77.5) 2 2)) "")

	;rowTwo
	(command "line" (strcat "-330.5," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-330.5," (rtos (- (* d -5) 80) 2 2)) "")
	(command "line" (strcat "-334.5," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-334.5," (rtos (- (* d -5) 80) 2 2)) "")
	(command "line" (strcat "-338.5," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-338.5," (rtos (- (* d -5) 80) 2 2)) "")
	(command "line" (strcat "-342.5," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-342.5," (rtos (- (* d -5) 80) 2 2)) "")
	
)

 (defun drawShiHuiYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-345," (rtos (- (* d -5) 75) 2 2)) (strcat "-330," (rtos (- (* d -5) 85) 2 2)) " ")
	(command "line" (strcat "-345," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-330," (rtos (- (* d -5) 82.5) 2 2)) "")

	;rowThree
	(command "line" (strcat "-344.5," (rtos (- (* d -5) 80) 2 2)) (strcat "-344.5," (rtos (- (* d -5) 82.5) 2 2)) "")
	(command "line" (strcat "-340.5," (rtos (- (* d -5) 80) 2 2)) (strcat "-340.5," (rtos (- (* d -5) 82.5) 2 2)) "")
	(command "line" (strcat "-336.5," (rtos (- (* d -5) 80) 2 2)) (strcat "-336.5," (rtos (- (* d -5) 82.5) 2 2)) "")
	(command "line" (strcat "-332.5," (rtos (- (* d -5) 80) 2 2)) (strcat "-332.5," (rtos (- (* d -5) 82.5) 2 2)) "")

	;rowFour
	(command "line" (strcat "-330.5," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-330.5," (rtos (- (* d -5) 85) 2 2)) "")
	(command "line" (strcat "-334.5," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-334.5," (rtos (- (* d -5) 85) 2 2)) "")
	(command "line" (strcat "-338.5," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-338.5," (rtos (- (* d -5) 85) 2 2)) "")
	(command "line" (strcat "-342.5," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-342.5," (rtos (- (* d -5) 85) 2 2)) "")
	
	(command "text" "J" "C" (list -337.5 (- (* d -5) 91.5)) 3 0 "石灰岩")
 ) 
 
     ;泥质灰岩
(defun drawNiZhiHuiYan()
	(drawNiZhiHuiYanTop)
	(drawNiZhiHuiYanBottom)
)

 (defun drawNiZhiHuiYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	(command "line" (strcat "-287.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-287.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-291.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-291.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-283.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-283.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-288.75," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-290.25," (rtos (- (* d -5) 76.25) 2 2)) "")
	(command "line" (strcat "-286.25," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-284.75," (rtos (- (* d -5) 76.25) 2 2)) "")
 )

 (defun drawNiZhiHuiYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-295," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-280," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-295," (rtos (- (* d -5) 80) 2 2)) (strcat "-280," (rtos (- (* d -5) 80) 2 2)) "")

	;rowOne
	(command "line" (strcat "-287.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-287.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-291.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-291.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-283.5," (rtos (- (* d -5) 75) 2 2)) (strcat "-283.5," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-288.75," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-290.25," (rtos (- (* d -5) 76.25) 2 2)) "")
	(command "line" (strcat "-286.25," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-284.75," (rtos (- (* d -5) 76.25) 2 2)) "")


	;rowTwo
	(command "line" (strcat "-289.5," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-289.5," (rtos (- (* d -5) 80) 2 2)) "")
	(command "line" (strcat "-293.5," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-293.5," (rtos (- (* d -5) 80) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-285.5," (rtos (- (* d -5) 80) 2 2)) "")
	(command "line" (strcat "-281.5," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-281.5," (rtos (- (* d -5) 80) 2 2)) "")
	
)

 (defun drawNiZhiHuiYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-295," (rtos (- (* d -5) 75) 2 2)) (strcat "-280," (rtos (- (* d -5) 85) 2 2)) " ")
	(command "line" (strcat "-295," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-280," (rtos (- (* d -5) 82.5) 2 2)) "")

	;rowThree
	(command "line" (strcat "-287.5," (rtos (- (* d -5) 80) 2 2)) (strcat "-287.5," (rtos (- (* d -5) 82.5) 2 2)) "")
	(command "line" (strcat "-291.5," (rtos (- (* d -5) 80) 2 2)) (strcat "-291.5," (rtos (- (* d -5) 82.5) 2 2)) "")
	(command "line" (strcat "-283.5," (rtos (- (* d -5) 80) 2 2)) (strcat "-283.5," (rtos (- (* d -5) 82.5) 2 2)) "")
	(command "line" (strcat "-288.75," (rtos (- (* d -5) 81.25) 2 2)) (strcat "-290.25," (rtos (- (* d -5) 81.25) 2 2)) "")
	(command "line" (strcat "-286.25," (rtos (- (* d -5) 81.25) 2 2)) (strcat "-284.75," (rtos (- (* d -5) 81.25) 2 2)) "")


	;rowFour
	(command "line" (strcat "-289.5," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-289.5," (rtos (- (* d -5) 85) 2 2)) "")
	(command "line" (strcat "-293.5," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-293.5," (rtos (- (* d -5) 85) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-285.5," (rtos (- (* d -5) 85) 2 2)) "")
	(command "line" (strcat "-281.5," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-281.5," (rtos (- (* d -5) 85) 2 2)) "")
	
	(command "text" "J" "C" (list -287.5 (- (* d -5) 91.5)) 3 0 "泥质灰岩")
 ) 
 
      ;泥岩
(defun drawNiYan()
	(drawNiYanTop)
	(drawNiYanBottom)
)

 (defun drawNiYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	(command "line" (strcat "-289.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-291," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-284," (rtos (- (* d -5) 41.25) 2 2)) "")

 )

 (defun drawNiYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-295," (rtos (- (* d -5) 42.5) 2 2)) (strcat "-280," (rtos (- (* d -5) 42.5) 2 2)) "")
	(command "line" (strcat "-295," (rtos (- (* d -5) 45) 2 2)) (strcat "-280," (rtos (- (* d -5) 45) 2 2)) "")

	;rowOne
	(command "line" (strcat "-289.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-291," (rtos (- (* d -5) 41.25) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-284," (rtos (- (* d -5) 41.25) 2 2)) "")



	;rowTwo
	(command "line" (strcat "-288.25," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-286.75," (rtos (- (* d -5) 43.75) 2 2)) "")
	(command "line" (strcat "-292.25," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-293.75," (rtos (- (* d -5) 43.75) 2 2)) "")
	(command "line" (strcat "-282.75," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-281.25," (rtos (- (* d -5) 43.75) 2 2)) "")

)

 (defun drawNiYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-295," (rtos (- (* d -5) 40) 2 2)) (strcat "-280," (rtos (- (* d -5) 50) 2 2)) " ")
	(command "line" (strcat "-295," (rtos (- (* d -5) 47.5) 2 2)) (strcat "-280," (rtos (- (* d -5) 47.5) 2 2)) "")

	;rowThree
	(command "line" (strcat "-289.5," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-291," (rtos (- (* d -5) 46.25) 2 2)) "")
	(command "line" (strcat "-285.5," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-284," (rtos (- (* d -5) 46.25) 2 2)) "")

	;rowFour
	(command "line" (strcat "-288.25," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-286.75," (rtos (- (* d -5) 48.75) 2 2)) "")
	(command "line" (strcat "-292.25," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-293.75," (rtos (- (* d -5) 48.75) 2 2)) "")
	(command "line" (strcat "-282.75," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-281.25," (rtos (- (* d -5) 48.75) 2 2)) "")
	
	(command "text" "J" "C" (list -287.5 (- (* d -5) 56.5)) 3 0 "泥岩")
 ) 
 
       ;铝质泥岩
(defun drawLvZhiNiYan()
	(drawLvZhiNiYanTop)
	(drawLvZhiNiYanBottom)
)

 (defun drawLvZhiNiYanOne()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	(command "circle" (strcat "-237.5," (rtos (- (* d -5) 76.25) 2 2)) 0.30)
	(command "circle" (strcat "-237.5," (rtos (- (* d -5) 76.25) 2 2)) 1.0)

	(command "line" (strcat "-239.5," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-241," (rtos (- (* d -5) 76.25) 2 2)) "")
	(command "line" (strcat "-235.5," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-234," (rtos (- (* d -5) 76.25) 2 2)) "")

 )

 (defun drawLvZhiNiYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-245," (rtos (- (* d -5) 77.5) 2 2)) (strcat "-230," (rtos (- (* d -5) 77.5) 2 2)) "")
	(command "line" (strcat "-245," (rtos (- (* d -5) 80) 2 2)) (strcat "-230," (rtos (- (* d -5) 80) 2 2)) "")

	;rowOne
	(command "circle" (strcat "-237.5," (rtos (- (* d -5) 76.25) 2 2)) 0.30)
	(command "circle" (strcat "-237.5," (rtos (- (* d -5) 76.25) 2 2)) 1.0)

	(command "line" (strcat "-239.5," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-241," (rtos (- (* d -5) 76.25) 2 2)) "")
	(command "line" (strcat "-235.5," (rtos (- (* d -5) 76.25) 2 2)) (strcat "-234," (rtos (- (* d -5) 76.25) 2 2)) "")



	;rowTwo
	;mid
	(command "line" (strcat "-238.25," (rtos (- (* d -5) 78.75) 2 2)) (strcat "-236.75," (rtos (- (* d -5) 78.75) 2 2)) "")
	;left
	(command "circle" (strcat "-240.25," (rtos (- (* d -5) 78.75) 2 2)) 0.30)
	(command "circle" (strcat "-240.25," (rtos (- (* d -5) 78.75) 2 2)) 1.0)
	(command "line" (strcat "-242.25," (rtos (- (* d -5) 78.75) 2 2)) (strcat "-243.75," (rtos (- (* d -5) 78.75) 2 2)) "")
	;right
	(command "circle" (strcat "-234.75," (rtos (- (* d -5) 78.75) 2 2)) 0.30)
	(command "circle" (strcat "-234.75," (rtos (- (* d -5) 78.75) 2 2)) 1.0)
	(command "line" (strcat "-232.75," (rtos (- (* d -5) 78.75) 2 2)) (strcat "-231.25," (rtos (- (* d -5) 78.75) 2 2)) "")
)

 (defun drawLvZhiNiYanBottom()
   	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-245," (rtos (- (* d -5) 75) 2 2)) (strcat "-230," (rtos (- (* d -5) 85) 2 2)) " ")
	(command "line" (strcat "-245," (rtos (- (* d -5) 82.5) 2 2)) (strcat "-230," (rtos (- (* d -5) 82.5) 2 2)) "")

	;rowThree
	(command "circle" (strcat "-237.5," (rtos (- (* d -5) 81.25) 2 2)) 0.30)
	(command "circle" (strcat "-237.5," (rtos (- (* d -5) 81.25) 2 2)) 1.0)

	(command "line" (strcat "-239.5," (rtos (- (* d -5) 81.25) 2 2)) (strcat "-241," (rtos (- (* d -5) 81.25) 2 2)) "")
	(command "line" (strcat "-235.5," (rtos (- (* d -5) 81.25) 2 2)) (strcat "-234," (rtos (- (* d -5) 81.25) 2 2)) "")

	;rowFour
	;mid
	(command "line" (strcat "-238.25," (rtos (- (* d -5) 83.75) 2 2)) (strcat "-236.75," (rtos (- (* d -5) 83.75) 2 2)) "")
	;left
	(command "circle" (strcat "-240.25," (rtos (- (* d -5) 83.75) 2 2)) 0.30)
	(command "circle" (strcat "-240.25," (rtos (- (* d -5) 83.75) 2 2)) 1.0)
	(command "line" (strcat "-242.25," (rtos (- (* d -5) 83.75) 2 2)) (strcat "-243.75," (rtos (- (* d -5) 83.75) 2 2)) "")
	;right
	(command "circle" (strcat "-234.75," (rtos (- (* d -5) 83.75) 2 2)) 0.30)
	(command "circle" (strcat "-234.75," (rtos (- (* d -5) 83.75) 2 2)) 1.0)
	(command "line" (strcat "-232.75," (rtos (- (* d -5) 83.75) 2 2)) (strcat "-231.25," (rtos (- (* d -5) 83.75) 2 2)) "")
	
	(command "text" "J" "C" (list -237.5 (- (* d -5) 91.5)) 3 0 "铝质泥岩")
 ) 
 
  ;炭质泥岩
 
 (defun drawTanZhiNiYan()
	(drawTanZhiNiYanTop)
  	(drawTanZhiNiYanBottom)
 )
 
 (defun drawTanZhiNiYanOne()
	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	;rowOne
	(command "line" (strcat "-191," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-189.5," (rtos (- (* d -5) 41.25) 2 2)) "")
	
	(command "line" (strcat "-185.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-184," (rtos (- (* d -5) 41.25) 2 2)) "")
	
	(command "circle" (strcat "-187.5," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	(command "-hatch" "W" "N" (strcat "-187.9," (rtos (- (* d -5) 40.5) 2 2)) (strcat "-186.7," (rtos (- (* d -5) 40.5) 2 2)) (strcat "-187.1," (rtos (- (* d -5) 42) 2 2))  (strcat "-188.3," (rtos (- (* d -5) 42) 2 2)) "C" "" "P" "SOLID" "")
 )
 
(defun drawTanZhiNiYanTop()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "line" (strcat "-195," (rtos (- (* d -5) 42.5) 2 2)) (strcat "-180," (rtos (- (* d -5) 42.5) 2 2)) "")
	(command "line" (strcat "-195," (rtos (- (* d -5) 45) 2 2)) (strcat "-180," (rtos (- (* d -5) 45) 2 2)) "")
	
	;rowOne
	(command "line" (strcat "-191," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-189.5," (rtos (- (* d -5) 41.25) 2 2)) "")
	
	(command "line" (strcat "-185.5," (rtos (- (* d -5) 41.25) 2 2)) (strcat "-184," (rtos (- (* d -5) 41.25) 2 2)) "")
	
	(command "circle" (strcat "-187.5," (rtos (- (* d -5) 41.25) 2 2)) 0.10)
	
	(command "-hatch" "W" "N" (strcat "-187.5," (rtos (- (* d -5) 40.5) 2 2)) (strcat "-186.3," (rtos (- (* d -5) 40.5) 2 2)) (strcat "-187," (rtos (- (* d -5) 42) 2 2))  (strcat "-188.2," (rtos (- (* d -5) 42) 2 2)) "C" "" "P" "SOLID" "")

	;rowTwo
	(command "line" (strcat "-192.5," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-194," (rtos (- (* d -5) 43.75) 2 2)) "")
	(command "line" (strcat "-186.75," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-188.25," (rtos (- (* d -5) 43.75) 2 2)) "")
	(command "line" (strcat "-182.75," (rtos (- (* d -5) 43.75) 2 2)) (strcat "-181.25," (rtos (- (* d -5) 43.75) 2 2)) "")

)

(defun drawTanZhiNiYanBottom()
  	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-195," (rtos (- (* d -5) 40) 2 2)) (strcat "-180," (rtos (- (* d -5) 50) 2 2)) " ")
	(command "line" (strcat "-195," (rtos (- (* d -5) 47.5) 2 2)) (strcat "-180," (rtos (- (* d -5) 47.5) 2 2)) "")
	
	;rowThree
	(command "line" (strcat "-191," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-189.5," (rtos (- (* d -5) 46.25) 2 2)) "")
	
	(command "line" (strcat "-185.5," (rtos (- (* d -5) 46.25) 2 2)) (strcat "-184," (rtos (- (* d -5) 46.25) 2 2)) "")
	
	(command "circle" (strcat "-187.5," (rtos (- (* d -5) 46.25) 2 2)) 0.10)
	
	(command "-hatch" "W" "N" (strcat "-187.5," (rtos (- (* d -5) 45.5) 2 2)) (strcat "-186.3," (rtos (- (* d -5) 45.5) 2 2)) (strcat "-187," (rtos (- (* d -5) 47) 2 2))  (strcat "-188.2," (rtos (- (* d -5) 47) 2 2)) "C" "" "P" "SOLID" "")

	;rowFour
	(command "line" (strcat "-192.5," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-194," (rtos (- (* d -5) 48.75) 2 2)) "")
	(command "line" (strcat "-186.75," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-188.25," (rtos (- (* d -5) 48.75) 2 2)) "")
	(command "line" (strcat "-182.75," (rtos (- (* d -5) 48.75) 2 2)) (strcat "-181.25," (rtos (- (* d -5) 48.75) 2 2)) "")
	
	
	(command "text" "J" "C" (list -187.5 (- (* d -5) 56.5)) 3 0 "炭质泥岩")
   	
)

  ;煤层
 
 (defun drawMeiCeng()
	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "-hatch" "W" "Y" (strcat "-245," (rtos (- (* d -5) 40) 2 2)) (strcat "-230," (rtos (- (* d -5) 40) 2 2)) (strcat "-230," (rtos (- (* d -5) 50) 2 2))  (strcat "-245," (rtos (- (* d -5) 50) 2 2)) "C" "" "P" "SOLID" "")
	(command "text" "J" "C" (list -237.5 (- (* d -5) 56.5)) 3 0 "煤层")

 )
 
   ;假整合
(defun drawJiaZhengHe()
	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-195," (rtos (- (* d -5) 75) 2 2)) (strcat "-180," (rtos (- (* d -5) 85) 2 2)) " ")
	(command "line" (strcat "-195," (rtos (- (* d -5) 80) 2 2)) (strcat "-192.85," (rtos (- (* d -5) 80) 2 2)) "")
	(command "line" (strcat "-190.7," (rtos (- (* d -5) 80) 2 2)) (strcat "-188.55," (rtos (- (* d -5) 80) 2 2)) "")
	(command "line" (strcat "-186.4," (rtos (- (* d -5) 80) 2 2)) (strcat "-184.25," (rtos (- (* d -5) 80) 2 2)) "")
	(command "line" (strcat "-182.1," (rtos (- (* d -5) 80) 2 2)) (strcat "-180," (rtos (- (* d -5) 80) 2 2)) "")

	(command "text" "J" "C" (list -187.5 (- (* d -5) 91.5)) 3 0 "假整合")
)

   ;角度不整合
(defun drawJiaoDuBuZhengHe()
	(command "zoom" "C" (list -222.5 (* d -5)) "250")
	(command "Rectang" (strcat "-145," (rtos (- (* d -5) 75) 2 2)) (strcat "-130," (rtos (- (* d -5) 85) 2 2)) " ")
	(command "SPLine" (strcat "-145," (rtos (- (* d -5) 79) 2 2)) (strcat "-143," (rtos (- (* d -5) 81) 2 2)) (strcat "-141," (rtos (- (* d -5) 79) 2 2)) (strcat "-139," (rtos (- (* d -5) 81) 2 2)) (strcat "-137," (rtos (- (* d -5) 79) 2 2)) (strcat "-135," (rtos (- (* d -5) 81) 2 2)) (strcat "-133," (rtos (- (* d -5) 79) 2 2)) (strcat "-130," (rtos (- (* d -5) 81) 2 2)) "" "" "")
	(command "text" "J" "C" (list -137.5 (- (* d -5) 91.5)) 3 0 "角度不整合")
)

;黄土层
(defun drawHuangTuCeng()
	(drawHuangTuCengTop)
	(drawHuangTuCengBottom)
)

(defun drawHuangTuCengTop()
	(command "line" (strcat "-394.5," (rtos (- (* d -5) 6.5) 2 2)) (strcat "-393," (rtos (- (* d -5) 5.5) 2 2)) "")
	(command "line" (strcat "-394," (rtos (- (* d -5) 7.5) 2 2)) (strcat "-391," (rtos (- (* d -5) 5.5) 2 2)) "")
	(command "line" (strcat "-393.5," (rtos (- (* d -5) 7.165) 2 2)) "@1.5<300" "")
	(command "line" (strcat "-392.5," (rtos (- (* d -5) 6.5) 2 2)) "@1.5<300" "")
	(command "-hatch" "W" "Y" (strcat "-388," (rtos (- (* d -5) 5.5) 2 2)) (strcat "-390," (rtos (- (* d -5) 5.5) 2 2)) (strcat "-389," (rtos (- (* d -5) 8) 2 2)) "C" "" "P" "SOLID" "")
	
	
	(command "line" (strcat "-387," (rtos (- (* d -5) 6.5) 2 2)) (strcat "-385.5," (rtos (- (* d -5) 5.5) 2 2)) "")
	(command "line" (strcat "-386.5," (rtos (- (* d -5) 7.5) 2 2)) (strcat "-383.5," (rtos (- (* d -5) 5.5) 2 2)) "")
	(command "line" (strcat "-386," (rtos (- (* d -5) 7.165) 2 2)) "@1.5<300" "")
	(command "line" (strcat "-385," (rtos (- (* d -5) 6.5) 2 2)) "@1.5<300" "")
	(command "-hatch" "W" "Y" (strcat "-380.5," (rtos (- (* d -5) 5.5) 2 2)) (strcat "-382.5," (rtos (- (* d -5) 5.5) 2 2)) (strcat "-381.5," (rtos (- (* d -5) 8) 2 2)) "C" "" "P" "SOLID" "")
) 

(defun drawHuangTuCengBottom()
	(command "Rectang" (strcat "-395," (rtos (- (* d -5) 5) 2 2)) (strcat "-380," (rtos (- (* d -5) 15) 2 2)) " ")
	(command "text" "J" "C" (list -387.5 (- (* d -5) 21.5)) 3 0 "黄土")
	
	(command "line" (strcat "-393.5," (rtos (- (* d -5) 9) 2 2)) "@5.5<270" "")
	(command "line" (strcat "-390.5," (rtos (- (* d -5) 9) 2 2)) "@5.5<270" "")
	(command "line" (strcat "-387.5," (rtos (- (* d -5) 9) 2 2)) "@5.5<270" "")
	(command "line" (strcat "-384.5," (rtos (- (* d -5) 9) 2 2)) "@5.5<270" "")
	(command "line" (strcat "-381.5," (rtos (- (* d -5) 9) 2 2)) "@5.5<270" "")

)

(defun drawHuangTuCengBottomLineWithRockThinkness()
	(command "line" (strcat (strcat (rtos (- RockHatchXOffset 4.5) 2 2) ",") (rtos (* (- rockDeep (- rockThickness 0.8)) -5) 2 2)) (strcat (strcat "@" (rtos (* (- rockThickness 0.8) 5) 2 2)) "<270") "") 
	(command "line" (strcat (strcat (rtos (- RockHatchXOffset 1.5) 2 2) ",") (rtos (* (- rockDeep (- rockThickness 0.8)) -5) 2 2)) (strcat (strcat "@" (rtos (* (- rockThickness 0.8) 5) 2 2)) "<270") "") 
	(command "line" (strcat (strcat (rtos (+ RockHatchXOffset 1.5) 2 2) ",") (rtos (* (- rockDeep (- rockThickness 0.8)) -5) 2 2)) (strcat (strcat "@" (rtos (* (- rockThickness 0.8) 5) 2 2)) "<270") "") 
	(command "line" (strcat (strcat (rtos (+ RockHatchXOffset 4.5) 2 2) ",") (rtos (* (- rockDeep (- rockThickness 0.8)) -5) 2 2)) (strcat (strcat "@" (rtos (* (- rockThickness 0.8) 5) 2 2)) "<270") "") 
)

;UserClient
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
