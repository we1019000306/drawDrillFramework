(defun c:lockBlocks(/ BLOCKNAME SELECTIONSET)  ;定义函数 
 (vl-load-com)  ;加载Vlisp函数  
 (if (setq Selectionset (ssget))  ;选择加密对象 
	(progn  (setq BlockName (getvar "CDATE"))  ;以当前时间作为块名  ;因为我们要用多重引用块进行加密，在制作多重引用块之前我们必须将我们需要 ;加密的图元制作成块，为避免与图档内块名冲突，引用了当前时间作为块名 
		 (command "block" BlockName '(0 0 0) Selectionset "" "minsert"   BlockName '(0 0 0) 1 1 0 100 1 10 0) ;用block命令制作块，然后用minsert命令制作多重引用块
		 (vla-put-name (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
		 (vla-get-name (vlax-ename->vla-object (entlast)))) "*U")  ;将多用引用块改成匿名多重引用块。匿名块不是无名块，它是以*U命名的，在块 ;编辑框中不现实匿名块的块名，从而无法编辑。
		 (princ "\n加密完成") 
		  (print (cdr (entget (entlast)))) 
		  (setq eHuangTu (entlast))
		 ) 
		 (princ "\n没有选中图元")  
		
	 ) 
 (princ)
) 

(defun emkunameblk(ss pt / i name)
	(entmake (list '(0 . "block") '(2 . "*U") '(70 . 1) (cons 10 pt)))
	(repeat (setq i (sslength ss)))
	(entmake (cdr (entget (ssname ss (setq i (i - 1))))))
	(setq name (entmake '((0 . "ENDBLK"))))
	;(command "_.erase" ss "")
	(entmake (list '(0 . "INSERT") (cons 2 name) (cons 10 pt)))
)

(defun testBlock(e)
	;(print eHuangTu)
	;((0 . "INSERT") (330 . <图元名: 410a29f0>) (5 . "CE1") (100 . "AcDbEntity") (67 . 0) (410 . "Model") (8 . "0") (100 . "AcDbMInsertBlock") (2 . "*U26") (10 0.0 0.0 0.0) (41 . 1.0) (42 . 1.0) (43 . 1.0) (50 . 0.0) (70 . 1) (71 . 100) (44 . 0.0) (45 . 10.0) (210 0.0 0.0 1.0))
	(modifyLockBlocks e '(1000 1000 0)
)

(defun modifyLockBlocks(e insertPt rowIndex rowSpace  yProportion)
		(entmod (subst (cons 10 insertPt) (assoc 10 e) e)
		(entmod (subst (cons 42 yProportion) (assoc 42 e) e)
		(entmod (subst (cons 45 rowSpace) (assoc 45 e) e)
		(entmod (subst (cons 71 rowIndex) (assoc 71 e) e)
)

(defun creatLockedBlocks(excelRowNum / selectBlockPt insertBlockPt  rowIndex rowSpace  yProportion)
	 (vl-load-com)  ;加载Vlisp函数  
	 
	
	 
	 
	 
	 
	 (setq Selectionset ((ssget "w" '(68 73) '(54 78))))
 (if  Selectionset
	(progn  (setq BlockName (getvar "CDATE"))  ;以当前时间作为块名  ;因为我们要用多重引用块进行加密，在制作多重引用块之前我们必须将我们需要 ;加密的图元制作成块，为避免与图档内块名冲突，引用了当前时间作为块名 
		 (command "block" BlockName '(0 0 0) Selectionset "" "minsert"   BlockName '(0 0 0) 1 1 0 10 1 10 0) ;用block命令制作块，然后用minsert命令制作多重引用块
		 (vla-put-name (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
		 (vla-get-name (vlax-ename->vla-object (entlast)))) "*U")  ;将多用引用块改成匿名多重引用块。匿名块不是无名块，它是以*U命名的，在块 ;编辑框中不现实匿名块的块名，从而无法编辑。
		 (princ "\n加密完成") 
		  (print (cdr (entget (entlast)))) 
		  (setq eFen (entlast))
		 ) 
		 (princ "\n没有选中图元")  
		
	 ) 
)

(defun checkRockStyle()
	(cond ((= strRock "黄土")  
					(progn
						(print "黄土")
							
					) 
				)
			   ((= strRock "粉砂岩")
					(progn 
						(print "粉砂岩")
					)
				)  
				((= strRock "细粒砂岩")
					(progn 
						(print "细粒砂岩")
					)
				)  
				((= strRock "中粒砂岩")
					(progn 
						(print "中粒砂岩")
					)
				)  
				((= strRock "粗粒砂岩")
					(progn 
						(print "粗粒砂岩")
					)
				)  
				((= strRock "粉砂质泥岩")
					(progn 
						(print "粉砂质泥岩")
					)
				)  
				((= strRock "泥质粉砂岩")
					(progn 
						(print "泥质粉砂岩")
					)
				)  
				((= strRock "砂砾岩")
					(progn 
						(print "砂砾岩")
					)
				)  
				((= strRock "泥岩")
					(progn 
						(print "泥岩")
					)
				)  
				((= strRock "煤层")
					(progn 
						(print "煤层")
					)
				)  
				((= strRock "炭质泥岩")
					(progn 
						(print "炭质泥岩")
					)
				)  
				((= strRock "砂质泥岩")
					(progn 
						(print "砂质泥岩")
					)
				)  
				((= strRock "泥质砂岩")
					(progn 
						(print "泥质砂岩")
					)
				)  
				((= strRock "石灰岩")
					(progn 
						(print "石灰岩")
					)
				)  
				((= strRock "泥质灰岩")
					(progn 
						(print "泥质灰岩")
					)
				)  
				((= strRock "铝土质泥岩")
					(progn 
						(print "铝土质泥岩")
					)
				)  
				((= strRock "假整合")
					(progn 
						(print "假整合")
					)
				)  
				((= strRock "角度不整合")
					(progn 
						(print "角度不整合")
					)
				)  
	
	)
)