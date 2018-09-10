(defun c:lockBlocks(/ BLOCKNAME SELECTIONSET)  ;定义函数 
 (vl-load-com)  ;加载Vlisp函数  
 (if (setq Selectionset (ssget))  ;选择加密对象 
	(progn  (setq BlockName (getvar "CDATE"))  ;以当前时间作为块名  ;因为我们要用多重引用块进行加密，在制作多重引用块之前我们必须将我们需要 ;加密的图元制作成块，为避免与图档内块名冲突，引用了当前时间作为块名 
		 (command "block" BlockName '(0 0 0) Selectionset "" "minsert"   BlockName '(0 0 0) 1 1 0 100 1 10 0) ;用block命令制作块，然后用minsert命令制作多重引用块
		 (vla-put-name (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
		 (vla-get-name (vlax-ename->vla-object (entlast)))) "*U")  ;将多用引用块改成匿名多重引用块。匿名块不是无名块，它是以*U命名的，在块 ;编辑框中不现实匿名块的块名，从而无法编辑。
		 (princ "\n加密完成")  ) 
		 (princ "\n没有选中图元")  
	 ) 
 (princ)
) 

(defun emkunameblk(ss pt / i name)
	(entmake (list '(0 . "block") '(2 . "*U") '(70 . 1) (cons 10 pt)))
	(repeat (setq i (sslength ss)))
	(entmake (cdr (entget (ssname ss (setq i (i - 1))))))
	(setq name (entmake '((0 . "ENDBLK"))))
	(command "_.erase" ss "")
	(entmake (list '(0 . "INSERT") (cons 2 name) (cons 10 pt)))
)