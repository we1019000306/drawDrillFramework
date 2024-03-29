(defun c:waterLevel()
  	(vl-load-com)
	(Setq EXCEL (Vlax-Get-Or-Create-Object "Excel.Application"))
	(Vlax-Put-Property EXCEL 'Visible -1 )
	(setq cells (Vlax-Get excel 'Cells ))
	(setq i 3)
  	(setq XX (Vlax-Get (vlax-variant-value (vlax-get-property cells "item" (+ i 1) 1)) 'Value2))
	(setq TT (= XX nil))
  	(while (= TT nil)
  		(setq XX (Vlax-Get (vlax-variant-value (vlax-get-property cells "item" (+ i 1) 1)) 'Value2))
		(setq TT (= XX nil))
  		(setq SX (Vlax-Get (vlax-variant-value (vlax-get-property cells "item" i 1)) 'Value2))
		(if (= (type SX) 'STR) (setq SX (atof SX)) (setq SX SX))
	  	(setq SX (* SX -5))
		(setq SY (Vlax-Get (vlax-variant-value (vlax-get-property cells "item" i 2)) 'Value2))
  		(if (= (type SY) 'STR) (setq SY (atof SY)) (setq SY SY))
	        (setq SY (* SY 0.5))
	  	(setq SY (- SY 385))
  		(setq SP (list SY SX))
	  	(setq i (+ i 1))
	  	(setq TX (Vlax-Get (vlax-variant-value (vlax-get-property cells "item" i 1)) 'Value2))
  		(if (= (type TX) 'STR) (setq TX (atof TX)) (setq TX TX))
	  	(setq TX (* TX -5))
  		(setq TY (Vlax-Get (vlax-variant-value (vlax-get-property cells "item" i 2)) 'Value2))
  		(if (= (type TY) 'STR) (setq TY (atof TY)) (setq TY TY))
	        (setq TY (* TY 0.5))
	  	(setq TY (- TY 385))
  		(setq TP (list TY TX))
  		(command "line" SP TP "")
	 )
)



;;;	(setq c1c13 (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range ("a"&rows.count).end(3).row))
;;;  	(setq lst (vlax-safearray->list (vlax-variant-value (Vlax-Get-Property c1c13 'Value2))))
;;;
;;;	(setq indexA 1)
;;;  	(setq indexB 1)
;;;  	(setq ListLength (length lst))
;;;  	
;;;	(while (<= indexA ListLength)
;;;;;;		(print (Vlax-Get (vlax-variant-value (Vlax-Get-Property c1c13 'Item indexA )) 'Value2 ))
;;;		(if (> indexA 1)
;;;		  (progn
;;;		  	(setq indexB (- indexA 1))
;;;		    	(setq ptB (Vlax-Get (vlax-variant-value (Vlax-Get-Property c1c13 'Item indexB)) 'Value2))
;;;			(setq ptA (Vlax-Get (vlax-variant-value (Vlax-Get-Property c1c13 'Item indexA)) 'Value2))
;;;			(print ptB)
;;;			(print ptA)
;;;			(command "line" ptB ptA "")	    
;;;;;;		  	(print (Vlax-Get (vlax-variant-value (Vlax-Get-Property c1c13 'Item indexB)) 'Value2 ))
;;;;;;	  	  	(print indexB)
;;;		  )
;;;		)
;;;	
;;;		(setq indexA (1+ indexA))
;;;        ) 