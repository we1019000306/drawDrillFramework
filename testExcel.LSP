(defun c:testExcel()
		(vl-load-com)
		(Setq excel (Vlax-Get-Object "Excel.Application"))
		(Vlax-Put-Property excel 'Visible -1)

		(setq baseDataExcelFilePathName (getfiled "选择数据源Excel文件" "c:" "" 8))
		
		(setq wbs (vlax-get-property excel "WorkBooks"))
		(setq wb (vlax-invoke-method wbs "open" baseDataExcelFilePathName))
		
		(setq shs (vlax-get-property wb "Sheets"))
		(setq sh (vlax-get-property shs 'Item "钻时"))
		(setq rg (vlax-get-property sh 'Range "A65536"))
		(setq dataRg (vlax-get-property rg 'end -4162))
  		(setq finalRow (vlax-get-property dataRg 'row))
  		(setq finalDataRg (vlax-get-property sh 'Range (strcat "A2:" (strcat "A" (itoa finalRow)))))
		


	
		
		;(setq rockDataRangeOfY (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockOne))
		;(setq rockDataThickness (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockTwo))
		;(setq rockDataName (Vlax-Get-Property (Vlax-Get excel 'ActiveSheet) 'Range columnRockThree))
)

(defun non_getfiles ()
	(setq ob (vlax-create-object "MSComDlg.CommonDialog"))
	(vlax-put-property ob 'DialogTitle "nonsmall -- Lisp多选DWG文件")
	(vlax-put-property ob 'Filter  "DWG Files|*.dwg|All Files|*.*")
	(vlax-put-property ob 'MaxFileSize  10000)
	(vlax-put-property ob 'Flags 1574404)
	(vlax-put-property ob 'Action  1)
	(setq dataFileName (vlax-get ob 'filename))
)